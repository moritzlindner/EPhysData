#' Get Data from EPhysData Object
#'
#' This method extracts raw or processed data from an \code{EPhysData} object.
#'
#' @inheritParams Get_Set_EPhysData
#' @param Time Numeric vector of length 2 representing the time range for data extraction.
#'             Default is the entire time range (i.e., keep all data).
#' @param TimeExclusive Keep only the two points stated under 'Time', not the range.
#' @param Repeats Specifies which of the repeated measurements (if any) to use for extraction.
#'                It can be either a numeric vector specifying the indices of the repeated measurements
#'                or a logical vector of the same length as repeats stored,
#'                where `TRUE` indicates using that column for extraction. Default is the inverse of the \code{\link{Rejected}}(X) vector.
#' @param Raw Logical indicating whether to get raw data or processed (filtered, averaged) data.
#' @return A data matrix containing either raw or processed (filtered, averaged) values.
#'
#' @details The `GetData` function extracts the recorded data from an `EPhysData` or related object.
#'          By default, the resulting data matrix contains unfiltered data from all repeated measurements.
#'
#' @seealso \code{\link{EPhysData-class}} \code{\link{TimeTrace}}, \code{\link{Rejected}}
#'
#' @name GetData
#' @examples
#' myEPhysData <- makeExampleEPhysData(sample_points = sample(seq(1, 400, 10), 1),replicate_count = sample(1:5, 1))
#' myEPhysData
#' # Get raw data
#' raw_data <- GetData(myEPhysData, Raw = TRUE)
#' head(raw_data)
#' # Get processed (filtered and averaged) data
#' AverageFunction(myEPhysData) <- median
#' filtered_and_averaged_data <- GetData(myEPhysData, Raw = FALSE)
#' head(filtered_and_averaged_data)
#' @export
#' @docType methods
#' @rdname GetData-methods
setGeneric(
  name = "GetData",
  def = function(X,
                 Time = range(TimeTrace(X)),
                 TimeExclusive = F,
                 Repeats = !Rejected(X),
                 Raw = F)
  {
    standardGeneric("GetData")
  }
)


#' @importFrom  units deparse_unit as_units
#' @rdname GetData-methods
#' @aliases GetData,EPhysData,ANY-method
setMethod("GetData",
          "EPhysData",
          function(X,
                   Time = range(TimeTrace(X)),
                   TimeExclusive = F,
                   Repeats = !Rejected(X),
                   Raw = F) {
            # failsafe checks
            if (!(all(Time >= range(TimeTrace(X))[1]) &
                  all(Time <= range(TimeTrace(X))[2]))) {
              stop("'Time' outside range of X.")
            }
            if (!is.null(dim(X@Data))){
              RepeatsLogical = logical(length = dim(X@Data)[2])

              if (is.numeric(Repeats)) {
                if (all(Repeats > 0) && all(Repeats < dim(X@Data)[2])) {
                  RepeatsLogical[Repeats] <- T
                } else{
                  stop("'Repeats' outside range of X.")
                }
              } else{
                if (is.logical(Repeats)) {
                  if (length(Repeats) == dim(X@Data)[2]) {
                    RepeatsLogical <- Repeats
                  } else{
                    stop("'Repeats' not same lenght as columns in 'X'")
                  }
                } else{
                  stop("'Repeats' neither logical nor numeric.")
                }
              }
              if (any(RepeatsLogical != (!Rejected(X)))
                  & !Raw) {
                RepeatsLogical <- !Rejected(X)
                warning("Subsetting by Repeats is not recomended when 'Raw = F'. Processing will be performed from all unrejected repeats (see: 'Rejected(X)').")
              }
            } else {
              if(length(Repeats)!=1 | any(!Repeats)){
                stop("'Repeats' must be TRUE of data has no repeated recordings stored.")
              } else {
                RepeatsLogical = TRUE
              }
            }

            # Time Ranges
            Time <- condition_time(X, Time, TimeExclusive)

            # extract data
            out<-X@Data

            # filter
            if (!Raw) {
              unit.buffer<-deparse_unit(out)
              out <- tryCatch(
                apply(out, 2, FilterFunction(X), simplify = T),
                error = function (e) {
                  stop("could not apply 'filter.fx' (",
                       deparse(FilterFunction(X)),
                       ") to data.")
                }
              )
              out<-as_units(as.matrix(out),unit.buffer)
            }

            # subset by time
            out <-
              out[TimeTrace(X) %in% Time, , drop = FALSE]
            if (is.null(dim(out))) {
              stop("No data left after subsetting with the given parameters for 'Repeats'.")
            }

            # subset by repeats
            out <-
              out[ , RepeatsLogical, drop=FALSE]
            if (is.null(dim(out))) {
              stop("No data left after subsetting with the given parameters for 'Repeats'.")
            }

            # averaging
            if (!Raw){
              tryCatch(
                valid.avg.fx <- length(AverageFunction(X)(1:3))==1,
                error = function (e) {
                  stop("Could not apply 'average.fx' (",
                       deparse(AverageFunction(X)),
                       ") to test vector 'c(1:3)'")
                }
              )
              if (dim(X)[2]>1 && !valid.avg.fx) {
                warning(
                  "Averaging function ",
                  deparse(AverageFunction(X)),
                  " returns more than a single value per time point. Has a valid function been set? Try e.g.: AverageFunction(X)<-mean"
                )
              }
            } else {
              valid.avg.fx <- T
            }


            if (!Raw & ncol(out)>1 & valid.avg.fx) {
              out <- tryCatch(
                apply(out, 1, AverageFunction(X), simplify = T),
                error = function (e) {
                  stop("Could not apply 'average.fx' (",
                       deparse(AverageFunction(X)),
                       ") to data.")
                }
              )
              out <- as.matrix(out)
              colnames(out) <- "Averaged"
              return(as_units(out, deparse_unit(X@Data)))
            } else{
              return(out)
            }
          })

#' @keywords internal
#' @noMd
condition_time <- function(X, Time, TimeExclusive) {
  if (!isTRUE(all.equal(Time, range(TimeTrace(X))))) {
    if (!TimeExclusive | length(Time)==1) {
      if (length(Time)>1){
        Time <-
          TimeTrace(X)[TimeTrace(X) >= Time[1] &
                         TimeTrace(X) <= Time[2]]
      }else{
        if(length(Time)==0){
          stop("'Time' argument is empty.")
        }
        Time <- TimeTrace(X)[which(abs(TimeTrace(X) - Time) == min(abs(TimeTrace(X) -
                                                                         Time)))]
      }
    } else{
      # if extracting exact time points. get closest to values entered
      if(length(Time)==2){
        Time[1] <-
          TimeTrace(X)[which(abs(TimeTrace(X) - Time[1]) == min(abs(TimeTrace(X) -
                                                                      Time[1])))]
        Time[2] <-
          TimeTrace(X)[which(abs(TimeTrace(X) - Time[2]) == min(abs(TimeTrace(X) -
                                                                      Time[2])))]
      }
    }
  } else{
    Time <- TimeTrace(X)
  }
  return(Time)
}
