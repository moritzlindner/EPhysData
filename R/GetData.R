#' Get Data from EPhysData Object
#'
#' This method extracts raw or processed data from an \code{EPhysData} object.
#'
#' @inheritParams Get_Set_EPhysData
#' @param Time Numeric vector of length 2 representing the time range for data extraction.
#'             Default is the entire time range (i.e., keep all data).
#' @param TimeExclusive Keep only the two points stated under 'Time', not the range.
#' @param Trials Specifies which of the repeated measurements (if any) to use for extraction.
#'                It can be either a numeric vector specifying the indices of the repeated measurements,
#'                a logical vector of the same length as trials stored, where `TRUE` indicates using that column for extraction, or
#'                NULL, indicating that the function stored in the Rejected (\link{Rejected}) slot will be used (default).
#'
#' @param Raw Logical indicating whether to get raw data or processed (filtered - see: \link{FilterFunction}, averaged - see: \link{AverageFunction}) data.
#' @return A data matrix containing either raw or processed (filtered, averaged) values.
#'
#' @details The `GetData` function extracts the recorded data from an `EPhysData` or related object.
#'          By default, the resulting data matrix contains unfiltered data from all repeated measurements.
#'          Note, the main difference to the \link{as.data.frame}-Method is that this function returns a matrix in the wide format and without a time and stimulus trace.
#'
#' @seealso \code{\link{EPhysData-class}} \code{\link{TimeTrace}}, \code{\link{Rejected}}
#' @docType methods
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
#' @name GetData
#' @rdname GetData-methods
setGeneric(
  name = "GetData",
  def = function(X,
                 Time = range(TimeTrace(X)),
                 TimeExclusive = F,
                 Trials = NULL,
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
                   Trials = NULL,
                   Raw = F) {

            # failsafe checks
            ## Time Range
            if (!(all(Time >= range(TimeTrace(X))[1]) &
                  all(Time <= range(TimeTrace(X))[2]))) {
              stop("'Time' outside range of X.")
            }

            ## Trials
            if(!is.null(Trials) && !is.numeric(Trials) && !is.logical(Trials)){
              stop("'Trials must be NULL, 'numeric' or 'logical', but is ", class(Trials),".")
            }
            if(is.numeric(Trials)){
              if (all(Trials > 0) && all(Trials <= dim(X@Data)[2])) {
                TrialsLogical<-logical(dim(X)[2])
                TrialsLogical[Trials] <- T
              } else{
                stop("'Trials' outside range of X.")
              }
            }
            if(is.logical(Trials)){
              if (length(Trials) == dim(X@Data)[2]) {
                TrialsLogical <- Trials
                if(!all(TrialsLogical == (!Rejected(X))) && !Raw){
                  warning(
                    "Manual subsetting by trials requested for retruning non-raw data ('Raw = F'). Usually the 'Rejected(X)' method should be used instead."
                  )
                }
              } else{
                stop("'Trials' has length ", length(Trials), " which is different from the number of trials stored in the EPhysData object 'X' (", dim(X)[2], "). ")
              }
            }
            if ((dim(X@Data)[2]==1) & !is.null(Trials)){
              if(!Trials){
                stop("'Trials' must  be 'TRUE' if only one trace contained in object.")
              }
              TrialsLogical<-TRUE
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

            # subset by Trials
            if(is.null(Trials)){
              TrialsLogical<-!Rejected(X, return.fx = T)(out)
            }
            out<-out[TimeTrace(X) %in% Time,TrialsLogical,drop=F]

            if(any(dim(out)==0)){
              stop("No data left after subsetting with the given parameters for 'Trials' and 'Time'.")
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
