#' Get Data from EPhysData Object
#'
#' This method extracts raw or processed data from an \code{EPhysData} object.
#'
#' @inheritParams Get_Set_EPhysData
#' @param Time Numeric vector of length 2 representing the time range for data extraction.
#'             Default is the entire time range (i.e., keep all data).
#' @param TimeExclusive Keep only the two time points stated under 'Time', not the range.
#' @param Repeats Specifies which of the repeated measurements (if any) to use for extraction.
#'                It can be either a numeric vector specifying the indices of the repeated measurements
#'                or a logical vector of the same length as repeats stored,
#'                where `TRUE` indicates using that column for extraction. Default is the inverse of the \code{\link{Rejected-method}}(X) vector.
#' @param Raw Logical indicating whether to get raw data or processed (filtered, averaged) data.
#' @return A data matrix containing either raw or processed (filtered, averaged) values.
#'
#' @details The `GetData` function extracts the recorded data from an `EPhysData` or related object.
#'          By default, the resulting data matrix contains unfiltered data from all repeated measurements.
#'
#' @seealso \code{\link{EPhysData-class}} \code{\link{TimeTrace-method}}, \code{\link{Rejected-method}}
#'
#' @name GetData
#' @examples
#' myEPhysData <- makeExampleEPhysData()

#' # Get raw data
#' raw_data <- GetData(myEPhysData)

#' # Get processed (filtered and averaged) data
#' AverageFunction(myEPhysData) <- median
#' filtered_and_averaged_data <- GetData(myEPhysData, Raw = FALSE)
#' @exportMethod GetData
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


#' @import units
#' @noMd
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
              warning("Subsetting by Repeats is not allowed when 'Raw = F'. Processing will be performed from all unrejected repeats (see: 'Rejected(X)').")
            }

            # Time Ranges
            Time <- condition_time(X, Time, TimeExclusive)

            # extract data
            out<-X@Data

            # subset by time
            out <-
              out[TimeTrace(X) %in% Time, , drop = FALSE]
            if (is.null(dim(out))) {
              stop("No data left after subsetting with the given parameters for 'Repeats'.")
            }

            # filter
            if (!Raw) {
              out <- tryCatch(
                apply(out, 2, FilterFunction(X)),
                error = function (e) {
                  stop("could not apply 'filter.fx' (",
                       deparse(FilterFunction(X)),
                       ") to data.")
                }
              )
            }

            # subset by repeats
            out <-
              out[ , RepeatsLogical, drop=FALSE]
            if (is.null(dim(out))) {
              stop("No data left after subsetting with the given parameters for 'Repeats'.")
            }

            # averaging
            if (!Raw) {
              out <- tryCatch(
                apply(out, 1, AverageFunction(X)),
                error = function (e) {
                  stop("could not apply 'average.fx' (",
                       deparse(AverageFunction(X)),
                       ") to data.")
                }
              )
              if(!is.null(dim(out))){
                out<-t(out)
              }
              return(as_units(out, deparse_unit(X@Data)))
            } else{
              return(out)
            }
          })
