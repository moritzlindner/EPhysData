#' @import units
#' @noMd
setOldClass("units")
setOldClass("symbolic_units")


#' @importFrom  units deparse_unit
#' @keywords internal
#' @noMd
validEPhysData <- function(object) {
  return(TRUE)
  if (!(inherits(object@Data, "units"))){
    stop("'Data' does not contain units.")
  }
  if (!(inherits(TimeTrace(object), "units"))){
    stop("'TimeTrace' does not contain units.")
  }
  err <- tryCatch(
    StimulusTrace(object),
    error = function(e)
      e
  )
  has_stimtrace <- !any(class(err) == "error")

  if(has_stimtrace){
    if (!(inherits(StimulusTrace(object), "units"))){
      stop("'StimulusTrace' does not contain units.")
    }

  }
  # Check that TimeTrace and StimulusTrace are the same length as the number of rows in Data
  n_rows <- nrow(object@Data)
  if (length(TimeTrace(object)) != n_rows) {
    stop("'TimeTrace' length should be the same as the number of rows in 'Data'.")
  }

  if(has_stimtrace){
    if (length(StimulusTrace(object)) > 0 &&
        length(StimulusTrace(object)) != n_rows) {
      stop("'StimulusTrace' length should be the same as the number of rows in 'Data'.")
    }
  }

  # Check TimeUnit
  if (!(deparse_unit(TimeTrace(object)) %in% c("us","ms","s", "min", "h", "d", "w"))) {
    stop("'TimeUnit' should be either of 's','min','h','d','w'")
  }

  # Check Rejected
  n_cols <- ncol(object@Data)
  if (length(Rejected(object)) != n_cols) {
    stop("'Rejected' length should be the same as the number of columns in 'Data', i.e. the number of repeated measurements.")
  }

  # check functions
  if (!is.function(FilterFunction(object))) {
    stop("'filter.fx' must be a function.")
  }

  tryCatch(
    FilterFunction(object)(c(1:10)),
    error = function(e) {
      stop("'filter.fx' can not handle numeric vectors. Recheck the function and make sure it can be run with one single parameter representing a numeric vector.")
    }
  )

  if (!is.function(AverageFunction(object))) {
    stop("'average.fx' must be a function.")
  }

  tryCatch(
    AverageFunction(object)(c(1:10)),
    error = function(e) {
      stop("'average.fx' can not handle numeric vectors. Recheck the function and make sure it can be run with one single parameter representing a numeric vector.")
    }
  )
  # If all checks passed, return TRUE
  TRUE
}

#' EPhysData - Class for Electrophysiological Data
#'
#' This class represents the most fundamental unit of electrophysiological data acquisition, typically obtained in response to a one single type of stimulus. It includes repeated measurements, time and stimulus traces, units, as well as information on post-processing steps (such as filtering and averaging) that can be applied to the data.
#'
#' @slot Data A 2D array representing the  electrophysiological data with columns for repeated measurements
#' @slot TimeTrace A numeric vector containing the time trace of the electrophysiological data.
#' @slot StimulusTrace A numeric vector containing the stimulus trace (optional).
#' @slot Created A POSIXct object representing the creation date and time of the object.
#' @slot Rejected A logical vector indicating which repeated measurements to exclude from averaging.
#' @slot filter.fx A function that can be applied to each repeated measurement By default, it leaves the data unchanged.
#' @slot average.fx A function describing how averaging across repeated measurement should be performed. By default, repeated measurements are returned and no averaging is performed.
#' @importFrom units as_units
#' @name EPhysData-class
#' @docType class
#' @return An object of class \code{EPhysData}
#' @seealso \link{newEPhysData}
#' @exportClass EPhysData
EPhysData <- setClass(
  "EPhysData",
  slots = list(
    Data = "units",
    TimeTrace = "units",
    StimulusTrace = "units",
    Created = "POSIXct",
    Rejected = "function",
    filter.fx = "function",
    average.fx = "function"
  ),
  prototype = list(
    Data = as_units(array(dim=c(0,0)),unitless),
    TimeTrace = as_units(integer(),"s"),
    StimulusTrace = as_units(integer(),unitless),
    Created = as.POSIXct(Sys.time()),
    Rejected = function(x) {
      rep(FALSE, 0)
    },
    filter.fx = function(x) {
      x
    },
    average.fx = function(x) {
      x
    }
  ),
  validity = validEPhysData
)

#' @importFrom units deparse_unit
#' @keywords internal
#' @noMd
#'
setMethod("show",
          "EPhysData",
          function(object) {
            cat("An object of class EPhysData \n")
            cat("Sample points:", dim(object@Data)[1], " \n")
            cat("Repeats:", dim(object@Data)[2], " \n")
            cat("Duration", as.character(diff(range(TimeTrace(object)))), deparse_unit(TimeTrace(object)),"\n")
            cat("Created", as.character(object@Created),"\n")
          })
