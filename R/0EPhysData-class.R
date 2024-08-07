#' @import units
#' @noMd
setOldClass("units")
setOldClass("symbolic_units")


#' @importFrom  units deparse_unit
#' @importFrom cli cli_abort
#' @keywords internal
#' @noMd
validEPhysData <- function(object) {
  return(TRUE)
  if (!(inherits(object@Data, "units"))){
    cli_abort("'Data' does not contain units.")
  }
  if (!(inherits(TimeTrace(object), "units"))){
    cli_abort("'TimeTrace' does not contain units.")
  }
  err <- tryCatch(
    StimulusTrace(object),
    error = function(e)
      e
  )
  has_stimtrace <- !any(class(err) == "error")

  if(has_stimtrace){
    if (!(inherits(StimulusTrace(object), "units"))){
      cli_abort("'StimulusTrace' does not contain units.")
    }

  }
  # Check that TimeTrace and StimulusTrace are the same length as the number of rows in Data
  n_rows <- nrow(object@Data)
  if (length(TimeTrace(object)) != n_rows) {
    cli_abort("'TimeTrace' length should be the same as the number of rows in 'Data'.")
  }

  if(has_stimtrace){
    if (length(StimulusTrace(object)) > 0 &&
        length(StimulusTrace(object)) != n_rows) {
      cli_abort("'StimulusTrace' length should be the same as the number of rows in 'Data'.")
    }
  }

  # Check TimeUnit
  if (!(deparse_unit(TimeTrace(object)) %in% c("us","ms","s", "min", "h", "d", "w"))) {
    cli_abort("'TimeUnit' should be either of 's','min','h','d','w'")
  }

  # Check Rejected
  n_cols <- ncol(object@Data)
  if (length(Rejected(object)) != n_cols) {
    cli_abort("'Rejected' length should be the same as the number of columns in 'Data', i.e. the number of trials (see the Data slot in {.help [EPhysData-class](EPhysData::EPhysData-class)}")
  }

  # check functions
  if (!is.function(FilterFunction(object))) {
    stop("'filter.fx' must be a function.")
  }

  tryCatch(
    FilterFunction(object)(c(1:10)),
    error = function(e) {
      cli_abort("'filter.fx' can not handle numeric vectors. Recheck the function and make sure it can be run with one single parameter representing a numeric vector.")
    }
  )

  if (!is.function(AverageFunction(object))) {
    cli_abort("'average.fx' must be a function.")
  }

  tryCatch(
    AverageFunction(object)(c(1:10)),
    error = function(e) {
      cli_abort("'average.fx' can not handle numeric vectors. Recheck the function and make sure it can be run with one single parameter representing a numeric vector.")
    }
  )
  # If all checks passed, return TRUE
  TRUE
}

#' EPhysData - Class for Electrophysiological Data
#'
#' This class represents the most fundamental unit of electrophysiological data acquisition, typically obtained in response to a one single type of stimulus. It includes data (trials in columns), time and stimulus traces, units, as well as information on post-processing steps (such as filtering and averaging) that can be applied to the data.
#'
#' @slot Data A 2D array representing the  electrophysiological data. trials (i.e. linked data with a common time basis, like repeated measurements) are represented by columns. Trials may be equivalent to “segments”, “episodes”, “runs”, “recordings”, depending on the nature of the data.
#' @slot TimeTrace A numeric vector containing the time trace of the electrophysiological data.
#' @slot StimulusTrace A numeric vector containing the stimulus trace (optional).
#' @slot Created A POSIXct object representing the creation date and time of the object.
#' @slot Rejected A logical vector indicating which trials to exclude from averaging.
#' @slot filter.fx A function that can be applied to each segment. By default, it leaves the data unchanged.
#' @slot average.fx A function describing how averaging across trials should be performed. By default, each segment is returned and no averaging is performed.
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
            cat("Trials:", dim(object@Data)[2], " \n")
            cat("Duration", as.character(diff(range(TimeTrace(object)))), deparse_unit(TimeTrace(object)),"\n")
            cat("Created", as.character(object@Created),"\n")
          })
