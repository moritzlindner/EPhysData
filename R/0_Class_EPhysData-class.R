#' @import units
#' @noMd
setOldClass("units")
setOldClass("symbolic_units")


#' @importFrom  units deparse_unit
#' @keywords internal
#' @noMd
validEPhysData <- function(object) {
  problems <- character()
  data <- object@Data
  time <- object@TimeTrace
  stimulus <- object@StimulusTrace

  if (!is.matrix(data) || !is.numeric(data)) {
    problems <- c(problems, "`Data` must be a numeric matrix [time x trial] with units.")
  } else if (length(time) != nrow(data)) {
    problems <- c(problems, sprintf("`TimeTrace` has %d samples. Expected %d to match `Data`.",
                                    length(time), nrow(data)))
  }
  if (!is.numeric(time) || any(!is.finite(time))) {
    problems <- c(problems, "`TimeTrace` must contain finite numeric sample times.")
  }
  if (length(stimulus) && length(stimulus) != length(time)) {
    problems <- c(problems, sprintf("`StimulusTrace` has %d samples. Expected 0 or %d to match `TimeTrace`.",
                                    length(stimulus), length(time)))
  }
  if (length(time)) {
    problem <- tryCatch({
      units::set_units(time[1L], "s", mode = "standard")
      NULL
    }, error = conditionMessage)
    if (!is.null(problem)) {
      problems <- c(problems, paste("`TimeTrace` must have time units.", problem))
    }
  }
  if (length(problems)) problems else TRUE
}

.ephys_identity <- function(x) x

.ephys_reject_none <- function(x) rep(FALSE, ncol(x))

.ephys_is_identity <- function(FUN) {
  is.function(FUN) && identical(formals(FUN), formals(.ephys_identity)) &&
    (identical(body(FUN), quote(x)) ||
       identical(body(FUN), quote({ x })) ||
       identical(body(FUN), quote({ return(x) })))
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
    Rejected = .ephys_reject_none,
    filter.fx = .ephys_identity,
    average.fx = .ephys_identity
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
