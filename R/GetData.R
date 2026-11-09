#' Get Data from EPhysData Object
#'
#' This method extracts raw or processed data from an \code{EPhysData} object.
#'
#' @inheritParams GetSet-methods
#' @param Time Numeric vector of length 2 representing the time range for data extraction.
#'             Default is the entire time range (i.e., keep all data).
#' @param TimeExclusive Keep only the two points stated under 'Time', not the range.
#' @param Trials Specifies which of the trials (if more than one) to use .
#'                It can be either a numeric vector specifying the indices of the trials,
#'                a logical vector of the same length as trials stored, where `TRUE` indicates using that column for extraction, or
#'                NULL. if \code{Trials = NULL} and \code{Raw = false}(see below), then the function stored in the Rejected (\link{Rejected}) slot will be used (default), if \code{Raw = false} then all Trials are returned.
#' @param Raw Logical indicating whether to get raw data or processed (filtered - see: \link{FilterFunction}, averaged - see: \link{AverageFunction}) data.
#' @return A data matrix containing either raw or processed (filtered, averaged) values.
#'
#' @details The `GetData` function extracts the recorded data from an `EPhysData` or related object.
#'          By default, the resulting data matrix contains unfiltered data from all trials.
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

            operation <- "GetData(EPhysData)"
            validObject(X)
            if (!length(X@TimeTrace)) .ephys_abort(paste(operation, "found no time samples."))
            Time <- .ephys_legacy_time(X, Time, operation)
            if (!is.logical(Raw) || length(Raw) != 1L || is.na(Raw) ||
                !is.logical(TimeExclusive) || length(TimeExclusive) != 1L || is.na(TimeExclusive)) {
              .ephys_abort(paste(operation, "requires Raw and TimeExclusive to be TRUE or FALSE."))
            }

            n_trials <- ncol(X@Data)
            if (!is.null(Trials)) {
              if (is.numeric(Trials) && all(is.finite(Trials)) &&
                  all(Trials == floor(Trials) & Trials >= 1 & Trials <= n_trials)) {
                trial_mask <- seq_len(n_trials) %in% Trials
              } else if (is.logical(Trials) && length(Trials) == n_trials && !anyNA(Trials)) {
                trial_mask <- Trials
              } else {
                .ephys_abort(sprintf("%s requires trial indices in 1..%d or %d non-missing logical values.",
                                     operation, n_trials, n_trials))
              }
            }
            time <- condition_time(X, Time, TimeExclusive)
            time_mask <- TimeTrace(X) %in% time
            data <- if (Raw) X@Data else .ephys_filter_data(X, FilterFunction(X), operation)
            if (is.null(Trials)) {
              trial_mask <- if (Raw) rep(TRUE, n_trials) else
                !.ephys_rejected(data, Rejected(X, return.fx = TRUE), operation)
            }

            data <- data[time_mask, trial_mask, drop = FALSE]
            if (!nrow(data) || !ncol(data)) {
              .ephys_abort(sprintf("%s selected %d samples and %d trials. Both must be non-empty.",
                                   operation, nrow(data), ncol(data)))
            }

            average <- AverageFunction(X)
            if (!Raw && ncol(data) > 1L && !.ephys_is_identity(average)) {
              values <- .ephys_average_data(data, average, operation)
              data <- as_units(matrix(values, ncol = 1L, dimnames = list(NULL, "Averaged")),
                               deparse_unit(X@Data))
            }
            data
          })

.ephys_legacy_time <- function(X, Time, operation) {
  if (!is.numeric(Time) || !length(Time) || any(!is.finite(Time))) {
    .ephys_abort(paste(operation, "requires finite numeric Time values."))
  }
  unit <- deparse_unit(TimeTrace(X))
  value <- tryCatch(
    units::set_units(Time, unit, mode = "standard"),
    error = function(e) .ephys_abort(sprintf("%s could not convert Time to %s.",
                                            operation, shQuote(unit)), parent = e)
  )
  available <- range(as.numeric(TimeTrace(X)))
  requested <- range(as.numeric(value))
  if (requested[1L] < available[1L] || requested[2L] > available[2L]) {
    .ephys_abort(sprintf("%s received Time [%g, %g] %s. Available range is [%g, %g] %s.",
                         operation, requested[1L], requested[2L], unit,
                         available[1L], available[2L], unit))
  }
  value
}

#' @keywords internal
#' @noMd
condition_time <- function(X, Time, TimeExclusive) {
  trace <- TimeTrace(X)
  if (isTRUE(all.equal(Time, range(trace))) && !TimeExclusive) return(trace)
  if (length(Time) == 1L || (TimeExclusive && length(Time) == 2L)) {
    positions <- vapply(seq_along(Time), function(i) {
      which.min(abs(as.numeric(trace) - as.numeric(Time[i])))
    }, integer(1))
    return(trace[positions])
  }
  if (TimeExclusive) return(Time)
  if (length(Time) != 2L || Time[1L] > Time[2L]) {
    .ephys_abort("GetData(EPhysData) requires two ordered bounds for a Time interval.")
  }
  trace[trace >= Time[1L] & trace <= Time[2L]]
}
