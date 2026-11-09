#' Get/Set methods and accessors for EPhysData / EPhysContainer
#'
#' These methods get and set non-data slots of \link{EPhysData} (filtering,
#' trial rejection, averaging) and provide accessors for
#' \link{EPhysContainer}-like classes (time/stimulus traces, units, channels,
#' metadata lists). In addition, setter methods are provided for the stimulus
#' trace and its units in \link{EPhysContainer}.
#'
#' When retrieving data (e.g., via \link{GetData} or \link{as.data.frame}),
#' the following functions are applied **in order**:
#' \enumerate{
#'   \item \code{FilterFunction()}
#'   \item \code{Rejected()}
#'   \item \code{AverageFunction()}
#' }
#'
#' @param X An \link{EPhysData} or \link{EPhysContainer} instance, depending on the method.
#' @param value A value to set. For \code{Rejected<-}: a function **or** a logical
#'   vector. For \code{FilterFunction<-} and \code{AverageFunction<-}: a function.
#'   For \code{StimulusTrace<-}: numeric or a \pkg{units} vector (unit captured).
#'   For \code{StimulusUnits<-}: unit string (e.g., \code{"ms"}, \code{"Hz"}, \code{"V"})
#'   or a \pkg{units} object (its unit string is captured).
#' @param return.fx For \code{Rejected()}: if \code{TRUE}, return the stored function;
#'   otherwise (default) return its computed logical vector.
#' @param ... Currently unused.
#'
#' @details
#' Functions assigned to \code{value} must accept a single argument:
#' a 2D numeric matrix for \code{Rejected()}, and a numeric vector for
#' \code{FilterFunction()} and \code{AverageFunction()}.
#' If your function depends on external values at creation time, embed those
#' values inside the function (e.g., with \code{eval(substitute(...))} or
#' \code{\link[base:local]{local}}) so they survive save/load.
#'
#' @seealso
#' \link[base:substitute]{substitute}, \link[base:eval]{eval},
#' \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance},
#' \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree},
#' \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass},
#' \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend}
#'
#' @examples
#' # Create an EPhysData object with example data
#' myEPhysData <- makeExampleEPhysData(replicate_count = sample(5:8, 1))
#'
#' # Get the "Rejected" slot
#' Rejected(myEPhysData)
#' Rejected(myEPhysData, return.fx=TRUE)
#' head(GetData(myEPhysData))
#'
#' # Set the "Rejected" slot
#' Rejected(myEPhysData) <- seq_len(ncol(myEPhysData)) == 1L
#' Rejected(myEPhysData)
#' Rejected(myEPhysData, return.fx=TRUE)
#' head(GetData(myEPhysData))
#' head(GetData(myEPhysData,Trials=1:dim(myEPhysData)[2]))
#'
#' # Get the "filter.fx" slot
#' FilterFunction(myEPhysData)
#'
#' # Set the "filter.fx" slot
#' FilterFunction(myEPhysData) <- scale
#' FilterFunction(myEPhysData)
#'
#' # Set a fucntion to the "filter.fx" slot that depends on a variable upon creation
#' value <- TRUE
#' FilterFunction(myEPhysData)<- eval(substitute(function(x) {
#'   scale(x, center = VALUE)
#' }, list(VALUE = value)))
#' rm(value)
#' FilterFunction(myEPhysData)
#'
#' # Get the "average.fx" slot
#' AverageFunction(myEPhysData)
#' head(GetData(myEPhysData))
#'
#' # Set the "average.fx" slot
#' AverageFunction(myEPhysData) <- median
#' AverageFunction(myEPhysData)
#' head(GetData(myEPhysData))
#' head(GetData(myEPhysData,Raw=TRUE))
#'
#' @docType methods
#' @name GetSet-methods
#' @rdname GetSet-methods
#' @noMd
#' @importFrom units deparse_unit as_units drop_units set_units
NULL

# ---- Generics: EPhysData Get/Set (no guards) ----

#' @rdname GetSet-methods
#' @export
setGeneric("Rejected", function(X, return.fx = FALSE, ...) standardGeneric("Rejected"))

#' @rdname GetSet-methods
#' @export
setGeneric("Rejected<-", function(X, ..., value) standardGeneric("Rejected<-"))

#' @rdname GetSet-methods
#' @export
setGeneric("FilterFunction", function(X, ...) standardGeneric("FilterFunction"))

#' @rdname GetSet-methods
#' @export
setGeneric("FilterFunction<-", function(X, ..., value) standardGeneric("FilterFunction<-"))

#' @rdname GetSet-methods
#' @export
setGeneric("AverageFunction", function(X, ...) standardGeneric("AverageFunction"))

#' @rdname GetSet-methods
#' @export
setGeneric("AverageFunction<-", function(X, ..., value) standardGeneric("AverageFunction<-"))


# ---- Generics: EPhysContainer accessors (no guards) ----

#' @rdname GetSet-methods
#' @export
setGeneric("TimeTrace", function(X) standardGeneric("TimeTrace"))

#' @rdname GetSet-methods
#' @export
setGeneric("StimulusTrace", function(X) standardGeneric("StimulusTrace"))

#' @rdname GetSet-methods
#' @export
setGeneric("TimeUnits", function(X) standardGeneric("TimeUnits"))

#' @rdname GetSet-methods
#' @export
setGeneric("StimulusUnits", function(X) standardGeneric("StimulusUnits"))

#' @rdname GetSet-methods
#' @export
setGeneric("Channels", function(X) standardGeneric("Channels"))

#' @rdname GetSet-methods
#' @export
setGeneric("ExamInfo", function(X) standardGeneric("ExamInfo"))

#' @rdname GetSet-methods
#' @export
setGeneric("SubjectInfo", function(X) standardGeneric("SubjectInfo"))

#' @rdname GetSet-methods
#' @export
setGeneric("Imported", function(X) standardGeneric("Imported"))

# Replacement generics for EPhysContainer
#' @rdname GetSet-methods
#' @export
setGeneric("StimulusTrace<-", function(X, value) standardGeneric("StimulusTrace<-"))

#' @rdname GetSet-methods
#' @export
setGeneric("StimulusUnits<-", function(X, value) standardGeneric("StimulusUnits<-"))


# ---- Methods: EPhysData Get/Set ----

.ephys_check_callback <- function(value, operation) {
  if (!is.function(value)) {
    .ephys_abort(sprintf("%s requires a function. Received %s.",
                         operation, paste(class(value), collapse = "/")))
  }
}

.ephys_filter_data <- function(X, FUN, operation) {
  data <- units::drop_units(X@Data)
  cols <- base::lapply(seq_len(ncol(data)), function(i) {
    context <- sprintf("%s filter failed for trial %d.", operation, i)
    result <- .ephys_callback(FUN, list(data[, i]), context)
    if (!is.numeric(result) || length(result) != nrow(data)) {
      .ephys_abort(sprintf("%s Expected %d numeric samples, received %d values of type %s.",
                           context, nrow(data), length(result), typeof(result)),
                   "ephys_callback_error")
    }
    as.numeric(result)
  })
  filtered <- matrix(as.numeric(unlist(cols)), nrow(data), ncol(data), dimnames = dimnames(data))
  as_units(filtered, deparse_unit(X@Data))
}

.ephys_rejected <- function(data, FUN, operation) {
  context <- paste(operation, "rejection callback failed.")
  result <- .ephys_callback(FUN, list(data), context)
  if (!is.logical(result) || !is.null(dim(result)) ||
      length(result) != ncol(data) || anyNA(result)) {
    .ephys_abort(sprintf("%s Expected %d non-missing logical values, received %d values of type %s.",
                         context, ncol(data), length(result), typeof(result)),
                 "ephys_callback_error")
  }
  result
}

.ephys_average_data <- function(data, FUN, operation) {
  data <- units::drop_units(data)
  vapply(seq_len(nrow(data)), function(i) {
    context <- sprintf("%s averaging callback failed at sample %d.", operation, i)
    result <- .ephys_callback(FUN, list(data[i, ]), context)
    if (!is.numeric(result) || length(result) != 1L) {
      .ephys_abort(sprintf("%s Expected one numeric value, received %d values of type %s.",
                           context, length(result), typeof(result)), "ephys_callback_error")
    }
    as.numeric(result)
  }, numeric(1))
}

#' @describeIn GetSet-methods Get the rejection function or its computed logical vector.
#' @export
setMethod("Rejected", signature = "EPhysData", function(X, return.fx = FALSE) {
  if (return.fx) return(X@Rejected)
  data <- .ephys_filter_data(X, X@filter.fx, "Rejected(EPhysData)")
  .ephys_rejected(data, X@Rejected, "Rejected(EPhysData)")
})

#' @describeIn GetSet-methods Set the rejection function or a logical vector.
#' @export
setMethod("Rejected<-", signature = "EPhysData", function(X, value) {
  operation <- "Rejected<-(EPhysData)"
  if (is.function(value)) {
    data <- .ephys_filter_data(X, X@filter.fx, operation)
    .ephys_rejected(data, value, operation)
  } else if (is.logical(value) && is.null(dim(value)) &&
             length(value) == ncol(X@Data) && !anyNA(value)) {
    value <- eval(substitute(function(x) MASK, list(MASK = value)))
  } else {
    .ephys_abort(sprintf("%s requires a function or %d non-missing logical values. Received %d values of type %s.",
                         operation, ncol(X@Data), length(value), typeof(value)))
  }
  X@Rejected <- value
  validObject(X)
  X
})

#' @details \code{FilterFunction}: Set a per-trial filter returning one numeric value per input sample.
#' @describeIn GetSet-methods Get the per-trial filter function.
#' @export
setMethod("FilterFunction", signature = "EPhysData", function(X) X@filter.fx)

#' @describeIn GetSet-methods Set the per-trial filter function.
#' @export
setMethod("FilterFunction<-", signature = "EPhysData", function(X, value) {
  operation <- "FilterFunction<-(EPhysData)"
  .ephys_check_callback(value, operation)
  .ephys_filter_data(X, value, operation)
  X@filter.fx <- value
  validObject(X)
  X
})

#' @describeIn GetSet-methods Get the averaging function across trials.
#' @export
setMethod("AverageFunction", signature = "EPhysData", function(X) X@average.fx)

#' @describeIn GetSet-methods Set the averaging function across trials.
#' @export
setMethod("AverageFunction<-", signature = "EPhysData", function(X, value) {
  operation <- "AverageFunction<-(EPhysData)"
  .ephys_check_callback(value, operation)
  if (!.ephys_is_identity(value)) {
    data <- .ephys_filter_data(X, X@filter.fx, operation)
    .ephys_average_data(data, value, operation)
  }
  X@average.fx <- value
  validObject(X)
  X
})

#' @describeIn GetSet-methods Return the time trace (EPhysData).
#' @export
setMethod("TimeTrace", signature = "EPhysData", function(X) X@TimeTrace)

#' @describeIn GetSet-methods Return the stimulus trace (EPhysData).
#' @export
setMethod("StimulusTrace", signature = "EPhysData", function(X) {
  if (length(X@StimulusTrace) == 0) {
    stop("No stimulus trace contained in 'EPhysData' object.")
  }
  X@StimulusTrace
})


# ---- Methods: EPhysContainer accessors ----

#' @describeIn GetSet-methods Return the time trace (units applied via TimeUnits).
#' @export
setMethod("TimeTrace", signature = "EPhysContainer", function(X) {
  tryCatch(
    set_units(X@TimeTrace, TimeUnits(X), mode = "standard"),
    error = function(e) .ephys_abort(sprintf("TimeTrace(%s) could not apply TimeUnits %s.",
                                            class(X)[1L], shQuote(TimeUnits(X))), parent = e)
  )
})

#' @describeIn GetSet-methods Return the stimulus trace (units applied via StimulusUnits).
#' @export
setMethod("StimulusTrace", signature = "EPhysContainer", function(X) {
  st <- X@StimulusTrace
  u <- StimulusUnits(X)
  if (!length(u) || identical(u, "")) u <- "1"
  tryCatch(
    set_units(st, u, mode = "standard"),
    error = function(e) .ephys_abort(sprintf("StimulusTrace(%s) could not apply StimulusUnits %s.",
                                            class(X)[1L], shQuote(u)), parent = e)
  )
})

#' @describeIn GetSet-methods Returns the declared time units (udunits symbol).
#' @export
setMethod("TimeUnits", signature = "EPhysContainer", function(X) X@TimeUnits)

#' @describeIn GetSet-methods Returns the declared stimulus units (udunits symbol).
#' @export
setMethod("StimulusUnits", signature = "EPhysContainer", function(X) X@StimulusUnits)

#' @describeIn GetSet-methods Returns the channel names.
#' @export
setMethod("Channels", signature = "EPhysContainer", function(X) X@Channels)

#' @describeIn GetSet-methods Returns the exam information list.
#' @export
setMethod("ExamInfo", signature = "EPhysContainer", function(X) X@ExamInfo)

#' @describeIn GetSet-methods Returns the subject information list.
#' @export
setMethod("SubjectInfo", signature = "EPhysContainer", function(X) X@SubjectInfo)

#' @describeIn GetSet-methods Returns the import timestamp (POSIXct).
#' @export
setMethod("Imported", signature = "EPhysContainer", function(X) X@Imported)


# ---- Methods: EPhysContainer replacement (setters) ----

#' @describeIn GetSet-methods Set the stimulus trace (units allowed; unit captured).
#' @export
setReplaceMethod("StimulusTrace",
                 signature(X = "EPhysContainer", value = "ANY"),
                 function(X, value) {
                   if (inherits(value, "units")) {
                     StimulusUnits(X) <- units::deparse_unit(value)
                     value <- units::drop_units(value)
                   }
                   value <- as.numeric(value)
                   tt_len <- length(X@TimeTrace)
                   if (length(value) && length(value) != tt_len) {
                     .ephys_abort(sprintf("StimulusTrace<-(%s) received %d samples. Expected 0 or %d.",
                                          class(X)[1L], length(value), tt_len))
                   }
                   X@StimulusTrace <- value
                   validObject(X)
                   X
                 }
)

#' @describeIn GetSet-methods Set the stimulus units (string or units object).
#' @export
setReplaceMethod("StimulusUnits",
                 signature(X = "EPhysContainer", value = "ANY"),
                 function(X, value) {
                   if (inherits(value, "units")) {
                     value <- units::deparse_unit(value)
                   } else {
                     tryCatch(units::as_units(value),
                              error = function(e) stop("`value` is not a valid unit string for the units package."))
                   }
                   X@StimulusUnits <- as.character(value)
                   validObject(X)
                   X
                 }
)
