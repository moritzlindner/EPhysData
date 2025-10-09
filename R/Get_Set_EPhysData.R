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
#' head(GetData(myEPhysData)) # the error can be ignored, as no averaging function has yet been set
#'
#' # Set the "Rejected" slot
#' Rejected(myEPhysData) <- sample(c(TRUE,FALSE), dim(myEPhysData)[2], TRUE)
#' Rejected(myEPhysData)
#' Rejected(myEPhysData, return.fx=TRUE)
#' head(GetData(myEPhysData))  # the error can be ignored, as no averaging function has yet been set
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
#' value = sample(c(TRUE,FALSE),1)
#' FilterFunction(myEPhysData)<- eval(substitute(function(x) {
#'   scale(x, center= VALUE)
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

#' @describeIn GetSet-methods Get the rejection function or its computed logical vector.
#' @export
setMethod("Rejected", signature = "EPhysData", function(X, return.fx = FALSE) {
  if (!return.fx) {
    tryCatch({
      dat <- X@Data
      unit.buffer <- deparse_unit(dat)
      dat <- apply(dat, 2, FilterFunction(X), simplify = TRUE)
      dat <- as_units(dat, unit.buffer)
      out <- as.vector(X@Rejected(dat))
      if (length(out) != dim(X)[2]) {
        stop("Function call does not return vector of correct length.")
      }
      out
    }, error = function(e) {
      stop("The function stored in the 'Rejected' slot could not be applied. ",
           "Object has: ", dim(X)[2], " trials. Function string is: '",
           deparse1(X@Rejected), "' and returned error message is '", e, "' ")
    })
  } else {
    X@Rejected
  }
})

#' @describeIn GetSet-methods Set the rejection function or a logical vector.
#' @export
setMethod("Rejected<-", signature = "EPhysData", function(X, value) {
  if (is.function(value)) {
    if (dim(X)[2] > 1) {
      success <- tryCatch({
        out <- value(X@Data)
        !all(is.na(out))
      }, error = function(e) FALSE)
      if (success) {
        X@Rejected <- value
      } else {
        warning("Can't set a Rejected function for 'X'. ",
                "It must return a logical vector of length dim(X)[2]. Keeping all.")
        value <- logical(dim(X)[2])
      }
    } else {
      message("Can't set a Rejected function because 'X' contains only one trial. Keeping it.")
      value <- logical(dim(X)[2])
    }
  } else if (is.logical(value)) {
    if (length(value) == dim(X)[2]) {
      X@Rejected <- eval(substitute(function(x) VALUE, list(VALUE = value)))
    } else {
      stop("Incorrect length of logical vector.")
    }
  } else {
    stop("Incorrect data type; must be logical or function.")
  }
  if (validEPhysData(X)) X
})

#' @details \code{FilterFunction}: Set a function for filtering each trial in the
#' \link{EPhysData} object (e.g., downsampling or noise removal).
#' @describeIn GetSet-methods Get the per-trial filter function.
#' @export
setMethod("FilterFunction", signature = "EPhysData", function(X) X@filter.fx)

#' @describeIn GetSet-methods Set the per-trial filter function.
#' @export
setMethod("FilterFunction<-", signature = "EPhysData", function(X, value) {
  success <- tryCatch({
    out <- apply(X@Data, 2, value, simplify = TRUE)
    ok <- !all(is.na(out))
    if (!ok) return(FALSE)
    if (!is.matrix(out)) return(FALSE)
    if (nrow(out) != dim(X)[1]) return(FALSE)
    if (ncol(out) != dim(X)[2]) return(FALSE)  # fixed dimension check
    TRUE
  }, error = function(e) FALSE)
  if (!success) {
    warning("Can't set filter function for 'X'; it must return a matrix with the ",
            "same dimensions as X@Data (per-trial, column-wise).")
  }
  X@filter.fx <- value
  if (validEPhysData(X)) X
})

#' @describeIn GetSet-methods Get the averaging function across trials.
#' @export
setMethod("AverageFunction", signature = "EPhysData", function(X) X@average.fx)

#' @describeIn GetSet-methods Set the averaging function across trials.
#' @export
setMethod("AverageFunction<-", signature = "EPhysData", function(X, value) {
  success <- tryCatch({
    out <- apply(X@Data, 1, value, simplify = TRUE)
    if (!is.null(dim(out))) {
      if (nrow(out) != dim(X)[1]) return(FALSE)
      if (ncol(out) != 1) return(FALSE)
    } else {
      if (length(out) != dim(X)[1]) return(FALSE)
    }
    !all(is.na(out))
  }, error = function(e) FALSE)
  if (!success) {
    if (dim(X)[2] == 1) {
      warning("Object contains a single trial; the provided averaging function is not valid.")
    } else {
      stop("Can't set averaging function. It must return a single value for a vector (per row).")
    }
  }
  X@average.fx <- value
  if (validEPhysData(X)) X
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
  tt <- X@TimeTrace
  u <- TimeUnits(X)
  if (length(u) == 0) {
    u <- ""
  }
  tt <-
    tryCatch(
      set_units(tt, u, mode = "standard"),
      error = function(e)
        NULL
    )
  tt
})

#' @describeIn GetSet-methods Return the stimulus trace (units applied via StimulusUnits).
#' @export
setMethod("StimulusTrace", signature = "EPhysContainer", function(X) {
  st <- X@StimulusTrace
  u <- StimulusUnits(X)
  if (length(u) == 0) {
    u <- ""
  }
  st <-
    tryCatch(
      set_units(st, u, mode = "standard"),
      error = function(e)
        NULL
    )
  st
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
                   tt_len <- tryCatch(length(TimeTrace(X)), error = function(e) NA_integer_)
                   if (!is.na(tt_len) && length(value) != tt_len) {
                     stop(sprintf("Length mismatch: StimulusTrace (%d) vs TimeTrace (%d).",
                                  length(value), tt_len))
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
