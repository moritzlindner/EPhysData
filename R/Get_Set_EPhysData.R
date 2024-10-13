#' Get/Set methods for EPhysData objects
#'
#' These methods are used to get and set the non-data slots from \link{EPhysData} objects, including functions for filtering, averaging, and rejecting trials. They are applied before retrieving data from the object (e.g. using When calling \link{GetData}, \link{as.data.frame}, or \link{ggEPhysData})
#'
#' @param X An \link{EPhysData} object
#' @param value A value to set, which must be a function (for \code{'Rejected()<-'}, for \code{'FilterFunction()<-'}, or for \code{'AverageFunction()<-'}) or, for \code{'Rejected()<-'} either a function or a logical vector.
#' @param return.fx For \code{Rejected()}: Whether to return the function or the resulting logical vector. Default is \code{FALSE}, i.e. to return the function. Not then when value is set to a function that requires parameters not contained in the \code{EPhysData} object itself, these must be either constants or, if expressions, they need to be evaluated when the function is created. i.e. those expressions should be called inside local() (\link[base:local]{base:local}).
#' @param ... Currently unused.
#' @details
#' The Functions assigned using the methods described herein will be applied before retrieving data from an \link{EPhysData} object in the following order:
#' \enumerate{
#'   \item \code{'FilterFunction()'}
#'   \item \code{'Rejected()'}
#'   \item \code{'AverageFunction()'}
#' }
#'  The Functions to \code{value} need to follow a few rules in order to work in general and also to be compatible with \link{Save}/\link{Load} in particular:
#'  \itemize{
#'  \item{Functions must be able to run with a single argument, which is a 2D numeric array for \code{'Rejected()'} and a numeric vector in case of \code{'FilterFunction()'} and \code{'AverageFunction()'}. Further explanation is given below}
#'  \item{If a function should use more parameters upon creation, it must be ensured that these are stored as values inside the function instead just referencing a variable in the current namespace (as the namespace is not persevered upon saving the object). This can be achieved by encapsulating the function in an \code{eval(substitute())} statement, and pass these additional variables on to \code{substitute()} as the second parameter. See also \link[base:substitute]{base:substitute()} and \link[base:eval]{base:eval()}. }
#' }
#'
#' @name Get_Set_EPhysData
#' @rdname GetSet-methods
#' @seealso \link[base:substitute]{base:substitute} \link[base:eval]{base:eval} \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}, \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass}, \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend},
#' @docType methods
#' @noMd
#' @examples
#' # Create an EPhysData object with example data
#' myEPhysData <- makeExampleEPhysData(replicate_count = sample(5:8, 1))
#'
#' # Get the "Rejected" slot
#' Rejected(myEPhysData)
#' Rejected(myEPhysData, return.fx=T)
#' head(GetData(myEPhysData)) # the error can be ignored, as no averaging function has yet been set
#'
#' # Set the "Rejected" slot
#' Rejected(myEPhysData) <- sample(c(TRUE,FALSE), dim(myEPhysData)[2], TRUE)
#' Rejected(myEPhysData)
#' Rejected(myEPhysData, return.fx=T)
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
#' head(GetData(myEPhysData,Raw=T))
#'
NULL


#' @details \code{Rejected}: These methods set or get a function returning a logical vector indicating which of the trials stored in an \link{EPhysData} object to exclude from averaging. The function set needs to be able to run with a single argument, which will receive the numeric 2D array stored in the data slot of the \link{EPhysData} object  as input. It is applied after running the filter set with \link[FilterFunction]{FilterFunction}. The following functions from the EPhysMethods package may be helpful: \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}.
#' @export
#' @docType methods
#' @rdname GetSet-methods
setGeneric(
  name = "Rejected",
  def = function(X, return.fx = F, ...) {
    standardGeneric("Rejected")
  }
)
#' @rdname GetSet-methods
#' @aliases Rejected,EPhysData,ANY-method
setMethod("Rejected", signature = "EPhysData", function(X, return.fx = F) {
  if (!return.fx) {
    tryCatch({
      dat<-X@Data
      unit.buffer<-deparse_unit(dat)
      dat<-apply(dat, 2, FilterFunction(X), simplify = T)
      dat<-as_units(dat,unit.buffer)
      out<-as.vector(X@Rejected(dat))
      if(length(out)!=dim(X)[2]){
        stop("Function call does not return vector of correct length.")
      }
      out
    }, error = function(e){
      stop("The function stored in the 'Rejected' slot could not be applied. A likely reason is that the function is malformed or does not fit to the data stored in the object. Object has: ", dim(X)[2]," trials. Function string is: '", deparse1(X@Rejected),"' and returned error message is '", e,"' " )
    })
  } else{
    return(X@Rejected)
  }
})

#' @export
#' @docType methods
#' @rdname GetSet-methods
setGeneric(
  name = "Rejected<-",
  def = function(X, ..., value) {
    standardGeneric("Rejected<-")
  }
)
#' @rdname GetSet-methods
#' @aliases `Rejected<-`,EPhysData,ANY-method
setMethod("Rejected<-", signature = "EPhysData", function(X, value) {
  if ("function" %in% class(value)) {
    if (dim(X)[2]>1){
    # test if function is defined for a matrix of the given size of X
      success <- tryCatch({
        out <- value(X@Data)
        if (!all(is.na(out)))
        {
          TRUE
        } else {
          FALSE
        }

      }, error = function(e) {
        FALSE
      })
      if (success) {
        X@Rejected <- value
      } else {
        warning(
          "Can't set a Rejected function for 'X', either because 'X' contains no, or too few trials or because the function isn't appropriate. It must return a logical vector of the same length as trials (dim(X)[2]) in the object. Keeping all."
        )
        value <- logical(dim(X)[2])
      }
    } else {
      message(
        "Can't set a Rejected function for 'X', because 'X' contains only one trial. Keeping it."
      )
      value <- logical(dim(X)[2])
    }
  } else{
    if ("logical" %in% class(value)) {
      if (length(value) == dim(X)[2]) {
        X@Rejected <- eval(substitute(function(x) {
          return(VALUE)
        }, list(VALUE = value)))
      } else{
        stop("Incorrect length of logical vector.")
      }
    } else{
      stop("Incorrect data type; must be logical or function.")
    }
  }
  if (validEPhysData(X)) {
    return(X)
  }
})

#' @details \code{FilterFunction}: Set  a function for filtering each individual of the trials in the \link{EPhysData} object. Could be downsampling or noise removal, for instance. The following functions from the EPhysMethods package may be helpful: \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass}, \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend},
#' @export
#' @docType methods
#' @rdname GetSet-methods
setGeneric(
  name = "FilterFunction",
  def = function(X, ...) {
    standardGeneric("FilterFunction")
  }
)
#' @rdname GetSet-methods
#' @aliases FilterFunction,EPhysData,ANY-method
setMethod("FilterFunction", signature = "EPhysData", function(X) {
  return(X@filter.fx)
})

#' @export
#' @docType methods
#' @rdname GetSet-methods
setGeneric(
  name = "FilterFunction<-",
  def = function(X, ..., value) {
    standardGeneric("FilterFunction<-")
  }
)
#' @rdname GetSet-methods
#' @aliases `FilterFunction<-`,EPhysData,ANY-method
setMethod("FilterFunction<-", signature = "EPhysData", function(X, value) {
  success <- tryCatch({
    out<-apply(X@Data, 2, value, simplify = T)
    ret<-T
    if (!all(is.na(out)))
    {
      ret<-TRUE
    } else {
      ret<-FALSE
    }
    if(dim(out)[1]!=dim(X)[1]){
      ret<-FALSE
    }
    if(dim(out)[2]!=dim(out)[2]){
      ret<-FALSE
    }
    ret
  }, error = function(e) {
    FALSE
  })
  if(!success){
    warning("Can't set filter function for 'X', likely because the function isn't appropriate. Must return a vector of the same length as the template vector.")
  }
  X@filter.fx <- value
  if (validEPhysData(X)) {
    return(X)
  }
})

#' @details \code{AverageFunction}: Set a function describing how averaging across trials measurement should be performed in the \link{EPhysData} object. Usually, \link[base:mean]{mean} can be a good start.
#' @export
#' @docType methods
#' @rdname GetSet-methods
setGeneric(
  name = "AverageFunction",
  def = function(X, ...) {
    standardGeneric("AverageFunction")
  }
)

#' @rdname GetSet-methods
#' @aliases AverageFunction,EPhysData,ANY-method
setMethod("AverageFunction", signature = "EPhysData", function(X) {
  return(X@average.fx)
})

#' @export
#' @docType methods
#' @rdname GetSet-methods
setGeneric(
  name = "AverageFunction<-",
  def = function(X, ..., value) {
    standardGeneric("AverageFunction<-")
  }
)

#' @rdname GetSet-methods
#' @aliases `AverageFunction<-`,EPhysData,ANY-method
setMethod("AverageFunction<-", signature = "EPhysData", function(X, value) {
    success <- tryCatch({
      ret <- TRUE
      out <- apply(X@Data, 1, value, simplify = T)
      ret <- (!all(is.na(out)))
      if (!is.null(dim(out))) {
        if (dim(out)[1] != dim(X)[1]) {
          ret <- FALSE
        }
        if (dim(out)[2] != 1) {
          ret <- FALSE
        }
      } else{
        if (length(out) != dim(X)[1]) {
          ret <- FALSE
        }
      }
      ret
    }, error = function(e) {
      FALSE
    })
    if (!success) {
      if (dim(X)[[2]] == 1) {
        warning("Object only contains single trial. The function you are trying to set is not valid and cant be set.")
      } else {
        stop(
          "Can't set averaging function for 'X', either because 'X' contains too few trials or because the function isn't appropriate. Function must return a single value when applied to a vector."
        )
      }
    }
    X@average.fx <- value
  if (validEPhysData(X)) {
    return(X)
  }
})

#' @details \code{TimeTrace}: These functions set or get the time trace belonging to the measurements stored in an \link{EPhysData} object.
#' @export
#' @docType methods
#' @rdname GetSet-methods
setGeneric(
  name = "TimeTrace",
  def = function(X) {
    standardGeneric("TimeTrace")
  }
)

#' @rdname GetSet-methods
#' @aliases TimeTrace,EPhysData,ANY-method
setMethod("TimeTrace", signature = "EPhysData", function(X) {
  return(X@TimeTrace)
})

#' @details \code{StimulusTrace}: These functions set or get the stimulus trace belonging to the measurements stored in an \link{EPhysData} object.
#' @export
#' @docType methods
#' @rdname GetSet-methods
setGeneric(
  name = "StimulusTrace",
  def = function(X)
  {
    standardGeneric("StimulusTrace")
  }
)

#' @rdname GetSet-methods
#' @aliases StimulusTrace,EPhysData,ANY-method
setMethod("StimulusTrace",
          "EPhysData",
          function(X) {
            if (length(X@StimulusTrace)!=0){
              out<-X@StimulusTrace
              return(out)
            }else{
              stop("No stimulus trace contained in 'EPhysData' object")
            }
          })
