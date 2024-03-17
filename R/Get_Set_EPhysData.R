#' Get/Set methods for EPhysData objects
#'
#' These methods are used to get and set the non-data slots from \link{EPhysData} objects.
#' @param X An \code{EPhysData} object
#' @param value A value (usually a function) to set.
#' @param return.fx For \code{Rejected()}: Whether to return the function or the resulting logical vector. Default is \code{FALSE}, i.e. to return the function.
#' @param ... Currently unused.
#' @name Get_Set_EPhysData
#' @rdname GetSet-methods
#' @seealso \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}, \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass}, \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend},
#' @docType methods
#' @noMd
#' @examples
#' # Create an EPhysData object with example data
#' myEPhysData <- makeExampleEPhysData()
#'
#' # Get the "Rejected" slot
#' Rejected(myEPhysData)
#' head(GetData(myEPhysData))
#'
#' # Set the "Rejected" slot
#' Rejected(myEPhysData) <- sample(c(TRUE,FALSE), dim(myEPhysData)[2], TRUE)
#' Rejected(myEPhysData)
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


#' @details \code{Rejected}: These functions set or get a function returning a logical vector indicating which of the repeated measurements stored in an \link{EPhysData} object to exclude from averaging. The following function from the EPhysMethods package may be helpful: \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}
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
      out<-as.vector(X@Rejected(X@Data))
      if(length(out)!=dim(X)[2]){
        stop("Function call does not return vector of correct length.")
      }
      out
    }, error = function(e){
      stop("The function stored in the 'Rejected' slot could not be applied. A lkely reason is that the function is malformed or does not fit to the data stored in the object. Object has: ", dim(X)[2]," repeated measurements. Function string is: '", deparse1(X@Rejected),"' and returned error message is '", e,"' " )
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
    # test if function is defined for a matrix of the given size of X
    success <- tryCatch({
      out<-value(X@Data)
      if (!all(is.na(out)))
      {
        TRUE
      } else {
        FALSE
      }

    }, error = function(e) {
      FALSE
    })
    if(success){
      X@Rejected <- value
    } else {
      warning("Can't set a Rejected function for 'X', either because 'X' contains no, or to few trials or because the function isn't appropriate. It must return a logical vector of the same length as trials (dim(X)[2]) in the object. Keeping all.")
      value <- logical(dim(X)[2])
    }
  } else{
    if ("logical" %in% class(value)) {
      if (length(value) == dim(X)[2]) {
        X@Rejected <- function(x) {
          return(value)
        }
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

#' @details \code{FilterFunction}: Set  a function for filtering each individual of the repeated measurements in the \link{EPhysData} object. Could be downsampling or noise removal, for instance. The following functions from the EPhysMethods package may be helpful: \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass}, \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend},
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

#' @details \code{AverageFunction}: Set a function describing how averaging across repeated measurement should be performed in the \link{EPhysData} object. Usually, \link[base:mean]{mean} can be a good start.
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
    out<-apply(X@Data, 1, value, simplify = T)
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
  if(!success){
    stop("Can't set averaging function for 'X', either because 'X' contains no, or to few trials or because the function isn't appropriate. Function must return a single value when applied to a vector.")
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
