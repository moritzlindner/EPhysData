#' Get/Set methods for EPhysData objects
#'
#' These methods are used to get and set the non-data slots from \link{EPhysData} objects.
#' @param X An \code{EPhysData} object
#' @param value A value (usually a function) to set.
#' @param return.fx For \code{Rejected()}: Whether to return the function or the resulting logical vector. Default is \code{FALSE}, i.e. to return the function.
#' @param ... Currently unused.
#' @name Get_Set_EPhysData
#' @rdname Get_Set_EPhysData
#' @seealso \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}, \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass}, \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend},
#' @docType methods
#' @noMd
#' @examples
#' # Create an EPhysData object with example data
#' myEPhysData <- makeExampleEPhysData()
#'
#' # Get the "Rejected" slot
#' Rejected(myEPhysData)
#'
#' # Set the "Rejected" slot
#' Rejected(myEPhysData) <- sample(c(TRUE,FALSE), dim(myEPhysData)[2], TRUE)
#'
#' # Get the "filter.fx" slot
#' FilterFunction(myEPhysData)
#'
#' # Set the "filter.fx" slot
#' FilterFunction(myEPhysData) <- function(x) x^2
#'
#' # Get the "average.fx" slot
#' AverageFunction(myEPhysData)
#'
#' # Set the "average.fx" slot
#' AverageFunction(myEPhysData) <- median
#'
NULL


#' @describeIn Get_Set_EPhysData Rejected
#' @details \code{Rejected}: These functions set or get a function returning a logical vector indicating which of the repeated measurements stored in an \link{EPhysData} object to exclude from averaging. The following function from the EPhysMethods package may be helpful: \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}
#' @exportMethod Rejected
setGeneric(
  name = "Rejected",
  def = function(X, return.fx = F, ...) {
    standardGeneric("Rejected")
  }
)
#' @noMd
setMethod("Rejected", signature = "EPhysData", function(X, return.fx = F) {
  if (!return.fx) {
    return(X@Rejected(X@Data))
  } else{
    return(X@Rejected)
  }
})

#' @describeIn Get_Set_EPhysData Rejected<-
#' @exportMethod Rejected<-
setGeneric(
  name = "Rejected<-",
  def = function(X, ..., value) {
    standardGeneric("Rejected<-")
  }
)
#' @noMd
setMethod("Rejected<-", signature = "EPhysData", function(X, value) {
  if ("function" %in% class(value)) {
    X@Rejected <- value
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

#' @describeIn Get_Set_EPhysData FilterFunction
#' @details \code{FilterFunction}: Set  a function for filtering each individual of the repeated measurements in the \link{EPhysData} object. Could be downsampling or noise removal, for instance. The following functions from the EPhysMethods package may be helpful: \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass}, \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend},
#' @exportMethod FilterFunction
setGeneric(
  name = "FilterFunction",
  def = function(X, ...) {
    standardGeneric("FilterFunction")
  }
)
#' @noMd
setMethod("FilterFunction", signature = "EPhysData", function(X) {
  return(X@filter.fx)
})

#' @describeIn Get_Set_EPhysData FilterFunction<-
#' @exportMethod FilterFunction<-
setGeneric(
  name = "FilterFunction<-",
  def = function(X, ..., value) {
    standardGeneric("FilterFunction<-")
  }
)
#' @noMd
setMethod("FilterFunction<-", signature = "EPhysData", function(X, value) {
  X@filter.fx <- value
  if (validEPhysData(X)) {
    return(X)
  }
})

#' @describeIn Get_Set_EPhysData AverageFunction
#' @details \code{AverageFunction}: Set a function describing how averaging across repeated measurement should be performed in the \link{EPhysData} object. Usually, \link[base:mean]{mean} can be a good start.
#' @exportMethod AverageFunction
setGeneric(
  name = "AverageFunction",
  def = function(X, ...) {
    standardGeneric("AverageFunction")
  }
)

#' @noMd
setMethod("AverageFunction", signature = "EPhysData", function(X) {
  return(X@average.fx)
})

#' @describeIn Get_Set_EPhysData AverageFunction<-
#' @exportMethod AverageFunction<-
setGeneric(
  name = "AverageFunction<-",
  def = function(X, ..., value) {
    standardGeneric("AverageFunction<-")
  }
)

#' @noMd
setMethod("AverageFunction<-", signature = "EPhysData", function(X, value) {
  X@average.fx <- value
  if (validEPhysData(X)) {
    return(X)
  }
})

#' @describeIn Get_Set_EPhysData TimeTrace
#' @details \code{TimeTrace}: These functions set or get the time trace belonging to the measurements stored in an \link{EPhysData} object.
#' @exportMethod TimeTrace
setGeneric(
  name = "TimeTrace",
  def = function(X) {
    standardGeneric("TimeTrace")
  }
)

#' @noMd
setMethod("TimeTrace", signature = "EPhysData", function(X) {
  return(X@TimeTrace)
})

#' @describeIn Get_Set_EPhysData StimulusTrace
#' @import units
#' @exportMethod StimulusTrace
setGeneric(
  name = "StimulusTrace",
  def = function(X)
  {
    standardGeneric("StimulusTrace")
  }
)

#' @noMd
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
