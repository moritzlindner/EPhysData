#' Get/Set methods for EPhysData objects
#'
#' These methods are used to get and set the non-data slots from \link{EPhysData} objects.
#' @param X An \code{EPhysData} object
#' @param value A value (usually a function) to set.
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
#' Rejected(myEPhysData) <- c(FALSE, FALSE, TRUE)
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


#' @rdname Get_Set_EPhysData
#' @details \code{Rejected}: These functions set or get a function returning a logical vector indicating which of the repeated measurements stored in an \link{EPhysData} object to exclude from averaging. The following function from the EPhysMethods package may be helpful: \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}
#' @noMd
setGeneric(
  name = "Rejected",
  def = function(X, return.fx = F, ...) {
    standardGeneric("Rejected")
  }
)
#' @exportMethod Rejected
setMethod("Rejected", signature = "EPhysData", function(X, return.fx = F) {
  if (!return.fx) {
    return(X@Rejected(X@Data))
  } else{
    return(X@Rejected)
  }
})

#' @rdname Get_Set_EPhysData
#' @exportMethod Rejected<-
setGeneric(
  name = "Rejected<-",
  def = function(X, value, ...) {
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

#' @rdname Get_Set_EPhysData
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

#' @rdname Get_Set_EPhysData
#' @exportMethod FilterFunction<-
setGeneric(
  name = "FilterFunction<-",
  def = function(X, value, ...) {
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

#' @rdname Get_Set_EPhysData
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

#' @rdname Get_Set_EPhysData
#' @exportMethod AverageFunction<-
setGeneric(
  name = "AverageFunction<-",
  def = function(X, value, ...) {
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

#' @rdname Get_Set_EPhysData
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

#' #' @rdname Get_Set_EPhysData
#' #' @exportMethod TimeTrace<-
#' setGeneric(
#'   name = "TimeTrace<-",
#'   def = function(X, value) {
#'     standardGeneric("TimeTrace<-")
#'   }
#' )
#' #' @noMd
#' setMethod("TimeTrace<-", signature = "EPhysData", function(X, value) {
#'   X@TimeTrace <- value
#'   if (validEPhysData(X)) {
#'     return(X)
#'   }
#' })

#' @rdname Get_Set_EPhysData
#' @import units
#' @exportMethod StimulusTrace
setGeneric(
  name = "StimulusTrace",
  def = function(X)
  {
    standardGeneric("StimulusTrace")
  }
)

#' @import units
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
