#' Get/Set methods for EPhysData objects
#'
#' These methods are used to get and set the non-data slots from \linkS4class{EPhysData} objects.
#' @param X An \code{EPhysData} object
#' @name Get_Set_EPhysData
#' @rdname Get_Set_EPhysData
#' @docType methods
#' @examples
#' # Create an EPhysData object with example data
#' myData <- matrix(rnorm(10), ncol = 1)
#' myTimeTrace <- seq(0, 1, length.out = 10)
#' myEPhysData <- newEPhysData(Data = myData, TimeTrace = myTimeTrace)
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
#' @details \code{Rejected}: These functions set or get the logical vector indicating which of the repeated measurements stored in an \linkS4class{EPhysData} object to exclude from averaging.
#' @exportMethod Rejected
setGeneric(
  name = "Rejected",
  def = function(X) {
    standardGeneric("Rejected")
  }
)
#' @noMd
setMethod("Rejected", signature = "EPhysData", function(X) {
  return(X@Rejected)
})

#' @rdname Get_Set_EPhysData
#' @exportMethod Rejected<-
setGeneric(
  name = "Rejected<-",
  def = function(X, value) {
    standardGeneric("Rejected<-")
  }
)
#' @noMd
setMethod("Rejected<-", signature = "EPhysData", function(X, value) {
  X@Rejected <- value
  if (validEPhysData(X)) {
    return(X)
  }
})

#' @rdname Get_Set_EPhysData
#' @details \code{filter.fx}: Set  a function for filtering each individual of the repeated measurements in the \linkS4class{EPhysData} object. Could be downsampling or noise removal, for instance.
#' @exportMethod FilterFunction
setGeneric(
  name = "FilterFunction",
  def = function(X) {
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
  def = function(X, value) {
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
#' @details \code{AverageFunction}: Set a function describing how averaging across repeated measurement should be performed in the \linkS4class{EPhysData} object. Usually, \link{mean}[base:mean] can be a good start.
#' @exportMethod AverageFunction
setGeneric(
  name = "AverageFunction",
  def = function(X) {
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
  def = function(X, value) {
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
#' @details \code{TimeTrace}: These functions set or get the time trace belonging to the measurements stored in an \linkS4class{EPhysData} object.
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

#' @rdname Get_Set_EPhysData
#' @exportMethod TimeTrace<-
setGeneric(
  name = "TimeTrace<-",
  def = function(X, value) {
    standardGeneric("TimeTrace<-")
  }
)
#' @noMd
setMethod("TimeTrace<-", signature = "EPhysData", function(X, value) {
  X@TimeTrace <- value
  if (validEPhysData(X)) {
    return(X)
  }
})
