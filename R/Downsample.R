#' Downsample EPhysData or EPhysSet Object
#'
#' This method downsamples an \link{EPhysData} or an \link{EPhysSet} object by sampling the time trace and applying a filter function.
#'
#' @inheritParams GetData
#' @inheritParams base::pretty
#' @return An object of class \link{EPhysData} or \link{EPhysSet} after downsampling
#'
#' @details The \code{Downsample} function samples the time trace of the input object and applies a filter function to the data. The sampling is based on the \link[base:pretty]{bsae::pretty} function, which creates a sequence of about 250 equally spaced, nice values that cover the range of the input time trace. The filter function applied is a simple identity function, which does not modify the data.
#'
#' @examples
#' # For an EPhysData object
#' myEPhysData <- makeExampleEPhysData(sample_points=500, replicate_count = 3)
#' myEPhysData
#' downsampled_myEPhysData <- Downsample(myEPhysData)
#' downsampled_myEPhysData
#'
#' # For an EPhysSet object
#' myEPhysSet <- makeExampleEPhysSet(sample_points=750, nsets=10)
#' myEPhysSet
#' myEPhysSet[[1]]
#' downsampled_myEPhysSet <- Downsample(myEPhysSet)
#' downsampled_myEPhysSet
#' downsampled_myEPhysSet[[1]]
#'
#' @export
#' @importFrom units as_units deparse_unit
#' @docType methods
#' @rdname Downsample-methods
setGeneric("Downsample", function(X, n = 250) standardGeneric("Downsample"))

#' @rdname Downsample-methods
#' @aliases Downsample,EPhysData-method
setMethod("Downsample",
          "EPhysData",
          function(X, n = 250) {
            Xt <- TimeTrace(X)
            sample <- Xt %in% pretty(Xt, n)
            unitbuffer <- deparse_unit(X@Data)
            X@Data <- as_units(apply(X@Data, 2, FilterFunction(X))[sample, 1:dim(X)[2], drop = FALSE], unitbuffer)
            X@TimeTrace <- Xt[sample]
            FilterFunction(X) <- function(x) x
            message("The filter function is reset upon downsampling.")
            return(X)
          })

#' @rdname Downsample-methods
#' @aliases Downsample,EPhysSet-method
setMethod("Downsample",
          "EPhysSet",
          function(X, n = 250) {
            # Iterate over each EPhysData object in the EPhysSet
            X@Data <- lapply(X@Data, function(data) {
              Downsample(data, n) # Recursively call Downsample for EPhysData
            })
            return(X)
          })
