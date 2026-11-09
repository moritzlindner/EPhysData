#' Downsample EPhysData or EPhysSet Object
#'
#' This method downsamples an \link{EPhysData} or an \link{EPhysSet} object by sampling the time trace and applying a filter function.
#'
#' @param X An \link{EPhysData} or \link{EPhysSet} object.
#' @param n Maximum number of samples to retain per recording.
#' @return An object of class \link{EPhysData} or \link{EPhysSet} after downsampling
#'
#' @details The stored filter is applied before selecting approximately equally
#' spaced sample indices. Data, time and any stimulus trace use the same indices.
#' The stored filter is then replaced by the identity function.
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
            if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
                n < 1 || n != floor(n)) {
              .ephys_abort("Downsample() requires n to be a positive whole number.")
            }
            validObject(X)
            samples <- nrow(X@Data)
            if (!samples) .ephys_abort("Downsample(EPhysData) found no input samples.")
            indices <- round(seq.int(1, samples, length.out = min(n, samples)))
            data <- .ephys_filter_data(X, FilterFunction(X), "Downsample(EPhysData)")
            X@Data <- data[indices, , drop = FALSE]
            X@TimeTrace <- X@TimeTrace[indices]
            if (length(X@StimulusTrace)) X@StimulusTrace <- X@StimulusTrace[indices]
            X@filter.fx <- .ephys_identity
            validObject(X)
            X
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
