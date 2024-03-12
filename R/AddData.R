#' Add Data to an EPhysSet Object
#'
#' Adds a new \link{EPhysData} object to the \code{Data} slot of an \code{EPhysSet} object and the corresponding metadata to the \code{Metadata} slot.
#'
#' @param X An \code{EPhysSet} object.
#' @param newData An \linkS4class{EPhysData} object to be added to the \code{Data} slot.
#' @param newMetadata A data frame containing metadata information associated with the new data.
#'
#' @return An updated \code{EPhysSet} object with the new data and metadata added.
#' @importFrom methods validObject
#'
#'
#' @examples
#' # Create an example EPhysSet object
#' ephys_set <- makeExampleEPhysSet()
#'
#' # Create new EPhysData and metadata
#' new_data <- makeExampleEPhysData()
#' new_metadata <- data.frame(StepID = "B3")
#'
#' # Add new data to the EPhysSet
#' updated_ephys_set <- AddData(ephys_set, newData = new_data, newMetadata = new_metadata)
#' updated_ephys_set
#' @export
#' @docType methods
#' @rdname AddData-methods
setGeneric(
  name = "AddData",
  def = function(X, newData, newMetadata) {
    standardGeneric("AddData")
  }
)

#' @rdname AddData-methods
#' @aliases AddData,EPhysSet,ANY-method
setMethod("AddData",
          "EPhysSet",
          function(X, newData, newMetadata) {
            # Check if the provided data and metadata have consistent dimensions
            if(!("EPhysData" %in% class(newData))){
              stop("'newData' must be of class 'EPhysData'.")
            }
            if (nrow(newMetadata) != 1) {
              stop("newMetadata must have exactly one row.")
            }
            if((length(newMetadata) != ncol(Metadata(X)))){
              stop("Number of elements in newMetadata must match the number of columns of Metadata in 'X'.")
            }

            # Add new data and metadata
            X@Data[[nrow(Metadata(X)) + 1]] <- newData
            X@Metadata <- rbind(Metadata(X), newMetadata)

            if(validObject(X)){
              return(X)
            }
          })
