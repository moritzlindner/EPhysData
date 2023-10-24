#' Get or modify the metadata slot
#'
#' These functions are used to modify (\code{AddMetadata}, \code{Metadata<-}) metadata of or retrieve metadata from an \code{EPhysSet} object.
#'
#' @param X An \code{EPhysSet} object.
#' @return Either an updated \code{EPhysSet} object or it's metadata.
#' @examples
#' # Create example data
#' Data <- data.frame(rep1 = c(0.5, 0.52, 0.48, 0.49),
#'                    rep2 = c(0.55, 0.57, 0.54, 0.56),
#'                    rep3 = c(0.48, 0.47, 0.46, 0.48))
#' TimeTrace <- c(1, 2, 3, 4)
#' Unit <- "mV"
#' TimeUnit <- "s"
#' data1 <- newEPhysData(Data = Data, TimeTrace = TimeTrace, Unit = Unit, TimeUnit = TimeUnit)
#'
#' # Create metadata and an EPhysSet object
#' metadata <- data.frame(StepID = c("A1", "A2", "A3", "A4"))
#' ephysSet <- new("EPhysSet", Data = list(data1, data1, data1, data1), Metadata = metadata)
#'
#' # Add a new column to the metadata
#' ephysSet <- AddMetadata(ephysSet, "Condition", c("Control", "Treatment", "Control", "Treatment"))
#'
#' # Replace the metadata
#' newMetadata <- data.frame(StepID = c("B1", "B2", "B3", "B4"))
#' Metadata(ephysSet) <- newMetadata
#'
#' # Retrieve metadata
#' retrievedMetadata <- Metadata(ephysSet)
#' retrievedMetadata
#' @name Metadata-method
#' @exportMethod Metadata
setGeneric(
  name = "Metadata",
  def = function(X) {
    standardGeneric("Metadata")
  }
)

#' @noRd
setMethod("Metadata", "EPhysSet", function(X) {
  return(X@Metadata)
})

#' @describeIn Metadata-method AddMetadata
#' @param columnName Name of the new column to be added.
#' @param columnData Data for the new column.
#' @exportMethod AddMetadata
setGeneric(
  name = "AddMetadata",
  def = function(X, columnName, columnData) {
    standardGeneric("AddMetadata")
  }
)

#' @importFrom methods validObject
#' @noRd
setMethod("AddMetadata", "EPhysSet", function(X, columnName, columnData) {
  if (length(columnData) != nrow(X@Metadata)) {
    stop("Column data length should match the number of rows in Metadata.")
  }
  X@Metadata[[columnName]] <- columnData
  if (validObject(X)) {
    return(X)
  }
})

#' @param value New metadata to replace the existing metadata.
#' @describeIn Metadata-method Metadata<-
#' @exportMethod Metadata<-
setGeneric(
  name = "Metadata<-",
  def = function(X, value) {
    standardGeneric("Metadata<-")
  }
)

#' @importFrom methods validObject
#' @noRd
setMethod("Metadata<-", "EPhysSet", function(X, value) {
  if (!is.data.frame(value)) {
    stop("New metadata must be a data frame.")
  }
  if (nrow(value) != nrow(X@Metadata)) {
    stop("New metadata must have the same number of rows as existing metadata.")
  }
  X@Metadata <- value
  if (validObject(X)) {
    return(X)
  }
})
