#' Get or modify Metadata on EPhysContainer objects
#'
#' Retrieve or modify the \code{Metadata} slot on \link{EPhysSet} and
#' \link{EPhysContainer}. Use:
#' \itemize{
#'   \item \code{Metadata(X)} to get the data.frame,
#'   \item \code{Metadata(X) <- value} to replace it entirely,
#'   \item \code{AddMetadata(X, name, data)} to add/replace a single column.
#' }
#'
#' @param X An \link{EPhysContainer} object (or subclass such as \link{EPhysSet}).
#' @return For \code{Metadata()}: the metadata \code{data.frame}.
#'   For \code{Metadata<-} and \code{AddMetadata}: the updated object.
#'
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
#'
#' @name Metadata
#' @rdname Metadata-methods
#' @docType methods
#' @importFrom methods validObject
NULL

# ---- Generics (define once) ----
#' @rdname Metadata-methods
#' @export
setGeneric("Metadata", function(X) standardGeneric("Metadata"))

#' @rdname Metadata-methods
#' @export
setGeneric("Metadata<-", function(X, value) standardGeneric("Metadata<-"))

#' @rdname Metadata-methods
#' @export
setGeneric("AddMetadata", function(X, columnName, columnData) standardGeneric("AddMetadata"))

# ---- Small internal helpers to avoid repeating logic ----
.meta_get <- function(X) X@Metadata
.meta_set <- function(X, value) {
  if (!is.data.frame(value)) stop("New metadata must be a data.frame.")
  if (!is.null(X@Metadata) && nrow(value) != nrow(X@Metadata))
    stop("Row count must match existing Metadata.")
  X@Metadata <- value
  if (validObject(X)) X
}
.meta_add <- function(X, columnName, columnData) {
  if (!is.data.frame(X@Metadata)) stop("Metadata(X) is not a data.frame.")
  if (!is.character(columnName) || length(columnName) != 1L || !nzchar(columnName))
    stop("`columnName` must be a non-empty single string.")
  if (length(columnData) != nrow(X@Metadata))
    stop("`columnData` length must equal nrow(Metadata(X)).")
  X@Metadata[[columnName]] <- columnData
  if (validObject(X)) X
}

# ---- Methods for EPhysSet ----
#' @describeIn Metadata-methods Get metadata (EPhysSet)
#' @export
setMethod("Metadata", "EPhysSet", function(X) .meta_get(X))

#' @param value New metadata data.frame (row count must match).
#' @describeIn Metadata-methods Replace metadata (EPhysSet)
#' @export
setMethod("Metadata<-", "EPhysSet", function(X, value) .meta_set(X, value))

#' @param columnName Name of column to add/replace.
#' @param columnData Vector to insert.
#' @describeIn Metadata-methods Add/replace one column (EPhysSet)
#' @export
setMethod("AddMetadata", "EPhysSet", function(X, columnName, columnData)
  .meta_add(X, columnName, columnData))

# ---- Methods for EPhysContainer ----
#' @describeIn Metadata-methods Get metadata (EPhysContainer)
#' @export
setMethod("Metadata", "EPhysContainer", function(X) .meta_get(X))

#' @describeIn Metadata-methods Replace metadata (EPhysContainer)
#' @export
setMethod("Metadata<-", "EPhysContainer", function(X, value) .meta_set(X, value))

#' @describeIn Metadata-methods Add/replace one column (EPhysContainer)
#' @export
setMethod("AddMetadata", "EPhysContainer", function(X, columnName, columnData)
  .meta_add(X, columnName, columnData))
