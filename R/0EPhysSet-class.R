validEPhysSet <- function(object) {
  if (!is.list(object@Data)) {
    stop("Data must be a list.")
  }

  if (!all(unlist(lapply(object@Data, function(x) {
    inherits(x, "EPhysData")
  })))) {
    stop("Data must be a list of 'EPhysData' objects.")
  }

  # Check if Data and MetaData are of the same length
  if (nrow(Metadata(object)) != length(object@Data)) {
    stop("Metadata must have one row per each list entry in data.")
  }

  if(nrow(Metadata(object))!=nrow(unique(Metadata(object)))){
    stop("Metadata are not inambiuously defining the individual data in the dataset. I.e. 'metadata' rows are not unique.")
  }

  TRUE
}

#' EPhysSet Class
#'
#' A class representing a set of Electrophysiological recordings (e.g. a time series or data from the same subject obtained in response to different stimuly) together with defining metadata.
#'
#' @slot Data Data A list of \linkS4class{EPhysData} objects.
#' @slot Metadata  A data frame containing metadata information associated with the data, each row corresponds to one list item. Rows must be unique
#' @name EPhysSet-class
#' @exportClass EPhysSet
EPhysSet<-setClass("EPhysSet",
                   slots = list(
                     Data = "list",
                     Metadata = "data.frame"
                   ),
                   prototype = list(
                     Data = list(NULL),
                     Metadata = data.frame(
                       stringsAsFactors = FALSE
                     )
                   ),
                   validity = validEPhysSet
)

#' Create an instance of the EPhysSet class
#'
#' @description This function creates an instance of the \code{EPhysSet} class
#' with the specified data and attributes.
#'
#' @param Data A list of \link[=EPhysData]{EPhysData}objects.
#' @param Metadata A data frame containing metadata information associated with the data, each row corresponds to one list item.
#' \describe{
#'   \item{Step}{A character vector containing the steps associated with the data.}
#'   \item{Eye}{A character vector containing the possible values "RE" (right eye) and "LE" (left eye).}
#'   \item{Channel}{A character vector containing the unique names of the third level of \code{Data}.}
#' }
#'
#' @return An object of class \code{EPhysSet}.
#' @examples
#' # Create example data

#' # Create metadata and an EPhysSet object
#' metadata <- data.frame(StepID = c("A1", "A2", "A3", "A4"))
#' ephysSet <-
#'   newEPhysSet(
#'     Data = list(
#'       makeExampleEPhysData(),
#'       makeExampleEPhysData(),
#'       makeExampleEPhysData(),
#'       makeExampleEPhysData()
#'     ),
#'     Metadata = metadata
#'  )
#' ephysSet
#' @seealso \code{\link{EPhysSet-class}}
#' @importFrom methods new validObject
#' @export
newEPhysSet <- function(Data, Metadata) {
  # Call the default constructor
  obj <- new("EPhysSet")

  # Set the values for the slots
  obj@Data <- Data
  obj@Metadata <- Metadata

  # Call the validity method to check if the object is valid
  if(validObject(obj)){
    return(obj)
  }

}

#' @importFrom utils object.size
#' @noMd
setMethod("show",
          "EPhysSet",
          function(object) {

            if(is.null(names(object@Data))){
              items<-length(object@Data)
            }else{
              items<-(names(object@Data))
            }
            cat("An object of class EPhysSet \n")
            cat("Data Items:",items,sep="\n\t")
            cat("Metadata:",colnames(Metadata(object)),sep="\n\t")
            cat("Size:", format(object.size(object),"auto"))
          })
