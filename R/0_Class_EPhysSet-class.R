validEPhysSet <- function(object) {
  problems <- character()
  data <- object@Data
  md <- object@Metadata
  invalid <- which(!vapply(data, inherits, logical(1), "EPhysData"))
  if (length(invalid)) {
    problems <- c(problems, sprintf("`Data` entries must be EPhysData objects. Invalid positions: %s.",
                                    paste(invalid, collapse = ", ")))
  }
  if (nrow(md) != length(data)) {
    problems <- c(problems, sprintf("`Metadata` has %d rows. Expected %d to match `Data`.",
                                    nrow(md), length(data)))
  }
  duplicates <- which(duplicated(md))
  if (length(duplicates)) {
    problems <- c(problems, sprintf("`Metadata` rows must be unique. Duplicated rows: %s.",
                                    paste(duplicates, collapse = ", ")))
  }
  if (length(problems)) problems else TRUE
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
                     Data = list(),
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
  new("EPhysSet", Data = Data, Metadata = Metadata)
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
