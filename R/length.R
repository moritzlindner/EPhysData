#' Length of an EPhysSet object
#'
#' This method returns the length of the data slot in an \link{EPhysSet} object.
#'
#' @param x An object of class \link{EPhysSet}.
#' @return An integer representing the number of recordings stored in the object.
#' @name length
#' @exportMethod length
setMethod("length",
          "EPhysSet",
          function(x) {
            length(x@Data)
          })
