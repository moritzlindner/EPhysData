#' Dimensions of an EPhysData object
#'
#' This method returns the dimensions of the data slot in an \link{EPhysData} object.
#'
#' @param x An object of class \link{EPhysData}.
#' @return A vector of length 2 containing the number of rows and columns in the data slot.
#' @exportMethod dim
setMethod("dim",
          "EPhysData",
          function(x) {
            dim(x@Data)
          })
