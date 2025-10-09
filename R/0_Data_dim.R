#' Dimensions for EPhys* objects
#'
#' Returns the matrix-like dimensions of the `@Data` slot.
#'
#' Supported classes:
#' \itemize{
#'   \item \link{EPhysData}
#'   \item \link{EPhysContinuous}
#' }
#'
#' @param x An \link{EPhysData} or \link{EPhysContinuous} object.
#' @return An integer vector of length 2: number of rows and columns in `x@Data`.
#'
#' @examples
#' \dontrun{
#' dim(ephys_data)        # for EPhysData
#' dim(ephys_continuous)  # for EPhysContinuous
#' }
#'
#' @name dim
#' @rdname dim-ephys-methods
#' @docType methods
NULL

#' @describeIn dim-ephys-methods Dimensions of an \link{EPhysData} object.
#' @export
setMethod("dim", "EPhysData", function(x) {
  dim(x@Data)
})

#' @describeIn dim-ephys-methods Dimensions of an \link{EPhysContinuous} object.
#' @export
setMethod("dim", "EPhysContinuous", function(x) {
  dim(x@Data)
})
