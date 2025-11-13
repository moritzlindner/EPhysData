#' Test whether an EPhys container has a valid stimulus trace
#'
#' This function checks if an object carries a stimulus trace that is
#' aligned with its time trace and free of missing values.
#'
#' For an [`EPhysContainer`] object this is `TRUE` if and only if:
#' \itemize{
#'   \item `StimulusTrace(X)` is not `NULL`,
#'   \item `length(StimulusTrace(X)) == length(TimeTrace(X))`, and
#'   \item `StimulusTrace(X)` contains no `NA` values.
#' }
#'
#' @param X An [`EPhysContainer`] object.
#'
#' @return
#' A single logical value:
#' \itemize{
#'   \item `TRUE` if a valid stimulus trace is present.
#'   \item `FALSE` otherwise.
#' }
#'
#' @examples
#' \dontrun{
#' HasStimulus(X)
#' }
#'
#' @export
#' @rdname HasStimulus
setGeneric("HasStimulus", function(X)
  standardGeneric("HasStimulus"))

#' @rdname HasStimulus
#' @aliases HasStimulus,EPhysContainer-method
#' @export
setMethod("HasStimulus", "EPhysContainer", function(X) {
  !is.null(StimulusTrace(X)) &&
    length(StimulusTrace(X)) == length(TimeTrace(X)) &&
    !any(is.na(StimulusTrace(X)))
})
