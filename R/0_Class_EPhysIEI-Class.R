#’ Validity function for EPhysIEI objects (matrix‐based)
#’
#’ Ensures that, in addition to \code{validEPhysContainer}:
#’ each \code{Data[[i]][[ch]]} is a numeric matrix with exactly two columns
#’ named “Time” and “IEI”.
#’
#’ @param object An \code{EPhysIEI} instance
#’ @return \code{TRUE} if valid; otherwise a character vector of error messages
#’ @keywords internal
#’ @noRd
validEPhysIEI <- function(object) {
  msgs <- validEPhysContainer(object)
  if (isTRUE(msgs)) msgs <- character()

  dat <- object@Data
  for (i in seq_along(dat)) {
    rowdat   <- dat[[i]]
    prefix_i <- paste0("Data[[", i, "]]")
    if (!is.list(rowdat) || is.null(names(rowdat))) {
      msgs <- c(msgs, paste0(prefix_i, " must be a named list of channels."))
      next
    }
    for (ch in object@Channels) {
      mat    <- rowdat[[ch]]
      prefix <- paste0(prefix_i, "[[\"", ch, "\"]]")
      if (!is.matrix(mat)) {
        msgs <- c(msgs, paste0(prefix, " must be a matrix."))
        next
      }
      if (ncol(mat) != 2) {
        msgs <- c(msgs, paste0(prefix, " must have exactly 2 columns."))
      }
      cn <- colnames(mat)
      if (is.null(cn) || !identical(cn, c("Time", "IEI"))) {
        msgs <- c(msgs, paste0(prefix, " must have column names c(\"Time\",\"IEI\")."))
      }
      if (!is.numeric(mat[, "Time"]) || !is.numeric(mat[, "IEI"])) {
        msgs <- c(msgs, paste0(prefix, " both columns must be numeric."))
      }
    }
  }

  if (length(msgs)) msgs else TRUE
}

#' EPhysIEI-class
#'
#' Subclass of \code{EPhysContainer} for inter‐event‐interval data. Each
#' \code{Data[[i]]} is a named list of channels, each a two‐column data.frame
#' with \code{Time} and \code{IEI}.
#'
#' @name EPhysIEI-class
#' @docType class
#' @keywords classes
#' @exportClass EPhysIEI
setClass(
  "EPhysIEI",
  contains = "EPhysContainer",
  validity = validEPhysIEI
)


#’ Show method for EPhysIEI objects
#’
#’ @param object An \code{EPhysIEI} object.
#’ @export
setMethod("show", "EPhysIEI", function(object) {
  cat("<EPhysIEI>\n")
  ntrials <- nrow(object@Metadata)
  cat(" Number of trials:", ntrials, "\n")

  # gather channel names
  chan_names <- unique(unlist(lapply(object@Data, names)))
  nch <- length(chan_names)
  cat(" Number of channels:", nch, "\n")
  cat(" Channels:", paste(head(chan_names, 10), collapse = ", "))
  if (nch > 10) cat(", ...\n") else cat("\n")

  # show a preview of the first trail × first channel
  first_ch <- chan_names[1]
  cat("\n First few rows of IEI for trail 1, channel", first_ch, ":\n")
  mat <- object@Data[[1]][[first_ch]]
  print(head(mat))
})
