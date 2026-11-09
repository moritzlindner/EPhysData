#' Validity function for EPhysEvents objects (internal)
#'
#' Ensures that:
#' \itemize{
#'   \item \code{Metadata} is a data.frame.
#'   \item \code{Data} is a list of length \code{nrow(Metadata)}.
#'   \item Each \code{Data[[i]]} is a \emph{named} list (one element per channel),
#'         with numeric vectors of timestamps.
#'   \item If \code{TimeTrace} is present, all timestamps lie within
#'         \code{range(TimeTrace)}.
#' }
#'
#' @param object An \code{EPhysEvents} instance.
#' @return \code{TRUE} if valid; otherwise a character vector of error messages.
#' @keywords internal
#' @noRd
#' @seealso \code{\link{validEPhysContainer}}
validEPhysEvents <- function(object) {
  msgs <- character()
  dat <- object@Data
  if (!is.list(dat)) return("`Data` must be a list of event runs.")

  tt <- object@TimeTrace
  check_range <- length(tt) && all(is.finite(tt))
  if (check_range) bounds <- range(tt)
  for (i in seq_along(dat)) {
    row <- dat[[i]]
    if (!is.list(row)) next
    for (j in seq_along(row)) {
      ts <- row[[j]]
      name <- if (is.null(names(row))) as.character(j) else shQuote(names(row)[j])
      context <- sprintf("Data[[%d]][[%s]]", i, name)
      if (!is.numeric(ts) || !is.null(dim(ts)) || any(!is.finite(ts))) {
        msgs <- c(msgs, paste0(context, " must be a finite numeric vector of timestamps."))
      } else if (check_range && any(ts < bounds[1L] | ts > bounds[2L])) {
        msgs <- c(msgs, sprintf("%s has timestamps outside the TimeTrace range [%g, %g].",
                                context, bounds[1L], bounds[2L]))
      }
    }
  }

  if (length(msgs)) msgs else TRUE
}



#' EPhysEvents — event-type data (subclass of EPhysContainer)
#'
#' EPhysEvents: Subclass of \code{EPhysContainer} for spike/event timestamps. See
#' \emph{Subclass: EPhysEvents} section on the \code{EPhysContainer} help page
#' for details on the required \code{Data} shape and additional validity checks.
#' @section Subclass: EPhysEvents
#' \code{EPhysEvents} specializes \code{EPhysContainer} for event/spike timestamps.
#' It inherits all slots and validity checks from \code{EPhysContainer} and adds:
#' \itemize{
#'   \item \strong{Data shape:} \code{Data} must be a \emph{list} of length
#'         \code{nrow(Metadata)}; each \code{Data[[i]]} is a \emph{named list}
#'         with one element per channel. Each channel entry is a numeric vector
#'         of event timestamps.
#'   \item \strong{Time range check:} If \code{TimeTrace} is present (non-empty),
#'         all event timestamps must lie within \code{range(TimeTrace)}.
#' }
#'
#' @rdname EPhysContainer-class
#' @exportClass EPhysEvents
setClass(
  "EPhysEvents",
  contains = "EPhysContainer",
  validity = validEPhysEvents
)

#' Constructor for EPhysEvents
#'
#' @param Data A \emph{list} of length \code{nrow(Metadata)}; each \code{Data[[i]]} is a \emph{named list} with one element per channel. Each channel entry is a numeric vector of event timestamps.
#' @inheritParams newEPhysContinuous
#'
#' @return A new \code{EPhysEvents} object.
#' @importFrom methods new
#' @rdname newEPhysEvents
#' @export
newEPhysEvents <- function(Data,
                               TimeTrace,
                               Metadata,
                               Channels,
                               Channel_Metadata,
                               StimulusTrace = numeric(0),
                               TimeUnits     = "s",
                               StimulusUnits = "",
                               Imported       = as.POSIXct(Sys.time()),
                               ExamInfo      = list(),
                               SubjectInfo   = list()) {
  out<-new("EPhysEvents",
           Metadata      = Metadata,
           Data          = Data,
           ExamInfo      = ExamInfo,
           SubjectInfo   = SubjectInfo,
           Imported      = Imported,
           TimeTrace     = TimeTrace,
           Channels      = Channels,
           Channel_Metadata = Channel_Metadata,
           StimulusTrace = StimulusTrace,
           TimeUnits     = TimeUnits,
           StimulusUnits = StimulusUnits)
  out
}


#' Show method for EPhysEvents
#'
#' Prints a concise summary: counts of events and channels, channel names (up to 10),
#' and the first rows of \code{Metadata}.
#'
#' @rdname EPhysContainer-class
#' @noRd
#' @export
#' @importFrom methods show
#' @importFrom utils head
setMethod("show", "EPhysEvents", function(object) {
  cat("<EPhysEvents> object\n")
  n_events <- nrow(object@Metadata)
  cat(" Number of events:", n_events, "\n")

  chan_names <- unique(unlist(lapply(object@Data, names)))
  cat(" Number of channels:", length(chan_names), "\n")
  cat(" Channels:", paste(head(chan_names, 10), collapse = ", "))
  if (length(chan_names) > 10) cat(", ...\n") else cat("\n")

  cat("\n First five rows of Metadata:\n")
  print(head(object@Metadata, 5))
})
