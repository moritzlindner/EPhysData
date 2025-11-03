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
  msgs <- validEPhysContainer(object)
  if (isTRUE(msgs)) msgs <- character()

  md  <- object@Metadata
  dat <- object@Data
  ch  <- object@Channels

  if (!is.data.frame(md)) {
    msgs <- c(msgs, "'Metadata' must be a data.frame.")
  }
  if (!is.list(dat) || length(dat) != nrow(md)) {
    msgs <- c(msgs, "'Data' must be a list of length equal to nrow(Metadata).")
  } else {
    for (i in seq_along(dat)) {
      rowdat <- dat[[i]]
      prefix <- paste0("Data[[", i, "]]")

      # Must be a list
      if (!is.list(rowdat)) {
        msgs <- c(msgs, paste0(prefix, " must be a list (one element per channel)."))
        next
      }

      # Must have exactly one entry per channel
      if (length(rowdat) != length(ch)) {
        msgs <- c(msgs, sprintf(
          "%s must have length %d (one per channel), found %d.",
          prefix, length(ch), length(rowdat)
        ))
      }

      # Each channel entry must be numeric (possibly length 0)
      for (ch_idx in seq_along(ch)) {
        ts <- rowdat[[ch_idx]]
        if (!is.numeric(ts)) {
          msgs <- c(msgs, sprintf("%s[['%s']] must be a numeric vector of timestamps.", prefix, ch[[ch_idx]]))
        }
      }
    }
  }

  ## Time range check (only if TimeTrace present)
  tt <- object@TimeTrace
  if (length(tt) > 0L && is.list(dat) && length(dat) == nrow(md)) {
    tmin <- min(tt, na.rm = TRUE)
    tmax <- max(tt, na.rm = TRUE)
    for (i in seq_along(dat)) {
      rowdat <- dat[[i]]
      if (!is.list(rowdat) || is.null(names(rowdat))) next
      for (ch_name in names(rowdat)) {
        ts <- rowdat[[ch_name]]
        if (length(ts) == 0L) next
        if (!is.numeric(ts)) next  # already flagged above
        out <- which(ts < tmin | ts > tmax)
        if (length(out) > 0L) {
          msgs <- c(msgs, sprintf(
            "Found %d event(s) outside TimeTrace range [%g, %g] in Data[[%d]][['%s']].",
            length(out), tmin, tmax, i, ch_name
          ))
        }
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
  validEPhysEvents(out)
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
