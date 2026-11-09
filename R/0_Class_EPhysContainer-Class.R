#' Internal validator used by the \code{EPhysContainer} S4 class.
#'
#' @section Validation rules:
#' \itemize{
#'   \item \strong{Metadata and structure}
#'     \itemize{
#'       \item \code{Metadata} must be a \code{data.frame}.
#'       \item \code{Data} must either:
#'         \enumerate{
#'           \item be a \strong{list} of length \code{nrow(Metadata)}, where each element is a \emph{named} list with names equal to \code{Channels}; or
#'           \item be a \strong{3D array} with the channel dimension in the 3rd slot; if \code{dimnames(Data)$channel} is present it must exactly match \code{Channels}.
#'         }
#'       \item \code{ExamInfo} and \code{SubjectInfo} must be lists.
#'       \item \code{Imported} must be a single \code{POSIXct} timestamp.
#'     }
#'
#'   \item \strong{Channels and channel metadata}
#'     \itemize{
#'       \item \code{Channels} must be a non-empty character vector with unique values (no duplicates).
#'       \item \code{Channel_Metadata} must be a \code{data.frame}. If it is non-empty, it must have exactly \code{length(Channels)} rows (one row per channel).
#'       \item If \code{Data} is a list, every \code{Data[[i]]} must have the same channel names as \code{Channels}.
#'       \item If \code{Data} is an array, \code{length(Channels)} must equal \code{dim(Data)[3]} (number of channels).
#'     }
#'
#'   \item \strong{Time and stimulus traces}
#'     \itemize{
#'       \item \code{TimeTrace} (if non-empty) must be numeric, \emph{strictly increasing}, and \emph{(approximately) equally spaced}. Equidistance is checked relative to the median step with tolerances \code{rtol = 1e-8} and \code{atol = 1e-12}.
#'       \item \code{StimulusTrace} must be numeric and have length \code{0} or \code{length(TimeTrace)}.
#'     }
#'
#'   \item \strong{Units}
#'     \itemize{
#'       \item \code{TimeUnits} and \code{StimulusUnits} (when provided) must be valid udunits \emph{symbols} (e.g., \code{"s"}, \code{"ms"}), validated via \code{units::set_units()}.
#'       \item \code{StimulusUnits} may be empty when no stimulus unit was recorded.
#'     }
#' }
#'
#' @param object An \code{EPhysContainer} instance.
#' @return \code{TRUE} if valid; otherwise a character vector of error messages.
#' @keywords internal
#' @noRd
#' @importFrom units set_units
#' @importFrom methods slot
validEPhysContainer <- function(object) {
  msgs <- character()
  md  <- object@Metadata
  dat <- object@Data
  ch_names <- object@Channels
  ch_md <- object@Channel_Metadata

  if (!is.data.frame(md)) {
    msgs <- c(msgs, "`Metadata` must be a data.frame.")
  }
  if (!is.data.frame(ch_md)) {
    msgs <- c(msgs, "`Channel_Metadata` must be a data.frame.")
  }

  if (!is.character(ch_names)) msgs <- c(msgs, "`Channels` must be a character vector.")
  if (length(ch_names) == 0L)  msgs <- c(msgs, "`Channels` may not be empty.")
  if (anyNA(ch_names) || any(!nzchar(ch_names))) {
    msgs <- c(msgs, "`Channels` must contain non-empty, non-missing names.")
  }
  if (anyDuplicated(ch_names)) msgs <- c(msgs, "`Channels` must be unique (no duplicates).")

  #Metadata checks

  req_cols <- c("Experiment","Step","RecordingID","RunUID")
  missing_cols <- setdiff(req_cols, names(md))
  if (length(missing_cols)) {
    msgs <- c(msgs, sprintf("`Metadata` must contain columns: %s.",
                            paste(missing_cols, collapse = ", ")))
  } else {
    if (!is.factor(md$Experiment))
      msgs <- c(msgs, "Column `Experiment` must be a factor.")
    if (any(is.na(md$Experiment)))
      msgs <- c(msgs, "Column `Experiment` contains missing values.")

    if (!is.ordered(md$Step))
      msgs <- c(msgs, "Column `Step` must be an ordered factor.")
    if (any(is.na(md$Step)))
      msgs <- c(msgs, "Column `Step` contains missing values.")

    if (!is.integer(md$RecordingID))
      msgs <- c(msgs, "Column `RecordingID` must be of type integer.")
    if (anyNA(md$RecordingID))
      msgs <- c(msgs, "Column `RecordingID` contains missing values.")

    if (!is.integer(md$RunUID))
      msgs <- c(msgs, "Column `RunUID` must be of type integer.")
    if (anyNA(md$RunUID))
      msgs <- c(msgs, "Column `RunUID` contains missing values.")
    if (anyDuplicated(md$RunUID))
      msgs <- c(msgs, "Column `RunUID` must be unique (no duplicates).")
  }

  if (is.list(dat)) {
    if (length(dat) != nrow(md)) {
      msgs <- c(msgs, sprintf("`Data` has %d runs, but `Metadata` has %d rows.",
                              length(dat), nrow(md)))
    }
    for (i in seq_along(dat)) {
      run_id <- if (i <= length(md$RunUID)) as.character(md$RunUID[i]) else "unknown"
      context <- sprintf("Data[[%d]] (RunUID %s)", i, run_id)
      rowdat <- dat[[i]]
      if (!is.list(rowdat) || is.null(names(rowdat))) {
        msgs <- c(msgs, paste0(context, " must be a named list of channels."))
        next
      }
      run_channels <- names(rowdat)
      missing <- setdiff(ch_names, run_channels)
      extra <- setdiff(run_channels, ch_names)
      duplicated_channels <- unique(run_channels[duplicated(run_channels)])
      if (length(missing) || length(extra) || length(duplicated_channels)) {
        format_names <- function(x) if (length(x)) paste(shQuote(x), collapse = ", ") else "none"
        msgs <- c(msgs, sprintf("%s has invalid channel names. Missing: %s. Unexpected: %s. Duplicated: %s.",
                                context, format_names(missing), format_names(extra),
                                format_names(duplicated_channels)))
      }
    }
  } else if (is.array(dat) && length(dim(dat)) == 3L) {
    if (length(ch_names) != dim(dat)[3L]) {
      msgs <- c(msgs, sprintf("`Data` has %d channels, but `Channels` has %d names.",
                              dim(dat)[3L], length(ch_names)))
    }
    if (!is.null(dimnames(dat)$channel) && !identical(dimnames(dat)$channel, ch_names)) {
      msgs <- c(msgs, "Channel dimnames of Data array must match Channels slot.")
    }
  } else {
    msgs <- c(msgs, "`Data` must be a list of runs or a 3D array [time x run x channel].")
  }
  if (nrow(ch_md)!=0){
    if (length(ch_names) != nrow(ch_md)) {
      msgs <- c(msgs, "Length of channel metadata does not match the number of Channels")
    }
  }

  if (!is.list(object@ExamInfo)) {
    msgs <- c(msgs, "`ExamInfo` must be a list.")
  }
  if (!is.list(object@SubjectInfo)) {
    msgs <- c(msgs, "`SubjectInfo` must be a list.")
  }
  if (!(inherits(object@Imported, "POSIXct") && length(object@Imported)==1L)) {
    msgs <- c(msgs, "`Imported` must be a single POSIXct timestamp.")
  }

  tt <- object@TimeTrace
  if (any(!is.finite(tt))) {
    msgs <- c(msgs, "`TimeTrace` must contain only finite values.")
  } else if (length(tt) > 1L) {
    d <- diff(tt)
    if (any(d <= 0)) msgs <- c(msgs, "`TimeTrace` must be strictly increasing.")
    step <- median(d)
    if (max(abs(d - step)) > 1e-12 + 1e-8 * abs(step)) {
      msgs <- c(msgs, "`TimeTrace` must be approximately equally spaced.")
    }
  }

  st <- object@StimulusTrace
  if (length(st) && length(st) != length(tt)) {
    msgs <- c(msgs, sprintf("`StimulusTrace` has %d samples. Expected 0 or %d to match `TimeTrace`.",
                            length(st), length(tt)))
  }

  for (nm in c("TimeUnits", "StimulusUnits")) {
    val <- slot(object, nm)
    if (nm == "StimulusUnits" && (!length(val) || identical(val, ""))) next
    if (length(val) != 1L || is.na(val) || !nzchar(val)) {
      msgs <- c(msgs, sprintf("`%s` must be a single unit string.", nm))
      next
    }
    problem <- tryCatch({
      value <- set_units(1, val, mode = "standard")
      if (nm == "TimeUnits") set_units(value, "s", mode = "standard")
      NULL
    }, error = conditionMessage)
    if (!is.null(problem)) {
      msgs <- c(msgs, sprintf("`%s` value %s is invalid. %s", nm, shQuote(val), problem))
    }
  }

  if (length(msgs)) msgs else TRUE
}


#' EPhys* Classes — Electrophysiology container
#'
#' EPhysContainer: S4 class to hold per-run metadata, multi-channel data (as a list-of-lists or
#' a 3D array), timing information, optional stimulus trace, and units.
#'
#' @slot Metadata \code{data.frame}. One row per run. Required columns:
#' \itemize{
#'   \item \strong{Experiment}: *factor*. Factorized identifier for each Experiment. All recordings/runs acquired on the basis of an identical acquisition protocol can belong to the same Experiment.
#'   \item \strong{Step}: *ordered*. Ordered identifier for each step. One Step corresponds to a single protocol stage within an Experiment; values are compared in their defined order to reflect the progression of the acquisition.
#'   \item \strong{RecordingID}: *integer*. Identifier for each Recording. One Recording may have multiple runs.
#'   \item \strong{RunUID}: *integer*. Unique run ID.
#' }
#' Any number of additional columns is allowed.
#' @slot Data Either:
#' \enumerate{
#'   \item \strong{list}: A \emph{list} of length \code{nrow(Metadata)}; each \code{Data[[i]]} is a \emph{named list} with one element per channel. Each channel entry is a numeric vector of event timestamps; or
#'   \item \strong{3D array}: A 3D \strong{numeric} array
#'         with dimensions \code{[time × trial × channel]}.; if \code{dimnames(.)$channel}
#'         exists it must match \code{Channels}.
#' }
#' @slot ExamInfo \code{list}. Free-form exam-level information.
#' @slot SubjectInfo \code{list}. Free-form subject-level information.
#' @slot Imported \code{POSIXct} (length 1). Import timestamp.
#' @slot TimeTrace \code{numeric}. Sample times; must be strictly increasing and approximately equally spaced when non-empty.
#' @slot Channels \code{character}. Non-empty, unique channel names.
#' @slot Channel_Metadata \code{data.frame}. Either empty or one row per channel (i.e., \code{nrow(Channel_Metadata) == length(Channels)}).
#' @slot StimulusTrace \code{numeric}. Length 0 or the same length as \code{TimeTrace}.
#' @slot TimeUnits \code{character}. A valid udunits \emph{symbol} (e.g., \code{"s"}, \code{"ms"}).
#' @slot StimulusUnits \code{character}. A valid udunits symbol; may be empty when no stimulus unit was recorded.
#'
#' @section Validity:
#' \itemize{
#'   \item \strong{Metadata and structure}
#'     \itemize{
#'       \item \code{Metadata} must be a \code{data.frame}.
#'       \item \code{Data} must either:
#'         \enumerate{
#'           \item be a \strong{list} of length \code{nrow(Metadata)}, where each element is a \emph{named} list with names equal to \code{Channels}; or
#'           \item be a \strong{3D array} with channel dimension in the 3rd slot; if \code{dimnames(Data)$channel} is present it must exactly match \code{Channels}.
#'         }
#'       \item \code{ExamInfo} and \code{SubjectInfo} must be lists.
#'       \item \code{Imported} must be a single \code{POSIXct} timestamp.
#'     }
#'   \item \strong{Channels and channel metadata}
#'     \itemize{
#'       \item \code{Channels} must be a non-empty character vector with unique values.
#'       \item \code{Channel_Metadata} must be a \code{data.frame}. If non-empty, it must have exactly \code{length(Channels)} rows.
#'       \item If \code{Data} is a list, every \code{Data[[i]]} must have the same channel names as \code{Channels}.
#'       \item If \code{Data} is an array, \code{length(Channels)} must equal \code{dim(Data)[3]} (number of channels).
#'     }
#'   \item \strong{Time and stimulus traces}
#'     \itemize{
#'       \item \code{TimeTrace} (if non-empty) must be numeric, strictly increasing, and (approximately) equally spaced.
#'       \item \code{StimulusTrace} must be numeric and have length 0 or \code{length(TimeTrace)}.
#'     }
#'   \item \strong{Units}
#'     \itemize{
#'       \item \code{TimeUnits} and \code{StimulusUnits} (when provided) must be valid udunits \emph{symbols}, validated via \code{units::set_units()}.
#'       \item \code{StimulusUnits} may be empty when no stimulus unit was recorded.
#'     }
#' }
#'
#' @aliases EPhysContainer-class EPhysContainer EPhysEvents-class EPhysEvents
#' @seealso \code{\link{validEPhysContainer}}, \code{\link{validEPhysEvents}}
#' @exportClass EPhysContainer
#' @name EPhysContainer-class
#' @docType class
setClass("EPhysContainer",
         slots = c(
           Metadata       = "data.frame",
           Data           = "ANY",
           ExamInfo       = "list",
           SubjectInfo    = "list",
           Imported       = "POSIXct",
           TimeTrace      = "numeric",
           Channels       = "character",
           Channel_Metadata = "data.frame",
           StimulusTrace  = "numeric",
           TimeUnits      = "character",
           StimulusUnits  = "character"
         ),
         validity = validEPhysContainer
)
