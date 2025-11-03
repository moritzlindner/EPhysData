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
#'       \item \code{StimulusUnits} may be empty if \code{StimulusTrace} is empty. If \code{StimulusTrace} is present but \code{StimulusUnits} is empty, a message is emitted.
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
  if (anyDuplicated(ch_names)) msgs <- c(msgs, "`Channels` must be unique (no duplicates).")

  #Metadata checks

  if (!"RunUID" %in% names(md)) {
    msgs <- c(msgs, "`Metadata` must contain `RunUID` column.")
  } else {
    if (any(is.na(md$RunUID))) {
      msgs <- c(msgs, "Column RunUID contains missing values.")
    }
  }

  if (!"RecordingID" %in% names(md)) {
    msgs <- c(msgs, "`Metadata` must contain `RecordingID` column.")
  } else {
    if (any(is.na(md$RecordingID))) {
      msgs <- c(msgs, "Column RecordingID contains missing values.")
    }
  }

  # Channel checks
  ## In validEPhysContainer(), replace the current list-case block with:

  if (is.list(dat)) {
    # top-level: one element per run
    if (length(dat) != nrow(md)) {
      msgs <- c(msgs, "`Data` (list) must have one element per run: length(Data) == nrow(Metadata).")
    }

    if (length(dat) > 0L) {
      for (i in seq_along(dat)) {
        rowdat <- dat[[i]]
        if (!is.list(rowdat)) {
          msgs <- c(msgs, sprintf("Data[[%d]] must be a list (one element per channel).", i))
          break
        }
        if (length(rowdat) != length(ch_names)) {
          msgs <- c(
            msgs,
            sprintf("Data[[%d]] must have length %d (one per channel); found %d.",
                    i, length(ch_names), length(rowdat))
          )
          break
        }
      }
    }
  } else if (is.array(dat)) {
    dch <- dim(dat)[3]
    if (length(ch_names) != dch) {
      msgs <- c(msgs, "Length of Channels slot must match third dimension of Data array.")
    }
    # Also check channel dimnames if present
    if (!is.null(dimnames(dat)$channel) && !identical(dimnames(dat)$channel, ch_names)) {
      msgs <- c(msgs, "Channel dimnames of Data array must match Channels slot.")
    }
  }
  if (length(ch_names) == 0) {
    msgs <- c(msgs, "Channels slot is empty.")
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

  # time, stimulus & channels
  tt <- object@TimeTrace
  d <- diff(tt)
  if (is.numeric(tt) && (length(tt)!=0 )) {
    if (!isTRUE(all(d > 0)))  msgs <- c(msgs,"TimeTrace must be linearly increasing.")

    rtol <- 1e-8
    atol <- 1e-12
    step <- median(d, na.rm = TRUE)

    if (max(abs(d - step), na.rm = TRUE) > (atol + rtol * abs(step))) {
      msgs <- c(msgs, "`TimeTrace` must be (approximately) equally spaced.")
    }
  }

  ch <- object@Channels
  if (!is.character(ch)) {
    msgs <- c(msgs, "`Channels` must be a character vector.")
  }

  st <- object@StimulusTrace
  if (is.numeric(st) && (length(st)!=0 && length(st)!=length(tt))) {
    msgs <- c(msgs, "`StimulusTrace` must be numeric of length 0 or same as TimeTrace.")
  }

  # units validation
  for (nm in c("TimeUnits", "StimulusUnits")) {
    val <- slot(object, nm)
    if (nm == "StimulusUnits"){
      if(length(val)==0){
        if(length(object@StimulusTrace)!=0){
          message("No StimulusUnits provided.")
        }
        next
      }
    }
    ok <- tryCatch({
      # attempt to assign the unit to the number 1
      set_units(1, val,  mode="standard")
      TRUE
    }, error = function(e) FALSE)
    if (!ok) {
      msgs <- c(
        msgs,
        sprintf("`%s` must be a valid udunits (i.e. one of units::valid_udunits()$symbol)",
                nm)
      )
    }
  }

  if (length(msgs)) msgs else TRUE
}


#' EPhys* Classes — Electrophysiology container
#'
#' EPhysContainer: S4 class to hold per-run metadata, multi-channel data (as a list-of-lists or
#' a 3D array), timing information, optional stimulus trace, and units.
#'
#' @slot Metadata \code{data.frame}. One row per run/recording.
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
#' @slot StimulusUnits \code{character}. A valid udunits symbol; may be empty if \code{StimulusTrace} is empty.
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
#'       \item \code{StimulusUnits} may be empty if \code{StimulusTrace} is empty; if \code{StimulusTrace} is present but \code{StimulusUnits} is empty, a message is emitted.
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
