#' Validity for EPhysContinuous objects (internal)
#'
#' Additional checks beyond \code{EPhysContainer}:
#' \itemize{
#'   \item \code{Data} is a 3D numeric array \code{[time × trial × channel]}.
#'   \item \code{length(TimeTrace) == dim(Data)[1]}.
#'   \item \code{nrow(Metadata) == dim(Data)[2]}.
#'   \item \code{length(Channels) == dim(Data)[3]}.
#'   \item \code{Metadata} contains \code{RunUID}; if \code{dimnames(Data)$trial}
#'         is present, it must match \code{Metadata$RunUID}.
#' }
#'
#' @param object An \code{EPhysContinuous} instance.
#' @return \code{TRUE} if valid; otherwise a character vector of error messages.
#' @keywords internal
#' @noRd
#' @rdname EPhysContainer-class
validEPhysContinuous <- function(object) {
  msgs <- validEPhysContainer(object)
  if (isTRUE(msgs)) msgs <- character()

  d   <- object@Data
  tt  <- object@TimeTrace
  md  <- object@Metadata
  ch  <- object@Channels

  if (!(is.array(d) && length(dim(d))==3 && is.numeric(d))) {
    msgs <- c(msgs, "`Data` must be a 3D numeric array [time × trial × channel].")
  } else {
    if (length(tt) != dim(d)[1]) {
      msgs <- c(msgs, "`length(TimeTrace)` must equal `dim(Data)[1]`.")
    }
    if (nrow(md) != dim(d)[2]) {
      msgs <- c(msgs, "`nrow(Metadata)` must equal `dim(Data)[2]`.")
    }
    if (length(ch) != dim(d)[3]) {
      msgs <- c(msgs, "`length(Channels)` must equal `dim(Data)[3]`.")
    }
    if (!"RunUID" %in% names(md)) {
      msgs <- c(msgs, "`Metadata` must contain `RunUID` column.")
    } else {
      run_uids <- as.character(md$RunUID)
      data_uids  <- dimnames(d)$trial
      if (!is.null(data_uids) && !identical(run_uids, data_uids)) {
        msgs <- c(msgs, "`Metadata$RunUID` must match `dimnames(Data)$trial`.")
      }
    }
  }

  if (length(msgs)) msgs else TRUE
}

#' EPhysContinuous — continuous time-series (subclass of EPhysContainer)
#'
#' EPhysContinuous: Subclass of \code{EPhysContainer} for continuous multi-channel time series.
#' See the \emph{Subclass: EPhysContinuous} section on the
#' \code{EPhysContainer} help page for validity details.
#' @section Subclass: EPhysContinuous
#' \code{EPhysContinuous} specializes \code{EPhysContainer} for continuous multi-channel
#' time-series data (e.g., binned spike rates or LFP). In addition to the base checks,
#' its validity enforces:
#' \itemize{
#'   \item \strong{Data shape:} \code{Data} is a 3D \strong{numeric} array
#'         with dimensions \code{[time × trial × channel]}.
#'   \item \strong{Dimension consistency:}
#'         \code{length(TimeTrace) == dim(Data)[1]},
#'         \code{nrow(Metadata) == dim(Data)[2]},
#'         \code{length(Channels) == dim(Data)[3]}.
#'   \item \strong{Trial identity:} \code{Metadata} must contain \code{RunUID}.
#'         If \code{dimnames(Data)$trial} is present, it must exactly match
#'         \code{as.character(Metadata$RunUID)}.
#' }
#'
#' @rdname EPhysContainer-class
#' @exportClass EPhysContinuous
setClass("EPhysContinuous",
         contains = "EPhysContainer",
         validity = validEPhysContinuous
)

#' Constructor for EPhys* Objects
#'
#' @param Data             A 3D numeric array \code{[time × trial × channel]}.
#' @param TimeTrace        Numeric vector of bin-center times (length \code{dim(Data)[1]}).
#' @param Metadata         \code{data.frame} with \code{nrow(.) == dim(Data)[2]} and column \code{RunUID}.
#' @param Channels         Character vector of channel names (length \code{dim(Data)[3]}).
#' @param Channel_Metadata \code{data.frame} with per-channel metadata (rows = \code{length(Channels)}).
#' @param StimulusTrace    Numeric vector length 0 or equal to \code{TimeTrace}.
#' @param TimeUnits        Single valid udunits symbol for time (e.g., \code{"s"}).
#' @param StimulusUnits    Single valid udunits symbol for stimulus (may be empty).
#' @param Imported         \code{POSIXct} import timestamp (default: current time).
#' @param ExamInfo         List of exam-level info.
#' @param SubjectInfo      List of subject-level info.
#'
#' @return A new \code{EPhysContinuous} object.
#' @importFrom methods new
#' @rdname newEPhysContinuous
#' @export
newEPhysContinuous <- function(Data,
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
  out<-new("EPhysContinuous",
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
  validEPhysContinuous(out)
  out
}

#' Show method for EPhysContinuous
#'
#' Prints a concise summary of array dimensions and selected metadata.
#'
#' @param object An \code{EPhysContinuous} object.
#' @rdname EPhysContainer-class
#' @noRd
#' @export
setMethod("show", "EPhysContinuous", function(object) {
  cat("<EPhysContinuous>\n")
  dims <- dim(object@Data)
  cat(" Time bins    :", dims[1], "\n")
  cat(" Runs       :", dims[2], "\n")
  cat(" Channels     :", dims[3], "\n")
  cat(" Stimulus unit:", if (nzchar(object@StimulusUnits)) object@StimulusUnits else "(none)", "\n")
  cat(" Imported     :", format(object@Imported), "\n")
})
