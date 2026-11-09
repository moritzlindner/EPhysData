#' Subset EPhys objects (Data / Set / Events / Continuous)
#'
#' Subset an \code{EPhysData}, \code{EPhysSet}, \code{EPhysEvents}, or
#' \code{EPhysContinuous} object into a new object of the same class.
#'
#' \strong{Class-specific behavior}
#' \itemize{
#'   \item \strong{EPhysData}: Subsets by a time window and/or trials; if data are subset by time,
#'         the stored per-trial \code{FilterFunction} is reset; when subsetting by trials, the
#'         \code{Rejected} function is reset. See Details.
#'   \item \strong{EPhysSet}: Selects items from a set of \code{EPhysData}; optionally applies the
#'         same time/trial subsetting to all items (when lengths allow); can simplify to a single
#'         \code{EPhysData} via \code{Simplify=TRUE}.
#'   \item \strong{EPhysEvents}: Filters trials by \code{Metadata} (e.g., Step/Experiment/repeat),
#'         optionally keeps only selected \code{Channels}, applies a time window to event timestamps,
#'         zero-shifts times by the lower bound, updates \code{Start}/\code{Stop}/\code{Diff} in
#'         \code{Metadata} if present, and retains runs with no remaining spikes.
#'   \item \strong{EPhysContinuous}: Same metadata/channel/time handling as \emph{Events}, but
#'         subsetting the 3D array \code{[time × run × channel]}. \code{StimulusTrace} is sliced
#'         with \code{TimeTrace} when present.
#' }
#'
#' @inheritParams GetData
#' @inheritParams as.data.frame
#'
#' @param i,j Indices specifying elements to extract.
#' @param ... Reserved for method compatibility. For \code{EPhysEvents} and
#'   \code{EPhysContinuous}, any unused arguments passed through \code{...}
#'   cause an error.
#'
#' \strong{EPhysData / EPhysSet}
#' @param Time A length-2 vector giving the time window; may be plain numeric in the same unit
#'   as \code{TimeTrace(X)} \emph{or} a \pkg{units} vector convertible to that unit.
#'   The window is treated as a closed interval \code{[start, end]} and the returned time
#'   keeps the original sample times.
#' @param TimeExclusive Logical; if \code{TRUE}, select the requested time points.
#' @param Trials Trial indices \emph{or} a logical vector of length \code{ntrials}. For
#'   \code{EPhysSet}, this can be used only when all items have the same number of trials.
#' @param SetItems Which items of an \code{EPhysSet} to keep; logical vector of length \code{length(X)}
#'   or a numeric index vector.
#' @param Simplify Logical; if \code{TRUE} and the result contains only one item, return
#'   that \code{EPhysData} instead of an \code{EPhysSet}.
#'
#' \strong{EPhysEvents / EPhysContinuous}
#' @param Step Optional values matched to the levels of \code{Metadata$Step}.
#' @param Experiment Optional character vector matched via \code{\%in\%} to \code{Metadata$Experiment}.
#' @param Repeat Optional numeric criterion; two values specify a closed range, otherwise match individual values.
#' @param RecordingID Optional numeric criterion; two values specify a closed range, otherwise match individual values.
#' @param Channels Optional character vector of channel names to keep. Requested order is preserved. Missing names cause an error.
#' @param TimeRange Length-2 numeric or \pkg{units} vector giving \code{[start, end]} in the
#'   \code{TimeTrace} unit (converted if \pkg{units}-aware). Returned sample times
#'   and event timestamps are shifted by a finite \code{start}.
#'
#' @return
#' A new object of the same class as \code{X} with the requested subset applied:
#' \itemize{
#'   \item \code{Subset(EPhysData)} → \code{EPhysData}
#'   \item \code{Subset(EPhysSet)} → \code{EPhysSet} (or \code{EPhysData} if \code{Simplify=TRUE})
#'   \item \code{Subset(EPhysEvents)} → \code{EPhysEvents}
#'   \item \code{Subset(EPhysContinuous)} → \code{EPhysContinuous}
#' }
#'
#' @details
#' \itemize{
#'   \item \strong{Range vs. membership matching}: For numeric \code{Repeat}
#'         and \code{RecordingID}, a length-2 vector is interpreted as \code{[min, max]} (closed);
#'         any other length uses \code{\%in\%}. \code{Experiment} and \code{Step} are matched via \code{\%in\%}.
#'   \item \strong{Units}: \code{Time} and \code{TimeRange} can be plain numeric (interpreted in
#'         the same unit as \code{TimeTrace(X)}) or \pkg{units} vectors convertible to that unit.
#'   \item \strong{Stimulus alignment}: A present stimulus trace uses the same
#'         sample indices as the time trace. An absent stimulus remains empty.
#'   \item \strong{Metadata updates}: If \code{Metadata} contains \code{Start}/\code{Stop}, these are
#'         adjusted to reflect the time window; \code{Diff} is recomputed as \code{Stop - Start}.
#'   \item \strong{EPhysData resets}: Subsetting by time resets \code{FilterFunction}; subsetting
#'         by trials resets \code{Rejected}.
#' }
#'
#' @family EPhysData-methods
#' @family Subsetting_Dataextraction
#' @examples
#' # Subset EPhysData
#' myEPhysData <- makeExampleEPhysData(replicate_count = 3)
#'
#' ## Get subsetted data based on time range and trials
#' subsetted_myEPhysData <- Subset(myEPhysData, Time = TimeTrace(myEPhysData)[c(1, 3)], Trials = c(1,2))
#' subsetted_myEPhysData
#'
#' # Subset EPhysSet
#' myEPhysSet <- makeExampleEPhysSet(nsets=10)
#' subsetted_myEPhysSet <- Subset(myEPhysSet, SetItems=c(4:7))
#' subsetted_myEPhysSet
#' Metadata(subsetted_myEPhysSet)

#' @importFrom units as_units set_units
#' @importFrom methods new validObject
#' @export
#' @docType methods
#' @rdname Subset-methods
setGeneric(
  name = "Subset",
  def = function(X, ...) {
    standardGeneric("Subset")
  }
)

#' @rdname Subset-methods
#' @aliases Subset,EPhysData,EPhysSet,ANY-method
setMethod("Subset",
          "EPhysData",
          function(X,
                   Time = range(TimeTrace(X)),
                   TimeExclusive = FALSE,
                   Trials = NULL,
                   Raw = T,
                   ...) {
            validObject(X)
            if (!length(X@TimeTrace)) .ephys_abort("Subset(EPhysData) found no time samples.")
            Time <- .ephys_legacy_time(X, Time, "Subset(EPhysData)")
            Data <- GetData(
              X = X,
              Time = Time,
              TimeExclusive = TimeExclusive,
              Trials = Trials,
              Raw = Raw
            )

            if (!isTRUE(all.equal(Time, range(TimeTrace(X)))) || TimeExclusive) {
              filter.fx <- .ephys_identity
              message("Subset(EPhysData) reset the filter for the time subset.")
            } else {
              filter.fx <- FilterFunction(X)
            }

            rejected.fx <- Rejected(X, return.fx = TRUE)
            if (!is.null(Trials)) {
              .ephys_warn("Subset(EPhysData) reset the rejection callback after selecting trials.",
                          "ephys_selection_warning")
              rejected.fx <- .ephys_reject_none
            }

            trace <- TimeTrace(X)
            keep_time <- trace %in% condition_time(X, Time, TimeExclusive)
            Time <- trace[keep_time]
            new_stim <- if (length(X@StimulusTrace)) X@StimulusTrace[keep_time] else X@StimulusTrace

            average.fx <- AverageFunction(X)
            if (!Raw) {
              rejected.fx <- .ephys_reject_none
              filter.fx <- .ephys_identity
              average.fx <- .ephys_identity
            }

            out <- new(
              "EPhysData",
              Data = Data,
              TimeTrace = Time,
              StimulusTrace = new_stim,
              Rejected = rejected.fx,
              average.fx = average.fx,
              filter.fx = filter.fx,
              Created = X@Created
            )
            if (validObject(out)) {
              return(out)
            } else{
              stop("No valid EPhysData object could be created.")
            }
          })

#' @importFrom units as_units
#' @importFrom methods validObject
#' @importFrom stringr str_extract
#' @rdname Subset-methods
setMethod("Subset",
          "EPhysSet",
          function(X,
                   Time = NULL,
                   TimeExclusive = FALSE,
                   Trials = NULL,
                   SetItems = rep(TRUE, nrow(Metadata(X))),
                   Raw = T,
                   Simplify = F,
                   ...
          ) {

            if(is.logical(SetItems)){
              if(length(SetItems)!=length(X)){
                stop("Lengths mismatch: 'SetItems' is a ", typeof(SetItems), " of length ", length(SetItems), " but must be a logical vector of the same length as 'X' (", length(X), ") or a numeric vector representing valid item indices.")
              }
            }else{
              if(!is.numeric(SetItems)){
                stop("'SetItems' is neither logical nor numeric. 'SetItems' is a ", typeof(SetItems), " of length ", length(SetItems), " but must be a logical vector of the same length as 'X' (", length(X), ") or a numeric vector representing valid item indices.")
              }else{
                if(!(all(SetItems %in% 1:length(X)))){
                  stop("'SetItems' contains invalid indices. 'SetItems' is a ", typeof(SetItems), " of length ", length(SetItems), " and with data in the range of ", min(SetItems), " to ", max(SetItems), " but must be a logical vector of the same length as 'X' (", length(X), ") or a numeric vector representing valid item indices (i.e. value range must be within the length of 'X').")
                }
              }
            }

            md.orig<-Metadata(X)

            X@Metadata<-Metadata(X)[SetItems,, drop=FALSE]
            X@Data<-X@Data[SetItems]

            if (!is.null(Trials)) { # if "Trials" not null, check that all EPhysData have same number of trials
              if (length(unique(unlist(lapply(X@Data, function(x) {
                dim(x)[2]
              })))) != 1) {
                ntr <- range(unlist(lapply(X@Data, function(x) {
                  dim(x)[2]
                })))
                stop("Paramater 'Trials' is defined, but Items selected have differing number or trials (Ranging from ", ntr[1], " to ", ntr[2], "). 'Trials' can only be used if all items (EPhysData) have the same number of trials.")
              }
            }

            tryCatch({
              X@Data <- lapply(X@Data, function(x) {
                if (is.null(Time)){
                  Time = range(TimeTrace(x))
                }
                # if (is.null(Trials) && !Raw){
                #   curr.Trials = !Rejected(x)
                # } else {
                #   if (is.null(Trials)){
                #     curr.Trials <- !logical(dim(x)[2])
                #   } else {
                #     curr.Trials<-Trials
                #   }
                # }
                x<-Subset(
                  X = x,
                  Time = Time,
                  TimeExclusive = TimeExclusive,
                  Trials = Trials,
                  Raw = Raw
                )
                return(x)
              })
            }, error = function (e){
              stop(e)
              #stop("Subsetting EPhysSet failed for recording ", Metadata(X)[as.integer(str_extract(e$message, "(?<=\\[\\[)\\d+(?=L\\]\\])")),1], )
            })
            if(nrow(Metadata(X)) == 1 && Simplify == T){
              X<-X@Data[[1]]
            }

            if(validObject(X)){
              return(X)
            }
          })


# ---- Internal helpers (not exported) ----------------------------------------

#' Common subsetting logic for EPhys* containers (internal)
#'
#' Applies metadata filters, channel selection, time-window cropping (units-aware),
#' Start/Stop/Diff updates, and aligned StimulusTrace/TimeTrace slicing.
#' Returns indices and pre-sliced components for use by class-specific methods.
#'
#' @importFrom units set_units deparse_unit
#' @keywords internal
.subset_common_container <- function(
    X,
    Step    = NULL,
    Experiment         = NULL,
    Repeat       = NULL,
    RecordingID  = NULL,
    Channels     = NULL,
    TimeRange    = c(0, Inf)
) {
  md <- Metadata(X)

  # --- helper: numeric field criterion (closed range if length 2, else %in%) ---
  match_numeric <- function(vec, crit) {
    if (length(crit) == 2L && is.numeric(crit)) {
      (vec >= crit[1]) & (vec <= crit[2])
    } else {
      vec %in% crit
    }
  }

  # ---- trial filtering via Metadata ----
  keep <- rep(TRUE, nrow(md))
  if (!is.null(Step)){
    Step<- as.character(Step)
    if (!all(Step %in% as.character(levels(md$Step)))){
      missing <- setdiff(Step, levels(md$Step))
      .ephys_abort(sprintf("Subset(%s) received unknown Step values: %s.",
                           class(X)[1L], paste(shQuote(missing), collapse = ", ")))
    }
    keep <- keep & (as.character(md$Step) %in% Step)
  }
  if (!is.null(Experiment))        keep <- keep & (as.character(md$Experiment) %in% Experiment)
  if (!is.null(Repeat)) {
    if (!"Repeat" %in% names(md)) {
      .ephys_abort(sprintf("Subset(%s) requires Metadata$Repeat when Repeat is supplied.",
                           class(X)[1L]))
    }
    keep <- keep & match_numeric(md$Repeat, Repeat)
  }
  if (!is.null(RecordingID)) keep <- keep & match_numeric(md$RecordingID, RecordingID)

  run_idx <- which(keep)
  if (!length(run_idx)) {
    .ephys_abort(sprintf("Subset(%s) matched no runs among %d metadata rows.",
                         class(X)[1L], nrow(md)))
  }
  new_md <- md[run_idx, , drop = FALSE]

  # ---- channels (respect requested order if provided) ----
  old_ch <- Channels(X)
  if (!is.null(Channels)) {
    req <- unique(as.character(Channels))
    if (!length(req)) {
      .ephys_abort(sprintf("Subset(%s) requires at least one channel name.", class(X)[1L]))
    }
    missing <- setdiff(req, old_ch)
    if (length(missing)) {
      .ephys_abort(sprintf("Subset(%s) could not find channels: %s. Matched %d of %d requested.",
                           class(X)[1L], paste(shQuote(missing), collapse = ", "),
                           length(req) - length(missing), length(req)))
    }
    ch_idx <- match(req, old_ch)
    new_channels <- req
  } else {
    ch_idx <- seq_along(old_ch)
    new_channels <- old_ch
  }

  # ---- Channel_Metadata aligned subset (if present) ----
  cm <- ChannelMetadata(X)
  if (is.data.frame(cm) && nrow(cm) > 0) {
    # If cm is aligned by row to Channels(X), take index subset:
    new_cm <- cm[ch_idx, , drop = FALSE]
  } else {
    new_cm <- cm
  }

  # ---- time handling (units-aware) ----
  tt <- TimeTrace(X)
  operation <- sprintf("Subset(%s)", class(X)[1L])
  if (!is.numeric(TimeRange) || length(TimeRange) != 2L || anyNA(TimeRange)) {
    .ephys_abort(paste(operation, "requires a numeric TimeRange of two non-missing bounds."))
  }
  tr_u <- tryCatch(
    set_units(TimeRange, TimeUnits(X), mode = "standard"),
    error = function(e) .ephys_abort(
      sprintf("%s could not convert TimeRange to %s.", operation, shQuote(TimeUnits(X))),
      parent = e
    )
  )
  a <- as.numeric(tr_u[1L])
  b <- as.numeric(tr_u[2L])
  if (a > b || a == Inf || b == -Inf) {
    .ephys_abort(sprintf("%s requires ordered TimeRange bounds. Received [%g, %g] %s.",
                         operation, a, b, TimeUnits(X)))
  }

  ttn <- as.numeric(tt)
  time_idx <- which(ttn >= a & ttn <= b)
  if (length(tt) && !length(time_idx)) {
    .ephys_abort(sprintf("%s found no samples in [%g, %g] %s. Available range is [%g, %g] %s.",
                         operation, a, b, TimeUnits(X), min(ttn), max(ttn), TimeUnits(X)))
  }

  # New TimeTrace, zero-shift if finite lower bound
  new_tt <- tt[time_idx]
  if (is.finite(a) && a != -Inf) {
    new_tt <- new_tt - tr_u[1]
  }

  # StimulusTrace aligned slice (warn on mismatch)
  st <- StimulusTrace(X)
  new_st <- if (length(st)) st[time_idx] else st

  # ---- Metadata Start/Stop/Diff updates (if columns exist) ----
  # We update in the unit of md (assumes md$Start/Stop in same base as tt)
  if ("Start" %in% names(new_md)) {
    original_start <- new_md$Start
    if (is.finite(a)) new_md$Start <- original_start + a
    if ("Stop" %in% names(new_md) && is.finite(b)) new_md$Stop <- original_start + b
  } else if ("Stop" %in% names(new_md) && is.finite(b)) {
    .ephys_warn(paste(operation, "preserved Metadata$Stop because Metadata$Start is absent."),
                "ephys_metadata_warning")
  }
  if (all(c("Start", "Stop") %in% names(new_md))) {
    new_md$Diff <- round(new_md$Stop - new_md$Start, 5)
  }

  list(
    run_idx        = run_idx,
    new_md         = new_md,
    ch_idx         = ch_idx,
    new_channels   = new_channels,
    new_channel_md = new_cm,
    time_idx       = time_idx,
    time_bounds    = c(a, b),
    new_tt         = new_tt,
    new_st         = new_st
  )
}

#' Check for dot arguments
#' @keywords internal
.check_unused_dots <- function(..., .fun = "Subset") {
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    return(invisible(NULL))
  }

  dot_labels <- names(dots)
  if (is.null(dot_labels)) dot_labels <- rep("", length(dots))
  unnamed <- !nzchar(dot_labels)
  dot_labels[unnamed] <- paste("unnamed argument", which(unnamed))
  msg <- sprintf("%s received unused arguments: %s.",
                 .fun, paste(shQuote(dot_labels), collapse = ", "))
  if ("Channel" %in% dot_labels) msg <- paste(msg, "Use Channels to select channels.")
  .ephys_abort(msg)
}

# ---- Methods: Subset for EPhysEvents / EPhysContinuous -----------------------

#' @rdname Subset-methods
#' @importFrom units set_units deparse_unit
#' @export
setMethod(
  "Subset",
  signature(X = "EPhysEvents"),
  function(X,
           Step    = NULL,
           Experiment         = NULL,
           Repeat       = NULL,
           RecordingID  = NULL,
           Channels     = NULL,
           TimeRange    = c(0, Inf),
           ...) {

    .check_unused_dots(..., .fun = "Subset(EPhysEvents)")

    validObject(X)
    cmm <- .subset_common_container(
      X, Step = Step, Experiment = Experiment, Repeat = Repeat,
      RecordingID = RecordingID, Channels = Channels, TimeRange = TimeRange
    )

    # Subset event data by runs, then by channels, then time-window the timestamps
    raw_runs <- X@Data[cmm$run_idx]

    new_data <- lapply(raw_runs, function(ch_list) {
      ch_list[cmm$new_channels]
    })

    a_num <- cmm$time_bounds[1L]
    b_num <- cmm$time_bounds[2L]
    new_data <- lapply(new_data, function(ch_list) {
      lapply(ch_list, function(ts) {
        ts <- ts[ts >= a_num & ts <= b_num]
        if (is.finite(a_num) && a_num != -Inf) ts <- ts - a_num
        ts
      })
    })

    new("EPhysEvents",
        Metadata          = cmm$new_md,
        Data              = new_data,
        ExamInfo          = X@ExamInfo,
        SubjectInfo       = X@SubjectInfo,
        Imported          = X@Imported,
        TimeTrace         = as.numeric(cmm$new_tt),
        Channels          = cmm$new_channels,
        Channel_Metadata  = cmm$new_channel_md,
        StimulusTrace     = as.numeric(cmm$new_st),
        TimeUnits         = X@TimeUnits,
        StimulusUnits     = X@StimulusUnits)
  }
)

#' @rdname Subset-methods
#' @importFrom units set_units deparse_unit drop_units
#' @export
setMethod(
  "Subset",
  signature(X = "EPhysContinuous"),
  function(X,
           Step    = NULL,
           Experiment         = NULL,
           Repeat       = NULL,
           RecordingID  = NULL,
           Channels     = NULL,
           TimeRange    = c(0, Inf),
           ...) {

    .check_unused_dots(..., .fun = "Subset(EPhysContinuous)")
    validObject(X)

    cmm <- .subset_common_container(
      X, Step = Step, Experiment = Experiment, Repeat = Repeat,
      RecordingID = RecordingID, Channels = Channels, TimeRange = TimeRange
    )

    arr <- X@Data
    new_arr <- arr[cmm$time_idx, cmm$run_idx, cmm$ch_idx, drop = FALSE]

    new("EPhysContinuous",
        Metadata          = cmm$new_md,
        Data              = new_arr,
        ExamInfo          = X@ExamInfo,
        SubjectInfo       = X@SubjectInfo,
        Imported          = X@Imported,
        TimeTrace         = as.numeric(cmm$new_tt),
        Channels          = cmm$new_channels,
        Channel_Metadata  = cmm$new_channel_md,
        StimulusTrace     = as.numeric(cmm$new_st),
        TimeUnits         = X@TimeUnits,
        StimulusUnits     = X@StimulusUnits)
  }
)



#' @describeIn Subset-methods Extract specific items from an \linkS4class{EPhysSet} object. Returns an \linkS4class{EPhysData} object or a list thereof.
#' @aliases `[[`,EPhysSet,ANY-method
#' @export
setMethod("[[",
          "EPhysSet",
          function(x, i) {
            if (length(i) == 1) {
              return(x@Data[[i]])
            } else {
              out <- lapply(i, function(ii) {
                return(x@Data[[ii]])
              })
              return(out)
            }
          })

#' @keywords internal
#' @aliases `[[<-`,EPhysSet,ANY-method
#' @noMd
setMethod("[[<-",
          "EPhysSet",
          function(x, i, value) {
            if (length(i) != 1L && length(i) != length(value)) {
              stop("Index and replacement must have the same length")
            }
            if (length(i) == 1) {
              if(!("EPhysData" %in% class(value))){
                stop("Replacement item must be of class 'EPhysData'")
              }
              x@Data[[i]]<-value
            } else {
              rightclass<-all(unlist(lapply(value, function(v) {
                ("EPhysData" %in% class(v))
              })))
              if(!rightclass){
                stop("Replacement items must ve of class 'EPhysData'")
              }
              x@Data[i]<-value
            }
            validObject(x)
            x
          })

#' @describeIn Subset-methods Extract specific items from an \linkS4class{EPhysData} object. Returns a data.frame.
#' @aliases `[`,EPhysData,ANY-method
#' @export
setMethod("[",
          "EPhysData",
          function(x, i, j) {
            return(x@Data[i, j, drop = F])
          })
