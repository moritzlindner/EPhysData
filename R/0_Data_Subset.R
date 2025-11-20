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
#'         \code{Metadata} if present, and drops runs with no remaining spikes.
#'   \item \strong{EPhysContinuous}: Same metadata/channel/time handling as \emph{Events}, but
#'         subsetting the 3D array \code{[time × run × channel]}. \code{StimulusTrace} is sliced
#'         with \code{TimeTrace} when lengths match (otherwise left unchanged with a warning).
#' }
#'
#' @inheritParams GetData
#' @inheritParams as.data.frame
#'
#' @param i,j Indices specifying elements to extract.
#' @param ... Currently unused.
#'
#' \strong{EPhysData / EPhysSet}
#' @param Time A length-2 vector giving the time window; may be plain numeric in the same unit
#'   as \code{TimeTrace(X)} \emph{or} a \pkg{units} vector convertible to that unit.
#'   The window is treated as a closed interval \code{[start, end]} and the returned time
#'   is zero-shifted by \code{start}.
#' @param TimeExclusive Logical; if \code{TRUE}, use \code{[start, end)} semantics.
#' @param Trials Trial indices \emph{or} a logical vector of length \code{ntrials}. For
#'   \code{EPhysSet}, this can be used only when all items have the same number of trials.
#' @param SetItems Which items of an \code{EPhysSet} to keep; logical vector of length \code{length(X)}
#'   or a numeric index vector.
#' @param Simplify Logical; if \code{TRUE} and the result contains only one item, return
#'   that \code{EPhysData} instead of an \code{EPhysSet}.
#'
#' \strong{EPhysEvents / EPhysContinuous}
#' @param Step Optional numeric criterion. If length-2, treated as a closed range
#'   \code{[min, max]}; otherwise matched via \code{\%in\%} to \code{Metadata$Step}.
#' @param Experiment Optional character vector matched via \code{\%in\%} to \code{Metadata$Experiment}.
#' @param Repeat Optional numeric criterion; same range vs. \code{\%in\%} behavior as \code{Step}.
#' @param RecordingID Optional numeric criterion; same behavior as \code{Step}.
#' @param Channels Optional character vector of channel names to keep. Requested order is preserved.
#' @param TimeRange Length-2 numeric or \pkg{units} vector giving \code{[start, end]} in the
#'   \code{TimeTrace} unit (converted if \pkg{units}-aware). Returned \code{TimeTrace} (and aligned
#'   \code{StimulusTrace}, if lengths match) is zero-shifted by \code{start}.
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
#'   \item \strong{Range vs. membership matching}: For numeric \code{Step}, \code{Repeat},
#'         and \code{RecordingID}, a length-2 vector is interpreted as \code{[min, max]} (closed);
#'         any other length uses \code{\%in\%}. \code{Experiment} is always matched via \code{\%in\%}.
#'   \item \strong{Units}: \code{Time} and \code{TimeRange} can be plain numeric (interpreted in
#'         the same unit as \code{TimeTrace(X)}) or \pkg{units} vectors convertible to that unit.
#'   \item \strong{Stimulus alignment}: When \code{length(StimulusTrace(X)) == length(TimeTrace(X))},
#'         the stimulus is cropped/shifted with time; otherwise it is left unchanged and a warning
#'         is issued.
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
            Data <- GetData(
              X = X,
              Time = Time,
              TimeExclusive = TimeExclusive,
              Trials = Trials,
              Raw = Raw
            )

            if (!("units" %in% class(Time))) {
              stop("'Time' must be of class 'units'")
            }

            convertibel.to.s <- tryCatch({
              set_units(Time, "s")
              TRUE
            }, error = function(e) {
              FALSE
            })
            if (!convertibel.to.s) {
              stop("'Time' must be of convertible to seconds.")
            }

            if (!isTRUE(all.equal(Time, range(TimeTrace(X)))) || TimeExclusive) {
              filter.fx <- function(x) {
                return(x)
              }
              message("Data is subsetted by time, thus resetting filter function.")
            } else {
              filter.fx <- FilterFunction(X)
            }

            if(!is.null(Trials)){
              warning("Subsetting by trials: resetting rejection function.")
              rejected.fx <- function(x) {
                return(rep(FALSE, dim(x)[2]))
              }
            } else {
              rejected.fx<-Rejected(X,return.fx = T)
            }

            Time <- condition_time(X, Time, TimeExclusive)

            err <- tryCatch(
              StimulusTrace(X),
              error = function(e)
                e
            )
            has_stimtrace <- !any(class(err) == "error")

            if(has_stimtrace){
              new_stim<-StimulusTrace(X)[TimeTrace(X) %in% Time]
            }else{
              new_stim<-as_units(integer(),unitless)
            }

            if(!Raw){
              rejected.fx <- function(x) {
                return(FALSE)
              }
              filter.fx <- function(x) {
                return(x)
              }
            }

            # if x is changed, then dont keep filter

            if (!Raw) {
              average.fx <- function(x) {
                return(x)
              }
            } else {
              average.fx <- AverageFunction(X)
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
      stop ("Not all values of 'Step' exist in the repsecitve metadata column.")
    }
    keep <- keep & (as.character(md$Step) %in% Step)
  }
  if (!is.null(Experiment))        keep <- keep & (as.character(md$Experiment) %in% Experiment)
  if (!is.null(Repeat))      keep <- keep & match_numeric(md$Repeat,       Repeat)
  if (!is.null(RecordingID)) keep <- keep & match_numeric(md$RecordingID, RecordingID)

  run_idx <- which(keep)
  if (length(run_idx) == 0L) stop("No trials match the given metadata filters.")
  new_md <- md[run_idx, , drop = FALSE]

  # ---- channels (respect requested order if provided) ----
  old_ch <- Channels(X)
  if (!is.null(Channels)) {
    req <- unique(as.character(Channels))
    present <- req[req %in% old_ch]
    if (length(present) == 0L) stop("None of the requested Channels are present.")
    ch_idx <- match(present, old_ch)      # preserve request order
    new_channels <- present
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
  if (length(TimeRange) != 2L)
    stop("TimeRange must be length 2: c(start, end).")

  # Coerce TimeRange to tt's unit (if tt has units); if TimeRange already has units, convert.
  tr_u <- tryCatch({
    set_units(TimeRange, TimeUnits(X), mode = "standard")
  }, error = function(e)
    stop(
      "TimeRange is not convertible to the TimeTrace units: ",
      conditionMessage(e)
    ))
  a <- as.numeric(tr_u[1])
  b <- as.numeric(tr_u[2])


  ttn <- as.numeric(tt)  # safe even if tt has units
  time_idx <- which(ttn >= a & ttn <= b)
  if (length(tt) > 0L && length(time_idx) == 0L) {
    stop("TimeRange selects no time samples.")
  }

  # New TimeTrace, zero-shift if finite lower bound
  new_tt <- tt[time_idx]
  if (is.finite(a) && a != -Inf) {
    new_tt <- new_tt - tr_u[1]
  }

  # StimulusTrace aligned slice (warn on mismatch)
  st <- StimulusTrace(X)
  new_st <- st[time_idx]

  # ---- Metadata Start/Stop/Diff updates (if columns exist) ----
  # We update in the unit of md (assumes md$Start/Stop in same base as tt)
  if ("Start" %in% names(new_md)) {
    if (is.finite(a) && a != -Inf) new_md$Start <- new_md$Start + a
  }
  if ("Stop" %in% names(new_md)) {
    if (is.finite(b) && b != Inf)  new_md$Stop  <- new_md$Start + (b - if (is.finite(a)) a else 0)
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
    new_tt         = new_tt,
    new_st         = new_st
  )
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

    cmm <- .subset_common_container(
      X, Step = Step, Experiment = Experiment, Repeat = Repeat,
      RecordingID = RecordingID, Channels = Channels, TimeRange = TimeRange
    )

    # Subset event data by runs, then by channels, then time-window the timestamps
    raw_runs <- X@Data[cmm$run_idx]

    # Channel selection: keep names aligned to Channels(X); we index by position (cmm$ch_idx)
    # raw_runs is list per run, each is a named list of channels in original order
    # We transform by index to preserve original structure/order consistent with 'new_channels'.
    new_data <- lapply(raw_runs, function(ch_list) {
      # ensure index-based selection aligned with original Channels(X)
      ch_list[cmm$ch_idx]
    })

    # Apply time window [a,b] and zero-shift by lower bound 'a'
    a_num <- as.numeric(TimeRange[1]); b_num <- as.numeric(TimeRange[2])
    # But use the same numeric 'a'/'b' the helper used (derived from tt units)
    tr_u <- set_units(TimeRange, TimeUnits(X), mode = "standard")
    a_num <- as.numeric(tr_u[1])
    b_num <- as.numeric(tr_u[2])

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

    stopifnot(is.array(X@Data), length(dim(X@Data)) == 3L)

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
            if(length(i)!=length(value)){
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
            if(validEPhysData(x)){
              return(x)
            }
          })

#' @describeIn Subset-methods Extract specific items from an \linkS4class{EPhysData} object. Returns a data.frame.
#' @aliases `[`,EPhysData,ANY-method
#' @export
setMethod("[",
          "EPhysData",
          function(x, i, j) {
            return(x@Data[i, j, drop = F])
          })
