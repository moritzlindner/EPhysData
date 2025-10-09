#' as.data.frame for EPhys* objects
#'
#' Converts electrophysiology objects into long data frames suitable for plotting
#' and analysis.
#'
#' Supported classes:
#' \itemize{
#'   \item \link{EPhysData}: single recording (multiple trials/columns) with time (+ optional stimulus).
#'   \item \link{EPhysSet}: collection of \link{EPhysData}; returns per-record \code{Metadata} columns prepended.
#'   \item \link{EPhysContinuous}: binned spike counts/rates (multi-run × multi-channel).
#'   \item \link{EPhysEvents}: spike timestamps (multi-run × multi-channel).
#' }
#'
#' @param x An \link{EPhysData}, \link{EPhysSet}, \link{EPhysContinuous}, or
#'   \link{EPhysEvents} object.
#' @param IncludeRejected (for \link{EPhysData}, \link{EPhysSet}) Logical. If \code{TRUE}, rejected trials are included;
#'   if \code{FALSE} (default), rejected trials are dropped.
#' @param Raw (for \link{EPhysData}, \link{EPhysSet}) Logical. If \code{TRUE} (default), use raw data; otherwise use processed data as defined by \code{GetData()}.
#' @param ReturnAs (for \link{EPhysContinuous} only) Character; either \dQuote{count} (raw
#'   spike counts) or \dQuote{freq} (counts divided by bin width → Hz). Default \dQuote{count}.
#' @param ... Additional arguments passed to or ignored by methods.
#'
#' @section Returned columns:
#'
#' \strong{For \code{EPhysData}} the data.frame has columns:
#' \describe{
#'   \item{Trial}{Integer trial index (derived from original trial columns).}
#'   \item{Time}{Time vector (units preserved).}
#'   \item{Stimulus}{(If available) stimulus vector aligned to \code{Time} (units preserved).}
#'   \item{Value}{Signal value per time point (units preserved).}
#' }
#'
#' \strong{For \code{EPhysSet}} the data.frame has the same columns as
#' \code{EPhysData}, \emph{preceded} by all columns from \code{Metadata(x)} for
#' the corresponding record (replicated to each row).
#'
#' \strong{For \code{EPhysContinuous}} the data.frame has columns:
#' \describe{
#'   \item{…metadata…}{All columns from \code{Metadata(x)}, except
#'     \code{Start}, \code{Stop}, \code{Diff}, \code{Trials}.}
#'   \item{Channel}{Factor of channel names.}
#'   \item{Time}{Bin time (in \code{TimeUnits(x)}).}
#'   \item{Stimulus}{(If present) stimulus value aligned to bins.}
#'   \item{Value}{Integer count or numeric rate per bin (Hz), depending on \code{ReturnAs}.}
#' }
#'
#' \strong{For \code{EPhysEvents}} the data.frame has columns:
#' \describe{
#'   \item{…metadata…}{All columns from \code{Metadata(x)}, except
#'     \code{Start}, \code{Stop}, \code{Diff}, \code{Trials}.}
#'   \item{Channel}{Factor of channel names.}
#'   \item{Timestamp}{Spike time (in \code{TimeUnits(x)}).}
#' }
#'
#' @examples
#' \dontrun{
#' # EPhysData / EPhysSet
#' d <- makeExampleEPhysData()
#' df_d <- as.data.frame(d)
#'
#' s <- makeExampleEPhysSet()
#' df_s <- as.data.frame(s)
#'
#' # EPhysContinuous
#' df_c <- as.data.frame(ephys_continuous, ReturnAs = "freq")
#'
#' # EPhysEvents
#' df_e <- as.data.frame(ephys_events)
#' }
#'
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom units drop_units as_units deparse_unit
#' @name as.data.frame
#' @rdname as.data.frame-methods
NULL

# -------- EPhysData method --------
#' @describeIn as.data.frame-methods Convert an \link{EPhysData} to long format.
#' @inheritParams GetData
#' @param IncludeRejected Logical. If \code{TRUE}, rejected trials are included;
#'   if \code{FALSE} (default), rejected trials are dropped.
#' @param Raw Logical. If \code{TRUE} (default), use raw data; otherwise use processed data as defined by \code{GetData()}.
#' @export
setMethod("as.data.frame",
          "EPhysData",
          function(x,
                   Raw = T,
                   IncludeRejected = F,
                   ...) {
            if(IncludeRejected){
              dat <- GetData(x, Raw = Raw)
            } else {
              dat <- GetData(x, Raw = Raw, Trials = !Rejected(x))
            }
            time <- TimeTrace(x)
            stim <-
              tryCatch(
                StimulusTrace(x),
                error = function(e) {
                  return(NULL)
                }
              )
            dat_units <- deparse_unit(dat)
            time_units <- deparse_unit(time)
            if (is.null(stim)) {
              stim_units <- NULL
            } else{
              stim_units <- deparse_unit(stim)
              stim<-as.vector(stim)
            }
            dat <- as.data.frame(drop_units(dat))
            time <- as.vector(drop_units(time))
            colnames(dat) <-
              paste0("Trial_", 1:length(colnames(dat)))
            dat$Time <- time
            if (!is.null(stim)) {
              dat$Stimulus <- stim
            }
            dat <- pivot_longer(
              dat,
              starts_with("Trial_"),
              names_prefix = "Trial_",
              names_to = "Trial",
              names_transform = list(Trial = as.numeric),
              values_to = "Value"
            )
            dat$Value <- as_units(dat$Value, dat_units)
            dat$Time <- as_units(dat$Time, time_units)
            if (!is.null(stim)) {
              dat$Stimulus <- as_units(dat$Stimulus, stim_units)
              dat <- dat[, c("Trial", "Time", "Stimulus", "Value")]
            }else{
              dat <- dat[, c("Trial", "Time", "Value")]
            }
            dat <- dat[order(dat$Trial, dat$Time), ]
            return(as.data.frame(dat))
          })

# -------- EPhysSet method --------
#' @describeIn as.data.frame-methods Convert an \link{EPhysSet} to long format
#' by row-binding record-wise \code{as.data.frame(EPhysData)} and prepending
#' each record's \code{Metadata}.
#' @export
setMethod("as.data.frame",
          "EPhysSet",
          function(x,
                   Raw = T,
                   IncludeRejected = F,
                   ...) {
            data.list <-
              lapply(x, function(y) {
                as.data.frame(y, Raw = Raw, IncludeRejected = IncludeRejected)
              }, ReturnEPhysSet = F)
            for (i in 1:length(data.list)) {
              metadata_cols <- Metadata(x)[i, , drop = F]
              metadata_cols <-
                metadata_cols[rep(1, nrow(data.list[[i]])), , drop = F]

              data.list[[i]] <- cbind(metadata_cols, data.list[[i]])
            }

            dat <- as.data.frame(do.call(rbind, data.list))
            rownames(dat) <- NULL
            return(dat)
          })

# -------- EPhysContinuous method --------
#' @describeIn as.data.frame-methods Convert an \link{EPhysContinuous} to long format.
#' @export
setMethod("as.data.frame",
          signature(x = "EPhysContinuous"),
          function(x, ReturnAs = c("count", "freq"), ...) {
            ReturnAs <- match.arg(ReturnAs)
            cnts    <- x@Data
            time    <- TimeTrace(x)
            channels<- Channels(x)
            stim    <- StimulusTrace(x)
            meta    <- Metadata(x)

            # Drop unneeded metadata columns
            meta <- meta[, setdiff(names(meta), c("Start","Stop","Diff","Trials")), drop = FALSE]

            # Prepare bin width for frequency if requested
            if (ReturnAs == "freq") {
              diffs <- diff(time)
              bw <- if (length(unique(diffs))==1) unique(diffs) else median(diffs)
            }

            # Expand grid of indices
            dims <- dim(cnts)
            idx <- expand.grid(
              binIdx   = seq_len(dims[1]),
              runIdx = seq_len(dims[2]),
              chIdx    = seq_len(dims[3])
            )

            # Extract values
            vals <- cnts[cbind(idx$binIdx, idx$runIdx, idx$chIdx)]
            if (ReturnAs == "freq") {
              vals <- vals / bw
            }
            #else {
            #   if (vals%%1==0){
            #     vals<-as.integer(vals)
            #   }
            # }

            # Build data.frame
            df <- data.frame(
              RunUID = idx$runIdx,
              Channel  = as.factor(channels[idx$chIdx]),
              Time     = time[idx$binIdx],
              Value    = vals,
              stringsAsFactors = FALSE
            )
            if (length(stim) == length(time)) {
              df$Stimulus <- stim[idx$binIdx]
            }

            # Attach metadata rows
            md <- meta[ match(df$RunUID, 1:nrow(meta)), , drop = FALSE ]

            # Combine meta + main df
            out <- cbind(md, df[c("Channel","Time",
                                  if (length(stim)==length(time)) "Stimulus",
                                  "Value")])

            # Type optimizations:
            # - integer columns
            for (nm in intersect(c("RecordingID","Trial","RunUID"), names(out))) {
              out[[nm]] <- as.integer(out[[nm]])
            }
            # - Channel as factor
            out$Channel <- factor(out$Channel)

            rownames(out) <- NULL
            out
          })

# -------- EPhysEvents method --------
#' @describeIn as.data.frame-methods Convert an \link{EPhysEvents} to long format.
#' @export
setMethod("as.data.frame",
          signature(x = "EPhysEvents"),
          function(x, ...) {

            meta <- Metadata(x)
            # Drop unneeded metadata columns
            meta <- meta[, setdiff(names(meta), c("Start","Stop","Diff","Trials")), drop = FALSE]

            ch_master <- as.character(Channels(x))
            n_rows    <- length(x@Data)

            # helpers
            channels_in_row <- function(i) {
              nm <- names(x@Data[[i]])
              if (!is.null(nm)) return(nm)
              # Fallback: if unnamed but lengths match, assume master order; else index labels
              if (length(ch_master) == length(x@Data[[i]])) ch_master else as.character(seq_along(x@Data[[i]]))
            }

            chunks <- vector("list", n_rows)  # we’ll grow inner lists per row
            out_idx <- 1L

            for (i in seq_len(n_rows)) {
              row_ch <- channels_in_row(i)
              row_list <- x@Data[[i]]

              inner <- vector("list", length(row_ch))
              has_any <- FALSE

              for (j in seq_along(row_ch)) {
                ch <- row_ch[j]
                ts <- row_list[[ch]]
                n_sp <- length(ts)
                if (!n_sp) { inner[[j]] <- NULL; next }
                has_any <- TRUE

                md_i <- meta[rep.int(i, n_sp), , drop = FALSE]
                inner[[j]] <- cbind(
                  md_i,
                  data.frame(
                    Channel   = ch,
                    Timestamp = as.numeric(ts),
                    stringsAsFactors = FALSE
                  )
                )
              }

              if (has_any) {
                chunks[[out_idx]] <- do.call(rbind, inner)
                out_idx <- out_idx + 1L
              }
            }

            if (out_idx == 1L) {
              # No spikes anywhere → return 0-row df with correct columns/classes
              out <- meta[0, , drop = FALSE]
              out$Channel   <- factor(character(0), levels = ch_master)
              out$Timestamp <- numeric(0)
              return(out)
            }

            out <- do.call(rbind, chunks)

            # Type niceties
            # - common integer-ish metadata columns
            for (nm in intersect(c("RecordingID","Trial","RunUID"), names(out))) {
              out[[nm]] <- as.integer(out[[nm]])
            }

            rownames(out) <- NULL
            out
          })
