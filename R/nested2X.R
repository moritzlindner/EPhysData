#' Pack nested per-run/channel metrics into a wide data.frame
#'
#' Converts a nested list of per-channel metric vectors (one sub-list per run)
#' into a 3D array \code{[channel × metric × run]} and then reshapes it to a
#' wide \code{data.frame} with one row per \code{RunUID × Channel}.
#'
#' @param X An object that provides \code{Channels()} and \code{Metadata()}
#'   (e.g., an \code{EPhysContinuous} from \pkg{EPhysData}). \code{Metadata(X)}
#'   must contain columns \code{RunUID} and \code{RecordingID}.
#' @param nestedlist A list of length \emph{nRuns}; each element is a named list
#'   of channels where each channel entry is a \emph{named numeric vector} of
#'   scalar metrics (same metric names across channels and runs).
#'
#' @return A \code{data.frame} with columns \code{RunUID}, \code{RecordingID},
#'   \code{Channel}, and one column per metric.
#'
#' @details
#' Assumptions (no validation here):
#' \itemize{
#'   \item Each run contains all channels in \code{Channels(X)}.
#'   \item Each channel returns a named numeric vector with identical metric
#'         names across channels and runs.
#'   \item Values are scalar numerics (NA allowed).
#' }
#' Run labels are taken from the third array dim created by \code{melt()}—
#' if absent, they will be auto-indexed.
#'
#' @importFrom reshape2 melt dcast
#' @importFrom EPhysData Channels Metadata
#' @export
nested2df <- function(X, nestedlist) {
  md <- Metadata(X)
  ch <- Channels(X)

  run_dfs <- lapply(seq_along(nestedlist), function(i) {
    run <- nestedlist[[i]]                 # list: one element per channel
    if (!is.null(names(run))) run <- run[ch]  # reorder to Channels(X) if named

    # 1 row per channel; columns are metrics (types preserved)
    rows <- lapply(run, function(v)
      as.data.frame(as.list(v), stringsAsFactors = FALSE, check.names = FALSE))

    df <- do.call(rbind, rows)

    # use a proper Channel column (don’t touch rownames)
    chan_names <- if (!is.null(names(run))) names(run) else ch[seq_along(run)]
    n_per_chan <- vapply(rows, nrow, integer(1))

    df$Channel <- rep(chan_names, times = n_per_chan)
    runid <- if (!is.null(names(nestedlist))) names(nestedlist)[i] else md$RunUID[i]
    df$RunUID  <- rep(runid, nrow(df))

    df
  })

  metrics_df <- do.call(rbind, run_dfs)
  rownames(metrics_df) <- NULL

  # add RecordingID and order columns
  metrics_df$RecordingID <- md$RecordingID[match(metrics_df$RunUID, md$RunUID)]
  fixed <- c("RunUID", "RecordingID", "Channel")
  metrics_df <- metrics_df[, c(fixed, setdiff(names(metrics_df), fixed)), drop = FALSE]

  metrics_df
}



#' Pack nested per-recording/channel vectors into a [time × trial × channel] array
#'
#' Converts a nested list indexed by \emph{RecordingID × Channel} into a numeric
#' array matching the \code{EPhysContinuous@Data} convention \code{[time × trial × channel]}.
#'
#' @param nestedlist A nested list \code{nestedlist[[RecordingID]][[Channel]]}
#'   where each leaf is a numeric vector of identical length.
#'
#' @return A numeric array \code{[time × trial × channel]}.
#'   Dimnames (when available):
#'   \itemize{
#'     \item \code{time}: taken from element names if present and consistent; otherwise \code{NULL}.
#'     \item \code{trial}: taken from \code{names(nestedlist)}; auto-indexed if unnamed.
#'     \item \code{channel}: taken from the first recording if channel names are present; otherwise \code{NULL}.
#'   }
#'
#' @details
#' Assumptions (validated; no reordering is performed):
#' \itemize{
#'   \item Each recording contains the same channels in the \emph{same order}
#'         as the first recording. If channel names are present, they must be
#'         present for all recordings and be \strong{identical and ordered identically};
#'         otherwise all must be unnamed.
#'   \item Every leaf is a \strong{numeric} vector and all leaves share the
#'         \strong{same length} (interpreted as the time dimension).
#'   \item Optional element names (on the leaf vectors) must be consistent to be
#'         used as the \code{time} dimnames.
#' }
#'
#' @examples
#' \dontrun{
#' # nestedlist[[RecordingID]][[Channel]] -> numeric vector (time)
#' arr <- nested2array(nestedlist)  # [time × trial × channel]
#' }
#'
#' @export
nested2array <- function(nestedlist) {
  stopifnot(is.list(nestedlist), length(nestedlist) > 0L)

  rec_names <- names(nestedlist)
  if (is.null(rec_names)) rec_names <- as.character(seq_along(nestedlist))

  ch_first <- nestedlist[[1L]]
  stopifnot(is.list(ch_first), length(ch_first) > 0L)

  ch_names <- names(ch_first)             # may be NULL, but must be consistent
  n_ch     <- length(ch_first)

  v_first <- ch_first[[1L]]
  if (!is.numeric(v_first)) stop("Leaf vectors must be numeric.")
  k <- length(v_first)
  time_names <- names(v_first)
  if (!is.null(time_names) && length(time_names) != k) time_names <- NULL

  # Validate structure for every recording; no reordering permitted
  for (r in seq_along(nestedlist)) {
    xr <- nestedlist[[r]]
    if (!is.list(xr) || length(xr) != n_ch)
      stop("All recordings must have the same number of channels as the first recording.")
    xn <- names(xr)
    if (xor(is.null(xn), is.null(ch_names)))
      stop("Channel names must be consistently present (or absent) across recordings.")
    if (!is.null(ch_names) && !identical(xn, ch_names))
      stop("Channel names and order must be identical across recordings.")
    ok <- vapply(xr, function(v) is.numeric(v) && length(v) == k, logical(1L))
    if (!all(ok)) stop("All leaves must be numeric vectors of identical length.")
  }

  # Allocate: [time × trial(RecordingID) × channel]
  arr <- array(NA_real_,
               dim = c(k, length(nestedlist), n_ch),
               dimnames = list(time    = time_names,
                               trial   = rec_names,
                               channel = ch_names))

  for (ri in seq_along(nestedlist)) {
    xr <- nestedlist[[ri]]
    for (ci in seq_len(n_ch)) {
      arr[, ri, ci] <- xr[[ci]]
    }
  }
  arr
}
