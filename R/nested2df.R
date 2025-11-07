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
#' @noRd
#' @importFrom reshape2 melt dcast
#' @importFrom EPhysData Channels Metadata
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
    chan <- if (!is.null(names(run))) names(run) else ch[seq_len(nrow(df))]
    df$Channel <- chan
    df$RunUID  <- if (!is.null(names(nestedlist))) names(nestedlist)[i] else md$RunUID[i]
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

