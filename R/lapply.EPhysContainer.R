#' @rdname lapply-EPhys
setMethod("lapply", signature(X = "EPhysEvents", FUN = "function"),
          function(X, FUN, parallel = FALSE, error = c("stop", "warn")[1],
                   progress = interactive(), ...) {
            dat <- X@Data
            channels <- Channels(X)
            .ephys_lapply_runs(
              X, FUN, list(...), match.arg(error, c("stop", "warn")), parallel, progress,
              function(i, j) dat[[i]][[channels[j]]]
            )
          })

#' @rdname lapply-EPhys
setMethod("lapply", signature(X = "EPhysContinuous", FUN = "function"),
          function(X, FUN, parallel = FALSE, error = c("stop", "warn")[1],
                   progress = interactive(), ...) {
            dat <- X@Data
            .ephys_lapply_runs(
              X, FUN, list(...), match.arg(error, c("stop", "warn")), parallel, progress,
              function(i, j) dat[, i, j]
            )
          })

.ephys_lapply_runs <- function(X, FUN, dots, error, parallel, progress, get_trace) {
  validObject(X)
  md <- Metadata(X)
  channels <- Channels(X)
  run_names <- as.character(md$RunUID)
  eval_run <- function(i) {
    result <- base::lapply(seq_along(channels), function(j) {
      context <- sprintf("lapply(%s) failed for RunUID %s, RecordingID %s, channel %s.",
                         class(X)[1L], run_names[i], md$RecordingID[i], shQuote(channels[j]))
      .ephys_callback(FUN, c(list(get_trace(i, j)), dots), context, error)
    })
    names(result) <- channels
    result
  }
  result <- .ephys_map(seq_along(run_names), eval_run, parallel, progress, "lapply")
  names(result) <- run_names
  result
}
