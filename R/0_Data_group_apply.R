#' Apply over repeated runs for an individual Recording
#'
#' For EPhysContinuous, FUN receives a numeric matrix time × runs_in_recording.
#' For EPhysEvents, FUN receives a list of numeric vectors (timestamps), one per run.
#'
#'
#' @param X An \code{EPhysContinuous} or \code{EPhysEvents} object.
#' @param FUN A function of the form \code{function(mat, ...)}. See \emph{Details}
#'   for the precise type of \code{mat} in each class.
#' @inheritParams lapply-EPhys
#' @param ... Additional arguments passed to \code{FUN}.
#'
#' @return A nested list \code{out[[RecordingID]][[Channel]]}, where each leaf is
#'   the value returned by \code{FUN(mat, ...)} for that group. Names of the
#'   outer level are the distinct \code{RecordingID}s; inner names are
#'   \code{Channels(X)}.
#'
#' @section Methods:
#' \itemize{
#'   \item \code{group_apply,EPhysContinuous-method}: builds the time-by-runs matrix.
#'   \item \code{group_apply,EPhysEvents-method}: builds the per-run timestamp list.
#' }
#'
#' @examples
#' \dontrun{
#' ## Continuous: time-wise mean across runs for each (RecordingID, Channel)
#' res_c <- group_apply(Xc, function(mat) rowMeans(mat, na.rm = TRUE))
#'
#' ## Events: total spike count per (RecordingID, Channel)
#' res_e <- group_apply(Xe, function(mat) sum(lengths(mat)))
#'
#' ## Events: per-run spike counts (named by RunUID)
#' res_e2 <- group_apply(Xe, function(mat) lengths(mat))
#' }
#'
#' @aliases group_apply group_apply,EPhysContinuous,function-method group_apply,EPhysEvents,function-method
#' @name group_apply
#' @rdname group_apply
#' @export
#' @importFrom stats setNames
setGeneric("group_apply", function(X,
                                   FUN,
                                   parallel = FALSE,
                                   error = c("stop", "warn")[1],
                                   progress = interactive(),
                                   ...) {
  standardGeneric("group_apply")
})

.ephys_group_apply <- function(X, FUN, dots, error, parallel, progress, get_block) {
  validObject(X)
  md <- Metadata(X)
  groups <- split(seq_len(nrow(md)), md$RecordingID)
  channels <- Channels(X)
  combos <- expand.grid(recording = names(groups), channel = channels,
                        stringsAsFactors = FALSE)

  eval_group <- function(i) {
    recording <- combos$recording[i]
    channel <- combos$channel[i]
    context <- sprintf("group_apply(%s) failed for RecordingID %s, channel %s.",
                       class(X)[1L], recording, shQuote(channel))
    .ephys_callback(FUN, c(list(get_block(groups[[recording]], channel)), dots),
                    context, error)
  }
  result <- .ephys_map(seq_len(nrow(combos)), eval_group, parallel, progress, "group_apply")
  out <- setNames(base::lapply(groups, function(rows) {
    setNames(vector("list", length(channels)), channels)
  }), names(groups))
  for (i in seq_along(result)) {
    out[[combos$recording[i]]][combos$channel[i]] <- result[i]
  }
  out
}

setMethod("group_apply", signature(X = "EPhysContinuous", FUN = "function"),
          function(X, FUN, parallel = FALSE, error = c("stop", "warn")[1],
                   progress = interactive(), ...) {
            dat <- X@Data
            channels <- Channels(X)
            run_names <- as.character(Metadata(X)$RunUID)
            .ephys_group_apply(
              X, FUN, list(...), match.arg(error, c("stop", "warn")), parallel, progress,
              function(rows, channel) {
                block <- dat[, rows, match(channel, channels), drop = FALSE]
                matrix(block, nrow = dim(dat)[1L], ncol = length(rows),
                       dimnames = list(NULL, run_names[rows]))
              }
            )
          })

setMethod("group_apply", signature(X = "EPhysEvents", FUN = "function"),
          function(X, FUN, parallel = FALSE, error = c("stop", "warn")[1],
                   progress = interactive(), ...) {
            dat <- X@Data
            run_names <- as.character(Metadata(X)$RunUID)
            .ephys_group_apply(
              X, FUN, list(...), match.arg(error, c("stop", "warn")), parallel, progress,
              function(rows, channel) {
                setNames(base::lapply(rows, function(i) dat[[i]][[channel]]), run_names[rows])
              }
            )
          })
