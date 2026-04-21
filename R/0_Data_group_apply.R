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
#' @aliases group_apply group_apply,EPhysContinuous-method group_apply,EPhysEvents-method
#' @name group_apply
#' @rdname group_apply
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done cli_warn
setGeneric("group_apply", function(X,
                                   FUN,
                                   parallel = FALSE,
                                   error = c("stop", "warn")[1],
                                   progress = interactive(),
                                   ...) {
  standardGeneric("group_apply")
})

#' @keywords internal
.group_indices_by_rec <- function(md) {
  split(seq_len(nrow(md)), md$RecordingID)
}


# ---------- EPhysContinuous ----------
setMethod("group_apply",
          signature(X = "EPhysContinuous", FUN = "function"),
          function(X,
                   FUN,
                   parallel = FALSE,
                   error = c("stop", "warn")[1],
                   progress = interactive(),
                   ...) {
            dots     <- list(...)
            err_mode <- match.arg(error, c("stop", "warn"))

            d      <- X@Data
            dims   <-
              dim(d)
            n_time <- dims[1]
            n_runs <- dims[2]
            n_ch <- dims[3]
            md     <- Metadata(X)
            rec_groups <-
              .group_indices_by_rec(md)        # from your helper
            run_names  <-
              as.character(md$RunUID)          # guaranteed present
            ch_names   <-
              Channels(X)                      # dimnames(d)$channel assumed NULL

            # (RecordingID × Channel) pairs
            combos <-
              expand.grid(
                rec = names(rec_groups),
                ch = ch_names,
                stringsAsFactors = FALSE
              )
            ch_index <- setNames(seq_along(ch_names), ch_names)

            eval_one <- function(i) {
              rec  <- combos$rec[[i]]
              ch   <- combos$ch[[i]]
              ridx <- rec_groups[[rec]]
              j    <- ch_index[[ch]]

              # [time × runs_in_recording] slice
              block <- d[, ridx, j, drop = FALSE]
              dim(block) <- c(n_time, length(ridx))
              colnames(block) <- run_names[ridx]

              do.call(FUN, c(list(block), dots))           # only mat + your ...
            }

            # execute
            if (isTRUE(parallel)) {
              res <-
                future_lapply(seq_len(nrow(combos)), eval_one, future.scheduling = 1, future.seed = TRUE)
            } else {
              if (progress) {
                pb <- cli_progress_bar(total = nrow(combos),
                                            format = "group_apply {pb_bar} {pb_percent}")
                on.exit(try(cli_progress_done(id = pb), silent = TRUE)
                        , add = TRUE)
              }
              res <- vector("list", nrow(combos))
              for (i in seq_len(nrow(combos))) {
                res[[i]] <- tryCatch(
                  eval_one(i),
                  error = function(e) {
                    msg <- sprintf(
                      "FUN error at RecordingID=%s, Channel=%s: %s",
                      combos$rec[[i]],
                      combos$ch[[i]],
                      conditionMessage(e)
                    )
                    if (identical(err_mode, "stop"))
                      stop(msg, call. = FALSE)
                    if (requireNamespace("cli", quietly = TRUE))
                      cli_warn(msg)
                    else
                      warning(msg, call. = FALSE)
                    NULL
                  }
                )
                if (progress)
                  cli_progress_update()
              }
            }

            # pack nested [RecordingID][Channel]
            out <-
              setNames(vector("list", length(rec_groups)), names(rec_groups))
            for (r in names(rec_groups))
              out[[r]] <- setNames(vector("list", length(ch_names)), ch_names)
            for (i in seq_len(nrow(combos)))
              out[[combos$rec[[i]]]][[combos$ch[[i]]]] <- res[[i]]
            out
          })


# ---------- EPhysEvents ----------
setMethod("group_apply",
          signature(X = "EPhysEvents", FUN = "function"),
          function(X,
                   FUN,
                   parallel = FALSE,
                   error = c("stop", "warn")[1],
                   progress = interactive(),
                   ...) {
            dots     <- list(...)
            err_mode <- match.arg(error, c("stop", "warn"))

            dat        <- X@Data
            md         <- Metadata(X)
            rec_groups <-
              .group_indices_by_rec(md)       # your helper
            run_names  <-
              as.character(md$RunUID)         # guaranteed present
            ch_names   <-
              Channels(X)                     # assume no channel dimnames

            # (RecordingID × Channel) pairs
            combos   <-
              expand.grid(
                rec = names(rec_groups),
                ch = ch_names,
                stringsAsFactors = FALSE
              )

            eval_one <- function(i) {
              rec  <- combos$rec[[i]]
              ch   <- combos$ch[[i]]
              ridx <- rec_groups[[rec]]

              # list of spike vectors (one per run in the recording), named by RunUID
              spike_list <- lapply(ridx, function(k)
                dat[[k]][[ch]])
              names(spike_list) <- run_names[ridx]

              do.call(FUN, c(list(spike_list), dots))     # pass only 'mat' (list) + your ...
            }

            # execute
            if (isTRUE(parallel)) {
              res <-
                future_lapply(seq_len(nrow(combos)), eval_one, future.scheduling = 1)
            } else {
              if (progress) {
                pb <- cli_progress_bar(total = nrow(combos),
                                            format = "group_apply {pb_bar} {pb_percent}")
                on.exit(try(cli_progress_done(id = pb), silent = TRUE)
                        , add = TRUE)
              }
              res <- vector("list", nrow(combos))
              for (i in seq_len(nrow(combos))) {
                res[[i]] <- tryCatch(
                  eval_one(i),
                  error = function(e) {
                    msg <- sprintf(
                      "FUN error at RecordingID=%s, Channel=%s: %s",
                      combos$rec[[i]],
                      combos$ch[[i]],
                      conditionMessage(e)
                    )
                    if (identical(err_mode, "stop"))
                      stop(msg, call. = FALSE)
                    if (requireNamespace("cli", quietly = TRUE))
                      cli_warn(msg)
                    else
                      warning(msg, call. = FALSE)
                    NULL
                  }
                )
                if (progress)
                  cli_progress_update()
              }
            }

            # pack nested [RecordingID][Channel]
            out <-
              setNames(vector("list", length(rec_groups)), names(rec_groups))
            for (r in names(rec_groups))
              out[[r]] <- setNames(vector("list", length(ch_names)), ch_names)
            for (i in seq_len(nrow(combos)))
              out[[combos$rec[[i]]]][[combos$ch[[i]]]] <- res[[i]]
            out
          })
