#' Apply a Function over EPhys Containers
#'
#' Methods for applying a function `FUN` to elements of electrophysiology
#' container classes in this package.
#'
#' For `EPhysEvents` and `EPhysContinuous`, `FUN` is applied per run × per channel:
#' \itemize{
#'   \item For \code{EPhysEvents}: each element is a numeric vector of event timestamps
#'         (possibly length 0).
#'   \item For \code{EPhysContinuous}: each element is the time series
#'         \code{Data[, run, channel]}.
#' }
#'
#' For `EPhysSet`, `FUN` is applied to each element of \code{X@Data}. Depending on
#' \code{ReturnEPhysSet}, the result is returned either as a modified
#' \code{EPhysSet} or as a list.
#'
#' @name lapply-EPhys
#' @rdname lapply-EPhys
#' @docType methods
#'
#' @param X
#'   For \code{lapply(X = "EPhysEvents")} or \code{"EPhysContinuous"}:
#'   an object whose runs × channels are iterated.
#'
#'   For \code{lapply(X = "EPhysSet")}: an \code{EPhysSet} object.
#'
#' @param FUN
#'   For \code{EPhysEvents} / \code{EPhysContinuous}:
#'   function applied to each per-run × per-channel vector.
#'
#'   For \code{EPhysSet}:
#'   function applied to the elements of \code{X@Data}.
#'
#' @param parallel logical.
#'   For \code{EPhysEvents} / \code{EPhysContinuous} methods only:
#'   if \code{TRUE}, use \code{future.apply::future_lapply()} over runs.
#'
#' @param error character.
#'   For \code{EPhysEvents} / \code{EPhysContinuous} methods only:
#'   one of \code{c("stop", "warn")}. Used in sequential mode; in
#'   \code{"warn"} mode, errors are warned and the corresponding result
#'   is set to \code{NULL}.
#'
#' @param progress logical.
#'   For \code{EPhysEvents} / \code{EPhysContinuous} methods only:
#'   show a \code{cli} progress bar in sequential mode.
#'
#' @param ReturnEPhysSet logical.
#'   For the \code{EPhysSet} method only:
#'   if \code{TRUE}, return a modified \code{EPhysSet} object with
#'   \code{FUN} applied; if \code{FALSE}, return a list of results.
#'
#' @param ... Additional arguments passed to \code{FUN}.
#'
#' @return
#' \describe{
#'   \item{\code{EPhysEvents} / \code{EPhysContinuous}}{
#'     A nested list mirroring the run × channel structure:
#'     \code{out[[run]][[channel]]}, with names taken from the run and
#'     channel dimension names (or corresponding metadata).
#'   }
#'   \item{\code{EPhysSet}}{
#'     If \code{ReturnEPhysSet = TRUE}, a modified \code{EPhysSet} object.
#'     Otherwise, a list (or other structure) containing the results of \code{FUN}.
#'   }
#' }
#'
#' @section Methods (by class):
#' \describe{
#'   \item{\code{lapply(X = "EPhysEvents")}}{
#'     Apply \code{FUN} to each per-run × per-channel event timestamp vector.
#'   }
#'   \item{\code{lapply(X = "EPhysContinuous")}}{
#'     Apply \code{FUN} to each per-run × per-channel continuous trace.
#'   }
#'   \item{\code{lapply(X = "EPhysSet")}}{
#'     Apply \code{FUN} to elements of \code{X@Data}; optionally
#'     return a modified \code{EPhysSet}.
#'   }
#' }
#'
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done cli_warn pb_bar pb_percent
#' @importFrom future.apply future_lapply
#' @importFrom methods validObject
#'
#' @exportMethod lapply
setMethod("lapply",
          "EPhysSet",
          function(X, FUN, ReturnEPhysSet = T) {
            dat <- lapply(X@Data, FUN)
            if (ReturnEPhysSet) {
              X@Data <- dat
              if (!validObject(X)) {
                stop(
                  paste(
                    "Applying to EPhysSet",
                    deparse(substitute(X)),
                    "failed. No valid EPhysSet object returned"
                  )
                )
              }
              return(X)
            } else{
              return(dat)
            }
          })
