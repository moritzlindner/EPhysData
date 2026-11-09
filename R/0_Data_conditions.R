.ephys_abort <- function(message, class = "ephys_invalid_argument", parent = NULL) {
  cli::cli_abort("{message}", class = class, parent = parent, call = NULL)
}

.ephys_warn <- function(message, class) {
  cli::cli_warn("{message}", class = class, call = NULL)
}

.ephys_callback <- function(FUN, args, context, error = "stop") {
  tryCatch(do.call(FUN, args), error = function(e) {
    if (identical(error, "stop")) {
      .ephys_abort(context, "ephys_callback_error", parent = e)
    }
    .ephys_warn(paste(context, conditionMessage(e)), "ephys_callback_warning")
    NULL
  })
}

.ephys_map <- function(indices, FUN, parallel, progress, label) {
  if (!length(indices)) return(list())
  if (isTRUE(parallel)) {
    return(future.apply::future_lapply(indices, FUN, future.scheduling = 1, future.seed = TRUE))
  }
  if (!progress) return(base::lapply(indices, FUN))

  id <- cli::cli_progress_bar(name = label, total = length(indices))
  on.exit(cli::cli_progress_done(id = id), add = TRUE)
  base::lapply(indices, function(i) {
    result <- FUN(i)
    cli::cli_progress_update(id = id)
    result
  })
}
