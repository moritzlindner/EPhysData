#' @rdname lapply-EPhys
setMethod("lapply",
          signature(X = "EPhysEvents", FUN = "function"),
          function(X,
                   FUN,
                   parallel = FALSE,
                   error = c("stop", "warn")[1],
                   progress = interactive(),
                   ...) {
            # Collect ... once so we can forward in both serial and parallel paths
            dots <- list(...)
            err_mode <- match.arg(error, c("stop", "warn"))

            dat <- X@Data
            n_runs <- length(dat)
            run_names <- names(dat)

            mc <- match.call()
            fun_expr <- mc$FUN

            fun_name <- if (is.null(fun_expr)) {
              "<unknown>"
            } else if (is.symbol(fun_expr)) {
              as.character(fun_expr)
            } else {
              paste(deparse(fun_expr), collapse = " ")
            }

            # --- Sequential path -----------------------------------------------------
            if (!isTRUE(parallel)) {
              if (progress) {
                total <- sum(vapply(dat, length, integer(1L)))
                pb_id <-
                  cli_progress_bar(total = total, format =  "Applying function {pb_bar} {pb_percent}")
                on.exit(try(cli_progress_done(id = pb_id), silent = TRUE)
                        , add = TRUE)
              }

              out <- vector("list", n_runs)
              names(out) <- run_names

              for (i in seq_len(n_runs)) {
                row <- dat[[i]]
                ch_out <- vector("list", length(row))
                names(ch_out) <- names(row)

                for (j in seq_along(row)) {
                  chname <- names(row)[j]
                  ts <- row[[j]]

                  res <- tryCatch(
                    do.call(FUN, c(list(ts), dots)),
                    error = function(e) {
                      msg <- sprintf(
                        "FUN '%s' error at run %s (index %d), channel '%s': %s",
                        fun_name,
                        if (is.null(run_names))
                          as.character(i)
                        else
                          run_names[[i]],
                        i,
                        chname %||% as.character(j),
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

                  ch_out[[j]] <- res
                  if (progress)
                    cli_progress_update()
                }

                out[[i]] <- ch_out
              }

              return(out)
            }

            # --- Parallel path (one future per run) ---------------------------------
            results <- future_lapply(
              X = seq_len(n_runs),
              FUN = function(i) {
                row <- dat[[i]]
                ch_out <- vector("list", length(row))
                names(ch_out) <- names(row)
                for (j in seq_along(row)) {
                  ts <- row[[j]]
                  # let errors propagate to stop the whole call in parallel mode
                  ch_out[[j]] <- do.call(FUN, c(list(ts), dots))
                }
                ch_out
              },
              future.scheduling = 1,  # suitable for both multicore and multisession
              future.seed = TRUE
            )

            names(results) <- run_names
            results
          })

#' @rdname lapply-EPhys
setMethod(
  "lapply",
  signature(X = "EPhysContinuous", FUN = "function"),
  function(X, FUN, parallel = FALSE, error = c("stop","warn")[1], progress = interactive(), ...) {

    dots <- list(...)

    mc <- match.call()
    fun_expr <- mc$FUN

    fun_name <- if (is.null(fun_expr)) {
      "<unknown>"
    } else if (is.symbol(fun_expr)) {
      as.character(fun_expr)
    } else {
      paste(deparse(fun_expr), collapse = " ")
    }

    err_mode <- match.arg(error, c("stop","warn"))

    d <- X@Data
    dims <- dim(d)
    n_time <- dims[1]; n_runs <- dims[2]; n_ch <- dims[3]

    # Derive run and channel names
    dn <- dimnames(d)
    run_names <- if (!is.null(dn$trial)) dn$trial else if ("RunUID" %in% names(X@Metadata)) as.character(X@Metadata$RunUID) else as.character(seq_len(n_runs))
    ch_names  <- if (!is.null(dn$channel)) dn$channel else X@Channels

    # -------- Sequential path --------
    if (!isTRUE(parallel)) {
      if (progress) {
        total <- n_runs * n_ch
        pb_id <- cli_progress_bar(total = total, format = "Applying function {pb_bar} {pb_percent}")
      }

      out <- vector("list", n_runs)
      names(out) <- run_names

      for (i in seq_len(n_runs)) {
        ch_out <- vector("list", n_ch)
        names(ch_out) <- ch_names

        for (j in seq_len(n_ch)) {
          ts <- d[, i, j]

          res <- tryCatch(
            do.call(FUN, c(list(ts), dots)),
            error = function(e) {
              msg <- sprintf(
                "FUN '%s' error at run %s (index %d), channel '%s': %s",
                fun_name, run_names[[i]], i, ch_names[[j]], conditionMessage(e)
              )
              if (identical(err_mode, "stop")) stop(msg, call. = FALSE)
              cli_warn(msg)
              NULL
            }
          )

          ch_out[[j]] <- res
          if (progress) cli_progress_update()
        }

        out[[i]] <- ch_out
      }

      if (progress) {
        try(cli_progress_done(id = pb_id), silent = TRUE)
      }

      return(out)
    }

    # -------- Parallel path (one future per run) --------
    results <- future_lapply(
      X = seq_len(n_runs),
      FUN = function(i) {
        ch_out <- vector("list", n_ch)
        names(ch_out) <- ch_names
        for (j in seq_len(n_ch)) {
          ts <- d[, i, j]
          # In parallel mode, let errors propagate to stop the whole call
          ch_out[[j]] <- do.call(FUN, c(list(ts), dots))
        }
        ch_out
      },
      future.scheduling = 1,
      future.seed = T
    )

    names(results) <- run_names
    results
  }
)

# helper: infix for defaulting when NULL
`%||%` <- function(a, b) {
  if (is.null(a))
    b
  else
    a
}

