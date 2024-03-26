#' as.data.frame for EPhysData and EPhysSet
#'
#' Converts an \link{EPhysData} or \link{EPhysSet} object to a data frame format. Each repeat of the data will be represented in a long format with columns for repeat number, time, stimulus (if avaliable), and value.
#'
#' @param x An \link{EPhysData} or \link{EPhysSet} object.
#' @inheritParams GetData
#' @param IncludeRejected Logical. If \code{TRUE}, rejected trials will be included; if \code{FALSE} (default), rejected trials will be dropped.
#' @param ... currently unused.
#'
#' @return A data frame representing the \link{EPhysData} object in long format. Note that, by default, rejected Trials are excluded.
#'
#' @examples
#' # Create an example EPhysData object
#' new_data <- makeExampleEPhysData()
#' # Convert EPhysData to a data frame
#' new_data <- as.data.frame(new_data)
#' head(new_data)
#'
#' # Create an example EPhysSet object
#' new_data <- makeExampleEPhysSet()
#' new_data <- as.data.frame(new_data)
#' head(new_data)
#'
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom units drop_units as_units
#' @docType method
#' @name as.data.frame
#' @rdname as.data.frame-methods
NULL

#' @describeIn as.data.frame-methods Method for EPhysData
#' @export as.data.frame
setMethod("as.data.frame",
          "EPhysData",
          function(x,
                   Raw = T,
                   IncludeRejected = F,
                   ...) {
            if(IncludeRejected){
              dat <- GetData(x, Raw = Raw, Trials = !logical(dim(x)[2]))
            } else {
              dat <- GetData(x, Raw = Raw)
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
              paste0("Repeat_", 1:length(colnames(dat)))
            dat$Time <- time
            if (!is.null(stim)) {
              dat$Stimulus <- stim
            }
            dat <- pivot_longer(
              dat,
              starts_with("Repeat_"),
              names_prefix = "Repeat_",
              names_to = "Repeat",
              names_transform = list(Repeat = as.numeric),
              values_to = "Value"
            )
            dat$Value <- as_units(dat$Value, dat_units)
            dat$Time <- as_units(dat$Time, time_units)
            if (!is.null(stim)) {
              dat$Stimulus <- as_units(dat$Stimulus, stim_units)
              dat <- dat[, c("Repeat", "Time", "Stimulus", "Value")]
            }else{
              dat <- dat[, c("Repeat", "Time", "Value")]
            }
            dat <- dat[order(dat$Repeat, dat$Time), ]
            return(as.data.frame(dat))
          })

#' @describeIn as.data.frame-methods Method for EPhysSet
#' @export as.data.frame
setMethod("as.data.frame",
          "EPhysSet",
          function(x,
                   Raw = T) {
            data.list <- lapply(x, as.data.frame, ReturnEPhysSet = F)
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
