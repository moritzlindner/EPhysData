#' Channel Metadata Accessors for EPhysContainer
#'
#' Get or modify the \code{Channel_Metadata} slot of an \code{EPhysContainer}.
#' Optionally fetch a single column by name (returned as a vector aligned to \code{Channels}).
#'
#' @param X An \code{EPhysContainer} object.
#' @param column Optional metadata column name.
#' @param columnName Name of the column to add.
#' @param columnData One value per channel in \code{Channels(X)} order.
#' @param value A data frame with one row per channel, or an empty data frame.
#'   Rows can be keyed by row names or a \code{Channel} column. Automatic row
#'   names without a key are interpreted in \code{Channels(X)} order.
#' @param ... Reserved for method compatibility.
#' @name ChannelMetadata
#' @title Channel Metadata Accessors for EPhysContainer
#' @return
#' \describe{
#'   \item{\code{ChannelMetadata(X)}}{The channel metadata data.frame.}
#'   \item{\code{ChannelMetadata(X, column="...")}}{A vector ordered by \code{Channels(X)}.}
#'   \item{\code{AddChannelMetadata(X, columnName, columnData)}}{Modified object.}
#'   \item{\code{ChannelMetadata(X) <- value}}{Modified object.}
#' }
#' @aliases ChannelMetadata AddChannelMetadata ChannelMetadata<-
NULL

## Generics ----
#' @rdname ChannelMetadata
#' @export
setGeneric("ChannelMetadata", function(X, column = NULL, ...)
  standardGeneric("ChannelMetadata"))

#' @rdname ChannelMetadata
#' @export
setGeneric("AddChannelMetadata", function(X, columnName, columnData, ...)
  standardGeneric("AddChannelMetadata"))

#' @rdname ChannelMetadata
#' @export
setGeneric("ChannelMetadata<-", function(X, value)
  standardGeneric("ChannelMetadata<-"))

.ephys_align_channel_metadata <- function(value, channels, operation) {
  if (!is.data.frame(value)) {
    .ephys_abort(sprintf("%s requires a data frame. Received %s.",
                         operation, paste(class(value), collapse = "/")))
  }
  if (!nrow(value)) return(data.frame(row.names = channels))
  if (nrow(value) != length(channels)) {
    .ephys_abort(sprintf("%s received %d rows. Expected %d, one per channel.",
                         operation, nrow(value), length(channels)))
  }

  keys <- rownames(value)
  if ("Channel" %in% names(value)) {
    column_keys <- as.character(value$Channel)
    if (setequal(keys, channels) && !identical(keys, column_keys)) {
      .ephys_abort(paste(operation, "found conflicting row names and Channel values."))
    }
    keys <- column_keys
  } else if (.row_names_info(value, type = 1L) < 0L) {
    keys <- channels
  }
  if (anyDuplicated(keys) || !setequal(keys, channels)) {
    missing <- setdiff(channels, keys)
    unexpected <- setdiff(keys, channels)
    .ephys_abort(sprintf("%s requires one unique key per channel. Missing: %s. Unexpected: %s.",
                         operation, toString(shQuote(missing)), toString(shQuote(unexpected))))
  }
  out <- value[match(channels, keys), , drop = FALSE]
  rownames(out) <- channels
  out
}

#' @rdname ChannelMetadata
#' @aliases ChannelMetadata,EPhysContainer-method
#' @export
setMethod("ChannelMetadata", "EPhysContainer", function(X, column = NULL, ...) {
  operation <- sprintf("ChannelMetadata(%s)", class(X)[1L])
  metadata <- .ephys_align_channel_metadata(X@Channel_Metadata, Channels(X), operation)
  if (is.null(column)) return(metadata)
  if (!is.character(column) || length(column) != 1L || is.na(column)) {
    .ephys_abort(paste(operation, "requires one column name."))
  }
  if (!column %in% names(metadata)) {
    .ephys_abort(sprintf("%s could not find column %s.", operation, shQuote(column)))
  }
  metadata[[column]]
})

#' @rdname ChannelMetadata
#' @aliases AddChannelMetadata,EPhysContainer-method
#' @export
setMethod("AddChannelMetadata", "EPhysContainer", function(X, columnName, columnData, ...) {
  if (!is.character(columnName) || length(columnName) != 1L ||
      is.na(columnName) || !nzchar(columnName)) {
    .ephys_abort("AddChannelMetadata() requires one non-empty columnName.")
  }
  if (length(columnData) != length(Channels(X))) {
    .ephys_abort(sprintf("AddChannelMetadata() received %d values for %s. Expected %d.",
                         length(columnData), shQuote(columnName), length(Channels(X))))
  }
  metadata <- ChannelMetadata(X)
  metadata[[columnName]] <- columnData
  ChannelMetadata(X) <- metadata
  X
})

#' @rdname ChannelMetadata
#' @aliases ChannelMetadata<-,EPhysContainer-method
#' @export
setReplaceMethod("ChannelMetadata", signature(X = "EPhysContainer"), function(X, value) {
  operation <- sprintf("ChannelMetadata<-(%s)", class(X)[1L])
  X@Channel_Metadata <- .ephys_align_channel_metadata(value, Channels(X), operation)
  validObject(X)
  X
})
