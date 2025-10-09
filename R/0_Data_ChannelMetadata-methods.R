#' Channel Metadata Accessors for EPhysContainer
#'
#' Get or modify the \code{Channel_Metadata} slot of an \code{EPhysContainer}.
#' Optionally fetch a single column by name (returned as a vector aligned to \code{Channels}).
#'
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

## Accessor ----
#' @rdname ChannelMetadata
#' @noRd
#' @aliases ChannelMetadata,EPhysContainer-method
#' @export
setMethod("ChannelMetadata", "EPhysContainer", function(X, column = NULL, ...) {
  chm <-
    X@Channel_Metadata
  if (!is.data.frame(chm) ||
      nrow(chm) == 0L) {
    chm <-
      data.frame(row.names = X@Channels)
  }
  if (!is.null(rownames(chm)) &&
      setequal(rownames(chm), X@Channels)) {
    chm <-
      chm[match(X@Channels, rownames(chm)), , drop = FALSE]
  } else if ("Channel" %in% names(chm) &&
             setequal(chm$Channel, X@Channels)) {
    chm <-
      chm[match(X@Channels, chm$Channel), , drop = FALSE]
    rownames(chm) <-
      X@Channels
  } else if (nrow(chm) == length(X@Channels) &&
             is.null(rownames(chm))) {
    rownames(chm) <-
      X@Channels
  }
  if (!is.null(column)) {
    if (!(is.character(column) &&
          length(column) == 1L))
      stop("column must be a single character string.")
    if (!(column %in% colnames(chm)))
      stop(sprintf("Column '%s' not found in Channel_Metadata.", column))
    return(chm[[column]])
  }
  chm
})
## Adder ----
#' @rdname ChannelMetadata
#' @noRd
#' @aliases AddChannelMetadata,EPhysContainer-method
#' @export
setMethod("AddChannelMetadata", "EPhysContainer", function(X, columnName, columnData, ...) {
  if (!(is.character(columnName) &&
        length(columnName) == 1L))
    stop("columnName must be a single character string.")
  if (length(columnData) != length(X@Channels))
    stop("columnData length must match length(Channels(X)).")
  chm <-
    ChannelMetadata(X)
  chm[[columnName]] <-
    columnData
  X@Channel_Metadata <- chm
  validObject(X)
  X
})
## Replacement ----
#' @rdname ChannelMetadata
#' @noRd
#' @aliases ChannelMetadata<-,EPhysContainer-method
#' @export
setReplaceMethod("ChannelMetadata", signature(X = "EPhysContainer"), function(X, value) {
  if (!is.data.frame(value))
    stop("New channel metadata must be a data.frame.")
  if (nrow(value) != length(X@Channels))
      stop("New channel metadata must have the same number of rows as length(Channels(X)).")
  if (!is.null(rownames(value)) &&
      setequal(rownames(value), X@Channels)) {
    value <-
      value[match(X@Channels, rownames(value)), , drop = FALSE]
  } else if ("Channel" %in% names(value) &&
             setequal(value$Channel, X@Channels)) {
    value <-
      value[match(X@Channels, value$Channel), , drop = FALSE]
    rownames(value) <-
        X@Channels
  } else if (is.null(rownames(value))) {
    rownames(value) <-
      X@Channels
  } else {
    stop("Row names (or a 'Channel' column) of new channel metadata must match Channels(X).")
  }
  X@Channel_Metadata <- value
  validObject(X)
  X
})
