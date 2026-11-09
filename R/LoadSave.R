#' Load/Save EPhysData or EPhysSet objects to or from  HDF5 files
#'
#' These functions load and save \linkS4class{EPhysData} or \linkS4class{EPhysSet} objects from or into HDF5 files.
#' @param X A \linkS4class{EPhysData} or \linkS4class{EPhysSet} object.
#' @param filename Path the data is read from or written to.
#' @param overwrite Should existing files be overwritten?
#' @param restore_callbacks Restore saved R callback definitions. Defaults to
#'   \code{FALSE}, which loads data with built-in callbacks. Use \code{TRUE}
#'   only for files you trust. Restored callback bodies can execute R code when used.
#' @return
#' * Save: Does not return any values.
#' * Load: An \linkS4class{EPhysData} or \linkS4class{EPhysSet} object.
#' @examples
#' fn<-tempfile()
#' SampleData<-makeExampleEPhysData()
#' Save(SampleData, fn, overwrite = TRUE)
#' SampleData
#' rm(SampleData)
#' SampleData<-Load(fn)
#' SampleData
#' Rejected(SampleData) <- function(x) rep(FALSE, ncol(x))
#' Save(SampleData, fn, overwrite = TRUE)
#' SampleData
#' rm(SampleData)
#' SampleData<-Load(fn, restore_callbacks = TRUE)
#' SampleData
#' rm(SampleData)
#' SampleData<-makeExampleEPhysSet()
#' Save(SampleData, fn, overwrite = TRUE)
#' SampleData
#' rm(SampleData)
#' SampleData<-Load(fn)
#' SampleData
#' @seealso \link[=EPhysData]{EPhysData}, \link[=EPhysSet]{EPhysSet}
#' @name LoadSave
#' @rdname LoadSave-methods
#' @docType methods
NULL
#'
#' @describeIn LoadSave-methods Save \linkS4class{EPhysData} or \linkS4class{EPhysSet} objects to an HDF5 file
#' @export
setGeneric(
  name = "Save",
  def = function(X,
                 filename,
                 overwrite = F)
  {
    standardGeneric("Save")
  }
)

#' @rdname LoadSave-methods
setMethod("Save", "EPhysData", function(X, filename, overwrite) {
  .ephys_save_file(X, filename, overwrite, Save.EPhysData)
})

#' @rdname LoadSave-methods
setMethod("Save", "EPhysSet", function(X, filename, overwrite) {
  .ephys_save_file(X, filename, overwrite, Save.EPhysSet)
})

.ephys_file_argument <- function(filename, operation) {
  if (!is.character(filename) || length(filename) != 1L ||
      is.na(filename) || !nzchar(filename)) {
    .ephys_abort(paste(operation, "requires one non-empty filename."))
  }
  path.expand(filename)
}

.ephys_file_operation <- function(expr, context) {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "ephys_io_error")) stop(e)
    .ephys_abort(context, "ephys_io_error", parent = e)
  })
}

.ephys_close_hdf <- function(con) {
  if (con$is_valid) con$close_all()
}

.ephys_save_file <- function(X, filename, overwrite, writer) {
  filename <- .ephys_file_argument(filename, "Save()")
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    .ephys_abort("Save() requires overwrite to be TRUE or FALSE.")
  }
  context <- sprintf("Save() failed for %s.", shQuote(filename))
  check_destination <- function() {
    if (dir.exists(filename)) {
      .ephys_abort(paste(context, "The destination is a directory."), "ephys_io_error")
    }
    if (file.exists(filename) && !overwrite) {
      .ephys_abort(paste(context, "The file already exists. Set overwrite = TRUE to replace it."),
                   "ephys_io_error")
    }
  }
  check_destination()
  temp <- tempfile(".ephys-", tmpdir = dirname(filename))
  on.exit(unlink(temp), add = TRUE)
  .ephys_file_operation({
    validObject(X)
    con <- hdf5r::H5File$new(temp, mode = "w")
    tryCatch(writer(con, X, context), finally = .ephys_close_hdf(con))
    check_destination()
    if (!suppressWarnings(file.rename(temp, filename))) {
      .ephys_abort(paste(context, "Could not move the completed file into place."),
                   "ephys_io_error")
    }
  }, context)
  invisible(NULL)
}

#' @importFrom utils packageVersion compareVersion
Save.EPhysData <- function(con, X, context) {
  .ephys_file_operation(validObject(X), context)
  con$create_dataset("Data", units::drop_units(X@Data), gzip_level = 9)
  con[["Units"]] <- units::deparse_unit(X@Data)
  con[["TimeTrace"]] <- units::drop_units(X@TimeTrace)
  con[["TimeUnit"]] <- units::deparse_unit(X@TimeTrace)
  con[["StimulusTrace"]] <- units::drop_units(X@StimulusTrace)
  con[["StimulusUnit"]] <- units::deparse_unit(X@StimulusTrace)
  con[["Rejected"]] <- paste(deparse(X@Rejected), collapse = "\n")
  con[["filter.fx"]] <- paste(deparse(X@filter.fx), collapse = "\n")
  con[["average.fx"]] <- paste(deparse(X@average.fx), collapse = "\n")
  con[["Created"]] <- X@Created
  con[["Type"]] <- "EPhysData"
  con[["EPhysDataVersion"]] <- as.character(packageVersion("EPhysData"))
  invisible(con)
}

Save.EPhysSet <- function(con, X, context) {
  recordings <- con$create_group("Data")
  for (i in seq_along(X@Data)) {
    record_context <- sprintf("%s Recording %d.", context, i)
    .ephys_file_operation(
      Save.EPhysData(recordings$create_group(as.character(i)), X@Data[[i]], record_context),
      record_context
    )
  }
  con$create_dataset("Metadata", X@Metadata)
  con[["Type"]] <- "EPhysSet"
  con[["EPhysDataVersion"]] <- as.character(packageVersion("EPhysData"))
  invisible(con)
}

.ephys_hdf_read <- function(con, name, context, ...) {
  .ephys_file_operation(con$open(name)$read(...),
                        sprintf("%s Could not read HDF5 field %s.", context, shQuote(name)))
}

#' @describeIn LoadSave-methods Load \linkS4class{EPhysData} or \linkS4class{EPhysSet} from HDF5.
#' @export
Load <- function(filename, restore_callbacks = FALSE) {
  filename <- .ephys_file_argument(filename, "Load()")
  if (!is.logical(restore_callbacks) || length(restore_callbacks) != 1L || is.na(restore_callbacks)) {
    .ephys_abort("Load() requires restore_callbacks to be TRUE or FALSE.")
  }
  context <- sprintf("Load() failed for %s.", shQuote(filename))
  if (!file.exists(filename) || dir.exists(filename)) {
    .ephys_abort(paste(context, "Expected an existing HDF5 file."), "ephys_io_error")
  }

  skipped <- character()
  callback_env <- parent.frame()
  read_callback <- function(con, name, default, record_context) {
    if (!con$exists(name)) return(default)
    text <- .ephys_hdf_read(con, name, record_context)
    if (!restore_callbacks) {
      compact <- gsub("[[:space:]{}]", "", paste(text, collapse = ""))
      default_code <- gsub("[[:space:]{}]", "", paste(deparse(default), collapse = ""))
      if (!compact %in% c(default_code, if (name != "Rejected") "function(x)return(x)")) skipped <<- c(skipped, name)
      return(default)
    }
    tryCatch({
      code <- parse(text = text, keep.source = FALSE)
      if (length(code) != 1L || !is.call(code[[1L]]) ||
          !identical(code[[1L]][[1L]], as.name("function"))) {
        stop("Expected a single function definition.", call. = FALSE)
      }
      eval(code[[1L]], envir = callback_env)
    }, error = function(e) {
      .ephys_warn(sprintf("%s Could not restore callback %s. Using the built-in default. %s",
                          record_context, shQuote(name), conditionMessage(e)),
                  "ephys_io_warning")
      default
    })
  }

  con <- .ephys_file_operation(hdf5r::H5File$new(filename, mode = "r"), context)
  on.exit(.ephys_close_hdf(con), add = TRUE)
  version <- .ephys_hdf_read(con, "EPhysDataVersion", context)
  .ephys_file_operation({
    if (compareVersion(version, as.character(packageVersion("EPhysData"))) > 0) {
      .ephys_warn(sprintf("Load(%s) is reading a file written by newer EPhysData %s.",
                          shQuote(filename), version), "ephys_io_warning")
    }
  }, paste(context, "Invalid EPhysDataVersion."))
  type <- .ephys_hdf_read(con, "Type", context)
  out <- .ephys_file_operation(
    switch(as.character(type),
           EPhysData = Load.EPhysData(con, context, read_callback),
           EPhysSet = Load.EPhysSet(con, context, read_callback),
           .ephys_abort(sprintf("%s Unsupported HDF5 Type %s. Expected EPhysData or EPhysSet.",
                                context, shQuote(toString(type))), "ephys_io_error")),
    context
  )
  if (length(skipped)) {
    .ephys_warn(sprintf("Load(%s) used built-in defaults for saved callbacks: %s. Use restore_callbacks = TRUE for trusted files.",
                        shQuote(filename), paste(unique(skipped), collapse = ", ")),
                "ephys_io_warning")
  }
  out
}

Load.EPhysData <- function(con, context, read_callback) {
  read <- function(name, ...) .ephys_hdf_read(con, name, context, ...)
  if (!identical(as.character(read("Type")), "EPhysData")) {
    .ephys_abort(paste(context, "Expected HDF5 Type EPhysData."), "ephys_io_error")
  }
  EPhysData(
    Data = as_units(read("Data", drop = FALSE), read("Units")),
    TimeTrace = as_units(read("TimeTrace"), read("TimeUnit")),
    StimulusTrace = as_units(read("StimulusTrace"), read("StimulusUnit")),
    Rejected = read_callback(con, "Rejected", .ephys_reject_none, context),
    filter.fx = read_callback(con, "filter.fx", .ephys_identity, context),
    average.fx = read_callback(con, "average.fx", .ephys_identity, context),
    Created = as.POSIXct(read("Created"), origin = "1970-01-01")
  )
}

Load.EPhysSet <- function(con, context, read_callback) {
  recordings <- .ephys_file_operation(con$open("Data"), paste(context, "Could not open HDF5 group Data."))
  ids <- names(recordings)
  expected <- as.character(seq_along(ids))
  if (!setequal(ids, expected)) {
    .ephys_abort(paste(context, "Recording groups must be numbered consecutively from 1."),
                 "ephys_io_error")
  }
  data <- base::lapply(expected, function(id) {
    record_context <- sprintf("%s Recording %s.", context, id)
    .ephys_file_operation(Load.EPhysData(recordings$open(id), record_context, read_callback),
                          record_context)
  })
  EPhysSet(Data = data, Metadata = .ephys_hdf_read(con, "Metadata", context))
}
