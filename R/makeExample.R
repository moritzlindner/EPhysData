#' Create Example EPhysData Object
#'
#' These functions generate an example \code{EPhysData} or \code{EPhysSet} object for demonstration purposes.
#'
#' @param nrows Number of rows for the example data frame.
#' @param ncols Number of columns for the example data frame.
#' @param nsets Number of data sets to include into an \code{EPhysSet} object.
#' @param start_time The starting time for the time trace.
#' @param time_unit The time unit for the time trace.
#' @param data_unit The unit for the data values.
#' @param replicate_count The number of replicate columns in the data frame.
#' @param metadata_data The metadata data frame to be associated with the \code{EPhysSet}.
#' @return An \code{EPhysData} object with example data.
#' @name makeExample
#'
#' @export
makeExampleEPhysData <- function(nrows = 4, ncols = 3, start_time = 1, time_unit = "s", data_unit = "mV", replicate_count = 3) {

  Data <- data.frame(matrix(runif(nrows * replicate_count), ncol = replicate_count))
  colnames(Data) <- paste0("rep", 1:replicate_count)

  TimeTrace <- seq(start_time, length.out = nrows, by = 1)

  myEPhysData <- newEPhysData(Data = Data, TimeTrace = TimeTrace, Unit = data_unit, TimeUnit = time_unit)
  return(myEPhysData)
}

#' @describeIn makeExample makeExampleEPhysSet
#' @export
makeExampleEPhysSet <- function(nsets = 4, nrows = 4, ncols = 3, start_time = 1, time_unit = "s", data_unit = "mV", replicate_count = 3, metadata_data = NULL) {
  library(EPhysData)  # Assuming the library is loaded

  if (is.null(metadata_data)) {
    metadata_data <- data.frame(StepID = paste0("A", 1:nsets))
  }

  ephysDataList <- vector("list", nsets)
  for (i in 1:nsets) {
    ephysDataList[[i]] <- makeExampleEPhysData(nrows, ncols, start_time, time_unit, data_unit, replicate_count)
  }

  ephysSet <- new("EPhysSet", Data = ephysDataList, Metadata = metadata_data)
  return(ephysSet)
}
