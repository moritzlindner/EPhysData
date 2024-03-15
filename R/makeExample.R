#' Create Example EPhysData Object
#'
#' These functions generate an example \code{EPhysData} or \code{EPhysSet} object for demonstration purposes.
#'
#' @param sample_points Number of rows for the example data frame.
#' @param nsets Number of data sets to include into an \code{EPhysSet} object.
#' @param start_time The starting time for the time trace.
#' @param time_unit The time unit for the time trace.
#' @param data_unit The unit for the data values.
#' @param replicate_count The number of replicate columns in the data frame.
#' @param metadata_data The metadata data frame to be associated with the \code{EPhysSet}.
#' @return An \code{EPhysData} object with example data.
#' @importFrom stats runif
#' @name makeExample
#'
#' @export
makeExampleEPhysData <-
  function(sample_points = sample(seq(1, 400, 10), 1),
           start_time = 0,
           time_unit = "ms",
           data_unit = "mV",
           replicate_count = sample(3:7, 1)) {

    signal<-sin(seq(0, length.out = sample_points, by = 0.1))

    Data<-data.frame(matrix(NA,nrow=sample_points,ncol = replicate_count))
    for (i in 1:ncol(Data)){
      Data[,i]<-signal+(runif(sample_points, min=-0.3, max=+0.3))
    }

    colnames(Data) <- paste0("rep", 1:replicate_count)

    TimeTrace <- seq(start_time, length.out = sample_points, by = 1)

    myEPhysData <-
      newEPhysData(
        Data = Data,
        TimeTrace = TimeTrace,
        Unit = data_unit,
        TimeUnit = time_unit
      )
    return(myEPhysData)
  }

#' @describeIn makeExample makeExampleEPhysSet
#' @importFrom methods new
#' @export
makeExampleEPhysSet <-
  function(nsets = 4,
           sample_points = sample(seq(1, 400, 10), 1),
           start_time = 1,
           time_unit = "s",
           data_unit = "mV",
           replicate_count = sample(2:6, 1),
           metadata_data = NULL) {
    library(EPhysData)  # Assuming the library is loaded

    if (is.null(metadata_data)) {
      metadata_data <- data.frame(StepID = paste0("A", 1:nsets))
    }

    ephysDataList <- vector("list", nsets)
    for (i in 1:nsets) {
      ephysDataList[[i]] <-
        makeExampleEPhysData(sample_points = sample_points,
                             start_time = start_time,
                             time_unit = time_unit,
                             data_unit = data_unit,
                             replicate_count = replicate_count)
    }

    ephysSet <-
      new("EPhysSet", Data = ephysDataList, Metadata = metadata_data)
    return(ephysSet)
  }
