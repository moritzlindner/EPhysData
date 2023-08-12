#' Store raw electrophysiological data into a \link{EPhysData} object.
#'
#' This function takes in raw electrophysiological stores them into a \link{EPhysData} object.
#'
#' @param Data A matrix or data.frame containing the raw electrophysiological data.
#'             Columns represent repeated measurements.
#' @param TimeTrace A numeric vector representing the time trace of the data.
#' @param Unit A character string representing the unit of the electrophysiological data.
#'             It should be a basic or derived SI unit. If the unit has a prefix, it must be converted
#'             into a factor and stored in the 'Factor' slot of the output object. If 'Data' inherits the type 'units', this parameter is optional.
#' @param TimeUnit A character string representing the unit of time for the time trace.
#'                It should be one of 's', 'min', 'h', 'd', or 'w'. If 'Data' inherits the type 'units', this parameter is optional.
#' @param StimulusTrace A numeric vector representing the stimulus trace (optional).
#' @param StimulusUnit A character string representing the unit of the stimulus data (optional).
#'                     It should be a basic or derived SI unit. If the unit has a prefix, it must be converted
#'                     into a factor and stored in the 'StimulusFactor' slot of the output object. If 'Data' inherits the type 'units', this parameter is optional.
#' @return An object of class \link{EPhysData} containing the raw electrophysiological data and other information.
#' @examples
#' Data <- data.frame(rep1 = c(0.5, 0.52, 0.48, 0.49),
#'                    rep2 = c(0.55, 0.57, 0.54, 0.56),
#'                    rep3 = c(0.48, 0.47, 0.46, 0.48))
#' TimeTrace <- c(1, 2, 3, 4)
#' Unit <- "mV"
#' TimeUnit <- "s"
#' newEPhysData(Data = Data, TimeTrace = TimeTrace, Unit = Unit, TimeUnit = TimeUnit)
#' # Output: An object of class 'EPhysData' with processed data and information.
#' @importFrom  units deparse_unit as_units unitless
#' @seealso \link{EPhysData}
#' @export newEPhysData
newEPhysData <-
  function(Data,
           TimeTrace,
           StimulusTrace = as_units(integer(0),unitless),
           Unit = NULL,
           TimeUnit = NULL,
           StimulusUnit = NULL) {

    # Check if Data is a matrix or data frame
    if (!is.matrix(Data) && !is.data.frame(Data) && !(inherits(Data, "units"))) {
      stop("Input 'Data' must be a matrix or data frame.")
    }

    n_rows <- nrow(Data)
    if(is.null(n_rows)){
      Data<-as_units(matrix(Data),deparse_unit(Data))
      n_rows<-nrow(Data)
    }

    if (!is.numeric(TimeTrace) || length(TimeTrace) != n_rows) {
      stop("'TimeTrace' should be a numeric vector with the same length as the number of rows in 'Data'.")
    }

    if(is.data.frame(Data)){
      Data<-as.matrix(Data)
    }

    # check if data are given with units and use those, if not, use
    if (inherits(Data, "units")) {
      if (!is.null(Unit) && Unit != deparse_unit(Data)) {
        stop(
          "Units provided via the 'Unit' argument (",
          Unit,
          ")  and with 'Data' (",
          deparse_unit(Data) ,
          "). Both do not match."
        )
      }
      Unit <- deparse_unit(Data)
    } else{
      Data<-tryCatch(
        as_units(as.array(Data), Unit),
        error = function(e){
          stop("Can not set unit of 'Data' to '",Unit,"'. Failed with error message: ", e)
        }
      )
    }
    if (inherits(TimeTrace, "units")) {
      if (!is.null(TimeUnit) && Unit != deparse_unit(TimeTrace)) {
        stop(
          "Units provided via the 'TimeUnit' argument (",
          TimeUnit,
          ")  and with 'TimeTrace' (",
          deparse_unit(TimeTrace) ,
          "). Both do not match."
        )
      }
      TimeUnit <- deparse_unit(TimeTrace)
    } else{
      TimeTrace<-tryCatch(
        as_units(as.array(TimeTrace), TimeUnit),
        error = function(e){
          stop("Can not set unit of 'TimeTrace' to '",TimeUnit,"'. Failed with error message: ", e)
        }
      )
    }

    if (!length(StimulusTrace)==0) {
      if (inherits(StimulusTrace, "units")) {
        if (!is.null(StimulusUnit) && StimulusUnit != deparse_unit(StimulusTrace)) {
          stop(
            "Units provided via the 'StimulusUnit' argument (",
            StimulusUnit,
            ")  and with 'StimulusTrace' (",
            deparse_unit(StimulusTrace) ,
            "). Both do not match."
          )
        }
        StimulusUnit <- deparse_unit(StimulusTrace)
      } else{
        StimulusTrace <- tryCatch(
          as_units(as.array(StimulusTrace), StimulusUnit),
          error = function(e) {
            stop(
              "Can not set unit of 'StimulusTrace' to '",
              StimulusUnit,
              "'. Failed with error message: ",
              e
            )
          }
        )
      }
    } else {
      StimulusTrace <- as_units(integer(), unitless)
    }


    out<-new(
      "EPhysData",
      Data = Data,
      TimeTrace = TimeTrace,
      StimulusTrace = StimulusTrace,
      Rejected = logical(length = dim(Data)[2]),
      Created = as.POSIXct(Sys.time())
    )
    if(validEPhysData(out)){
      return(out)
    }else{
      stop("No valid PhysRAW object could be created.")
    }
  }
