#' @import units
#' @noMd
setOldClass("units")


#' @importFrom  units deparse_unit
#' @noMd
validEPhysData <- function(object) {

  if (!(inherits(object@Data, "units"))){
    stop("'Data' does not contain units.")
  }
  if (!(inherits(object@TimeTrace, "units"))){
    stop("'TimeTrace' does not contain units.")
  }
  if (!(inherits(object@StimulusTrace, "units"))){
    stop("'StimulusTrace' does not contain units.")
  }
  # Check that TimeTrace and StimulusTrace are the same length as the number of rows in Data
  n_rows <- nrow(object@Data)
  if (length(object@TimeTrace) != n_rows) {
    stop("'TimeTrace' length should be the same as the number of rows in 'Data'.")
  }

  if (length(object@StimulusTrace) > 0 &&
      length(object@StimulusTrace) != n_rows) {
    stop("'StimulusTrace' length should be the same as the number of rows in 'Data'.")
  }

  # Check TimeUnit
  if (!(deparse_unit(object@TimeTrace) %in% c("us","ms","s", "min", "h", "d", "w"))) {
    stop("'TimeUnit' should be either of 's','min','h','d','w'")
  }

  # Check Rejected
  n_cols <- ncol(object@Data)
  if (length(Rejected(object)) != n_cols) {
    stop("'Rejected' length should be the same as the number of columns in 'Data', i.e. the number of repeated measurements.")
  }

  # check functions
  if (!is.function(object@filter.fx)) {
    stop("'filter.fx' must be a function.")
  }

  tryCatch(
    object@filter.fx(c(1:10)),
    error = function(e) {
      stop("'filter.fx' can not handle numeric vectors. Recheck the function and make sure it can be run with one single parameter representing a numeric vector.")
    }
  )

  if (!is.function(object@average.fx)) {
    stop("'average.fx' must be a function.")
  }

  tryCatch(
    object@average.fx(c(1:10)),
    error = function(e) {
      stop("'average.fx' can not handle numeric vectors. Recheck the function and make sure it can be run with one single parameter representing a numeric vector.")
    }
  )
  # If all checks passed, return TRUE
  TRUE
}

#' EPhysData - Class for raw Electrophysiological Data
#'
#' This class represents raw electrophysiological data, including repeated measurements, time and stimulus traces, units, as well as information on the post-processing steps (filtering, averaging of repeats) that can be applied to the data
#'
#' @slot Data A 2d array representing the raw electrophysiological data with columns for repeated measurements
#' @slot TimeTrace A numeric vector containing the time trace of the electrophysiological data.
#' @slot StimulusTrace A numeric vector containing the stimulus trace (optional).
#' @slot Created A POSIXct object representing the creation date and time of the object.
#' @slot Rejected A logical vector indicating which repeated measurements to exclude from averaging.
#' @slot filter.fx A function that can be applied to each repeated measurement By default, it leaves the data unchanged.
#' @slot average.fx A function describing how averaging across repeated measurement should be performed. By default, repeated measurements are returned and no averaging is performed.
#' @importFrom units as_units
#' @exportClass EPhysData
EPhysData <- setClass(
  "EPhysData",
  slots = list(
    Data = "units",
    TimeTrace = "units",
    StimulusTrace = "units",
    Created = "POSIXct",
    Rejected = "logical",
    filter.fx = "function",
    average.fx = "function"
  ),
  prototype = list(
    Data = as_units(array(dim=c(0,0)),unitless),
    TimeTrace = as_units(integer(),"s"),
    StimulusTrace = as_units(integer(),unitless),
    Created = as.POSIXct(Sys.time()),
    Rejected = logical(),
    filter.fx = function(x) {
      x
    },
    average.fx = function(x) {
      x
    }
  ),
  validity = validEPhysData
)

#' Store raw electrophysiological data into a \linkS4class{EohysRAW} object.
#'
#' This function takes in raw electrophysiological stores them into a \linkS4class{EohysRAW} object.
#'
#' @param Data A matrix or data frame containing the raw electrophysiological data.
#'             Columns represent repeated measurements.
#' @param TimeTrace A numeric vector representing the time trace of the data.
#' @param Unit A character string representing the unit of the electrophysiological data.
#'             It should be a basic or derived SI unit. If the unit has a prefix, it must be converted
#'             into a factor and stored in the 'Factor' slot of the output object.
#' @param TimeUnit A character string representing the unit of time for the time trace.
#'                It should be one of 's', 'min', 'h', 'd', or 'w'.
#' @param StimulusTrace A numeric vector representing the stimulus trace (optional).
#' @param StimulusUnit A character string representing the unit of the stimulus data (optional).
#'                     It should be a basic or derived SI unit. If the unit has a prefix, it must be converted
#'                     into a factor and stored in the 'StimulusFactor' slot of the output object.
#' @return An object of class \linkS4class{EohysRAW} containing the raw electrophysiological data and other information.
#' @examples
#' Data <- data.frame(time = c(0, 0.001, 0.002, 0.003),
#'                    voltage = c(0.5, 0.6, 0.7, 0.8))
#' TimeTrace <- c(1, 2, 3, 4)
#' Unit <- "mV"
#' TimeUnit <- "s"
#' newEPhysData(Data, TimeTrace, Unit, TimeUnit)
#' # Output: An object of class 'EPhysData' with processed data and information.
#' @importFrom  units deparse_unit as_units
#' @export
newEPhysData <-
  function(Data,
           TimeTrace,
           StimulusTrace = as_units(integer(),unitless),
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
        as_units(as.array(Data), eval(Unit)),
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
        as_units(as.array(TimeTrace), eval(TimeUnit)),
        error = function(e){
          stop("Can not set unit of 'TimeTrace' to '",TimeUnit,"'. Failed with error message: ", e)
        }
      )
    }

    if (!is.null(StimulusTrace)) {
      if (inherits(StimulusTrace, "units")) {
        if (!is.null(StimulusUnit) && Unit != deparse_unit(StimulusTrace)) {
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
          as_units(as.array(StimulusTrace), eval(StimulusUnit)),
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

#' @noMd
setMethod("show",
          "EPhysData",
          function(object) {
            cat("An object of class EPhysData \n")
            cat("Sample points:", dim(object@Data)[1], " \n")
            cat("Repeats:", dim(object@Data)[2], " \n")
            cat("Duration", as.character(diff(range(object@TimeTrace))), deparse_unit(object@TimeTrace),"\n")
            cat("Created", as.character(object@Created),"\n")
          })
