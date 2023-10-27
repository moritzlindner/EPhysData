#' Subset from EPhysData Object
#'
#' This method subsets an \code{EPhysData} or an \code{EPhysSet} object into a new object of the same class.
#'
#' @inheritParams GetData
#' @inheritParams as.data.frame-method
#' @param i,j Indices specifying elements to extract.
#' @param Simplify Logical if 'True' will return \code{EPhysData} instead of \code{EPhysSet} if only one \code{EPhysData} is left in the set.
#' @param ... currently unused.
#' @param SetItems Which items of the set to subset/keep.
#' @param Repeats If X is an EPhysSet, this parameter can only be used if all EPhysData contained in the set has the same number of repeats.
#'                Numeric index/indices or a logical vector of the same length as repeats stored.
#' @return `Subset`: An \code{EPhysData} or an \code{EPhysSet} object representing the subsetted data.
#'
#' @details The \code{Subset} function creates a new \code{EPhysData} or \code{EPhysSet} object containing a subset of the data (and metadata for \code{EPhysSet}) from the original object, based on the provided parameters.
#' @family EPhysData-methods
#' @family Subsetting_Dataextraction
#' @name Subset
#' @examples
#' # Subset EPhysData
#' myEPhysData <- makeExampleEPhysData(replicate_count = 3)
#'
#' ## Get subsetted data based on time range and repeated measurements
#' subsetted_myEPhysData <- Subset(myEPhysData, Time = TimeTrace(myEPhysData)[c(1, 3)], Repeats = c(1, 2))
#' subsetted_myEPhysData
#'
#' # Subset EPhysSet
#' myEPhysSet <- makeExampleEPhysSet(nsets=10)
#' subsetted_myEPhysSet <- Subset(myEPhysSet, SetItems=c(4:7))
#' subsetted_myEPhysSet
#' Metadata(subsetted_myEPhysSet)

#' @importFrom units as_units
#' @importFrom methods new validObject
#' @exportMethod Subset
setGeneric(
  name = "Subset",
  def = function(X, ...) {
    standardGeneric("Subset")
  }
)

#' @describeIn Subset Subset method for EPhysSet
#' @noMd
setMethod("Subset",
          "EPhysData",
          function(X,
                   Time = range(TimeTrace(X)),
                   TimeExclusive = FALSE,
                   Repeats = !Rejected(X),
                   Raw = T,
                   ...) {
            Data <- GetData(
              X = X,
              Time = Time,
              TimeExclusive = TimeExclusive,
              Repeats = Repeats,
              Raw = Raw
            )

            Time <- condition_time(X, Time, TimeExclusive)

            err <- tryCatch(
              StimulusTrace(X),
              error = function(e)
                e
            )
            has_stimtrace <- !any(class(err) == "error")

            if(has_stimtrace){
              StimulusTrace<-StimulusTrace(X)[TimeTrace(X) %in% Time]
            }else{
              StimulusTrace<-as_units(integer(),unitless)
            }

            if(!Raw){
              rejected.fx<-function(x) {
                return(FALSE)
              }
            } else {
              rejected.fx<-function(x) {
                return(Rejected(X)[Repeats])
              }
            }

            out <- new(
              "EPhysData",
              Data = Data,
              TimeTrace = Time,
              StimulusTrace = StimulusTrace,
              Rejected = rejected.fx,
              Created = X@Created
            )
            if (validObject(out)) {
              return(out)
            } else{
              stop("No valid PhysRAW object could be created.")
            }
          })

#' @importFrom units as_units
#' @importFrom methods validObject
#' @describeIn Subset Subset method for EPhysSet
#' @noMd
setMethod("Subset",
          "EPhysSet",
          function(X,
                   Time = NULL,
                   TimeExclusive = FALSE,
                   Repeats = NULL,
                   SetItems = rep(TRUE, nrow(Metadata(X))),
                   Raw = T,
                   Simplify = F,
                   ...
                   ) {

            if(is.logical(SetItems)){
              if(length(SetItems)!=length(X)){
                stop("Lengths mismatch: \n'SetItems' must be a logical vector of the same length as 'X' or a numeric vector representing valid item indices.")
              }
            }else{
              if(!is.numeric(SetItems)){
                stop("'SetItems' is neither logical nor numeric. \nSetItems' must be a logical vector of the same length as 'X' or a numeric vector representing valid item indices.")
              }else{
                if(!(all(SetItems %in% 1:length(X)))){
                  stop("'SetItems' contains invalid indices. \nSetItems' must be a logical vector of the same length as 'X' or a numeric vector representing valid item indices.")
                }
              }
            }

            X@Metadata<-Metadata(X)[SetItems,, drop=FALSE]
            X@Data<-X@Data[SetItems]

            if (!is.null(Repeats)) { # if "Repeats" not null, check that all EPhysData have same number of repeats
              if (length(unique(unlist(lapply(X@Data, function(x) {
                dim(x)[2]
              })))) != 1) {
                stop("Paramater 'Repeats' is defined, but EPhysData stored in the set have different number or repeats.  'Repeats' can only be used if all EPhysData have the same number of repeats.")
              }
            }

            X@Data <- lapply(X@Data, function(x) {
              if (is.null(Time)){
                Time = range(TimeTrace(x))
              }
              if (is.null(Repeats)){
                Repeats = !Rejected(x)
              }
              X<-Subset(
                X = x,
                Time = Time,
                TimeExclusive = TimeExclusive,
                Repeats = Repeats,
                Raw = Raw
              )
            })

            if(nrow(Metadata(X)) == 1 && Simplify == T){
              X<-X@Data[[1]]
            }

            if(validObject(X)){
              return(X)
            }
          })

#' @describeIn Subset Extract specific items from an \linkS4class{EPhysSet} object. Returns an \linkS4class{EPhysData} object or a list thereof.
#' @exportMethod [[
setMethod("[[",
          "EPhysSet",
          function(x, i) {
            if (length(i) == 1) {
              return(x@Data[[i]])
            } else {
              out <- lapply(i, function(ii) {
                return(x@Data[[ii]])
              })
              return(out)
            }
          })

#' @keywords internal
#' @noMd
setMethod("[[<-",
          "EPhysSet",
          function(x, i, value) {
            if(length(i)!=length(value)){
              stop("Index and replacement must have the same length")
            }
            if (length(i) == 1) {
              if(!("EPhysData" %in% class(value))){
                stop("Replacement item must be of class 'EPhysData'")
              }
              x@Data[[i]]<-value
            } else {
              rightclass<-all(unlist(lapply(value, function(v) {
                ("EPhysData" %in% class(v))
              })))
              if(!rightclass){
                stop("Replacement items must ve of class 'EPhysData'")
              }
              x@Data[i]<-value
            }
            if(validEPhysData(x)){
              return(x)
            }
          })

#' @describeIn Subset Extract specific items from an \linkS4class{EPhysData} object. Returns a data.frame.
#' @exportMethod [
setMethod("[",
          "EPhysData",
          function(x, i, j) {
            return(x@Data[i, j, drop = F])
          })
