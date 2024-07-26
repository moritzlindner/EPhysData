#' Subset from EPhysData Object
#'
#' This method subsets an \code{EPhysData} or an \code{EPhysSet} object into a new object of the same class.
#'
#' @inheritParams GetData
#' @inheritParams as.data.frame
#' @param i,j Indices specifying elements to extract.
#' @param Simplify Logical if 'True' will return \code{EPhysData} instead of \code{EPhysSet} if only one \code{EPhysData} is left in the set.
#' @param ... currently unused.
#' @param SetItems Which items of the set to subset/keep.
#' @param Trials If X is an EPhysSet, this parameter can only be used if all EPhysData contained in the set has the same number of trials.
#'                Numeric index/indices or a logical vector of the same length as trials stored.
#' @return `Subset`: An \code{EPhysData} or an \code{EPhysSet} object representing the subsetted data.
#'
#' @details The \code{Subset} function creates a new \code{EPhysData} or \code{EPhysSet} object containing a subset of the data (and metadata for \code{EPhysSet}) from the original object, based on the provided parameters. If subsetting by time, the filter function stored in the \code{EPhysData} object gets reset.
#' @family EPhysData-methods
#' @family Subsetting_Dataextraction
#' @name Subset
#' @examples
#' # Subset EPhysData
#' myEPhysData <- makeExampleEPhysData(replicate_count = 3)
#'
#' ## Get subsetted data based on time range and trials
#' subsetted_myEPhysData <- Subset(myEPhysData, Time = TimeTrace(myEPhysData)[c(1, 3)], Trials = c(1, 2))
#' subsetted_myEPhysData
#'
#' # Subset EPhysSet
#' myEPhysSet <- makeExampleEPhysSet(nsets=10)
#' subsetted_myEPhysSet <- Subset(myEPhysSet, SetItems=c(4:7))
#' subsetted_myEPhysSet
#' Metadata(subsetted_myEPhysSet)

#' @importFrom units as_units
#' @importFrom methods new validObject
#' @export
#' @docType methods
#' @rdname Subset-methods
setGeneric(
  name = "Subset",
  def = function(X, ...) {
    standardGeneric("Subset")
  }
)

#' @rdname Subset-methods
#' @aliases Subset,EPhysData,EPhysSet,ANY-method
setMethod("Subset",
          "EPhysData",
          function(X,
                   Time = range(TimeTrace(X)),
                   TimeExclusive = FALSE,
                   Trials = !Rejected(X),
                   Raw = T,
                   ...) {
            Data <- GetData(
              X = X,
              Time = Time,
              TimeExclusive = TimeExclusive,
              Trials = Trials,
              Raw = Raw
            )

            if (!("units" %in% class(Time))) {
              stop("'Time' must be of class 'units'")
            }

            convertibel.to.s <- tryCatch({
              set_units(Time, "s")
              TRUE
            }, error = function(e) {
              FALSE
            })
            if (!convertibel.to.s) {
              stop("'Time' must be of convertible to seconds.")
            }

            if (!isTRUE(all.equal(Time, range(TimeTrace(X)))) || TimeExclusive) {
              filter.fx <- function(x) {
                return(x)
              }
              message("Data is subsetted by time, thus resetting filter function.")
            } else {
              filter.fx <- FilterFunction(X)
            }

            if(is.null(Trials)){
              Trials<-!Rejected(X, return.fx = F)
            }

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
              rejected.fx <- function(x) {
                return(FALSE)
              }
            } else {
              fx <- Rejected(X)[Trials]
              function_string <-
                paste0("function(x) { return(c(", paste(fx, collapse = ", "), ")) }")
              rejected.fx <- eval(parse(text = function_string))
            }

            # if x is changed, then dont keep filter

            if (!Raw) {
              average.fx <- function(x) {
                return(x)
              }
            } else {
              average.fx <- AverageFunction(X)
            }

            out <- new(
              "EPhysData",
              Data = Data,
              TimeTrace = Time,
              StimulusTrace = StimulusTrace,
              Rejected = rejected.fx,
              average.fx = average.fx,
              filter.fx = filter.fx,
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
#' @importFrom stringr str_extract
#' @rdname Subset-methods
setMethod("Subset",
          "EPhysSet",
          function(X,
                   Time = NULL,
                   TimeExclusive = FALSE,
                   Trials = NULL,
                   SetItems = rep(TRUE, nrow(Metadata(X))),
                   Raw = T,
                   Simplify = F,
                   ...
                   ) {

            if(is.logical(SetItems)){
              if(length(SetItems)!=length(X)){
                stop("Lengths mismatch: 'SetItems' is a ", typeof(SetItems), " of length ", length(SetItems), " but must be a logical vector of the same length as 'X' (", length(X), ") or a numeric vector representing valid item indices.")
              }
            }else{
              if(!is.numeric(SetItems)){
                stop("'SetItems' is neither logical nor numeric. 'SetItems' is a ", typeof(SetItems), " of length ", length(SetItems), " but must be a logical vector of the same length as 'X' (", length(X), ") or a numeric vector representing valid item indices.")
              }else{
                if(!(all(SetItems %in% 1:length(X)))){
                  stop("'SetItems' contains invalid indices. 'SetItems' is a ", typeof(SetItems), " of length ", length(SetItems), " and with data in the range of ", min(SetItems), " to ", max(SetItems), " but must be a logical vector of the same length as 'X' (", length(X), ") or a numeric vector representing valid item indices (i.e. value range must be within the length of 'X').")
                }
              }
            }

            md.orig<-Metadata(X)

            X@Metadata<-Metadata(X)[SetItems,, drop=FALSE]
            X@Data<-X@Data[SetItems]

            if (!is.null(Trials)) { # if "Trials" not null, check that all EPhysData have same number of trials
              if (length(unique(unlist(lapply(X@Data, function(x) {
                dim(x)[2]
              })))) != 1) {
                ntr <- range(unlist(lapply(X@Data, function(x) {
                  dim(x)[2]
                })))
                stop("Paramater 'Trials' is defined, but Items selected have differing number or trials (Ranging from ", ntr[1], " to ", ntr[2], "). 'Trials' can only be used if all items (EPhysData) have the same number of trials.")
              }
            }

            tryCatch({
              X@Data <- lapply(X@Data, function(x) {
                if (is.null(Time)){
                  Time = range(TimeTrace(x))
                }
                if (is.null(Trials) && !Raw){
                  curr.Trials = !Rejected(x)
                } else {
                  if (is.null(Trials)){
                    curr.Trials <- !logical(dim(x)[2])
                  } else {
                    curr.Trials<-Trials
                  }
                }
                x<-Subset(
                  X = x,
                  Time = Time,
                  TimeExclusive = TimeExclusive,
                  Trials = curr.Trials,
                  Raw = Raw
                )
                return(x)
              })
            }, error = function (e){
              stop("Subsetting EPhysSet failed for recording ", Metadata(X)[as.integer(str_extract(e, "(?<=\\[\\[)\\d+(?=L\\]\\])")),1], )
            })
            if(nrow(Metadata(X)) == 1 && Simplify == T){
              X<-X@Data[[1]]
            }

            if(validObject(X)){
              return(X)
            }
          })

#' @describeIn Subset-methods Extract specific items from an \linkS4class{EPhysSet} object. Returns an \linkS4class{EPhysData} object or a list thereof.
#' @aliases `[[`,EPhysSet,ANY-method
#' @export
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
#' @aliases `[[<-`,EPhysSet,ANY-method
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

#' @describeIn Subset-methods Extract specific items from an \linkS4class{EPhysData} object. Returns a data.frame.
#' @aliases `[`,EPhysData,ANY-method
#' @export
setMethod("[",
          "EPhysData",
          function(x, i, j) {
            return(x@Data[i, j, drop = F])
          })
