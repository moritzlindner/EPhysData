#' Subset from EPhysData Object
#'
#' This method subsets an \code{EPhysData} or an \code{EPhysSet} object into a new object of the same class.
#'
#' @inheritParams GetData
#' @param Metadata_select A pairlist of metadata parameters for subsetting (only for \code{EPhysSet}).
#' @param Simplify Logical if 'True' will return \code{EPhysData} instead of \code{EPhysSet} if only one \code{EPhysData} is left in the set.
#' @param ... currently unused.
#' @return `Subset`: An \code{EPhysData} or an \code{EPhysSet} object representing the subsetted data.
#'
#' @details The \code{Subset} function creates a new \code{EPhysData} or \code{EPhysSet} object containing a subset of the data (and metadata for \code{EPhysSet}) from the original object, based on the provided parameters.
#' @family EPhysData-methods
#' @family Subsetting_Dataextraction
#' @examples
#' # Subset EPhysData
#' myEPhysData <- makeExampleEPhysData()
#'
#' ## Get subsetted data based on time range and repeated measurements
#' subsetted_myEPhysData <- Subset(myEPhysData, Time = TimeTrace(myEPhysData)[c(1, 3)], Repeats = c(1, 2))
#' subsetted_myEPhysData
#'
#' # Subset EPhysSet with make_metadata_parilist
#' myEPhysSet <- makeExampleEPhysSet()
#' selection_list <- make_metadata_parilist(myEPhysSet,except=list(StepID=c("A3")))
#' selection_list
#' subsetted_myEPhysSet <- Subset(myEPhysSet,Metadata_select=selection_list)
#' subsetted_myEPhysSet
#' Metadata(subsetted_myEPhysSet)
#' @exportMethod Subset
setGeneric(
  name = "Subset",
  def = function(X,
                 ...)
  {
    standardGeneric("Subset")
  }
)

#' @importFrom units as_units
#' @describeIn Subset Subset method for EPhysData
setMethod("Subset",
          "EPhysData",
          function(X,
                   Time = range(TimeTrace(X)),
                   TimeExclusive = FALSE,
                   Repeats = !Rejected(X),
                   Raw = T) {
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


            out <- new(
              "EPhysData",
              Data = Data,
              TimeTrace = Time,
              StimulusTrace = StimulusTrace,
              Rejected = Rejected(X)[Repeats],
              Created = X@Created
            )
            if (validObject(out)) {
              return(out)
            } else{
              stop("No valid PhysRAW object could be created.")
            }
          })

#' @importFrom units as_units
#' @describeIn Subset Subset method for EPhysSet
#' @noMd
setMethod("Subset",
          "EPhysSet",
          function(X,
                   Time = NULL,
                   TimeExclusive = FALSE,
                   Repeats = NULL,
                   Metadata_select = make_metadata_parilist(X),
                   Raw = T,
                   Simplify = F
                   ) {

            if(!all(names(Metadata_select) %in% colnames(Metadata(X)))){
              stop("Not all keys from 'Metadata_select' are column names of the metadata of 'X'.")
            }

            for (i in names(Metadata_select)){
              if(!all(Metadata_select[[i]] %in% Metadata(X)[,i])){
                warning("Not all criteria specified for ",i," occure in the column '",i,"' of the metadata of 'X'.")
              }
              if(!any(Metadata_select[[i]] %in% Metadata(X)[,i])){
                stop("None of the criteria specified for ",i," occure in the column '",i,"' of the metadata of 'X'.")
              }
            }

            MetaSubset<-array(dim=dim(Metadata(X)))
            colnames(MetaSubset)<-colnames(Metadata(X))
            for (i in names(Metadata_select)){
              MetaSubset[,i]<-(Metadata(X)[,i] %in% Metadata_select[[i]])
            }

            MetaSubset<-apply(MetaSubset,1,all)

            X@Metadata<-Metadata(X)[MetaSubset,, drop=FALSE]
            X@Data<-X@Data[MetaSubset]
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

#' @keywords internal
#' @noMd
condition_time <- function(X, Time, TimeExclusive) {
  if (!isTRUE(all.equal(Time, range(TimeTrace(X))))) {
    if (!TimeExclusive) {
      Time <-
        TimeTrace(X)[TimeTrace(X) >= Time[1] &
                       TimeTrace(X) <= Time[2]]
    } else{
      # if extracting exact time points. get closest to values entered
      Time[1] <-
        TimeTrace(X)[which(abs(TimeTrace(X) - Time[1]) == min(abs(TimeTrace(X) -
                                                                    Time[1])))]
      Time[2] <-
        TimeTrace(X)[which(abs(TimeTrace(X) - Time[2]) == min(abs(TimeTrace(X) -
                                                                    Time[2])))]
    }
  } else{
    Time <- TimeTrace(X)
  }
}

#' @describeIn Subset make_metadata_parilist
#' @param except Which values to remove in the subset.
#' @return `make_metadata_parilist`: A pairlist providing input for the 'Metadata_select' parameter of \code{Subset} for \code{EPhysSet}.
#'
#' @details The \code{make_metadata_parilist} function is a helper function for \code{Subset}. It creates a pairlist that provides input for the 'Metadata_select' parameter of \code{Subset} for \code{EPhysSet}. as 'Metadata_select' is a positive selector, \code{make_metadata_parilist} may be particularly helpful when only a single condition should be excluded. See example below.
#' @exportMethod make_metadata_parilist
setGeneric(
  name = "make_metadata_parilist",
  def = function(X,
                 except = list())
  {
    standardGeneric("make_metadata_parilist")
  }
)

setMethod("make_metadata_parilist",
          "EPhysSet",
          function(X, except = list()) {
            Metadata_select <- pairlist()
            for (cn in colnames(Metadata(X))) {
              Metadata_select[[cn]] <- unique(Metadata(X)[, cn])
            }
            if (length(except) != 0) {
              for (ex in names(except)) {
                Metadata_select[[ex]] <- Metadata_select[[ex]][!(Metadata_select[[ex]] %in% except[[ex]])]
              }
            }
            return(Metadata_select)
          })
