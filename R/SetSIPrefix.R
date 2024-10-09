#' Set the SI prefix for an EPhysData or EPhysSet object
#'
#' Sets the SI prefix to a user-define value or so that it minimizes the number of relevant digits for a vector of units.
#'
#' @param X An EPhysData or EPhysSet object
#' @param value SI prefix as a character string, "auto" to choose the best SI prefix or "keep", for keeping the current SI prefix.
#' @importFrom units deparse_unit set_units drop_units as_units
#' @importFrom stats median
#' @importFrom stringr str_sub str_locate
#' @importFrom methods setGeneric setMethod
#' @return The EPhysData or EPhysSet object with units converted to the new SI prefix.
#' @docType methods
#' @name SetSIPrefix-methods
#' @aliases BestSIPrefix
#' @examples
#' ## BestSIPrefix
#' X<-makeExampleEPhysData()
#' X@Data<-X@Data/1000
#' ggEPhysData(X)
#' X<-BestSIPrefix(X)
#' ggEPhysData(X)
#'
#'  X<-makeExampleEPhysSet()
#'  X<-BestSIPrefix(X)
#'
#'  ## SetSIPrefix
#' X<-makeExampleEPhysData()
#' ggEPhysData(X)
#' X<-SetSIPrefix(X,"k")
#' ggEPhysData(X)
#'
#'  X<-makeExampleEPhysSet()
#'  X<-SetSIPrefix(X,"k")
#' @export
setGeneric(
  name = "BestSIPrefix",
  def = function(X) {
    standardGeneric("BestSIPrefix")
  }
)
#' @rdname SetSIPrefix-methods
#' @aliases BestSIPrefix,EPhysData,EPhysSet,ANY-method
setMethod("BestSIPrefix", signature = "EPhysData", function(X) {
  X@Data<-best_si_prefix(X@Data)
  X
})
#' @rdname SetSIPrefix-methods
setMethod("BestSIPrefix", signature = "EPhysSet", function(X) {
  X<-lapply(X,BestSIPrefix)
  X
})

#' @export
#' @docType methods
#' @rdname SetSIPrefix-methods
setGeneric(
  name = "SetSIPrefix",
  def = function(X, value) {
    standardGeneric("SetSIPrefix")
  }
)
#' @rdname SetSIPrefix-methods
#' @aliases SetSIPrefix,EPhysData,EPhysSet,ANY-method
setMethod("SetSIPrefix", signature = "EPhysData", function(X, value) {
  if(value=="auto"){
    return(BestSIPrefix(X))
  }
  if(value=="keep"){
    return(X)
  }
  if(value %in% names(si_prefixes())){
    si_unit<-get_si_unit(X@Data)
    X@Data <- set_units(X@Data,paste0(value,si_unit),mode="standard")
    return(X)
  } else {
    stop("SI Prefix provided is invalid.")
  }
})
#' @rdname SetSIPrefix-methods
setMethod("SetSIPrefix", signature = "EPhysSet", function(X,value) {
  X<-lapply(X,function(x){SetSIPrefix(x,value)})
  X
})


#' @noMd
#' @keywords internal
si_prefixes<-function(){
  si_prefixes <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k", "M", "G", "T", "P", "E", "Z", "Y")
  si_factors <- 10^(seq(-24, 24, by = 3))
  names(si_factors) <- si_prefixes
  si_factors
}


#' @noMd
#' @keywords internal
extract_si_prefix <- function(units_obj) {
  unit_str <- deparse_unit(units_obj)
  if (nchar(unit_str)>1){
    for (prefix in names(si_prefixes())) {
      if(nchar(prefix)>0){
        if (startsWith(unit_str, prefix)) {
          return(prefix)
        }
      }
    }
  }
  return("") # No SI prefix found
}

#' @noMd
#' @keywords internal
get_si_unit <- function(units_obj) {
  unit_str <- deparse_unit(units_obj)
  if (nchar(unit_str)>1){
    for (prefix in names(si_prefixes())) {
      if(nchar(prefix)>0){
        if (startsWith(unit_str, prefix)) {
          return(sub(prefix, "", unit_str))
        }
      }
    }
  }
  return(unit_str) # No SI prefix found
}

#' @noMd
#' @keywords internal
best_si_prefix <- function(X) {
  si_unit<-get_si_unit(X)
  X <- set_units(X, si_unit, mode = "standard")
  X<-drop_units(X)

  decimals<-log10(si_prefixes())
  X.str<-c(format(X, scientific = T))
  dec_best <- decimals[which.min(abs(decimals - median(as.numeric(unique(
    str_sub(X.str,
            (str_locate(X.str,
                        "e[\\+\\-]"))
            [, 2])
  )))))]
  X <- as_units(X, si_unit, mode = "standard")
  X <- set_units(X,paste0(names(dec_best),si_unit),mode="standard")
  X
}
