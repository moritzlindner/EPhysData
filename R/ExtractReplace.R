#' @describeIn Subset-method Subset specific items from an \linkS4class{EPhysSet} object. Returns an \linkS4class{EPhysData} object or a list thereof.
#' @param i Index specifying elements to extract.
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
