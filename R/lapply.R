#' Apply a Function to Elements of an EPhysSet Object
#'
#' Applies a specified function to elements of an \code{EPhysSet} object. The function is applied to the data contained within the \code{EPhysSet} and the results are returned as a modified \code{EPhysSet} object or as a matrix, depending on the chosen options.
#'
#' @param X An \code{EPhysSet} object.
#' @param FUN The function to apply to the elements of \code{X@Data}.
#' @param ReturnEPhysSet Logical. If \code{TRUE}, the results are returned as a modified \code{EPhysSet} object with the function applied to the data. If \code{FALSE}, the results are returned as a list.
#'
#' @return If ReturnEPhysSet is \code{TRUE} modified \code{EPhysSet} object with the function applied to the data. If ReturnEPhysSet is \code{FALSE}, a list with the results will be returned.
#'
#' @examples
#' myEPhysSet <- makeExampleEPhysSet()
#' myEPhysSet
#' myEPhysSet<-lapply(myEPhysSet, function(x){FilterFunction(x)<-sin; return(x)})
#' myEPhysSet
#' FilterFunction(myEPhysSet@Data[[2]])
#'
#' @name lapply
#' @exportMethod lapply
setMethod("lapply",
          "EPhysSet",
          function(X, FUN, ReturnEPhysSet = T) {
            dat <- lapply(X@Data, FUN)
            if (ReturnEPhysSet) {
              X@Data <- dat
              if (!validObject(X)) {
                stop(
                  paste(
                    "Applying to EPhysSet",
                    deparse(substitute(object)),
                    "failed. No valid EPhysSet object returned"
                  )
                )
              }
              return(X)
            } else{
              return(dat)
            }
          })
