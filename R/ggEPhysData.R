#' Generate a ggplot2 plot for EPhysData objects
#'
#' This method generates a ggplot2 plot for objects of class 'EPhysData'. It allows you to visualize electrical physiology data over time.
#'
#' @inheritParams GetData
#' @param ShowFiltered Apply the filter to the raw traces before showing them.
#' @param SetSIPrefix Change the SI prefix. Set to \code{keep} (default), for not to change anything, to \code{auto} for using the \link{BestSIPrefix-methods} to minimize the number of relevant digits or to any SI prefix to use that. Calls the \link{SetSIPrefix}method
#'
#' @return A ggplot2 plot visualizing the EPhysData.
#' @importFrom ggplot2 ggplot aes geom_line scale_alpha_manual scale_color_manual labs
#' @importFrom ggpubr theme_pubr
#' @examples
#' ephys_data <- makeExampleEPhysData(,replicate_count = 15)  # Create an EPhysData object
#' Rejected(ephys_data) <- c(rep(c(FALSE,FALSE,TRUE),5)) # randomly reject some
#' AverageFunction(ephys_data) <- mean
#' ggEPhysData(ephys_data)  # Generate a ggplot2 plot
#' ggEPhysData(ephys_data,SetSIPrefix="k")  # Generate a ggplot2 plot
#' @name ggEPhysData
#' @export
#' @docType methods
#' @rdname ggEPhysData-methods
setGeneric(
  name = "ggEPhysData",
  def = function(X, Raw = T, ShowFiltered = T, SetSIPrefix="keep") {
    standardGeneric("ggEPhysData")
  }
)

#' @importFrom units as_units deparse_unit
#' @rdname ggEPhysData-methods
#' @aliases ggEPhysData,EPhysData,ANY-method
setMethod("ggEPhysData",
          "EPhysData",
          function(X, Raw = T, ShowFiltered = T, SetSIPrefix="keep") {
            X<-SetSIPrefix(X,SetSIPrefix)
            if (Raw) {
              rej <- Rejected(X)
              if(!ShowFiltered){
                df <- as.data.frame(X, Raw = T, IncludeRejected = T)
              } else {
                dat<-X@Data
                unit.buffer<-deparse_unit(dat)
                dat<-apply(dat, 2, FilterFunction(X), simplify = T)
                dat<-as_units(dat,unit.buffer)
                X.tmp<-X
                X.tmp@Data<-dat
                df <- as.data.frame(X.tmp, Raw = T, IncludeRejected = T)
              }
              df$Type <- "Raw"
              df$Rejected <- rej[df$Trial]
            }
            Time<-Value<-Type<-Trial<-NULL

            df.avg <- as.data.frame(X, Raw = F)
            if (length(unique(df.avg$Trial)) != dim(X)[2]) {
              if (length(unique(df.avg$Trial) == 1)) {
                df.avg$Trial <- paste0("AVG", df.avg$Trial)
                df.avg$Type <- "Averaged"
                df.avg$Rejected <- FALSE
                if (Raw) {
                  df <- rbind(df, df.avg)
                } else{
                  df <- df.avg
                }
              }
            }else{
              if(Raw){
                warning("No averaging function set.")
              } else {
                stop("No averaging function set.")
              }
            }

            out <- ggplot(data = df,
                          aes(
                            x = Time,
                            y = Value,
                            alpha = (Type),
                            colour = Rejected,
                            group = Trial
                          )) +
              geom_line() +
              scale_alpha_manual(values = c("Raw" = 1/sqrt(length(unique(df$Trial))), "Averaged" = 1)) +
              scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
              labs(color = "Rejected", alpha = "Type") +
              theme_pubr()
            return(out)
          })
