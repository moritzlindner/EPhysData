#' Generate a ggplot2 plot for EPhysData objects
#'
#' This method generates a ggplot2 plot for objects of class 'EPhysData'. It allows you to visualize electrical physiology data over time.
#'
#' @inheritParams GetData
#'
#' @return A ggplot2 plot visualizing the EPhysData.
#' @importFrom ggplot2 ggplot aes geom_line scale_alpha_manual scale_color_manual labs
#' @importFrom ggpubr theme_pubr
#' @examples
#' ephys_data <- makeExampleEPhysData(,replicate_count = )  # Create an EPhysData object
#' Rejected(ephys_data) <- c(F,F,F,F,T,T,T,F,F,F,F,T,F,T,F) # randmly reject some
#' AverageFunction(ephys_data) <- mean
#' ggEPhysData(ephys_data)  # Generate a ggplot2 plot
#' @name ggEPhysData
NULL
setGeneric(
  name = "ggEPhysData",
  def = function(X, ...) {
    standardGeneric("ggEPhysData")
  }
)

#' @rdname ggEPhysData
#' @exportMethod ggEPhysData
setMethod("ggEPhysData",
          "EPhysData",
          function(X, Raw = T) {
            if (Raw) {
              df <- as.data.frame(X, Raw = T)
              df$Type <- "RAW"
              df$Rejected <- F
            }

            df.avg <- as.data.frame(X, Raw = F)
            if (length(unique(df.avg$Repeat) == 1)) {
              df.avg$Repeat <- "AVG"
              df.avg$Type <- "AVG"
              df.avg$Rejected <- F
              if (Raw) {
                df <- rbind(df, df.avg)
              } else{
                df <- df.avg
              }
            }

            rej <- Rejected(X)
            if (any(rej) & Raw) {
              Rejected(X) <- !Rejected(X)
              df.rej <- as.data.frame(X, Raw = T)
              df.rej$Type <- "RAW"
              df.rej$Rejected <- T
              df.rej$Repeat<-length(unique(df$Repeat))
              df <- rbind(df, df.rej)
            }

            out <- ggplot(data = df,
                          aes(
                            x = Time,
                            y = Value,
                            alpha = (Type),
                            colour = as.factor(Rejected),
                            group=Repeat
                          )) +
              geom_line() +
              scale_alpha_manual(values = c("RAW" = 0.3, "AVG" = 1)) +
              scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
              labs(color = "Rejected", alpha = "Type") +
              theme_pubr()
            return(out)
          })
