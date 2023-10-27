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
#' ephys_data <- makeExampleEPhysData(,replicate_count = 15)  # Create an EPhysData object
#' Rejected(ephys_data) <- c(rep(c(FALSE,FALSE,TRUE),5)) # randomly reject some
#' AverageFunction(ephys_data) <- mean
#' ggEPhysData(ephys_data)  # Generate a ggplot2 plot
#' @name ggEPhysData
#' @exportMethod ggEPhysData
setGeneric(
  name = "ggEPhysData",
  def = function(X, Raw = T) {
    standardGeneric("ggEPhysData")
  }
)

#' @noMd
setMethod("ggEPhysData",
          "EPhysData",
          function(X, Raw = T) {
            if (Raw) {
              df <- as.data.frame(X, Raw = T)
              df$Type <- "RAW"
              df$Rejected <- "NO"
            }

            df.avg <- as.data.frame(X, Raw = F)
            if(length(unique(df.avg$Repeat))!=length(unique(df$Repeat))){
              if (length(unique(df.avg$Repeat) == 1)) {
                df.avg$Repeat <- paste0("AVG", df.avg$Repeat)
                df.avg$Type <- "AVG"
                df.avg$Rejected <- "NO"
                if (Raw) {
                  df <- rbind(df, df.avg)
                } else{
                  df <- df.avg
                }
              }
            }else{
              warning("No averaging function set.")
            }

            rej <- Rejected(X)
            if (any(rej) & Raw) {
              Rejected(X) <- !Rejected(X)
              df.rej <- as.data.frame(X, Raw = T)
              df.rej$Type <- "RAW"
              df.rej$Rejected <- "YES"
              df.rej$Repeat<-length(unique(df$Repeat))+df.rej$Repeat
              df <- rbind(df, df.rej)
            }

            out <- ggplot(data = df,
                          aes(
                            x = Time,
                            y = Value,
                            alpha = (Type),
                            colour = Rejected,
                            group = Repeat
                          )) +
              geom_line() +
              scale_alpha_manual(values = c("RAW" = 1/length(unique(df$Repeat)), "AVG" = 1)) +
              scale_color_manual(values = c("YES" = "red", "NO" = "black")) +
              labs(color = "Rejected", alpha = "Type") +
              theme_pubr()
            return(out)
          })
