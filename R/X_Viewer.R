#' setGeneric(
#'   name = "Rejected",
#'   def = function(X) {
#'     standardGeneric("Rejected")
#'   }
#' )
#' #' @exportMethod Rejected
#' #' @importFrom tidyr pivot_longer
#' #' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' setMethod("Rejected", signature = "EPhysData", function(X) {
#'   df<-as.data.frame(GetData(X))
#'   df$TimeTrace <- TimeTrace(X)
#'   data_melted <- pivot_longer(df, cols = -TimeTrace, names_to = "Repeat", values_to = "Value")
#'   ggplot(data_melted, aes(x = TimeTrace, y = Value, colour = Repeat)) +
#'     geom_line() +
#'     labs(title = "EPhysData Plot", x = "TimeTrace", y = "Value") +
#'     theme_minimal()
#' })
#'
#'
#'
#' library(shiny)
#' library(ggplot2)
#' library(tidyr)
#' library(dplyr)
#'
#' # Define the UI
#' ui <- fluidPage(
#'   titlePanel("Interactive EPhysData Plot"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       selectInput("repeat_select", "Select Repeat", choices = NULL)
#'     ),
#'     mainPanel(
#'       plotOutput("ephys_plot")
#'     )
#'   )
#' )
#'
#' # Define the server logic
#' server <- function(input, output, session) {
#'   # Create a reactive data frame for the selected repeat
#'   selected_repeat_data <- reactive({
#'     repeat_idx <- input$repeat_select
#'     if (!is.null(repeat_idx)) {
#'       data_df <- as.data.frame(example_data@Data)
#'       data_df <- data_df %>% mutate(TimeTrace = example_data@TimeTrace)
#'       data_melted <- pivot_longer(data_df, cols = -TimeTrace, names_to = "variable", values_to = "value")
#'       filter(data_melted, variable == repeat_idx)
#'     }
#'   })
#'
#'   # Update the select input choices
#'   observe({
#'     repeat_names <- colnames(example_data@Data)
#'     updateSelectInput(session, "repeat_select", choices = repeat_names)
#'   })
#'
#'   # Create the ggplot plot
#'   output$ephys_plot <- renderPlot({
#'     req(selected_repeat_data())
#'     ggplot(selected_repeat_data(), aes(x = TimeTrace, y = value)) +
#'       geom_line() +
#'       labs(title = paste("EPhysData Plot -", input$repeat_select), x = "TimeTrace", y = "Value") +
#'       theme_minimal()
#'   })
#' }
#'
#' # Run the Shiny app
#' shinyApp(ui = ui, server = server)
