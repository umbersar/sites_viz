library(shiny)
library(ggplot2)

columns <- unique(colnames(mtcars))

ui <- fluidPage(
  
  
  verbatimTextOutput("columns"),
  selectInput("graph_type", label = "Number of Variables", choices = c("1" = "one", "2" = "two", "3" = "three"))
  
  # sliderInput("height", "height", min = 400, max = 800, value = 500),
  # sliderInput("width", "width", min = 400, max = 800, value = 500),
  # selectInput("variable", "Variable:",
  #             c("Cylinders" = "cyl",
  #               "Transmission" = "am",
  #               "Gears" = "gear")),
  # tableOutput("data")
)


server <- function(input, output, session) {
  # output$plot <- renderPlot(
  #   width = function() input$width,
  #   height = function() input$height,
  #   res = 96,
  #   {
  #     plot(rnorm(input$n), rnorm(input$n))
  #   }
  # )
  output$columns <- renderText(columns)
  
  
  output$data <- renderTable({
    mtcars[, c("mpg", input$variable), drop = FALSE]
  }, rownames = TRUE)
}

shinyApp(ui, server)