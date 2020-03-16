library(shiny)


ui <- fluidPage(
  
  navbarPage(
    title = 'DataTable Demo',
    tabPanel('Pagination', DT::dataTableOutput('pagination')),
    tabPanel('Function Callback', DT::dataTableOutput('funCallBack'))
  )
  
)


server <- function(input, output) {
  
  # pagination with three levels" 5, 15, or all
  # default at 15
  output$pagination <- DT::renderDataTable(
    
    DT::datatable(
      iris, options = list(
        lengthMenu = list(c(5,15,-1), c('5', '15', 'All')),
        pageLength = 15
      )
    )

  )
  
  # function call back with JS
  output$funCallBack <- DT::renderDataTable(
    DT::datatable(
      iris,
      options = list(rowCallback = DT::JS(
        'function(row, data) {
          // Bold and highlight cells whose sepal length is >= 5
          if (parseFloat(data[1]) >= 5.0) {
            $("td:eq(1)", row).css("font-weight", "bold");
            $(row).css("backgroundColor", "yellow");          
          }
        }'
      ))
    )
    
  )
  
}

shinyApp(ui = ui, server = server)

