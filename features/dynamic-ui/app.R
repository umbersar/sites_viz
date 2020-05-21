library(shiny)

ui <- shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Dynamically append arbitrary number of inputs"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    uiOutput("allInputs"),
    actionButton("appendInput", "Append Input")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    p("The crux of the problem is to dynamically add an arbitrary number of inputs
      without resetting the values of existing inputs each time a new input is added.
      For example, add a new input, set the new input's value to Option 2, then add
      another input. Note that the value of the first input resets to Option 1."),
    
    p("I suppose one hack would be to store the values of all existing inputs prior
      to adding a new input. Then,", code("updateSelectInput()"), "could be used to 
      return inputs to their previously set values, but I'm wondering if there is a 
      more efficient method of doing this.")
  )
))

server <- shinyServer(function(input, output) {
  
  # Initialize list of inputs
  inputTagList <- tagList()
  
  output$allInputs <- renderUI({
    # Get value of button, which represents number of times pressed (i.e. number of inputs added)
    i <- input$appendInput
    # Return if button not pressed yet
    if(is.null(i) || i < 1) return()
    # Define unique input id and label
    newInputId <- paste0("input", i)
    newInputLabel <- paste("Input", i)
    # Define new input
    newInput <- selectInput(newInputId, newInputLabel, c("Option 1", "Option 2", "Option 3"))
    # Append new input to list of existing inputs
    inputTagList <<- tagAppendChild(inputTagList, newInput)
    # Return updated list of inputs
    inputTagList
  })
  
})

shinyApp(ui, server)