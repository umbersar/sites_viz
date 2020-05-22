library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
source("elements.R")
source("datasets.R")


dashboardPage(
  skin = "black",
  dashboardHeader(title = "Soil Data Explorer"),
  
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("dashboard")),
      menuItem("Explore", tabName = "explore", icon = icon("flask"))
    )),
  
  # body  
  dashboardBody(
    tabItems(
      # --------------------------------------------------------------------- #
      # About Page
      # --------------------------------------------------------------------- #
      tabItem(
        tabName = "about",
        fluidRow(
          column(width = 6,box(width = 12,title = "About",about)),
          column(width = 6,box(width = 12,title = "How To",how_to))
        )
      ),
      
      # --------------------------------------------------------------------- #
      # Explore Page
      # --------------------------------------------------------------------- #
      tabItem(
        tabName = "explore",
        # select morphology class attribute and then
        # load the according data
        fixedRow(
          box (
            width = 5,
            title = "Dataset Selection and Filtering",
            
            # dataset selection
            selectizeInput(
              inputId = "dataset", 
              label = "Select a Dataset", 
              choices = datasets, 
              selected = "",
              multiple = FALSE
            ),
            
            actionButton("doLoad", "Load Dataset"),
            
            # drop columns
            selectizeInput(
              inputId = "drop_cols", 
              label = "Select Columns to Drop", 
              choices = NULL, 
              multiple = TRUE
            ),
            
            actionButton("doDrop", "Drop Selected Columns"),
            
            # decide if we need to apply filtering or not
            checkboxInput("filter_checkbox", 
                          "Filter Data?", 
                          value = FALSE, 
                          width = "100%"),
            
            selectizeInput(
              inputId = "filter_attr", 
              label = "Filter Dataset By Column", 
              choices = NULL, 
              selected = "",
              multiple = FALSE
            ),
            selectizeInput(
              inputId = "filter_val",
              label = "Column Value Selection",
              choices = NULL,
              selected = "",
              multiple = FALSE
            )
          ),
          
          # basic summary of the current dataset
          box (
            width = 7,
            title = "Data Summary",
            selectizeInput(
              inputId = "summary_attr", 
              label = "Select a column to see its summary.", 
              choices = NULL, 
              selected = "",
              multiple = FALSE
            ),
            verbatimTextOutput("summary")
          )
        ),
        
        # data display of the selected morphology data
        fixedRow(
          box(
            title = "section for map",
            leafletOutput("morph_map", width = "100%", height = "540px"),
            width = 5,
            solidHeader = TRUE
          ),
          box(
            title = "Data Display -- Filtered Dataset",
            DT::dataTableOutput('morph_data'),
            uiOutput("downloadUI"),
            id = "datatable",
            width = 7,
            solidHeader = TRUE
          )
        ),
        
        # plots
        fixedRow(
          box(
            width = 5,
            title = "Plot Selection",
            # plot selection
            selectizeInput(
              inputId = "plot_variable_type", 
              label = "Number of Variables", 
              choices = c("1", "2"), 
              selected = "1",
              multiple = FALSE
            ),
            
            #  number of variables selection
            selectizeInput(
              inputId = "plot_selection", 
              label = "Plot", 
              choices = NULL, 
              selected = "",
              multiple = FALSE
            ),
            
            # provide columns for the selected graph
            uiOutput("allInputs")
          ),
          
          box(
            width = 7,
            title = "Plot Display",
            plotOutput("plot_output")
          )
        )
      )
    ))
)