library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
source("src/elements.R")
source("src/datasets.R")

# parameters and variables
left_width <- 5
right_width <- 12 - left_width

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Soil Data Explorer"),
  
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("dashboard")),
      menuItem("Explore", tabName = "explore", icon = icon("flask"), selected = T)
    )
  ),
  
  # body  
  dashboardBody(
    tabItems(
      # About Page
      tabItem(
        tabName = "about",
        fluidRow(
          column(width = 6, box(width = 12, title = "About", about)),
          column(width = 6, box(width = 12, title = "How To", how_to))
        )
      ),
      
      # Explore Page
      tabItem(
        tabName = "explore",
        
        #---------------------------- data -------------------------#
        fluidRow(
          ############################
          # data loading and summary #
          ############################
          column(
            width=left_width,
            #--------------#
            # data loading #
            #--------------#
            box (
              width = 12,
              title = "Dataset Selection",
              
              # dataset selection
              selectizeInput(
                inputId = "dataset",
                label = "Select a Dataset",
                choices = datasets,
                selected = "",
                multiple = FALSE
              ),
              
              actionButton("doLoad", "Load Dataset")
            ),
            
            #--------------#
            # data summary #
            #--------------#
            box (
              width = 12,
              title = "DataTable Data Summary",
              selectizeInput(
                inputId = "summary_col", 
                label = "Select a column to see its summary.", 
                choices = NULL, 
                selected = "",
                multiple = FALSE
              ),
              p("Summary: "),
              conditionalPanel("output.show_summary", verbatimTextOutput("summary")),
              actionButton(
                "summary_button",
                "Show Summary"
              )
            )
          ),
          
          ####################
          # dataset trimming #
          ####################
          column(
            width = right_width,
            #--------------#
            # drop columns #
            #--------------#
            box(
              width = 12,
              title = "Dataset Trimming",
              # drop columns
              selectizeInput(
                inputId = "drop_cols", 
                label = "Select Columns to Hide", 
                choices = NULL, 
                multiple = TRUE
              ),
              
              # do drop button
              actionButton("doDrop", "Hide Selected Columns")
            ),
            
            box(
              width = 12,
              title = "Missing Geolocations",
              uiOutput("geo_nas"),
              
              conditionalPanel(
                condition = "output.show_geo_nas",
                DT::dataTableOutput("geo_nas_dt")
              )
            )
          )
        ),
        
        #-------------------------- map and data table -----------------------#
        fluidRow(
          column(
            width = left_width,
            #----------------------#
            # map for geolocations #
            #----------------------#
            box(
              title = "Geolocation Map",
              leafletOutput("geo_map", width = "100%", height = "450px"),
              width = 12
            ),
            
            #----------------#
            # Filter Dataset #
            #----------------#
            box(
              title = "Data Filtering",
              width = 12,
              # select column to provide a basis for filtering
              selectizeInput(
                inputId = "filter_col", 
                label = "Filter Dataset By Column", 
                choices = NULL, 
                selected = "",
                multiple = FALSE
              ),
              
              # select a value in the above selected column to filter
              selectizeInput(
                inputId = "filter_val",
                label = "Column Value Selection",
                choices = NULL,
                selected = "",
                multiple = FALSE
              ),
              
              radioButtons(
                inputId = "filter_target", 
                label = "Apply Filter To", 
                choices = c("Whole Dataset" = "dataset", "Current DataTable" = "datable"),
                selected = "dataset"
              ),
              # filter button
              actionButton("doFilter", "Apply Filter")
            )
          ),
          
          column(
            width = right_width,
            #-------------------#
            # display datatable #
            #-------------------#
            box(
              title = "DataTable",
              DT::dataTableOutput('datatable'),
              uiOutput("downloadUI"),
              width = 12
            )
          )
        ),
        
        
        # plots
        fluidRow(
          column(
            width = left_width,
            box(
              width = 12,
              title = "Plot Selection",
              # plot selection
              selectizeInput(
                inputId = "plot_var_type", 
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
            )
          ),
          
          column(
            width = right_width,
            
            box(
              width = 12,
              title = "Plot Display",
              plotOutput("plot_output")
            )
          )
        )
      )
    )
  )
)