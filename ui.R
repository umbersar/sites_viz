library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)


morph_meta <- read.csv("data/morph_metadata.csv")

# using morphology attribute name to load the corresponding .csv file
# for the time being, we are only testing with a small subset
morph_attrs <- c(
  "cf_shape", 
  "crack_width", 
  "h_texture", 
  "o_drainage", 
  "o_elevation", 
  "s_patt_type", 
  "samp_upper_depth", 
  "scf_strength"
)
# uncomment the following to work with the full set of attribute names
# morph_attrs <- morph_meta %>% select("Morphology_Class_Attribute")
###############################################################################
################################# ui ##########################################
###############################################################################
about_one <- p("This dashboard is developed using the 
           R package Shiny to provide researchers an easy platform to play with 
           the soil datasets that CSIRO is responsible for managing.")

about_two <- p("For researchers who do not have much R programming experiences or 
               simply cannot commit the amount of time needed to write 
               complex R scripts, it would be very handy for them to have an 
               application that is built on top of R and does the dirty job for them.")

about_three <- p("And that's exactly what this dashboard is trying to deliver. 
                 With this dashboard, a researcher can do the following easily:")

about_three_list <- tags$ol(
  tags$li("Load a dataset of interest;"), 
  tags$li("View basic summary information about that dataset;"), 
  tags$li("View the data points from the selected dataset in a data table;"),
  tags$li("Filter the dataset by meaningful columns and the values in those columns;"),
  tags$li("View the data points on an interactive map according to their geolocations;"),
  tags$li("Select areas of a map and have the data points in the selected areas displayed;"),
  tags$li("Zoom in and out of the map and have the data table updated accordingly"),
  tags$li("Choose between different plots and generate them dynamically.")
)


how_to_one <- p("To get started, simply click on the \"Explore\" tab on the left.")
how_to_two <- p("Please note that after collapsing or expanding the tab menu, 
                the data points on the map may appear to have disappeared. 
                Simply move around the map and the data points shall reappear.")


dashboardPage(
  skin = "black",
  dashboardHeader(title = "Soil Data Explorer"),
  
  # sidebar
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
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
          box(
            width = 12,
            title = "About",
            about_one,
            about_two,
            about_three,
            about_three_list
          ),
          box(
            width = 12,
            title = "How To",
            how_to_one,
            how_to_two
          )
        )),
      
      
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
            title = "Select a Morphology Class Attribute",
            
            # morphology selection
            selectizeInput(
              inputId = "morph_attr", 
              label = "Select a Morphology Class Attribute", 
              choices = morph_attrs, 
              selected = "o_elevation",
              multiple = FALSE
            ),
            
            # decide if we need to apply filtering or not
            checkboxInput("filter_checkbox", 
                          "Check the Box to Filter Data", 
                          value = FALSE, 
                          width = "100%"),
            selectizeInput(
              inputId = "filter_attr", 
              label = "Filter dataset based on values of a column", 
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
            height = "600px",
            width = 5,
            solidHeader = TRUE
          ),
          box(
            title = "Data Display -- Filtered Dataset",
            DT::dataTableOutput('morph_data'),
            height = "600px",
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