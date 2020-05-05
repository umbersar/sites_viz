library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(dplyr)


geo_long <- "o_longitude_GDA94"
geo_lat <- "o_latitude_GDA94"

# -------------------------------------------------------------------------- #
# helper functions
# -------------------------------------------------------------------------- #
col_has_val <- function(col, val="N/A") {
  for(i in 1:length(col)) {
    if(is.na(col[i])){
      next
    }
    if(col[i] == val) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# -------------------------------------------------------------------------- #
# load all the available morphology class attribtues
# -------------------------------------------------------------------------- #
morph_meta <- read.csv("data/morph_metadata.csv")
# morph_attrs <- morph_meta %>% select("Morphology_Class_Attribute")
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


#########################################################################################################
############################################## ui #######################################################
#########################################################################################################


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Soil Data Explorer"),
  
  # sidebar
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("dashboard")),
      menuItem("Explore", tabName = "explore", icon = icon("flask")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )),
  
  # body  
  dashboardBody(
    tabItems(
      # -------------------------------------------------------------------------- #
      # About Page
      # -------------------------------------------------------------------------- #
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "About",
            p("1. About the Project itself\n"),
            p("2. About Soil Data\n"),
            p("3. About the web app\n")
          )
        )),
      
      
      # -------------------------------------------------------------------------- #
      # Explore Page
      # -------------------------------------------------------------------------- #
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
            checkboxInput("filter_checkbox", "Check the Box to Filter Data", value = FALSE, width = "100%"),
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
            title = "Overview of Selected Morphology Attribute",
            verbatimTextOutput("summary")
          )
        ),
        
        # data display of the selected morphology data
        fixedRow(
          box(
            title = "section for map",
            leafletOutput("lab_results_map", width = "100%", height = "540px"),
            height = "600px",
            width = 4,
            solidHeader = TRUE
          ),
          box(
            title = "Data Display -- Filtered Dataset",
            DT::dataTableOutput('morph_data'),
            height = "600px",
            id = "datatable",
            width = 8,
            solidHeader = TRUE
          )
        ),
        
        # plots
        fixedRow(
          box(
            width = 12,
            title = "section for plots",
            
            # Morphology Attribute Values Against Latitude Values
            plotOutput("scatter_morph_attr_lat"),
            
            # Morphology Attribute Values Box Plot
            plotOutput("boxplot_morph_attr"),
            
            # Pie Chart for Project Code
            plotOutput("bar_agency_code")
          )
        )
      ),
      
      
      # -------------------------------------------------------------------------- #
      # Settings Page
      # -------------------------------------------------------------------------- #
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            width = 12,
            title = "Settings"
          )
        )
      )
    ))
)


#########################################################################################################
############################################ server #####################################################
#########################################################################################################


server <- function(input, output, session) {
  # -------------------------------------------------------------------------- #
  # load lab results for the selected morphology attribute
  # -------------------------------------------------------------------------- #
  morphData <- reactive({
    data <- read.csv(stringr::str_interp("data/${input$morph_attr}.csv"))
    filtered <- data %>% 
      select_if(function(col) return(!col_has_val(col))) %>%
      select(-"morphology_attribute", -"X")
    return(filtered)
  })
  
  # trigger shiny to update choices for the selectizeInput, "filter_attr"
  observeEvent(morphData(), {
    choices <- unique(names(morphData()))
    updateSelectInput(session, "filter_attr", choices = choices)
  })
  
  
  # load the unqiue values present in a morph attribute column
  # according to the selection of filter attribute
  filterOptions <- reactive({
    req(input$filter_attr)
    unique(select(morphData(), input$filter_attr))
  })
  # trigger shiny to update choices for the selectizeInput, "filter_val"
  observeEvent(filterOptions(), {
    choices <- filterOptions()
    updateSelectInput(session, "filter_val", choices = choices)
  })
  
  output$morph_data <- DT::renderDataTable({
    if (input$filter_checkbox) {
      DT::datatable(
        dplyr::filter(morphData(), (!!sym(input$filter_attr)) == input$filter_val), 
        options = list(pageLength = 10, scrollX = TRUE)
      )
    } else {
      DT::datatable(
        morphData(),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    }
  })
  
  output$summary <- renderPrint({
    if (input$filter_checkbox) {
      dplyr::filter(morphData(), (!!sym(input$filter_attr)) == input$filter_val) %>%
        select(-input$filter_attr) %>%
        summary()
    } else {
      summary(morphData())
    }
  })
  
  # -------------------------------------------------------------------------- #
  # plots
  # -------------------------------------------------------------------------- #
  
  output$scatter_morph_attr_lat <- renderPlot({
    plot(
      morphData()$o_latitude_GDA94, 
      morphData()$morphology_attribute_value, 
      xlab = "Latitude", 
      ylab = "Morphology Attribute Value (Elevation)",
      main = "Morphology Attribute Value Against Latitude")
  })
  
  output$boxplot_morph_attr <- renderPlot({
    boxplot(
      morphData()$morphology_attribute_value, 
      main = "Morphology Attribute Values Box Plot"
    )
  })
  
  
  output$bar_agency_code <- renderPlot({
    result = plyr::count(morphData(), "agency_code")
     barplot(
      result$freq,
      names.arg = result$agency_code,
      main = "Number of Projects by Agency"
    )
  })
  
  # -------------------------------------------------------------------------- #
  # map
  # -------------------------------------------------------------------------- #
  get_geo_locations <- reactive({
    if (input$filter_checkbox) {
      data <- dplyr::filter(morphData(), (!!sym(input$filter_attr)) == input$filter_val)
    } else {
      data <- morphData()
    }
    data <- data %>% select(geo_long, geo_lat)
    names(data) <- c("lng", "lat")
    return(data)
  })
  
  output$lab_results_map <- renderLeaflet(
    get_geo_locations() %>%
      leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(clusterOptions = markerClusterOptions())
  )
}

shinyApp(ui, server)
