library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(leaflet.extras)
library(leafgl)
library(sf)
library(sp)


###############################################################################
############################### parameters  ###################################
###############################################################################


###############################################################################
########################### data preparation ##################################
###############################################################################


longitutde <- "o_longitude_GDA94"
latitude <- "o_latitude_GDA94"

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
########################### helper functions ##################################
###############################################################################


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


###############################################################################
################################# ui ##########################################
###############################################################################


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
      # --------------------------------------------------------------------- #
      # About Page
      # --------------------------------------------------------------------- #
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
            title = "Overview of Selected Morphology Attribute",
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
      
      
      # --------------------------------------------------------------------- #
      # Settings Page
      # --------------------------------------------------------------------- #
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


###############################################################################
############################### server ########################################
###############################################################################


server <- function(input, output, session) {
  layerID <- reactiveValues(
    ids = list()
  )
  
  data_of_click <- reactiveValues(
    clickedMarker = list(),
    layerCount = 0
  )
  mapData <- reactiveValues(
    data=NULL,
    pts=NULL,
    coordinates=NULL
  )
  # ------------------------------------------------------------------------- #
  # reactive function for loading lab results for the 
  # selected morphology attribute
  # ------------------------------------------------------------------------- #
  
  # load whole morphology dataset from .csv file
  loadData <- reactive({
    data <- read.csv(stringr::str_interp("data/${input$morph_attr}.csv"))
    filtered <- data %>%
      select_if(function(col) return(!col_has_val(col))) %>%
      select(-"morphology_attribute")
    return(filtered)
  })
  
  # filter out the "X" column to give us the morphology data
  morphData <- reactive({
    return(loadData() %>% select(-"X"))
  })
  
  loadMapData <- reactive({
    d <- loadData()
    data <- na.omit(select(d, "X", longitutde, latitude))
    pts <- st_as_sf(data, coords=c(longitutde, latitude))
    return(list(data=data, pts=pts))
  })
  
  # trigger shiny to update choices for the selectizeInput, "filter_attr"
  observeEvent(loadData(), {
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
        options = list(
          pageLength = 10, 
          scrollX = TRUE,
          ordering = FALSE,
          autoWidth = TRUE,
          info = FALSE
        )
      )
    } else {
      DT::datatable(
        morphData(),
        options = list(
          pageLength = 10, 
          scrollX = TRUE,
          ordering = FALSE,
          autoWidth = TRUE,
          info = FALSE
        )
      )
    }
  })
  
  
  # ------------------------------------------------------------------------- #
  # plots
  # ------------------------------------------------------------------------- #
  
  
  # ------------------------------------------------------------------------- #
  # map
  # ------------------------------------------------------------------------- #
  
  observeEvent(loadData(), {
    map_data <- loadMapData()
    mapData$data <- map_data$data
    mapData$pts <- map_data$pts
    mapData$coordinates <- SpatialPointsDataFrame(
      mapData$data[, c(longitutde, latitude)], 
      mapData$data
    )
    output$morph_map <- renderLeaflet(
      leaflet() %>%
        addTiles(options = tileOptions(minZoom=2, maxZoom=15)) %>%
        setView(lng = 134, lat = -24, zoom = 7) %>%
        addGlPoints(
          data = mapData$pts,
          group = "pts",
          radius = 5,
          fillColor = "black",
          fillOpacity = 1,
          weight = 2,
          stroke = T,
          layerId = as.character(mapData$X),
          highlightOptions = highlightOptions(
            fillColor = "red",
            weight = 10,
            bringToFront = TRUE
          )
        ) %>%
        addDrawToolbar(
          targetGroup = 'Selected',
          polylineOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          polygonOptions = drawPolygonOptions(
            shapeOptions = drawShapeOptions(
              fillOpacity = 0,
              color = "white",
              weight = 3
            )
          ),
          rectangleOptions = drawRectangleOptions(
            shapeOptions = drawShapeOptions(
              fillOpacity = 0,
              color = "white",
              weight = 3
            )
          ),
          circleOptions = drawCircleOptions(
            shapeOptions = drawShapeOptions(
              fillOpacity = 0,
              color = "white",
              weight = 3
            )
          ),
          editOptions = editToolbarOptions(
            edit = FALSE,
            selectedPathOptions = selectedPathOptions()
          )
        ) 
    )
  })
  
  # draw new shape
  observeEvent(input$morph_map_draw_new_feature, {
    # only add new layers for bounded locations
    data_of_click$layerCount <- data_of_click$layerCount + 1
    found_in_bounds <- findLocations(
      shape = input$morph_map_draw_new_feature,
      location_coordinates = mapData$coordinates,
      location_id_colname = "X"
    )
    
    cat("length of layerID before adding: ")
    cat(length(layerID$ids))
    cat("\n")
    cat("\n")
    
    for (id in found_in_bounds) {
      if (id %in% data_of_click$clickedMarker) {
        # do not add id
      } 
      else {
        # add id
        data_of_click$clickedMarker <- append(
          data_of_click$clickedMarker, id, 0
        )
        key <- as.character(id)
        layerID$ids[[key]] <- data_of_click$layerCount
      }
    }
    
    cat("length of layerID after adding: ")
    cat(length(layerID$ids))
    cat("\n")
    cat("\n")
    
    # look up data points by ids found
    selected <- subset(mapData$data, X %in% data_of_click$clickedMarker)
    selected_pts <- st_as_sf(selected, coords = c(longitutde, latitude))
    proxy <- leafletProxy("morph_map")
    proxy %>% 
      addGlPoints(
        data = selected_pts,
        radius = 5,
        fillOpacity = 1,
        color = "red",
        weight = 3,
        stroke = T,
        layerId = as.character(data_of_click$layerCount),
      )
  })
  
  # delete shape(s)
  observeEvent(input$morph_map_draw_deleted_features, {
    # loop through list of one or more deleted features/polygons
    for (feature in input$morph_map_draw_deleted_features$features) {
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(
        shape = feature,
        location_coordinates = mapData$coordinates,
        location_id_colname = "X"
      )
      
      # remove second layer representing selected locations
      proxy <- leafletProxy("morph_map")
      
      cat("to delete layer id: ")
      cat(layerID$ids[[ as.character(bounded_layer_ids[1]) ]])
      cat("\n")
      
      # remove deleted points
      for (id in bounded_layer_ids) {
        key <- as.character(id)
        proxy %>% removeGlPoints(
          layerId = as.character(layerID$ids[[ key ]])
        )
        layerID$ids[[key]] <- NULL
      }
      
      output$summary <- renderText(
        length(layerID$ids)
      )
      
      ids_to_remove <- subset(mapData$data, X %in% bounded_layer_ids)$X
      
      data_of_click$clickedMarker <- data_of_click$clickedMarker[
        !data_of_click$clickedMarker %in% ids_to_remove
      ]
    }
  })
  
  findLocations <- function(shape, location_coordinates, location_id_colname) {
    # derive polygon coordinates and feature_type from shape input
    polygon_coordinates <- shape$geometry$coordinates
    feature_type <- shape$properties$feature_type
    
    if (feature_type %in% c("rectangle", "polygon")) {
      # transform into a spatial polygon
      drawn_polygon <- Polygon(
        do.call(
          rbind, 
          lapply(polygon_coordinates[[1]], 
                 function(x){
                   c(x[[1]][1], x[[2]][1])
                 }
          )
        )
      )
      
      # use 'over' from the sp package to identify selected locations
      selected_locs <- sp::over(
        location_coordinates,
        sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon), "drawn_polygon")))
      )
      
      # get ids
      x = (location_coordinates[
        which(!is.na(selected_locs)), location_id_colname
        ]
      )
      selected_loc_id = as.character(x[[location_id_colname]])
      
      return (selected_loc_id)
    } 
    else if (feature_type == "circle") {
      center_coords <- matrix(
        c(polygon_coordinates[[1]], polygon_coordinates[[2]]), 
        ncol = 2
      )
      # get distances to center of drawn circle for all locations in location_coordinates
      # distance is in kilometers
      dist_to_center <- spDistsN1(
        location_coordinates, 
        center_coords, 
        longlat = TRUE
      )
      
      # get location ids
      # radius in meters
      x <- location_coordinates[
        dist_to_center < shape$properties$radius/1000, 
        location_id_colname
      ]
      
      selected_loc_id = as.character(x[[location_id_colname]])
      return(selected_loc_id)
    }
  }
}

shinyApp(ui, server)
