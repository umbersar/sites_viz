library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(leaflet.extras)
library(leafgl)
library(sf)
library(sp)

plots <- list(
  var_1 =list(
    Histogram = list(
      type = "numerics",
      plot = geom_histogram()
    ),
    Density = list(
      type = "numerics",
      plot = geom_density()
    ),
    Boxplot = list(
      type = "numerics",
      plot = geom_boxplot()
    )
  ),
  var_2 =list(
    Scatter = list(
      type = "numerics",
      plot = geom_point()
    ),
    Smooth = list(
      type = "numerics",
      plot = geom_smooth()
    )
  )
)

longitude <- "o_longitude_GDA94"
latitude <- "o_latitude_GDA94"

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

numerical_columns <- function(data) {
  return (
    names(select_if(data, function(col) return(
      Reduce('&', sapply(col, is.numeric))
    )))
  )
}

###############################################################################
############################### server ########################################
###############################################################################
function(input, output, session) {
  # ------------------------------------------------------------------------- #
  # reactive values
  # ------------------------------------------------------------------------- #
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
  
  # select only the "X", long, and lat columns as map data
  loadMapData <- reactive({
    d <- loadData()
    data <- na.omit(select(d, "X", longitude, latitude))
    pts <- st_as_sf(data, coords=c(longitude, latitude))
    return(list(data=data, pts=pts))
  })
  
  # after the morphology dataset has been loaded, we load
  observeEvent(loadData(), {
    choices <- unique(names(morphData()))
    updateSelectInput(session, "filter_attr", choices = choices)
    updateSelectInput(session, "summary_attr", choices = choices)
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
  
  observeEvent(input$summary_attr, {
    output$summary <- renderText(
      paste(
        names(summary(morphData()[[input$summary_attr]])),
        summary(morphData()[[input$summary_attr]]),
        sep = ": ",
        collapse = "\n"
      )
    )
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
  # map
  # ------------------------------------------------------------------------- #
  
  observeEvent(loadData(), {
    map_data <- loadMapData()
    mapData$data <- map_data$data
    mapData$pts <- map_data$pts
    mapData$coordinates <- SpatialPointsDataFrame(
      mapData$data[, c(longitude, latitude)], 
      mapData$data
    )
    output$morph_map <- renderLeaflet(
      leaflet() %>%
        addTiles(options = tileOptions(minZoom=2, maxZoom=15)) %>%
        setView(lng = 134, lat = -24, zoom = 2) %>%
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
    
    # display selected data points in the data table
    if (length(data_of_click$clickedMarker) > 0) {
      renderTable(
        loadData() %>%
        subset(X %in% data_of_click$clickedMarker) %>%
        select(-"X")
      )
    }
    
    cat("length of layerID after adding: ")
    cat(length(layerID$ids))
    cat("\n")
    cat("\n")
    
    # look up data points by ids found
    selected <- subset(mapData$data, X %in% found_in_bounds)
    selected_pts <- st_as_sf(selected, coords = c(longitude, latitude))
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
      
      ids_to_remove <- subset(mapData$data, X %in% bounded_layer_ids)$X
      
      data_of_click$clickedMarker <- data_of_click$clickedMarker[
        !data_of_click$clickedMarker %in% ids_to_remove
      ]
      
      # populate data table with original dataset 
      # in case there are no data points selected
      if (length(data_of_click$clickedMarker) == 0) {
        renderTable(morphData())
      }
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
  
  # ------------------------------------------------------------------------- #
  # plots
  # ------------------------------------------------------------------------- #
  
  # trigger shiny to update choices for the selectizeInput, "plot_variable_type"
  loadPlots <- reactive({
    req(input$plot_variable_type)
    choices = NULL
    if (input$plot_variable_type == "1") {
      choices <- names(plots$var_1)
    }
    else if (input$plot_variable_type == "2") {
      choices <- names(plots$var_2)
    }
    return(choices)
  })
  
  observeEvent(loadPlots(), {
    req(input$plot_variable_type)
    choices <- loadPlots()
    updateSelectInput(session, "plot_selection", choices = choices)
  })
  
  # initialise an input tag list
  inputTagList <- tagList()
  # create column inputs
  createColumnInputs <- reactive({
    req(input$plot_variable_type)
    req(input$plot_selection)
    if (input$plot_variable_type == "1") {
      inputId <- "plot_one_var"
      inputLabel <- "Select a Column as Input"
      # set inputTagList
      choices <- morphData()
      type <- plots$var_1[[input$plot_selection]]$type
      if (!is.null(type)) {
        if (type == "numerics") {
          choices <- numerical_columns(morphData())
        }
      }
      inputTagList <<- tagSetChildren(
        inputTagList, 
        selectInput(
          inputId = inputId, 
          label = inputLabel,
          choices = choices
        )
      )
    }
    else if (input$plot_variable_type == "2") {
      newInputs = list();
      choices <- NULL;
      choices <- names(morphData())
      type <- plots$var_2[[input$plot_selection]]$type
      cat(type)
      if(!is.null(type)) {
        if (type == "numerics") {
          choices <- numerical_columns(morphData())
        }
      }
      for (var in c("x", "y")) {
        inputId <- paste0("plot_two_var_", var)
        inputLabel <- paste0("Select a Column as ", var)
        newInputs[[var]] <- selectInput(
          inputId = inputId, 
          label = inputLabel, 
          choices = choices
        )
      }
      inputTagList <<- tagSetChildren(inputTagList, newInputs)
    }
    # add submit button
    inputTagList <<- tagAppendChild(inputTagList, actionButton("doPlot", "Plot"))
    return(inputTagList)
  })
  
  observeEvent(createColumnInputs(), {
    output$allInputs <- renderUI({createColumnInputs()})
  })
  
  observeEvent(input$doPlot, {
    req(input$plot_selection)
    plot_selection <- input$plot_selection
    if (input$plot_variable_type == "1") {
      output$plot_output <- renderPlot(
        ggplot(morphData(), aes_string(input$plot_one_var)) +
          plots$var_1[[plot_selection]]$plot
      )
    }
    else if (input$plot_variable_type == "2") {
      output$plot_output <- renderPlot(
        ggplot(morphData(), aes_string(input$plot_two_var_x, input$plot_two_var_y)) +
          plots$var_2[[plot_selection]]$plot
      )
    }
  })
  
  # render table helper
  renderTable <- function(data) {
    output$morph_data <- DT::renderDataTable(
      DT::datatable(
        data,
        options = list(
          pageLength = 10, 
          scrollX = TRUE,
          ordering = FALSE,
          autoWidth = TRUE,
          info = FALSE
        )
      )
    )
  }
}
