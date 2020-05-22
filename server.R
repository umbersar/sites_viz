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
library(DT)

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

col_has_val <- function(col, val=c("N/A", "NA")) {
  for(i in 1:length(col)) {
    if(is.na(col[i])){
      next
    }
    if(col[i] %in% val) {
      return(TRUE)
    }
  }
  return(FALSE)
}


###############################################################################
############################### server ########################################
###############################################################################
function(input, output, session) {
  # ------------------------------------------------------------------------- #
  # reactive values
  # ------------------------------------------------------------------------- #
  mapData <- reactiveValues(
    data=NULL,
    pts=NULL,
    coordinates=NULL
  )
  plotData <- reactiveValues(
    numerical_columns = NULL
  )
  datatableData <- reactiveValues(
    data = NULL,
    dataX = NULL
  )
  
  rv <- reactiveValues(
    map_layer_ids = list(),
    map_selected = list(),
    map_layer_count = 0,
    map_data = NULL,
    map_pts = NULL,
    map_coordinates = NULL,
    plot_numerical_columns = NULL,
    dt_dataX = NULL,
    dt_data = NULL
  )
  
  # ------------------------------------------------------------------------- #
  # reactive function for loading lab results for the 
  # selected morphology attribute
  # ------------------------------------------------------------------------- #
  
  # load whole morphology dataset from .csv file
  loadData <- eventReactive(input$doLoad, {
    data <- read.csv(stringr::str_interp("data/${input$dataset}.csv"))
    # filtered <- data %>%
    #   select_if(function(col) return(!col_has_val(col)))
    # if ("morphology_attribute" %in% names(filtered)) {
    #   filtered %>% select(-"morphology_attribute")
    # }
    # data init
    rv$map_layer_ids <- list()
    rv$map_selected <- list()
    rv$map_layer_count <- 0
    mapData$data <- NULL
    mapData$pts <- NULL
    mapData$coordinates <- NULL
    plotData$numerical_columns <- NULL
    datatableData$data <- data %>% select(-"X")
    datatableData$dataX <- data
    
    # update select input for drop_cols
    updateSelectizeInput(
      session, "drop_cols", 
      choices = names(datatableData$data)
    )
    
    # create download button for data table
    output$downloadUI <- renderUI({
      tagList() %>%
        tagAppendChild(p()) %>% # space
        tagAppendChild(
          p(
            class='text-center', 
            downloadButton("download", "Download Current DataTable")
          )
        )
    })
    # bind download button with downloadHandler
    output$download = downloadHandler('data.csv', content = function(file) {
      s = input$datatable_rows_all
      write.csv(datatableData$data[s, , drop = FALSE], file)
    })
    
    # return
    return(data)
  })
  
  observeEvent(input$doDrop, {
    req(loadData())
    # if we are displaying data points in selected areas
    # then drop based on current set of data points
    if (length(rv$map_selected) > 0 ) {
      datatableData$data <- datatableData$dataX %>% 
        select(-input$drop_cols) %>%
        select(-"X")
    }
    else { # otherwise drop based on the original dataset
      datatableData$data <- datatableData$dataX %>% 
        select(-input$drop_cols) %>% 
        select(-"X")
    }
  })
  
  # # filter out the "X" column to give us the morphology data
  # morphData <- reactive({
  #   return(loadData() %>% select(-"X"))
  # })
  
  # select only the "X", long, and lat columns as map data
  loadMapData <- eventReactive(loadData(), {
    d <- loadData()
    data <- na.omit(select(d, "X", longitude, latitude))
    pts <- st_as_sf(data, coords=c(longitude, latitude))
    return(list(data=data, pts=pts))
  })
  
  # after the morphology dataset has been loaded, we load
  observeEvent(loadData(), {
    choices <- unique(names(datatableData$data))
    updateSelectizeInput(session, "filter_attr", choices = choices)
    updateSelectizeInput(session, "summary_attr", choices = choices)
  })
  
  # load the unqiue values present in a morph attribute column
  # according to the selection of filter attribute
  # trigger shiny to update choices for the selectizeInput, "filter_val"
  observeEvent(input$filter_attr, {
    req(input$filter_attr)
    choices <- unique(select(datatableData$dataX, input$filter_attr))
    updateSelectizeInput(session, "filter_val", choices = choices)
  })
  
  observeEvent(input$filter_val, {
    if (input$filter_checkbox) {
      datatableData$dataX <- loadData() %>% 
        dplyr::filter((!!sym(input$filter_attr)) == input$filter_val)
      datatableData$data <- datatableData$dataX %>% 
        select(-input$drop_cols) %>%
        select(-"X")
    } else {
      datatableData$dataX <- loadData()
      datatableData$data <- datatableData$dataX %>% 
        select(-input$drop_cols) %>%
        select(-"X")
    }
  })
  
  observeEvent(input$summary_attr, {
    output$summary <- renderText(
      paste(
        names(summary(datatableData$data[[input$summary_attr]])),
        summary(datatableData$data[[input$summary_attr]]),
        sep = ":  ",
        collapse = "\n"
      )
    )
  })
  
  # filter data
  observeEvent(input$filter_checkbox, {
    output$datatable <- DT::renderDataTable({
      if (input$filter_checkbox) {
        datatableData$dataX <- loadData() %>% 
          dplyr::filter((!!sym(input$filter_attr)) == input$filter_val)
        datatableData$data <- datatableData$dataX %>% 
          select(-input$drop_cols) %>%
          select(-"X")
      } else {
        datatableData$dataX <- loadData()
        datatableData$data <- datatableData$dataX %>% 
          select(-input$drop_cols) %>%
          select(-"X")
      }
    })
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
    output$geo_map <- renderLeaflet(
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
  observeEvent(input$geo_map_draw_new_feature, {
    # only add new layers for bounded locations
    rv$map_layer_count <- rv$map_layer_count + 1
    found_in_bounds <- findLocations(
      shape = input$geo_map_draw_new_feature,
      location_coordinates = mapData$coordinates,
      location_id_colname = "X"
    )
    
    for (id in found_in_bounds) {
      if (id %in% rv$map_selected) {
        # do not add id
      } 
      else {
        # add id
        rv$map_selected <- append(
          rv$map_selected, id, 0
        )
        key <- as.character(id)
        rv$map_layer_ids[[key]] <- rv$map_layer_count
      }
    }
    
    # display selected data points in the data table
    if (length(rv$map_selected) > 0) {
      # filter dataX
      datatableData$dataX <- datatableData$dataX %>% 
        subset(X %in% rv$map_selected)
      # display data by dropping X
      datatableData$data <- datatableData$dataX %>%
        select(-input$drop_cols) %>%
        select(-"X")
    }
    
    # look up data points by ids found
    selected <- subset(mapData$data, X %in% found_in_bounds)
    selected_pts <- st_as_sf(selected, coords = c(longitude, latitude))
    proxy <- leafletProxy("geo_map")
    proxy %>% 
      addGlPoints(
        data = selected_pts,
        radius = 5,
        fillOpacity = 1,
        color = "red",
        weight = 3,
        stroke = T,
        layerId = as.character(rv$map_layer_count),
      )
  })
  
  # delete shape(s)
  observeEvent(input$geo_map_draw_deleted_features, {
    # loop through list of one or more deleted features/polygons
    for (feature in input$geo_map_draw_deleted_features$features) {
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(
        shape = feature,
        location_coordinates = mapData$coordinates,
        location_id_colname = "X"
      )
      
      # remove second layer representing selected locations
      proxy <- leafletProxy("geo_map")
      
      # remove deleted points
      for (id in bounded_layer_ids) {
        key <- as.character(id)
        proxy %>% removeGlPoints(
          layerId = as.character(rv$map_layer_ids[[ key ]])
        )
        rv$map_layer_ids[[key]] <- NULL
      }
      
      ids_to_remove <- subset(mapData$data, X %in% bounded_layer_ids)$X
      
      rv$map_selected <- rv$map_selected[
        !rv$map_selected %in% ids_to_remove
      ]
      
      # populate data table with original dataset 
      # in case there are no data points selected
      if (length(rv$map_selected) == 0) {
        datatableData$dataX <- loadData()
        datatableData$data <- datatableData$dataX %>% 
          select(-input$drop_cols) %>%
          select(-"X")
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
  observeEvent(input$plot_variable_type, {
    req(input$plot_variable_type)
    choices = NULL
    if (input$plot_variable_type == "1") {
      choices <- names(plots$var_1)
    }
    else if (input$plot_variable_type == "2") {
      choices <- names(plots$var_2)
    }
    updateSelectizeInput(session, "plot_selection", choices = choices)
  })
  
  # initialise an input tag list
  inputTagList <- tagList()
  # create column inputs
  
  
  createColumnInputs <- reactive({
    req(loadData())
    req(input$plot_variable_type)
    req(input$plot_selection)
    if (input$plot_variable_type == "1") {
      inputId <- "plot_one_var"
      inputLabel <- "Select a Column as Input"
      # set inputTagList
      choices <- names(datatableData$data)
      type <- plots$var_1[[input$plot_selection]]$type
      if (!is.null(type)) {
        if (type == "numerics") {
          choices <- numerical_columns(datatableData$data)
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
      choices <- names(datatableData$data)
      type <- plots$var_2[[input$plot_selection]]$type
      if(!is.null(type)) {
        if (type == "numerics") {
          choices <- numerical_columns(datatableData$data)
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
  
  generatePlot <- eventReactive(input$doPlot, {
    # req(input$plot_selection)
    if (input$doPlot >= 1) {
      plot_selection <- input$plot_selection
      if (input$plot_variable_type == "1") {
        ggplot(datatableData$data, aes_string(input$plot_one_var)) +
          plots$var_1[[plot_selection]]$plot
      }
      else if (input$plot_variable_type == "2") {
        ggplot(datatableData$data, aes_string(input$plot_two_var_x, input$plot_two_var_y)) +
          plots$var_2[[plot_selection]]$plot
      }
    }
  })
  
  output$plot_output <- renderPlot({
    generatePlot()
  })
  
  # ------------------------------------------------------------------------- #
  # data table update only on changes to datatableData$data
  # ------------------------------------------------------------------------- #
  
  observeEvent(datatableData$data, {
    output$datatable <- DT::renderDataTable(
      DT::datatable(
        datatableData$data,
        options = list(
          pageLength = 10, 
          lengthMenu = list(c(5,10,15), c("5", "10", "15")),
          scrollX = TRUE,
          ordering = FALSE,
          autoWidth = TRUE,
          info = FALSE
        )
      )
    )
  })
  
  # ------------------------------------------------------------------------- #
  # helper functions
  # ------------------------------------------------------------------------- #
  
  # return numerical columns in the given "data" data table  
  numerical_columns <- function(data) {
    if (is.null(plotData$numerical_columns)) {
      plotData$numerical_columns <- names(select_if(data, function(col) return(
        Reduce('&', sapply(col, is.numeric))
      )))
    }
    return(plotData$numerical_columns)
  }
}
