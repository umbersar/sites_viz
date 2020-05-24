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


# variables and parameters
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

############################### server ########################################
function(input, output, session) {
  #############################################################################
  ############################# reactive values ###############################
  #############################################################################
  
  rv <- reactiveValues(
    geo_nas = NULL,
    show_geo_nas = FALSE,
    show_summary = FALSE,
    map_layer_ids = list(),
    map_selected = list(),
    map_overlaps = list(),
    map_layer_count = 0,
    map_data = NULL,
    map_pts = NULL,
    map_coords = NULL,
    filter_flag = FALSE,
    plot_numerical_columns = NULL,
    dt_data_x = NULL,
    dt_data = NULL
  )
  
  # output values for conditionalPanels
  output$show_geo_nas <- reactive({
    rv$show_geo_nas
  })
  
  output$show_summary <- reactive({
    rv$show_summary
  })
  
  #############################################################################
  ################## reactive/eventReactive functions #########################
  #############################################################################
  
  # loadData: eventReactive on input$doLoad
  # load selected dataset and do initial work
  # -----------------------------------------
  # specifically:
  # 1. load selected dataset
  # 2. init rv$*
  # 3. UI
  # 4. return loaded dataset: now a df
  loadData <- eventReactive(input$doLoad, {
    # 1. load data
    data <- read.csv(stringr::str_interp("data/${input$dataset}.csv"))
    
    # 2. init rv$*
    rv$geo_nas <- subset(data, c(o_longitude_GDA94, o_latitude_GDA94) %in% NA)
    data <- na.omit(data)
    # 2. init rv$*
    rv$show_geo_nas <- FALSE
    rv$show_summary <- FALSE
    rv$map_layer_ids <- list()
    rv$map_selected <- list()
    rv$map_shapes <- list()
    rv$map_layer_count <- 0
    rv$map_data <- NULL
    rv$map_pts <- NULL
    rv$map_coords <- NULL
    rv$filter_flag <- FALSE
    rv$plot_numerical_columns <- NULL
    rv$dt_data <- data %>% select(-"X")
    rv$dt_data_x <- data
    
    # 3. update select input for drop_cols
    updateSelectizeInput(
      session, "drop_cols", 
      choices = names(rv$dt_data)
    )
    
    # 3. create append and download buttons for data table
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
    
    # 3. render summary text for geo_nas
    output$geo_nas_summary <- renderText(
      paste0(
        "There are ",
        nrow(rv$geo_nas),
        " rows with incomplete geolocations"
      )
    )

    output$download_geo_nas = downloadHandler(
      'missing_geo.csv', 
      content = function(file) {
        s = input$geo_nas_dt_rows_all
        write.csv(rv$geo_nas[s, , drop = FALSE], file)
      }
    )
    # 3. bind download button with downloadHandler
    output$download = downloadHandler('data.csv', content = function(file) {
      s = input$datatable_rows_all
      write.csv(rv$dt_data[s, , drop = FALSE], file)
    })

    # 4 return loaded dataset: now a df
    return(data)
  })
  
  
  # loadMapData: eventReactive on loadData()
  # create map data from the loaded dataset once loadData() is called
  # -----------------------------------------------------------------
  # specifically:
  # 1. select only the X, long, and lat columns of the loaded dataset
  # 2. drop any rows with NA, i.e. a data point without a proper geolocation
  # 3. create sf object pts
  # 4. return a list of data and pts
  loadMapData <- eventReactive(loadData(), {
    d <- loadData()
    data <- na.omit(select(d, "X", longitude, latitude))
    pts <- st_as_sf(data, coords=c(longitude, latitude))
    return(list(data=data, pts=pts))
  })
  
  
  # createPlotInputs: reactive
  # dynamically create column inputs for generating plots
  # -----------------------------------------------------
  createPlotInputs <- reactive({
    req(loadData())
    req(input$plot_var_type)
    req(input$plot_selection)
    # load available plots based on input$plot_var_type
    if (input$plot_var_type == "1") {
      inputId <- "plot_one_var"
      inputLabel <- "Select a Column as Input"
      # set inputTagList
      choices <- names(rv$dt_data)
      type <- plots$var_1[[input$plot_selection]]$type
      # written in a nested fashion due to R's way of viewing booleans
      if (!is.null(type)) {
        if (type == "numerics") {
          choices <- numerical_columns(rv$dt_data)
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
    else if (input$plot_var_type == "2") {
      newInputs = list();
      choices <- NULL;
      choices <- names(rv$dt_data)
      type <- plots$var_2[[input$plot_selection]]$type
      if(!is.null(type)) {
        if (type == "numerics") {
          choices <- numerical_columns(rv$dt_data)
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
  
  
  # generatePlot: eventReactive on input$doPlot
  # only plot when the plot button is pressed, plot using rv$dt_data
  # ----------------------------------------------------------------
  generatePlot <- eventReactive(input$doPlot, {
    # req(input$plot_selection)
    if (input$doPlot >= 1) {
      plot_selection <- input$plot_selection
      if (input$plot_var_type == "1") {
        ggplot(rv$dt_data_x, aes_string(input$plot_one_var)) +
          plots$var_1[[plot_selection]]$plot
      }
      else if (input$plot_var_type == "2") {
        ggplot(rv$dt_data_x, aes_string(input$plot_two_var_x, input$plot_two_var_y)) +
          plots$var_2[[plot_selection]]$plot
      }
    }
  })
  
  
  # return geo_nas_table on data loading and columns hiding
  geo_nas_table <- eventReactive(list(input$doDrop, loadData()), {
    DT::datatable(
      rv$geo_nas %>% select(-input$drop_cols) %>% select(-"X"),
      options = list(
        pageLength = 5, 
        lengthMenu = list(c(5,10,15), c("5", "10", "15")),
        scrollX = TRUE,
        ordering = FALSE,
        autoWidth = TRUE,
        info = FALSE,
        dom = 'ltipr'
      )
    )
  })
  #############################################################################
  ############################## observeEvents ################################
  #############################################################################
  
  # observe rv$dt_data
  # ------------------
  # use rv$dt_data to update choices for data filtering and summary
  # and also input$filter_val
  observeEvent(rv$dt_data, {
    req(loadData())
    # only use rv$dt_data to update main data table 
    # when we are not in the process of filtering
    if (!rv$filter_flag) {
      cols <- unique(names(rv$dt_data))
      updateSelectizeInput(session, "filter_col", choices = cols)
      updateSelectizeInput(session, "summary_col", choices = cols)
      
      if (length(cols) > 0) { # update input$filter_val
        vals <- unique(select(rv$dt_data, cols[1]))
        updateSelectizeInput(
          session, 
          "filter_val", 
          choices = vals[[ cols[[1]] ]]
        )
      }
    }
  })
  
  
  # observe input$doDrop
  # ------------------------------
  # drop columns as specified in input$drop_cols
  # this applies for both geo_nas dt and main dt
  observeEvent(input$doDrop, {
    req(loadData())
    if (input$doDrop >= 1) {
      update_dt_data(rv$dt_data_x)
      output$geo_nas_dt <- DT::renderDataTable(geo_nas_table())
    }
  })
  
  
  # observe input$filter_col
  # -------------------------
  # update input$filter_val according to input$filter_col
  observeEvent(input$filter_col, {
    req(input$filter_col)
    choices <- unique(select(rv$dt_data, input$filter_col))
    updateSelectizeInput(
      session, 
      "filter_val", 
      choices = choices[[input$filter_col]]
    )
  })
  
  
  # observe input$filter_val
  # ------------------------
  # when input$filter_checkbox is checked, 
  # update rv$dt_data_x and rv$dt_data according to 
  # input$filter_col and input$filter_val
  observeEvent(input$filter_val, {

  })
  
  
  # observe input$filter_checkbox
  # -----------------------------
  # filter loaded dataset when input$filter_checkbox is checked
  # and restore datatable when input$filter_checkbox is unchecked
  # *** note that in either case, we drop the columns that are selected in input$drop_cols
  # 1. highlight filtered data points on the map when checked
  # 2. remove highlight when unchecked
  # 3. 
  observeEvent(input$doFilter, {
    req(input$filter_col)
    req(input$filter_val)
    
    if (!rv$filter_flag) { # no filter, then apply filter
      rv$filter_flag <- !rv$filter_flag
      apply_filter_helper()
      # update button
      updateActionButton(
        session,
        "doFilter",
        "Remove Filter"
      )
    }
    else { # has filter, then drop filter
      rv$filter_flag <- !rv$filter_flag
      remove_filter_helper()
      # update button
      updateActionButton(
        session,
        "doFilter",
        "Apply Filter"
      )
    }
  })
  
  # observe input$summary_col
  # --------------------------
  observeEvent(input$summary_col, {
    output$summary <- renderText(
      paste(
        names(summary(rv$dt_data[[input$summary_col]])),
        summary(rv$dt_data[[input$summary_col]]),
        sep = ":  ",
        collapse = "\n"
      )
    )
  })
  
  observeEvent(input$summary_button, {
    req(loadData())
    rv$show_summary <- !rv$show_summary
    outputOptions(output, "show_summary", suspendWhenHidden = FALSE)
    if (rv$show_summary) {
      updateActionButton(session, "summary_button", label = "Hide Summary")
    }
    else {
      updateActionButton(session, "summary_button", label = "Show Summary")
    }
  })
  
  # observe input$geo_nas_button
  observeEvent(input$geo_nas_button, {
    req(loadData())
    rv$show_geo_nas <- !rv$show_geo_nas
    outputOptions(output, "show_geo_nas", suspendWhenHidden = FALSE)
    if (rv$show_geo_nas) {
      updateActionButton(session, "geo_nas_button", label = "Hide Incomplete Geolocations")
      output$geo_nas_dt <- DT::renderDataTable({
        geo_nas_table()
      })
    }
    else {
      updateActionButton(session, "geo_nas_button", label = "Show Incomplete Geolocations")
    }
  }
  )
  
  # observe loadData()
  # ------------------
  # prepare map related data: map_data, map_pts, and map_coords
  # add leaflet map with addGlPoints (high performance)
  # base layer has layerId of "base"
  observeEvent(loadData(), {
    map_data <- loadMapData()
    rv$map_data <- map_data$data
    rv$map_pts <- map_data$pts
    rv$map_coords <- SpatialPointsDataFrame(
      rv$map_data[, c(longitude, latitude)], 
      rv$map_data
    )
    output$geo_map <- renderLeaflet(
      leaflet() %>%
        addTiles(options = tileOptions(minZoom=2, maxZoom=15)) %>%
        setView(lng = 134, lat = -24, zoom = 2) %>%
        addGlPoints(
          data = rv$map_pts,
          group = "pts",
          radius = 5,
          fillColor = "black",
          fillOpacity = 1,
          weight = 2,
          stroke = T,
          layerId = "base"
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
    found_in_bounds <- findLocations(
      shape = input$geo_map_draw_new_feature,
      location_coordinates = rv$map_coords,
      location_id_colname = "X"
    )
    
    # only plot when there are no overlapping with exsiting points
    if (sum(found_in_bounds %in% rv$map_selected) > 0) {
      showModal(modalDialog(
        title = "Map Selection Error",
        "Overlapping detected! Please remove the last shape.",
        easyClose = TRUE
      ))
      # keep track of those shapes that are overlaps
      # this is used for deletion later
      rv$map_overlaps <- append(
        rv$map_overlaps,
        input$geo_map_draw_new_feature$properties$`_leaflet_id`,
        0
      )
    }
    else { # the newly drawn feature is not an overlap with existing features
      # get selected
      rv$map_selected <- unique(append(rv$map_selected, found_in_bounds, 0))
      # update layer coount
      rv$map_layer_count <- rv$map_layer_count + 1
      # attach an layer id
      rv$map_layer_ids[as.character(found_in_bounds)] <- rv$map_layer_count
      # display selected data points in the data table
      if (length(rv$map_selected) > 0) {
        # filter dataX
        update_dt_data(
          loadData() %>% 
            subset(X %in% rv$map_selected)
        )
      }
      # look up data points by ids found
      selected <- subset(rv$map_data, X %in% found_in_bounds)
      selected_pts <- st_as_sf(selected, coords = c(longitude, latitude))
      
      leafletProxy("geo_map") %>% 
        addGlPoints(
          data = selected_pts,
          radius = 5,
          fillOpacity = 1,
          fillColor = "red",
          weight = 3,
          stroke = T,
          layerId = as.character(rv$map_layer_count),
        )
    }
  })
  
  # delete shape(s)
  observeEvent(input$geo_map_draw_deleted_features, {
    # loop through list of one or more deleted features/polygons
    for (feature in input$geo_map_draw_deleted_features$features) {
      # get ids for locations within the bounding shape
      if (sum(feature$properties$`_leaflet_id` %in% rv$map_overlaps) > 0) {
        rv$map_overlaps <- rv$map_overlaps[
          !rv$map_overlaps %in% feature$properties$`_leaflet_id`
        ]
      }
      else {
        found_in_bounds <- findLocations(
          shape = feature,
          location_coordinates = rv$map_coords,
          location_id_colname = "X"
        )
        
        # remove second layer representing selected locations
        proxy <- leafletProxy("geo_map")
        
        if (length(found_in_bounds) > 0) {
          key <- as.character(found_in_bounds[1])
          proxy %>% removeGlPoints(
            layerId = as.character(rv$map_layer_ids[[key]])
          )
          rv$map_layer_ids[as.character(found_in_bounds)] <- NULL
        }
        
        ids_to_remove <- subset(rv$map_data, X %in% found_in_bounds)$X
        
        rv$map_selected <- rv$map_selected[
          !rv$map_selected %in% ids_to_remove
          ]
        
        # populate data table with original dataset 
        # in case there are no data points selected
        if (length(rv$map_selected) == 0) {
          if (rv$filter_flag) {
            update_dt_data(
              loadData() %>%
                subset(X %in% findZoom(input$geo_map_bounds, rv$map_coords, "X")) %>%
                dplyr::filter((!!sym(input$filter_col)) == input$filter_val)
            )
          }
          else {
            update_dt_data(
              loadData() %>%
                subset(X %in% findZoom(input$geo_map_bounds, rv$map_coords, "X"))
            )
          }
        }
        # when there are still points selected, update the DT accordingly
        else {
          update_dt_data(
            loadData() %>%
              subset(X %in% rv$map_selected)
          )
        }
      }
    }
  })
  
  observeEvent(input$geo_map_bounds, {
    # only associate map zooming and moving with dt when there are no
    # data points selected
    req(loadData())
    if (length(rv$map_selected) ==  0) {
      if (rv$filter_flag) {
        update_dt_data(
          loadData() %>%
            subset(X %in% findZoom(input$geo_map_bounds, rv$map_coords, "X")) %>%
            dplyr::filter((!!sym(input$filter_col)) == input$filter_val)
        )
      }
      else {
        update_dt_data (
          loadData() %>% 
            subset(X %in% findZoom(input$geo_map_bounds, rv$map_coords, "X"))
        )
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
          lapply(
            polygon_coordinates[[1]], 
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
  
  findZoom <- function(bounds, location_coordinates, location_id_colname) {
    # derive polygon coordinates and feature_type from shape input
    polygon_coordinates <- list(
      list(
        c(bounds$west, bounds$south),
        c(bounds$west, bounds$north),
        c(bounds$east, bounds$north),
        c(bounds$east, bounds$south),
        c(bounds$west, bounds$south)
      )
    )
    
    # transform into a spatial polygon
    drawn_polygon <- Polygon(
      do.call(
        rbind, 
        lapply(
          polygon_coordinates[[1]], 
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
  
  # ------------------------------------------------------------------------- #
  # plots
  # ------------------------------------------------------------------------- #
  
  # trigger shiny to update choices for the selectizeInput, "plot_variable_type"
  observeEvent(input$plot_var_type, {
    req(input$plot_var_type)
    choices = NULL
    if (input$plot_var_type == "1") {
      choices <- names(plots$var_1)
    }
    else if (input$plot_var_type == "2") {
      choices <- names(plots$var_2)
    }
    updateSelectizeInput(session, "plot_selection", choices = choices)
  })
  
  # initialise an input tag list
  inputTagList <- tagList()
  # create column inputs
  
  observeEvent(createPlotInputs(), {
    output$allInputs <- renderUI({createPlotInputs()})
  })
  
  output$plot_output <- renderPlot({
    generatePlot()
  })
  
  # ------------------------------------------------------------------------- #
  # data table update only on changes to rv$dt_data
  # ------------------------------------------------------------------------- #
  
  observeEvent(rv$dt_data, {
    output$datatable <- DT::renderDataTable(
      DT::datatable(
        rv$dt_data,
        options = list(
          pageLength = 10, 
          lengthMenu = list(c(5,10,15,20), c("5", "10", "15", "20")),
          scrollX = TRUE,
          ordering = FALSE,
          autoWidth = TRUE,
          dom = 'ltipr'
        )
      )
    )
  })
  
  # ------------------------------------------------------------------------- #
  # helper functions
  # ------------------------------------------------------------------------- #
  apply_filter_helper <- function() {
    # -- highlight map -- #
    d <- NULL
    if (input$filter_target == "dataset") {
      d <- loadData() %>%
        dplyr::filter((!!sym(input$filter_col)) == input$filter_val)
    }
    else {
      d <- rv$dt_data_x %>%
        dplyr::filter((!!sym(input$filter_col)) == input$filter_val)
    }
    selected <- subset(
      rv$map_data,
      X %in% d$X
    )
    selected_pts <- st_as_sf(selected, coords = c(longitude, latitude))
    leafletProxy("geo_map") %>%
      addGlPoints(
        data = selected_pts,
        radius = 5,
        fillOpacity = 1,
        fillColor = "yellow",
        weight = 3,
        stroke = T,
        layerId = "filtered",
      )
    
    # -- update DT -- #
    update_dt_data(d)
  }
  
  remove_filter_helper <- function() {
    # -- remove highlights -- #
    leafletProxy("geo_map") %>%
      removeGlPoints("filtered")
    # -- restore DT -- #
    if (length(rv$map_selected) > 0) {
      update_dt_data(
        loadData() %>%
          subset(X %in% rv$map_selected) %>%
          subset(X %in% findZoom(input$geo_map_bounds, rv$map_coords, "X"))
      )
    }
    else {
      update_dt_data(
        loadData() %>%
          subset(X %in% findZoom(input$geo_map_bounds, rv$map_coords, "X"))
      )
    }
  }
  
  update_dt_data <- function(data) {
    rv$dt_data_x <- data
    rv$dt_data <- data %>%
      select(-input$drop_cols) %>%
      select(-"X")
  }
  
  
  # return numerical columns in the given "data" data table  
  numerical_columns <- function(data) {
    if (is.null(rv$plot_numerical_columns)) {
      rv$plot_numerical_columns <- names(select_if(data, function(col) return(
        Reduce('&', sapply(col, is.numeric))
      )))
    }
    return(rv$plot_numerical_columns)
  }
}
