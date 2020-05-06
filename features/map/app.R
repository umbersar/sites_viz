# Author: Ryan Di
# Australian National University
# CSIRO (Land and Water)
# Exploring drawing shapes on leafleft maps
# essential packages: leaflet, leaflet.extra
# reference: https://redoakstrategic.com/geoshaper/

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(leafgl)
library(sf)
library(sp)


longitutde <- "o_longitude_GDA94"
latitude <- "o_latitude_GDA94"

# data course
myData <- read.csv('../../data/o_elevation.csv')
myData <- na.omit(select(myData, "X", longitutde, latitude))

# generate second set of unique location IDs for second layer of selected locations
myData$layerID <- paste(as.character(myData$X), "_sl", sep="")
pts <- st_as_sf(myData, coords = c(longitutde, latitude))

coordinates <- SpatialPointsDataFrame(myData[, c(longitutde, latitude)], myData)

shinyApp(
  ui <- fluidPage(
    verbatimTextOutput("simple"),
    leafletOutput("mymap", height = "800"),
    verbatimTextOutput("debug")
  ),
  
  server <- function(input, output) {
    ##################################################################################
    # section one
    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarker = list())
    output_text <- reactiveValues(text = "")

    ##################################################################################
    # section two
    # base map, with area selection, powered by leaflet.extras
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addGlPoints(
          data = pts,
          group = "pts",
          radius = 5,
          fillColor = "black",
          fillOpacity = 1,
          weight = 2,
          stroke = T,
          layerId = "base",
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
    })
    
    ##################################################################################  
    # section three
    # events
    observeEvent(input$mymap_draw_new_feature, {
      # only add new layers for bounded locations
      found_in_bounds <- findLocations(
        shape = input$mymap_draw_new_feature,
        location_coordinates = coordinates,
        location_id_colname = "X"
      )
      
      for (id in found_in_bounds) {
        if (id %in% data_of_click$clickedMarker) {
          # do not add id
        } 
        else {
          # add id
          data_of_click$clickedMarker <- append(data_of_click$clickedMarker, id, 0)  
        }
      }
      
      output$debug <-renderText(
        as.character(data_of_click$clickedMarker)
      )
      
      # look up data points by ids found
      selected <- subset(myData, X %in% data_of_click$clickedMarker)
      selected_pts <- st_as_sf(selected, coords = c(longitutde, latitude))
      proxy <- leafletProxy("mymap")
      proxy %>% 
      addCircles(
        data = selected_pts,
        radius = 5,
        fillOpacity = 1,
        color = "red",
        weight = 3,
        stroke = T,
        layerId = as.character(selected$layerID),
        highlightOptions = highlightOptions(
          bringToFront = TRUE
        )
      )
      # addGlPoints(
      #   data = selected_pts,
      #   group = "pts",
      #   radius = 5,
      #   fillColor = "red",
      #   fillOpacity = "1",
      #   weight = 3,
      #   stroke = T,
      #   layerId = "drawn",
      #   highlightOptions = highlightOptions(
      #     bringToFront = TRUE
      #   )
      # )
    })
    
    ##################################################################################
    # section four
    observeEvent(input$mymap_draw_deleted_features, {
      # loop through list of one or more deleted features/polygons
      for (feature in input$mymap_draw_deleted_features$features) {
        # get ids for locations within the bounding shape
        bounded_layer_ids <- findLocations(
          shape = feature,
          location_coordinates = coordinates,
          location_id_colname = "layerID"
        )
        
        output$debug <-renderText(
          bounded_layer_ids
        )
        # remove second layer representing selected locations
        proxy <- leafletProxy("mymap")
        proxy %>% removeShape(layerId = bounded_layer_ids)
        
        first_layer_ids <- subset(myData, layerID %in% bounded_layer_ids)$X

        data_of_click$clickedMarker <- data_of_click$clickedMarker[
          !data_of_click$clickedMarker %in% first_layer_ids
        ]
      }
    })
    
    observeEvent(input$mymap_zoom, {
      output$simple <- renderText(
        paste0("(", paste(
          input$mymap_bounds$north, 
          input$mymap_bounds$east, 
          input$mymap_bounds$south, 
          input$mymap_bounds$west,
          sep = ", "), ")"
        )
      )
    })
    ##################################################################################
    # section five
    # findLocations function
    findLocations <- function(shape, location_coordinates, location_id_colname) {
      # derive polygon coordinates and feature_type from shape input
      polygon_coordinates <- shape$geometry$coordinates
      feature_type <- shape$properties$feature_type
      
      if (feature_type %in% c("rectangle", "polygon")) {
        # transform into a spatial polygon
        drawn_polygon <- Polygon(
          do.call(rbind, lapply(polygon_coordinates[[1]], function(x){c(x[[1]][1], x[[2]][1])}))
        )
        
        # use 'over' from the sp package to identify selected locations
        selected_locs <- sp::over(
          location_coordinates,
          sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon), "drawn_polygon")))
        )
        
        # get ids
        x = (location_coordinates[which(!is.na(selected_locs)), location_id_colname])
        selected_loc_id = as.character(x[[location_id_colname]])
        
        return (selected_loc_id)
      } 
      else if (feature_type == "circle") {
        center_coords <- matrix(c(polygon_coordinates[[1]], polygon_coordinates[[2]]), ncol = 2)
        # get distances to center of drawn circle for all locations in location_coordinates
        # distance is in kilometers
        dist_to_center <- spDistsN1(location_coordinates, center_coords, longlat = TRUE)
        
        # get location ids
        # radius in meters
        x <- location_coordinates[dist_to_center < shape$properties$radius/1000, location_id_colname]
        
        selected_loc_id = as.character(x[[location_id_colname]])
        return(selected_loc_id)
      }
    }
  },
  
  options = list(height = 1000)
)

