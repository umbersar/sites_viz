# Author: Ryan Di
# Australian National University
# CSIRO (Land and Water)
# Exploring drawing shapes on leafleft maps
# essential packages: leaflet, leaflet.extra
# reference: https://redoakstrategic.com/geoshaper/

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sp)
library(dplyr)


longitutde <- "o_longitude_GDA94"
latitude <- "o_latitude_GDA94"

# data course
myData <- read.csv('../../data/o_elevation.csv')
myData <- select(myData, "X", longitutde, latitude)[2000:3000, ]

# generate second set of unique location IDs for second layer of selected locations
myData$secondLocationID <- paste(as.character(myData$X), "_selectedLayer", sep="")

coordinates <- SpatialPointsDataFrame(myData[, c(longitutde, latitude)], myData)

head(myData)

shinyApp(
  ui <- fluidPage(
    leafletOutput("mymap", height = "1050")
  ),
  
  server <- function(input, output) {
    ##################################################################################
    # section one
    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarkers = list())

    ##################################################################################
    # section two
    # base map, with area selection, powered by leaflet.extras
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircles(
          data = myData,
          radius = 10,
          lat = myData$o_latitude_GDA94,
          lng = myData$o_longitude_GDA94,
          fillColor = "white",
          fillOpacity = "1",
          color = "hotpink",
          weight = 2,
          stroke = T,
          layerId = as.character(myData$X),
          highlightOptions = highlightOptions(
            color = "mediumseagreen",
            opacity = 1.0,
            weight = 2,
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
      
      # look up data points by ids found
      selected <- subset(myData, X %in% data_of_click$clickedMarker)
      
      proxy <- leafletProxy("mymap")
      proxy %>% addCircles(
        data = selected,
        radius = 10,
        lat = selected$o_latitude_GDA94,
        lng = selected$o_longitude_GDA94,
        fillColor = "wheat",
        fillOpacity = 1,
        color = "mediumseagreen",
        weight = 3,
        stroke = T,
        layerId = as.character(selected$secondLocationID),
        highlightOptions = highlightOptions(
          color = "hotpink",
          opacity = 1.0,
          weight = 2,
          bringToFront = TRUE
        )
      )
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
          location_id_colname = "secondLocationID"
        )
        
        # remove second layer representing selected locations
        proxy <- leafletProxy("mymap")
        proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
        
        first_layer_ids <- subset(myData, secondLocationID %in% bounded_layer_ids)$X
        
        data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker 
                                                                   %in% first_layer_ids]
      }
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































