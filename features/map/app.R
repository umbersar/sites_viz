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

shinyApp(
  ui <- fluidPage(
    splitLayout(
      cellWidths = c("40%", "60%"),
      leafletOutput("mymap", height = "500"),
      DT::dataTableOutput('mydt')
    ),
    checkboxInput("checked", "Show Data Points in Bounds in DT?"),
    verbatimTextOutput("bounds_explain"),
    verbatimTextOutput("bounds")
  ),
  
  server <- function(input, output) {
    longitutde <- "o_longitude_GDA94"
    latitude <- "o_latitude_GDA94"
    
    # data course
    myData <- read.csv('../../data/o_elevation.csv')
    myData <- myData %>%
      select_if(function(col) return(!col_has_val(col))) %>%
      select(-"morphology_attribute")
    mapData <- na.omit(select(myData, "X", longitutde, latitude))
    pts <- st_as_sf(mapData, coords = c(longitutde, latitude))
    
    coordinates <- SpatialPointsDataFrame(mapData[, c(longitutde, latitude)], mapData)
    
    ##################################################################################
    zoom_data <- reactiveValues(
      in_frame = list(),
      bounds = list(
        north = 90,
        east = 180,
        south = -90,
        west = 0
      ),
      first_time = TRUE
    )
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addGlPoints(
          data = pts,
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
        setView(lng = 133, lat = -24, zoom = 3) %>%
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
    })
    
    output$mydt <- DT::renderDataTable(
      myData %>% select(-"X"),
      options = list(
        pageLength = 10, 
        scrollX = TRUE,
        ordering = FALSE,
        autoWidth = TRUE,
        info = FALSE
      )
    )
    
    observeEvent(input$mymap_bounds, {
      output$bounds_explain <- renderText(
        "(north, east, south, west)"
      )
      output$bounds <- renderText(
        paste0("(", paste(
          input$mymap_bounds$north, 
          input$mymap_bounds$east, 
          input$mymap_bounds$south, 
          input$mymap_bounds$west,
          sep = ", "), ")"
        )
      )
      if (input$checked) {
        update_dt_bounds(
          input$mymap_bounds
        )
      }
    })

    observeEvent(input$checked, {
      if (input$checked) {
        showNotification("Updating the table, please wait...")
        update_dt_bounds(
          input$mymap_bounds
        )
      }
    })
    
    update_dt_bounds <- function (bounds) {
      # on start up loop through all
      if (zoom_data$first_time) {
        find_in_frame(coordinates, bounds)
        zoom_data$first_time <- FALSE
      }
      else {
        cat("subset start")
        cat("\n")
        in_frame <- subset(coordinates, X %in% zoom_data$in_frame)
        cat("subset end")
        cat("\n")
        # zooming in
        cat("find in frame start")
        cat("\n")
        if (
          zoom_data$bounds$west <= bounds$west &
          bounds$east <= zoom_data$bounds$east &
          zoom_data$bounds$south <= bounds$south &
          bounds$north <= zoom_data$bounds$north
        ) {
          cat("zooming in ...")
          find_in_frame(in_frame, bounds)
        }
        else {
          find_in_frame(coordinates, bounds)
        }
        cat("find in frame end")
        cat("\n")
      }
      # update data table
      cat("myData length: ")
      cat(length(myData))
      cat("\n")
      output$mydt <- DT::renderDataTable(
        subset(myData, X %in% zoom_data$in_frame) %>%
          select(-"X"),
        options = list(
          pageLength = 10, 
          scrollX = TRUE,
          ordering = FALSE,
          autoWidth = TRUE,
          info = FALSE
        )
      )
      cat("length of points in frame: ")
      cat(length(zoom_data$in_frame))
      cat("\n")
      
      # update zoom_data$bounds
      zoom_data$bounds <- input$mymap_bounds
    }
    
    find_in_frame <- function (data, bounds) {
      zoom_data$in_frame <- list()
      count <- 0
      len <- length(data)
      cat("length of data is: ")
      cat(len)
      cat("\n")
      while(count < len) {
        count <- count + 1
        if (
          bounds$west <= data[count,]$o_longitude_GDA94 &
          data[count,]$o_longitude_GDA94 <= bounds$east &
          bounds$south <= data[count,]$o_latitude_GDA94 &
          data[count,]$o_latitude_GDA94 <= bounds$north
        )
        {
          zoom_data$in_frame <- append(
            zoom_data$in_frame, data[count, ]$X, 0
          )
        }
      }
    }
  },
  
  options = list(height = 500)
)

