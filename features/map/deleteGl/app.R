library(mapview)
library(leaflet)
library(leafgl)
library(sf)
library(shiny)

lines = suppressWarnings(st_cast(trails, "LINESTRING")); lines = st_transform(lines, 4326)
polys <- st_transform(st_buffer(st_transform(st_as_sf(breweries91[1,]), 3035), 50000), 4326)

ui <- fluidPage(
  leafglOutput("map")
  ,hr()
  ,actionButton("remove_red_pts", "Remove Glify Red Points")
  ,hr()
  ,actionButton("remove_black_pts", "Remove Glify Black Polygons")
)

m = leaflet() %>%
  addTiles() %>%
  addGlPoints(
    data = breweries91, 
    layerId = "1", 
    fillColor = "red",
    radius = 10
  ) %>%
  addGlPoints(
    data = breweries91, 
    layerId = "2", 
    fillColor = "black",
    radius = 5
  ) %>%
  setView(lng = 10.5, lat = 49.5, zoom = 5)

server <- function(input, output, session) {
  output$map <- renderLeaflet({m})
  
  ## Remove Red Points by layerId
  observeEvent(input$remove_red_pts, {
    leafletProxy("map") %>% 
      removeGlPoints(layerId = "1")
  })  

  ## Remove Black Points by layerId
  observeEvent(input$remove_black_pts, {
    leafletProxy("map") %>% 
      removeGlPoints(layerId = "2")
  })  
}

shinyApp(ui, server)