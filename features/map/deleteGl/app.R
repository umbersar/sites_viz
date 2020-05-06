library(mapview)
library(leaflet)
library(leafgl)
library(sf)
library(shiny)

lines = suppressWarnings(st_cast(trails, "LINESTRING")); lines = st_transform(lines, 4326)
polys <- st_transform(st_buffer(st_transform(st_as_sf(breweries91[1,]), 3035), 50000), 4326)

ui <- fluidPage(
  leafglOutput("map")
  ,actionButton("remove_lns", "Remove Glify Lines")
  ,hr()
  ,actionButton("remove_pts", "Remove Glify Points")
  ,hr()
  ,actionButton("remove_pol", "Remove Glify Polygons")
  ,hr()
  ,actionButton("clear", "Clear all Glify Layers")
)

m = leaflet() %>%
  addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
  addGlPoints(data = breweries91, layerId = "points", group = "pointsgroup", col=cbind(10, 0.2, 10)) %>%
  addGlPolylines(data = lines, popup = "FGN", layerId = "lines", group = "linesgroup") %>%
  addGlPolygons(data = polys, layerId = "polys", group = "polygroup", col=cbind(30, 0.2, 10)) %>% 
  setView(lng = 10.5, lat = 49.5, zoom = 5) %>% 
  addLayersControl(overlayGroups = c("pointsgroup","linesgroup","polygroup"))

server <- function(input, output, session) {
  output$map <- renderLeaflet({m})
  
  ## Remove Lines by layerId
  observeEvent(input$remove_lns, {
    leafletProxy("map") %>% 
      removeGlPolylines(layerId = "lines")
  })
  
  ## Remove Points by layerId
  observeEvent(input$remove_pts, {
    leafletProxy("map") %>% 
      removeGlPoints(layerId = "points")
  })  
  
  ## Remove Polygons by layerId
  observeEvent(input$remove_pol, {
    leafletProxy("map") %>% 
      removeGlPolygons(layerId = "polys")
  })
  
  ## Clear all Glify Layers (category = glify)
  observeEvent(input$clear, {
    leafletProxy("map") %>% 
      clearGlLayers()
  })
  
}

shinyApp(ui, server)