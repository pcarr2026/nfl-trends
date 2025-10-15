library(shiny)
library(leaflet)

stadiums <- data.frame(
  name = c("Lambeau Field", "AT&T Stadium", "MetLife Stadium", "Arrowhead Stadium", "SoFi Stadium"),
  lat = c(44.5013, 32.7473, 40.8136, 39.0490, 33.9535),
  lng = c(-88.0622, -97.0945, -74.0745, -94.4839, -118.3391)
)

shinyServer(function(input, output, session) {
  
  output$worldMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95, lat = 39, zoom = 4) %>%
      addCircles(
        data = stadiums,
        lng = ~lng,
        lat = ~lat,
        label = ~name,
        radius = 50000,
        color = "blue",
        fillOpacity = 0.6
      )
  })
  
  observeEvent(input$stadiumSelect, {
    selected <- stadiums[stadiums$name == input$stadiumSelect, ]
    leafletProxy("worldMap") %>%
      setView(lng = selected$lng, lat = selected$lat, zoom = 10) %>%
      addPopups(lng = selected$lng, lat = selected$lat, popup = selected$name)
  })
})

