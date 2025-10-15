library(shiny)
library(leaflet)

# Sample stadium data
stadiums <- data.frame(
  name = c("Lambeau Field", "AT&T Stadium", "MetLife Stadium", "Arrowhead Stadium", "SoFi Stadium"),
  lat = c(44.5013, 32.7473, 40.8136, 39.0490, 33.9535),
  lng = c(-88.0622, -97.0945, -74.0745, -94.4839, -118.3391)
)

shinyUI(
  fluidPage(
    leafletOutput("worldMap", height = 600),
    br(),
    selectInput("stadiumSelect", "Choose a stadium:", choices = stadiums$name)
  )
)