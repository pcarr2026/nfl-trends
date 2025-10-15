library(shiny)
library(leaflet)

fluidPage(
  # map output
  leafletOutput("worldMap"),
  
  # line break (puts some space between map and button)
  br(),
  
  # a button
  actionButton("newButton", "New place!")
)