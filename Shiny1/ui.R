library(shiny)
library(leaflet)
library(dplyr)

fluidPage(
  titlePanel("NFL Stadium Over/Under Performance (2000-2025)"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("metric", "Select Metric:",
                   choices = c("Overs" = "overs", "Unders" = "unders"),
                   selected = "overs"),
      hr(),
      selectInput("stadium_select", "Zoom to Stadium:",
                      choices = c("All Stadiums" = "all"),
                      selected = "all"),
      hr(),
      helpText("Toggle between Overs and Unders to see the count at each NFL stadium.",
               "Larger circles indicate more games hitting that outcome.",
               "Click on markers for detailed information.")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)
