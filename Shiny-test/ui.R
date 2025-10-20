library(shiny)
library(tidyverse)
library(plotly)

fluidPage(
  titlePanel("NFL Betting Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("bet_type", "What to Analyze:",
                  choices = list("Favorite Covered" = "fav_correct",
                                 "Over Hit" = "over_hit",
                                 "Under Hit" = "under_hit")),
      
      selectInput("factor", "Factor:",
                  choices = list("Season Type" = "season_type",
                                 "Stadium Type" = "stadium_type",
                                 "Temperature" = "temp_category",
                                 "Home Team" = "team_home")),
      
      hr(),
      
      h4("Graph Explanation:"),
      uiOutput("explanation_text")
    ),
    
    mainPanel(
      plotlyOutput("plot", height = "500px"),
      br(),
      tableOutput("table")
    )
  )
)