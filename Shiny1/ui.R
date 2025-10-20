# Load ALL libraries at the very top
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "NFL Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",  # IMPORTANT: Keep this ID for tracking active tab
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Stadium Map", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("gear"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # ----------------------------------------
      # HOME TAB - FILLED IN WITH CONTENT
      # ----------------------------------------
      tabItem(tabName = "home",
              fluidRow(
                column(width = 12,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = "Welcome to NFL Stadium Analytics",
                           tags$div(
                             style = "text-align: center; padding: 20px;",
                             tags$img(src = "your_image.png", width = "400px", style = "margin-bottom: 20px;"),
                             # To use an image, place it in a folder called "www" in your app directory
                             # Or use a URL: tags$img(src = "https://example.com/image.png", ...)
                             
                             tags$h3("Explore NFL Over/Under Trends Across Stadiums"),
                             tags$p(
                               style = "font-size: 16px; line-height: 1.6; text-align: left; max-width: 800px; margin: 0 auto;",
                               "This interactive dashboard analyzes over/under betting performance across NFL stadiums from 2000-2025.",
                               "Discover which venues consistently favor overs or unders, and explore geographical patterns in scoring trends."
                             ),
                             tags$br(),
                             tags$h4("Features:"),
                             tags$ul(
                               style = "text-align: left; max-width: 600px; margin: 0 auto; font-size: 15px;",
                               tags$li("Interactive map showing over/under performance by stadium"),
                               tags$li("Toggle between overs and unders to compare trends"),
                               tags$li("Zoom to specific stadiums for detailed analysis"),
                               tags$li("Color-coded visualizations for easy pattern recognition")
                             ),
                             tags$br(),
                             tags$p(
                               style = "font-size: 14px; color: #666;",
                               "Click 'Stadium Map' in the sidebar to begin exploring the data."
                             )
                           )
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # STADIUM MAP TAB
      # ----------------------------------------
      tabItem(tabName = "analytics",
              h2("NFL Stadium Over/Under Performance (2000-2025)"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary",
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
                       )
                ),
                column(width = 8,
                       box(width = NULL,
                           leafletOutput("map", height = "600px")
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # SETTINGS TAB
      # ----------------------------------------
      tabItem(tabName = "settings",
              h2("Settings"),
              box(width = 12,
                  "Configuration options here"
              )
      )
    )
  )
)