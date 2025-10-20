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
      # HOME TAB - WITH NFL LOGO
      # ----------------------------------------
      tabItem(tabName = "home",
              fluidRow(
                column(width = 12,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = "Welcome to NFL Stadium Analytics",
                           tags$div(
                             style = "text-align: center; padding: 20px;",
                             
                             # NFL LOGO - UPDATED TO .jpg
                             tags$img(src = "nfl_logo.jpg", width = "400px", style = "margin-bottom: 30px;"),
                             
                             tags$h1("NFL Over/Under Analysis", 
                                     style = "color: #013369; font-weight: bold; margin-bottom: 10px;"),
                             tags$h3("Stadium Performance Dashboard", 
                                     style = "color: #D50A0A; margin-bottom: 30px;"),
                             
                             tags$p(
                               style = "font-size: 18px; line-height: 1.6; text-align: center; max-width: 800px; margin: 0 auto 20px auto;",
                               "This interactive dashboard analyzes over/under betting performance across NFL stadiums from 2000-2025.",
                               "Discover which venues consistently favor overs or unders, and explore geographical patterns in scoring trends."
                             ),
                             
                             tags$br(),
                             tags$h4("Features:", style = "color: #013369;"),
                             tags$ul(
                               style = "text-align: left; max-width: 600px; margin: 0 auto 30px auto; font-size: 15px;",
                               tags$li("Interactive map showing over/under performance by stadium"),
                               tags$li("Toggle between overs and unders to compare trends"),
                               tags$li("Zoom to specific stadiums for detailed analysis"),
                               tags$li("Color-coded visualizations for easy pattern recognition")
                             ),
                             
                             # BUTTON TO NAVIGATE TO MAP
                             actionButton("go_to_map", 
                                          "Explore Stadium Map", 
                                          icon = icon("map-marked-alt"),
                                          class = "btn-lg",
                                          style = "background-color: #013369; 
                                                 color: white; 
                                                 border: none; 
                                                 font-size: 18px; 
                                                 padding: 15px 40px;
                                                 margin-top: 20px;
                                                 font-weight: bold;")
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