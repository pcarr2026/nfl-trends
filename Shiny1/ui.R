# Load ALL libraries at the very top
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyverse)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "NFL Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",  # IMPORTANT: Keep this ID for tracking active tab
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Stadium Map", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Betting Analysis", tabName = "betting_analysis", icon = icon("dollar")),
      menuItem("ROI Calculator", tabName = "roi_calculator", icon = icon("calculator")),
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
      # BETTING ANALYSIS TAB
      # ----------------------------------------
      tabItem(tabName = "betting_analysis",
              h2("NFL Betting Analysis (2000-2025)"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary",
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
                       )
                ),
                column(width = 8,
                       box(width = NULL,
                           plotlyOutput("plot", height = "500px"),
                           br(),
                           tableOutput("table")
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # ROI CALCULATOR TAB (NEW)
      # ----------------------------------------
      tabItem(tabName = "roi_calculator",
              h2("ROI Calculator"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary",
                           h4("Test a Betting Strategy"),
                           
                           selectInput("roi_strategy", "Select Strategy:",
                                       choices = list(
                                         "Always Bet the Favorite" = "favorite",
                                         "Always Bet the Underdog" = "underdog",
                                         "Always Take the Over" = "over",
                                         "Always Take the Under" = "under"
                                       )),
                           
                           sliderInput("roi_bet_amount", "Bet Amount per Game:",
                                       min = 10, max = 500, value = 100, step = 10,
                                       pre = "$"),
                           
                           selectInput("roi_team_filter", "Filter by Team:",
                                       choices = c("All Teams" = "all")),
                           
                           hr(),
                           
                           h4("How It Works:"),
                           p("This calculator shows what would have happened if you used this strategy on every game in the dataset."),
                           p(strong("Betting Odds:")),
                           p("• Over, Under: -110 (standard)"),
                           p("• Favorite (moneyline):"),
                           p("  - Spread ≤3: -110 odds"),
                           p("  - Spread 3.5-7: -150 odds"),
                           p("  - Spread 7.5-10: -200 odds"),
                           p("  - Spread >10: -300 odds"),
                           p("• Underdog (moneyline):"),
                           p("  - Spread ≤3: +120 odds"),
                           p("  - Spread 3.5-7: +180 odds"),
                           p("  - Spread 7.5-10: +250 odds"),
                           p("  - Spread >10: +350 odds")
                       )
                ),
                column(width = 8,
                       box(width = NULL,
                           h3("Strategy Results"),
                           
                           fluidRow(
                             column(3, 
                                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center;",
                                        h4("Games Bet"),
                                        h2(textOutput("roi_total_games", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center;",
                                        h4("Win Rate"),
                                        h2(textOutput("roi_win_rate", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center;",
                                        h4("Total Profit"),
                                        h2(textOutput("roi_profit", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center;",
                                        h4("ROI"),
                                        h2(textOutput("roi_percentage", inline = TRUE))
                                    )
                             )
                           ),
                           
                           br(),
                           
                           plotlyOutput("roi_plot", height = "400px"),
                           
                           br(),
                           
                           h4("Team Performance for This Strategy"),
                           tableOutput("roi_team_table")
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




# More ideas: 
# Add images for the stadiums on the map.
# Consider ggplot instead of plotly
# Change graph titles for betting analysis
# Represent the betting odds in a diferent way (visual)
# Add year filter for ROI calc
# Upset predictor (Easier)
# Predictive model based on historical data (Complex)
# Cluster analysis: k means (input a number "k" and k means will put 5 dots on the space which will minimize the distance between stadiums)
# ^ Cluster of stadiums that are better for certain teams than other teams
# 