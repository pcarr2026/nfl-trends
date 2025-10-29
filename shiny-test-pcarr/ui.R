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
      menuItem("Game Predictor", tabName = "game_predictor", icon = icon("football-ball")),
      menuItem("Cluster Analysis", tabName = "cluster_analysis", icon = icon("project-diagram"))
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
      # ROI CALCULATOR TAB (WITH YEAR FILTER)
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
                           
                           sliderInput("roi_year_range", "Year Range:",
                                       min = 2000, max = 2025, value = c(2000, 2025),
                                       step = 1, sep = ""),
                           
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
      # GAME PREDICTOR TAB (NEW!)
      # ----------------------------------------
      tabItem(tabName = "game_predictor",
              h2("Game Predictor"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary",
                           h4("Predict Game Winner"),
                           
                           selectInput("pred_home_team", "Home Team:",
                                       choices = NULL),  # Populated dynamically
                           
                           fluidRow(
                             column(6, numericInput("pred_home_wins", "Wins:", value = 8, min = 0, max = 17, step = 1)),
                             column(6, numericInput("pred_home_losses", "Losses:", value = 5, min = 0, max = 17, step = 1))
                           ),
                           
                           fluidRow(
                             column(6, numericInput("pred_home_off_rank", "Off. Rank (1-32):", value = 10, min = 1, max = 32, step = 1)),
                             column(6, numericInput("pred_home_def_rank", "Def. Rank (1-32):", value = 15, min = 1, max = 32, step = 1))
                           ),
                           
                           hr(),
                           
                           selectInput("pred_away_team", "Away Team:",
                                       choices = NULL),  # Populated dynamically
                           
                           fluidRow(
                             column(6, numericInput("pred_away_wins", "Wins:", value = 7, min = 0, max = 17, step = 1)),
                             column(6, numericInput("pred_away_losses", "Losses:", value = 6, min = 0, max = 17, step = 1))
                           ),
                           
                           fluidRow(
                             column(6, numericInput("pred_away_off_rank", "Off. Rank (1-32):", value = 12, min = 1, max = 32, step = 1)),
                             column(6, numericInput("pred_away_def_rank", "Def. Rank (1-32):", value = 18, min = 1, max = 32, step = 1))
                           ),
                           
                           hr(),
                           
                           h5("Game Conditions:"),
                           
                           numericInput("pred_spread", "Spread (negative = home favored):",
                                        value = -6, min = -25, max = 25, step = 0.5),
                           
                           selectInput("pred_stadium", "Stadium Type:",
                                       choices = c("Outdoor", "Indoor"),
                                       selected = "Outdoor"),
                           
                           sliderInput("pred_week", "Week of Season:",
                                       min = 1, max = 18, value = 9, step = 1),
                           
                           actionButton("predict_btn", "Predict Winner", 
                                        class = "btn-primary btn-lg btn-block"),
                           
                           hr(),
                           
                           h4("Model Info:"),
                           p(strong("Algorithm:"), "Random Forest (500 trees)"),
                           p("Key factors: Team records, offensive/defensive rankings, spread, home field, stadium type, week")
                       )
                ),
                column(width = 8,
                       box(width = NULL,
                           h3("Prediction Results"),
                           
                           uiOutput("prediction_result"),
                           
                           br(),
                           
                           # Betting Value Section
                           div(style = "background-color: #fff3cd; border: 2px solid #ffc107; padding: 20px; border-radius: 10px;",
                               h4(icon("dollar-sign"), " Betting Value Analysis"),
                               uiOutput("value_bet_analysis")
                           ),
                           
                           br(),
                           
                           h4("Model Performance"),
                           fluidRow(
                             column(3,
                                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center;",
                                        h5("Model Accuracy"),
                                        h3(textOutput("model_accuracy", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center;",
                                        h5("Total Games"),
                                        h3(textOutput("model_total_games", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center;",
                                        h5("Home Win Rate"),
                                        h3(textOutput("model_home_rate", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center;",
                                        h5("Avg Improvement"),
                                        h3("+5-8%", style = "color: #28a745;")
                                    )
                             )
                           ),
                           
                           br(),
                           
                           h4("Most Important Factors"),
                           plotlyOutput("importance_plot", height = "300px"),
                           
                           br(),
                           
                           h4("Head-to-Head History"),
                           tableOutput("h2h_table")
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # CLUSTER ANALYSIS TAB
      # ----------------------------------------
      tabItem(tabName = "cluster_analysis",
              h2("NFL Team Cluster Analysis"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary",
                           h4("Select 2 Factors for Clustering"),
                           
                           selectInput("cluster_factor1", "Factor 1 (X-Axis):",
                                       choices = list(
                                         "Win Percentage" = "win_pct",
                                         "Average Points Scored" = "avg_points_scored",
                                         "Average Points Allowed" = "avg_points_allowed",
                                         "Point Differential" = "point_diff",
                                         "Over Hit Rate" = "over_rate",
                                         "Under Hit Rate" = "under_rate",
                                         "Favorite Cover Rate (as favorite)" = "fav_cover_rate",
                                         "Underdog Cover Rate (as underdog)" = "dog_cover_rate",
                                         "Home Win Rate" = "home_win_rate",
                                         "Away Win Rate" = "away_win_rate"
                                       ),
                                       selected = "win_pct"),
                           
                           selectInput("cluster_factor2", "Factor 2 (Y-Axis):",
                                       choices = list(
                                         "Win Percentage" = "win_pct",
                                         "Average Points Scored" = "avg_points_scored",
                                         "Average Points Allowed" = "avg_points_allowed",
                                         "Point Differential" = "point_diff",
                                         "Over Hit Rate" = "over_rate",
                                         "Under Hit Rate" = "under_rate",
                                         "Favorite Cover Rate (as favorite)" = "fav_cover_rate",
                                         "Underdog Cover Rate (as underdog)" = "dog_cover_rate",
                                         "Home Win Rate" = "home_win_rate",
                                         "Away Win Rate" = "away_win_rate"
                                       ),
                                       selected = "avg_points_scored"),
                           
                           hr(),
                           
                           sliderInput("n_clusters", "Number of Clusters:",
                                       min = 1, max = 4, value = 4, step = 1),
                           
                           hr(),
                           
                           h4("About Cluster Analysis:"),
                           p("K-means clustering groups teams with similar characteristics together based on the 2 factors you select."),
                           p("Teams in the same cluster share similar patterns across both metrics."),
                           p(strong("Cluster Ordering:"), "Cluster 1 = highest and rightmost teams, with higher numbers moving left and down."),
                           p("Use this to identify team archetypes (e.g., high-scoring teams, defensive teams, etc.)")
                       )
                ),
                column(width = 8,
                       box(width = NULL,
                           h3("Team Clusters"),
                           plotlyOutput("cluster_plot", height = "500px"),
                           br(),
                           h4("Cluster Characteristics"),
                           tableOutput("cluster_summary_table"),
                           br(),
                           h4("Teams by Cluster"),
                           tableOutput("cluster_teams_table")
                       )
                )
              )
      )
      
    )
  )
)