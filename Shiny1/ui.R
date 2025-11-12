# Load ALL libraries at the very top
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyverse)
library(plotly)
library(htmltools)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = span(
      icon("football-ball", style = "margin-right: 10px;"),
      "NFL Analytics Pro"
    ),
    titleWidth = 280
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Stadium Map", tabName = "analytics", icon = icon("map-marker-alt")),
      menuItem("Betting Analysis", tabName = "betting_analysis", icon = icon("chart-bar")),
      menuItem("ROI Calculator", tabName = "roi_calculator", icon = icon("calculator")),
      menuItem("Game Predictor", tabName = "game_predictor", icon = icon("brain")),
      menuItem("Cluster Analysis", tabName = "cluster_analysis", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    # Custom CSS for modern styling
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
        
        /* Global Styles */
        body, .content-wrapper, .main-sidebar {
          font-family: 'Inter', sans-serif !important;
        }
        
        .content-wrapper {
          background: linear-gradient(135deg, #f5f7fa 0%, #e8ecf1 100%) !important;
        }
        
        /* Header Styling */
        .main-header .logo {
          background: linear-gradient(135deg, #013369 0%, #1a4d8f 100%) !important;
          font-weight: 700 !important;
          border-bottom: 3px solid #D50A0A;
        }
        
        .main-header .navbar {
          background: linear-gradient(135deg, #013369 0%, #1a4d8f 100%) !important;
        }
        
        /* Sidebar Styling */
        .main-sidebar {
          background: linear-gradient(180deg, #1a1a2e 0%, #16213e 100%) !important;
          box-shadow: 4px 0 15px rgba(0,0,0,0.1);
        }
        
        .sidebar-menu > li > a {
          color: #e8ecf1 !important;
          border-left: 3px solid transparent;
          transition: all 0.3s ease;
          padding: 15px 20px !important;
        }
        
        .sidebar-menu > li > a:hover {
          background: rgba(213, 10, 10, 0.1) !important;
          border-left: 3px solid #D50A0A;
          transform: translateX(5px);
        }
        
        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, rgba(213, 10, 10, 0.2) 0%, transparent 100%) !important;
          border-left: 3px solid #D50A0A;
          font-weight: 600;
        }
        
        /* Enhanced Box Styling */
        .box {
          border-radius: 15px !important;
          box-shadow: 0 8px 25px rgba(0,0,0,0.08) !important;
          border: none !important;
          transition: transform 0.3s ease, box-shadow 0.3s ease;
          background: white !important;
          overflow: hidden;
        }
        
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 12px 35px rgba(0,0,0,0.12) !important;
        }
        
        .box-header {
          background: linear-gradient(135deg, #013369 0%, #1a4d8f 100%) !important;
          color: white !important;
          padding: 20px !important;
          border-radius: 15px 15px 0 0 !important;
        }
        
        .box-header .box-title {
          font-weight: 700 !important;
          font-size: 18px !important;
          letter-spacing: 0.5px;
        }
        
        .box.box-solid.box-primary > .box-header {
          background: linear-gradient(135deg, #013369 0%, #1a4d8f 100%) !important;
        }
        
        .box-body {
          padding: 25px !important;
        }
        
        /* Metric Cards */
        .metric-card {
          background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
          padding: 25px;
          border-radius: 15px;
          text-align: center;
          box-shadow: 0 5px 20px rgba(0,0,0,0.06);
          border: 2px solid transparent;
          transition: all 0.3s ease;
          position: relative;
          overflow: hidden;
        }
        
        .metric-card::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 4px;
          background: linear-gradient(90deg, #013369, #D50A0A);
        }
        
        .metric-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 30px rgba(0,0,0,0.12);
          border-color: #D50A0A;
        }
        
        .metric-card h4 {
          color: #6c757d;
          font-size: 14px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 1px;
          margin-bottom: 10px;
        }
        
        .metric-card h2 {
          color: #013369;
          font-size: 36px;
          font-weight: 700;
          margin: 0;
          text-shadow: 2px 2px 4px rgba(0,0,0,0.05);
        }
        
        /* Buttons */
        .btn {
          border-radius: 10px !important;
          font-weight: 600 !important;
          letter-spacing: 0.5px;
          transition: all 0.3s ease !important;
          border: none !important;
          text-transform: uppercase;
          font-size: 13px !important;
          color: white !important;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #013369 0%, #1a4d8f 100%) !important;
          box-shadow: 0 4px 15px rgba(1, 51, 105, 0.3) !important;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, #D50A0A 0%, #ff1a1a 100%) !important;
          box-shadow: 0 6px 25px rgba(213, 10, 10, 0.4) !important;
          transform: translateY(-2px);
        }
        
        .btn-lg {
          padding: 18px 45px !important;
          font-size: 16px !important;
          border-radius: 50px !important;
        }
        
        #go_to_map {
          background: linear-gradient(135deg, #D50A0A 0%, #ff1a1a 100%) !important;
          box-shadow: 0 6px 25px rgba(213, 10, 10, 0.3) !important;
          animation: pulse 2s infinite;
        }
        
        @keyframes pulse {
          0%, 100% { transform: scale(1); }
          50% { transform: scale(1.05); }
        }
        
        /* Form Controls */
        .form-control, .selectize-input {
          border-radius: 10px !important;
          border: 2px solid #e8ecf1 !important;
          padding: 12px 15px !important;
          transition: all 0.3s ease !important;
          font-size: 14px !important;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #013369 !important;
          box-shadow: 0 0 0 0.2rem rgba(1, 51, 105, 0.15) !important;
        }
        
        .radio label, .checkbox label {
          font-weight: 500;
          color: #495057;
          transition: color 0.3s ease;
        }
        
        .radio label:hover, .checkbox label:hover {
          color: #013369;
        }
        
        /* Home Page Styling */
        .home-container {
          background: white;
          border-radius: 20px;
          padding: 50px;
          box-shadow: 0 10px 40px rgba(0,0,0,0.1);
          position: relative;
          overflow: hidden;
        }
        
        .home-container::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 5px;
          background: linear-gradient(90deg, #013369, #D50A0A, #013369);
        }
        
        .home-title {
          color: #013369;
          font-weight: 800;
          margin-bottom: 15px;
          font-size: 48px;
          text-shadow: 2px 2px 4px rgba(0,0,0,0.05);
        }
        
        .home-subtitle {
          color: #D50A0A;
          margin-bottom: 30px;
          font-size: 28px;
          font-weight: 600;
        }
        
        .feature-list {
          text-align: left;
          max-width: 650px;
          margin: 30px auto;
          background: #f8f9fa;
          padding: 30px;
          border-radius: 15px;
          border-left: 5px solid #D50A0A;
        }
        
        .feature-list li {
          padding: 12px 0;
          font-size: 16px;
          color: #495057;
          font-weight: 500;
          position: relative;
          padding-left: 30px;
        }
        
        .feature-list li::before {
          content: '';
          position: absolute;
          left: 0;
          color: #D50A0A;
          font-size: 18px;
        }
        
        /* Headers */
        h2 {
          color: #013369;
          font-weight: 700;
          margin-bottom: 25px;
          padding-bottom: 15px;
          border-bottom: 3px solid #D50A0A;
          display: inline-block;
        }
        
        h3, h4 {
          color: #013369;
          font-weight: 600;
        }
        
        /* Tables */
        table {
          border-radius: 10px;
          overflow: hidden;
          box-shadow: 0 4px 15px rgba(0,0,0,0.05);
        }
        
        table thead {
          background: linear-gradient(135deg, #013369 0%, #1a4d8f 100%);
          color: white;
        }
        
        table thead th {
          padding: 15px;
          font-weight: 600;
          text-transform: uppercase;
          font-size: 12px;
          letter-spacing: 1px;
        }
        
        table tbody tr {
          transition: background 0.3s ease;
        }
        
        table tbody tr:hover {
          background: rgba(213, 10, 10, 0.05);
        }
        
        /* Value Bet Box */
        .value-bet-box {
          background: linear-gradient(135deg, #fff3cd 0%, #ffe69c 100%);
          border: 3px solid #ffc107;
          padding: 25px;
          border-radius: 15px;
          box-shadow: 0 6px 20px rgba(255, 193, 7, 0.2);
        }
        
        .value-bet-box h4 {
          color: #856404;
          font-weight: 700;
        }
        
        /* HR Styling */
        hr {
          border: 0;
          height: 2px;
          background: linear-gradient(90deg, transparent, #D50A0A, transparent);
          margin: 25px 0;
        }
        
        /* Scrollbar */
        ::-webkit-scrollbar {
          width: 10px;
        }
        
        ::-webkit-scrollbar-track {
          background: #f1f1f1;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(180deg, #013369, #D50A0A);
          border-radius: 5px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: #D50A0A;
        }
        
        /* Leaflet Map Styling */
        .leaflet-container {
          border-radius: 15px;
          box-shadow: 0 6px 20px rgba(0,0,0,0.1);
        }
        
        /* Loading Animation */
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        .box {
          animation: fadeIn 0.5s ease;
        }
      "))
    ),
      
      # ADD THIS LINE - Dynamic CSS based on favorite team
      uiOutput("dynamic_css"),
      
        # ... rest of your tabs
    
      tabItems(
      # ----------------------------------------
      # HOME TAB - Enhanced Design
      # ----------------------------------------
        tabItem(tabName = "home",
              fluidRow(
                column(width = 12,
                       tags$div(
                         class = "home-container",
                         style = "text-align: center;",
                         
                         tags$img(src = "nfl_logo.jpg", width = "350px", 
                                  style = "margin-bottom: 40px; filter: drop-shadow(0 10px 20px rgba(0,0,0,0.15));"),
                         
                         tags$h1("NFL Over/Under Analysis", class = "home-title"),
                         tags$h3("Stadium Performance Dashboard", class = "home-subtitle"),
                         
                         tags$p(
                           style = "font-size: 19px; line-height: 1.8; max-width: 850px; margin: 0 auto 30px auto; color: #495057; font-weight: 400;",
                           "This interactive dashboard analyzes over/under betting performance across NFL stadiums from 2000-2025.",
                           "Discover which venues consistently favor overs or unders, and explore geographical patterns in scoring trends."
                         ),
                         
                         tags$div(
                           class = "feature-list",
                           tags$h4("Features:", style = "color: #013369; margin-bottom: 20px; font-size: 22px;"),
                           tags$ul(
                             style = "list-style: none; padding: 0;",
                             tags$li("Interactive map showing over/under performance by stadium"),
                             tags$li("Toggle between overs and unders to compare trends"),
                             tags$li("Zoom to specific stadiums for detailed analysis"),
                             tags$li("Color-coded visualizations for easy pattern recognition"),
                             tags$li("Advanced ROI calculator for betting strategies"),
                             tags$li("AI-powered game predictions with value bet analysis")
                           )
                         ),
                         
                         actionButton("go_to_map", 
                                      "Explore Stadium Map", 
                                      icon = icon("map-marked-alt"),
                                      class = "btn-lg")
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # STADIUM MAP TAB
      # ----------------------------------------
      tabItem(tabName = "analytics",
              h2(icon("map-marker-alt"), " NFL Stadium Over/Under Performance (2000-2025)"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Map Controls"),
                           radioButtons("metric", "Select Metric:",
                                        choices = c("Overs" = "overs", "Unders" = "unders"),
                                        selected = "overs"),
                           hr(),
                           selectInput("stadium_select", "Zoom to Stadium:",
                                       choices = c("All Stadiums" = "all"),
                                       selected = "all"),
                           hr(),
                           helpText(icon("info-circle"), " Toggle between Overs and Unders to see the count at each NFL stadium.",
                                    "Larger circles indicate more games hitting that outcome.",
                                    "Click on markers for detailed information.")
                       )
                ),
                column(width = 8,
                       box(width = NULL, solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Interactive Stadium Map"),
                           leafletOutput("map", height = "600px")
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # BETTING ANALYSIS TAB
      # ----------------------------------------
      tabItem(tabName = "betting_analysis",
              h2(icon("chart-bar"), " NFL Betting Analysis (2000-2025)"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Analysis Controls"),
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
                           
                           h4(icon("lightbulb"), " Graph Explanation:"),
                           uiOutput("explanation_text")
                       )
                ),
                column(width = 8,
                       box(width = NULL, solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Analysis Results"),
                           plotlyOutput("plot", height = "500px"),
                           br(),
                           tableOutput("table"),
                           br(),
                           downloadButton("download_betting_data", "Export Table to CSV", 
                                          class = "btn-primary", icon = icon("download"))
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # ROI CALCULATOR TAB
      # ----------------------------------------
      tabItem(tabName = "roi_calculator",
              h2(icon("calculator"), " ROI Calculator"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Strategy Configuration"),
                           
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
                           
                           h4(icon("info-circle"), " How It Works:"),
                           p("This calculator shows what would have happened if you used this strategy on every game in the dataset."),
                           p(strong("Betting Odds:")),
                           tags$ul(
                             tags$li("Over, Under: -110 (standard)"),
                             tags$li("Favorite/Underdog: varies by spread")
                           )
                       )
                ),
                column(width = 8,
                       box(width = NULL, solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Strategy Results"),
                           
                           fluidRow(
                             column(3, 
                                    div(class = "metric-card",
                                        h4("Games Bet"),
                                        h2(textOutput("roi_total_games", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(class = "metric-card",
                                        h4("Win Rate"),
                                        h2(textOutput("roi_win_rate", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(class = "metric-card",
                                        h4("Total Profit"),
                                        h2(textOutput("roi_profit", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(class = "metric-card",
                                        h4("ROI"),
                                        h2(textOutput("roi_percentage", inline = TRUE))
                                    )
                             )
                           ),
                           
                           br(),
                           
                           plotlyOutput("roi_plot", height = "400px"),
                           
                           br(),
                           
                           h4("Team Performance for This Strategy"),
                           tableOutput("roi_team_table"),
                           br(),
                           downloadButton("download_roi_data", "Export ROI Data to CSV", 
                                          class = "btn-primary", icon = icon("download"))
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # GAME PREDICTOR TAB
      # ----------------------------------------
      tabItem(tabName = "game_predictor",
              h2(icon("brain"), " AI Game Predictor"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Game Setup"),
                           
                           h4(icon("home"), " Home Team"),
                           selectInput("pred_home_team", NULL, choices = NULL),
                           
                           fluidRow(
                             column(6, numericInput("pred_home_wins", "Wins:", value = 8, min = 0, max = 17, step = 1)),
                             column(6, numericInput("pred_home_losses", "Losses:", value = 5, min = 0, max = 17, step = 1))
                           ),
                           
                           fluidRow(
                             column(6, numericInput("pred_home_off_rank", "Off. Rank:", value = 10, min = 1, max = 32, step = 1)),
                             column(6, numericInput("pred_home_def_rank", "Def. Rank:", value = 15, min = 1, max = 32, step = 1))
                           ),
                           
                           hr(),
                           
                           h4(icon("plane"), " Away Team"),
                           selectInput("pred_away_team", NULL, choices = NULL),
                           
                           fluidRow(
                             column(6, numericInput("pred_away_wins", "Wins:", value = 7, min = 0, max = 17, step = 1)),
                             column(6, numericInput("pred_away_losses", "Losses:", value = 6, min = 0, max = 17, step = 1))
                           ),
                           
                           fluidRow(
                             column(6, numericInput("pred_away_off_rank", "Off. Rank:", value = 12, min = 1, max = 32, step = 1)),
                             column(6, numericInput("pred_away_def_rank", "Def. Rank:", value = 18, min = 1, max = 32, step = 1))
                           ),
                           
                           hr(),
                           
                           h4(icon("cog"), " Game Conditions"),
                           
                           numericInput("pred_spread", "Spread (+ = away favored, - = home favored):",
                                        value = 3, min = -25, max = 25, step = 0.5),
                           
                           selectInput("pred_stadium", "Stadium Type:",
                                       choices = c("Outdoor", "Indoor"),
                                       selected = "Outdoor"),
                           
                           sliderInput("pred_week", "Week of Season:",
                                       min = 1, max = 18, value = 9, step = 1),
                           
                           actionButton("predict_btn", "Predict Winner", 
                                        class = "btn-primary btn-lg btn-block",
                                        icon = icon("magic"))
                       )
                ),
                column(width = 8,
                       box(width = NULL, solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Prediction Results"),
                           
                           uiOutput("prediction_result"),
                           
                           br(),
                           
                           div(class = "value-bet-box",
                               h4(icon("dollar-sign"), " Betting Value Analysis"),
                               uiOutput("value_bet_analysis")
                           ),
                           
                           br(),
                           
                           h4("Model Performance"),
                           fluidRow(
                             column(3,
                                    div(class = "metric-card",
                                        h4("Accuracy"),
                                        h3(textOutput("model_accuracy", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(class = "metric-card",
                                        h4("Total Games"),
                                        h3(textOutput("model_total_games", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(class = "metric-card",
                                        h4("Home Win %"),
                                        h3(textOutput("model_home_rate", inline = TRUE))
                                    )
                             ),
                             column(3,
                                    div(class = "metric-card",
                                        h4("Improvement"),
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
              h2(icon("project-diagram"), " NFL Team Cluster Analysis"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Clustering Configuration"),
                           
                           selectInput("cluster_factor1", "Factor 1 (X-Axis):",
                                       choices = list(
                                         "Win Percentage" = "win_pct",
                                         "Average Points Scored" = "avg_points_scored",
                                         "Average Points Allowed" = "avg_points_allowed",
                                         "Point Differential" = "point_diff",
                                         "Over Hit Rate" = "over_rate",
                                         "Under Hit Rate" = "under_rate",
                                         "Favorite Cover Rate" = "fav_cover_rate",
                                         "Underdog Cover Rate" = "dog_cover_rate",
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
                                         "Favorite Cover Rate" = "fav_cover_rate",
                                         "Underdog Cover Rate" = "dog_cover_rate",
                                         "Home Win Rate" = "home_win_rate",
                                         "Away Win Rate" = "away_win_rate"
                                       ),
                                       selected = "avg_points_scored"),
                           
                           hr(),
                           
                           sliderInput("n_clusters", "Number of Clusters:",
                                       min = 1, max = 4, value = 4, step = 1),
                           
                           hr(),
                           
                           h4(icon("info-circle"), " About Clustering:"),
                           p("K-means clustering groups teams with similar characteristics together based on the 2 factors you select."),
                           p("Teams in the same cluster share similar patterns across both metrics."),
                           p("Use this to identify team archetypes and strategic patterns.")
                       )
                ),
                column(width = 8,
                       box(width = NULL, solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Team Clusters"),
                           plotlyOutput("cluster_plot", height = "500px"),
                           br(),
                           h4("Cluster Characteristics"),
                           tableOutput("cluster_summary_table"),
                           br(),
                           h4("Teams by Cluster"),
                           tableOutput("cluster_teams_table"),
                           br(),
                           downloadButton("download_cluster_data", "Export Cluster Data to CSV", 
                                          class = "btn-primary", icon = icon("download"))
                       )
                )
              )
      )
    )
  )
)