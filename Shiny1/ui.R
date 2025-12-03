# Load ALL libraries at the very top
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyverse)
library(plotly)
library(htmltools)
library(shinyStore)
library(DT)
library(randomForest)
print('text')

# NFL Teams list (shared across UI)
nfl_teams <- c(
  "Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills",
  "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
  "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
  "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
  "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
  "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
  "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
  "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"
)

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
      menuItem("Cluster Analysis", tabName = "cluster_analysis", icon = icon("project-diagram")),
      menuItem("My Betting Library", tabName = "betting_library", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    initStore("store", "nfl_analytics_store"),
    
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
    
    # Dynamic CSS based on favorite team
    uiOutput("dynamic_css"),
    
    tabItems(
      # ----------------------------------------
      # HOME TAB
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
                           sliderInput("stadium_year_range", "Year Range:",
                                       min = 2000, max = 2025, 
                                       value = c(2000, 2025),
                                       step = 1, sep = ""),
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
              ),
              fluidRow(
                column(width = 12,
                       box(width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                           title = tags$span(style = "color: white;", " How to Read the Stadium Map (Click to Expand)"),
                           tags$div(style = "padding: 10px;",
                                    h4(" What Are Overs and Unders?"),
                                    p(strong("Over/Under Betting"), "is a wager on the total combined points scored by both teams in a game."),
                                    tags$ul(
                                      tags$li(strong("Over:"), "The bet wins if the total points scored ", tags$em("exceeds"), "the betting line set by oddsmakers."),
                                      tags$li(strong("Under:"), "The bet wins if the total points scored ", tags$em("falls below"), "the betting line.")
                                    ),
                                    p(tags$strong("Example:"), "If the over/under line is 47.5 points, and the final score is 28-24 (52 total), the ", 
                                      tags$span(style="color: #28a745; font-weight: bold;", "Over"), " wins."),
                                    hr(),
                                    h4(" How to Read This Map"),
                                    tags$ul(
                                      tags$li(tags$strong("Multiple Circles:"), "Each circle represents one NFL stadium. The size of the circle is proportional to the ", tags$em("number"), " of Overs or Unders that hit at that venue."),
                                      tags$li(tags$strong("Circle Colors:"), "The gradient (blue to red) shows the ", tags$em("count"), " of that outcome relative to other stadiums. Darker red = more Overs/Unders."),
                                      tags$li(tags$strong("Click for Details:"), "Click any stadium marker to see the exact count, percentage, and a photo of the venue."),
                                      tags$li(tags$strong("Percentage Matters:"), "A stadium with 60% Overs means that historically, games there tend to be ", tags$em("higher-scoring"), " than the betting line suggests.")
                                    ),
                                    hr(),
                                    h4(" Why This Matters for Bettors"),
                                    p("Certain stadiums consistently favor overs or unders due to factors like:"),
                                    tags$ul(
                                      tags$li(tags$strong("Weather:"), "Outdoor stadiums in cold/windy cities (Buffalo, Green Bay) often see lower scores"),
                                      tags$li(tags$strong("Altitude:"), "Denver's thin air can lead to more scoring"),
                                      tags$li(tags$strong("Dome Effect:"), "Indoor stadiums eliminate weather, creating consistent scoring environments"),
                                      tags$li(tags$strong("Field Conditions:"), "Grass vs. turf affects offensive efficiency")
                                    ),
                                    p(style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107;",
                                      icon("chart-line"), " ", tags$strong("Pro Tip:"), 
                                      " If a stadium shows 58%+ Overs over 200+ games, it's statistically significant. ",
                                      "Bettors can use this trend when that stadium hosts future games.")
                           )
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
              ),
              fluidRow(
                column(width = 12,
                       box(width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                           title = tags$span(style = "color: white;", "Understanding Betting Analysis (Click to Expand)"),
                           tags$div(style = "padding: 10px;",
                                    h4(" What is Spread Betting?"),
                                    p(strong("The Point Spread"), "is the predicted margin of victory set by oddsmakers. When you bet on the favorite, they must win by ", 
                                      tags$em("more"), " than the spread. When you bet on the underdog, they must either win outright ", 
                                      tags$em("or"), " lose by ", tags$em("less"), " than the spread."),
                                    tags$ul(
                                      tags$li(tags$strong("Favorite Covered:"), "The favored team won by more than the spread. If Kansas City was -7 and won 31-20 (11-point margin), the favorite covered."),
                                      tags$li(tags$strong("Underdog Covered:"), "The underdog either won the game OR lost by less than the spread. If Buffalo was +7 and lost 27-24 (3-point margin), the underdog covered."),
                                      tags$li(tags$strong("Push:"), "When the margin exactly equals the spread (e.g., -7 favorite wins by exactly 7), bets are refunded.")
                                    ),
                                    p(tags$strong("Example:"), "If the Patriots are favored by -3.5 over the Dolphins and win 24-17 (7-point margin), the Patriots ", 
                                      tags$span(style="color: #28a745; font-weight: bold;", "covered"), " because 7 > 3.5."),
                                    hr(),
                                    h4(" What Does This Analysis Show?"),
                                    p("This tool reveals ", tags$strong("situational betting patterns"), " by breaking down historical performance across different factors:"),
                                    tags$ul(
                                      tags$li(tags$strong("Season Type:"), "How do teams perform in Week 1 vs. mid-season vs. playoffs? Early season games often have more unpredictable outcomes."),
                                      tags$li(tags$strong("Stadium Type:"), "Do indoor domes favor favorites more than outdoor stadiums? Weather-controlled environments reduce unpredictability."),
                                      tags$li(tags$strong("Temperature:"), "Cold weather games often favor defenses and underdogs. Teams struggle to score in sub-freezing conditions."),
                                      tags$li(tags$strong("Home Team:"), "Which teams consistently beat the spread when playing at home? Some franchises have strong home-field advantages.")
                                    ),
                                    hr(),
                                    h4(" Understanding the 50% Red Line"),
                                    p("The ", tags$span(style="color: red; font-weight: bold;", "red dashed line at 50%"), " represents ", 
                                      tags$strong("break-even performance"), ". Here's why it matters:"),
                                    tags$ul(
                                      tags$li(tags$strong("Above 50%:"), "If favorites cover 55% of the time in indoor stadiums, betting favorites in domes would be ", 
                                              tags$span(style="color: #28a745;", "profitable"), " long-term (assuming standard -110 odds)."),
                                      tags$li(tags$strong("Below 50%:"), "If favorites only cover 45% of the time in cold weather, betting favorites in those conditions would ", 
                                              tags$span(style="color: #dc3545;", "lose money"), " over time."),
                                      tags$li(tags$strong("At 50%:"), "This is exactly what oddsmakers expect. Due to the \"vig\" (betting commission), you need to win ", 
                                              tags$em("52.4%"), " of bets at -110 odds to break even.")
                                    ),
                                    p(style = "background: #e7f3ff; padding: 15px; border-radius: 8px; border-left: 4px solid #013369;",
                                      icon("calculator"), " ", tags$strong("Math Behind It:"), 
                                      " To profit at standard -110 odds (bet $110 to win $100), you need a 52.4% win rate. ",
                                      "Any factor showing 54%+ success over 100+ games represents a potential edge."),
                                    hr(),
                                    h4(" How to Use the Filters"),
                                    tags$ol(
                                      tags$li(tags$strong("Select 'What to Analyze':"), 
                                              tags$ul(
                                                tags$li(tags$strong("Favorite Covered:"), "See when betting favorites pays off"),
                                                tags$li(tags$strong("Over Hit:"), "Identify high-scoring situations"),
                                                tags$li(tags$strong("Under Hit:"), "Find low-scoring patterns")
                                              )),
                                      tags$li(tags$strong("Choose a Factor:"), "Pick the variable you want to test (weather, week, team, etc.)"),
                                      tags$li(tags$strong("Analyze the Results:"), "Look for bars significantly above or below 50%"),
                                      tags$li(tags$strong("Check Sample Size:"), "Hover over bars to see game counts - patterns with 50+ games are more reliable")
                                    ),
                                    hr(),
                                    h4(" Real-World Betting Applications"),
                                    tags$ul(
                                      tags$li(tags$strong("Identify Situational Edges:"), "If Week 1 shows favorites only cover 42% of the time, consider betting underdogs in season openers."),
                                      tags$li(tags$strong("Weather-Based Strategy:"), "If cold weather games go Under 58% of the time, target unders in December games in Buffalo/Green Bay."),
                                      tags$li(tags$strong("Home Field Advantage:"), "Some teams (Seattle, Kansas City) consistently cover at home due to crowd noise and altitude/weather factors."),
                                      tags$li(tags$strong("Playoff Trends:"), "Playoff games often trend differently than regular season - tighter defenses, lower scoring.")
                                    ),
                                    p(style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107;",
                                      icon("exclamation-triangle"), " ", tags$strong("Important Disclaimer:"), 
                                      " Past performance does not guarantee future results. Vegas oddsmakers adjust lines based on these same factors. ",
                                      "Use this analysis as ", tags$em("one tool"), " in a broader betting strategy, not as a guaranteed prediction system. ",
                                      "Always bet responsibly and within your means.")
                           )
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
              ),
              fluidRow(
                column(width = 12,
                       box(width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                           title = tags$span(style = "color: white;", "Understanding the ROI Calculator (Click to Expand)"),
                           tags$div(style = "padding: 15px;",
                                    h4("What This Calculator Does"),
                                    p("This tool simulates what would have happened if you blindly followed a single betting strategy on ", 
                                      tags$strong("every game"), " in the dataset from 2000-2025. It calculates your total profit/loss and ROI (Return on Investment) to show which strategies would have been profitable over time."),
                                    hr(),
                                    h4("The Four Strategies Explained"),
                                    tags$ul(
                                      tags$li(tags$strong("Always Bet the Favorite:"), " Bet on the team favored to win (negative spread) in every game. You're betting they'll cover the spread."),
                                      tags$li(tags$strong("Always Bet the Underdog:"), " Bet on the team expected to lose (positive spread) in every game. Underdogs offer higher payouts when they cover."),
                                      tags$li(tags$strong("Always Take the Over:"), " Bet that the total combined score will be higher than the betting line in every game."),
                                      tags$li(tags$strong("Always Take the Under:"), " Bet that the total combined score will be lower than the betting line in every game.")
                                    ),
                                    hr(),
                                    h4("How Betting Odds Work"),
                                    p("The calculator uses realistic odds based on the point spread:"),
                                    tags$ul(
                                      tags$li(tags$strong("Over/Under Bets:"), " Standard -110 odds (risk $110 to win $100). This is the most common betting line."),
                                      tags$li(tags$strong("Favorite Bets:"), " The bigger the spread, the worse the payout:"),
                                      tags$ul(
                                        tags$li("Small favorite (-3 or less): -110 odds"),
                                        tags$li("Medium favorite (-3 to -7): -150 odds"),
                                        tags$li("Big favorite (-7 to -10): -200 odds"),
                                        tags$li("Heavy favorite (-10+): -300 odds")
                                      ),
                                      tags$li(tags$strong("Underdog Bets:"), " The bigger the spread, the better the payout:"),
                                      tags$ul(
                                        tags$li("Small underdog (+3 or less): +120 odds"),
                                        tags$li("Medium underdog (+3 to +7): +180 odds"),
                                        tags$li("Big underdog (+7 to +10): +250 odds"),
                                        tags$li("Heavy underdog (+10+): +350 odds")
                                      )
                                    ),
                                    p(style = "background: #e7f3ff; padding: 15px; border-radius: 8px; border-left: 4px solid #013369;",
                                      tags$strong("Odds Example:"), 
                                      " If you bet $100 on a heavy underdog at +350 odds and win, you profit $350 (total return: $450). ",
                                      "If you bet $100 on a heavy favorite at -300 odds and win, you only profit $33 (total return: $133)."),
                                    hr(),
                                    h4("Understanding ROI"),
                                    p(tags$strong("ROI (Return on Investment)"), " shows your profit as a percentage of total money wagered:"),
                                    tags$ul(
                                      tags$li(tags$strong("Positive ROI (+15%):"), " You made a 15% profit. For every $100 wagered, you made $15."),
                                      tags$li(tags$strong("Negative ROI (-8%):"), " You lost 8%. For every $100 wagered, you lost $8."),
                                      tags$li(tags$strong("Break-Even (0%):"), " You didn't make or lose money overall.")
                                    ),
                                    p(style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107;",
                                      tags$strong("The 52.4% Rule:"), 
                                      " Due to the betting commission (\"vig\"), you need to win ", tags$strong("52.4% of your bets at -110 odds"), 
                                      " just to break even. This is why many strategies show negative ROI even with 50% win rates."),
                                    hr(),
                                    p(style = "background: #f8d7da; padding: 15px; border-radius: 8px; border-left: 4px solid #dc3545;",
                                      tags$strong("Responsible Gambling:"), 
                                      " This tool is for educational analysis only. Past performance does not predict future results. ",
                                      "Sports betting involves significant financial risk. Never wager more than you can afford to lose. ",
                                      "If you choose to bet, use this as ONE data point among many, not a guaranteed system.")
                           )
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
                             column(6, numericInput("pred_home_avg_scored", "Avg Points Scored:", value = 24, min = 0, max = 100, step = 0.1)),
                             column(6, numericInput("pred_home_avg_allowed", "Avg Points Allowed:", value = 22, min = 0, max = 100, step = 0.1))
                           ),
                           hr(),
                           h4(icon("plane"), " Away Team"),
                           selectInput("pred_away_team", NULL, choices = NULL),
                           fluidRow(
                             column(6, numericInput("pred_away_wins", "Wins:", value = 7, min = 0, max = 17, step = 1)),
                             column(6, numericInput("pred_away_losses", "Losses:", value = 6, min = 0, max = 17, step = 1))
                           ),
                           fluidRow(
                             column(6, numericInput("pred_away_avg_scored", "Avg Points Scored:", value = 22, min = 0, max = 100, step = 0.1)),
                             column(6, numericInput("pred_away_avg_allowed", "Avg Points Allowed:", value = 24, min = 0, max = 100, step = 0.1))
                           ),
                           hr(),
                           h4(icon("cog"), " Game Conditions"),
                           fluidRow(
                             column(6,
                                    numericInput("pred_spread", "Spread (points):",
                                                 value = 3, min = 0.5, max = 25, step = 0.5)
                             ),
                             column(6,
                                    br(),
                                    checkboxInput("pred_home_favored", "Home Team Favored?", value = TRUE)
                             )
                           ),
                           helpText(icon("info-circle"), " Check the box if the home team is favored. The spread will be applied with the correct sign automatically."),
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
              ),
              fluidRow(
                column(width = 12,
                       box(width = NULL, status = "info", solidHeader = TRUE, 
                           collapsible = TRUE, collapsed = TRUE,
                           title = tags$span(style = "color: white;", 
                                             icon("question-circle"), 
                                             " How Does the Game Predictor Work? (Click to Expand)"),
                           tags$div(style = "padding: 15px;",
                                    h4(" The AI Prediction Model"),
                                    p("This predictor uses a ", tags$strong("Random Forest machine learning model"), 
                                      " trained on ", tags$strong("25 years of NFL games (2000-2025)"), 
                                      " to predict game outcomes. It's like having 500 expert analysts who each studied thousands of games and vote on who will win."),
                                    hr(),
                                    h4(" Factor Weights (What Matters Most)"),
                                    p("The model weighs different factors based on their predictive power:"),
                                    tags$ol(
                                      tags$li(tags$strong("Point Spread (3.0x weight)"), " - The betting line is the single most important factor. Vegas oddsmakers are very accurate, so a team favored by 7+ points has a significantly higher win probability."),
                                      tags$li(tags$strong("Home Team Win % (1.5x weight)"), " - A team's overall record matters, especially when they're playing at home."),
                                      tags$li(tags$strong("Away Team Win % (1.4x weight)"), " - The visiting team's record, slightly less important than home win rate due to home-field advantage."),
                                      tags$li(tags$strong("Offensive Strength (1.3x weight)"), " - Average points scored per game for both teams. High-powered offenses win more often."),
                                      tags$li(tags$strong("Defensive Strength (1.2x weight)"), " - Average points allowed per game. Better defenses give teams an edge."),
                                      tags$li(tags$strong("Week of Season (1.0x weight)"), " - Early season games are less predictable than late-season games as teams improve/decline.")
                                    ),
                                    p(style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107;",
                                      icon("exclamation-triangle"), " ", tags$strong("Responsible Betting:"), 
                                      " This tool provides statistical analysis, NOT guaranteed predictions. Past performance does not guarantee future results. ",
                                      "Always bet responsibly, never wager more than you can afford to lose, and use this as ONE tool in your research, not your only source.")
                           )
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # CLUSTER ANALYSIS TAB
      # ----------------------------------------
      tabItem(tabName = "cluster_analysis",
              h2(icon("project-diagram"), " NFL Team Cluster & PCA Analysis"),
              fluidRow(
                column(width = 4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Analysis Configuration"),
                           
                           radioButtons("analysis_method", "Analysis Method:",
                                        choices = c("K-Means Clustering" = "kmeans",
                                                    "PCA (Principal Component Analysis)" = "pca"),
                                        selected = "kmeans"),
                           
                           hr(),
                           
                           conditionalPanel(
                             condition = "input.analysis_method == 'kmeans'",
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
                                         min = 2, max = 6, value = 4, step = 1)
                           ),
                           
                           hr(),
                           
                           h4(icon("info-circle"), " About This Analysis:"),
                           conditionalPanel(
                             condition = "input.analysis_method == 'kmeans'",
                             p("K-means clustering groups teams with similar characteristics together based on the 2 factors you select."),
                             p("Teams in the same cluster share similar patterns across both metrics.")
                           ),
                           conditionalPanel(
                             condition = "input.analysis_method == 'pca'",
                             p(strong("PCA"), " reduces all team metrics into 2 principal components that capture the most variance."),
                             p("Teams close together are similar across ", strong("all"), " metrics, not just two."),
                             p("The loadings plot shows which variables contribute most to each component.")
                           )
                       )
                ),
                column(width = 8,
                       box(width = NULL, solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Analysis Results"),
                           plotlyOutput("cluster_plot", height = "500px"),
                           
                           conditionalPanel(
                             condition = "input.analysis_method == 'pca'",
                             br(),
                             h4("PCA Loadings (What Drives Each Component)"),
                             plotlyOutput("pca_loadings_plot", height = "350px"),
                             br(),
                             h4("Variance Explained"),
                             tableOutput("pca_variance_table")
                           ),
                           
                           conditionalPanel(
                             condition = "input.analysis_method == 'kmeans'",
                             br(),
                             h4("Cluster Characteristics"),
                             tableOutput("cluster_summary_table")
                           ),
                           
                           br(),
                           h4(uiOutput("teams_table_title")),
                           tableOutput("cluster_teams_table"),
                           br(),
                           downloadButton("download_cluster_data", "Export Data to CSV", 
                                          class = "btn-primary", icon = icon("download"))
                       )
                )
              ),
              
              # Explanation Box
              fluidRow(
                column(width = 12,
                       box(width = NULL, status = "info", solidHeader = TRUE, 
                           collapsible = TRUE, collapsed = TRUE,
                           title = tags$span(style = "color: white;", 
                                             icon("question-circle"), 
                                             " Understanding PCA & Cluster Analysis (Click to Expand)"),
                           tags$div(style = "padding: 15px;",
                                    
                                    h3(icon("compress-arrows-alt"), " What is PCA (Principal Component Analysis)?"),
                                    
                                    h4("The Problem PCA Solves"),
                                    p("Imagine you have 32 NFL teams and want to compare them. But each team has ", 
                                      strong("9 different statistics:"), " Win %, Points Scored, Points Allowed, Point Differential, Over Rate, Favorite Cover Rate, Underdog Cover Rate, Home Win Rate, and Away Win Rate."),
                                    p(strong("How do you visualize 9 dimensions on a 2D screen?"), 
                                      " You can't plot 9 axes on paper. PCA solves this by compressing all 9 statistics into ", 
                                      strong("2 'super-statistics'"), " called Principal Components (PC1 and PC2)."),
                                    
                                    tags$div(style = "background: #e7f3ff; padding: 15px; border-radius: 8px; border-left: 4px solid #013369; margin: 15px 0;",
                                             p(style = "margin: 0;", icon("lightbulb"), " ", 
                                               strong("Simple Analogy:"), " Imagine describing houses with 20 features (square footage, bedrooms, bathrooms, lot size, age, etc.). ",
                                               "OR you could summarize with just 2 things: ", strong("'Size'"), " and ", strong("'Luxury Level'"), 
                                               ". These two summaries capture MOST of what matters. PCA does the same thing mathematically with team stats.")
                                    ),
                                    
                                    hr(),
                                    
                                    h4(icon("chart-line"), " Understanding PC1 (Principal Component 1)"),
                                    p("PC1 is the direction that captures the ", strong("most variation"), " between teams. Think of it as the single most important way teams differ from each other."),
                                    p("In NFL data, PC1 typically represents ", strong("'Overall Team Quality'"), ":"),
                                    tags$ul(
                                      tags$li(strong("High PC1 (right side of plot):"), " Good win %, scores lots of points, doesn't allow many points - ", span("excellent teams", style = "color: #28a745; font-weight: bold;")),
                                      tags$li(strong("Low PC1 (left side of plot):"), " Poor win %, low scoring, gives up lots of points - ", span("struggling teams", style = "color: #dc3545; font-weight: bold;"))
                                    ),
                                    p(strong("PC1 answers:"), " 'On a single scale from worst to best, where does each team rank?'"),
                                    
                                    hr(),
                                    
                                    h4(icon("chart-bar"), " Understanding PC2 (Principal Component 2)"),
                                    p("PC2 captures the ", strong("second biggest source of variation"), " - patterns that PC1 missed."),
                                    p("In NFL data, PC2 often represents ", strong("'Betting Performance vs Actual Performance'"), ":"),
                                    tags$ul(
                                      tags$li(strong("High PC2 (top of plot):"), " Covers spreads often, good betting value - regardless of overall win %"),
                                      tags$li(strong("Low PC2 (bottom of plot):"), " Doesn't cover spreads, poor betting value - regardless of overall win %")
                                    ),
                                    p(strong("PC2 answers:"), " 'Among teams of similar quality, what else differentiates them?'"),
                                    
                                    hr(),
                                    
                                    h4(icon("map-marker-alt"), " Reading the PCA Plot - Position Guide"),
                                    
                                    tags$table(style = "width: 100%; border-collapse: collapse; margin: 15px 0;",
                                               tags$thead(style = "background: #013369; color: white;",
                                                          tags$tr(
                                                            tags$th(style = "padding: 10px; text-align: left;", "Position on Plot"),
                                                            tags$th(style = "padding: 10px; text-align: left;", "What It Means"),
                                                            tags$th(style = "padding: 10px; text-align: left;", "Example")
                                                          )
                                               ),
                                               tags$tbody(
                                                 tags$tr(style = "background: #f8f9fa;",
                                                         tags$td(style = "padding: 10px;", strong("Top-Right")),
                                                         tags$td(style = "padding: 10px;", "Great team + great betting value"),
                                                         tags$td(style = "padding: 10px;", "Dynasty teams that dominate AND cover spreads")
                                                 ),
                                                 tags$tr(
                                                   tags$td(style = "padding: 10px;", strong("Bottom-Right")),
                                                   tags$td(style = "padding: 10px;", "Great team but poor betting value"),
                                                   tags$td(style = "padding: 10px;", "Heavy favorites that win but don't beat inflated spreads")
                                                 ),
                                                 tags$tr(style = "background: #f8f9fa;",
                                                         tags$td(style = "padding: 10px;", strong("Top-Left")),
                                                         tags$td(style = "padding: 10px;", "Bad team but good betting value"),
                                                         tags$td(style = "padding: 10px;", "Scrappy underdogs that cover spreads despite losing")
                                                 ),
                                                 tags$tr(
                                                   tags$td(style = "padding: 10px;", strong("Bottom-Left")),
                                                   tags$td(style = "padding: 10px;", "Bad team AND bad betting value"),
                                                   tags$td(style = "padding: 10px;", "Truly terrible teams that lose AND don't cover")
                                                 ),
                                                 tags$tr(style = "background: #f8f9fa;",
                                                         tags$td(style = "padding: 10px;", strong("Teams Close Together")),
                                                         tags$td(style = "padding: 10px;", "Similar across ALL 9 metrics"),
                                                         tags$td(style = "padding: 10px;", "Comparable team profiles")
                                                 ),
                                                 tags$tr(
                                                   tags$td(style = "padding: 10px;", strong("Teams Far Apart")),
                                                   tags$td(style = "padding: 10px;", "Very different team profiles"),
                                                   tags$td(style = "padding: 10px;", "Opposite ends of the spectrum")
                                                 )
                                               )
                                    ),
                                    
                                    hr(),
                                    
                                    h4(icon("arrows-alt"), " The Loadings Plot - What Drives Each Component?"),
                                    p("The loadings plot shows ", strong("how much each original statistic contributes"), " to PC1 and PC2."),
                                    tags$ul(
                                      tags$li(strong("Variable pointing RIGHT:"), " That stat increases PC1 (team quality)"),
                                      tags$li(strong("Variable pointing UP:"), " That stat increases PC2 (betting value)"),
                                      tags$li(strong("Longer arrow:"), " That stat has MORE influence on the components"),
                                      tags$li(strong("Shorter arrow:"), " That stat has LESS influence")
                                    ),
                                    p("Example: If 'Win %' points far right, it heavily influences PC1. If 'Underdog Cover %' points up, it drives PC2."),
                                    
                                    hr(),
                                    
                                    h4(icon("percentage"), " Variance Explained - How Good is the Summary?"),
                                    p("The Variance Explained table shows ", strong("how much information each component captures"), ":"),
                                    tags$ul(
                                      tags$li(strong("PC1 = 45%:"), " PC1 alone captures 45% of all differences between teams"),
                                      tags$li(strong("PC2 = 20%:"), " PC2 captures an additional 20%"),
                                      tags$li(strong("Combined = 65%:"), " Together, PC1 + PC2 explain 65% of everything")
                                    ),
                                    
                                    tags$div(style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107; margin: 15px 0;",
                                             p(style = "margin: 0;", icon("info-circle"), " ",
                                               strong("Is 65% Good?"), " Above 60% means the 2D plot is a reliable summary. 40-60% is decent but some nuance is lost. Below 40% means the plot is missing important differences.")
                                    ),
                                    
                                    hr(),
                                    
                                    h4(icon("balance-scale"), " PCA vs K-Means Clustering - When to Use Each"),
                                    
                                    tags$table(style = "width: 100%; border-collapse: collapse; margin: 15px 0;",
                                               tags$thead(style = "background: #013369; color: white;",
                                                          tags$tr(
                                                            tags$th(style = "padding: 10px; text-align: left;", "Method"),
                                                            tags$th(style = "padding: 10px; text-align: left;", "Best For"),
                                                            tags$th(style = "padding: 10px; text-align: left;", "Limitations")
                                                          )
                                               ),
                                               tags$tbody(
                                                 tags$tr(style = "background: #f8f9fa;",
                                                         tags$td(style = "padding: 10px;", strong("PCA")),
                                                         tags$td(style = "padding: 10px;", "Seeing the 'big picture' of how ALL teams compare using ALL 9 stats at once. Finding hidden patterns and similar teams."),
                                                         tags$td(style = "padding: 10px;", "Components can be harder to interpret than raw stats.")
                                                 ),
                                                 tags$tr(
                                                   tags$td(style = "padding: 10px;", strong("K-Means Clustering")),
                                                   tags$td(style = "padding: 10px;", "Grouping teams into distinct categories based on 2 specific stats you choose. Easy to interpret."),
                                                   tags$td(style = "padding: 10px;", "Only uses 2 of 9 available stats; ignores the rest.")
                                                 )
                                               )
                                    ),
                                    
                                    hr(),
                                    
                                    h4(icon("futbol"), " Real-World NFL Example"),
                                    tags$div(style = "background: #d4edda; padding: 15px; border-radius: 8px; border-left: 4px solid #28a745;",
                                             p("Let's say we want to find which NFL teams are most similar to the ", strong("Philadelphia Eagles"), 
                                               " based on ALL their statistics - not just wins, but scoring, betting performance, home/away splits, everything."),
                                             p("With PCA, we compress all 9 statistics into 2 scores and plot every team. ", 
                                               strong("The teams closest to the Eagles on the PCA plot are the most similar overall.")),
                                             p(style = "margin: 0;", "Without PCA, we'd have to make 36 different charts (every pair of 9 statistics) and try to mentally combine them. PCA does that combination mathematically in one view.")
                                    ),
                                    
                                    hr(),
                                    
                                    h4(icon("check-circle"), " Key Takeaways"),
                                    tags$ol(
                                      tags$li(strong("PCA compresses many statistics into 2 'super-statistics'"), " so we can visualize complex data on a simple 2D plot."),
                                      tags$li(strong("PC1 is the most important pattern"), " - usually representing overall team quality (good teams right, bad teams left)."),
                                      tags$li(strong("PC2 is the second most important pattern"), " - often representing betting value or other secondary differences."),
                                      tags$li(strong("Teams close together on the plot are similar"), " across all original statistics."),
                                      tags$li(strong("Variance explained tells you how much information is preserved"), " - higher is better (aim for 60%+)."),
                                      tags$li(strong("The loadings plot shows which stats drive each component"), " - helping you interpret what PC1 and PC2 represent.")
                                    )
                           )
                       )
                )
              )
      ),
      
      # ----------------------------------------
      # BETTING LIBRARY TAB
      # ----------------------------------------
      tabItem(tabName = "betting_library",
              h2(icon("book"), " My Betting Library"),
              
              fluidRow(
                column(3,
                       div(class = "metric-card",
                           h4("Total Bets"),
                           h2(textOutput("lib_total_bets", inline = TRUE))
                       )
                ),
                column(3,
                       div(class = "metric-card",
                           h4("Total Wagered"),
                           h2(textOutput("lib_total_wagered", inline = TRUE))
                       )
                ),
                column(3,
                       div(class = "metric-card",
                           h4("Net Profit/Loss"),
                           h2(textOutput("lib_net_profit", inline = TRUE))
                       )
                ),
                column(3,
                       div(class = "metric-card",
                           h4("Win Rate"),
                           h2(textOutput("lib_win_rate", inline = TRUE))
                       )
                )
              ),
              
              br(),
              
              fluidRow(
                column(3,
                       div(class = "metric-card",
                           h4("Current Streak"),
                           h2(textOutput("lib_streak", inline = TRUE))
                       )
                ),
                column(3,
                       div(class = "metric-card",
                           h4("Best Bet Type"),
                           h2(textOutput("lib_best_type", inline = TRUE))
                       )
                ),
                column(3,
                       div(class = "metric-card",
                           h4("Avg Bet Size"),
                           h2(textOutput("lib_avg_bet", inline = TRUE))
                       )
                ),
                column(3,
                       div(class = "metric-card",
                           h4("ROI"),
                           h2(textOutput("lib_roi", inline = TRUE))
                       )
                )
              ),
              
              br(),
              
              fluidRow(
                # Add Bet Form
                column(4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Add New Bet"),
                           
                           checkboxInput("lib_is_parlay", "Parlay Bet?", value = FALSE),
                           
                           conditionalPanel(
                             condition = "input.lib_is_parlay == false",
                             selectInput("lib_home_team", "Home Team:", 
                                         choices = c("Select..." = "", sort(nfl_teams))),
                             selectInput("lib_away_team", "Away Team:", 
                                         choices = c("Select..." = "", sort(nfl_teams))),
                             selectInput("lib_bet_type", "Bet Type:",
                                         choices = c("Favorite", "Underdog", "Over", "Under"))
                           ),
                           
                           conditionalPanel(
                             condition = "input.lib_is_parlay == true",
                             numericInput("lib_num_legs", "Number of Legs:", 
                                          value = 2, min = 2, max = 10, step = 1),
                             textAreaInput("lib_parlay_details", "Parlay Details:",
                                           placeholder = "e.g., Chiefs -3, Bills Over 47.5, Eagles ML",
                                           rows = 4)
                           ),
                           
                           numericInput("lib_amount_wagered", "Amount Wagered ($):",
                                        value = 100, min = 1, step = 1),
                           
                           numericInput("lib_amount_won", "Amount Won ($):",
                                        value = 0, min = 0, step = 1),
                           
                           numericInput("lib_amount_lost", "Amount Lost ($):",
                                        value = 0, min = 0, step = 1),
                           
                           textAreaInput("lib_notes", "Notes (optional):",
                                         placeholder = "Any additional details...",
                                         rows = 3),
                           
                           hr(),
                           
                           actionButton("lib_add_bet", "Add Bet", 
                                        class = "btn-primary btn-block",
                                        icon = icon("plus")),
                           
                           br(), br(),
                           
                           actionButton("lib_clear_all", "Clear All Bets", 
                                        class = "btn-danger btn-block",
                                        icon = icon("trash"))
                       )
                ),
                
                # Bets Table
                column(8,
                       box(width = NULL, solidHeader = TRUE,
                           title = tags$span(style = "color: white;", "Your Bets"),
                           
                           DT::dataTableOutput("lib_bets_table"),
                           
                           br(),
                           
                           downloadButton("lib_export_csv", "Export to CSV", 
                                          class = "btn-primary", icon = icon("download"))
                       )
                )
              )
      )
    )
  )
)