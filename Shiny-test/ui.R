library(shiny)
library(tidyverse)
library(plotly)

fluidPage(
  titlePanel("NFL Betting Analysis"),
  
  tabsetPanel(
    tabPanel("Betting Trends",
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
    ),
    
    tabPanel("ROI Calculator",
             sidebarLayout(
               sidebarPanel(
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
               ),
               
               mainPanel(
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
    ),
    
    tabPanel("Game Predictor",
             sidebarLayout(
               sidebarPanel(
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
               ),
               
               mainPanel(
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
  )
)