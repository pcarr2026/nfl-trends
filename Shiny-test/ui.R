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
    )
  )
)