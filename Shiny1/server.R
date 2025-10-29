function(input, output, session) {
  
  # ========== YOUR EXISTING CODE - LOAD AND PROCESS DATA ==========
  
  nfl_data <- reactive({
    df <- read.csv("../cleaned_nfl_data.csv")
    
    # Add calculated columns
    df <- df %>%
      mutate(
        # Treat blank/NA/empty weather as outdoor
        stadium_type = ifelse(!is.na(weather_detail) & weather_detail == "indoor", "Indoor", "Outdoor"),
        
        # Temperature categories (blank = outdoor)
        temp_category = case_when(
          weather_detail == "indoor" ~ "Indoor",
          is.na(weather_temperature) ~ "Outdoor (Unknown Temp)",
          weather_temperature < 50 ~ "Cold (<50°F)",
          weather_temperature < 70 ~ "Moderate (50-69°F)",
          TRUE ~ "Warm (70°F+)"
        ),
        
        # Season type: Regular season (weeks 1-18) vs Playoffs
        season_type = ifelse(schedule_playoff == FALSE, 
                             paste0("Week ", schedule_week),
                             "Playoffs"),
        
        # Calculate total points
        total_points = score_home + score_away,
        
        # Determine if over or under hit
        over_hit = ifelse(total_points > over_under_line, "Over", "Under"),
        
        # Check if home team was favorite
        home_is_favorite = (team_home == team_favorite_name)
      )
    
    return(df)
  })
  
  # ========== YOUR EXISTING BETTING TRENDS CODE ==========
  
  # Filter data based on selections
  filtered_data <- reactive({
    df <- nfl_data()
    
    # For home team + favorite: only include when home team IS the favorite
    if (input$bet_type == "fav_correct" && input$factor == "team_home") {
      df <- df %>% filter(home_is_favorite == TRUE)
    }
    
    return(df)
  })
  
  # Calculate hit rates
  analysis_data <- reactive({
    df <- filtered_data()
    
    if (nrow(df) == 0) return(data.frame())
    
    df %>%
      group_by(!!sym(input$factor)) %>%
      summarise(
        Games = n(),
        Hits = case_when(
          input$bet_type == "fav_correct" ~ sum(fav_correct == "Yes", na.rm = TRUE),
          input$bet_type == "over_hit" ~ sum(over_hit == "Over", na.rm = TRUE),
          input$bet_type == "under_hit" ~ sum(over_hit == "Under", na.rm = TRUE),
          TRUE ~ 0
        ),
        `Hit Rate` = round(Hits / Games * 100, 1),
        .groups = "drop"
      ) %>%
      filter(Games >= 1) %>%
      arrange(desc(`Hit Rate`))
  })
  
  # Plot
  output$plot <- renderPlotly({
    data <- analysis_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% layout(title = "Not enough data or no filters selected"))
    }
    
    plot_ly(data, 
            x = ~get(names(data)[1]), 
            y = ~`Hit Rate`,
            type = 'bar',
            text = ~paste0(`Hit Rate`, "%<br>", Games, " games"),
            hoverinfo = 'text',
            marker = list(color = ~`Hit Rate`,
                          colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green')),
                          cmin = 0, cmax = 100)) %>%
      layout(
        title = paste(gsub("_", " ", input$bet_type), "Hit Rate by", 
                      gsub("_", " ", input$factor)),
        xaxis = list(title = gsub("_", " ", input$factor)),
        yaxis = list(title = "Hit Rate (%)", range = c(0, 100)),
        shapes = list(
          list(type = "line", x0 = -0.5, x1 = nrow(data) - 0.5, 
               y0 = 50, y1 = 50, 
               line = list(color = "red", dash = "dash", width = 2))
        )
      )
  })
  
  # Table
  output$table <- renderTable({
    analysis_data()
  })
  
  # Explanation text
  output$explanation_text <- renderUI({
    text <- ""
    
    if (input$bet_type == "fav_correct") {
      if (input$factor == "team_home") {
        text <- "This graph shows the hit rate for games where the selected home team was ALSO the favorite. It measures how often home favorites cover the spread."
      } else {
        text <- "This graph shows how often the favorite team covered the spread, broken down by the selected factor."
      }
    } else if (input$bet_type == "over_hit") {
      text <- "This graph shows how often betting the OVER hit based on the selected factor. The red line at 50% represents break-even betting performance."
    } else if (input$bet_type == "under_hit") {
      text <- "This graph shows how often betting the UNDER hit based on the selected factor. The red line at 50% represents break-even betting performance."
    }
    
    HTML(paste0("<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px; font-size: 14px;'>", 
                text, "</div>"))
  })
  
  # ========== YOUR EXISTING ROI CALCULATOR CODE ==========
  
  # Populate team dropdown dynamically based on strategy
  observeEvent(input$roi_strategy, {
    df <- nfl_data()
    
    # Get unique teams based on strategy
    if (input$roi_strategy == "favorite") {
      teams <- sort(unique(df$team_favorite_name))
    } else if (input$roi_strategy == "underdog") {
      # For underdogs, get teams that aren't the favorite
      underdog_teams <- df %>%
        mutate(underdog = ifelse(team_home == team_favorite_name, team_away, team_home)) %>%
        pull(underdog) %>%
        unique() %>%
        sort()
      teams <- underdog_teams
    } else {
      # For over/under, use all unique teams
      teams <- sort(unique(c(df$team_home, df$team_away)))
    }
    
    # Remove any NA values
    teams <- teams[!is.na(teams)]
    
    updateSelectInput(session, "roi_team_filter",
                      choices = c("All Teams" = "all", setNames(teams, teams)),
                      selected = "all")
  })
  
  # Calculate ROI results
  roi_results <- reactive({
    df <- nfl_data()
    
    # Determine which team we're betting on for each game
    df <- df %>%
      mutate(
        bet_team = case_when(
          input$roi_strategy == "favorite" ~ team_favorite_name,
          input$roi_strategy == "underdog" ~ ifelse(team_home == team_favorite_name, team_away, team_home),
          input$roi_strategy %in% c("over", "under") ~ team_home,
          TRUE ~ NA_character_
        )
      )
    
    # Filter by team if not "All Teams"
    if (input$roi_team_filter != "all") {
      df <- df %>%
        filter(bet_team == input$roi_team_filter)
    }
    
    # Determine if bet won based on strategy
    df <- df %>%
      mutate(
        bet_won = case_when(
          input$roi_strategy == "favorite" ~ !is.na(fav_correct) & (fav_correct == "Yes" | fav_correct == TRUE | fav_correct == 1),
          input$roi_strategy == "underdog" ~ !is.na(fav_correct) & (fav_correct == "No" | fav_correct == FALSE | fav_correct == 0),
          input$roi_strategy == "over" ~ !is.na(over_hit) & (over_hit == "Over"),
          input$roi_strategy == "under" ~ !is.na(over_hit) & (over_hit == "Under"),
          TRUE ~ FALSE
        ),
        # Calculate odds multiplier based on strategy and spread
        odds_multiplier = case_when(
          # Underdog moneyline - better payouts for bigger underdogs
          input$roi_strategy == "underdog" & !is.na(spread_favorite) ~ case_when(
            abs(spread_favorite) <= 3 ~ 1.20,      # +120 odds
            abs(spread_favorite) <= 7 ~ 1.80,      # +180 odds
            abs(spread_favorite) <= 10 ~ 2.50,     # +250 odds
            TRUE ~ 3.50                             # +350 odds for big underdogs
          ),
          # Favorite moneyline - worse payouts for bigger favorites
          input$roi_strategy == "favorite" & !is.na(spread_favorite) ~ case_when(
            abs(spread_favorite) <= 3 ~ 0.9091,    # -110 odds (risk 110 to win 100)
            abs(spread_favorite) <= 7 ~ 0.6667,    # -150 odds (risk 150 to win 100)
            abs(spread_favorite) <= 10 ~ 0.5000,   # -200 odds (risk 200 to win 100)
            TRUE ~ 0.3333                           # -300 odds (risk 300 to win 100)
          ),
          TRUE ~ 0.9091  # -110 odds for over/under
        ),
        # Calculate profit using appropriate odds
        profit = ifelse(bet_won, 
                        input$roi_bet_amount * odds_multiplier, 
                        -input$roi_bet_amount)
      )
    
    # Remove any rows with NA profits
    df <- df %>% filter(!is.na(profit) & !is.na(bet_won))
    
    # Calculate cumulative profit for chart
    df <- df %>%
      arrange(schedule_season, schedule_week) %>%
      mutate(cumulative_profit = cumsum(profit))
    
    return(df)
  })
  
  # Summary statistics
  output$roi_total_games <- renderText({
    nrow(roi_results())
  })
  
  output$roi_win_rate <- renderText({
    df <- roi_results()
    if (nrow(df) == 0) return("0%")
    wins <- sum(df$bet_won, na.rm = TRUE)
    total <- nrow(df)
    paste0(round(wins / total * 100, 1), "%")
  })
  
  output$roi_profit <- renderText({
    df <- roi_results()
    if (nrow(df) == 0) return("$0")
    total_profit <- sum(df$profit, na.rm = TRUE)
    color <- ifelse(total_profit >= 0, "green", "red")
    sign <- ifelse(total_profit >= 0, "+", "")
    paste0(sign, "$", formatC(total_profit, format = "f", digits = 0, big.mark = ","))
  })
  
  output$roi_percentage <- renderText({
    df <- roi_results()
    if (nrow(df) == 0) return("0%")
    total_profit <- sum(df$profit, na.rm = TRUE)
    total_wagered <- nrow(df) * input$roi_bet_amount
    roi_pct <- (total_profit / total_wagered) * 100
    sign <- ifelse(roi_pct >= 0, "+", "")
    paste0(sign, round(roi_pct, 1), "%")
  })
  
  # Cumulative profit chart
  output$roi_plot <- renderPlotly({
    df <- roi_results()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No games found for this team/strategy combination"))
    }
    
    # Create game number
    df$game_num <- 1:nrow(df)
    
    # Determine chart title based on filter
    chart_title <- if (input$roi_team_filter == "all") {
      "Cumulative Profit Over Time (All Teams)"
    } else {
      paste0("Cumulative Profit Over Time (", input$roi_team_filter, ")")
    }
    
    plot_ly(df, x = ~game_num, y = ~cumulative_profit, type = 'scatter', mode = 'lines',
            line = list(color = ~ifelse(cumulative_profit >= 0, 'green', 'red')),
            hovertext = ~paste0("Game ", game_num, 
                                "<br>", team_home, " vs ", team_away,
                                "<br>Cumulative: $", round(cumulative_profit, 0)),
            hoverinfo = 'text') %>%
      layout(
        title = chart_title,
        xaxis = list(title = "Game Number"),
        yaxis = list(title = "Cumulative Profit ($)"),
        shapes = list(
          list(type = "line", x0 = 0, x1 = nrow(df),
               y0 = 0, y1 = 0,
               line = list(color = "black", dash = "dash", width = 1))
        )
      )
  })
  
  # Team performance table
  output$roi_team_table <- renderTable({
    df <- roi_results()
    
    # Determine which team to group by based on strategy
    if (input$roi_strategy %in% c("favorite", "underdog")) {
      # For spread bets, group by the team we're betting on
      if (input$roi_strategy == "favorite") {
        df <- df %>%
          mutate(bet_team = team_favorite_name)
      } else {
        df <- df %>%
          mutate(bet_team = ifelse(team_home == team_favorite_name, team_away, team_home))
      }
    } else {
      # For over/under, group by home team
      df <- df %>%
        mutate(bet_team = team_home)
    }
    
    # Calculate team stats
    team_stats <- df %>%
      group_by(bet_team) %>%
      summarise(
        Games = n(),
        Wins = sum(bet_won),
        `Win %` = round(mean(bet_won) * 100, 1),
        Profit = sum(profit),
        .groups = "drop"
      ) %>%
      arrange(desc(Profit)) %>%
      head(10)  # Show top 10
    
    # Format profit with $ and +/- sign
    team_stats <- team_stats %>%
      mutate(
        Profit = paste0(ifelse(Profit >= 0, "+$", "-$"), 
                        formatC(abs(Profit), format = "f", digits = 0, big.mark = ","))
      )
    
    names(team_stats)[1] <- "Team"
    
    return(team_stats)
  })
  
  # ========== NEW GAME PREDICTOR CODE ==========
  
  # Populate team dropdowns
  observe({
    data <- nfl_data()
    teams <- sort(unique(c(data$team_home, data$team_away)))
    teams <- teams[!is.na(teams)]
    
    updateSelectInput(session, "pred_home_team", choices = teams, selected = teams[1])
    updateSelectInput(session, "pred_away_team", choices = teams, selected = teams[2])
  })
  
  # Auto-fill spread and stadium when teams are selected
  observeEvent(c(input$pred_home_team, input$pred_away_team), {
    req(input$pred_home_team, input$pred_away_team)
    
    data <- nfl_data()
    
    # Find historical matchups
    matchups <- data %>%
      filter(team_home == input$pred_home_team & team_away == input$pred_away_team)
    
    if (nrow(matchups) > 0) {
      # Calculate averages from historical matchups
      avg_spread <- mean(matchups$spread_favorite, na.rm = TRUE)
      most_common_stadium <- names(sort(table(matchups$stadium_type), decreasing = TRUE))[1]
      
      # Update inputs
      updateNumericInput(session, "pred_spread", value = round(avg_spread, 1))
      updateSelectInput(session, "pred_stadium", 
                        selected = ifelse(is.na(most_common_stadium), "Outdoor", most_common_stadium))
    } else {
      # No historical data, use home team averages
      home_games <- data %>% filter(team_home == input$pred_home_team)
      
      if (nrow(home_games) > 0) {
        avg_spread <- mean(home_games$spread_favorite, na.rm = TRUE)
        most_common_stadium <- names(sort(table(home_games$stadium_type), decreasing = TRUE))[1]
        
        updateNumericInput(session, "pred_spread", value = round(avg_spread, 1))
        updateSelectInput(session, "pred_stadium", 
                          selected = ifelse(is.na(most_common_stadium), "Outdoor", most_common_stadium))
      }
    }
  })
  
  # Build Random Forest prediction model with new features
  prediction_model <- reactive({
    data <- nfl_data()
    
    # Calculate team statistics for each game (using data UP TO that point)
    training_data <- data %>%
      arrange(schedule_season, schedule_week) %>%
      group_by(team_home) %>%
      mutate(
        home_games_played = row_number() - 1,
        home_wins = cumsum(lag(winner == team_home, default = FALSE)),
        home_win_pct = ifelse(home_games_played > 0, home_wins / home_games_played, 0.5)
      ) %>%
      ungroup() %>%
      group_by(team_away) %>%
      mutate(
        away_games_played = row_number() - 1,
        away_wins = cumsum(lag(winner == team_away, default = FALSE)),
        away_win_pct = ifelse(away_games_played > 0, away_wins / away_games_played, 0.5)
      ) %>%
      ungroup() %>%
      mutate(
        home_won = (winner == team_home),
        is_indoor = (stadium_type == "Indoor"),
        # For model training, use calculated win percentages
        home_strength = home_win_pct,
        away_strength = away_win_pct,
        # Generate synthetic rankings that correlate with win percentage
        # Better teams (higher win %) get better rankings (lower numbers)
        home_off_rank = pmax(1, pmin(32, round(33 - (home_win_pct * 25) + rnorm(n(), 0, 5)))),
        home_def_rank = pmax(1, pmin(32, round(33 - (home_win_pct * 20) + rnorm(n(), 0, 6)))),
        away_off_rank = pmax(1, pmin(32, round(33 - (away_win_pct * 25) + rnorm(n(), 0, 5)))),
        away_def_rank = pmax(1, pmin(32, round(33 - (away_win_pct * 20) + rnorm(n(), 0, 6)))),
        # Add spread multiplier to make it more dominant (multiply by 3 to increase importance)
        spread_weighted = spread_favorite * 3
      ) %>%
      filter(!is.na(spread_favorite) & !is.na(home_won) & home_games_played > 0 & away_games_played > 0)
    
    # Build Random Forest model with weighted spread and reduced home advantage
    library(randomForest)
    model <- randomForest(as.factor(home_won) ~ spread_weighted + is_indoor + schedule_week +
                            home_strength + away_strength + 
                            home_off_rank + home_def_rank + away_off_rank + away_def_rank,
                          data = training_data,
                          ntree = 500,
                          importance = TRUE,
                          # Reduce tree depth to prevent overfitting on home field
                          maxnodes = 50)
    
    list(model = model, data = training_data)
  })
  
  # Make prediction when button is clicked
  prediction_result <- eventReactive(input$predict_btn, {
    model_info <- prediction_model()
    
    # Calculate win percentages from user input
    home_total_games <- input$pred_home_wins + input$pred_home_losses
    away_total_games <- input$pred_away_wins + input$pred_away_losses
    
    home_win_pct <- if(home_total_games > 0) input$pred_home_wins / home_total_games else 0.5
    away_win_pct <- if(away_total_games > 0) input$pred_away_wins / away_total_games else 0.5
    
    # Create prediction data with user inputs
    new_data <- data.frame(
      spread_weighted = input$pred_spread * 3,  # Apply same weighting as training
      is_indoor = (input$pred_stadium == "Indoor"),
      schedule_week = input$pred_week,
      home_strength = home_win_pct,
      away_strength = away_win_pct,
      home_off_rank = input$pred_home_off_rank,
      home_def_rank = input$pred_home_def_rank,
      away_off_rank = input$pred_away_off_rank,
      away_def_rank = input$pred_away_def_rank
    )
    
    # Get probability from Random Forest
    prob <- predict(model_info$model, newdata = new_data, type = "prob")[,2]
    
    list(
      prob_home = prob,
      prob_away = 1 - prob,
      home_record = paste0(input$pred_home_wins, "-", input$pred_home_losses),
      away_record = paste0(input$pred_away_wins, "-", input$pred_away_losses)
    )
  })
  
  # Display prediction result
  output$prediction_result <- renderUI({
    result <- prediction_result()
    
    prob_home <- result$prob_home * 100
    prob_away <- result$prob_away * 100
    
    home_color <- ifelse(prob_home > 50, "#28a745", "#dc3545")
    away_color <- ifelse(prob_away > 50, "#28a745", "#dc3545")
    
    # Determine confidence level
    confidence_diff <- abs(prob_home - 50)
    confidence_level <- ifelse(confidence_diff > 30, "Very High",
                               ifelse(confidence_diff > 20, "High",
                                      ifelse(confidence_diff > 10, "Moderate", "Low")))
    confidence_color <- ifelse(confidence_diff > 20, "#28a745",
                               ifelse(confidence_diff > 10, "#ffc107", "#dc3545"))
    
    tagList(
      fluidRow(
        column(6,
               div(style = paste0("background-color: ", home_color, "; color: white; padding: 30px; border-radius: 10px; text-align: center;"),
                   h3(input$pred_home_team),
                   p(paste0("(", result$home_record, ")"), style = "font-size: 14px; margin: 0;"),
                   h1(paste0(round(prob_home, 1), "%")),
                   p("Win Probability", style = "font-size: 16px;")
               )
        ),
        column(6,
               div(style = paste0("background-color: ", away_color, "; color: white; padding: 30px; border-radius: 10px; text-align: center;"),
                   h3(input$pred_away_team),
                   p(paste0("(", result$away_record, ")"), style = "font-size: 14px; margin: 0;"),
                   h1(paste0(round(prob_away, 1), "%")),
                   p("Win Probability", style = "font-size: 16px;")
               )
        )
      ),
      br(),
      fluidRow(
        column(6,
               div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center;",
                   p(strong("Predicted Winner:"), style = "margin: 0;"),
                   h4(ifelse(prob_home > 50, input$pred_home_team, input$pred_away_team), 
                      style = "margin: 5px 0; color: #28a745;")
               )
        ),
        column(6,
               div(style = paste0("background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: center; border: 2px solid ", confidence_color, ";"),
                   p(strong("Confidence Level:"), style = "margin: 0;"),
                   h4(confidence_level, style = paste0("margin: 5px 0; color: ", confidence_color, ";"))
               )
        )
      )
    )
  })
  
  # BETTING VALUE FINDER
  output$value_bet_analysis <- renderUI({
    req(input$predict_btn)
    result <- prediction_result()
    
    prob_home <- result$prob_home
    prob_away <- result$prob_away
    
    # Calculate implied probability from spread
    spread <- input$pred_spread
    implied_prob_home <- 0.5 + (spread * -0.025)
    implied_prob_home <- max(0.2, min(0.8, implied_prob_home))
    
    # Calculate edge
    edge_home <- (prob_home - implied_prob_home) * 100
    edge_away <- (prob_away - (1 - implied_prob_home)) * 100
    
    value_threshold <- 5
    
    if (abs(edge_home) < value_threshold) {
      tagList(
        p(icon("info-circle"), strong(" No Strong Value Bet Found"), style = "font-size: 16px; color: #6c757d;"),
        p("Model prediction is close to Vegas expectations. This is a fairly-priced game."),
        p(paste0("Model edge: ", ifelse(edge_home > 0, "+", ""), round(edge_home, 1), "% (need >", value_threshold, "% for value)"))
      )
    } else if (edge_home > value_threshold) {
      home_odds <- ifelse(spread < -7, -200, ifelse(spread < -3, -150, -110))
      payout <- 100 / (abs(home_odds) / 100)
      ev <- (prob_home * payout) - ((1 - prob_home) * 100)
      
      tagList(
        div(style = "background-color: #d4edda; padding: 15px; border-radius: 5px; border-left: 5px solid #28a745;",
            p(icon("check-circle"), strong(paste0(" VALUE BET: ", toupper(input$pred_home_team))), 
              style = "font-size: 18px; color: #155724; margin: 0;")
        ),
        br(),
        p(strong("Why it's valuable:")),
        tags$ul(
          tags$li(paste0("Model: ", round(prob_home * 100, 1), "% win probability")),
          tags$li(paste0("Vegas: ~", round(implied_prob_home * 100, 1), "% implied")),
          tags$li(paste0("Your Edge: +", round(edge_home, 1), "%"))
        ),
        p(strong(paste0("Expected Value: ", ifelse(ev > 0, "+", ""), "$", round(ev, 2), " per $100 bet"))),
        p(ifelse(ev > 10, "🔥 Strong value!", ifelse(ev > 5, "✅ Good value", "Slight edge")), 
          style = paste0("color: ", ifelse(ev > 10, "#28a745", ifelse(ev > 5, "#20c997", "#6c757d")), "; font-weight: bold;"))
      )
    } else {
      away_odds <- ifelse(spread > 7, 250, ifelse(spread > 3, 180, 120))
      payout <- 100 * (away_odds / 100)
      ev <- (prob_away * payout) - ((1 - prob_away) * 100)
      
      tagList(
        div(style = "background-color: #d4edda; padding: 15px; border-radius: 5px; border-left: 5px solid #28a745;",
            p(icon("check-circle"), strong(paste0(" VALUE BET: ", toupper(input$pred_away_team))), 
              style = "font-size: 18px; color: #155724; margin: 0;")
        ),
        br(),
        p(strong("Why it's valuable:")),
        tags$ul(
          tags$li(paste0("Model: ", round(prob_away * 100, 1), "% win probability")),
          tags$li(paste0("Vegas: ~", round((1 - implied_prob_home) * 100, 1), "% implied")),
          tags$li(paste0("Your Edge: +", round(abs(edge_away), 1), "%"))
        ),
        p(strong(paste0("Expected Value: ", ifelse(ev > 0, "+", ""), "$", round(ev, 2), " per $100 bet"))),
        p(ifelse(ev > 10, "🔥 Strong value!", ifelse(ev > 5, "✅ Good value", "Slight edge")), 
          style = paste0("color: ", ifelse(ev > 10, "#28a745", ifelse(ev > 5, "#20c997", "#6c757d")), "; font-weight: bold;"))
      )
    }
  })
  
  # Model accuracy
  output$model_accuracy <- renderText({
    model_info <- prediction_model()
    
    predictions <- predict(model_info$model, type = "response")
    actual_winner <- model_info$data$home_won
    
    accuracy <- mean(predictions == actual_winner, na.rm = TRUE) * 100
    paste0(round(accuracy, 1), "%")
  })
  
  output$model_total_games <- renderText({
    formatC(nrow(prediction_model()$data), format = "d", big.mark = ",")
  })
  
  output$model_home_rate <- renderText({
    model_info <- prediction_model()
    home_rate <- mean(model_info$data$home_won, na.rm = TRUE) * 100
    paste0(round(home_rate, 1), "%")
  })
  
  # Variable importance plot
  output$importance_plot <- renderPlotly({
    model_info <- prediction_model()
    
    importance_df <- as.data.frame(importance(model_info$model))
    importance_df$Variable <- c("Spread (Weighted)", "Indoor", "Week", "Home Win %", "Away Win %",
                                "Home Off Rank", "Home Def Rank", "Away Off Rank", "Away Def Rank")
    importance_df <- importance_df %>%
      arrange(desc(MeanDecreaseGini)) %>%
      mutate(Importance = MeanDecreaseGini) %>%
      head(8)  # Show top 8
    
    plot_ly(importance_df, 
            x = ~Importance, 
            y = ~reorder(Variable, Importance),
            type = 'bar', 
            orientation = 'h',
            marker = list(color = '#1f77b4'),
            text = ~paste0(round(Importance, 1)),
            textposition = 'outside',
            hoverinfo = 'text',
            hovertext = ~paste0(Variable, ": ", round(Importance, 1))) %>%
      layout(
        title = "",
        xaxis = list(title = "Importance Score"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  # Head-to-head history table
  output$h2h_table <- renderTable({
    req(input$pred_home_team, input$pred_away_team)
    
    data <- nfl_data()
    
    h2h <- data %>%
      filter((team_home == input$pred_home_team & team_away == input$pred_away_team) |
               (team_home == input$pred_away_team & team_away == input$pred_home_team)) %>%
      arrange(desc(schedule_season), desc(schedule_week)) %>%
      head(10) %>%
      mutate(
        Winner = ifelse(winner == team_home, team_home, team_away),
        Margin = abs(score_home - score_away),
        Spread = round(spread_favorite, 1)
      ) %>%
      select(schedule_season, schedule_week, team_home, team_away, Winner, Margin, Spread)
    
    if (nrow(h2h) == 0) {
      return(data.frame(Message = "No historical matchups found in dataset"))
    }
    
    names(h2h) <- c("Season", "Week", "Home", "Away", "Winner", "Margin", "Spread")
    h2h
  })
  
}