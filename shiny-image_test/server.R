function(input, output, session) {
  
  # ========================================
  # STADIUM MAP TAB - Data Loading
  # ========================================
  
  # STEP 1: Load your stadium coordinates
  stadium_coords <- read.csv("../nfl_stadiums_coordinates.csv")
  
  # STEP 1.5: Load stadium images (NEW!)
  
  stadium_images <- read.csv("nfl_stadium_images.csv")
  
  # STEP 2: Load your game data with over/under results
  set.seed(123)
  game_data <- read.csv("../cleaned_nfl_data.csv")
  
  # STEP 3: Calculate overs and unders by stadium
  stadium_summary <- game_data %>%
    group_by(stadium) %>%
    summarise(
      overs = sum(Over_Under_Hit == "Over"),
      unders = sum(Over_Under_Hit == "Under"),
      total_games = n(),
      .groups = "drop"
    )
  
  # STEP 4: Merge with coordinates AND images (UPDATED!)
  stadium_data <- stadium_summary %>%
    left_join(stadium_coords, by = "stadium") %>%
    left_join(stadium_images, by = "stadium") %>%
    filter(!is.na(latitude) & !is.na(longitude))
  
  # Update dropdown choices with stadium names
  observe({
    stadium_choices <- c("All Stadiums" = "all", setNames(stadium_data$stadium, stadium_data$stadium))
    updateSelectInput(session, "stadium_select", choices = stadium_choices)
  })
  
  # Navigate to map when button is clicked
  observeEvent(input$go_to_map, {
    updateTabItems(session, "tabs", selected = "analytics")
  })
  
  # ========================================
  # STADIUM MAP TAB - Reactive Data & Map
  # ========================================
  
  # Reactive data based on selected metric
  map_data <- reactive({
    req(input$metric)
    
    metric_col <- input$metric
    stadium_data %>%
      mutate(
        value = if(metric_col == "overs") overs else unders,
        percentage = round(value / total_games * 100, 1)
      ) 
  })
  
  # Create color palette
  color_pal <- reactive({
    req(input$metric)
    
    if(input$metric == "overs") {
      colorNumeric(palette = "Reds", domain = map_data()$value)
    } else {
      colorNumeric(palette = "Blues", domain = map_data()$value)
    }
  })
  
  # Render the BASE map (only runs once)
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -40, lat = 30, zoom = 3)
  })
  
  # Observer to ADD circles when on the map tab
  observe({
    req(input$tabs == "analytics")  # Only run when on Stadium Map tab
    req(input$metric, input$stadium_select)
    
    data <- map_data()
    pal <- color_pal()
    
    metric_label <- if(input$metric == "overs") "Overs" else "Unders"
    
    # Handle stadium selection for zoom
    if(input$stadium_select != "all") {
      selected_stadium <- data %>% filter(stadium == input$stadium_select)
      if(nrow(selected_stadium) > 0) {
        leafletProxy("map") %>%
          setView(lng = selected_stadium$longitude[1],
                  lat = selected_stadium$latitude[1],
                  zoom = 10)
      }
    } else {
      leafletProxy("map") %>%
        setView(lng = -40, lat = 30, zoom = 3)
    }
    
    # UPDATED POPUP WITH IMAGES
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~sqrt(value) * 1.3,
        color = ~pal(value),
        fillColor = ~pal(value),
        fillOpacity = 0.6,
        stroke = TRUE,
        weight = 2,
        popup = ~{
          img_tag <- ifelse(!is.na(image_url) & image_url != "",
                            paste0("<img src='", image_url, 
                                   "' width='250' style='border-radius: 5px; margin-bottom: 10px;'><br/>"),
                            "")
          
          paste0(
            "<div style='text-align: center; min-width: 250px;'>",
            img_tag,
            "<strong style='font-size: 16px;'>", stadium, "</strong><br/>",
            "<hr style='margin: 8px 0;'>",
            "<div style='text-align: left;'>",
            "<b>Total Games:</b> ", total_games, "<br/>",
            "<b>Overs:</b> ", overs, " (", round(overs/total_games*100, 1), "%)<br/>",
            "<b>Unders:</b> ", unders, " (", round(unders/total_games*100, 1), "%)<br/>",
            "<hr style='margin: 8px 0;'>",
            "<b style='color: ", if(metric_label == "Overs") "#d73027" else "#4575b4", ";'>",
            metric_label, ": ", value, " (", percentage, "%)</b>",
            "</div>",
            "</div>"
          )
        },
        label = ~paste0(stadium, ": ", value, " ", metric_label)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~value,
        title = paste0("Number of<br/>", metric_label),
        opacity = 0.7
      )
  })
  
  # ========================================
  # BETTING ANALYSIS TAB - Data Processing
  # ========================================
  
  # Load and process data for betting analysis
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
  
  # ========================================
  # BETTING ANALYSIS TAB - Outputs
  # ========================================
  
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
  
  # ========================================
  # ROI CALCULATOR TAB - WITH YEAR FILTER
  # ========================================
  
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
  
  # Calculate ROI results - NOW WITH YEAR FILTER
  roi_results <- reactive({
    df <- nfl_data()
    
    # Filter by year range (NEW!)
    df <- df %>%
      filter(schedule_season >= input$roi_year_range[1] & 
               schedule_season <= input$roi_year_range[2])
    
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
  
}
