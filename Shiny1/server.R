function(input, output, session) {
  
  # ========================================
  # STADIUM MAP TAB - Data Loading
  # ========================================
  
  # STEP 1: Load your stadium coordinates
  stadium_coords <- read.csv("../nfl_stadiums_coordinates.csv")
  
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
  
  # STEP 4: Merge with coordinates
  stadium_data <- stadium_summary %>%
    left_join(stadium_coords, by = "stadium") %>%
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
        popup = ~paste0(
          "<strong>", stadium, "</strong><br/>",
          "Total Games: ", total_games, "<br/>",
          "Overs: ", overs, " (", round(overs/total_games*100, 1), "%)<br/>",
          "Unders: ", unders, " (", round(unders/total_games*100, 1), "%)<br/>",
          "<strong>", metric_label, ": ", value, " (", percentage, "%)</strong>"
        ),
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
}