function(input, output, session) {
  
  # Load and process data
  nfl_data <- reactive({
    # Read your CSV file here
    # df <- read.csv("your_nfl_data.csv")
    
    # For demonstration, I'll create the structure based on your columns
    # Replace this with: df <- read.csv("path/to/your/file.csv")
    
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