server <- function(input, output, session) {
  
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
  
  # NEW: Navigate to map when button is clicked
  observeEvent(input$go_to_map, {
    updateTabItems(session, "tabs", selected = "analytics")
  })
  
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
}