function(input, output, session) {
  
  # ========== SHARED DATA ==========
  # NFL Teams list (used throughout the app)
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
  
  # ========== TEAM PERSONALIZATION ==========
  # NFL Team Colors
  team_colors <- reactive({
    data.frame(
      team = nfl_teams,
      primary_color = c(
        "#97233F", "#A71930", "#241773", "#00338D",
        "#0085CA", "#0B162A", "#FB4F14", "#311D00",
        "#041E42", "#FB4F14", "#0076B6", "#203731",
        "#03202F", "#002C5F", "#006778", "#E31837",
        "#000000", "#0080C6", "#003594", "#008E97",
        "#4F2683", "#002244", "#D3BC8D", "#0B2265",
        "#125740", "#004C54", "#FFB612", "#AA0000",
        "#002244", "#D50A0A", "#0C2340", "#773141"
      ),
      secondary_color = c(
        "#FFB612", "#000000", "#9E7C0C", "#C60C30",
        "#101820", "#C83803", "#000000", "#FF3C00",
        "#869397", "#002244", "#B0B7BC", "#FFB612",
        "#A71930", "#A2AAAD", "#D7A22A", "#FFB81C",
        "#A5ACAF", "#FFC20E", "#FFA300", "#FC4C02",
        "#FFC62F", "#C60C30", "#101820", "#A5ACAF",
        "#000000", "#A5ACAF", "#000000", "#B3995D",
        "#69BE28", "#FF7900", "#418FDE", "#FFB612"
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # Store favorite team
  favorite_team <- reactiveVal(NULL)
  
  # Track if modal has been shown
  modal_shown <- reactiveVal(FALSE)
  
  # Load stored team on startup
  observeEvent(input$store$favorite_team, {
    if (!is.null(input$store$favorite_team)) {
      favorite_team(input$store$favorite_team)
    }
  }, once = TRUE, ignoreNULL = FALSE)
  
  # Show modal once on startup - use invalidateLater to ensure session is ready
  observe({
    # Only run once
    if (modal_shown()) return()
    
    # Small delay to let session initialize
    invalidateLater(100, session)
    
    # Check if this is the first real run (after the delay)
    isolate({
      if (!modal_shown()) {
        modal_shown(TRUE)
        
        stored_team <- favorite_team()
        
        showModal(modalDialog(
          title = tags$div(
            style = "text-align: center;",
            icon("football-ball", style = "font-size: 48px; color: #013369; margin-bottom: 20px;"),
            h2("Welcome to NFL Analytics Pro!", style = "color: #013369; margin-top: 10px;")
          ),
          tags$div(
            style = "text-align: center; padding: 20px;",
            h4("Select Your Favorite Team", style = "margin-bottom: 20px;"),
            p("The dashboard will personalize to your team's colors!"),
            selectInput("favorite_team_select", NULL,
                        choices = nfl_teams,
                        selected = if (!is.null(stored_team) && stored_team != "Default") stored_team else "Philadelphia Eagles",
                        width = "100%")
          ),
          footer = tagList(
            actionButton("skip_team", "Skip", class = "btn-default"),
            actionButton("confirm_team", "Let's Go!", class = "btn-primary", 
                         icon = icon("check"))
          ),
          size = "m",
          easyClose = FALSE
        ))
      }
    })
  })
  
  observeEvent(input$confirm_team, {
    favorite_team(input$favorite_team_select)
    updateStore(session, "favorite_team", input$favorite_team_select)
    removeModal()
  })
  
  observeEvent(input$skip_team, {
    favorite_team("Default")
    removeModal()
  })
  
  # Generate dynamic CSS based on favorite team
  output$dynamic_css <- renderUI({
    req(favorite_team())
    
    if (favorite_team() == "Default") {
      return(NULL)
    }
    
    colors <- team_colors()
    team_color_data <- colors[colors$team == favorite_team(), ]
    
    if (nrow(team_color_data) == 0) {
      return(NULL)
    }
    
    primary <- team_color_data$primary_color
    secondary <- team_color_data$secondary_color
    
    tags$style(HTML(paste0("
      /* Header Colors */
      .main-header .logo {
        background: linear-gradient(135deg, ", primary, " 0%, ", secondary, " 100%) !important;
      }
      .main-header .navbar {
        background: linear-gradient(135deg, ", primary, " 0%, ", secondary, " 100%) !important;
      }
      /* Sidebar Active Item */
      .sidebar-menu > li.active > a {
        background: linear-gradient(90deg, ", primary, "0%, transparent 100%) !important;
        border-left-color: ", primary, " !important;
      }
      /* Box Headers */
      .box-header {
        background: linear-gradient(135deg, ", primary, " 0%, ", secondary, " 100%) !important;
      }
      .box.box-solid.box-primary > .box-header {
        background: linear-gradient(135deg, ", primary, " 0%, ", secondary, " 100%) !important;
      }
      /* Buttons */
      .btn-primary {
        background: linear-gradient(135deg, ", primary, " 0%, ", secondary, " 100%) !important;
        box-shadow: 0 4px 15px ", primary, "66 !important;
      }
      .btn-primary:hover {
        background: linear-gradient(135deg, ", secondary, " 0%, ", primary, " 100%) !important;
        box-shadow: 0 6px 25px ", secondary, "66 !important;
      }
      /* Headers */
      h2 {
        color: ", primary, " !important;
        border-bottom-color: ", secondary, " !important;
      }
      h3, h4 {
        color: ", primary, " !important;
      }
      /* Metric Cards Accent */
      .metric-card::before {
        background: linear-gradient(90deg, ", primary, ", ", secondary, ") !important;
      }
      .metric-card:hover {
        border-color: ", secondary, " !important;
      }
      .metric-card h2 {
        color: ", primary, " !important;
      }
      /* Home Container Accent */
      .home-container::before {
        background: linear-gradient(90deg, ", primary, ", ", secondary, ", ", primary, ") !important;
      }
      .home-title {
        color: ", primary, " !important;
      }
      .home-subtitle {
        color: ", secondary, " !important;
      }
      /* Feature List Accent */
      .feature-list {
        border-left-color: ", secondary, " !important;
      }
      .feature-list li::before {
        color: ", secondary, " !important;
      }
      /* Table Headers */
      table thead {
        background: linear-gradient(135deg, ", primary, " 0%, ", secondary, " 100%) !important;
      }
    ")))
  })
  
  # ========== TEAM LOGOS ==========
  team_logos <- reactive({
    data.frame(
      team = nfl_teams,
      logo_url = c(
        "https://a.espncdn.com/i/teamlogos/nfl/500/ari.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/atl.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/bal.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/buf.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/car.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/chi.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/cin.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/cle.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/dal.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/den.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/det.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/gb.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/hou.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/ind.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/jax.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/kc.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/lv.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/lac.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/lar.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/mia.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/min.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/ne.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/no.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/nyg.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/nyj.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/phi.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/pit.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/sf.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/sea.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/tb.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/ten.png",
        "https://a.espncdn.com/i/teamlogos/nfl/500/wsh.png"
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # ========== LOAD AND PROCESS DATA ==========
  nfl_data <- reactive({
    df <- read.csv("../cleaned_nfl_data.csv")
    
    df <- df %>%
      mutate(
        stadium_type = ifelse(!is.na(weather_detail) & weather_detail == "indoor", "Indoor", "Outdoor"),
        temp_category = case_when(
          weather_detail == "indoor" ~ "Indoor",
          is.na(weather_temperature) ~ "Outdoor (Unknown Temp)",
          weather_temperature < 50 ~ "Cold (<50°F)",
          weather_temperature < 70 ~ "Moderate (50-69°F)",
          TRUE ~ "Warm (70°F+)"
        ),
        season_type = ifelse(schedule_playoff == FALSE, 
                             paste0("Week ", schedule_week),
                             "Playoffs"),
        season_type = factor(season_type, 
                             levels = c(paste0("Week ", 1:18), "Playoffs"),
                             ordered = TRUE), 
        total_points = score_home + score_away,
        over_hit = ifelse(total_points > over_under_line, "Over", "Under"),
        home_is_favorite = (team_home == team_favorite_name)
      )
    
    return(df)
  })
  
  # ========== BETTING TRENDS ==========
  filtered_data <- reactive({
    df <- nfl_data()
    
    if (input$bet_type == "fav_correct" && input$factor == "team_home") {
      df <- df %>% filter(home_is_favorite == TRUE)
    }
    
    return(df)
  })
  
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
        title = paste("How Often", 
                      ifelse(input$bet_type == "fav_correct", "Favorites Covered the Spread",
                             ifelse(input$bet_type == "over_hit", "the Over Hit", "the Under Hit")),
                      "by", 
                      case_when(
                        input$factor == "season_type" ~ "Week/Playoff Status",
                        input$factor == "stadium_type" ~ "Stadium Type",
                        input$factor == "temp_category" ~ "Temperature",
                        input$factor == "team_home" ~ "Home Team",
                        TRUE ~ gsub("_", " ", input$factor)
                      )),
        xaxis = list(title = case_when(
          input$factor == "season_type" ~ "Week/Playoff Status",
          input$factor == "stadium_type" ~ "Stadium Type",
          input$factor == "temp_category" ~ "Temperature",
          input$factor == "team_home" ~ "Home Team",
          TRUE ~ gsub("_", " ", input$factor)
        )),
        yaxis = list(title = "Success Rate (%)", range = c(0, 100)),
        shapes = list(
          list(type = "line", x0 = -0.5, x1 = nrow(data) - 0.5, 
               y0 = 50, y1 = 50, 
               line = list(color = "red", dash = "dash", width = 2))
        )
      )
  })
  
  output$table <- renderTable({
    analysis_data()
  })
  
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
  
  # ========== ROI CALCULATOR ==========
  observeEvent(input$roi_strategy, {
    df <- nfl_data()
    
    if (input$roi_strategy == "favorite") {
      teams <- sort(unique(df$team_favorite_name))
    } else if (input$roi_strategy == "underdog") {
      underdog_teams <- df %>%
        mutate(underdog = ifelse(team_home == team_favorite_name, team_away, team_home)) %>%
        pull(underdog) %>%
        unique() %>%
        sort()
      teams <- underdog_teams
    } else {
      teams <- sort(unique(c(df$team_home, df$team_away)))
    }
    
    teams <- teams[!is.na(teams)]
    
    updateSelectInput(session, "roi_team_filter",
                      choices = c("All Teams" = "all", setNames(teams, teams)),
                      selected = "all")
  })
  
  roi_results <- reactive({
    df <- nfl_data()
    
    df <- df %>%
      filter(schedule_season >= input$roi_year_range[1] & 
               schedule_season <= input$roi_year_range[2])
    
    df <- df %>%
      mutate(
        bet_team = case_when(
          input$roi_strategy == "favorite" ~ team_favorite_name,
          input$roi_strategy == "underdog" ~ ifelse(team_home == team_favorite_name, team_away, team_home),
          input$roi_strategy %in% c("over", "under") ~ team_home,
          TRUE ~ NA_character_
        )
      )
    
    if (input$roi_team_filter != "all") {
      df <- df %>% filter(bet_team == input$roi_team_filter)
    }
    
    df <- df %>%
      mutate(
        bet_won = case_when(
          input$roi_strategy == "favorite" ~ !is.na(fav_correct) & (fav_correct == "Yes" | fav_correct == TRUE | fav_correct == 1),
          input$roi_strategy == "underdog" ~ !is.na(fav_correct) & (fav_correct == "No" | fav_correct == FALSE | fav_correct == 0),
          input$roi_strategy == "over" ~ !is.na(over_hit) & (over_hit == "Over"),
          input$roi_strategy == "under" ~ !is.na(over_hit) & (over_hit == "Under"),
          TRUE ~ FALSE
        ),
        odds_multiplier = case_when(
          input$roi_strategy == "underdog" & !is.na(spread_favorite) ~ case_when(
            abs(spread_favorite) <= 3 ~ 1.20,
            abs(spread_favorite) <= 7 ~ 1.80,
            abs(spread_favorite) <= 10 ~ 2.50,
            TRUE ~ 3.50
          ),
          input$roi_strategy == "favorite" & !is.na(spread_favorite) ~ case_when(
            abs(spread_favorite) <= 3 ~ 0.9091,
            abs(spread_favorite) <= 7 ~ 0.6667,
            abs(spread_favorite) <= 10 ~ 0.5000,
            TRUE ~ 0.3333
          ),
          TRUE ~ 0.9091
        ),
        profit = ifelse(bet_won, 
                        input$roi_bet_amount * odds_multiplier, 
                        -input$roi_bet_amount)
      )
    
    df <- df %>% filter(!is.na(profit) & !is.na(bet_won))
    
    df <- df %>%
      arrange(schedule_season, schedule_week) %>%
      mutate(cumulative_profit = cumsum(profit))
    
    return(df)
  })
  
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
  
  output$roi_plot <- renderPlotly({
    df <- roi_results()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No games found for this team/strategy combination"))
    }
    
    df$game_num <- 1:nrow(df)
    
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
  
  output$roi_team_table <- renderTable({
    df <- roi_results()
    
    if (input$roi_strategy %in% c("favorite", "underdog")) {
      if (input$roi_strategy == "favorite") {
        df <- df %>% mutate(bet_team = team_favorite_name)
      } else {
        df <- df %>% mutate(bet_team = ifelse(team_home == team_favorite_name, team_away, team_home))
      }
    } else {
      df <- df %>% mutate(bet_team = team_home)
    }
    
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
      head(10)
    
    team_stats <- team_stats %>%
      mutate(
        Profit = paste0(ifelse(Profit >= 0, "+$", "-$"), 
                        formatC(abs(Profit), format = "f", digits = 0, big.mark = ","))
      )
    
    names(team_stats)[1] <- "Team"
    
    return(team_stats)
  })
  
  # ========== STADIUM MAP ==========
  stadium_images <- reactive({
    data.frame(
      stadium = c("Arrowhead Stadium", "AT&T Stadium", "Lambeau Field", "Gillette Stadium",
                  "Levi's Stadium", "MetLife Stadium", "Mercedes-Benz Stadium", "Soldier Field",
                  "State Farm Stadium", "U.S. Bank Stadium", "Lincoln Financial Field",
                  "M&T Bank Stadium", "Empower Field at Mile High", "Lucas Oil Stadium",
                  "NRG Stadium", "Bank of America Stadium", "Raymond James Stadium",
                  "Acrisure Stadium", "Paul Brown Stadium", "Nissan Stadium",
                  "Ford Field", "Caesars Superdome", "SoFi Stadium", "Highmark Stadium",
                  "Hard Rock Stadium", "Allegiant Stadium", "Lumen Field"),
      image_url = c(
        "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/Arrowhead_Stadium_panorama.jpg/300px-Arrowhead_Stadium_panorama.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f8/Cowboys_Stadium_full_view.jpg/300px-Cowboys_Stadium_full_view.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/4/43/Lambeau_field.jpg/300px-Lambeau_field.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/5/52/Gillette_Stadium02.jpg/300px-Gillette_Stadium02.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Levi%27s_Stadium_exterior.jpg/300px-Levi%27s_Stadium_exterior.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b2/Metlife_Stadium.jpg/300px-Metlife_Stadium.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/6/60/Mercedes-Benz_Stadium_from_Centennial_Olympic_Park.jpg/300px-Mercedes-Benz_Stadium_from_Centennial_Olympic_Park.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/3/39/Soldier_Field.jpg/300px-Soldier_Field.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b9/State_Farm_Stadium_2022.jpg/300px-State_Farm_Stadium_2022.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/U.S._Bank_Stadium_-_West_Facade.jpg/300px-U.S._Bank_Stadium_-_West_Facade.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Lincoln_Financial_Field.jpg/300px-Lincoln_Financial_Field.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d4/M%26T_Bank_Stadium.jpg/300px-M%26T_Bank_Stadium.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/2/28/Empower_Field_at_Mile_High.jpg/300px-Empower_Field_at_Mile_High.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9e/Lucas_Oil_Stadium.jpg/300px-Lucas_Oil_Stadium.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/8/86/NRG_Stadium_outside.jpg/300px-NRG_Stadium_outside.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/Bank_of_America_Stadium.jpg/300px-Bank_of_America_Stadium.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/Raymond_James_Stadium.jpg/300px-Raymond_James_Stadium.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Heinz_Field.jpg/300px-Heinz_Field.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8f/Paul_Brown_Stadium.jpg/300px-Paul_Brown_Stadium.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b9/Nissan_Stadium.jpg/300px-Nissan_Stadium.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Ford_Field.jpg/300px-Ford_Field.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Superdome_2014.jpg/300px-Superdome_2014.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/75/SoFi_Stadium_exterior.jpg/300px-SoFi_Stadium_exterior.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Highmark_Stadium_2021.jpg/300px-Highmark_Stadium_2021.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/1/14/Hard_Rock_Stadium_2019.jpg/300px-Hard_Rock_Stadium_2019.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8c/Allegiant_Stadium_outside.jpg/300px-Allegiant_Stadium_outside.jpg",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1f/Lumen_Field.jpg/300px-Lumen_Field.jpg"
      ),
      stringsAsFactors = FALSE
    )
  })
  
  stadium_coords <- reactive({
    read.csv("../nfl_stadiums_coordinates.csv", stringsAsFactors = FALSE)
  })
  
  stadium_data <- reactive({
    df <- nfl_data()
    coords <- stadium_coords()
    
    df <- df %>%
      filter(schedule_season >= input$stadium_year_range[1],
             schedule_season <= input$stadium_year_range[2])
    
    stadium_stats <- df %>%
      group_by(stadium) %>%
      summarise(
        total_games = n(),
        overs = sum(over_hit == "Over", na.rm = TRUE),
        unders = sum(over_hit == "Under", na.rm = TRUE),
        over_pct = round((overs / total_games) * 100, 1),
        .groups = "drop"
      )
    
    stadium_stats <- stadium_stats %>%
      left_join(coords, by = "stadium") %>%
      filter(!is.na(latitude) & !is.na(longitude))
    
    stadium_stats <- stadium_stats %>%
      left_join(stadium_images(), by = "stadium")
    
    return(stadium_stats)
  })
  
  observe({
    stadiums <- stadium_data()$stadium
    updateSelectInput(session, "stadium_select",
                      choices = c("All Stadiums" = "all", setNames(stadiums, stadiums)))
  })
  
  output$map <- renderLeaflet({
    data <- stadium_data()
    
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = -98, lat = 39, zoom = 4))
    }
    
    if (input$metric == "overs") {
      data$display_count <- data$overs
      metric_label <- "Overs"
      
      color_palette <- colorNumeric(
        palette = c("#3B4CC0", "#7896D8", "#B4D79E", "#F6EB61", "#E8463A"),
        domain = c(min(data$overs), max(data$overs))
      )
      data$circle_color <- color_palette(data$overs)
      legend_title <- "# of Overs"
      legend_values <- data$overs
      
    } else {
      data$display_count <- data$unders
      metric_label <- "Unders"
      
      color_palette <- colorNumeric(
        palette = c("#3B4CC0", "#7896D8", "#B4D79E", "#F6EB61", "#E8463A"),
        domain = c(min(data$unders), max(data$unders))
      )
      data$circle_color <- color_palette(data$unders)
      legend_title <- "# of Unders"
      legend_values <- data$unders
    }
    
    data$popup_html <- sapply(1:nrow(data), function(i) {
      image_tag <- if (!is.na(data$image_url[i]) && data$image_url[i] != "") {
        paste0(
          "<img src='", data$image_url[i], "' ",
          "width='250px' ",
          "style='border-radius: 5px; margin-bottom: 10px; display: block;' ",
          "onerror=\"this.style.display='none'\" ",
          "loading='lazy'><br>"
        )
      } else {
        ""
      }
      
      paste0(
        "<div style='text-align: center; min-width: 200px;'>",
        image_tag,
        "<h4 style='margin: 5px 0;'>", data$stadium[i], "</h4>",
        "<b>Total Games:</b> ", data$total_games[i], "<br>",
        "<b>Overs:</b> ", data$overs[i], " (", data$over_pct[i], "%)<br>",
        "<b>Unders:</b> ", data$unders[i], " (", round(100 - data$over_pct[i], 1), "%)<br>",
        "</div>"
      )
    })
    
    if (input$stadium_select != "all") {
      data <- data %>% filter(stadium == input$stadium_select)
      zoom_level <- 12
      center_lat <- data$latitude[1]
      center_lon <- data$longitude[1]
    } else {
      zoom_level <- 4
      center_lat <- 39
      center_lon <- -98
    }
    
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = center_lon, lat = center_lat, zoom = zoom_level) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~sqrt(display_count) * 3 + 5,
        fillColor = ~circle_color,
        color = "#ffffff",
        weight = 2,
        fillOpacity = 0.7,
        popup = ~popup_html,
        label = ~paste0(stadium, ": ", over_pct, "% Overs (", display_count, " ", metric_label, ")")
      ) %>%
      addLegend(
        position = "bottomright",
        pal = color_palette,
        values = ~legend_values,
        title = legend_title,
        opacity = 0.7
      )
  })
  
  # ========== GAME PREDICTOR ==========
  observe({
    data <- nfl_data()
    teams <- sort(unique(c(data$team_home, data$team_away)))
    teams <- teams[!is.na(teams)]
    
    updateSelectInput(session, "pred_home_team", choices = teams, selected = teams[1])
    updateSelectInput(session, "pred_away_team", choices = teams, selected = teams[2])
  })
  
  observeEvent(c(input$pred_home_team, input$pred_away_team), {
    req(input$pred_home_team, input$pred_away_team)
    
    data <- nfl_data()
    
    matchups <- data %>%
      filter(team_home == input$pred_home_team & team_away == input$pred_away_team)
    
    if (nrow(matchups) > 0) {
      avg_spread <- mean(matchups$spread_favorite, na.rm = TRUE)
      most_common_stadium <- names(sort(table(matchups$stadium_type), decreasing = TRUE))[1]
      
      updateNumericInput(session, "pred_spread", value = round(avg_spread, 1))
      updateSelectInput(session, "pred_stadium", 
                        selected = ifelse(is.na(most_common_stadium), "Outdoor", most_common_stadium))
    } else {
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
  
  prediction_model <- reactive({
    data <- nfl_data()
    
    training_data <- data %>%
      arrange(schedule_season, schedule_week) %>%
      group_by(team_home) %>%
      mutate(
        home_games_played = row_number() - 1,
        home_wins = cumsum(lag(winner == team_home, default = FALSE)),
        home_win_pct = ifelse(home_games_played > 0, home_wins / home_games_played, 0.5),
        home_total_scored = cumsum(lag(score_home, default = 0)),
        home_total_allowed = cumsum(lag(score_away, default = 0)),
        home_avg_scored = ifelse(home_games_played > 0, home_total_scored / home_games_played, 20),
        home_avg_allowed = ifelse(home_games_played > 0, home_total_allowed / home_games_played, 20)
      ) %>%
      ungroup() %>%
      group_by(team_away) %>%
      mutate(
        away_games_played = row_number() - 1,
        away_wins = cumsum(lag(winner == team_away, default = FALSE)),
        away_win_pct = ifelse(away_games_played > 0, away_wins / away_games_played, 0.5),
        away_total_scored = cumsum(lag(score_away, default = 0)),
        away_total_allowed = cumsum(lag(score_home, default = 0)),
        away_avg_scored = ifelse(away_games_played > 0, away_total_scored / away_games_played, 20),
        away_avg_allowed = ifelse(away_games_played > 0, away_total_allowed / away_games_played, 20)
      ) %>%
      ungroup() %>%
      mutate(
        home_won = (winner == team_home),
        is_indoor = (stadium_type == "Indoor"),
        home_strength = home_win_pct * 1.5,
        away_strength = away_win_pct * 1.4,
        home_off_strength = log(home_avg_scored + 1) * 5 * 1.3,
        home_def_strength = 75 - (log(home_avg_allowed + 1) * 5 * 1.2),
        away_off_strength = log(away_avg_scored + 1) * 5 * 1.3,
        away_def_strength = 75 - (log(away_avg_allowed + 1) * 5 * 1.2),
        spread_weighted = case_when(
          is.na(spread_favorite) ~ 0,
          is.na(team_favorite_name) ~ 0,
          team_home == team_favorite_name ~ -abs(spread_favorite),
          team_away == team_favorite_name ~ abs(spread_favorite),
          TRUE ~ 0
        )
      ) %>%
      filter(!is.na(spread_favorite) & !is.na(home_won) & home_games_played > 0 & away_games_played > 0)
    
    model <- randomForest(as.factor(home_won) ~ spread_weighted + is_indoor + schedule_week +
                            home_strength + away_strength + 
                            home_off_strength + home_def_strength + 
                            away_off_strength + away_def_strength,
                          data = training_data,
                          ntree = 500,
                          importance = TRUE,
                          maxnodes = 50)
    
    list(model = model, data = training_data)
  })
  
  prediction_result <- eventReactive(input$predict_btn, {
    model_info <- prediction_model()
    
    home_total_games <- input$pred_home_wins + input$pred_home_losses
    away_total_games <- input$pred_away_wins + input$pred_away_losses
    
    home_win_pct <- if (home_total_games > 0) input$pred_home_wins / home_total_games else 0.5
    away_win_pct <- if (away_total_games > 0) input$pred_away_wins / away_total_games else 0.5
    
    actual_spread <- if (input$pred_home_favored) {
      -abs(input$pred_spread)
    } else {
      abs(input$pred_spread)
    }
    
    new_data <- data.frame(
      spread_weighted = actual_spread * 0.85,
      is_indoor = (input$pred_stadium == "Indoor"),
      schedule_week = input$pred_week,
      home_strength = home_win_pct * 1.5,
      away_strength = away_win_pct * 1.4,
      home_off_strength = log(input$pred_home_avg_scored + 1) * 5 * 1.3,
      home_def_strength = 75 - (log(input$pred_home_avg_allowed + 1) * 5 * 1.2),
      away_off_strength = log(input$pred_away_avg_scored + 1) * 5 * 1.3,
      away_def_strength = 75 - (log(input$pred_away_avg_allowed + 1) * 5 * 1.2)
    )
    
    prob <- predict(model_info$model, newdata = new_data, type = "prob")[, 2]
    
    spread_adjustment <- actual_spread * (0.0075)
    adjusted_prob <- prob + spread_adjustment
    adjusted_prob <- max(0.05, min(0.95, adjusted_prob))
    
    list(
      prob_home = adjusted_prob,
      prob_away = 1 - adjusted_prob,
      home_record = paste0(input$pred_home_wins, "-", input$pred_home_losses),
      away_record = paste0(input$pred_away_wins, "-", input$pred_away_losses)
    )
  })
  
  output$prediction_result <- renderUI({
    result <- prediction_result()
    logos <- team_logos()
    
    home_logo <- logos$logo_url[logos$team == input$pred_home_team]
    away_logo <- logos$logo_url[logos$team == input$pred_away_team]
    
    if (length(home_logo) == 0) home_logo <- ""
    if (length(away_logo) == 0) away_logo <- ""
    
    prob_home <- result$prob_home * 100
    prob_away <- result$prob_away * 100
    
    home_color <- ifelse(prob_home > 50, "#28a745", "#dc3545")
    away_color <- ifelse(prob_away > 50, "#28a745", "#dc3545")
    
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
                   img(src = home_logo, width = "80px", style = "margin-bottom: 10px;"),
                   h3(input$pred_home_team),
                   p(paste0("(", result$home_record, ")"), style = "font-size: 14px; margin: 0;"),
                   h1(paste0(round(prob_home, 1), "%")),
                   p("Win Probability", style = "font-size: 16px;")
               )
        ),
        column(6,
               div(style = paste0("background-color: ", away_color, "; color: white; padding: 30px; border-radius: 10px; text-align: center;"),
                   img(src = away_logo, width = "80px", style = "margin-bottom: 10px;"),
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
  
  output$value_bet_analysis <- renderUI({
    req(input$predict_btn)
    result <- prediction_result()
    
    prob_home <- result$prob_home
    prob_away <- result$prob_away
    
    actual_spread <- if (input$pred_home_favored) {
      -abs(input$pred_spread)
    } else {
      abs(input$pred_spread)
    }
    
    spread <- actual_spread
    
    if (abs(spread) < 1) {
      implied_prob_home <- 0.50
    } else if (abs(spread) < 3) {
      implied_prob_home <- 0.50 + (spread * -0.03)
    } else if (abs(spread) < 7) {
      implied_prob_home <- 0.50 + (spread * -0.035)
    } else if (abs(spread) < 10) {
      implied_prob_home <- 0.50 + (spread * -0.04)
    } else {
      implied_prob_home <- 0.50 + (spread * -0.045)
    }
    
    implied_prob_home <- max(0.05, min(0.95, implied_prob_home))
    
    edge_home <- (prob_home - implied_prob_home) * 100
    edge_away <- (prob_away - (1 - implied_prob_home)) * 100
    
    value_threshold <- 5
    
    if (actual_spread < 0) {
      home_odds <- ifelse(actual_spread < -7, -200, ifelse(actual_spread < -3, -150, -110))
      home_payout <- 100 / (abs(home_odds) / 100)
    } else {
      home_odds <- ifelse(actual_spread > 7, 250, ifelse(actual_spread > 3, 180, 120))
      home_payout <- 100 * (home_odds / 100)
    }
    home_ev <- (prob_home * home_payout) - ((1 - prob_home) * 100)
    
    if (actual_spread > 0) {
      away_odds <- ifelse(actual_spread > 7, -200, ifelse(actual_spread > 3, -150, -110))
      away_payout <- 100 / (abs(away_odds) / 100)
    } else {
      away_odds <- ifelse(actual_spread < -7, 250, ifelse(actual_spread < -3, 180, 120))
      away_payout <- 100 * (away_odds / 100)
    }
    away_ev <- (prob_away * away_payout) - ((1 - prob_away) * 100)
    
    home_has_value <- (edge_home > value_threshold && home_ev > 0)
    away_has_value <- (edge_away > value_threshold && away_ev > 0)
    
    if (!home_has_value && !away_has_value) {
      tagList(
        p(icon("info-circle"), strong(" No Value Bet Found"), style = "font-size: 16px; color: #6c757d;"),
        p("Model prediction does not show a predicted value bet compared to Vegas odds."),
        tags$ul(
          tags$li(paste0(input$pred_home_team, " EV: ", ifelse(home_ev > 0, "+", ""), "$", round(home_ev, 2), " per $100 (Edge: ", ifelse(edge_home > 0, "+", ""), round(edge_home, 1), "%)")),
          tags$li(paste0(input$pred_away_team, " EV: ", ifelse(away_ev > 0, "+", ""), "$", round(away_ev, 2), " per $100 (Edge: ", ifelse(edge_away > 0, "+", ""), round(edge_away, 1), "%)"))
        ),
        p(style = "color: #dc3545; font-weight: bold;", "Negative EV means you would lose money over time on these bets. While negative edge means that Vegas predicts that team to win at a higher probability than the model.")
      )
    } else if (home_has_value && (!away_has_value || home_ev > away_ev)) {
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
        p(strong(paste0("Expected Value: +$", round(home_ev, 2), " per $100 bet"))),
        p(ifelse(home_ev > 15, "Strong value!", ifelse(home_ev > 5, "Good value", "Slight edge")), 
          style = paste0("color: ", ifelse(home_ev > 15, "#28a745", ifelse(home_ev > 5, "#20c997", "#6c757d")), "; font-weight: bold;"))
      )
    } else {
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
        p(strong(paste0("Expected Value: +$", round(away_ev, 2), " per $100 bet"))),
        p(ifelse(away_ev > 15, "Strong value!", ifelse(away_ev > 5, "Good value", "Slight edge")), 
          style = paste0("color: ", ifelse(away_ev > 15, "#28a745", ifelse(away_ev > 5, "#20c997", "#6c757d")), "; font-weight: bold;"))
      )
    }
  })
  
  output$model_accuracy <- renderText({
    req(input$predict_btn)
    model_info <- prediction_model()
    
    predictions <- predict(model_info$model, type = "response")
    actual_winner <- model_info$data$home_won
    
    accuracy <- mean(predictions == actual_winner, na.rm = TRUE) * 100
    paste0(round(accuracy, 1), "%")
  })
  
  output$model_total_games <- renderText({
    req(input$predict_btn)
    formatC(nrow(prediction_model()$data), format = "d", big.mark = ",")
  })
  
  output$model_home_rate <- renderText({
    req(input$predict_btn)
    model_info <- prediction_model()
    home_rate <- mean(model_info$data$home_won, na.rm = TRUE) * 100
    paste0(round(home_rate, 1), "%")
  })
  
  output$importance_plot <- renderPlotly({
    req(input$predict_btn)
    model_info <- prediction_model()
    
    importance_df <- as.data.frame(importance(model_info$model))
    importance_df$Variable <- c("Spread (Weighted)", "Indoor", "Week", "Home Win %", "Away Win %",
                                "Home Avg Scored", "Home Avg Allowed", "Away Avg Scored", "Away Avg Allowed")
    importance_df <- importance_df %>%
      arrange(desc(MeanDecreaseGini)) %>%
      mutate(Importance = MeanDecreaseGini) %>%
      head(8)
    
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
  
  # ========== CLUSTER ANALYSIS ==========
  cluster_team_data <- reactive({
    df <- nfl_data()
    
    team_stats <- df %>%
      group_by(team_home) %>%
      summarise(
        home_games = n(),
        home_wins = sum(score_home > score_away),
        home_points_scored = sum(score_home),
        home_points_allowed = sum(score_away),
        home_overs = sum(over_hit == "Over", na.rm = TRUE),
        home_unders = sum(over_hit == "Under", na.rm = TRUE),
        home_fav_games = sum(team_home == team_favorite_name, na.rm = TRUE),
        home_fav_covers = sum(team_home == team_favorite_name & fav_correct == "Yes", na.rm = TRUE),
        home_dog_games = sum(team_home != team_favorite_name, na.rm = TRUE),
        home_dog_covers = sum(team_home != team_favorite_name & fav_correct == "No", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(team = team_home)
    
    away_stats <- df %>%
      group_by(team_away) %>%
      summarise(
        away_games = n(),
        away_wins = sum(score_away > score_home),
        away_points_scored = sum(score_away),
        away_points_allowed = sum(score_home),
        away_overs = sum(over_hit == "Over", na.rm = TRUE),
        away_unders = sum(over_hit == "Under", na.rm = TRUE),
        away_fav_games = sum(team_away == team_favorite_name, na.rm = TRUE),
        away_fav_covers = sum(team_away == team_favorite_name & fav_correct == "Yes", na.rm = TRUE),
        away_dog_games = sum(team_away != team_favorite_name, na.rm = TRUE),
        away_dog_covers = sum(team_away != team_favorite_name & fav_correct == "No", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(team = team_away)
    
    combined_stats <- team_stats %>%
      full_join(away_stats, by = "team") %>%
      mutate(across(everything(), ~replace_na(.x, 0))) %>%
      mutate(
        total_games = home_games + away_games,
        total_wins = home_wins + away_wins,
        win_pct = round((total_wins / total_games) * 100, 1),
        total_points_scored = home_points_scored + away_points_scored,
        total_points_allowed = home_points_allowed + away_points_allowed,
        avg_points_scored = round(total_points_scored / total_games, 1),
        avg_points_allowed = round(total_points_allowed / total_games, 1),
        point_diff = round((total_points_scored - total_points_allowed) / total_games, 1),
        total_overs = home_overs + away_overs,
        total_unders = home_unders + away_unders,
        over_rate = round((total_overs / (total_overs + total_unders)) * 100, 1),
        under_rate = round((total_unders / (total_overs + total_unders)) * 100, 1),
        total_fav_games = home_fav_games + away_fav_games,
        total_fav_covers = home_fav_covers + away_fav_covers,
        fav_cover_rate = ifelse(total_fav_games > 0, 
                                round((total_fav_covers / total_fav_games) * 100, 1), 
                                NA),
        total_dog_games = home_dog_games + away_dog_games,
        total_dog_covers = home_dog_covers + away_dog_covers,
        dog_cover_rate = ifelse(total_dog_games > 0,
                                round((total_dog_covers / total_dog_games) * 100, 1),
                                NA),
        home_win_rate = round((home_wins / home_games) * 100, 1),
        away_win_rate = round((away_wins / away_games) * 100, 1)
      ) %>%
      filter(total_games >= 10)
    
    return(combined_stats)
  })
  
  # PCA Analysis
  pca_results <- reactive({
    team_data <- cluster_team_data()
    
    # Select numeric columns for PCA
    pca_vars <- c("win_pct", "avg_points_scored", "avg_points_allowed", "point_diff",
                  "over_rate", "fav_cover_rate", "dog_cover_rate", "home_win_rate", "away_win_rate")
    
    # Prepare data - remove rows with NA
    pca_data <- team_data %>%
      select(team, all_of(pca_vars)) %>%
      filter(complete.cases(.))
    
    if (nrow(pca_data) < 5) {
      return(NULL)
    }
    
    # Run PCA
    pca_matrix <- pca_data %>% select(-team) %>% scale()
    pca_fit <- prcomp(pca_matrix, center = FALSE, scale. = FALSE)
    
    # Get scores
    scores <- as.data.frame(pca_fit$x)
    scores$team <- pca_data$team
    
    # Get loadings
    loadings <- as.data.frame(pca_fit$rotation)
    loadings$variable <- rownames(loadings)
    
    # Variance explained
    var_explained <- summary(pca_fit)$importance[2, ] * 100
    
    return(list(
      scores = scores,
      loadings = loadings,
      var_explained = var_explained,
      pca_fit = pca_fit
    ))
  })
  
  # K-means clustering results
  cluster_results <- reactive({
    req(input$cluster_factor1, input$cluster_factor2, input$n_clusters)
    
    team_data <- cluster_team_data()
    
    factors <- c(input$cluster_factor1, input$cluster_factor2)
    
    if (length(unique(factors)) != 2) {
      return(NULL)
    }
    
    cluster_data <- team_data %>%
      select(team, all_of(factors))
    
    cluster_data <- cluster_data %>%
      filter(complete.cases(.))
    
    if (nrow(cluster_data) < input$n_clusters) {
      return(NULL)
    }
    
    cluster_data$x_axis <- cluster_data[[factors[1]]]
    cluster_data$y_axis <- cluster_data[[factors[2]]]
    
    cluster_matrix <- cluster_data %>%
      select(all_of(factors)) %>%
      scale()
    
    set.seed(123)
    kmeans_result <- kmeans(cluster_matrix, centers = input$n_clusters, nstart = 25)
    
    cluster_data$original_cluster <- kmeans_result$cluster
    
    cluster_centers <- cluster_data %>%
      group_by(original_cluster) %>%
      summarise(
        center_x = mean(x_axis),
        center_y = mean(y_axis),
        .groups = "drop"
      ) %>%
      arrange(desc(center_y), desc(center_x)) %>%
      mutate(new_cluster = row_number())
    
    cluster_data <- cluster_data %>%
      left_join(cluster_centers %>% select(original_cluster, new_cluster), 
                by = "original_cluster") %>%
      mutate(cluster = as.factor(new_cluster)) %>%
      select(-original_cluster, -new_cluster)
    
    return(list(
      data = cluster_data,
      kmeans = kmeans_result,
      factors = factors,
      centers = cluster_centers
    ))
  })
  
  # Main cluster/PCA plot
  output$cluster_plot <- renderPlotly({
    
    if (input$analysis_method == "pca") {
      # PCA Plot
      results <- pca_results()
      
      if (is.null(results)) {
        return(plot_ly() %>% 
                 layout(title = "Not enough data for PCA analysis"))
      }
      
      scores <- results$scores
      var_explained <- results$var_explained
      
      plot_ly(scores, x = ~PC1, y = ~PC2,
              type = 'scatter', mode = 'markers+text',
              text = ~team,
              textposition = 'top center',
              marker = list(size = 12, color = '#1f77b4'),
              hovertext = ~paste0(
                "<b>", team, "</b><br>",
                "PC1: ", round(PC1, 2), "<br>",
                "PC2: ", round(PC2, 2)
              ),
              hoverinfo = 'text') %>%
        layout(
          title = paste0("PCA: Teams by Principal Components"),
          xaxis = list(title = paste0("PC1 (", round(var_explained[1], 1), "% variance)")),
          yaxis = list(title = paste0("PC2 (", round(var_explained[2], 1), "% variance)")),
          showlegend = FALSE
        )
      
    } else {
      # K-means Plot
      results <- cluster_results()
      
      if (is.null(results)) {
        return(plot_ly() %>% 
                 layout(title = "Please select 2 different factors or ensure enough data is available"))
      }
      
      data <- results$data
      factors <- results$factors
      
      factor_labels <- c(
        "win_pct" = "Win %",
        "avg_points_scored" = "Avg Points Scored",
        "avg_points_allowed" = "Avg Points Allowed",
        "point_diff" = "Point Differential",
        "over_rate" = "Over Hit %",
        "under_rate" = "Under Hit %",
        "fav_cover_rate" = "Favorite Cover %",
        "dog_cover_rate" = "Underdog Cover %",
        "home_win_rate" = "Home Win %",
        "away_win_rate" = "Away Win %"
      )
      
      x_label <- factor_labels[factors[1]]
      y_label <- factor_labels[factors[2]]
      title_text <- paste("Team Clusters by", x_label, "vs", y_label)
      
      plot_ly(data, x = ~x_axis, y = ~y_axis, color = ~cluster,
              type = 'scatter', mode = 'markers+text',
              text = ~team,
              textposition = 'top center',
              marker = list(size = 12),
              hovertext = ~paste0(
                "<b>", team, "</b><br>",
                "Cluster: ", cluster, "<br>",
                x_label, ": ", round(x_axis, 1), "<br>",
                y_label, ": ", round(y_axis, 1)
              ),
              hoverinfo = 'text') %>%
        layout(
          title = title_text,
          xaxis = list(title = x_label),
          yaxis = list(title = y_label),
          showlegend = TRUE
        )
    }
  })
  
  # PCA Loadings plot
  output$pca_loadings_plot <- renderPlotly({
    results <- pca_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    loadings <- results$loadings
    
    # Pretty labels
    var_labels <- c(
      "win_pct" = "Win %",
      "avg_points_scored" = "Avg Pts Scored",
      "avg_points_allowed" = "Avg Pts Allowed",
      "point_diff" = "Point Diff",
      "over_rate" = "Over Rate",
      "fav_cover_rate" = "Fav Cover %",
      "dog_cover_rate" = "Dog Cover %",
      "home_win_rate" = "Home Win %",
      "away_win_rate" = "Away Win %"
    )
    
    loadings$label <- var_labels[loadings$variable]
    
    plot_ly(loadings, x = ~PC1, y = ~PC2,
            type = 'scatter', mode = 'markers+text',
            text = ~label,
            textposition = 'top center',
            marker = list(size = 10, color = '#d62728'),
            hovertext = ~paste0(
              "<b>", label, "</b><br>",
              "PC1 Loading: ", round(PC1, 3), "<br>",
              "PC2 Loading: ", round(PC2, 3)
            ),
            hoverinfo = 'text') %>%
      layout(
        title = "PCA Loadings (Variable Contributions)",
        xaxis = list(title = "PC1 Loading", zeroline = TRUE),
        yaxis = list(title = "PC2 Loading", zeroline = TRUE),
        showlegend = FALSE
      )
  })
  
  # Variance explained table
  output$pca_variance_table <- renderTable({
    results <- pca_results()
    
    if (is.null(results)) {
      return(data.frame(Message = "Not enough data for PCA"))
    }
    
    var_exp <- results$var_explained
    
    data.frame(
      Component = paste0("PC", 1:min(5, length(var_exp))),
      `Variance Explained` = paste0(round(var_exp[1:min(5, length(var_exp))], 1), "%"),
      `Cumulative` = paste0(round(cumsum(var_exp)[1:min(5, length(var_exp))], 1), "%"),
      check.names = FALSE
    )
  })
  
  output$cluster_summary_table <- renderTable({
    if (input$analysis_method == "pca") {
      return(NULL)
    }
    
    results <- cluster_results()
    
    if (is.null(results)) {
      return(data.frame(Message = "Please select 2 different factors"))
    }
    
    data <- results$data
    factors <- results$factors
    
    summary <- data %>%
      group_by(cluster) %>%
      summarise(
        Teams = n(),
        across(all_of(factors), ~round(mean(.x, na.rm = TRUE), 1)),
        .groups = "drop"
      ) %>%
      arrange(as.numeric(as.character(cluster)))
    
    names(summary)[1] <- "Cluster"
    
    return(summary)
  })
  
  output$cluster_teams_table <- renderTable({
    if (input$analysis_method == "pca") {
      # For PCA, show teams sorted by PC1
      results <- pca_results()
      
      if (is.null(results)) {
        return(data.frame(Message = "Not enough data for PCA"))
      }
      
      scores <- results$scores %>%
        arrange(desc(PC1)) %>%
        mutate(
          PC1 = round(PC1, 2),
          PC2 = round(PC2, 2)
        ) %>%
        select(team, PC1, PC2)
      
      names(scores) <- c("Team", "PC1 Score", "PC2 Score")
      
      return(scores)
    }
    
    results <- cluster_results()
    
    if (is.null(results)) {
      return(data.frame(Message = "Please select 2 different factors"))
    }
    
    data <- results$data
    
    teams_by_cluster <- data %>%
      arrange(as.numeric(as.character(cluster)), team) %>%
      group_by(cluster) %>%
      summarise(
        Teams = paste(team, collapse = ", "),
        .groups = "drop"
      )
    
    names(teams_by_cluster)[1] <- "Cluster"
    
    return(teams_by_cluster)
  })
  
  # Dynamic title for teams table
  output$teams_table_title <- renderUI({
    if (input$analysis_method == "pca") {
      "Team PCA Scores"
    } else {
      "Teams by Cluster"
    }
  })
  
  # Navigate to stadium map
  observeEvent(input$go_to_map, {
    updateTabItems(session, "tabs", "analytics")
  })
  
  # Download handlers
  output$download_betting_data <- downloadHandler(
    filename = function() {
      paste0("nfl_betting_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(analysis_data(), file, row.names = FALSE)
    }
  )
  
  output$download_roi_data <- downloadHandler(
    filename = function() {
      paste0("nfl_roi_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- roi_results() %>%
        select(schedule_season, schedule_week, team_home, team_away, 
               bet_won, profit, cumulative_profit)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_cluster_data <- downloadHandler(
    filename = function() {
      paste0("nfl_cluster_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      results <- cluster_results()
      if (!is.null(results)) {
        write.csv(results$data, file, row.names = FALSE)
      }
    }
  )
  
  # ========== BETTING LIBRARY ==========
  # Helper function to create empty bets data frame
  create_empty_bets <- function() {
    data.frame(
      id = character(),
      date = character(),
      is_parlay = logical(),
      num_legs = numeric(),
      home_team = character(),
      away_team = character(),
      bet_type = character(),
      wagered = numeric(),
      won = numeric(),
      lost = numeric(),
      net = numeric(),
      result = character(),
      notes = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # Initialize betting library storage
  betting_library <- reactiveValues(
    bets = create_empty_bets()
  )
  
  # Load data from localStorage on startup
  observe({
    # Load betting library
    if (!is.null(input$store$betting_library_data)) {
      stored_bets <- input$store$betting_library_data
      
      if (length(stored_bets) > 0 && !is.null(stored_bets[[1]])) {
        tryCatch({
          betting_library$bets <- do.call(rbind, lapply(stored_bets, function(x) {
            as.data.frame(lapply(x, function(y) if (is.null(y)) NA else y), stringsAsFactors = FALSE)
          }))
        }, error = function(e) {
          message("Error loading stored bets: ", e$message)
        })
      }
    }
    
    # Load other stored settings
    if (!is.null(input$store$stadium_metric)) {
      updateRadioButtons(session, "metric", selected = input$store$stadium_metric)
    }
    if (!is.null(input$store$stadium_year_range)) {
      updateSliderInput(session, "stadium_year_range", value = input$store$stadium_year_range)
    }
    if (!is.null(input$store$stadium_select)) {
      updateSelectInput(session, "stadium_select", selected = input$store$stadium_select)
    }
    if (!is.null(input$store$bet_analysis_type)) {
      updateSelectInput(session, "bet_type", selected = input$store$bet_analysis_type)
    }
    if (!is.null(input$store$bet_analysis_factor)) {
      updateSelectInput(session, "factor", selected = input$store$bet_analysis_factor)
    }
    if (!is.null(input$store$roi_strategy)) {
      updateSelectInput(session, "roi_strategy", selected = input$store$roi_strategy)
    }
    if (!is.null(input$store$roi_bet_amount)) {
      updateSliderInput(session, "roi_bet_amount", value = input$store$roi_bet_amount)
    }
    if (!is.null(input$store$roi_year_range)) {
      updateSliderInput(session, "roi_year_range", value = input$store$roi_year_range)
    }
    if (!is.null(input$store$roi_team_filter)) {
      updateSelectInput(session, "roi_team_filter", selected = input$store$roi_team_filter)
    }
    if (!is.null(input$store$cluster_factor1)) {
      updateSelectInput(session, "cluster_factor1", selected = input$store$cluster_factor1)
    }
    if (!is.null(input$store$cluster_factor2)) {
      updateSelectInput(session, "cluster_factor2", selected = input$store$cluster_factor2)
    }
    if (!is.null(input$store$n_clusters)) {
      updateSliderInput(session, "n_clusters", value = input$store$n_clusters)
    }
  })
  
  # Save betting library whenever it changes
  observeEvent(betting_library$bets, {
    if (nrow(betting_library$bets) > 0) {
      data_list <- lapply(1:nrow(betting_library$bets), function(i) {
        as.list(betting_library$bets[i, ])
      })
      updateStore(session, "betting_library_data", data_list)
    } else {
      # Explicitly save empty list when no bets
      updateStore(session, "betting_library_data", list())
    }
  }, ignoreInit = TRUE)
  
  # Save other settings
  observeEvent(input$metric, {
    updateStore(session, "stadium_metric", input$metric)
  }, ignoreInit = TRUE)
  
  observeEvent(input$stadium_year_range, {
    updateStore(session, "stadium_year_range", input$stadium_year_range)
  }, ignoreInit = TRUE)
  
  observeEvent(input$stadium_select, {
    updateStore(session, "stadium_select", input$stadium_select)
  }, ignoreInit = TRUE)
  
  observeEvent(input$bet_type, {
    updateStore(session, "bet_analysis_type", input$bet_type)
  }, ignoreInit = TRUE)
  
  observeEvent(input$factor, {
    updateStore(session, "bet_analysis_factor", input$factor)
  }, ignoreInit = TRUE)
  
  observeEvent(input$roi_strategy, {
    updateStore(session, "roi_strategy", input$roi_strategy)
  }, ignoreInit = TRUE)
  
  observeEvent(input$roi_bet_amount, {
    updateStore(session, "roi_bet_amount", input$roi_bet_amount)
  }, ignoreInit = TRUE)
  
  observeEvent(input$roi_year_range, {
    updateStore(session, "roi_year_range", input$roi_year_range)
  }, ignoreInit = TRUE)
  
  observeEvent(input$roi_team_filter, {
    updateStore(session, "roi_team_filter", input$roi_team_filter)
  }, ignoreInit = TRUE)
  
  observeEvent(input$cluster_factor1, {
    updateStore(session, "cluster_factor1", input$cluster_factor1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$cluster_factor2, {
    updateStore(session, "cluster_factor2", input$cluster_factor2)
  }, ignoreInit = TRUE)
  
  observeEvent(input$n_clusters, {
    updateStore(session, "n_clusters", input$n_clusters)
  }, ignoreInit = TRUE)
  
  # Get all bets
  all_bets <- reactive({
    if (nrow(betting_library$bets) == 0) {
      return(create_empty_bets())
    }
    betting_library$bets %>% arrange(desc(date))
  })
  
  # Add new bet
  observeEvent(input$lib_add_bet, {
    req(input$lib_amount_wagered)
    
    # Validation for single bets
    if (!input$lib_is_parlay) {
      req(input$lib_home_team, input$lib_away_team, input$lib_bet_type)
      if (input$lib_home_team == "" || input$lib_away_team == "") {
        showModal(modalDialog(
          title = "Error",
          "Please select both home and away teams.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
    } else {
      # Validation for parlays
      req(input$lib_num_legs, input$lib_parlay_details)
      if (input$lib_parlay_details == "") {
        showModal(modalDialog(
          title = "Error",
          "Please enter parlay details.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
    }
    
    if (input$lib_amount_won == 0 && input$lib_amount_lost == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please enter either Amount Won or Amount Lost (not both zero).",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    net_result <- input$lib_amount_won - input$lib_amount_lost
    result_status <- ifelse(net_result > 0, "Won", 
                            ifelse(net_result < 0, "Lost", "Push"))
    
    bet_id <- paste0("bet_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
    
    if (!input$lib_is_parlay) {
      new_bet <- data.frame(
        id = bet_id,
        date = as.character(Sys.Date()),
        is_parlay = FALSE,
        num_legs = 1,
        home_team = input$lib_home_team,
        away_team = input$lib_away_team,
        bet_type = input$lib_bet_type,
        wagered = input$lib_amount_wagered,
        won = input$lib_amount_won,
        lost = input$lib_amount_lost,
        net = net_result,
        result = result_status,
        notes = input$lib_notes,
        stringsAsFactors = FALSE
      )
    } else {
      new_bet <- data.frame(
        id = bet_id,
        date = as.character(Sys.Date()),
        is_parlay = TRUE,
        num_legs = input$lib_num_legs,
        home_team = "PARLAY",
        away_team = "",
        bet_type = paste0(input$lib_num_legs, "-Leg Parlay"),
        wagered = input$lib_amount_wagered,
        won = input$lib_amount_won,
        lost = input$lib_amount_lost,
        net = net_result,
        result = result_status,
        notes = paste0("Legs: ", input$lib_parlay_details, 
                       ifelse(input$lib_notes != "", paste0(" | Notes: ", input$lib_notes), "")),
        stringsAsFactors = FALSE
      )
    }
    
    betting_library$bets <- rbind(betting_library$bets, new_bet)
    
    # Clear form
    updateCheckboxInput(session, "lib_is_parlay", value = FALSE)
    updateSelectInput(session, "lib_home_team", selected = "")
    updateSelectInput(session, "lib_away_team", selected = "")
    updateNumericInput(session, "lib_amount_wagered", value = 100)
    updateNumericInput(session, "lib_amount_won", value = 0)
    updateNumericInput(session, "lib_amount_lost", value = 0)
    updateTextAreaInput(session, "lib_notes", value = "")
    updateTextAreaInput(session, "lib_parlay_details", value = "")
    
    showNotification("Bet added successfully!", type = "message")
  })
  
  # Clear all bets - Show confirmation modal
  observeEvent(input$lib_clear_all, {
    showModal(modalDialog(
      title = "Confirm Delete",
      "Are you sure you want to delete ALL bets? This cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("lib_confirm_clear", "Yes, Delete All", class = "btn-danger")
      )
    ))
  })
  
  # Actually clear all bets when confirmed - FIXED
  observeEvent(input$lib_confirm_clear, {
    # Clear the reactive values
    betting_library$bets <- create_empty_bets()
    
    # Explicitly clear the store
    updateStore(session, "betting_library_data", list())
    
    removeModal()
    showNotification("All bets cleared!", type = "warning")
  })
  
  # Summary Cards
  output$lib_total_bets <- renderText({
    bets <- all_bets()
    formatC(nrow(bets), format = "d", big.mark = ",")
  })
  
  output$lib_total_wagered <- renderText({
    bets <- all_bets()
    if (nrow(bets) == 0) return("$0")
    total <- sum(bets$wagered, na.rm = TRUE)
    paste0("$", formatC(total, format = "f", digits = 0, big.mark = ","))
  })
  
  output$lib_net_profit <- renderText({
    bets <- all_bets()
    if (nrow(bets) == 0) return("$0")
    net <- sum(bets$net, na.rm = TRUE)
    sign <- ifelse(net >= 0, "+", "")
    paste0(sign, "$", formatC(net, format = "f", digits = 0, big.mark = ","))
  })
  
  output$lib_win_rate <- renderText({
    bets <- all_bets()
    if (nrow(bets) == 0) return("0%")
    wins <- sum(bets$result == "Won", na.rm = TRUE)
    paste0(round((wins / nrow(bets)) * 100, 1), "%")
  })
  
  output$lib_streak <- renderText({
    bets <- all_bets()
    if (nrow(bets) == 0) return("No bets")
    
    bets <- bets %>% arrange(desc(date))
    
    current_streak <- 0
    last_result <- bets$result[1]
    
    for (i in 1:nrow(bets)) {
      if (bets$result[i] == last_result && last_result %in% c("Won", "Lost")) {
        current_streak <- current_streak + 1
      } else {
        break
      }
    }
    
    if (current_streak == 0) return("No streak")
    
    streak_type <- ifelse(last_result == "Won", "W", "L")
    paste0(current_streak, streak_type)
  })
  
  output$lib_best_type <- renderText({
    bets <- all_bets()
    if (nrow(bets) == 0) return("N/A")
    
    type_performance <- bets %>%
      group_by(bet_type) %>%
      summarise(
        total_net = sum(net, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_net))
    
    if (nrow(type_performance) == 0) return("N/A")
    
    type_performance$bet_type[1]
  })
  
  output$lib_avg_bet <- renderText({
    bets <- all_bets()
    if (nrow(bets) == 0) return("$0")
    avg <- mean(bets$wagered, na.rm = TRUE)
    paste0("$", formatC(avg, format = "f", digits = 0, big.mark = ","))
  })
  
  output$lib_roi <- renderText({
    bets <- all_bets()
    if (nrow(bets) == 0) return("0%")
    total_wagered <- sum(bets$wagered, na.rm = TRUE)
    total_net <- sum(bets$net, na.rm = TRUE)
    if (total_wagered == 0) return("0%")
    roi <- (total_net / total_wagered) * 100
    sign <- ifelse(roi >= 0, "+", "")
    paste0(sign, round(roi, 1), "%")
  })
  
  # Bets Table
  output$lib_bets_table <- DT::renderDataTable({
    bets <- all_bets()
    
    if (nrow(bets) == 0) {
      return(DT::datatable(
        data.frame(Message = "No bets added yet. Add your first bet!"),
        options = list(
          dom = 't',
          searching = FALSE,
          ordering = FALSE,
          paging = FALSE
        ),
        rownames = FALSE
      ))
    }
    
    display_bets <- bets %>%
      mutate(
        parlay_display = ifelse(is_parlay, "Yes", "No"),
        type_display = ifelse(is_parlay, paste0("🎲 ", bet_type), bet_type),
        wagered_display = paste0("$", formatC(wagered, format = "f", digits = 0, big.mark = ",")),
        won_display = paste0("$", formatC(won, format = "f", digits = 0, big.mark = ",")),
        lost_display = paste0("$", formatC(lost, format = "f", digits = 0, big.mark = ",")),
        net_display = paste0(ifelse(net >= 0, "+", ""), "$", formatC(net, format = "f", digits = 0, big.mark = ","))
      ) %>%
      select(date, parlay_display, home_team, away_team, type_display, wagered_display, won_display, lost_display, net_display, result, notes)
    
    names(display_bets) <- c("Date", "Parlay?", "Home", "Away", "Type", "Wagered", "Won", "Lost", "Net", "Result", "Notes")
    
    DT::datatable(
      display_bets,
      options = list(
        pageLength = 10,
        dom = 'frtip',
        scrollX = TRUE,
        searching = TRUE,
        ordering = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Export CSV
  output$lib_export_csv <- downloadHandler(
    filename = function() {
      paste0("my_betting_library_", Sys.Date(), ".csv")
    },
    content = function(file) {
      bets <- all_bets()
      if (nrow(bets) > 0) {
        export_data <- bets %>%
          select(date, home_team, away_team, bet_type, wagered, won, lost, net, result, notes)
        write.csv(export_data, file, row.names = FALSE)
      }
    }
  )
}