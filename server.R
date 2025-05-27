server <- function(input, output, session) {


  output$introSection <- renderUI({
    intro_text
  })



  output$countryMap <- renderLeaflet({

    map_sf <- map_sf %>%
      mutate(
        label =
          paste0(
            "<b>", country, "</b><br/>",
            "<small><i>League:</i> ", name, "</small>"

        )
      ) %>%
      st_set_geometry("geometry")

    library(viridis)

    pal <- colorFactor(
      palette = viridis(length(unique(map_sf$country))),
      domain  = map_sf$country
    )


    leaflet(map_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor   = ~ pal(country),
        weight      = 1,
        color       = "#444",
        fillOpacity = 0.7,
        layerId     = ~id,
        label = ~lapply(label, HTML),
        labelOptions = labelOptions(
          direction = "auto",
          textsize  = "16px",
          opacity   = 0.9,
          offset    = c(0, 0)
        )
      ) %>%
      addCircleMarkers(
        lng = 2.1228, lat = 41.3809,
        label = "FC Barcelona (Camp Nou)",
        color = "red", radius = 6, fillOpacity = 1,
        labelOptions = labelOptions(
          direction = "auto",
          textsize  = "16px",
          opacity   = 0.9,
          offset    = c(0, 0)
        )
      ) %>%
      setView(
        lng  = 10,
        lat  = 48,
        zoom = 4.4
      )
  })

  # Track which country is selected on the map
  selectedCountry <- reactiveVal(NULL)
  observeEvent(input$countryMap_shape_click, {
    selectedCountry(input$countryMap_shape_click$id)
  })
  observeEvent(input$countryMap_shape_mouseover, {
    selectedCountry(input$countryMap_shape_mouseover$id)
  })

  #  Reactive: teams in that country_id
  countryTeams <- reactive({
    req(selectedCountry())
    # team_country
    #   country_id, name (league name), team_long_name, team_short_name
    team_country[country_id == selectedCountry(),
                 .(League       = name,
                   Team         = team_long_name,
                   Short        = team_short_name)]
  })

  # 3) Render as a DataTable
  output$countryTeams <- renderDT({
    datatable(
      countryTeams(),
      rownames = FALSE,
      options  = list(
        pageLength = 10,
        scrollY    = "300px",
        dom        = "tp"
      )
    )
  })







    #
    filtered_barca <- reactive({
      if (input$homeAwayOpp == "both") {
        barca
      } else {
        barca[home_away == input$homeAwayOpp]
      }
    })

    #
    summary_opponents <- reactive({
      dt <- filtered_barca()[ ,
                              .(
                                games  = .N,
                                wins   = sum(result ==  1),
                                draws  = sum(result ==  0),
                                losses = sum(result == -1)
                              ),
                              by = .(team_played_against, team_long_name)
      ]
      dt[ , win_rate := wins / games * 100 ]
      dt <- dt[games>5]
    })

    #
    selected_opponents <- reactive({
      dt <- summary_opponents()
      if (input$oppStrength == "Strongest") {
        dt <- dt[order(win_rate)]
      } else {
        dt <- dt[order( -win_rate)]
      }

      head(dt, 4)
    })

    #
    plot_dt <- reactive({
      dt <- selected_opponents()
      melted <- melt(
        dt,
        id.vars     = c("team_played_against", "team_long_name", "games"),
        measure.vars= c("wins", "draws", "losses"),
        variable.name = "result_label",
        value.name    = "n"
      )
      #
      melted[ , result_label := factor(result_label,
                                       levels = c("wins","draws","losses"),
                                       labels = c("Win","Draw","Loss")
      )]
      melted[ , pct := round(n / games * 100) ]
      melted
    })

    # ── 5. Expand into a 10×10 grid of “cells” for waffle chart ─────────────
    cells <- reactive({
      plot_dt()[ , {
        reps  <- rep(result_label, times = pct)
        ncell <- length(reps)
        data.table(
          team          = team_long_name[1],
          result        = reps,
          percentage    = rep(pct, times = pct),
          total_matches = rep(games, times = ncell),
          col           = rep(1:10, length.out = ncell),
          row           = rep(1:10, each = 10)[1:ncell]
        )
      }, by = .(team_played_against, team_long_name)]
    })

    # ── 6. Render interactive waffle‐style Plotly chart ──────────────────────
    output$oppFacet <- renderPlotly({
      d <- cells()
      p <- ggplot(d, aes(
        x = col, y = row,
        fill = result,
        text = paste0(
          "<b>", team, "</b><br>",
          result, ": ", percentage, "%<br>",
          "Total matches: ", total_matches
        )
      )) +
        geom_tile(colour = "white") +
        scale_fill_manual(values = c(
          Win  = "#2ECC71",
          Draw = "#F1C40F",
          Loss = "#E74C3C"
        )) +
        facet_wrap(~ team, ncol = 2) +
        coord_equal() +
        theme_minimal() +
        theme(
          axis.text      = element_blank(),
          axis.title     = element_blank(),
          panel.grid     = element_blank(),
          strip.text     = element_text(face = "bold"),
          legend.position = "bottom"
        ) +
        labs(fill = "Result")

      ggplotly(p, tooltip = "text")
    })





  # Reactive expression for most improved teams:

  mostImprovedTeams <- reactive({
    # Filter based on home/away choice
    if (input$homeAwayChoice == "both") {
      big_improvers_dt
    } else {
      big_improvers_dt[home_away == input$homeAwayChoice]
    }
  })


  improvedTeamsdf <- reactive({

    big_improvers_dt_sum <- mostImprovedTeams()[ , .(
      games = .N,
      wins   = sum(result == 1),
      goals_for     = sum(goals_for),
      goals_against = sum(goals_against)
    ), by = .(team_long_name, team_api_id, season)]

    big_improvers_dt_sum[, win_rate := wins / games * 100]
    big_improvers_dt_sum

  })


  output$trendPlot <- renderPlotly({
    # Get the data for the selected teams
    dt <- improvedTeamsdf()

    # Create the plot
    p <- ggplot(dt, aes(x = season, y = win_rate,
                        color = team_long_name,
                        group = team_long_name,
                        text = paste("Team:", team_long_name,
                                     "<br>Season:", season,
                                     "<br>Win Rate:", round(win_rate, 1), "%"
                        ))) +
      geom_line() +
      geom_point() +
      labs(title = "Win Rate Trend of Most Improved Teams",
           x = "Season",
           y = "Win Rate (%)") +
      theme_minimal()

    # Convert to Plotly object
    ggplotly(p, tooltip = "text")
  })




  roleSelected <- reactive({
    # Position filter logic
    role_map <- list(
      "GK" = "goalkeeper",
      "DEF" = "defender",
      "MID" = "midfielder",
      "FWD" = "forward",
      "Overall" = "general"
    )
    role_map[[input$posFilter]]


  })




  # DATA for lollipop (top-N youngsters)
  lolliData <-  reactive({
    dt <- player_attributes_melted[age <= input$maxAge &
                                     role == roleSelected() &
                                     between(mean_rate,
                                            input$ratingRange[1],
                                            input$ratingRange[2])]
    dt
  })

  #  DATA for radar (melted + scaled)
  radarData <- reactive({
    focus      <- roleSelected()
    role_vars  <- position_attribute_map[[focus]]
    dt     <- lolliData()
    top_player_nms <- dt[order(-mean_rate)][1:10, player_name]

    melt(player_attributes_merge[player_name %in% top_player_nms,
                                 c("player_name", role_vars), with = FALSE],
         id.vars       = "player_name",
         variable.name = "attribute",
         value.name    = "score")[
           , scaled := scales::rescale(score, to = c(0, 100)), by = attribute]
  })



    output$talentPlot <- renderPlotly({
      dt <- lolliData()

      top_dt <- dt[order(-mean_rate)][1:20]

      p <- ggplot(top_dt, aes(x = reorder(player_name, mean_rate),
                              y = mean_rate, text = paste(player_name,
                                                         "<br>Age:", round(age),
                                                         "<br>Rating:", round(mean_rate, 1)))) +
        geom_segment(aes(xend = player_name, y = 0, yend = mean_rate), color = "gray") +
        geom_point(color = "steelblue", size = 3) +
        coord_flip() +
        labs(x = "Player", y = "Mean Attribute Rating",
             title = "Top 20 Young Players") +
        theme_minimal()

      ggplotly(p, tooltip = "text")
    })




  output$radarTalentPlot <- renderPlotly({
    focus <- roleSelected()

    plot_ly(
      data   = radarData(),
      type   = "scatterpolar",
      mode   = "lines+markers",
      fill   = "toself",
      r      = ~scaled,
      theta  = ~attribute,
      split  = ~player_name,
      text   = ~paste(player_name,
                      "<br>", attribute, ": ", round(score, 1)),
      hoverinfo = "text"
    ) |>
      layout(
        title  = paste("Attribute profile – top", focus, "prospects"),
        polar  = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
        legend = list(orientation = "h", x = 0.1, y = -0.2)
      )
  })

## NEW: INTEGRATION OF POSSESSION APP
filtered_matches_possession <- reactive({
  
  if (!exists("match_table_possession") || nrow(match_table_possession) == 0) { 
    return(data.table()) # Return empty table if no data 
  } 
  matches_df <- copy(match_table_possession) 
  
  # Filter by league (input$league_possession) 
  if (exists("input$league_possession") && input$league_possession != "All" && "league" %in% 
      names(matches_df)) { 
    matches_df <- matches_df[league == input$league_possession] 
  } 
  
  # Filter by season_range (input$season_range_possession) 
  if (exists("input$season_range_possession") && "season_year" %in% names(matches_df)) { 
    matches_df <- matches_df[season_year >= input$season_range_possession[1] & season_year <= 
                               input$season_range_possession[2]] 
  } 
  
  # Filter by win_margin (input$win_margin_possession) 
  if (exists("input$win_margin_possession") && "win_margin" %in% names(matches_df)) { 
    matches_df <- matches_df[win_margin >= input$win_margin_possession[1] & win_margin <= 
                               input$win_margin_possession[2]] 
  } 
  matches_df 
}) 

output$possessionOutcomePlot <- renderPlotly({ 
  matches_to_plot <- filtered_matches_possession() 
  
  if (nrow(matches_to_plot) == 0 || !"possession_diff" %in% names(matches_to_plot)) { 
    # Return a blank plot or a message if no data 
    return(plot_ly() %>% layout(title = "No data available for selected filters")) 
  } 
  
  matches_to_plot <- na.omit(matches_to_plot, cols = "possession_diff") 
  if (nrow(matches_to_plot) == 0) { 
    return(plot_ly() %>% layout(title = "No data after removing missing possession values")) 
  } 
  
  stats <- matches_to_plot %>% 
    group_by(outcome) %>% 
    summarize( 
      Median = median(possession_diff, na.rm = TRUE), 
      Minimum = min(possession_diff, na.rm = TRUE), 
      Maximum = max(possession_diff, na.rm = TRUE), 
      .groups = "drop" 
    ) 
  
  matches_to_plot <- merge(matches_to_plot, stats, by = "outcome", all.x = TRUE) # Use all.x = TRUE 
  
  Q1 <- matches_to_plot %>% group_by(outcome) %>% summarise(Q1 = quantile(possession_diff, 0.25, na.rm 
                                                                          = TRUE), .groups="drop") 
  Q3 <- matches_to_plot %>% group_by(outcome) %>% summarise(Q3 = quantile(possession_diff, 0.75, na.rm 
                                                                          = TRUE), .groups="drop") 
  
  # Merge 
  if (nrow(Q1)>0 && nrow(Q3)>0) { 
    IQR_stats <- merge(Q1, Q3, by = "outcome") %>% mutate(IQR_val = Q3 - Q1)
    matches_to_plot <- merge(matches_to_plot, IQR_stats, by = "outcome", all.x = TRUE) 
    matches_to_plot[, is_outlier := possession_diff < (Q1 - 1.5 * IQR_val) | possession_diff > (Q3 + 1.5 * IQR_val)] 
  } else { 
    matches_to_plot[, is_outlier := FALSE] # Default if IQR cannot be calculated 
  } 
  
  
  matches_to_plot[, hover_text := { 
    base_text <- paste( 
      "League:", league, "<br>", 
      "Season:", season_year, "<br>",
      "Home Team:", home_team_name, "<br>", 
      "Away Team:", away_team_name, "<br>", 
      "Match Outcome:", outcome, "<br>", 
      "Possession Difference:", round(possession_diff, 1), "%<br>", 
      "Home Team Goals:", home_team_goal, "<br>", 
      "Away Team Goals:", away_team_goal, "<br>", 
      "Goal Difference:", win_margin, "<br>" 
    ) 
    # Conditionally add Median, Min, Max if they exist for the outcome 
    median_text <- if(!is.na(Median)) paste("Median:", round(Median, 1), "%<br>") else "" 
    min_text <- if(!is.na(Minimum)) paste("Minimum:", round(Minimum, 1), "%<br>") else "" 
    max_text <- if(!is.na(Maximum)) paste("Maximum:", round(Maximum, 1), "%<br>") else "" 
    
    paste0(base_text, median_text, min_text, max_text) 
  }, by = 1:nrow(matches_to_plot)] # Apply row by row 
  
  
  if (input$plot_type_possession == "Violin Plot") { 
    p <- plot_ly(data = matches_to_plot, x = ~outcome, y = ~possession_diff, type = "violin", 
                 color = ~outcome, colors = c("Home Win" = "green", "Away Win" = "red", "Draw" = "blue"), 
                 hoverinfo = "none", 
                 showlegend = FALSE, 
                 box = list(visible = TRUE), # Show box plot within violin 
                 points = "outliers" # Show outliers 
    ) %>% 
      add_trace( # Scatter trace for custom hover text on all points 
        type = "scatter", 
        mode = "markers", 
        x = ~jitter(as.numeric(factor(outcome))), # Jitter x for better visibility if needed 
        y = ~possession_diff, 
        text = ~hover_text, 
        hoverinfo = "text", 
        marker = list(color = 'rgba(0,0,0,0)'), # Invisible markers, hover only 
        showlegend = FALSE 
      ) %>% 
      layout( 
        title = "Possession Difference vs Match Outcome (Violin Plot with Boxplots)", 
        yaxis = list(title = "Possession Difference (Home - Away %)", ticksuffix = "%"), 
        xaxis = list(title = "Match Outcome"), 
        violinmode = "overlay"
      ) 
  } else { # Beeswarm Plot 
    ggp <- ggplot(matches_to_plot, aes(x = outcome, y = possession_diff, color = outcome, text = hover_text)) + 
      geom_beeswarm(cex = 1.5) + 
      scale_color_manual(values = c("Home Win" = "green", "Away Win" = "red", "Draw" = "blue")) + 
      labs( 
        title = "Possession Difference vs Match Outcome (Beeswarm Plot)", 
        x = "Match Outcome", 
        y = "Possession Difference (Home - Away %)" 
      ) + 
      scale_y_continuous(labels = function(x) paste0(x, "%")) + 
      theme_bw() + 
      theme(legend.position = "none") 
    p <- ggplotly(ggp, tooltip = "text") 
  } 
  p 
}) 
## END OF MODIFICATION

}
