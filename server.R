server <- function(input, output, session) {







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
    })

    #
    selected_opponents <- reactive({
      dt <- summary_opponents()
      if (input$oppStrength == "Strongest") {
        dt <- dt[order(-win_rate)]
      } else {
        dt <- dt[order( win_rate)]
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


}
