server <- function(input, output, session) {

  # ── 0.  Helper: translate radio to role string ─────────────────────────

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
    dt <- player_attributes_melted[age <= input$maxAge & role == roleSelected()]
    dt
  })

  #  DATA for radar (melted + scaled)
  radarData <- reactive({
    focus      <- roleSelected()
    role_vars  <- position_attribute_map[[focus]]
    top_player_nms     <- lolliData()[, unique(player_name)][1:10]

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
