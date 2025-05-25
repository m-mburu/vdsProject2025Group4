library(data.table)
library(tidyverse)
library(here)
library(plotly)
library(shiny)
library(shinydashboard)
library(ggbeeswarm)

data_path <- here("data")

# Load Data
tryCatch({
  match_table <- fread(file = file.path(data_path, "Match.csv"))
  leagues <- fread(file = file.path(data_path, "League.csv"))[, .(league_id = id, league = name)]
  team_table <- fread(file = file.path(data_path, "Team.csv"))
  possession_data <- tryCatch({
    fread(file.path(data_path, "Match_Possession.csv"))
  }, error = function(e) {
    warning(paste("Match_Possession.csv not loaded:", e$message))
    data.table()
  })
}, error = function(e) {
  stop(paste("Error loading data:", e$message))
})

# Data Preprocessing
if (nrow(possession_data) > 0) {
  possession_data[, id := as.numeric(id)]
  common_match_ids <- intersect(match_table$match_api_id, possession_data$id)
  match_table <- match_table[match_api_id %in% common_match_ids]
  possession_data <- possession_data[id %in% common_match_ids]
  setnames(possession_data, "id", "match_api_id")
  setkey(possession_data, match_api_id)
  setkey(match_table, match_api_id)
}

match_table[, win_margin := home_team_goal - away_team_goal]
match_table[, outcome := fifelse(win_margin > 0, "Home Win", fifelse(win_margin < 0, "Away Win", "Draw"))]

setkey(leagues, league_id)
setkey(match_table, league_id)
match_table <- merge(match_table, leagues, by = "league_id", all.x = TRUE, suffixes = c("", "_leagues"))

if (nrow(possession_data) > 0) {
  match_table <- merge(match_table, possession_data, by = "match_api_id", all.x = TRUE, suffixes = c("", "_possession"))
  
  if ("homepos" %in% names(match_table) && "awaypos" %in% names(match_table)) {
    match_table[, possession_diff := homepos - awaypos]
  } else {
    match_table[, `:=`(homepos = NA_real_, awaypos = NA_real_, possession_diff = NA_real_)]
  }
} else {
  match_table[, `:=`(homepos = NA_real_, awaypos = NA_real_, possession_diff = NA_real_)]
}

if (!"homepos" %in% names(match_table)) {
  match_table[, `:=`(homepos = NA, awaypos = NA, possession_diff = NA)]
}

team_id_to_name <- setNames(team_table$team_long_name, team_table$team_api_id)
match_table[, home_team_name := team_id_to_name[as.character(home_team_api_id)]]
match_table[, away_team_name := team_id_to_name[as.character(away_team_api_id)]]

match_table[, season_year := as.numeric(gsub("/.*", "", season))]

available_leagues_with_possession <- unique(match_table[!is.na(possession_diff), league])

# Define the UI
ui <- fluidPage(
  titlePanel("Possession vs. Match Outcome"),
  sidebarLayout(
    sidebarPanel(
      selectInput("league", "Select League:",
                  choices = c("All", available_leagues_with_possession),
                  selected = "All"
      ),
      sliderInput("season_range", "Select Season Range:",
                  min = min(match_table$season_year, na.rm = TRUE),
                  max = max(match_table$season_year, na.rm = TRUE),
                  value = c(min(match_table$season_year, na.rm = TRUE), max(match_table$season_year, na.rm = TRUE)),
                  sep = ""
      ),
      sliderInput("win_margin", "Select Win Margin:",
                  min = min(match_table$win_margin, na.rm = TRUE),
                  max = max(match_table$win_margin, na.rm = TRUE),
                  value = c(min(match_table$win_margin, na.rm = TRUE), max(match_table$win_margin, na.rm = TRUE)), step = 1
      ),
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("Violin Plot", "Beeswarm Plot"),
                  selected = "Violin Plot"
      )
    ),
    mainPanel(
      plotlyOutput("possessionOutcomePlot")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  filtered_matches <- reactive({
    req(match_table)
    
    matches <- copy(match_table)
    
    if (input$league != "All") {
      matches <- matches[league == input$league]
    }
    
    matches <- matches[season_year >= input$season_range[1] & season_year <= input$season_range[2]]
    
    matches <- matches[win_margin >= input$win_margin[1] & win_margin <= input$win_margin[2]]
    
    matches
  })
  
  output$possessionOutcomePlot <- renderPlotly({
    req(match_table)
    
    matches <- filtered_matches()
    matches <- na.omit(matches, cols = "possession_diff")
    
    stats <- matches %>%
      group_by(outcome) %>%
      summarize(
        Median = median(possession_diff, na.rm = TRUE),
        Minimum = min(possession_diff, na.rm = TRUE),
        Maximum = max(possession_diff, na.rm = TRUE),
        .groups = "drop"
      )
    
    matches <- merge(matches, stats, by = "outcome")
    
    Q1 <- matches %>% group_by(outcome) %>% summarise(Q1 = quantile(possession_diff, 0.25, na.rm = TRUE))
    Q3 <- matches %>% group_by(outcome) %>% summarise(Q3 = quantile(possession_diff, 0.75, na.rm = TRUE))
    IQR <- merge(Q1, Q3, by = "outcome") %>% mutate(IQR = Q3 - Q1)
    matches <- merge(matches, IQR, by = "outcome")
    matches[, is_outlier := possession_diff < (Q1 - 1.5 * IQR) | possession_diff > (Q3 + 1.5 * IQR)]
    
    matches[, hover_text := ifelse(is_outlier,
                                   paste(
                                     "League:", league, "<br>",
                                     "Season:", season, "<br>",
                                     "Home Team:", home_team_name, "<br>",
                                     "Away Team:", away_team_name, "<br>",
                                     "Match Outcome:", outcome, "<br>",
                                     "Possession Difference:", round(possession_diff, 1), "%<br>",
                                     "Home Team Goals:", home_team_goal, "<br>",
                                     "Away Team Goals:", away_team_goal, "<br>",
                                     "Goal Difference:", win_margin, "<br>",
                                     "Median:", round(Median, 1), "%<br>",
                                     "Minimum:", round(Minimum, 1), "%<br>",
                                     "Maximum:", round(Maximum, 1), "%"
                                   ),
                                   paste(
                                     "League:", league, "<br>",
                                     "Season:", season, "<br>",
                                     "Home Team:", home_team_name, "<br>",
                                     "Away Team:", away_team_name, "<br>",
                                     "Match Outcome:", outcome, "<br>",
                                     "Possession Difference:", round(possession_diff, 1), "%<br>",
                                     "Home Team Goals:", home_team_goal, "<br>",
                                     "Away Team Goals:", away_team_goal, "<br>",
                                     "Goal Difference:", win_margin, "<br>",
                                     "Median:", round(Median, 1), "%<br>",
                                     "Minimum:", round(Minimum, 1), "%<br>",
                                     "Maximum:", round(Maximum, 1), "%"
                                   )
    )]
    
    
    if (input$plot_type == "Violin Plot") {
      p <- plot_ly(
        data = matches, x = ~outcome, y = ~possession_diff, type = "violin",
        color = ~outcome, colors = c("Home Win" = "green", "Away Win" = "red", "Draw" = "blue"),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
        add_trace(
          type = "box",
          x = ~outcome,
          y = ~possession_diff,
          color = ~outcome,
          colors = c("Home Win" = "green", "Away Win" = "red", "Draw" = "blue"),
          text = ~hover_text,
          hoverinfo = "text",
          marker = list(color = "black"),
          showlegend = FALSE
        ) %>%
        add_trace(
          type = "scatter",
          x = ~outcome,
          y = ~possession_diff,
          mode = "markers",
          marker = list(color = ifelse(matches$is_outlier, "black", "rgba(0,0,0,0)")),
          text = ~hover_text,
          hoverinfo = "text",
          showlegend = FALSE
        ) %>%
        layout(
          title = "Possession Difference vs Match Outcome (Violin Plot with Boxplots)",
          yaxis = list(title = "Possession Difference (Home - Away)", ticksuffix = "%"),
          xaxis = list(title = "Match Outcome"),
          violinmode = "overlay",
          showlegend = FALSE
        )
    } else {
      ggp <- ggplot(matches, aes(x = outcome, y = possession_diff, color = outcome, text = hover_text)) +
        geom_beeswarm(cex = 1.5) +
        scale_color_manual(values = c("Home Win" = "green", "Away Win" = "red", "Draw" = "blue")) +
        labs(
          title = "Possession Difference vs Match Outcome (Beeswarm Plot)",
          x = "Match Outcome",
          y = "Possession Difference (Home - Away)"
        ) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        theme_bw() +
        theme(legend.position = "none")
      
      
      p <- ggplotly(ggp, tooltip = "text")
    }
    p
  })
}

# Run the app
shinyApp(ui = ui, server = server)