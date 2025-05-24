

home_barca <- match[home_team_api_id == "8634" , .(
  match_api_id,
  date,
  team_api_id   = home_team_api_id,
  goals_for     = home_team_goal,
  goals_against = away_team_goal,
  season,
  home_away = "home"
)]

away_barca <- match[away_api_id == "8634",
                     .(
  match_api_id,
  date,
  team_api_id   = away_team_api_id,
  goals_for     = away_team_goal,
  goals_against = home_team_goal,
  season,
  home_away = "away"
)]

## use tudyverse

home_barca <- match %>%
  filter(home_team_api_id == "8634") %>%
  select(match_api_id, date,
         team_api_id = home_team_api_id,
         team_played_against = away_team_api_id,
         goals_for = home_team_goal,
         goals_against = away_team_goal,
         season) %>%
  mutate(home_away = "home")

away_barca <- match %>%
  filter(away_team_api_id == "8634") %>%
  select(match_api_id, date,
         team_api_id = away_team_api_id,
         team_played_against = home_team_api_id,
         goals_for = away_team_goal,
         goals_against = home_team_goal,
         season) %>%
  mutate(home_away = "away")

# Combine home and away data

barca <- rbind(home_barca, away_barca)


barca <- merge(barca,
                    team[, .(
                      team_api_id,
                      team_long_name,
                      team_short_name
                    )],
                    by.x = "team_played_against",
                    by.y = "team_api_id")


barca <- barca %>%
  mutate(goals_diff = goals_for - goals_against,
    result = sign(goals_diff),
         points= case_when(
           goals_diff >= 1 ~ 3,  # Win
           goals_diff == 0 ~ 1,  # Draw
           goals_diff < 0 ~ 0  # Loss
         ))


## groupp by points to get win rates

barca_summary <- barca %>%
  group_by( team_api_id, team_long_name, team_short_name) %>%
  summarise(
    total_matches = n(),
    total_goals_for = sum(goals_for),
    total_goals_against = sum(goals_against),
    total_points = sum(points),
    wins = sum(result == 1),
    draws = sum(result == 0),
    losses = sum(result == -1)
  ) %>%
  ungroup() %>%
  mutate(
    win_rate = wins / total_matches,
    draw_rate = draws / total_matches,
    loss_rate = losses / total_matches
  ) %>% filter(total_matches > 7)


best_5_opponent_ids <- barca %>%
  group_by(team_played_against) %>%
  summarise(total_matches = n(),
            total_goals_for = sum(goals_for),
            total_goals_against = sum(goals_against),
            total_points = sum(points)) %>%
  filter(total_matches > 7) %>%
  arrange(desc(total_points)) %>%
  slice_head(n = 5) %>%
  pull(team_played_against)


worst_5_opponents_ids <- barca %>%
  group_by(team_played_against) %>%
  summarise(total_matches = n(),
            total_goals_for = sum(goals_for),
            total_goals_against = sum(goals_against),
            total_points = sum(points)) %>%
  filter(total_matches > 7) %>%
  arrange(total_points) %>%
  slice_head(n = 5) %>%
  pull(team_played_against)






top_ids <- best_5_opponent_ids

# 3. Summarise Win/Draw/Loss counts & percentages
plot_dt <- copy(barca)[ team_played_against %in% top_ids ]

# map numeric result → label
plot_dt[ , result_label := factor(result,
                                  levels = c( 1,  0, -1),
                                  labels = c("Win","Draw","Loss")) ]

# count per opponent
plot_dt <- plot_dt[ , .(n = .N), by = .(team_played_against, team_long_name, result_label)]

# compute percent share (round so waffle sums to ~100)
plot_dt[ , pct := round(n / sum(n) * 100), by = team_played_against]








# 1) Expand into a 10×10 grid with pct & total matches per cell
cells <- plot_dt[, {
  # reps of each result_label
  reps      <- rep(result_label, times = pct)
  ncell     <- length(reps)
  # replicate pct & n to match each cell
  percentages   <- rep(pct, times = pct)
  total_matches <- rep(sum(n), times = ncell)

  data.table(
    team          = team_long_name[1],
    result        = reps,
    percentage    = percentages,
    total_matches = total_matches,
    col           = rep(1:10, length.out = ncell),
    row           = rep(1:10, each = 10)[1:ncell]
  )
}, by = .(team_played_against, team_long_name)]

# 2) Draw with geom_tile + facet + custom text aesthetic
p <- ggplot(cells, aes(
  x = col,
  y = row,
  fill = result,
  text = paste0(
    team,
    "<br>Result: ", result,
    "<br>% of matches: ", percentage, "%",
    "<br>Total matches: ", total_matches
  )
)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c(
    Win  = "#2ECC71",
    Draw = "#F1C40F",
    Loss = "#E74C3C"
  )) +
  facet_wrap(~ team, ncol = 3) +
  coord_equal() +
  theme_minimal() +
  theme(
    axis.text      = element_blank(),
    axis.title     = element_blank(),
    panel.grid     = element_blank(),
    strip.text     = element_text(face = "bold"),
    legend.position = "none"
  ) +
  labs(title = "Interactive Waffle: Barça vs Top Opponents")

# 3) Convert to Plotly, using our `text` for the tooltip
ggplotly(p, tooltip = "text")
