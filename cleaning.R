library(tidyverse)
library(data.table)
library(here)
library(ggmap)
library(rnaturalearth)
library(sf)
data_path <- here( "data")
vds2445 <- here(data_path,"VDS2425 Football")

list.files(vds2445)



file_paths <- list.files(vds2445, full.names = TRUE)
file_names <- list.files(vds2445) %>%
  gsub("\\.csv$|\\.xlsx$", "", .)  %>%
  janitor::make_clean_names()

file_exts <- tools::file_ext(file_paths)

for (i in seq_along(file_paths)) {
  if (file_exts[i] == "csv") {
    assign(file_names[i], fread(file_paths[i]))
  } else if (file_exts[i] == "xlsx") {
    assign(file_names[i], readxl::read_excel(file_paths[i]))
    setDT(get(file_names[i]))
  }
}

# Geocode  lon , lat for teams

eu_countries <- c("Belgium","France","Germany","Italy",
                  "Netherlands","Poland","Portugal",
                  "Spain","Switzerland")

ne_countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% eu_countries) %>%
  select(country = admin, geometry)

# b) UK states, then pick England + Scotland
uk_states <- rnaturalearth::ne_states(country = "United Kingdom", returnclass = "sf")
eng_scot  <- uk_states %>%
  filter(geonunit %in% c("England","Scotland"))  %>%
  group_by(geonunit) %>%           # “England” and “Scotland”
  summarize(                     # this automatically unions the geometries
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  st_as_sf() %>%
  select(country = geonunit, geometry)

# c) Combine into one sf
map_sf <- bind_rows(ne_countries, eng_scot)


map_sf <- left_join(map_sf, country, by = c("country" = "name")) %>%
  left_join(league, by  = "id")

# Goalkeeper-specific attributes
gk_vars <- c(
  "gk_diving", "gk_handling", "gk_kicking",
  "gk_positioning", "gk_reflexes"
)

# Defender-specific attributes
defender_vars <- c(
  "marking", "standing_tackle", "sliding_tackle",
  "interceptions", "heading_accuracy", "strength",
  "aggression", "jumping"
)

# Midfielder-specific attributes
midfielder_vars <- c(
  "short_passing", "long_passing", "vision",
  "ball_control", "dribbling", "curve",
  "interceptions", "agility", "stamina",
  "balance"
)

# Forward / Attacker-specific attributes
forward_vars <- c(
  "finishing", "positioning", "shot_power",
  "volleys", "long_shots", "penalties",
  "sprint_speed", "acceleration", "heading_accuracy"
)

# General attributes useful across all roles
general_vars <- c(
  "overall_rating", "potential", "reactions",
  "agility", "balance", "stamina", "strength",
  "sprint_speed", "acceleration", "ball_control"
)

# combine all into a named list
position_attribute_map <- list(
  goalkeeper = gk_vars,
  defender = defender_vars,
  midfielder = midfielder_vars,
  forward = forward_vars,
  general = general_vars
)


player[, age := as.numeric(as.Date("2016-06-30") - as.Date(birthday)) / 365.25]


player_attributes_merge <- player_attributes %>%
  merge(player[age <= 21], by = c("player_api_id", "player_fifa_api_id"))

all_pl_attr_nms <- c(
  "player_api_id", "player_fifa_api_id", "player_name","age",
  unique(unlist(position_attribute_map))
)


player_attributes_merge <- player_attributes_merge[, ..all_pl_attr_nms]


for (role in names(position_attribute_map)) {
  vars <- position_attribute_map[[role]]

  # Create new column with mean across the selected columns
  player_attributes_merge[, (role) := rowMeans(.SD, na.rm = TRUE), .SDcols = vars]
}

player_attributes_melted <- player_attributes_merge %>%
  melt(id.vars = c("player_api_id", "player_fifa_api_id", "player_name", "age"),
       measure.vars = names(position_attribute_map),
       variable.name = "role",
       value.name = "rating")

## group by player name

player_attributes_melted <- player_attributes_melted[, .(mean_rate = mean(rating,
                                                                          na.rm = TRUE)),
                                                     by = .(player_name, age, role)] %>%
  unique(by = c("player_name", "role"))


home <- match[ , .(
  league_id,
  match_api_id,
  date,
  team_api_id   = home_team_api_id,
  goals_for     = home_team_goal,
  goals_against = away_team_goal,
  season,
  home_away = "home"
)]
away <- match[ , .(
  league_id,
  match_api_id,
  date,
  team_api_id   = away_team_api_id,
  goals_for     = away_team_goal,
  goals_against = home_team_goal,
  season,
  home_away = "away"
)]


match_long <- rbindlist(list(home, away))

# result & points
match_long[ , result := sign(goals_for - goals_against)]              #  1 / 0 / −1
match_long[ , pts    := fifelse(result ==  1, 3,
                          fifelse(result ==  0, 1, 0))]


match_long[, unique(season)]



# Defineseasons
before_seasons <- c("2008/2009", "2009/2010", "2010/2011",
                    "2011/2012")          # < 50 %

after_seasons  <- c("2012/2013", "2013/2014", "2014/2015", "2015/2016")  # > 70 %


winA <- match_long[ season %in% before_seasons,
                    .(games_A = .N,
                      wins_A  = sum(result == 1)),
                    by = team_api_id ]


winB <- match_long[ season %in% after_seasons,
                    .(games_B = .N,
                      wins_B  = sum(result == 1)),
                    by = team_api_id ]

#Combine + calculate win-rates
improve <- merge(winA, winB, by = "team_api_id")
improve[ , `:=`(
  win_A = wins_A / games_A * 100,
  win_B = wins_B / games_B * 100
)]

# Select significantly improved teams
big_improvers <- improve[ win_A < 50 & win_B > 70 ]
setorder(big_improvers, -win_B)

big_improvers_ids <- big_improvers$team_api_id

big_improvers_dt <- match_long[ team_api_id %in% big_improvers_ids]

## connect with team to get team names


big_improvers_dt <- merge(big_improvers_dt,
                            team[team_api_id %in% big_improvers_ids, .(
                              team_api_id,
                              team_long_name,
                              team_short_name
                            )],
                            by = "team_api_id")



big_improvers_dt_sum <- big_improvers_dt[ , .(
  games = .N,
  wins   = sum(result == 1),
  goals_for     = sum(goals_for),
  goals_against = sum(goals_against)
), by = .(team_long_name, team_api_id, season)]


big_improvers_dt_sum[, win_rate := wins / games * 100]


# ggplot(big_improvers_dt_sum, aes(season, win_rate,
#                                                   colour = team_long_name,
#                                                   group  = team_long_name)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   facet_wrap(~ home_away, nrow = 2) +
#   scale_y_continuous(limits = c(0,100)) +
#   labs(
#     title = "Seasonal Win-Rate for Top Improvers",
#     subtitle = paste("Filtered:", paste("home_away_choice", collapse = ", ")),
#     x = "Season",
#     y = "Win Rate (%)",
#     colour = "Team"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "bottom"
#   )






home_barca <- match[home_team_api_id == "8634" , .(
  match_api_id,
  date,
  team_api_id   = home_team_api_id,
  goals_for     = home_team_goal,
  goals_against = away_team_goal,
  season,
  home_away = "home"
)]

away_barca <- match[away_team_api_id == "8634",
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




setDT(barca)


intro_text <- HTML("
        <h4>Project Overview</h4>
        <p>
          <strong>Group 4 – Football Dataset</strong><br/>
          This project supports FC Barcelona’s coach in making data‐driven strategic decisions
          for the 2016–2017 season by exploring match, player, and team metadata from 2008 to 2016
          across Europe’s top leagues. We aim to uncover recruitment targets, performance trends,
          and tactical insights—such as possession’s impact on wins—to guide Barcelona’s planning.
        </p>

        <h4>Main Features & Connections</h4>
        <p>
          Our dataset spans 11 premier European leagues and includes:
          <ul>
            <li><strong>Match Data:</strong> Teams, scores, lineups, and outcomes.</li>
            <li><strong>Player Info:</strong> Demographics and FIFA‐style skill ratings.</li>
            <li><strong>Team Attributes:</strong> Tactical metrics like build‐up speed and defensive pressure.</li>
            <li><strong>Events & Possession:</strong> Goals, shots, crosses, fouls, and possession statistics.</li>
            <li><strong>League/Country Context:</strong> Enables cross‐league comparisons via common IDs.</li>
          </ul>
        </p>

        <h4>Guiding Questions</h4>
        <p>
          <ol>
            <li>Which young talents (≤ 21) should Barcelona recruit?</li>
            <li>Which clubs improved most between 2008–2012 and 2013–2016?</li>
            <li>Against which opponents has Barça fared best and worst?</li>
            <li>How strongly does ball possession predict match outcomes?</li>
          </ol>
        </p>
      ")

league_team <- match_long %>%
  distinct(league_id, team_api_id)
team_country <- merge(league, country[, .(id, country = name)],
              by.x = "country_id", by.y = "id") %>%
  merge(league_team, by.x = "id", by.y = "league_id") %>%
  merge(team[, .(team_api_id, team_long_name, team_short_name)],
              by = "team_api_id")

all_obj_shiny <- list(
  barca = barca,
  player_attributes_melted = player_attributes_melted,
  player_attributes_merge = player_attributes_merge,
  big_improvers_dt = big_improvers_dt,
  big_improvers_dt_sum = big_improvers_dt_sum,
  map_sf = map_sf,
  intro_text = intro_text,
  team_country = team_country
)


save(all_obj_shiny, file = here(data_path, "all_obj_shiny.rda"))
