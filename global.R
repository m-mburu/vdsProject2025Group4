# Define player attribute groupings by position

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
