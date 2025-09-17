# ============================
# Main Survivor Pool Pipeline
# ============================

library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)

# ---------------------------
# Source modules
# ---------------------------
source("elo_engine.R")       # compute_elo_ratings()
source("elo_optimizer.R")    # optimize_survivor_picks()
source("elo_plot.R")         # plot_elo_history()

# ---------------------------
# Config
# ---------------------------
current_season <- 2025
run_optimizer <- TRUE  # Set to TRUE to run survivor optimizer

# Initial survivor pool state
survivor_state <- list(
  used_teams = c("WAS"),
  picked_weeks = c(1)
)

# ---------------------------
# Step 1: Load schedules
# ---------------------------
games_list <- lapply((current_season - 4):current_season, fast_scraper_schedules)
all_games <- bind_rows(games_list) %>% arrange(season, week)

# ---------------------------
# Step 1a: Standardize team abbreviations
# ---------------------------
team_map <- c(
  "LA" = "LAR",        # Rams
  "STL" = "LAR",       # Old Rams before 2016
  "SD" = "LAC",        # Chargers relocation
  "OAK" = "LV",        # Raiders relocation
  "LV" = "LV"          # Explicitly define LV as Raiders
)

all_games <- all_games %>%
  mutate(
    home_team = ifelse(home_team %in% names(team_map), team_map[home_team], home_team),
    away_team = ifelse(away_team %in% names(team_map), team_map[away_team], away_team)
  )

# ---------------------------
# Step 2: Compute Elo ratings
# ---------------------------
elo_results <- compute_elo_ratings(all_games, current_season)
ratings <- elo_results$ratings
history <- elo_results$history

cat("Elo updated at:", elo_results$updated_at, "\n")
cat("Most recent game included:\n")
print(elo_results$last_game)

# ---------------------------
# Step 3: Run survivor optimizer (optional)
# ---------------------------
solution <- tibble()
if (run_optimizer) {
  solution <- optimize_survivor_picks(
    ratings,
    all_games,
    current_season,
    used_teams = survivor_state$used_teams,
    picked_weeks = survivor_state$picked_weeks
  )
  
  if (nrow(solution) == 0) {
    cat("No picks available for remaining weeks.\n")
  } else {
    cat("Recommended survivor picks:\n")
    print(solution)
    
    # Update survivor state
    survivor_state$used_teams  <- c(survivor_state$used_teams, solution$pick)
    survivor_state$picked_weeks <- c(survivor_state$picked_weeks, solution$week)
  }
}

cat("Updated survivor state:\n")
print(survivor_state)

# ---------------------------
# Step 4: Add division info & continuous game index
# ---------------------------
division_lookup <- tibble(
  team = c(
    # AFC East
    "BUF","MIA","NE","NYJ",
    # AFC North
    "BAL","CIN","CLE","PIT",
    # AFC South
    "HOU","IND","JAX","TEN",
    # AFC West
    "DEN","KC","LV","LAC",
    # NFC East
    "DAL","NYG","PHI","WAS",
    # NFC North
    "CHI","DET","GB","MIN",
    # NFC South
    "ATL","CAR","NO","TB",
    # NFC West
    "ARI","SEA","SF","LAR"
  ),
  division = c(
    rep("AFC East",4), rep("AFC North",4), rep("AFC South",4), rep("AFC West",4),
    rep("NFC East",4), rep("NFC North",4), rep("NFC South",4), rep("NFC West",4)
  )
)

history_div <- history %>%
  left_join(division_lookup, by = "team") %>%
  arrange(season, gameday) %>%
  # Continuous game index across all seasons (skip offseason)
  mutate(game_index = row_number())

# ---------------------------
# Step 5: Define NFL colors
# ---------------------------
team_colors <- c(
  # AFC East
  "BUF" = "#00338D", "MIA" = "#008E97", "NE" = "#002244", "NYJ" = "#203731",
  # AFC North
  "BAL" = "#241773", "CIN" = "#FB4F14", "CLE" = "#311D00", "PIT" = "#FFB612",
  # AFC South
  "HOU" = "#A71930", "IND" = "#002C5F", "JAX" = "#006778", "TEN" = "#4B92DB",
  # AFC West
  "DEN" = "#FB4F14", "KC" = "#E31837", "LV" = "#000000", "LAC" = "#0080C6",
  # NFC East
  "DAL" = "#003594", "NYG" = "#0B2265", "PHI" = "#004C54", "WAS" = "#773141",
  # NFC North
  "CHI" = "#0B162A", "DET" = "#0076B6", "GB" = "#203731", "MIN" = "#4F2683",
  # NFC South
  "ATL" = "#A71930", "CAR" = "#0085CA", "NO" = "#D3BC8D", "TB" = "#D50A0A",
  # NFC West
  "ARI" = "#97233F", "SEA" = "#002244", "SF" = "#AA0000", "LAR" = "#866D4B"
)

# ---------------------------
# Step 6: Plot Elo by division with year labels
# ---------------------------
season_breaks <- history_div %>%
  group_by(season) %>%
  summarize(
    start = min(game_index),
    end   = max(game_index),
    mid   = mean(game_index),
    .groups = "drop"
  )

# Right-aligned labels for last point per team
label_data <- history_div %>%
  group_by(division, team) %>%
  summarize(
    y = rating_post[which.max(game_index)],
    x = max(game_index),
    .groups = "drop"
  )

p <- ggplot(history_div, aes(x = game_index, y = rating_post, color = team)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ division, ncol = 1, scales = "free_y") +
  labs(
    title = "NFL Elo Ratings by Division",
    x = "Season",
    y = "Elo Rating",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  ) +
  geom_text_repel(
    data = label_data,
    aes(x = x, y = y, label = team),
    nudge_x = 1,
    direction = "y",
    hjust = 0,
    segment.color = NA,
    size = 3
  ) +
  scale_x_continuous(
    breaks = season_breaks$mid,
    labels = season_breaks$season
  ) +
  geom_vline(
    data = season_breaks,
    aes(xintercept = start),
    linetype = "dashed",
    color = "grey70"
  ) +
  scale_color_manual(values = team_colors)

print(p)
