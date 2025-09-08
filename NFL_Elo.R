# ---------------------------
# Survivor Pool Elo Optimizer (refined Elo)
# ---------------------------

library(dplyr)
library(nflfastR)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# ---------------------------
# Config
# ---------------------------
current_season <- 2025
used_teams <- c()  # Teams already picked

home_field_adv <- 55  # Elo points boost for home team
base_k <- 20          # Base K-factor

# ---------------------------
# Step 1: Load schedules
# ---------------------------
games_list <- lapply((current_season - 4):current_season, fast_scraper_schedules)
all_games <- bind_rows(games_list) %>% arrange(season, week)

# ---------------------------
# Step 2: Initialize Elo ratings with carryover
# ---------------------------
teams <- unique(c(all_games$home_team, all_games$away_team))
ratings <- setNames(rep(1500, length(teams)), teams)

# Optional: carryover Elo from previous season(s)
for (season in (current_season - 4):(current_season - 1)) {
  season_games <- all_games %>% filter(season == season)
  for (i in 1:nrow(season_games)) {
    home <- season_games$home_team[i]
    away <- season_games$away_team[i]
    hs <- season_games$home_score[i]
    as <- season_games$away_score[i]
    
    if (!is.na(hs) && !is.na(as)) {
      # Result from home perspective
      result <- if (hs > as) 1 else if (hs < as) 0 else 0.5
      
      # Home-field adjustment
      ra <- ratings[home] + home_field_adv
      rb <- ratings[away]
      
      # Margin of victory adjustment
      margin <- abs(hs - as)
      k_eff <- base_k * log(margin + 1)
      
      # Elo update
      ea <- 1 / (1 + 10 ^ ((rb - ra) / 400))
      eb <- 1 - ea
      ratings[home] <- ratings[home] + k_eff * (result - ea)
      ratings[away] <- ratings[away] + k_eff * ((1 - result) - eb)
    }
  }
  # Regress toward 1500 slightly
  ratings <- 0.75 * ratings + 0.25 * 1500
}

# ---------------------------
# Step 3: Update Elo with completed games for current season
# ---------------------------
current_games <- all_games %>% filter(season == current_season)
for (i in 1:nrow(current_games)) {
  home <- current_games$home_team[i]
  away <- current_games$away_team[i]
  hs <- current_games$home_score[i]
  as <- current_games$away_score[i]
  
  if (!is.na(hs) && !is.na(as)) {
    result <- if (hs > as) 1 else if (hs < as) 0 else 0.5
    ra <- ratings[home] + home_field_adv
    rb <- ratings[away]
    margin <- abs(hs - as)
    k_eff <- base_k * log(margin + 1)
    ea <- 1 / (1 + 10 ^ ((rb - ra) / 400))
    eb <- 1 - ea
    ratings[home] <- ratings[home] + k_eff * (result - ea)
    ratings[away] <- ratings[away] + k_eff * ((1 - result) - eb)
  }
}

# ---------------------------
# Step 4: Build survivor pool table for future games
# ---------------------------
future_games <- current_games %>% filter(is.na(home_score) | is.na(away_score))
survivor_table <- future_games %>%
  rowwise() %>%
  do(data.frame(
    week = .$week,
    team = c(.$home_team, .$away_team),
    opponent = c(.$away_team, .$home_team),
    stringsAsFactors = FALSE
  )) %>%
  rowwise() %>%
  mutate(
    team_elo = ratings[team],
    opponent_elo = ratings[opponent],
    elo_diff = team_elo - opponent_elo
  ) %>%
  ungroup()

# ---------------------------
# Step 5: Filter out used teams
# ---------------------------
valid_combinations <- survivor_table %>%
  filter(!(team %in% used_teams)) %>%
  select(team, week, opponent, elo_diff) %>%
  mutate(id = row_number())

# ---------------------------
# Step 6: Build integer program with streak constraint
# ---------------------------
ids <- valid_combinations$id
teams_unique <- unique(valid_combinations$team)
weeks_unique <- sort(unique(valid_combinations$week))
opponents_unique <- unique(valid_combinations$opponent)

model <- MIPModel() %>%
  add_variable(x[id], id = ids, type = "binary") %>%
  set_objective(sum_expr(valid_combinations$elo_diff[valid_combinations$id == id] * x[id], id = ids), sense = "max") %>%
  add_constraint(sum_expr(x[id], id = ids[valid_combinations$week == w]) == 1, w = weeks_unique) %>%
  add_constraint(sum_expr(x[id], id = ids[valid_combinations$team == t]) <= 1, t = teams_unique)

# Streak constraints: max 2 picks against same opponent in 3 consecutive weeks
for (opp in opponents_unique) {
  for (i in 1:(length(weeks_unique) - 2)) {
    w_block <- weeks_unique[i:(i+2)]
    model <- model %>%
      add_constraint(
        sum_expr(x[id], id = ids[valid_combinations$opponent == opp & valid_combinations$week %in% w_block]) <= 2
      )
  }
}

# ---------------------------
# Step 7: Solve and extract solution
# ---------------------------
result <- solve_model(model, with_ROI(solver = "glpk"))
solution <- get_solution(result, x[id]) %>%
  filter(value > 0.5) %>%
  left_join(valid_combinations, by = "id") %>%
  arrange(week) %>%
  select(week, team, opponent, elo_diff)

print(solution)
