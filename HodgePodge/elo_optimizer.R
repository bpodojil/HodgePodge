# ============================
# Survivor Pool Optimizer
# ============================
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

optimize_survivor_picks <- function(ratings, all_games, current_season,
                                    used_teams = character(),
                                    picked_weeks = integer(),
                                    home_field_adv = 55) {
  
  # ---------------------------
  # Step 1: Identify future games (exclude picked weeks)
  # ---------------------------
  future_games <- all_games %>%
    filter(season == current_season,
           is.na(home_score) | is.na(away_score),
           !(week %in% picked_weeks)) %>%
    mutate(game_id = row_number())  # unique identifier for each game
  
  if (nrow(future_games) == 0) {
    message("No weeks remaining to pick.")
    return(tibble())
  }
  
  # ---------------------------
  # Step 2: Build team-opponent table
  # ---------------------------
  survivor_table <- bind_rows(
    future_games %>% transmute(week, game_id, team = home_team, opponent = away_team, is_home = TRUE),
    future_games %>% transmute(week, game_id, team = away_team, opponent = home_team, is_home = FALSE)
  ) %>%
    mutate(team_elo = ratings[team],
           opp_elo  = ratings[opponent],
           elo_diff = (team_elo - opp_elo) + ifelse(is_home, home_field_adv, -home_field_adv),
           win_prob = 1 / (1 + 10 ^ (-elo_diff / 400))) %>%
    filter(!(team %in% used_teams)) %>%
    arrange(week, team) %>%
    mutate(id = row_number())
  
  # ---------------------------
  # Step 3: Build integer programming model
  # ---------------------------
  ids <- survivor_table$id
  weeks_unique <- sort(unique(survivor_table$week))
  teams_unique <- unique(survivor_table$team)
  opponents_unique <- unique(survivor_table$opponent)
  
  model <- MIPModel() %>%
    add_variable(x[id], id = ids, type = "binary") %>%
    set_objective(sum_expr(survivor_table$win_prob[survivor_table$id == id] * x[id], id = ids),
                  sense = "max") %>%
    
    # One pick per week
    add_constraint(sum_expr(x[id], id = ids[survivor_table$week == w]) == 1,
                   w = weeks_unique) %>%
    
    # Max one pick per team
    add_constraint(sum_expr(x[id], id = ids[survivor_table$team == t]) <= 1,
                   t = teams_unique)
  
  # Streak constraints: max 2 picks against same opponent in any 3-week window
  for (opp in opponents_unique) {
    for (i in seq_len(length(weeks_unique) - 2)) {
      w_block <- weeks_unique[i:(i + 2)]
      model <- model %>%
        add_constraint(
          sum_expr(x[id],
                   id = ids[survivor_table$opponent == opp &
                              survivor_table$week %in% w_block]) <= 2
        )
    }
  }
  
  # ---------------------------
  # Step 4: Solve and return solution
  # ---------------------------
  result <- solve_model(model, with_ROI(solver = "glpk"))
  
  if (!result$status %in% c("success", "optimal")) {
    stop(sprintf("Solver status: %s", result$status))
  }
  
  solution <- get_solution(result, x[id]) %>%
    filter(value > 0.5) %>%
    left_join(survivor_table, by = "id") %>%
    # Add game date from original future_games table
    left_join(future_games %>% select(week, home_team, away_team, gameday),
              by = c("week", "team" = "home_team")) %>%  # match home_team first
    mutate(
      gameday = as.Date(ifelse(is.na(gameday),
                               future_games$gameday[future_games$away_team == team],
                               gameday)),
      venue = ifelse(is_home, "home", "away"),
      win_prob = round(win_prob, 3)
    ) %>%
    arrange(week) %>%
    transmute(
      week,
      pick = team,
      opponent,
      gameday,
      win_prob
    )
  
  return(solution)
  }