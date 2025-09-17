# ============================
# Elo rating engine (with logging, pre & post ratings, win probability)
# ============================
library(dplyr)

compute_elo_ratings <- function(all_games, current_season,
                                home_field_adv = 55,
                                base_k = 20,
                                carry_years = 4,
                                regression = 0.25) {
  teams <- unique(c(all_games$home_team, all_games$away_team))
  ratings <- setNames(rep(1500, length(teams)), teams)
  
  # Container for history
  elo_history <- tibble(
    season = integer(),
    week   = integer(),
    gameday = as.Date(character()),
    team   = character(),
    rating_pre = double(),
    rating_post = double(),
    win_prob = double(),
    result = double()
  )
  
  update_elo <- function(home, away, hs, as, ratings) {
    result <- if (hs > as) 1 else if (hs < as) 0 else 0.5
    ra <- ratings[home] + home_field_adv
    rb <- ratings[away]
    margin <- abs(hs - as)
    k_eff <- base_k * log(margin + 1)
    ea <- 1 / (1 + 10 ^ ((rb - ra) / 400))
    ratings[home] <- ratings[home] + k_eff * (result - ea)
    ratings[away] <- ratings[away] + k_eff * ((1 - result) - (1 - ea))
    list(ratings = ratings, ea = ea, result = result)
  }
  
  last_game_included <- NULL
  
  # Helper to process a season’s games
  process_season <- function(season_games, season) {
    for (i in seq_len(nrow(season_games))) {
      home <- season_games$home_team[i]
      away <- season_games$away_team[i]
      hs   <- season_games$home_score[i]
      as   <- season_games$away_score[i]
      gd   <- as.Date(season_games$gameday[i])
      
      if (!is.na(hs) && !is.na(as)) {
        pre_home <- ratings[home]
        pre_away <- ratings[away]
        
        res <- update_elo(home, away, hs, as, ratings)
        ratings <<- res$ratings
        
        # Store both teams' win probs and results
        elo_history <<- bind_rows(elo_history,
                                  tibble(season = season,
                                         week = season_games$week[i],
                                         gameday = gd,
                                         team = home,
                                         rating_pre = pre_home,
                                         rating_post = ratings[home],
                                         win_prob = res$ea,
                                         result = res$result),
                                  tibble(season = season,
                                         week = season_games$week[i],
                                         gameday = gd,
                                         team = away,
                                         rating_pre = pre_away,
                                         rating_post = ratings[away],
                                         win_prob = 1 - res$ea,
                                         result = 1 - res$result)
        )
        
        last_game_included <<- season_games[i, ]
      }
    }
  }
  
  # Past seasons
  for (prev_season in (current_season - carry_years):(current_season - 1)) {
    season_games <- filter(all_games, .data$season == prev_season)
    process_season(season_games, prev_season)
    ratings <- (1 - regression) * ratings + regression * 1500
  }
  
  # Current season
  current_games <- filter(all_games, .data$season == current_season)
  process_season(current_games, current_season)
  
  # Metadata
  updated_at <- Sys.time()
  last_game <- if (!is.null(last_game_included)) {
    last_game_included %>%
      select(season, week, gameday, home_team, away_team, home_score, away_score)
  } else {
    tibble()
  }
  
  list(
    ratings = ratings,
    history = elo_history,
    last_game = last_game,
    updated_at = updated_at
  )
}
