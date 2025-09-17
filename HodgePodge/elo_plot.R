library(dplyr)
library(ggplot2)
library(ggrepel)

# ---------------------------
# Add division info to Elo history
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
  arrange(season, gameday)

# ---------------------------
# Add continuous game index (ignore offseason)
# ---------------------------
history_div <- history_div %>%
  group_by(season) %>%
  mutate(game_index = row_number()) %>%
  ungroup()

# Season boundaries for vertical lines
season_lines <- history_div %>%
  group_by(season) %>%
  summarize(start_index = min(game_index), .groups = "drop")

# ---------------------------
# Faceted plot by division
# ---------------------------
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
  # Label last point of each line
  geom_text_repel(
    data = history_div %>% group_by(team, division) %>% slice_tail(n = 1),
    aes(label = team),
    nudge_x = 0.5,
    direction = "y",
    hjust = 0,
    segment.color = NA,
    size = 3
  ) +
  # Vertical lines for season boundaries
  geom_vline(
    data = season_lines,
    aes(xintercept = start_index),
    linetype = "dashed",
    color = "grey70"
  )

# Apply custom team colors if provided
if (exists("team_colors")) {
  p <- p + scale_color_manual(values = team_colors)
}

print(p)
