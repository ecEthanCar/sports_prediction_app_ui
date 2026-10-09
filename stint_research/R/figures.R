# Figures for reports/figures.Rmd: the key visuals from every stage, built from
# pipeline results. Requires the other files in R/ (tar_source() loads them all).

FIG_DIR <- "figures"

# Save a figure as a PNG (for slides) and return it so the report shows it too
save_fig <- function(p, name, width, height) {
  dir.create(FIG_DIR, showWarnings = FALSE)
  ggplot2::ggsave(file.path(FIG_DIR, paste0(name, ".png")), p,
                  width = width, height = height, dpi = 200, bg = "white")
  p
}

GROUP_ORDER <- c("GK", "CB", "FB", "MF", "FW")
TIME_BINS <- c("P1 0-15", "P1 15-30", "P1 30-45+", "P2 0-15", "P2 15-30", "P2 30-45+")
COMPONENT_LABELS <- c("1" = "Long-range cluster", "2" = "Core", "3" = "High-value tail")

# ---- Stage 1 -------------------------------------------------------------------------

# One row per team: shots per match and xG per shot
team_profiles <- function(tm) {
  as.data.frame(tm)[, c("league", "att_team", "n_shots", "npxg")] |>
    dplyr::group_by(league, team = att_team) |>
    dplyr::summarise(shots_per_match = mean(n_shots),
                     xg_per_shot = sum(npxg) / sum(n_shots),
                     xg_per_match = mean(npxg), .groups = "drop")
}

plot_team_profiles <- function(tm, n_labels = 10) {
  d <- team_profiles(tm)
  z <- function(v) (v - mean(v)) / stats::sd(v)
  far <- order(-(z(d$shots_per_match)^2 + z(d$xg_per_shot)^2))[seq_len(min(n_labels, nrow(d)))]
  ggplot2::ggplot(d, ggplot2::aes(x = shots_per_match, y = xg_per_shot)) +
    ggplot2::geom_vline(xintercept = mean(d$shots_per_match), linetype = "dashed", color = "gray60") +
    ggplot2::geom_hline(yintercept = mean(d$xg_per_shot), linetype = "dashed", color = "gray60") +
    ggplot2::geom_point(ggplot2::aes(shape = league), size = 2) +
    ggplot2::geom_text(data = d[far, ], ggplot2::aes(label = team), vjust = -0.8, size = 3,
                       check_overlap = TRUE) +
    ggplot2::scale_shape_manual(values = c(16, 1, 17, 2)) +
    ggplot2::labs(title = "Team attacking profiles: shot volume vs shot quality",
                  x = "Open-play shots per match", y = "xG per shot (dashed lines = averages)",
                  shape = NULL) +
    plain_theme()
}

# Goal rate vs mean xG within xG deciles, for open play and direct free kicks,
# with exact binomial 95% intervals. Points above the diagonal = xG too low.
xg_reliability <- function(shots_all, types = c("Open Play", "Free Kick"), bins = 10) {
  d <- shots_all[shots_all$set_piece %in% types, ]
  do.call(rbind, lapply(types, function(t) {
    s <- d[d$set_piece == t, ]
    s$bin <- dplyr::ntile(s$xg, bins)
    do.call(rbind, lapply(split(s, s$bin), function(b) {
      ci <- stats::binom.test(sum(b$shot_made), nrow(b))$conf.int
      data.frame(type = t, n = nrow(b), mean_xg = mean(b$xg), goal_rate = mean(b$shot_made),
                 lo = ci[1], hi = ci[2])
    }))
  }))
}

plot_xg_reliability <- function(shots_all) {
  d <- xg_reliability(shots_all)
  d <- d[d$goal_rate > 0, ]                       # log scale cannot show zero
  d$lo <- pmax(d$lo, 1e-3)
  ggplot2::ggplot(d, ggplot2::aes(x = mean_xg, y = goal_rate, shape = type)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), color = "gray50") +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::scale_shape_manual(values = c("Open Play" = 16, "Free Kick" = 1)) +
    ggplot2::labs(title = "Is StatsBomb xG calibrated? Open play vs direct free kicks",
                  x = "Mean xG in decile (log scale)",
                  y = "Goal rate, 95% interval (dashed = perfect calibration)", shape = NULL) +
    plain_theme()
}

# Where shots come from, by mixture component (half pitch, goal at the top)
plot_shot_map <- function(shots) {
  if (!"component" %in% names(shots)) stop("shots have no component column: run targets::tar_make().")
  d <- shots[shots$shot_y >= 0 & shots$shot_y <= 40 & abs(shots$shot_x) <= 34, ]
  d$component <- factor(COMPONENT_LABELS[as.character(d$component)], levels = COMPONENT_LABELS)
  box <- data.frame(xmin = c(-22, -10), xmax = c(22, 10), ymin = c(0, 0), ymax = c(18, 6))
  ggplot2::ggplot(d, ggplot2::aes(x = shot_x, y = shot_y)) +
    ggplot2::geom_bin_2d(ggplot2::aes(fill = ggplot2::after_stat(density)), binwidth = c(2, 2)) +
    ggplot2::geom_rect(data = box, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       inherit.aes = FALSE, fill = NA, color = "gray30", linewidth = 0.3) +
    ggplot2::annotate("segment", x = -4, xend = 4, y = 0, yend = 0, linewidth = 1.2) +
    ggplot2::annotate("point", x = 0, y = 12, size = 0.8) +
    ggplot2::scale_fill_gradient(low = "gray95", high = "black", name = "Share of the\ncomponent's shots") +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed() +
    ggplot2::facet_wrap(~ component) +
    ggplot2::labs(title = "Where shots come from, by shot-quality component",
                  x = "Lateral position (yards from centre)", y = "Distance from goal line (yards)") +
    plain_theme()
}

# ---- Stage 2 -------------------------------------------------------------------------

stint_length_shares <- function(stints, breaks = c(0, 5, 10, 20, 30, 45, Inf)) {
  bin <- cut(stints$duration_min, breaks, right = FALSE,
             labels = c("0-5", "5-10", "10-20", "20-30", "30-45", "45+"))
  rbind(
    data.frame(bin = levels(bin), measure = "Share of stints",
               share = as.numeric(table(bin)) / length(bin)),
    data.frame(bin = levels(bin), measure = "Share of minutes",
               share = as.numeric(tapply(stints$duration_min, bin, sum, default = 0)) /
                 sum(stints$duration_min))
  )
}

plot_stint_lengths <- function(stints) {
  d <- stint_length_shares(stints)
  d$bin <- factor(d$bin, levels = unique(d$bin))
  ggplot2::ggplot(d, ggplot2::aes(x = bin, y = 100 * share, fill = measure)) +
    ggplot2::geom_col(position = "dodge", width = 0.7) +
    ggplot2::scale_fill_manual(values = c("Share of stints" = "gray70", "Share of minutes" = "gray20")) +
    ggplot2::labs(title = "Most stints are short, but most minutes are in long stints",
                  x = "Stint length (minutes)", y = "Percent", fill = NULL) +
    plain_theme()
}

# Distribution of player-effect SEs (no shrinkage) against the team benchmarks.
# pdat = precision_data() output.
plot_player_se <- function(pdat) {
  se_p <- pct(pdat$se_player)
  se_p <- se_p[is.finite(se_p)]
  team_se <- pct(stats::median(pdat$se_team, na.rm = TRUE))
  team_sd <- pct(pdat$team_sd)
  ggplot2::ggplot(data.frame(se = se_p), ggplot2::aes(x = se)) +
    ggplot2::geom_histogram(bins = 50, fill = "gray70", color = NA) +
    ggplot2::geom_vline(xintercept = team_se, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = team_sd, linetype = "dotted") +
    ggplot2::labs(
      title = "How precisely could each player's effect be estimated?",
      x = sprintf("Player effect SE, %% shots per 90\n(dashed = typical team SE, %.1f%%; dotted = spread of team effects, %.1f%%)",
                  team_se, team_sd),
      y = "Players") +
    plain_theme() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 9))
}

# Shots per 90 by time bin and score state (segment table)
game_state_rates <- function(seg, min_minutes = 500) {
  seg$score_state <- pmax(pmin(seg$score_diff, 2L), -2L)
  d <- seg |>
    dplyr::group_by(time_bin, score_state) |>
    dplyr::summarise(minutes = sum(minutes), shots_per_90 = 90 * sum(n_shots) / sum(minutes),
                     .groups = "drop")
  d <- d[d$minutes >= min_minutes, ]
  d$time_bin <- factor(d$time_bin, levels = TIME_BINS)
  d$score_state <- factor(d$score_state, levels = -2:2,
                          labels = c("Down 2+", "Down 1", "Level", "Up 1", "Up 2+"))
  d
}

plot_game_state <- function(seg) {
  d <- game_state_rates(seg)
  ggplot2::ggplot(d, ggplot2::aes(x = time_bin, y = shots_per_90, group = score_state,
                                  color = score_state, shape = score_state)) +
    ggplot2::geom_line() + ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_grey(start = 0, end = 0.7) +
    ggplot2::labs(title = "Shot rate by score and time remaining",
                  x = "Period and 15-minute bin", y = "Open-play shots per 90 (attacking side)",
                  color = "Score", shape = "Score") +
    plain_theme()
}

# ---- Stage 3 -------------------------------------------------------------------------

plot_split_half <- function(sh) {
  r <- sapply(split(sh, sh$side), function(d) stats::cor(d$half1, d$half2))
  sh$panel <- sprintf("%s (r = %.2f, %d players)", sh$side, r[sh$side], as.integer(table(sh$side)[sh$side]))
  lim <- range(pct(c(sh$half1, sh$half2)))
  ggplot2::ggplot(sh, ggplot2::aes(x = pct(half1), y = pct(half2))) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(alpha = 0.25, size = 0.8) +
    ggplot2::coord_equal(xlim = lim, ylim = lim) +
    ggplot2::facet_wrap(~ panel) +
    ggplot2::labs(title = "Do player effects replicate across halves of the season?",
                  x = "Effect in odd-numbered matches (% shots per 90)",
                  y = "Effect in even-numbered matches") +
    plain_theme()
}

# Player effects by position group: individual spread vs group means (+/- 2 SE)
plot_effects_by_position <- function(eff, positions) {
  d <- dplyr::left_join(eff, positions[, c("player_id", "group")], by = "player_id")
  d <- d[!is.na(d$group), ]
  d$group <- factor(d$group, levels = GROUP_ORDER)
  d$side <- factor(d$side, levels = c("Offense", "Defense"))
  s <- d |>
    dplyr::group_by(side, group) |>
    dplyr::summarise(m = mean(effect), se = stats::sd(effect) / sqrt(dplyr::n()), .groups = "drop")
  ggplot2::ggplot(d, ggplot2::aes(x = group, y = pct(effect))) +
    ggplot2::geom_hline(yintercept = 0, color = "gray60") +
    ggplot2::geom_jitter(width = 0.25, height = 0, alpha = 0.15, size = 0.6) +
    ggplot2::geom_errorbar(data = s, ggplot2::aes(y = pct(m), ymin = pct(m - 2 * se), ymax = pct(m + 2 * se)),
                           width = 0.3, linewidth = 0.8) +
    ggplot2::geom_point(data = s, ggplot2::aes(y = pct(m)), size = 2.5) +
    ggplot2::facet_wrap(~ side, scales = "free_x") +
    ggplot2::labs(title = "Player effects by position (M3)",
                  x = "Position group",
                  y = "Effect, % shots per 90 (black = group mean +/- 2 SE)") +
    plain_theme()
}

# ---- Stage 3b ------------------------------------------------------------------------

plot_position_comparison <- function(pe) {
  d <- rbind(cbind(pe$score, controls = "Score only"),
             cbind(pe$score_time, controls = "Score x time"))
  d$Group <- factor(d$Group, levels = rev(GROUP_ORDER))
  d$Side <- factor(d$Side, levels = c("Offense", "Defense"))
  ggplot2::ggplot(d, ggplot2::aes(x = pct(effect), y = Group, shape = controls)) +
    ggplot2::geom_vline(xintercept = 0, color = "gray60") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = pct(effect - 1.96 * se), xmax = pct(effect + 1.96 * se)),
                            position = ggplot2::position_dodge(width = 0.5), color = "gray40") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5), size = 2.2) +
    ggplot2::scale_shape_manual(values = c("Score only" = 1, "Score x time" = 16)) +
    ggplot2::facet_wrap(~ Side) +
    ggplot2::labs(title = "Position effects with and without time controls (M4)",
                  x = "One more player of the group in place of a midfielder, % shots per 90 (95% interval)",
                  y = NULL, shape = "Controls") +
    plain_theme()
}
