# Checks behind the pipeline's modelling choices. Each returns a small table.
# Requires models.R.

# Decision: analyse open-play shots only.
# Goals vs StatsBomb xG by shot type. A ratio well above 1 (large z) means the
# xG model underrates that shot type. Run on shots loaded with open_play_only = FALSE.
xg_calibration_by_type <- function(shots_all) {
  shots_all |>
    dplyr::group_by(set_piece) |>
    dplyr::summarise(n = dplyr::n(), goals = sum(shot_made), expected = sum(xg),
                     .groups = "drop") |>
    dplyr::mutate(ratio = round(goals / expected, 2),
                  z = round((goals - expected) / sqrt(expected), 1),
                  expected = round(expected, 1)) |>
    dplyr::arrange(dplyr::desc(n))
}

# Attach each shot's mixture component (most likely) and the posterior
# probability of the top component (p_top, the big-chance probability).
# The mixture must have been fit on exactly these shots.
assign_components <- function(shots, mix) {
  k <- best_mixture_k(mix)
  if (k == 1) {
    shots$component <- 1L
    shots$p_top <- NA_real_
    return(shots)
  }
  f <- mix$fits[[as.character(k)]]
  if (nrow(f$posterior) != nrow(shots)) {
    stop("Mixture posterior does not align with shots; fit the mixture on these shots.")
  }
  post <- f$posterior[, order(f$mu), drop = FALSE]
  shots$component <- max.col(post, ties.method = "first")
  shots$p_top <- post[, k]
  shots
}

# Decision: the top component is a meaningful definition of a big chance.
# Components should differ in interpretable ways, and goals / xG near 1 shows
# StatsBomb's xG is calibrated within each component.
component_profile <- function(shots) {
  if (!"component" %in% names(shots)) stop("Run assign_components() first.")
  n_total <- nrow(shots)
  shots |>
    dplyr::group_by(component) |>
    dplyr::summarise(n = dplyr::n(),
                     share = round(dplyr::n() / n_total, 3),
                     median_xg = round(stats::median(xg), 4),
                     median_distance = round(stats::median(distance), 1),
                     pct_header = round(mean(body_part == "Head"), 3),
                     pct_first_time = round(mean(first_time), 3),
                     goal_rate = round(mean(shot_made), 3),
                     goals_over_xg = round(sum(shot_made) / sum(xg), 2),
                     .groups = "drop")
}

# Decision: fit the mixture once on pooled data and hold it fixed.
# Refit the k-component mixture within each group. Similar components across
# groups, and several starts agreeing (starts_at_best), indicate a stable fit.
mixture_by_group <- function(x, groups, k = 3, n_starts = 10) {
  parts <- split(x, groups)
  out <- do.call(rbind, lapply(names(parts), function(g) {
    m <- fit_xg_mixture(parts[[g]], k_max = k, n_starts = n_starts)
    comp <- m$components[m$components$k == k, ]
    data.frame(group = g, component = comp$component,
               weight = round(comp$weight, 3), median_xg = round(comp$median_xg, 4),
               sdlog = round(comp$sdlog, 2),
               bic_best_k = best_mixture_k(m),
               starts_at_best = m$fit_table$starts_at_best[m$fit_table$k == k],
               row.names = NULL)
  }))
  out
}

# Random split of matches into two halves (shots from one match stay together).
match_halves <- function(shots, seed = 10) {
  set.seed(seed)
  games <- unique(shots$game_id)
  half <- sample(games, length(games) %/% 2)
  ifelse(shots$game_id %in% half, "Half A", "Half B")
}
