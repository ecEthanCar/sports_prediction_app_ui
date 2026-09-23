# data_pipeline.R
#
# Shared data layer for the EPL goal and xG models.
#
#   Section A: Understat shot loading and filtering (single source of truth
#              for which shots count, so every analysis uses the same data)
#   Section B: Match-level tables (goals + non-penalty xG + shot counts)
#   Section C: Rolling form (optional; used by the goal models)
#   Section D: Stacking into team-match rows with sum-to-zero team contrasts
#   Section E: One-call data prep for the xG decomposition analyses
#   Section F: Small statistical and plotting helpers
#
# Legacy note: fetch_and_cache_xg() is kept unchanged for the older goal-model
# scripts. It sums ALL shots (penalties and own goals included) and drops any
# match where one side had no shots. The xG analyses use prepare_xg_data().

library(dplyr)
library(tidyr)
library(readr)
library(zoo)

UNDERSTAT_SHOT_FILES <- c(
  "EPL" = "epl_shot_data.rds", "La liga" = "la_liga_shot_data.rds",
  "Bundesliga" = "bundesliga_shot_data.rds", "Serie A" = "serie_a_shot_data.rds",
  "Ligue 1" = "ligue_1_shot_data.rds", "RFPL" = "rfpl_shot_data.rds"
)
RELEASE_BASE <- "https://github.com/JaseZiv/worldfootballR_data/releases/download/understat_shots/"

# ============================================================================
# Section A: Shots
# ============================================================================

# Load the raw Understat shot file (downloading once), with a clean `side`
# column ("h"/"a"), numeric season, and Date.
load_understat_shots <- function(league_name = "EPL", data_dir = "data") {
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  shots_rds <- file.path(data_dir, UNDERSTAT_SHOT_FILES[[league_name]])
  if (!file.exists(shots_rds)) {
    utils::download.file(paste0(RELEASE_BASE, UNDERSTAT_SHOT_FILES[[league_name]]),
                         shots_rds, mode = "wb", quiet = TRUE)
  }
  shots <- readRDS(shots_rds)
  side_cols <- intersect(c("h_a", "home_away"), names(shots))
  shots$side <- Reduce(dplyr::coalesce, unname(as.list(shots[side_cols])))
  shots |>
    mutate(
      season = as.numeric(season),
      date = as.Date(date),
      xG = as.numeric(xG)
    )
}

# The shots every xG model uses. Own goals are always removed (Understat
# assigns them xG = 0 and they are not shots by the attacking team).
# Penalties are removed by default: they have a fixed xG (~0.76), are driven by
# referee decisions rather than chance creation, and would add an artificial
# spike to the shot-quality distribution.
filter_model_shots <- function(shots, seasons, exclude_penalties = TRUE) {
  out <- shots |>
    filter(season %in% seasons, result != "OwnGoal", !is.na(xG))
  if (exclude_penalties) out <- filter(out, situation != "Penalty")
  out
}

# ============================================================================
# Section B: Match table
# ============================================================================

# One row per match with goals, xG and shot counts for each side.
# Match metadata comes from ALL shots (so a match is kept even if one side
# had zero model shots); xG and counts come from the filtered shots, with
# zero-fill for a side that took no qualifying shots. This preserves the exact
# zeros that the Tweedie model is designed to handle.
build_xg_matches <- function(shots_all, shots_model, seasons) {
  meta <- shots_all |>
    filter(season %in% seasons) |>
    distinct(match_id, .keep_all = TRUE) |>
    transmute(
      understat_id = match_id, date, season = as.character(season),
      home_team, away_team,
      home_goals = as.integer(home_goals), away_goals = as.integer(away_goals)
    )

  agg <- shots_model |>
    group_by(match_id, side) |>
    summarise(xg = sum(xG), n_shots = n(), .groups = "drop") |>
    pivot_wider(names_from = side, values_from = c(xg, n_shots))

  meta |>
    left_join(agg, by = c("understat_id" = "match_id")) |>
    rename(home_xg = xg_h, away_xg = xg_a,
           home_n_shots = n_shots_h, away_n_shots = n_shots_a) |>
    mutate(across(c(home_xg, away_xg), ~ coalesce(.x, 0)),
           across(c(home_n_shots, away_n_shots), ~ as.integer(coalesce(.x, 0L)))) |>
    arrange(season, date, home_team) |>
    mutate(match_id = row_number())
}

# Legacy loader (see note at top). Unchanged so older scripts reproduce.
fetch_and_cache_xg <- function(league_name = "EPL", season_year = 2021, force_refresh = FALSE, data_dir = "data") {
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  cache_file <- file.path(data_dir, sprintf("xg_%s_%s.csv", gsub(" ", "_", league_name), season_year))

  if (file.exists(cache_file) && !force_refresh) {
    return(readr::read_csv(cache_file, col_types = readr::cols(season = readr::col_character())))
  }

  shots_rds <- file.path(data_dir, UNDERSTAT_SHOT_FILES[[league_name]])
  if (!file.exists(shots_rds)) {
    utils::download.file(paste0(RELEASE_BASE, UNDERSTAT_SHOT_FILES[[league_name]]), shots_rds, mode = "wb", quiet = TRUE)
  }

  shots <- readRDS(shots_rds)
  shots$side <- Reduce(dplyr::coalesce, unname(as.list(shots[intersect(c("h_a", "home_away"), names(shots))])))

  season_shots <- dplyr::filter(shots, as.numeric(season) == as.numeric(season_year))
  team_xg <- season_shots |>
    dplyr::group_by(match_id, side) |>
    dplyr::summarise(xg = sum(xG, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = side, values_from = xg, names_prefix = "xg_")

  clean_data <- season_shots |>
    dplyr::select(match_id, date, home_team, away_team, home_goals, away_goals) |>
    dplyr::distinct(match_id, .keep_all = TRUE) |>
    dplyr::inner_join(team_xg, by = "match_id") |>
    dplyr::transmute(
      date = as.Date(date), season = as.character(season_year),
      home_team, away_team, home_goals = as.integer(home_goals), away_goals = as.integer(away_goals),
      home_xg = xg_h, away_xg = xg_a
    ) |>
    dplyr::arrange(date, home_team) |>
    dplyr::filter(!is.na(home_xg), !is.na(away_xg))

  readr::write_csv(clean_data, cache_file)
  return(clean_data)
}

# ============================================================================
# Section C: Rolling form (optional)
# ============================================================================

# Adds lagged rolling sums of goals and xG for each side. Rows without a full
# window (the first `window` matches of each team-season) are dropped, so only
# call this when the model actually uses the form covariates.
compute_rolling_form <- function(xg_matches, window = 5) {
  xg_matches <- xg_matches |> arrange(season, date) |> mutate(match_id = row_number())

  team_matches <- bind_rows(
    xg_matches |> transmute(match_id, season, date, team = home_team, gf = home_goals, ga = away_goals, xg_for = home_xg, xg_against = away_xg),
    xg_matches |> transmute(match_id, season, date, team = away_team, gf = away_goals, ga = home_goals, xg_for = away_xg, xg_against = home_xg)
  )

  team_form <- team_matches |>
    arrange(team, season, date) |>
    group_by(team, season) |>
    mutate(
      roll_gf = lag(zoo::rollsumr(gf, k = window, fill = NA)),
      roll_ga = lag(zoo::rollsumr(ga, k = window, fill = NA)),
      roll_xg_for = lag(zoo::rollsumr(xg_for, k = window, fill = NA)),
      roll_xg_against = lag(zoo::rollsumr(xg_against, k = window, fill = NA))
    ) |>
    ungroup() |>
    dplyr::select(match_id, team, starts_with("roll_")) |>
    rename_with(~ paste0("roll", window, "_", sub("^roll_", "", .x)), starts_with("roll_"))

  roll_cols <- setdiff(names(team_form), c("match_id", "team"))

  out <- xg_matches |>
    left_join(team_form, by = c("match_id", "home_team" = "team")) |> rename_with(~ paste0("home_", .x), all_of(roll_cols)) |>
    left_join(team_form, by = c("match_id", "away_team" = "team")) |> rename_with(~ paste0("away_", .x), all_of(roll_cols))

  keep <- stats::complete.cases(out[, c(paste0("home_", roll_cols), paste0("away_", roll_cols))])
  out <- out[keep, , drop = FALSE]
  attr(out, "window") <- window
  return(out)
}

# ============================================================================
# Section D: Stacking
# ============================================================================

# Sum-to-zero contrasts applied separately within each group (season), so
# team-season effects are identified relative to that season's average.
contr_sum_within <- function(level_labels, groups) {
  g <- factor(groups, levels = unique(groups))
  idx <- split(seq_along(level_labels), g)
  blocks <- lapply(idx, function(i) stats::contr.sum(length(i)))
  ncols <- vapply(blocks, ncol, integer(1))
  col_names <- unlist(lapply(seq_along(idx), function(k) if (length(idx) == 1L) as.character(seq_len(ncols[k])) else paste0(names(idx)[k], ".", seq_len(ncols[k]))), use.names = FALSE)

  M <- matrix(0, nrow = length(level_labels), ncol = sum(ncols), dimnames = list(level_labels, col_names))
  offset <- 0L
  for (k in seq_along(idx)) {
    M[idx[[k]], offset + seq_len(ncols[k])] <- blocks[[k]]
    offset <- offset + ncols[k]
  }
  return(M)
}

# Two rows per match (one per attacking side). Carries shot counts when the
# match table has them and rolling form only when compute_rolling_form() was
# run, so the same function serves both the goal and xG analyses.
stack_xg_matches <- function(matches, team_season = NULL, window = NULL) {
  if (is.null(window)) window <- attr(matches, "window")
  has_form <- !is.null(window)
  has_shots <- all(c("home_n_shots", "away_n_shots") %in% names(matches))
  rc <- function(side, what) sprintf("%s_roll%d_%s", side, window, what)

  season_levels <- sort(unique(as.character(matches$season)))
  n_seasons <- length(season_levels)
  if (is.null(team_season)) team_season <- n_seasons > 1L

  m <- matches |> mutate(
    season = as.character(season),
    home_lab = if (team_season) paste(home_team, season, sep = "@") else home_team,
    away_lab = if (team_season) paste(away_team, season, sep = "@") else away_team
  )

  lvl <- bind_rows(m |> transmute(team = home_team, season, lab = home_lab),
                   m |> transmute(team = away_team, season, lab = away_lab)) |>
    distinct() |> arrange(season, team)
  if (!team_season) lvl <- lvl |> distinct(lab, .keep_all = TRUE)

  team_levels <- lvl$lab
  team_groups <- if (team_season) lvl$season else rep("all", nrow(lvl))

  side <- function(d, is_home) {
    s <- if (is_home) "home" else "away"
    o <- if (is_home) "away" else "home"
    out <- d |> transmute(
      match_id, date, season, season_f = factor(season, levels = season_levels),
      home = if (is_home) 1 else 0,
      att_name = .data[[paste0(s, "_team")]], def_name = .data[[paste0(o, "_team")]],
      att = factor(.data[[paste0(s, "_lab")]], levels = team_levels),
      def = factor(.data[[paste0(o, "_lab")]], levels = team_levels),
      goals = .data[[paste0(s, "_goals")]],
      xg = .data[[paste0(s, "_xg")]]
    )
    if (has_shots) out$n_shots <- d[[paste0(s, "_n_shots")]]
    if (has_form) {
      out$att_roll_xg <- d[[rc(s, "xg_for")]]
      out$def_roll_xg_against <- d[[rc(o, "xg_against")]]
    }
    out
  }

  stacked <- bind_rows(side(m, TRUE), side(m, FALSE)) |>
    arrange(match_id, desc(home)) |>
    mutate(match_f = factor(match_id))

  if (has_shots) {
    stacked <- stacked |> mutate(xg_mean_shot = ifelse(n_shots > 0, xg / n_shots, NA_real_))
  }

  C_team <- contr_sum_within(team_levels, team_groups)
  stacked <- as.data.frame(stacked)
  stacked$ATT <- C_team[as.integer(stacked$att), , drop = FALSE]
  stacked$DEF <- C_team[as.integer(stacked$def), , drop = FALSE]

  if (n_seasons > 1L) {
    C_season <- stats::contr.sum(n_seasons)
    dimnames(C_season) <- list(season_levels, season_levels[-n_seasons])
    contrasts(stacked$season_f) <- C_season
  }

  attr(stacked, "team_levels") <- team_levels
  attr(stacked, "contrasts_team") <- C_team
  attr(stacked, "window") <- window
  return(stacked)
}

# ============================================================================
# Section E: One-call prep for the xG decomposition
# ============================================================================

# Returns the model shots, the match table, and the stacked team-match rows,
# all built from the same filtered shots.
prepare_xg_data <- function(league = "EPL", seasons = 2014:2024,
                            exclude_penalties = TRUE, data_dir = "data") {
  shots_all <- load_understat_shots(league, data_dir)
  shots <- filter_model_shots(shots_all, seasons, exclude_penalties)
  matches <- build_xg_matches(shots_all, shots, seasons)
  stacked <- stack_xg_matches(matches)
  list(shots = shots, matches = matches, stacked = stacked)
}

# The fixed part of every model in this project: season, home advantage,
# and team-season attack and defence effects.
model_rhs <- function(stacked) {
  rhs <- c("home", "ATT", "DEF")
  if (nlevels(stacked$season_f) > 1L) rhs <- c("season_f", rhs)
  rhs
}

# ============================================================================
# Section F: Helpers
# ============================================================================

# Method-of-moments Gamma shape.
gamma_shape_mom <- function(x) mean(x)^2 / stats::var(x)

# In a compound Poisson-Gamma, Gamma shape alpha maps to Tweedie index p.
p_from_shape <- function(alpha) (alpha + 2) / (alpha + 1)

# Binned calibration: mean actual vs mean predicted within quantile bins of the
# prediction, with a normal-approximation 95% CI for the bin mean.
calibration_bins <- function(pred, actual, n_bins = 10) {
  tibble(pred = pred, actual = actual) |>
    mutate(bin = dplyr::ntile(pred, n_bins)) |>
    group_by(bin) |>
    summarise(
      n = n(),
      mean_pred = mean(pred),
      mean_actual = mean(actual),
      se = stats::sd(actual) / sqrt(n()),
      .groups = "drop"
    ) |>
    mutate(lo = mean_actual - 1.96 * se, hi = mean_actual + 1.96 * se)
}

XG_COLORS <- c(green = "#1E7A5A", navy = "#0F2233", gold = "#F2B705",
               slate = "#5A6B7B", red = "#B23A48", grey = "grey60")

theme_xg <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 3),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}
