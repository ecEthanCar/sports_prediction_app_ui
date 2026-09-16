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

stack_xg_matches <- function(matches_with_form, team_season = NULL, window = NULL) {
  if (is.null(window)) window <- attr(matches_with_form, "window")
  rc <- function(side, what) sprintf("%s_roll%d_%s", side, window, what)
  
  season_levels <- sort(unique(as.character(matches_with_form$season)))
  n_seasons <- length(season_levels)
  if (is.null(team_season)) team_season <- n_seasons > 1L
  
  m <- matches_with_form |> mutate(season = as.character(season), home_lab = if (team_season) paste(home_team, season, sep = "@") else home_team, away_lab = if (team_season) paste(away_team, season, sep = "@") else away_team)
  
  lvl <- bind_rows(m |> transmute(team = home_team, season, lab = home_lab), m |> transmute(team = away_team, season, lab = away_lab)) |> distinct() |> arrange(season, team)
  if (!team_season) lvl <- lvl |> distinct(lab, .keep_all = TRUE)
  
  team_levels <- lvl$lab
  team_groups <- if (team_season) lvl$season else rep("all", nrow(lvl))
  
  side <- function(d, is_home) {
    d |> transmute(
      match_id, date, season, season_f = factor(season, levels = season_levels), home = if (is_home) 1 else 0,
      att_name = if (is_home) home_team else away_team, def_name = if (is_home) away_team else home_team,
      att = factor(if (is_home) home_lab else away_lab, levels = team_levels), def = factor(if (is_home) away_lab else home_lab, levels = team_levels),
      xg = if (is_home) home_xg else away_xg,
      att_roll_xg = if (is_home) .data[[rc("home", "xg_for")]] else .data[[rc("away", "xg_for")]],
      def_roll_xg_against = if (is_home) .data[[rc("away", "xg_against")]] else .data[[rc("home", "xg_against")]]
    )
  }
  
  stacked <- bind_rows(side(m, TRUE), side(m, FALSE)) |> arrange(match_id, desc(home)) |> mutate(match_f = factor(match_id))
  
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