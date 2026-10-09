# Team-match table and design pieces shared by all models.

# Sum-to-zero contrasts within each group (here: teams within a league).
# Copied from the Understat pipeline so this pipeline stands alone.
contr_sum_within <- function(level_labels, groups) {
  g <- factor(groups, levels = unique(groups))
  idx <- split(seq_along(level_labels), g)
  if (any(lengths(idx) < 2L)) stop("Each group needs at least two levels for sum-to-zero contrasts.")
  blocks <- lapply(idx, function(i) stats::contr.sum(length(i)))
  ncols <- vapply(blocks, ncol, integer(1))
  col_names <- unlist(lapply(seq_along(idx), function(k) {
    if (length(idx) == 1L) as.character(seq_len(ncols[k])) else paste0(names(idx)[k], ".", seq_len(ncols[k]))
  }), use.names = FALSE)

  M <- matrix(0, nrow = length(level_labels), ncol = sum(ncols),
              dimnames = list(level_labels, col_names))
  offset <- 0L
  for (k in seq_along(idx)) {
    M[idx[[k]], offset + seq_len(ncols[k])] <- blocks[[k]]
    offset <- offset + ncols[k]
  }
  M
}

# Two rows per match (one per attacking side). Built from the matches table so
# a team that never shoots still gets a row with n_shots = 0.
build_team_match <- function(matches, shots) {
  sides <- dplyr::bind_rows(
    dplyr::transmute(matches, game_id, league, match_date, home = 1L,
                     att_team = home_team, def_team = away_team),
    dplyr::transmute(matches, game_id, league, match_date, home = 0L,
                     att_team = away_team, def_team = home_team)
  )

  totals <- shots |>
    dplyr::group_by(game_id, att_team = off_team) |>
    dplyr::summarise(n_shots = dplyr::n(), npxg = sum(xg), .groups = "drop")

  tm <- dplyr::left_join(sides, totals, by = c("game_id", "att_team")) |>
    dplyr::mutate(
      n_shots = dplyr::coalesce(n_shots, 0L),
      npxg = dplyr::coalesce(npxg, 0),
      xg_mean_shot = ifelse(n_shots > 0, npxg / n_shots, NA_real_)
    ) |>
    dplyr::arrange(league, match_date, game_id, dplyr::desc(home))

  # Conservation checks
  if (nrow(tm) != 2L * nrow(matches)) stop("Team-match table does not have two rows per match.")
  if (sum(tm$n_shots) != nrow(shots)) stop("Shot count not conserved in aggregation (team names may not match).")
  if (abs(sum(tm$npxg) - sum(shots$xg)) > 1e-6) stop("npxG not conserved in aggregation.")

  # Team factors and within-league sum-to-zero contrasts
  team_tbl <- dplyr::arrange(dplyr::distinct(tm, league, team = att_team), league, team)
  team_levels <- paste(team_tbl$league, team_tbl$team, sep = "@")
  C_team <- contr_sum_within(team_levels, team_tbl$league)

  tm <- as.data.frame(tm)
  tm$att <- factor(paste(tm$league, tm$att_team, sep = "@"), levels = team_levels)
  tm$def <- factor(paste(tm$league, tm$def_team, sep = "@"), levels = team_levels)
  if (anyNA(tm$att) || anyNA(tm$def)) stop("Unmatched team levels when building contrasts.")
  tm$ATT <- C_team[as.integer(tm$att), , drop = FALSE]
  tm$DEF <- C_team[as.integer(tm$def), , drop = FALSE]

  tm$league_f <- factor(tm$league)
  if (nlevels(tm$league_f) > 1L) {
    stats::contrasts(tm$league_f) <- stats::contr.sum(nlevels(tm$league_f))
  }
  tm$match_f <- factor(tm$game_id)

  attr(tm, "team_levels") <- team_levels
  attr(tm, "contrasts_team") <- C_team
  tm
}

# Right-hand side shared by every team-match model. League effect is dropped
# automatically if only one league is present.
model_rhs <- function(tm) {
  rhs <- c("home", "ATT", "DEF")
  if (nlevels(tm$league_f) > 1L) rhs <- c("league_f", rhs)
  rhs
}
