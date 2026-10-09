# Stage 2: stint-level data. A stint is a stretch of play with the same players
# on the pitch; a segment is a stint split further at goals and at 15-minute
# game-clock edges, so score and time bin are constant within it.
# Requires load.R (DATA_DIR) and dplyr.

STINT_HOME_COLS <- paste0("H", 1:11)
STINT_AWAY_COLS <- paste0("A", 1:11)
MATCH_MINUTES_RANGE <- c(88, 105)   # total stint minutes outside this range warn

stint_col_types <- function() {
  spec <- c(
    list(
      league = readr::col_character(), season = readr::col_character(),
      game_id = readr::col_integer(), stint_id = readr::col_character(),
      stint_num = readr::col_integer(), period = readr::col_integer(),
      start_sec = readr::col_double(), end_sec = readr::col_double(),
      duration_min = readr::col_double(), home_team = readr::col_character(),
      away_team = readr::col_character(), home_goals = readr::col_integer(),
      away_goals = readr::col_integer(), n_home = readr::col_integer(),
      n_away = readr::col_integer()
    ),
    stats::setNames(rep(list(readr::col_integer()), 22), c(STINT_HOME_COLS, STINT_AWAY_COLS))
  )
  do.call(readr::cols, c(spec, list(.default = readr::col_character())))
}

load_stints <- function(matches, path = file.path(DATA_DIR, "stints_1516.csv")) {
  s <- readr::read_csv(path, show_col_types = FALSE, col_types = stint_col_types())
  validate_stints(s, matches)
  s
}

segment_col_types <- function() {
  ct <- stint_col_types()
  ct$cols$stint_num <- NULL
  ct$cols$segment_id <- readr::col_character()
  ct$cols$segment_num <- readr::col_integer()
  ct$cols$time_bin <- readr::col_character()
  ct
}

load_segments <- function(matches, stints, path = file.path(DATA_DIR, "segments_1516.csv")) {
  if (!file.exists(path)) stop("No segments file: run targets::tar_make() to rebuild the data (uses the cache).")
  g <- readr::read_csv(path, show_col_types = FALSE, col_types = segment_col_types())
  validate_stints(g, matches, id_col = "segment_id")
  if (!setequal(g$stint_id, stints$stint_id)) stop("Segments and stints cover different stint_ids.")
  per_stint <- tapply(g$duration_min, g$stint_id, sum)
  if (any(abs(per_stint[stints$stint_id] - stints$duration_min) > 1e-3)) {
    stop("Segment minutes do not add up to stint minutes.")
  }
  g
}

validate_stints <- function(s, matches, id_col = "stint_id") {
  if (anyDuplicated(s[[id_col]])) stop("Duplicated ", id_col, " in table.")
  unknown <- setdiff(unique(s$game_id), matches$game_id)
  if (length(unknown)) stop(length(unknown), " stint game_id values are not in the matches table.")
  absent <- setdiff(matches$game_id, s$game_id)
  if (length(absent)) stop(length(absent), " matches have no stints.")
  if (any(s$duration_min <= 0)) stop("Stints with non-positive duration.")
  if (any(s$n_home < 7 | s$n_home > 11 | s$n_away < 7 | s$n_away > 11)) {
    stop("Stints with fewer than 7 or more than 11 players on a side.")
  }
  n_h <- rowSums(!is.na(as.matrix(s[, STINT_HOME_COLS])))
  n_a <- rowSums(!is.na(as.matrix(s[, STINT_AWAY_COLS])))
  if (any(n_h != s$n_home) || any(n_a != s$n_away)) {
    stop("Player columns disagree with n_home / n_away.")
  }
  per_match <- tapply(s$duration_min, s$game_id, sum)
  odd <- per_match[per_match < MATCH_MINUTES_RANGE[1] | per_match > MATCH_MINUTES_RANGE[2]]
  if (length(odd)) {
    warning(length(odd), " matches have total stint minutes outside ",
            MATCH_MINUTES_RANGE[1], " to ", MATCH_MINUTES_RANGE[2], ": ",
            paste(names(odd), round(odd, 1), sep = " = ", collapse = ", "))
  }
  invisible(TRUE)
}

load_players <- function(path = file.path(DATA_DIR, "players_1516.csv")) {
  p <- readr::read_csv(path, show_col_types = FALSE,
                       col_types = readr::cols(player_id = readr::col_integer(),
                                               is_goalkeeper = readr::col_integer(),
                                               .default = readr::col_character()))
  if (!all(c("primary_position", "is_goalkeeper") %in% names(p))) {
    stop("players table has no positions: run targets::tar_make() to rebuild the data (uses the cache).")
  }
  p
}

# Rows of a player-id matrix -> list of integer vectors, blanks removed
row_players <- function(m) {
  m <- as.matrix(m)
  lapply(seq_len(nrow(m)), function(i) {
    v <- m[i, ]
    unname(as.integer(v[!is.na(v)]))
  })
}

# Two rows per stint (or segment), one per attacking side, with open-play shot
# totals. shots: open-play shots with stint_id / segment_id and p_top
# (stage1_outputs.rds). team_match: the stage 1 team-match table; totals must
# reproduce it. id_col = "segment_id" builds the segment table, which also
# carries time_bin; its stint_id column still identifies the lineup.
build_stint_table <- function(stints, shots, team_match, id_col = "stint_id") {
  if (!all(c(id_col, "p_top") %in% names(shots))) {
    stop("shots must include ", id_col, " and p_top: run targets::tar_make().")
  }
  if (anyNA(shots[[id_col]])) stop("Some shots have no ", id_col, ".")
  unknown <- setdiff(unique(shots[[id_col]]), stints[[id_col]])
  if (length(unknown)) stop(length(unknown), " shot ", id_col, " values are not in the table.")

  # The argument is named is_home, not home: inside tibble(), the new "home"
  # column would otherwise shadow it in the expressions that follow.
  side_rows <- function(is_home) {
    att_cols <- if (is_home) STINT_HOME_COLS else STINT_AWAY_COLS
    def_cols <- if (is_home) STINT_AWAY_COLS else STINT_HOME_COLS
    out <- tibble::tibble(
      league = stints$league, game_id = stints$game_id, unit_id = stints[[id_col]],
      stint_id = stints$stint_id, period = stints$period, start_sec = stints$start_sec,
      minutes = stints$duration_min, home = as.integer(is_home),
      att_team = if (is_home) stints$home_team else stints$away_team,
      def_team = if (is_home) stints$away_team else stints$home_team,
      score_diff = if (is_home) stints$home_goals - stints$away_goals else stints$away_goals - stints$home_goals,
      n_att = if (is_home) stints$n_home else stints$n_away,
      n_def = if (is_home) stints$n_away else stints$n_home,
      att_players = row_players(stints[, att_cols]),
      def_players = row_players(stints[, def_cols])
    )
    if ("time_bin" %in% names(stints)) out$time_bin <- stints$time_bin
    out
  }

  totals <- shots |>
    dplyr::mutate(unit_id = .data[[id_col]]) |>
    dplyr::group_by(unit_id, att_team = off_team) |>
    dplyr::summarise(n_shots = dplyr::n(), xg = sum(xg), big_chances = sum(p_top),
                     .groups = "drop")

  st <- dplyr::bind_rows(side_rows(TRUE), side_rows(FALSE)) |>
    dplyr::left_join(totals, by = c("unit_id", "att_team")) |>
    dplyr::mutate(n_shots = dplyr::coalesce(n_shots, 0L),
                  xg = dplyr::coalesce(xg, 0),
                  big_chances = dplyr::coalesce(big_chances, 0),
                  strength = paste0(n_att, "v", n_def)) |>
    dplyr::arrange(league, game_id, period, start_sec, dplyr::desc(home))

  # Conservation: every shot is counted once, and stint totals summed by match
  # and side reproduce the stage 1 team-match table exactly
  if (sum(st$n_shots) != nrow(shots)) stop("Shot count not conserved in stint table (team names may not match).")
  by_tm <- st |>
    dplyr::group_by(game_id, att_team) |>
    dplyr::summarise(n_shots = sum(n_shots), xg = sum(xg), .groups = "drop")
  tm <- as.data.frame(team_match)[, c("game_id", "att_team", "n_shots", "npxg")]
  chk <- dplyr::inner_join(by_tm, tm, by = c("game_id", "att_team"), suffix = c("_stint", "_tm"))
  if (nrow(chk) != nrow(tm)) stop("Stint table and team-match table cover different team-matches.")
  if (any(chk$n_shots_stint != chk$n_shots_tm) || any(abs(chk$xg - chk$npxg) > 1e-6)) {
    stop("Stint totals do not reproduce the team-match table.")
  }
  st
}

# Overview of the stint table for the reports
stint_overview <- function(stints, st) {
  per_match <- table(stints$game_id)
  data.frame(
    Quantity = c("Stints", "Stints per match (mean / min / max)",
                 "Stint length in minutes (median / 10th / 90th percentile)",
                 "Share of minutes at 11 v 11",
                 "Distinct players appearing"),
    Value = c(
      fmt(nrow(stints), 0),
      sprintf("%s / %d / %d", fmt(mean(per_match), 1), min(per_match), max(per_match)),
      sprintf("%s / %s / %s", fmt(stats::median(stints$duration_min), 1),
              fmt(stats::quantile(stints$duration_min, 0.1), 1),
              fmt(stats::quantile(stints$duration_min, 0.9), 1)),
      sprintf("%s%%", fmt(100 * sum(stints$duration_min[stints$n_home == 11 & stints$n_away == 11]) /
                           sum(stints$duration_min), 1)),
      fmt(length(unique(unlist(c(st$att_players, st$def_players)))), 0)
    )
  )
}
