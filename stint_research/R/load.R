# Load and validate the files written by build_shots.py.
# Structural problems stop the pipeline; value problems warn and are reported.

DATA_DIR <- file.path("data", "raw")   # written by build/build_shots.py
EXPECTED_MATCHES <- 380L
EXPECTED_TEAMS <- 20L

load_matches <- function(path = file.path(DATA_DIR, "matches_1516.csv")) {
  m <- readr::read_csv(
    path, show_col_types = FALSE,
    col_types = readr::cols(
      game_id = readr::col_integer(),
      match_date = readr::col_date(),
      home_team_id = readr::col_integer(),
      away_team_id = readr::col_integer(),
      home_score = readr::col_integer(),
      away_score = readr::col_integer(),
      .default = readr::col_character()
    )
  )
  validate_matches(m)
  m
}

# Ligue 1 has 377 of 380 matches in the open data; the warning is intentional.
validate_matches <- function(m) {
  if (anyDuplicated(m$game_id)) stop("Duplicated game_id in matches table.")
  if (any(m$home_team == m$away_team)) stop("A match has identical home and away teams.")

  per_league <- table(m$league)
  short <- per_league[per_league < EXPECTED_MATCHES]
  if (length(short)) {
    warning("Leagues with fewer than ", EXPECTED_MATCHES, " matches: ",
            paste(names(short), short, sep = " = ", collapse = ", "),
            ". Team-match counts for these teams will be unequal.")
  }

  teams <- dplyr::distinct(dplyr::bind_rows(
    dplyr::transmute(m, league, team = home_team),
    dplyr::transmute(m, league, team = away_team)
  ))
  n_teams <- table(teams$league)
  if (any(n_teams != EXPECTED_TEAMS)) {
    stop("Expected ", EXPECTED_TEAMS, " teams per league; got: ",
         paste(names(n_teams), n_teams, sep = " = ", collapse = ", "))
  }
  multi <- unique(teams$team[duplicated(teams$team)])
  if (length(multi)) stop("Team name appears in more than one league: ", paste(multi, collapse = ", "))
  invisible(TRUE)
}

shot_col_types <- function() {
  od <- c(paste0("O", 1:11), paste0("D", 1:11))
  spec <- c(
    list(
      league = readr::col_character(), season = readr::col_character(),
      game_id = readr::col_integer(), event_id = readr::col_character(),
      match_date = readr::col_date(), period = readr::col_integer(),
      minute = readr::col_integer(), t_sec = readr::col_double(),
      stint_id = readr::col_character(), segment_id = readr::col_character(),
      poss_num = readr::col_integer(),
      home_off = readr::col_integer(), off_team = readr::col_character(),
      def_team = readr::col_character(), shooter = readr::col_character(),
      shooter_id = readr::col_integer(), outcome = readr::col_character(),
      shot_made = readr::col_integer(), poss_goals = readr::col_integer(),
      poss_end = readr::col_integer(), shot_x = readr::col_double(),
      shot_y = readr::col_double(), distance = readr::col_double(),
      angle = readr::col_double(), xg = readr::col_double(),
      set_piece = readr::col_character(), body_part = readr::col_character(),
      technique = readr::col_character(), play_pattern = readr::col_character(),
      first_time = readr::col_integer(), under_pressure = readr::col_integer(),
      strength = readr::col_character()
    ),
    stats::setNames(rep(list(readr::col_character()), length(od)), od)
  )
  do.call(readr::cols, c(spec, list(.default = readr::col_character())))
}

# Shots table with only open-play shots by default. StatsBomb's 2015/16
# open-data xG is poorly calibrated for direct set-piece shots (see
# xg_calibration_by_type() in checks.R), so penalties, direct free kicks, and
# direct corners are dropped. Set open_play_only = FALSE to keep every shot.
load_shots <- function(matches, path = file.path(DATA_DIR, "shots_1516.csv"),
                       open_play_only = TRUE) {
  s <- readr::read_csv(path, show_col_types = FALSE, col_types = shot_col_types())

  # Structural checks
  if (anyDuplicated(s$event_id)) stop("Duplicated event_id in shots table.")
  missing_games <- setdiff(unique(s$game_id), matches$game_id)
  if (length(missing_games)) {
    stop(length(missing_games), " game_id values in shots are not in the matches table.")
  }
  chk <- dplyr::inner_join(s[, c("game_id", "home_off", "off_team", "def_team")],
                           matches[, c("game_id", "home_team", "away_team")],
                           by = "game_id")
  bad_team <- ifelse(chk$home_off == 1L,
                     chk$off_team != chk$home_team | chk$def_team != chk$away_team,
                     chk$off_team != chk$away_team | chk$def_team != chk$home_team)
  if (any(bad_team)) stop(sum(bad_team), " shots have off/def teams inconsistent with the matches table.")

  # Value checks
  bad_xg <- !is.finite(s$xg) | s$xg <= 0 | s$xg >= 1
  if (any(bad_xg)) {
    warning(sprintf("Dropping %d shots with xg outside (0, 1).", sum(bad_xg)))
    s <- s[!bad_xg, ]
  }

  dropped <- table(s$set_piece[s$set_piece != "Open Play"])
  if (open_play_only) s <- s[s$set_piece == "Open Play", ]
  attr(s, "dropped_by_type") <- if (open_play_only) dropped else table(character(0))
  s
}
