# ==============================================================================
# data.R  -- league discovery and loading
# No Shiny code in this file. Everything here works from a plain R console.
# ==============================================================================

#' Registry of candidate leagues.
#'
#' Adding a league is one row. `tier_filter` applies only to datasets that
#' carry a `tier` column (the engsoccerdata-style ones); NA means no filter.
#' `sport` is unused today but is the hook for the NBA/NHL/cricket modules:
#' the app shell is sport-agnostic, only R/model.R is soccer-specific.
league_registry <- function() {
  tibble::tribble(
    ~league,                  ~pkg,        ~dataset,  ~sport,   ~tier_filter,
    "Italian Serie A",        "footBayes", "italy",   "soccer", NA_real_,
    "English Premier League", "footBayes", "england", "soccer", 1,
    "Spanish La Liga",        "footBayes", "spain",   "soccer", NA_real_,
    "German Bundesliga",      "footBayes", "germany", "soccer", 1
  )
}

#' Does `pkg` actually ship a dataset called `name`?
#' Uses the package index rather than loading the data, so startup stays cheap
#' and a registry row for a dataset that does not exist is simply skipped.
dataset_exists <- function(pkg, name) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
  items <- tryCatch(
    utils::data(package = pkg)$results[, "Item"],
    error = function(e) character(0)
  )
  # index entries can read "italy (soccer)" -- strip the parenthetical
  items <- sub("\\s+\\(.*\\)$", "", items)
  name %in% items
}

#' Registry rows whose datasets are installed. Empty tibble if none are.
available_leagues <- function(registry = league_registry()) {
  keep <- vapply(
    seq_len(nrow(registry)),
    function(i) dataset_exists(registry$pkg[i], registry$dataset[i]),
    logical(1)
  )
  registry[keep, , drop = FALSE]
}

#' Standard match schema used everywhere downstream:
#'   Season (chr), ht (chr), at (chr), goals1 (dbl), goals2 (dbl)
#' goals1 is the home side's goals, goals2 the away side's.
normalise_matches <- function(df, tier_filter = NA_real_) {
  if (!is.na(tier_filter) && "tier" %in% names(df)) {
    df <- df[df$tier == tier_filter, , drop = FALSE]
  }
  tibble::tibble(
    Season = as.character(df$Season),
    ht     = as.character(df$home),
    at     = as.character(df$visitor),
    goals1 = as.numeric(df$hgoal),
    goals2 = as.numeric(df$vgoal)
  ) |>
    dplyr::filter(!is.na(goals1), !is.na(goals2))
}

# Loaded leagues are cached for the life of the process. Datasets are a few MB
# and re-reading them on every league switch makes the picker feel sluggish.
.league_cache <- new.env(parent = emptyenv())

#' Load one league by display name. Returns the standard match schema.
load_league <- function(league_name, registry = available_leagues()) {
  if (!is.null(.league_cache[[league_name]])) return(.league_cache[[league_name]])

  if (league_name == SYNTHETIC_LEAGUE_NAME) {
    out <- synthetic_league()
  } else {
    row <- registry[registry$league == league_name, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop("No registry entry for league: ", league_name, call. = FALSE)
    }
    env <- new.env(parent = emptyenv())
    utils::data(list = row$dataset[[1]], package = row$pkg[[1]], envir = env)
    out <- normalise_matches(get(row$dataset[[1]], envir = env), row$tier_filter[[1]])
  }

  .league_cache[[league_name]] <- out
  out
}

#' Seasons present in a league, newest first.
season_choices <- function(league_df) {
  rev(sort(unique(league_df$Season)))
}

#' One season's matches, with `ht` and `at` sharing a common factor level set.
#' The shared level set is what makes the contr.sum indexing in model.R valid:
#' both factors must span the same k teams in the same order.
season_matches <- function(league_df, season) {
  df <- dplyr::filter(league_df, Season == season)
  teams <- sort(unique(c(df$ht, df$at)))
  dplyr::mutate(
    df,
    ht = factor(ht, levels = teams),
    at = factor(at, levels = teams)
  )
}

# ------------------------------------------------------------------------------
# Synthetic fallback, so the app runs on a machine without footBayes installed.
# ------------------------------------------------------------------------------
SYNTHETIC_LEAGUE_NAME <- "Sample League (simulated)"

synthetic_league <- function(seasons = c("2021", "2022", "2023"), n_teams = 20) {
  set.seed(42)
  teams <- paste("Team", LETTERS[seq_len(n_teams)])

  # Give each team a fixed latent attack/defence strength, so the fitted
  # ratings recover something meaningful rather than pure noise.
  att_true <- stats::setNames(stats::rnorm(n_teams, 0, 0.25), teams)
  def_true <- stats::setNames(stats::rnorm(n_teams, 0, 0.20), teams)
  mu <- 0.15
  home_adv <- 0.25

  fixtures <- expand.grid(
    Season = seasons, ht = teams, at = teams,
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(ht != at)

  dplyr::mutate(
    tibble::as_tibble(fixtures),
    goals1 = stats::rpois(dplyr::n(), exp(mu + home_adv + att_true[ht] - def_true[at])),
    goals2 = stats::rpois(dplyr::n(), exp(mu + att_true[at] - def_true[ht])),
    goals1 = as.numeric(goals1),
    goals2 = as.numeric(goals2)
  )
}
