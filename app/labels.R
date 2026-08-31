# ==============================================================================
# labels.R  -- display names
# The old single regex only knew Italian suffixes, so it was a no-op on every
# other league. Rules are per league, with a safe pass-through default.
# ==============================================================================

TEAM_LABEL_RULES <- list(
  "Italian Serie A"        = "\\b(Calcio|CFC|FC|ACF|SSC|AC|AS|US|SS|1919)\\b",
  "English Premier League" = "\\b(AFC|FC)\\b",
  "Spanish La Liga"        = "\\b(CF|FC|SD|RC|UD|CD|Club)\\b",
  "German Bundesliga"      = "\\b(FC|SV|VfL|VfB|BV|TSG|SC|1899|1846)\\b"
)

#' Shorten team names for plot labels. Unknown leagues pass through untouched,
#' which is the right failure mode: a long label beats a mangled one.
shorten_team <- function(x, league_name) {
  pattern <- TEAM_LABEL_RULES[[league_name]]
  if (is.null(pattern)) return(as.character(x))

  out <- gsub(pattern, "", as.character(x))
  out <- trimws(gsub("\\s+", " ", out))

  # Never return an empty label; fall back to the original name.
  ifelse(nzchar(out), out, as.character(x))
}

#' Render a season as a span when the source stores a single start year,
#' so "2021" reads as the 2021-22 campaign it actually is.
season_label <- function(season) {
  s <- as.character(season)
  if (grepl("^[0-9]{4}$", s)) {
    yr <- as.integer(s)
    sprintf("%d-%02d", yr, (yr + 1L) %% 100L)
  } else {
    s
  }
}
