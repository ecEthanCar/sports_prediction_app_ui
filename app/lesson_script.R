# ==============================================================================
# lesson_script.R  -- the console version of the module
#
# Same functions the app calls. Students can work through the model here and
# then open the app to explore it, without the two versions drifting apart.
#   setwd() to the app directory, then source this file.
# ==============================================================================

library(dplyr); library(tidyr); library(tibble)
library(ggplot2); library(ggrepel); library(scales)

for (f in list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)) source(f)

# ---- 1. pick a league and season ---------------------------------------------
leagues <- available_leagues()
league  <- if (nrow(leagues) > 0) leagues$league[1] else SYNTHETIC_LEAGUE_NAME

league_df <- load_league(league, leagues)
season    <- season_choices(league_df)[1]
matches   <- season_matches(league_df, season)

cat(sprintf("%s, season %s: %d matches, %d teams\n",
            league, season_label(season), nrow(matches), nlevels(matches$ht)))

# ---- 2. fit ------------------------------------------------------------------
fit   <- fit_double_poisson(matches)
terms <- model_terms(fit)

cat(sprintf("Baseline: %.3f log-goals (%.2f goals per side)\n", terms$mu, exp(terms$mu)))
cat(sprintf("Home advantage: %.2fx (log coef %.3f, p = %.3e)\n",
            terms$home_mult, terms$home_adv, terms$home_p_value))

# ---- 3. ratings --------------------------------------------------------------
ratings <- extract_ratings(fit, levels(matches$ht)) |>
  left_join(team_goal_diff(matches), by = "team") |>
  mutate(team_label = shorten_team(team, league))

print(arrange(ratings, desc(overall)), n = Inf)

# ---- 4. quadrant map ---------------------------------------------------------
print(plot_quadrant(
  ratings,
  title = sprintf("%s (%s): team attack vs. defense ratings", league, season_label(season)),
  subtitle = "Double Poisson GLM with sum-to-zero constraints (0 = league average)"
))

# ---- 5. one fixture, predicted and actual ------------------------------------
home <- ratings$team[which.max(ratings$overall)]
away <- ratings$team[which.min(ratings$overall)]

lam <- match_lambdas(ratings, terms, home, away)
sm  <- score_matrix(lam$home, lam$away)

print(outcome_probs(sm))

h2h <- head_to_head(matches, home, away)
leg <- h2h[h2h$venue == "Home leg", ]
actual <- if (nrow(leg) == 1) c(leg$home_goals, leg$away_goals) else NULL

print(plot_scoreline(sm, home, away, display_max = 5, actual = actual))

if (!is.null(actual)) {
  sp <- scoreline_prob(sm, actual[1], actual[2])
  cat(sprintf("Actual: %s %d-%d %s. Model gave that exact scoreline %.1f%% (rank %d).\n",
              home, actual[1], actual[2], away, sp$prob * 100, sp$rank))
}
