# ==============================================================================
# model.R  -- the double Poisson model
# Pure functions. Nothing here knows the app exists, so the lesson script and
# the Shiny app fit the identical model.
#
#   log(lambda) = mu + home * gamma + att[i] - def[j]
#
# with sum-to-zero constraints on att and def, so 0 is the league average.
# ==============================================================================

#' Reshape one season into the stacked long form the GLM expects.
#' Each match contributes two rows: the home side's goals and the away side's.
stack_matches <- function(matches) {
  n <- nrow(matches)
  team_levels <- levels(matches$ht)

  tibble::tibble(
    game  = c(seq_len(n), seq_len(n)),
    home  = c(rep(1, n), rep(0, n)),
    att   = factor(c(as.character(matches$ht), as.character(matches$at)),
                   levels = team_levels),
    def   = factor(c(as.character(matches$at), as.character(matches$ht)),
                   levels = team_levels),
    goals = c(matches$goals1, matches$goals2)
  ) |>
    dplyr::arrange(game)
}

#' Fit the double Poisson GLM with sum-to-zero contrasts.
fit_double_poisson <- function(matches) {
  stacked <- stack_matches(matches)
  k <- nlevels(stacked$att)

  if (k < 3) stop("Need at least three teams to fit the model.", call. = FALSE)

  contrasts(stacked$att) <- stats::contr.sum(k)
  contrasts(stacked$def) <- stats::contr.sum(k)

  stats::glm(goals ~ home + att + def,
             family = stats::poisson(link = "log"),
             data = stacked)
}

#' Baseline and home-advantage terms.
model_terms <- function(fit) {
  cf <- stats::coef(fit)
  p  <- summary(fit)$coefficients["home", 4]
  list(
    mu           = unname(cf[["(Intercept)"]]),
    home_adv     = unname(cf[["home"]]),
    home_mult    = exp(unname(cf[["home"]])),
    home_p_value = unname(p)
  )
}

#' Team ratings, including the reference team dropped by contr.sum.
#'
#' Coefficients are located by NAME, not position. Positional indexing
#' (`coef[3:(k+1)]`) silently grabs the wrong parameters whenever the fit is
#' rank deficient, which happens on partial seasons and on any season where a
#' team never appears in one of the two roles. Here that case aborts loudly.
#'
#' The k-th team's estimate is -sum(the other k-1). Its standard error follows
#' from the delta method: for the linear combination c'beta with c = -1 across
#' the block, Var = sum of every element of that block's covariance submatrix.
#'
#' Defence is returned NEGATED, so positive means "concedes fewer goals" and
#' both axes read in the same direction. Sign flips leave the SE unchanged.
extract_ratings <- function(fit, team_levels) {
  cf <- stats::coef(fit)
  V  <- stats::vcov(fit)

  if (anyNA(cf)) {
    stop(
      "The fit is rank deficient: at least one team never appears in one of ",
      "the home/away roles this season. Pick a complete season, or drop the ",
      "affected team.",
      call. = FALSE
    )
  }

  k <- length(team_levels)
  att_idx <- grep("^att[0-9]+$", names(cf))
  def_idx <- grep("^def[0-9]+$", names(cf))

  if (length(att_idx) != k - 1L || length(def_idx) != k - 1L) {
    stop("Expected ", k - 1L, " attack and defence contrasts, found ",
         length(att_idx), " and ", length(def_idx), ".", call. = FALSE)
  }

  att_est <- unname(cf[att_idx])
  V_att   <- V[att_idx, att_idx, drop = FALSE]
  att_all <- c(att_est, -sum(att_est))
  att_se  <- c(sqrt(diag(V_att)), sqrt(sum(V_att)))

  def_raw <- unname(cf[def_idx])
  V_def   <- V[def_idx, def_idx, drop = FALSE]
  def_all <- -c(def_raw, -sum(def_raw))
  def_se  <- c(sqrt(diag(V_def)), sqrt(sum(V_def)))

  tibble::tibble(
    team    = team_levels,
    att     = att_all,
    att_se  = att_se,
    def     = def_all,
    def_se  = def_se,
    overall = att_all + def_all
  )
}

#' Realised goal difference per team, straight from the results.
#' Deliberately model-free: this is the reality check the ratings get compared to.
team_goal_diff <- function(matches) {
  dplyr::bind_rows(
    dplyr::transmute(matches, team = as.character(ht), gf = goals1, ga = goals2),
    dplyr::transmute(matches, team = as.character(at), gf = goals2, ga = goals1)
  ) |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      played = dplyr::n(),
      gf = sum(gf), ga = sum(ga), gd = sum(gf) - sum(ga),
      .groups = "drop"
    )
}

#' Expected goals for a fixture, given ratings and the fitted terms.
match_lambdas <- function(ratings, terms, home_team, away_team) {
  pick <- function(team, column) {
    v <- ratings[[column]][ratings$team == team]
    if (length(v) != 1L) {
      stop("Team not found in this season's ratings: ", team, call. = FALSE)
    }
    v
  }

  list(
    home = exp(terms$mu + terms$home_adv + pick(home_team, "att") - pick(away_team, "def")),
    away = exp(terms$mu + pick(away_team, "att") - pick(home_team, "def"))
  )
}

#' Joint scoreline distribution under independent Poisson margins.
#'
#' The grid is truncated at `max_goals`, which discards a little probability
#' mass in the tail (well under 0.1% at league-typical rates). We renormalise
#' so the three outcome probabilities sum to exactly 1 and the implied fair
#' odds are internally consistent.
score_matrix <- function(lambda_h, lambda_a, max_goals = 8) {
  tidyr::expand_grid(Home = 0:max_goals, Away = 0:max_goals) |>
    dplyr::mutate(
      prob = stats::dpois(Home, lambda_h) * stats::dpois(Away, lambda_a),
      prob = prob / sum(prob)
    )
}

#' Home win / draw / away win, with fair decimal odds.
outcome_probs <- function(score_mat) {
  tibble::tibble(
    result = c("home", "draw", "away"),
    prob = c(
      sum(score_mat$prob[score_mat$Home >  score_mat$Away]),
      sum(score_mat$prob[score_mat$Home == score_mat$Away]),
      sum(score_mat$prob[score_mat$Home <  score_mat$Away])
    )
  ) |>
    dplyr::mutate(fair_odds = 1 / prob)
}

#' Probability the model assigned to one exact scoreline, and its rank among
#' all scorelines. Used to place an actual result on the predicted surface.
scoreline_prob <- function(score_mat, home_goals, away_goals) {
  ranked <- dplyr::mutate(score_mat, rank = rank(-prob, ties.method = "min"))
  hit <- ranked[ranked$Home == home_goals & ranked$Away == away_goals, ]
  if (nrow(hit) != 1L) return(list(prob = NA_real_, rank = NA_integer_))
  list(prob = hit$prob[[1]], rank = as.integer(hit$rank[[1]]))
}

#' Both legs these two teams actually played this season.
#' `venue` is relative to the currently selected home side, so "Home leg" is
#' the fixture the model is predicting and "Reverse leg" is the return.
head_to_head <- function(matches, home_team, away_team) {
  ht <- as.character(matches$ht)
  at <- as.character(matches$at)
  keep <- (ht == home_team & at == away_team) | (ht == away_team & at == home_team)

  matches[keep, , drop = FALSE] |>
    dplyr::transmute(
      venue     = dplyr::if_else(as.character(ht) == home_team, "Home leg", "Reverse leg"),
      home_side = as.character(ht),
      away_side = as.character(at),
      home_goals = goals1,
      away_goals = goals2
    ) |>
    dplyr::arrange(venue != "Home leg")
}
