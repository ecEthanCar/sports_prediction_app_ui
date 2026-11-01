# Player models: design, cross-validated ridge fits, split-half reliability.
# Requires aggregate.R (contr_sum_within), identification.R (player_design,
# long_players, IDENT_DEFAULT_THRESHOLD), helpers.R (fmt), glmnet, and dplyr.
#
# Models (all compared on the same held-out matches):
#   M0  league + home + game state                      (baseline)
#   M1  M0 + team attack and defense effects             (team-only)
#   M3  M1 + player effects as deviations from team, ridge on players only
# Structured versions of M3 (see structure.R), compared on the same folds:
#   M4      M3 + position-group counts (unpenalized)
#   MRoles  M4 + pairwise products of the position counts (role interactions)
#   MGraph  M3 columns, ridge plus a teammate-graph penalty (penalized IRLS)
#
# Game state: score difference crossed with 15-minute time bin
# ("score_time", segments) or score difference alone ("score").

PRIOR_SD_GRID <- exp(seq(log(0.01), log(0.5), length.out = 20))   # log scale
N_FOLDS <- 10
FOLD_SEED <- 2026

# ---- Data ------------------------------------------------------------------------------

# Game state at the start of each row: score difference from the attacking
# side's view (capped at -2 and +2), optionally crossed with the 15-minute time
# bin, plus player counts. Needed because substitutions depend on the game state.
add_game_state <- function(st, controls = c("score", "score_time")) {
  controls <- match.arg(controls)
  st$score_state <- factor(pmax(pmin(st$score_diff, 2L), -2L), levels = -2:2)
  st$strength_cat <- factor(
    ifelse(st$n_att == 11 & st$n_def == 11, "11v11",
    ifelse(st$n_att == 11 & st$n_def == 10, "11v10",
    ifelse(st$n_att == 10 & st$n_def == 11, "10v11", "other"))),
    levels = c("11v11", "11v10", "10v11", "other")
  )
  if (controls == "score_time") {
    if (!"time_bin" %in% names(st)) stop("score_time controls need the segment table (time_bin).")
    st$game_state <- interaction(st$score_state, factor(st$time_bin), drop = TRUE, sep = "_")
  } else {
    st$game_state <- st$score_state
  }
  st
}

# Coarse position group from a primary position. "Unknown" -> NA (not counted).
position_group <- function(p) {
  dplyr::case_when(
    is.na(p) | p == "Unknown" ~ NA_character_,
    p == "Goalkeeper" ~ "GK",
    grepl("Center Back", p) ~ "CB",
    grepl("Back", p) ~ "FB",
    grepl("Midfield", p) ~ "MF",
    TRUE ~ "FW"
  )
}

# One position group per player (primary position on his first listed team)
player_positions <- function(players) {
  players |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(position = dplyr::first(primary_position), .groups = "drop") |>
    dplyr::mutate(group = position_group(position))
}

# Rows x groups: number of attacking and of defending players in CB / FB / FW
# (midfield the reference). Goalkeepers are left out on both sides: nearly every
# lineup has exactly one, so a goalkeeper count is almost constant and
# confounded with the intercept.
position_counts <- function(st, positions) {
  grp <- stats::setNames(positions$group, positions$player_id)
  count_block <- function(side, groups, prefix) {
    lp <- long_players(st, side)
    g <- unname(grp[as.character(lp$player_id)])
    keep <- !is.na(g) & g %in% groups
    M <- Matrix::sparseMatrix(i = lp$row[keep], j = match(g[keep], groups), x = 1,
                              dims = c(nrow(st), length(groups)))
    colnames(M) <- paste0(prefix, groups)
    M
  }
  cbind(count_block("att", c("CB", "FB", "FW"), "pos_att_"),
        count_block("def", c("CB", "FB", "FW"), "pos_def_"))
}

# Outcome, offset, and the three blocks of columns: fixed (league, home, game
# state), team (within-league sum-to-zero attack and defense contrasts), and
# players (same design as the identification diagnostics, including pooled
# below-threshold counts per league).
player_model_data <- function(st, pm, threshold = IDENT_DEFAULT_THRESHOLD,
                              controls = c("score", "score_time"), positions = NULL) {
  controls <- match.arg(controls)
  st <- add_game_state(st, controls)
  d <- data.frame(league = factor(st$league), home = st$home,
                  game_state = st$game_state, strength_cat = st$strength_cat)
  form <- if (nlevels(d$league) > 1L) {
    ~ league + home + game_state + strength_cat
  } else {
    ~ home + game_state + strength_cat
  }
  X0 <- Matrix::sparse.model.matrix(form, data = d)
  X0 <- X0[, colnames(X0) != "(Intercept)", drop = FALSE]

  team_tbl <- dplyr::arrange(dplyr::distinct(data.frame(league = st$league, team = st$att_team,
                                                        stringsAsFactors = FALSE)), league, team)
  team_levels <- paste(team_tbl$league, team_tbl$team, sep = "@")
  C <- contr_sum_within(team_levels, team_tbl$league)
  ai <- match(paste(st$league, st$att_team, sep = "@"), team_levels)
  di <- match(paste(st$league, st$def_team, sep = "@"), team_levels)
  if (anyNA(ai) || anyNA(di)) stop("Unmatched teams in the segment table.")
  XT <- Matrix::Matrix(cbind(C[ai, , drop = FALSE], C[di, , drop = FALSE]), sparse = TRUE)
  colnames(XT) <- c(paste0("team_att_", seq_len(ncol(C))), paste0("team_def_", seq_len(ncol(C))))

  pd <- player_design(st, pm, threshold)
  XP <- pd$X[, grepl("^(off|def)_", colnames(pd$X)), drop = FALSE]
  pfP <- as.numeric(grepl("^(off|def)_[0-9]+$", colnames(XP)))   # pooled columns unpenalized

  XPos <- if (is.null(positions)) NULL else position_counts(st, positions)

  list(y = st$n_shots, offset = log(st$minutes), minutes = st$minutes,
       game_id = st$game_id, X0 = X0, XT = XT, XP = XP, pfP = pfP, XPos = XPos,
       C_team = C, team_levels = team_levels, controls = controls)
}

available_models <- function(md) c("M0", "M1", "M3")

model_matrix <- function(md, model) {
  needs <- function(field, fn) {
    if (is.null(md[[field]])) stop(model, " needs md$", field, ": see ", fn, ".")
  }
  switch(model,
    M0 = list(X = md$X0, pf = NULL),
    M1 = list(X = cbind(md$X0, md$XT), pf = NULL),
    M3 = , MGraph = list(X = cbind(md$X0, md$XT, md$XP),
                         pf = c(rep(0, ncol(md$X0) + ncol(md$XT)), md$pfP)),
    M4 = {
      needs("XPos", "player_model_data(positions = )")
      list(X = cbind(md$X0, md$XT, md$XPos, md$XP),
           pf = c(rep(0, ncol(md$X0) + ncol(md$XT) + ncol(md$XPos)), md$pfP))
    },
    MRoles = {
      needs("XRole", "add_role_interactions()")
      list(X = cbind(md$X0, md$XT, md$XPos, md$XRole, md$XP),
           pf = c(rep(0, ncol(md$X0) + ncol(md$XT) + ncol(md$XPos) + ncol(md$XRole)), md$pfP))
    },
    stop("Unknown model: ", model)
  )
}

# Folds by match: all of a match's segments share a fold
match_folds <- function(game_id, k = N_FOLDS, seed = FOLD_SEED) {
  games <- unique(game_id)
  set.seed(seed)
  fold_of_game <- sample(rep_len(seq_len(k), length(games)))
  fold_of_game[match(game_id, games)]
}

# ---- Unpenalized fits ------------------------------------------------------------------

fit_glm_matrix <- function(X, y, offset, family) {
  Xd <- cbind(`(Intercept)` = 1, as.matrix(X))
  b <- stats::glm.fit(Xd, y, offset = offset, family = family)$coefficients
  b[is.na(b)] <- 0
  b
}

predict_glm_matrix <- function(b, X, offset) {
  as.numeric(exp(cbind(1, as.matrix(X)) %*% b + offset))
}

heldout_deviance <- function(y, mu, family) sum(family$dev.resids(y, mu, rep(1, length(y))))

# ---- Penalized fits --------------------------------------------------------------------

# glmnet minimizes -loglik / n + lambda / 2 * sum(pf_internal * beta^2), and
# rescales penalty.factor to sum to the number of columns. With pf in {0, 1},
# this lambda makes the penalty equal to a N(0, tau^2) prior on each player
# effect (log scale).
tau_to_lambda <- function(tau, n, pf) (1 / (n * tau^2)) * sum(pf > 0) / length(pf)

fit_penalized <- function(X, y, offset, pf, taus, family = "poisson", ...) {
  taus <- sort(taus)                                  # increasing tau = decreasing lambda
  lambda <- tau_to_lambda(taus, length(y), pf)
  # glmnet can stop a lambda path early; disable that so every prior SD is fit
  old <- glmnet::glmnet.control()
  glmnet::glmnet.control(fdev = 0, devmax = 1)
  on.exit(glmnet::glmnet.control(fdev = old$fdev, devmax = old$devmax), add = TRUE)
  fit <- glmnet::glmnet(X, y, family = family, offset = offset, alpha = 0,
                        lambda = lambda, penalty.factor = pf, standardize = FALSE, ...)
  if (length(fit$lambda) != length(lambda)) {
    stop("glmnet returned ", length(fit$lambda), " of ", length(lambda), " penalty values.")
  }
  list(fit = fit, taus = taus, lambda = lambda)
}

predict_penalized <- function(pfit, X, offset) {
  as.matrix(stats::predict(pfit$fit, newx = X, newoffset = offset, type = "response"))
}

# Coefficients at a prior SD on the fitted grid
coef_at <- function(pfit, tau) {
  j <- which.min(abs(log(pfit$taus) - log(tau)))
  if (!is.null(pfit$beta)) return(as.numeric(pfit$beta[, j]))   # fit_graph_ridge()
  as.numeric(as.matrix(stats::coef(pfit$fit)[, j]))
}

# ---- Cross-validation ------------------------------------------------------------------

# Held-out deviance on one fold for each model: a number for unpenalized
# models, a vector over the prior-SD grid for penalized ones. The pipeline runs
# the folds in parallel and combines them with combine_cv().
cv_one_fold <- function(md, folds, fam, fold, taus = PRIOR_SD_GRID, models = available_models(md)) {
  taus <- sort(taus)
  tr <- folds != fold
  te <- !tr
  if (!any(te)) stop("Fold ", fold, " has no rows.")
  dev <- stats::setNames(lapply(models, function(m) {
    mm <- model_matrix(md, m)
    if (m %in% c("M0", "M1")) {
      b <- fit_glm_matrix(mm$X[tr, , drop = FALSE], md$y[tr], md$offset[tr], fam$family)
      mu <- predict_glm_matrix(b, mm$X[te, , drop = FALSE], md$offset[te])
      heldout_deviance(md$y[te], mu, fam$family)
    } else {
      pfit <- fit_path(md, m, tr, fam, taus)
      mu <- predict_path(pfit, mm$X[te, , drop = FALSE], md$offset[te])
      apply(mu, 2, function(col) heldout_deviance(md$y[te], col, fam$family))
    }
  }), models)
  list(fold = fold, dev = dev)
}

# Fold results -> list(taus, dev, k, models); dev[[m]] is a vector over folds
# (unpenalized) or a folds x prior-SD matrix (penalized).
combine_cv <- function(fold_results, taus = PRIOR_SD_GRID) {
  taus <- sort(taus)
  fold_results <- fold_results[order(vapply(fold_results, function(r) r$fold, numeric(1)))]
  k <- length(fold_results)
  models <- names(fold_results[[1]]$dev)
  dev <- stats::setNames(lapply(models, function(m) {
    parts <- lapply(fold_results, function(r) r$dev[[m]])
    if (m %in% c("M0", "M1")) unlist(parts) else do.call(rbind, parts)
  }), models)
  list(taus = taus, dev = dev, k = k, models = models)
}

best_tau <- function(cv, model) cv$taus[which.min(colSums(cv$dev[[model]]))]

# Fold-by-fold deviance at each model's best prior SD
fold_deviance <- function(cv, m) {
  d <- cv$dev[[m]]
  if (is.matrix(d)) d[, which.min(colSums(d))] else d
}

# ---- Final fits and reliability ---------------------------------------------------------

fit_final <- function(md, model, tau, fam, taus = PRIOR_SD_GRID) {
  mm <- model_matrix(md, model)
  pfit <- fit_path(md, model, rep(TRUE, length(md$y)), fam, taus)
  b <- coef_at(pfit, tau)
  names(b) <- c("(Intercept)", colnames(mm$X))
  b
}

# Player effects fit separately on alternating matches, for players with at
# least min_minutes in each half. Correlating half1 with half2 measures
# reliability: high = stable player effects; near zero = noise.
split_half_effects <- function(md, fam, tau, model = "M3", min_minutes = 300, taus = PRIOR_SD_GRID) {
  games <- unique(md$game_id)
  half <- ifelse(match(md$game_id, games) %% 2 == 1L, 1L, 2L)
  mm <- model_matrix(md, model)
  pcols <- which(mm$pf > 0)
  eff <- list(); mins <- list()
  for (h in 1:2) {
    r <- half == h
    pfit <- fit_path(md, model, r, fam, taus)
    eff[[h]] <- coef_at(pfit, tau)[-1][pcols]
    mins[[h]] <- as.numeric(Matrix::crossprod(mm$X[r, pcols, drop = FALSE], md$minutes[r]))
  }
  nm <- colnames(mm$X)[pcols]
  out <- data.frame(model = model,
                    side = ifelse(startsWith(nm, "off_"), "Offense", "Defense"),
                    player_id = as.integer(sub("^(off|def)_", "", nm)),
                    half1 = eff[[1]], half2 = eff[[2]], min1 = mins[[1]], min2 = mins[[2]])
  out[out$min1 >= min_minutes & out$min2 >= min_minutes, ]
}

# ---- Player effects --------------------------------------------------------------------

player_effects <- function(b, pm) {
  nm <- names(b)
  i <- grepl("^(off|def)_[0-9]+$", nm)
  d <- data.frame(side = ifelse(startsWith(nm[i], "off_"), "Offense", "Defense"),
                  player_id = as.integer(sub("^(off|def)_", "", nm[i])),
                  effect = unname(b[i]), stringsAsFactors = FALSE)
  dplyr::left_join(d, pm[, c("player_id", "player_name", "main_team", "minutes")], by = "player_id")
}

# Dispatch: penalized IRLS for the graph prior, glmnet ridge otherwise
fit_path <- function(md, model, rows, fam, taus = PRIOR_SD_GRID) {
  mm <- model_matrix(md, model)
  X <- mm$X[rows, , drop = FALSE]
  if (model == "MGraph") {
    if (is.null(md$P_graph)) stop("MGraph needs md$P_graph: see build_graph_penalty().")
    fit_graph_ridge(X, md$y[rows], md$offset[rows], md$P_graph, taus)
  } else {
    fit_penalized(X, md$y[rows], md$offset[rows], mm$pf, taus, fam$glmnet_family)
  }
}

predict_path <- function(pfit, X, offset) {
  if (!is.null(pfit$beta)) predict_graph_ridge(pfit, X, offset) else predict_penalized(pfit, X, offset)
}

# ---- Summaries -------------------------------------------------------------------------

# Held-out gain of model m_b (in cv_b) over model m_a (in cv_a); both CV runs
# must use the same folds
cv_gain <- function(cv_a, m_a, cv_b, m_b) {
  d <- fold_deviance(cv_a, m_a) - fold_deviance(cv_b, m_b)
  c(gain = sum(d), se = sqrt(length(d)) * stats::sd(d))
}

# Split-half reliability by side
sh_reliability <- function(sh) {
  vapply(c(Offense = "Offense", Defense = "Defense"),
         function(s) stats::cor(sh$half1[sh$side == s], sh$half2[sh$side == s]), numeric(1))
}

# The baseline the interaction models must beat: held-out gains with
# fold-to-fold SEs (teams over controls, players over teams), the chosen prior
# SD, and split-half reliability of player effects.
#   baseline_summary(tar_read(base_cv), tar_read(base_sh))
baseline_summary <- function(cv, sh) {
  gain <- function(m, ref) {
    d <- fold_deviance(cv, ref) - fold_deviance(cv, m)
    sprintf("%s (SE %s)", fmt(sum(d), 1), fmt(sqrt(cv$k) * stats::sd(d), 1))
  }
  rel <- function(s) {
    d <- sh[sh$side == s, ]
    sprintf("%s (%d players)", fmt(stats::cor(d$half1, d$half2), 3), nrow(d))
  }
  data.frame(
    Quantity = c("Teams over controls (M1 vs M0)", "Players over teams (M3 vs M1)",
                 "Prior SD on player effects (% shots per 90)",
                 "Split-half reliability, offense", "Split-half reliability, defense"),
    Value = c(gain("M1", "M0"), gain("M3", "M1"), fmt(100 * (exp(best_tau(cv, "M3")) - 1), 1),
              rel("Offense"), rel("Defense"))
  )
}
