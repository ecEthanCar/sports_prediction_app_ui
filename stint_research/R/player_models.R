# Stage 3: player models for shot volume.
# Requires aggregate.R (contr_sum_within), models.R (fit_volume),
# diagnostics.R (fmt), identification.R (player_design, pct,
# IDENT_DEFAULT_THRESHOLD), plots.R (plain_theme), and dplyr.
#
# Models (all compared on the same held-out matches):
#   M0  league + home + game state                      (baseline)
#   M1  M0 + team attack and defense effects             (team-only)
#   M2  M0 + player offense and defense effects, ridge   (players, no team terms)
#   M3  M1 + player effects as deviations from team, ridge on players only
#   M4  M3 + position-group counts (unpenalized): each player's ridge effect
#       becomes a deviation from his position's mean
#
# Game state: score difference ("score"), or score difference crossed with
# 15-minute time bin ("score_time", which needs the segment table).

PRIOR_SD_GRID <- exp(seq(log(0.01), log(0.5), length.out = 20))   # log scale
DISPERSION_THRESHOLD <- 1.1
N_FOLDS <- 10
FOLD_SEED <- 2026
MODEL_DESCRIPTIONS <- c(M0 = "League, home, game state", M1 = "M0 + teams",
                        M2 = "M0 + players (ridge)", M3 = "M0 + teams + players (ridge)",
                        M4 = "M3 + position groups")

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

# Rows x groups: number of attacking players in CB / FB / FW (GK excluded,
# midfield the reference) and defending players in GK / CB / FB / FW.
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
        count_block("def", c("GK", "CB", "FB", "FW"), "pos_def_"))
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
  if (anyNA(ai) || anyNA(di)) stop("Unmatched teams in the stint table.")
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

available_models <- function(md) c("M0", "M1", "M2", "M3", if (!is.null(md$XPos)) "M4")

model_matrix <- function(md, model) {
  switch(model,
    M0 = list(X = md$X0, pf = NULL),
    M1 = list(X = cbind(md$X0, md$XT), pf = NULL),
    M2 = list(X = cbind(md$X0, md$XP), pf = c(rep(0, ncol(md$X0)), md$pfP)),
    M3 = list(X = cbind(md$X0, md$XT, md$XP),
              pf = c(rep(0, ncol(md$X0) + ncol(md$XT)), md$pfP)),
    M4 = {
      if (is.null(md$XPos)) stop("M4 needs position counts: pass positions to player_model_data().")
      list(X = cbind(md$X0, md$XT, md$XPos, md$XP),
           pf = c(rep(0, ncol(md$X0) + ncol(md$XT) + ncol(md$XPos)), md$pfP))
    },
    stop("Unknown model: ", model)
  )
}

# Folds by match: all of a match's stints share a fold
match_folds <- function(game_id, k = N_FOLDS, seed = FOLD_SEED) {
  games <- unique(game_id)
  set.seed(seed)
  fold_of_game <- sample(rep_len(seq_len(k), length(games)))
  fold_of_game[match(game_id, games)]
}

# ---- Likelihood choice -----------------------------------------------------------------

fit_glm_matrix <- function(X, y, offset, family) {
  Xd <- cbind(`(Intercept)` = 1, as.matrix(X))
  b <- stats::glm.fit(Xd, y, offset = offset, family = family)$coefficients
  b[is.na(b)] <- 0
  b
}

predict_glm_matrix <- function(b, X, offset) {
  as.numeric(exp(cbind(1, as.matrix(X)) %*% b + offset))
}

# Fit M1 as an unpenalized Poisson model and compute the Pearson dispersion.
# Below the threshold: Poisson. Otherwise: negative binomial with theta fixed
# at its maximum-likelihood value given the Poisson fitted means.
choose_family <- function(md, threshold = DISPERSION_THRESHOLD) {
  X <- cbind(md$X0, md$XT)
  b <- fit_glm_matrix(X, md$y, md$offset, stats::poisson())
  mu <- predict_glm_matrix(b, X, md$offset)
  disp <- sum((md$y - mu)^2 / mu) / (length(md$y) - ncol(X) - 1)
  use_nb <- disp >= threshold
  theta <- if (use_nb) as.numeric(MASS::theta.ml(md$y, mu)) else NA_real_
  fam <- if (use_nb) MASS::negative.binomial(theta) else stats::poisson()
  list(dispersion = disp, use_nb = use_nb, theta = theta, family = fam,
       glmnet_family = if (use_nb) fam else "poisson")
}

family_table <- function(fam) {
  data.frame(
    Quantity = c("Pearson dispersion, M1 fitted as Poisson", "Threshold for switching",
                 "Likelihood used", "Negative binomial theta"),
    Value = c(fmt(fam$dispersion, 3), fmt(DISPERSION_THRESHOLD, 2),
              if (fam$use_nb) "Negative binomial (theta fixed)" else "Poisson",
              if (fam$use_nb) fmt(fam$theta, 1) else "-")
  )
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
    X <- mm$X
    if (m %in% c("M0", "M1")) {
      b <- fit_glm_matrix(X[tr, , drop = FALSE], md$y[tr], md$offset[tr], fam$family)
      mu <- predict_glm_matrix(b, X[te, , drop = FALSE], md$offset[te])
      heldout_deviance(md$y[te], mu, fam$family)
    } else {
      pfit <- fit_penalized(X[tr, , drop = FALSE], md$y[tr], md$offset[tr], mm$pf,
                            taus, fam$glmnet_family)
      mu <- predict_penalized(pfit, X[te, , drop = FALSE], md$offset[te])
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

cv_models <- function(md, folds, fam, taus = PRIOR_SD_GRID, models = available_models(md)) {
  combine_cv(lapply(seq_len(max(folds)), function(f) cv_one_fold(md, folds, fam, f, taus, models)), taus)
}

best_tau <- function(cv, model) cv$taus[which.min(colSums(cv$dev[[model]]))]

# Fold-by-fold deviance at each model's best prior SD
fold_deviance <- function(cv, m) {
  d <- cv$dev[[m]]
  if (is.matrix(d)) d[, which.min(colSums(d))] else d
}

# Held-out deviance by model at each model's best prior SD, and improvements
# (positive = better) with fold-to-fold standard errors. The prior SD is chosen
# on the same folds, so the penalized models' gains are slightly optimistic.
cv_table <- function(cv) {
  k <- cv$k
  models <- cv$models
  fold_dev <- stats::setNames(lapply(models, function(m) fold_deviance(cv, m)), models)
  improvement <- function(m, ref) {
    if (!ref %in% models || m == ref) return("-")
    d <- fold_dev[[ref]] - fold_dev[[m]]
    sprintf("%s (SE %s)", fmt(sum(d), 1), fmt(sqrt(k) * stats::sd(d), 1))
  }
  data.frame(
    Model = models,
    Description = unname(MODEL_DESCRIPTIONS[models]),
    `Prior SD (% shots per 90)` = vapply(models, function(m) {
      if (is.matrix(cv$dev[[m]])) fmt(pct(best_tau(cv, m)), 1) else "-"
    }, character(1)),
    `Held-out deviance` = vapply(models, function(m) fmt(sum(fold_dev[[m]]), 1), character(1)),
    `Improvement over M0` = vapply(models, improvement, character(1), ref = "M0"),
    `Improvement over M1` = vapply(models, function(m) if (m == "M0") "-" else improvement(m, "M1"),
                                   character(1)),
    `Improvement over M3` = vapply(models, function(m) if (m != "M4") "-" else improvement(m, "M3"),
                                   character(1)),
    check.names = FALSE, row.names = NULL
  )
}

plot_cv_curve <- function(cv) {
  pen <- names(cv$dev)[vapply(cv$dev, is.matrix, logical(1))]
  df <- do.call(rbind, lapply(pen, function(m) {
    data.frame(model = paste0(m, ": ", MODEL_DESCRIPTIONS[[m]]), tau = cv$taus, dev = colSums(cv$dev[[m]]))
  }))
  ggplot2::ggplot(df, ggplot2::aes(x = pct(tau), y = dev, linetype = model)) +
    ggplot2::geom_hline(yintercept = sum(cv$dev$M1), linetype = "dotted", color = "gray40") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(title = "Held-out deviance by prior SD on player effects",
                  x = "Prior SD (% shots per 90, log scale)",
                  y = "Held-out deviance (dotted line = team-only M1)",
                  linetype = NULL) +
    plain_theme()
}

# ---- Position effects ------------------------------------------------------------------

# Position-group coefficients in M4 (relative to midfield), from the full fit,
# with delete-one-fold jackknife standard errors over the cross-validation
# training sets. Offense: + = more shots taken. Defense: + = more shots conceded.
# The pipeline runs position_coefs() once on all rows and once per fold
# (leaving that fold out), then combines with position_effects_table().
position_coefs <- function(md, fam, tau, rows = NULL, taus = PRIOR_SD_GRID) {
  mm <- model_matrix(md, "M4")
  if (is.null(rows)) rows <- rep(TRUE, length(md$y))
  pfit <- fit_penalized(mm$X[rows, , drop = FALSE], md$y[rows], md$offset[rows], mm$pf, taus,
                        fam$glmnet_family)
  b <- coef_at(pfit, tau)
  names(b) <- c("(Intercept)", colnames(mm$X))
  b[colnames(md$XPos)]
}

position_effects_table <- function(full, jk) {
  jk <- do.call(rbind, jk)
  k <- nrow(jk)
  se <- sqrt((k - 1) / k * colSums(sweep(jk, 2, colMeans(jk))^2))
  pos_cols <- names(full)
  data.frame(Side = ifelse(startsWith(pos_cols, "pos_att_"), "Offense", "Defense"),
             Group = sub("^pos_(att|def)_", "", pos_cols),
             effect = unname(full), se = unname(se), row.names = NULL)
}

position_effects <- function(md, folds, fam, tau, taus = PRIOR_SD_GRID) {
  full <- position_coefs(md, fam, tau, taus = taus)
  jk <- lapply(seq_len(max(folds)), function(f) position_coefs(md, fam, tau, rows = folds != f, taus = taus))
  position_effects_table(full, jk)
}

# Side-by-side position effects under two sets of controls
position_comparison_table <- function(pe_score, pe_time) {
  data.frame(
    Side = pe_score$Side, `Group (vs midfield)` = pe_score$Group,
    `Score controls: % shots per 90 (SE)` = sprintf("%s (%s)", fmt(pct(pe_score$effect), 1),
                                                    fmt(100 * pe_score$se, 1)),
    `Score x time controls: % shots per 90 (SE)` = sprintf("%s (%s)", fmt(pct(pe_time$effect), 1),
                                                           fmt(100 * pe_time$se, 1)),
    check.names = FALSE
  )
}

# ---- Final fits, sanity check, reliability ---------------------------------------------

fit_final <- function(md, model, tau, fam, taus = PRIOR_SD_GRID) {
  mm <- model_matrix(md, model)
  pfit <- fit_penalized(mm$X, md$y, md$offset, mm$pf, taus, fam$glmnet_family)
  b <- coef_at(pfit, tau)
  names(b) <- c("(Intercept)", colnames(mm$X))
  b
}

# Team attack effects from the stint-level M1 vs the stage 1 team-match model
team_sanity <- function(md, fam, team_match) {
  X <- model_matrix(md, "M1")$X
  b <- fit_glm_matrix(X, md$y, md$offset, fam$family)
  b <- b[-1]
  names(b) <- colnames(X)
  eff_m1 <- stats::setNames(as.numeric(md$C_team %*% b[grepl("^team_att_", names(b))]), md$team_levels)
  nb <- fit_volume(team_match)$nb
  b1 <- stats::coef(nb)
  b1 <- b1[grepl("^ATT", names(b1))]
  eff_s1 <- stats::setNames(as.numeric(attr(team_match, "contrasts_team") %*% b1),
                            attr(team_match, "team_levels"))
  common <- intersect(names(eff_m1), names(eff_s1))
  data.frame(Quantity = c("Teams compared", "Correlation of team attack effects, stint M1 vs stage 1"),
             Value = c(fmt(length(common), 0), fmt(stats::cor(eff_m1[common], eff_s1[common]), 3)))
}

# Player effects fit separately on alternating matches, for players with at
# least min_minutes in each half. reliability_table() correlates them: high =
# stable player effects; near zero = noise.
split_half_effects <- function(md, fam, tau, model = "M3", min_minutes = 300, taus = PRIOR_SD_GRID) {
  games <- unique(md$game_id)
  half <- ifelse(match(md$game_id, games) %% 2 == 1L, 1L, 2L)
  mm <- model_matrix(md, model)
  pcols <- which(mm$pf > 0)
  eff <- list(); mins <- list()
  for (h in 1:2) {
    r <- half == h
    pfit <- fit_penalized(mm$X[r, , drop = FALSE], md$y[r], md$offset[r], mm$pf, taus, fam$glmnet_family)
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

reliability_table <- function(sh) {
  do.call(rbind, lapply(c("Offense", "Defense"), function(s) {
    d <- sh[sh$side == s, ]
    data.frame(Model = sh$model[1], Side = s, Players = nrow(d),
               Pearson = round(stats::cor(d$half1, d$half2), 3),
               Spearman = round(stats::cor(d$half1, d$half2, method = "spearman"), 3))
  }))
}

split_half_reliability <- function(md, fam, tau, min_minutes = 300, taus = PRIOR_SD_GRID,
                                   model = "M3") {
  reliability_table(split_half_effects(md, fam, tau, model, min_minutes, taus))
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

effects_spread_table <- function(eff) {
  do.call(rbind, lapply(c("Offense", "Defense"), function(s) {
    e <- eff$effect[eff$side == s]
    data.frame(Side = s, Players = length(e),
               `SD (% shots per 90)` = fmt(pct(stats::sd(e)), 1),
               `10th to 90th percentile (%)` = sprintf("%s to %s", fmt(pct(stats::quantile(e, 0.1)), 1),
                                                       fmt(pct(stats::quantile(e, 0.9)), 1)),
               check.names = FALSE)
  }))
}

# Offense: higher = team takes more shots with the player on. Defense: lower =
# team concedes fewer shots with the player on (better), so "best" is lowest.
top_players_table <- function(eff, side, n = 10) {
  e <- eff[eff$side == side, ]
  best_first <- if (side == "Offense") order(-e$effect) else order(e$effect)
  pick <- c(head(best_first, n), rev(tail(best_first, n)))
  data.frame(Group = rep(c("Best", "Worst"), each = n)[seq_along(pick)],
             Player = e$player_name[pick], Team = e$main_team[pick],
             Minutes = round(e$minutes[pick]),
             `Effect (% shots per 90)` = fmt(pct(e$effect[pick]), 1),
             check.names = FALSE, row.names = NULL)
}
