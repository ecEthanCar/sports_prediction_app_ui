# Run from the project root:  testthat::test_file("tests/test_pipeline.R")
# Uses small synthetic inputs only; no data files or network needed.

library(testthat)

root <- if (file.exists("R/load.R")) "." else if (file.exists("../R/load.R")) ".." else
  stop("Run from the project root or the tests/ directory.")
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

# ---- Synthetic inputs ----------------------------------------------------------------

fake_matches <- data.frame(
  game_id = 1:3, league = "L1",
  match_date = as.Date("2015-08-01") + 0:2,
  home_team = c("A", "B", "C"), away_team = c("B", "C", "A"),
  stringsAsFactors = FALSE
)
fake_shots <- data.frame(
  game_id = c(1L, 1L, 1L, 2L),
  off_team = c("A", "A", "B", "B"),
  xg = c(0.1, 0.2, 0.3, 0.4),
  stringsAsFactors = FALSE
)

# One row in the format written by build_shots.py
fake_shot_row <- function(game_id, event_id, home_off, off_team, def_team, set_piece, xg = 0.1) {
  row <- data.frame(
    league = "L1", season = "2015/16", game_id = game_id, event_id = event_id,
    match_date = "2015-08-01", period = 1L, minute = 10L, t_sec = 600,
    stint_id = paste0(game_id, "_1"), segment_id = paste0(game_id, "_1"), poss_num = 1L,
    home_off = home_off, off_team = off_team, def_team = def_team,
    shooter = "X16 Player", shooter_id = 1L, outcome = "Saved", shot_made = 0L,
    poss_goals = 0L, poss_end = 1L, shot_x = 0, shot_y = 10, distance = 10,
    angle = 0, xg = xg, set_piece = set_piece, body_part = "Right Foot",
    technique = "Normal", play_pattern = "Regular Play", first_time = 0L,
    under_pressure = 0L, strength = "11v11", stringsAsFactors = FALSE
  )
  for (nm in c(paste0("O", 1:11), paste0("D", 1:11))) row[[nm]] <- "P"
  row
}

write_fake_shots <- function(rows) {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(do.call(rbind, rows), path, row.names = FALSE)
  path
}

# ---- Loading -------------------------------------------------------------------------

test_that("validate_matches: catches duplicated game ids", {
  dup <- rbind(fake_matches, fake_matches[1, ])
  expect_error(validate_matches(dup), "Duplicated")
})

test_that("load_shots: keeps open play only and records dropped types", {
  path <- write_fake_shots(list(
    fake_shot_row(1L, "e1", 1L, "A", "B", "Open Play"),
    fake_shot_row(1L, "e2", 1L, "A", "B", "Free Kick"),
    fake_shot_row(1L, "e3", 0L, "B", "A", "Penalty", 0.76)
  ))
  s <- load_shots(fake_matches, path = path)
  expect_equal(nrow(s), 1L)
  expect_true(all(s$set_piece == "Open Play"))
  dropped <- attr(s, "dropped_by_type")
  expect_equal(as.integer(dropped["Free Kick"]), 1L)
  expect_equal(as.integer(dropped["Penalty"]), 1L)
  expect_equal(nrow(load_shots(fake_matches, path = path, open_play_only = FALSE)), 3L)
})

test_that("load_shots: stops when shot teams disagree with the match table", {
  path <- write_fake_shots(list(fake_shot_row(1L, "e1", 1L, "B", "A", "Open Play")))
  expect_error(load_shots(fake_matches, path = path), "inconsistent")
})

test_that("load_shots: stops on duplicated event ids", {
  path <- write_fake_shots(list(
    fake_shot_row(1L, "e1", 1L, "A", "B", "Open Play"),
    fake_shot_row(1L, "e1", 1L, "A", "B", "Open Play")
  ))
  expect_error(load_shots(fake_matches, path = path), "Duplicated")
})

# ---- Aggregation ---------------------------------------------------------------------

test_that("contr_sum_within: columns sum to zero within each group", {
  M <- contr_sum_within(c("a", "b", "c", "d", "e"), c("g1", "g1", "g1", "g2", "g2"))
  expect_equal(dim(M), c(5L, 3L))
  expect_equal(unname(colSums(M[1:3, , drop = FALSE])), c(0, 0, 0))
  expect_equal(unname(colSums(M[4:5, , drop = FALSE])), c(0, 0, 0))
  expect_true(all(M[4:5, 1:2] == 0))
  expect_true(all(M[1:3, 3] == 0))
  expect_equal(qr(M)$rank, 3L)
})

test_that("contr_sum_within: rejects single-member groups", {
  expect_error(contr_sum_within(c("a", "b", "c"), c("g1", "g1", "g2")))
})

test_that("build_team_match: two rows per match, zero-filled, totals conserved", {
  tm <- build_team_match(fake_matches, fake_shots)
  expect_equal(nrow(tm), 6L)
  expect_equal(sum(tm$n_shots), nrow(fake_shots))
  expect_equal(sum(tm$npxg), sum(fake_shots$xg))

  g3 <- tm[tm$game_id == 3L, ]
  expect_equal(g3$n_shots, c(0L, 0L))
  expect_true(all(is.na(g3$xg_mean_shot)))

  a1 <- tm[tm$game_id == 1L & tm$att_team == "A", ]
  expect_equal(a1$n_shots, 2L)
  expect_equal(a1$xg_mean_shot, 0.15)
  expect_equal(a1$home, 1L)
})

test_that("build_team_match: stops when shot teams do not match the match table", {
  bad <- fake_shots
  bad$off_team[1] <- "Z"
  expect_error(build_team_match(fake_matches, bad), "not conserved")
})

test_that("build_team_match: single league drops the league effect", {
  tm <- build_team_match(fake_matches, fake_shots)
  expect_false("league_f" %in% model_rhs(tm))
  expect_equal(ncol(tm$ATT), 2L)
})

# ---- Shot quality ----------------------------------------------------------------------

test_that("shape_stats: recovers a known gamma shape", {
  set.seed(42)
  s <- shape_stats(rgamma(20000, shape = 0.5, rate = 5))
  expect_lt(abs(s$alpha_mom - 0.5), 0.03)
  expect_lt(abs(s$alpha_mle - 0.5), 0.03)
  expect_equal(p_from_shape(0.5), 2.5 / 1.5)
})

test_that("run_em: returns NULL instead of erroring on degenerate data", {
  expect_null(run_em(rep(0, 200), 2))
})

synthetic_xg <- function() {
  set.seed(7)
  x <- c(exp(rnorm(4000, log(0.04), 0.5)), exp(rnorm(600, log(0.4), 0.3)))
  x[x < 1]
}

test_that("fit_xg_mixture: recovers a two-component lognormal mixture", {
  x <- synthetic_xg()
  mix <- fit_xg_mixture(x, k_max = 2)
  comps <- mix$components[mix$components$k == 2, ]
  expect_equal(best_mixture_k(mix), 2)
  expect_lt(abs(comps$weight[2] - 600 / 4600), 0.03)
  expect_lt(abs(comps$median_xg[1] - 0.04), 0.006)
  expect_lt(abs(comps$median_xg[2] - 0.4), 0.06)
  expect_true(mix$fit_table$starts_at_best[2] >= 1)
  expect_length(big_chance_prob(mix), length(x))
})

test_that("assign_components: labels shots and gives a valid big-chance probability", {
  x <- synthetic_xg()
  mix <- fit_xg_mixture(x, k_max = 2)
  s <- assign_components(data.frame(xg = x), mix)
  expect_true(all(s$component %in% 1:2))
  expect_true(all(s$p_top >= 0 & s$p_top <= 1))
  expect_gt(mean(s$p_top[s$xg > 0.3]), 0.5)
  expect_error(assign_components(data.frame(xg = x[-1]), mix), "align")
})

test_that("xg_calibration_by_type: computes goals over expected by type", {
  d <- data.frame(set_piece = c("Open Play", "Open Play", "Free Kick", "Free Kick"),
                  shot_made = c(1L, 0L, 1L, 1L), xg = c(0.5, 0.5, 0.25, 0.25))
  out <- xg_calibration_by_type(d)
  expect_equal(out$ratio[out$set_piece == "Open Play"], 1)
  expect_equal(out$ratio[out$set_piece == "Free Kick"], 4)
})

# ---- Calibration ---------------------------------------------------------------------

test_that("calibration_stats: perfect predictions give slope 1 and zero RMSE", {
  set.seed(1)
  a <- rexp(500)
  cs <- calibration_stats(a, a)
  expect_equal(cs[["slope"]], 1)
  expect_equal(cs[["rmse"]], 0)
  expect_equal(cs[["r"]], 1)
})

# ---- Stints --------------------------------------------------------------------------

fake_stint <- function(game_id, num, period, minutes, home, away, n_home = 11L, n_away = 11L) {
  row <- data.frame(
    league = "L1", season = "2015/16", game_id = game_id,
    stint_id = paste0(game_id, "_", num), stint_num = num, period = period,
    start_sec = 0, end_sec = minutes * 60, duration_min = minutes,
    home_team = home, away_team = away, home_goals = 0L, away_goals = 0L,
    n_home = n_home, n_away = n_away, stringsAsFactors = FALSE
  )
  h <- c(seq_len(n_home), rep(NA_integer_, 11 - n_home))
  a <- c(100L + seq_len(n_away), rep(NA_integer_, 11 - n_away))
  for (i in 1:11) {
    row[[paste0("H", i)]] <- h[i]
    row[[paste0("A", i)]] <- a[i]
  }
  row
}

fake_stints <- do.call(rbind, list(
  fake_stint(1L, 1L, 1L, 47, "A", "B"), fake_stint(1L, 2L, 2L, 48, "A", "B"),
  fake_stint(2L, 1L, 1L, 47, "B", "C"), fake_stint(2L, 2L, 2L, 48, "B", "C"),
  fake_stint(3L, 1L, 1L, 47, "C", "A"), fake_stint(3L, 2L, 2L, 48, "C", "A")
))

stint_shots <- fake_shots
stint_shots$stint_id <- c("1_1", "1_2", "1_2", "2_1")
stint_shots$p_top <- c(0.1, 0.2, 0.9, 0.5)

test_that("validate_stints: accepts a consistent stints table", {
  expect_silent(validate_stints(fake_stints, fake_matches))
})

test_that("validate_stints: catches player columns disagreeing with counts", {
  bad <- fake_stints
  bad$H11[1] <- NA
  expect_error(validate_stints(bad, fake_matches), "disagree")
})

test_that("validate_stints: catches matches with no stints", {
  expect_error(validate_stints(fake_stints[fake_stints$game_id != 3L, ], fake_matches), "no stints")
})

test_that("build_stint_table: two rows per stint, totals conserved", {
  tm <- build_team_match(fake_matches, fake_shots)
  st <- build_stint_table(fake_stints, stint_shots, tm)
  expect_equal(nrow(st), 2L * nrow(fake_stints))
  expect_equal(sum(st$n_shots), nrow(stint_shots))
  expect_equal(sum(st$big_chances), sum(stint_shots$p_top))
  expect_true(all(lengths(st$att_players) == 11L))

  b12 <- st[st$stint_id == "1_2" & st$att_team == "B", ]
  expect_equal(b12$n_shots, 1L)
  expect_equal(b12$big_chances, 0.9)
  expect_equal(b12$home, 0L)
  expect_equal(b12$att_players[[1]], 100L + 1:11)
})

test_that("build_stint_table: stops when totals disagree with the team-match table", {
  tm <- build_team_match(fake_matches, fake_shots)
  tm$npxg[1] <- tm$npxg[1] + 1
  expect_error(build_stint_table(fake_stints, stint_shots, tm), "reproduce")
})

test_that("build_stint_table: requires big-chance probabilities on shots", {
  tm <- build_team_match(fake_matches, fake_shots)
  expect_error(build_stint_table(fake_stints, stint_shots[, c("game_id", "off_team", "xg", "stint_id")], tm),
               "p_top")
})

# ---- Identification diagnostics --------------------------------------------------------

fake_players <- data.frame(
  player_id = c(1:11, 101:111), player_name = paste0("P", c(1:11, 101:111)),
  team = "A", league = "L1",
  primary_position = c("Goalkeeper", rep("Midfield", 10), "Goalkeeper", rep("Midfield", 10)),
  is_goalkeeper = c(1L, rep(0L, 10), 1L, rep(0L, 10)),
  stringsAsFactors = FALSE
)
fake_st <- build_stint_table(fake_stints, stint_shots, build_team_match(fake_matches, fake_shots))

test_that("player_minutes: counts each stint once per player and flags goalkeepers", {
  pm <- player_minutes(fake_st, fake_players)
  expect_equal(nrow(pm), 22L)
  expect_true(all(pm$minutes == 3 * 95))
  expect_true(pm$is_goalkeeper[pm$player_id == 1L])
  expect_false(pm$is_goalkeeper[pm$player_id == 2L])
})

test_that("teammate_overlap: players always on together have overlap 1", {
  pm <- player_minutes(fake_st, fake_players)
  ov <- teammate_overlap(fake_st, pm, threshold = 0)
  expect_equal(nrow(ov), 2L * choose(10, 2))   # outfield pairs within each side
  expect_true(all(abs(ov$overlap - 1) < 1e-12))
})

test_that("estimability: correct SEs for a well-conditioned information matrix", {
  e <- estimability(diag(c(4, 9)))
  expect_equal(e$se, c(0.5, 1 / 3))
  expect_length(e$aliased, 0L)
  expect_equal(e$condition, 9 / 4)
})

test_that("estimability: detects aliasing when players are never apart", {
  pm <- player_minutes(fake_st, fake_players)
  pd <- player_design(fake_st, pm, threshold = 0)
  e <- estimability(fisher_info(pd$X, fake_st))
  expect_gt(length(intersect(e$aliased, pd$player_cols)), 0L)
})

test_that("ridge_table: effective df is bounded and grows with the prior SD", {
  pm <- player_minutes(fake_st, fake_players)
  rt <- ridge_table(fake_st, pm, threshold = 0, prior_sd = c(0.02, 0.2, 2))
  edf <- as.numeric(gsub(",", "", rt$`Effective df`))
  expect_true(all(edf <= rt$`Player parameters`))
  expect_true(all(diff(edf) >= 0))
})

# ---- Player models ---------------------------------------------------------------------

test_that("match_folds: every match lies in exactly one fold", {
  g <- rep(1:50, times = sample(2:8, 50, replace = TRUE))
  f <- match_folds(g, k = 5)
  expect_true(all(tapply(f, g, function(x) length(unique(x))) == 1L))
  expect_setequal(unique(f), 1:5)
})

test_that("tau_to_lambda: glmnet ridge equals the posterior mode under a N(0, tau^2) prior", {
  skip_if_not_installed("glmnet")
  set.seed(3)
  n <- 3000
  X <- cbind(z = rbinom(n, 1, 0.5), x = rbinom(n, 1, 0.3))
  y <- rpois(n, exp(0.2 + 0.1 * X[, "z"] + 0.4 * X[, "x"]))
  tau <- 0.1
  pfit <- fit_penalized(X, y, rep(0, n), pf = c(0, 1), taus = tau, thresh = 1e-14)
  b_glmnet <- coef_at(pfit, tau)
  negpost <- function(b) {
    eta <- b[1] + X %*% b[2:3]
    -sum(y * eta - exp(eta)) + b[3]^2 / (2 * tau^2)
  }
  b_map <- stats::optim(c(0, 0, 0), negpost, method = "BFGS",
                        control = list(reltol = 1e-14, maxit = 1000))$par
  expect_lt(max(abs(b_glmnet - b_map)), 1e-3)
})

test_that("choose_family: Poisson for Poisson data, negative binomial for overdispersed data", {
  set.seed(5)
  n <- 4000
  md <- list(X0 = Matrix::Matrix(cbind(h = rbinom(n, 1, 0.5)), sparse = TRUE),
             XT = Matrix::Matrix(matrix(0, n, 0), sparse = TRUE),
             offset = rep(log(10), n))
  md$y <- rpois(n, 1.2 * exp(0.1 * md$X0[, 1]))
  expect_false(choose_family(md)$use_nb)
  md$y <- MASS::rnegbin(n, mu = 1.2 * exp(0.1 * md$X0[, 1]), theta = 1)
  expect_true(choose_family(md)$use_nb)
})

test_that("player_effects: reads only player columns, not team or pooled ones", {
  b <- c(`(Intercept)` = 0.1, team_att_1 = 0.2, team_def_1 = -0.1,
         off_7 = 0.05, def_7 = -0.02, `off_pool_L1` = 0.01)
  pm <- data.frame(player_id = 7L, player_name = "P7", main_team = "A", minutes = 1000)
  e <- player_effects(b, pm)
  expect_equal(nrow(e), 2L)
  expect_setequal(e$side, c("Offense", "Defense"))
  expect_true(all(e$player_id == 7L))
})

# ---- Segments, time controls, positions -------------------------------------------------

test_that("build_stint_table: works on segments keyed by segment_id", {
  segs <- fake_stints
  segs$segment_id <- segs$stint_id
  segs$time_bin <- ifelse(segs$period == 1L, "P1 0-15", "P2 0-15")
  shots_seg <- stint_shots
  shots_seg$segment_id <- shots_seg$stint_id
  tm <- build_team_match(fake_matches, fake_shots)
  sg <- build_stint_table(segs, shots_seg, tm, id_col = "segment_id")
  expect_equal(sum(sg$n_shots), nrow(shots_seg))
  expect_true("time_bin" %in% names(sg))
})

test_that("add_game_state: score x time interaction needs time bins", {
  expect_error(add_game_state(fake_st, "score_time"), "time_bin")
  g <- fake_st
  g$time_bin <- "P1 0-15"
  expect_true(nlevels(add_game_state(g, "score_time")$game_state) >= 1L)
})

test_that("position_group: maps StatsBomb positions to coarse groups", {
  expect_equal(position_group(c("Goalkeeper", "Left Center Back", "Right Back",
                                "Right Wing Back", "Center Defensive Midfield",
                                "Left Wing", "Center Forward", "Unknown")),
               c("GK", "CB", "FB", "FB", "MF", "FW", "FW", NA))
})

test_that("position_counts: counts players by group on each side", {
  pos <- data.frame(player_id = c(1:11, 101:111),
                    group = c("GK", "CB", "CB", "FB", "FB", "MF", "MF", "MF", "FW", "FW", "FW",
                              "GK", "CB", "CB", "FB", "FB", "MF", "MF", "MF", "FW", "FW", "FW"))
  pc <- position_counts(fake_st, pos)
  expect_equal(colnames(pc), c("pos_att_CB", "pos_att_FB", "pos_att_FW",
                               "pos_def_GK", "pos_def_CB", "pos_def_FB", "pos_def_FW"))
  expect_true(all(as.matrix(pc)[, "pos_att_CB"] == 2))
  expect_true(all(as.matrix(pc)[, "pos_def_GK"] == 1))
  expect_true(all(as.matrix(pc)[, "pos_att_FW"] == 3))
})

# ---- Figures -------------------------------------------------------------------------

test_that("stint_length_shares: shares of stints and of minutes each sum to 1", {
  d <- stint_length_shares(data.frame(duration_min = c(1, 2, 8, 25, 47)))
  expect_equal(sum(d$share[d$measure == "Share of stints"]), 1)
  expect_equal(sum(d$share[d$measure == "Share of minutes"]), 1)
  expect_equal(d$share[d$measure == "Share of minutes" & d$bin == "45+"], 47 / 83)
})

test_that("team_profiles: one row per team with volume and quality", {
  tm <- build_team_match(fake_matches, fake_shots)
  tp <- team_profiles(tm)
  expect_equal(nrow(tp), 3L)
  a <- tp[tp$team == "A", ]
  expect_equal(a$shots_per_match, 1)       # 2 shots in 2 matches
  expect_equal(a$xg_per_shot, 0.15)
})

# ---- Pipeline pieces -------------------------------------------------------------------

synthetic_md <- function(n = 900, seed = 11) {
  set.seed(seed)
  game_id <- rep(seq_len(n / 3), each = 3)
  h <- rbinom(n, 1, 0.5)
  teams <- matrix(rbinom(n * 2, 1, 0.5), n, 2, dimnames = list(NULL, c("team_att_1", "team_def_1")))
  plyr <- matrix(rbinom(n * 3, 1, 0.4), n, 3, dimnames = list(NULL, c("off_1", "off_2", "def_3")))
  minutes <- runif(n, 2, 20)
  y <- rpois(n, minutes / 8 * exp(0.1 * h + 0.2 * teams[, 1] + 0.1 * plyr[, 1]))
  list(y = y, offset = log(minutes), minutes = minutes, game_id = game_id,
       X0 = Matrix::Matrix(cbind(home = h), sparse = TRUE),
       XT = Matrix::Matrix(teams, sparse = TRUE),
       XP = Matrix::Matrix(plyr, sparse = TRUE), pfP = c(1, 1, 1), XPos = NULL)
}

test_that("combine_cv: per-fold results combine into the same structure as cv_models", {
  skip_if_not_installed("glmnet")
  md <- synthetic_md()
  folds <- match_folds(md$game_id, k = 3)
  fam <- list(family = stats::poisson(), glmnet_family = "poisson")
  taus <- c(0.05, 0.2)
  parts <- lapply(c(3, 1, 2), function(f) cv_one_fold(md, folds, fam, f, taus))   # out of order
  cv <- combine_cv(parts, taus)
  ref <- cv_models(md, folds, fam, taus)
  expect_equal(cv$models, c("M0", "M1", "M2", "M3"))
  expect_equal(length(cv$dev$M0), 3L)
  expect_equal(dim(cv$dev$M3), c(3L, 2L))
  expect_equal(cv$dev, ref$dev)
  expect_true(best_tau(cv, "M3") %in% taus)
})

test_that("position_effects_table: jackknife SE matches the formula", {
  full <- c(pos_att_FW = 0.1, pos_def_CB = -0.05)
  jk <- list(c(pos_att_FW = 0.10, pos_def_CB = -0.04),
             c(pos_att_FW = 0.12, pos_def_CB = -0.06),
             c(pos_att_FW = 0.08, pos_def_CB = -0.05))
  pe <- position_effects_table(full, jk)
  expect_equal(pe$Side, c("Offense", "Defense"))
  expect_equal(pe$Group, c("FW", "CB"))
  v <- c(0.10, 0.12, 0.08)
  expect_equal(pe$se[1], sqrt(2 / 3 * sum((v - mean(v))^2)))
})

test_that("reliability_table: correlations by side", {
  sh <- data.frame(model = "M3", side = rep(c("Offense", "Defense"), each = 4),
                   half1 = c(1, 2, 3, 4, 1, 2, 3, 4), half2 = c(1, 2, 3, 4, 4, 3, 2, 1))
  rt <- reliability_table(sh)
  expect_equal(rt$Pearson, c(1, -1))
  expect_equal(rt$Players, c(4L, 4L))
  expect_equal(rt$Model, c("M3", "M3"))
})

test_that("raw_file: picks one file by name and refuses ambiguity", {
  files <- file.path("data", "raw", RAW_FILES)
  expect_equal(raw_file(files, "shots_1516.csv"), file.path("data", "raw", "shots_1516.csv"))
  expect_error(raw_file(files, "missing.csv"), "exactly one")
})

