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
  expect_equal(sum(st$xg), sum(stint_shots$xg))
  expect_true(all(lengths(st$att_players) == 11L))

  b12 <- st[st$stint_id == "1_2" & st$att_team == "B", ]
  expect_equal(b12$n_shots, 1L)
  expect_equal(b12$xg, 0.3)
  expect_equal(b12$home, 0L)
  expect_equal(b12$att_players[[1]], 100L + 1:11)
})

test_that("build_stint_table: stops when totals disagree with the team-match table", {
  tm <- build_team_match(fake_matches, fake_shots)
  tm$npxg[1] <- tm$npxg[1] + 1
  expect_error(build_stint_table(fake_stints, stint_shots, tm), "reproduce")
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

test_that("build_segment_table: keyed by segment_id, keeps time bins, needs no big-chance column", {
  segs <- fake_stints
  segs$segment_id <- segs$stint_id
  segs$time_bin <- ifelse(segs$period == 1L, "P1 0-15", "P2 0-15")
  shots_seg <- stint_shots
  shots_seg$segment_id <- shots_seg$stint_id
  tm <- build_team_match(fake_matches, fake_shots)
  sg <- build_segment_table(segs, shots_seg, tm)
  expect_equal(sum(sg$n_shots), nrow(shots_seg))
  expect_true("time_bin" %in% names(sg))
  expect_false("big_chances" %in% names(sg))
  expect_equal(sg, build_stint_table(segs, shots_seg, tm, id_col = "segment_id"))
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
                               "pos_def_CB", "pos_def_FB", "pos_def_FW"))
  expect_true(all(as.matrix(pc)[, "pos_att_CB"] == 2))
  expect_true(all(as.matrix(pc)[, "pos_def_CB"] == 2))
  expect_true(all(as.matrix(pc)[, "pos_att_FW"] == 3))
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

test_that("combine_cv: fold order does not matter; vectors for unpenalized, matrices for penalized", {
  skip_if_not_installed("glmnet")
  md <- synthetic_md()
  folds <- match_folds(md$game_id, k = 3)
  fam <- list(family = stats::poisson(), glmnet_family = "poisson")
  taus <- c(0.05, 0.2)
  parts <- lapply(1:3, function(f) cv_one_fold(md, folds, fam, f, taus, models = c("M0", "M1", "M3")))
  cv <- combine_cv(parts[c(3, 1, 2)], taus)
  expect_equal(cv, combine_cv(parts, taus))
  expect_equal(cv$models, c("M0", "M1", "M3"))
  expect_equal(length(cv$dev$M0), 3L)
  expect_equal(dim(cv$dev$M3), c(3L, 2L))
  expect_true(best_tau(cv, "M3") %in% taus)
})

test_that("raw_file: picks one file by name and refuses ambiguity", {
  files <- file.path("data", "raw", RAW_FILES)
  expect_equal(raw_file(files, "shots_1516.csv"), file.path("data", "raw", "shots_1516.csv"))
  expect_error(raw_file(files, "missing.csv"), "exactly one")
})

test_that("baseline_summary: gains, prior SD and reliability by side", {
  md <- synthetic_md()
  folds <- match_folds(md$game_id, k = 3)
  fam <- list(family = stats::poisson(), glmnet_family = "poisson")
  taus <- c(0.05, 0.2)
  cv <- combine_cv(lapply(1:3, function(f) cv_one_fold(md, folds, fam, f, taus)), taus)
  sh <- data.frame(side = rep(c("Offense", "Defense"), each = 3), player_id = rep(1:3, 2),
                   half1 = c(1, 2, 3, 1, 2, 3), half2 = c(1, 2, 3, 3, 2, 1))
  bs <- baseline_summary(cv, sh)
  expect_equal(nrow(bs), 5L)
  expect_match(bs$Value[2], "SE")
  expect_equal(bs$Value[4], "1.000 (3 players)")
  expect_equal(bs$Value[5], "-1.000 (3 players)")
})

# ---- A. Structured models -------------------------------------------------------------

test_that("add_role_interactions: 15 pairwise products of the position counts", {
  md <- synthetic_md(9)
  md$XPos <- Matrix::Matrix(cbind(pos_att_CB = c(2, 2, 3), pos_att_FB = c(2, 1, 2), pos_att_FW = c(1, 2, 3),
                                  pos_def_CB = c(2, 2, 2), pos_def_FB = c(2, 2, 1), pos_def_FW = c(2, 1, 1))[
                                    rep(1:3, 3), ], sparse = TRUE)
  md2 <- add_role_interactions(md)
  expect_equal(ncol(md2$XRole), 15L)
  expect_true("att_CB_x_att_FW" %in% colnames(md2$XRole))
  expect_equal(as.numeric(md2$XRole[1:3, "att_CB_x_att_FW"]), c(2, 4, 9))
  expect_error(model_matrix(md, "MRoles"), "XRole")
  mm <- model_matrix(md2, "MRoles")
  expect_equal(sum(mm$pf == 0), ncol(md$X0) + ncol(md$XT) + 6 + 15)
})

test_that("build_laplacian: one edge gives [1 -1; -1 1]; isolated players get a zero row", {
  md <- synthetic_md(90)
  md$XP <- cbind(md$XP, Matrix::Matrix(0, nrow(md$XP), 1, dimnames = list(NULL, "off_9"), sparse = TRUE))
  ov <- data.frame(player_i = 1L, player_j = 2L, shared = 120, overlap = 0.5)
  L <- build_laplacian(md, ov)
  expect_equal(rownames(L), c("off_1", "off_2", "off_9"))
  expect_equal(unname(as.matrix(L)[1:2, 1:2]), matrix(c(1, -1, -1, 1), 2))
  expect_equal(as.numeric(as.matrix(L)[3, ]), c(0, 0, 0))
})

test_that("build_graph_penalty: kappa = 0 is the ridge penalty; kappa adds the Laplacian", {
  md <- synthetic_md(90)
  L <- build_laplacian(md, data.frame(player_i = 1L, player_j = 2L, shared = 120, overlap = 0.5))
  mm <- model_matrix(md, "M3")
  P0 <- build_graph_penalty(md, L, kappa = 0)
  expect_equal(unname(as.matrix(P0)), diag(as.numeric(mm$pf > 0)))
  P1 <- build_graph_penalty(md, L, kappa = 2)
  expect_equal(P1["off_1", "off_2"], -2)
  expect_equal(P1["off_1", "off_1"], 3)
  expect_equal(P1["def_3", "def_3"], 1)
})

test_that("fit_graph_ridge: kappa = 0 reproduces glmnet ridge", {
  md <- synthetic_md()
  L <- build_laplacian(md, data.frame(player_i = 1L, player_j = 2L, shared = 120, overlap = 0.5))
  mm <- model_matrix(md, "M3")
  taus <- c(0.05, 0.2)
  g <- fit_graph_ridge(mm$X, md$y, md$offset, build_graph_penalty(md, L, 0), taus)
  r <- fit_penalized(mm$X, md$y, md$offset, mm$pf, taus)
  expect_equal(unname(predict_graph_ridge(g, mm$X, md$offset)),
               unname(predict_penalized(r, mm$X, md$offset)), tolerance = 1e-3)
  expect_equal(coef_at(g, 0.2), coef_at(r, 0.2), tolerance = 1e-2)
})

test_that("cv_gain and sh_reliability", {
  cv_a <- list(dev = list(M3 = matrix(c(10, 12, 11, 9), 2)), k = 2)
  cv_b <- list(dev = list(MX = c(9, 10)), k = 2)
  g <- cv_gain(cv_a, "M3", cv_b, "MX")
  expect_equal(unname(g["gain"]), sum(c(11, 9) - c(9, 10)))   # column 2 is M3's best
  sh <- data.frame(side = rep(c("Offense", "Defense"), each = 3),
                   half1 = c(1, 2, 3, 1, 2, 3), half2 = c(1, 2, 3, 3, 2, 1))
  expect_equal(unname(sh_reliability(sh)), c(1, -1))
})

# ---- B. Frequency ladder --------------------------------------------------------------

test_that("frequency_outcomes: aligned, zero-filled, conserved; stops on unknown teams", {
  seg <- data.frame(unit_id = c("1_1", "1_1", "1_2", "1_2"), att_team = c("A", "B", "A", "B"),
                    n_shots = c(2L, 0L, 1L, 1L))
  shots <- data.frame(segment_id = c("1_1", "1_1", "1_2", "1_2"), off_team = c("A", "A", "A", "B"),
                      shot_made = c(0L, 1L, 0L, 0L))
  ev <- data.frame(game_id = 1L, segment_id = c("1_1", "1_2"), team = c("A", "B"),
                   final_third_entries = c(7L, 4L), box_entries = c(3L, 1L))
  y <- frequency_outcomes(seg, shots, ev)
  expect_equal(names(y), names(FREQ_OUTCOMES))
  expect_equal(y$goals, c(1, 0, 0, 0))
  expect_equal(y$shots, c(2, 0, 1, 1))
  expect_equal(y$final_third_entries, c(7, 0, 0, 4))
  bad <- ev; bad$team[1] <- "Z"
  expect_error(frequency_outcomes(seg, shots, bad), "match no segment")
})

test_that("frequency CV: the shots outcome reproduces the baseline; outcomes combine separately", {
  md <- synthetic_md()
  folds <- match_folds(md$game_id, k = 3)
  fam <- list(family = stats::poisson(), glmnet_family = "poisson")
  taus <- c(0.05, 0.2)
  y <- data.frame(shots = md$y, final_third_entries = md$y * 4L)
  parts <- unlist(lapply(c("final_third_entries", "shots"), function(o)
    lapply(3:1, function(f) outcome_cv_fold(md, y, o, folds, fam, f, taus))), recursive = FALSE)
  names(parts) <- paste0("freq_fold_", seq_along(parts))
  cvs <- combine_by_outcome(parts, taus)
  expect_equal(names(cvs), c("shots", "final_third_entries"))   # FREQ_OUTCOMES order
  ref <- combine_cv(lapply(1:3, function(f) cv_one_fold(md, folds, fam, f, taus, FREQ_MODELS)), taus)
  expect_equal(cvs$shots$dev, ref$dev)
})

# ---- C. Interactions --------------------------------------------------------------------

test_that("role_coefs and role_coef_table: 21 role terms, jackknife SE formula", {
  md <- synthetic_md()
  set.seed(4)
  md$XPos <- Matrix::Matrix(matrix(rpois(nrow(md$X0) * 6, 2), ncol = 6,
                                   dimnames = list(NULL, c("pos_att_CB", "pos_att_FB", "pos_att_FW",
                                                           "pos_def_CB", "pos_def_FB", "pos_def_FW"))),
                            sparse = TRUE)
  md <- add_role_interactions(md)
  fam <- list(family = stats::poisson(), glmnet_family = "poisson")
  b <- role_coefs(md, fam, 0.05, taus = c(0.05, 0.2))
  expect_length(b, 21L)
  expect_equal(names(b)[1], "pos_att_CB")
  jk <- list(c(a = 0.10, b = 0.0), c(a = 0.12, b = 0.1), c(a = 0.08, b = -0.1))
  tab <- role_coef_table(c(a = 0.1, b = 0), jk)
  v <- c(0.10, 0.12, 0.08)
  expect_equal(tab$`SE (%)`[1], fmt(100 * sqrt(2 / 3 * sum((v - mean(v))^2)), 1))
})
