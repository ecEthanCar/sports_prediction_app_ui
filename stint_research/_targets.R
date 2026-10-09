# The whole analysis as one pipeline. From the project root:
#
#   targets::tar_make()          run everything that is out of date
#   targets::tar_visnetwork()    see every step and what depends on what
#   targets::tar_read(cv3)       pull any result into your session
#
# Each step reruns only when its inputs or the functions it uses change. The R
# functions live in R/; the reports in reports/ only read results.

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("dplyr", "ggplot2", "Matrix", "MASS", "mcglm", "mixtools", "glmnet",
               "readr", "tibble"),
  format = "rds"
)

# Parallel workers if the crew package is installed. Default: up to 4, leaving
# one core free. Set SB_WORKERS=1 in ~/.Renviron to run everything in one session.
n_cores <- suppressWarnings(parallel::detectCores())
default_workers <- if (is.na(n_cores)) 1L else max(1L, min(4L, n_cores - 1L))
workers <- as.integer(Sys.getenv("SB_WORKERS", as.character(default_workers)))
if (!is.na(workers) && workers > 1L && requireNamespace("crew", quietly = TRUE)) {
  tar_option_set(controller = crew::crew_controller_local(workers = workers))
}

tar_source("R")

list(
  # ---- Data: StatsBomb build (cached download) and validated tables ----------------------
  tar_target(build_script, "build/build_shots.py", format = "file"),
  tar_target(raw_files, run_build(build_script), format = "file"),
  tar_target(matches, load_matches(raw_file(raw_files, "matches_1516.csv"))),
  tar_target(shots_all, load_shots(matches, raw_file(raw_files, "shots_1516.csv"),
                                   open_play_only = FALSE)),
  tar_target(shots_open, load_shots(matches, raw_file(raw_files, "shots_1516.csv"))),
  tar_target(team_match, build_team_match(matches, shots_open)),

  # ---- Stage 1: volume, quality, big chances ----------------------------------------------
  tar_target(tweedie, fit_tweedie(team_match)),
  tar_target(mixture, fit_xg_mixture(shots_open$xg)),
  tar_target(stage1, assemble_models(team_match, shots_open, tweedie, mixture)),
  tar_target(shots, assign_components(shots_open, mixture)),
  tar_target(stability_league, mixture_by_group(shots_open$xg, shots_open$league)),
  tar_target(stability_half, mixture_by_group(shots_open$xg, match_halves(shots_open))),

  # ---- Stage 2: stints, segments, identification -------------------------------------------
  tar_target(stints, load_stints(matches, raw_file(raw_files, "stints_1516.csv"))),
  tar_target(segments, load_segments(matches, stints, raw_file(raw_files, "segments_1516.csv"))),
  tar_target(players, load_players(raw_file(raw_files, "players_1516.csv"))),
  tar_target(stint_table, build_stint_table(stints, shots, team_match)),
  tar_target(segment_table, build_stint_table(segments, shots, team_match, id_col = "segment_id")),
  tar_target(player_mins, player_minutes(stint_table, players)),
  tar_target(overlap, teammate_overlap(stint_table, player_mins)),
  tar_target(precision, precision_data(stint_table, player_mins, team_match)),
  tar_target(ridge, ridge_table(stint_table, player_mins)),
  tar_target(thresholds, threshold_table(stint_table, player_mins)),

  # ---- Stage 3: player models on stints (folds run in parallel) ----------------------------
  tar_target(fold_ids, seq_len(N_FOLDS)),
  tar_target(md3, player_model_data(stint_table, player_mins)),
  tar_target(fam3, choose_family(md3)),
  tar_target(folds3, match_folds(md3$game_id)),
  tar_target(cv3_fold, cv_one_fold(md3, folds3, fam3, fold_ids),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(cv3, combine_cv(cv3_fold)),
  tar_target(tau3, best_tau(cv3, "M3")),
  tar_target(sanity3, team_sanity(md3, fam3, team_match)),
  tar_target(split_half3, split_half_effects(md3, fam3, tau3, "M3")),
  tar_target(coef3, fit_final(md3, "M3", tau3, fam3)),
  tar_target(effects3, player_effects(coef3, player_mins)),

  # ---- Stage 3b: segments, score x time controls, positional prior -------------------------
  tar_target(positions, player_positions(players)),
  tar_target(seg_mins, player_minutes(segment_table, players)),
  tar_target(md3b, player_model_data(segment_table, seg_mins, controls = "score_time",
                                     positions = positions)),
  tar_target(md3b_score, player_model_data(segment_table, seg_mins, controls = "score",
                                           positions = positions)),
  tar_target(fam3b, choose_family(md3b)),
  tar_target(folds3b, match_folds(md3b$game_id)),
  tar_target(cv3b_fold, cv_one_fold(md3b, folds3b, fam3b, fold_ids),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(cv3b, combine_cv(cv3b_fold)),
  tar_target(tau3b_m3, best_tau(cv3b, "M3")),
  tar_target(tau3b_m4, best_tau(cv3b, "M4")),
  tar_target(sanity3b, team_sanity(md3b, fam3b, team_match)),
  tar_target(split_half3b_m3, split_half_effects(md3b, fam3b, tau3b_m3, "M3")),
  tar_target(split_half3b_m4, split_half_effects(md3b, fam3b, tau3b_m4, "M4")),
  tar_target(pos_time_full, position_coefs(md3b, fam3b, tau3b_m4)),
  tar_target(pos_time_jk, position_coefs(md3b, fam3b, tau3b_m4, rows = folds3b != fold_ids),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(pos_score_full, position_coefs(md3b_score, fam3b, tau3b_m4)),
  tar_target(pos_score_jk, position_coefs(md3b_score, fam3b, tau3b_m4, rows = folds3b != fold_ids),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(position_fx, list(score = position_effects_table(pos_score_full, pos_score_jk),
                               score_time = position_effects_table(pos_time_full, pos_time_jk))),

  # ---- Reports: read results only ------------------------------------------------------------
  tar_render(report_team, "reports/1_team_models.Rmd"),
  tar_render(report_stints, "reports/2_stints.Rmd"),
  tar_render(report_players, "reports/3_players.Rmd"),
  tar_render(report_positions, "reports/3b_positions.Rmd"),
  tar_render(report_figures, "reports/figures.Rmd")
)
