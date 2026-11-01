# The research pipeline. From this folder (research/):
#
#   targets::tar_make()          run everything that is out of date
#   targets::tar_visnetwork()    see every step and what depends on what
#   targets::tar_read(base_cv)   pull any result into your session
#
# Organized by the four parts of the project; reports/results.Rmd displays
# them. Each step reruns only when its inputs or the functions it uses change.
#
#   Data   StatsBomb build, segments, identification diagnostics
#   A      shot baseline (M0, M1, M3) and structured versions (M4, MRoles, MGraph)
#   B      frequency ladder: the baseline on goals, shots, box and final-third entries
#   C      interactions on B's best outcome (to add if B succeeds)
#   D      identifiability simulation (to add)

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("dplyr", "ggplot2", "Matrix", "MASS", "glmnet", "readr", "tibble"),
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
  tar_target(shots_open, load_shots(matches, raw_file(raw_files, "shots_1516.csv"))),
  tar_target(team_match, build_team_match(matches, shots_open)),
  tar_target(stints, load_stints(matches, raw_file(raw_files, "stints_1516.csv"))),
  tar_target(segments, load_segments(matches, stints, raw_file(raw_files, "segments_1516.csv"))),
  tar_target(players, load_players(raw_file(raw_files, "players_1516.csv"))),
  tar_target(segment_table, build_segment_table(segments, shots_open, team_match)),
  tar_target(player_mins, player_minutes(segment_table, players)),
  tar_target(overlap, teammate_overlap(segment_table, player_mins)),
  tar_target(precision, precision_data(segment_table, player_mins, team_match)),

  # ---- A. Shot baseline (folds run in parallel) ------------------------------------------
  tar_target(fold_ids, seq_len(N_FOLDS)),
  tar_target(positions, player_positions(players)),
  tar_target(md_base, player_model_data(segment_table, player_mins, controls = "score_time",
                                        positions = positions)),
  tar_target(folds_base, match_folds(md_base$game_id)),
  tar_target(fam_base, list(family = stats::poisson(), glmnet_family = "poisson")),
  tar_target(base_fold, cv_one_fold(md_base, folds_base, fam_base, fold_ids),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(base_cv, combine_cv(base_fold)),
  tar_target(base_tau, best_tau(base_cv, "M3")),
  tar_target(base_sh, split_half_effects(md_base, fam_base, base_tau, "M3")),

  # ---- A. Structured versions of M3, same folds -------------------------------------------
  tar_target(md_roles, add_role_interactions(md_base)),
  tar_target(pos_fold, cv_one_fold(md_roles, folds_base, fam_base, fold_ids, models = c("M4", "MRoles")),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(pos_cv, combine_cv(pos_fold)),
  tar_target(m4_sh, split_half_effects(md_roles, fam_base, best_tau(pos_cv, "M4"), "M4")),
  tar_target(roles_sh, split_half_effects(md_roles, fam_base, best_tau(pos_cv, "MRoles"), "MRoles")),
  tar_target(L_norm, build_laplacian(md_base, overlap)),
  tar_target(md_graph, c(md_base, list(P_graph = build_graph_penalty(md_base, L_norm, GRAPH_KAPPA)))),
  tar_target(graph_fold, cv_one_fold(md_graph, folds_base, fam_base, fold_ids, models = "MGraph"),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(graph_cv, combine_cv(graph_fold)),
  tar_target(graph_sh, split_half_effects(md_graph, fam_base, best_tau(graph_cv, "MGraph"), "MGraph")),

  # ---- B. Frequency ladder (4 outcomes x 10 folds in parallel) ---------------------------
  tar_target(events_script, "build/build_events.py", format = "file"),
  tar_target(events_file, run_events_build(events_script, after = raw_files), format = "file"),
  tar_target(seg_events, load_segment_events(events_file)),
  tar_target(freq_y, frequency_outcomes(segment_table, shots_open, seg_events)),
  tar_target(freq_ids, names(FREQ_OUTCOMES)),
  tar_target(freq_fold, outcome_cv_fold(md_base, freq_y, freq_ids, folds_base, fam_base, fold_ids),
             pattern = cross(freq_ids, fold_ids), iteration = "list"),
  tar_target(freq_cv, combine_by_outcome(freq_fold)),
  tar_target(freq_tau, vapply(freq_cv, best_tau, numeric(1), model = "M3")),
  tar_target(freq_sh, outcome_split_half(md_base, freq_y, freq_ids, fam_base, freq_tau[[freq_ids]]),
             pattern = map(freq_ids), iteration = "list"),

  # ---- C. Interactions on final-third entries (same folds) --------------------------------
  tar_target(md_ft, with_outcome(md_roles, freq_y$final_third_entries)),
  tar_target(ft_pos_fold, cv_one_fold(md_ft, folds_base, fam_base, fold_ids, models = c("M4", "MRoles")),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(ft_pos_cv, combine_cv(ft_pos_fold)),
  tar_target(ft_roles_tau, best_tau(ft_pos_cv, "MRoles")),
  tar_target(ft_m4_sh, split_half_effects(md_ft, fam_base, best_tau(ft_pos_cv, "M4"), "M4")),
  tar_target(ft_roles_sh, split_half_effects(md_ft, fam_base, ft_roles_tau, "MRoles")),
  tar_target(ft_roles_full, role_coefs(md_ft, fam_base, ft_roles_tau)),
  tar_target(ft_roles_jk, role_coefs(md_ft, fam_base, ft_roles_tau, rows = folds_base != fold_ids),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(md_ft_graph, c(md_ft, list(P_graph = build_graph_penalty(md_ft, L_norm, GRAPH_KAPPA)))),
  tar_target(ft_graph_fold, cv_one_fold(md_ft_graph, folds_base, fam_base, fold_ids, models = "MGraph"),
             pattern = map(fold_ids), iteration = "list"),
  tar_target(ft_graph_cv, combine_cv(ft_graph_fold)),
  tar_target(ft_graph_sh, split_half_effects(md_ft_graph, fam_base, best_tau(ft_graph_cv, "MGraph"), "MGraph")),
  
  # ---- D. Identifiability simulation (42 simulated datasets in parallel) ------------------
  tar_target(sim_cells, sim_grid()),
  tar_target(sim_ids, sim_cells$id),
  tar_target(sim_eta, sim_fixed(md_base)),
  tar_target(sim_res, sim_one(md_base, sim_eta, sim_cells[sim_cells$id == sim_ids, ]),
             pattern = map(sim_ids)),
  
  # ---- Report: reads results only ------------------------------------------------------------
  tar_render(report_results, "reports/results.Rmd")
)
