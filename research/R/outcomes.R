# Part B: the frequency ladder. Does player reliability rise with more
# frequent outcomes, as it does in hockey (goals -> shots -> Corsi)?
# Requires player_models.R (cv_one_fold, combine_cv, best_tau, fold_deviance,
# split_half_effects, sh_reliability), helpers.R (fmt, plain_theme), dplyr.
#
# Same rows, offset, folds and prior grid as the baseline; only the outcome
# changes. All outcomes are counts, fit with a Poisson likelihood (a proper
# score for the mean even if entries are overdispersed).

FREQ_OUTCOMES <- c(goals = "Goals", shots = "Shots", box_entries = "Box entries",
                   final_third_entries = "Final 3rd entries")
FREQ_MODELS <- c("M0", "M1", "M3")

load_segment_events <- function(path) {
  readr::read_csv(path, show_col_types = FALSE,
                  col_types = readr::cols(game_id = readr::col_integer(),
                                          segment_id = readr::col_character(),
                                          team = readr::col_character(),
                                          final_third_entries = readr::col_integer(),
                                          box_entries = readr::col_integer()))
}

# One column per outcome, aligned to the rows of the segment table; zero where
# a row had none. Stops if event teams or segments do not match the table.
frequency_outcomes <- function(segment_table, shots, seg_events) {
  keys <- as.data.frame(segment_table)[, c("unit_id", "att_team")]
  goals <- dplyr::summarise(dplyr::group_by(shots, unit_id = segment_id, att_team = off_team),
                            goals = sum(shot_made), .groups = "drop")
  ev <- dplyr::transmute(seg_events, unit_id = segment_id, att_team = team,
                         box_entries, final_third_entries)
  unmatched <- dplyr::anti_join(ev, keys, by = c("unit_id", "att_team"))
  if (nrow(unmatched)) stop(nrow(unmatched), " segment-event rows match no segment row (check team names).")
  out <- keys |>
    dplyr::left_join(goals, by = c("unit_id", "att_team")) |>
    dplyr::left_join(ev, by = c("unit_id", "att_team"))
  for (v in c("goals", "box_entries", "final_third_entries")) out[[v]] <- dplyr::coalesce(out[[v]], 0L)
  out$shots <- segment_table$n_shots
  if (sum(out$goals) != sum(shots$shot_made)) stop("Goals not conserved.")
  if (sum(out$box_entries) != sum(ev$box_entries) ||
      sum(out$final_third_entries) != sum(ev$final_third_entries)) stop("Entries not conserved.")
  as.data.frame(out[, names(FREQ_OUTCOMES)])
}

with_outcome <- function(md, y) {
  if (length(y) != length(md$y)) stop("Outcome length differs from the model data.")
  md$y <- as.numeric(y)
  md
}

outcome_cv_fold <- function(md, y_tab, outcome, folds, fam, fold, taus = PRIOR_SD_GRID) {
  r <- cv_one_fold(with_outcome(md, y_tab[[outcome]]), folds, fam, fold, taus, FREQ_MODELS)
  r$outcome <- outcome
  r
}

# Fold results (any order, any outcome) -> one combine_cv() result per outcome
combine_by_outcome <- function(fold_results, taus = PRIOR_SD_GRID) {
  fold_results <- unname(fold_results)
  outcome <- vapply(fold_results, `[[`, character(1), "outcome")
  out <- lapply(split(fold_results, outcome), combine_cv, taus = taus)
  out[intersect(names(FREQ_OUTCOMES), names(out))]
}

outcome_split_half <- function(md, y_tab, outcome, fam, tau) {
  sh <- split_half_effects(with_outcome(md, y_tab[[outcome]]), fam, tau, "M3")
  sh$outcome <- outcome
  sh
}

# Per outcome: events per team per 90, held-out gain of players over teams
# (in SEs, since deviance scales differ), and split-half reliability
frequency_summary <- function(cvs, shs, y_tab, minutes) {
  shs <- stats::setNames(shs, vapply(shs, function(d) d$outcome[1], character(1)))
  do.call(rbind, lapply(names(cvs), function(o) {
    cv <- cvs[[o]]
    d <- fold_deviance(cv, "M1") - fold_deviance(cv, "M3")
    r <- sh_reliability(shs[[o]])
    data.frame(outcome = o, label = FREQ_OUTCOMES[[o]],
               per90 = 90 * sum(y_tab[[o]]) / sum(minutes),
               gain = sum(d), se = sqrt(cv$k) * stats::sd(d),
               rel_off = r[["Offense"]], rel_def = r[["Defense"]],
               tau = best_tau(cv, "M3"))
  }))
}

frequency_table <- function(s) {
  data.frame(
    Outcome = s$label,
    `Per team per 90` = fmt(s$per90, 1),
    `Players over teams: gain (SE)` = sprintf("%s (%s)", fmt(s$gain, 1), fmt(s$se, 1)),
    `Gain / SE` = fmt(s$gain / s$se, 1),
    `Prior SD (%)` = fmt(100 * (exp(s$tau) - 1), 1),
    `Reliability: offense` = fmt(s$rel_off, 3),
    `Reliability: defense` = fmt(s$rel_def, 3),
    check.names = FALSE, row.names = NULL
  )
}

plot_frequency_ladder <- function(s) {
  d <- rbind(data.frame(per90 = s$per90, label = s$label, rel = s$rel_off, side = "Offense"),
             data.frame(per90 = s$per90, label = s$label, rel = s$rel_def, side = "Defense"))
  ggplot2::ggplot(d, ggplot2::aes(x = per90, y = rel, linetype = side, shape = side)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray60") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_text(data = d[d$side == "Offense", ], ggplot2::aes(label = label),
                       vjust = -1, size = 3, show.legend = FALSE) +
    ggplot2::scale_x_log10(expand = ggplot2::expansion(mult = 0.2)) +
    ggplot2::labs(title = "Player reliability by outcome frequency",
                  x = "Events per team per 90 (log scale)",
                  y = "Split-half reliability of player effects", linetype = NULL, shape = NULL) +
    plain_theme()
}
