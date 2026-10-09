# Orchestration, summary tables, and table rendering.
# Requires load.R, aggregate.R, models.R.

# Reference values from the Understat EPL analysis (2014/15 to 2024/25)
UNDERSTAT_REF <- c(
  shots = "105,449", team_matches = "8,360", zero_shot = "3",
  mean_shots = "12.6", mean_quality = "0.106", mean_npxg = "1.34",
  p_fit = "1.299 (0.059)", split_implied = "70 / 30",
  elasticities = "0.66 (0.011) / 0.34 (0.014)", elasticity_sum = "1.001",
  alpha_mom = "0.52", alpha_mle = "0.95", p_shape = "1.66", p_gap_se = "~6",
  quasi_disp = "1.47", theta = "33.1 (1.9)", aic_gain = "495", nb_dev_df = "1.08",
  gamma_disp = "2.41 vs 1.93",
  mixture = "k = 2: 87.8% / 12.2% at 0.044 / 0.438",
  cal_slope = "0.98", r_tweedie = "0.579", r_nbg = "0.578", rmse = "0.699",
  top_decile = "2.39 vs 2.37 (2.30 to 2.44)"
)

# Everything stage 1 reports, from the two slow fits (Tweedie, mixture), which
# the pipeline caches as separate steps. run_models() does it all in one call.
run_models <- function(tm, shots, control = TWEEDIE_CONTROL) {
  assemble_models(tm, shots, fit_tweedie(tm, control), fit_xg_mixture(shots$xg))
}

assemble_models <- function(tm, shots, tw, mixture) {
  mu <- tweedie_fitted(tw)
  if (length(mu) != nrow(tm)) stop("Tweedie fitted values do not align with team-match rows.")
  vol <- fit_volume(tm)
  qual <- fit_quality(tm)
  pred_nbg <- recombine_nbg(tm, vol, qual)

  list(
    mu_tweedie = mu, power = tweedie_power(tw),
    elasticity = fit_elasticities(tm, mu),
    shape = shape_stats(shots$xg),
    volume = vol, quality = qual,
    mixture = mixture,
    pred_nbg = pred_nbg,
    cal_tweedie = calibration_stats(tm$npxg, mu),
    cal_nbg = calibration_stats(tm$npxg, pred_nbg),
    n_shots = nrow(shots), n_team_match = nrow(tm),
    n_zero = sum(tm$n_shots == 0),
    mean_shots = mean(tm$n_shots),
    mean_quality = mean(tm$xg_mean_shot, na.rm = TRUE),
    mean_npxg = mean(tm$npxg)
  )
}

# The five estimates of the Tweedie index used in the p-compare figure
p_estimates <- function(res) {
  el <- res$elasticity; pw <- res$power; sh <- res$shape
  z <- stats::qnorm(0.975)
  data.frame(
    method = c("Tweedie fit", "Volume elasticity (2 - e)", "Quality elasticity (1 + e)",
               "Shot shape (moments)", "Shot shape (MLE)"),
    p  = c(pw[["p"]], 2 - el$e_vol, 1 + el$e_sev,
           p_from_shape(sh$alpha_mom), p_from_shape(sh$alpha_mle)),
    lo = c(pw[["p"]] - z * pw[["se"]], 2 - el$e_vol - z * el$se_vol,
           1 + el$e_sev - z * el$se_sev, NA, NA),
    hi = c(pw[["p"]] + z * pw[["se"]], 2 - el$e_vol + z * el$se_vol,
           1 + el$e_sev + z * el$se_sev, NA, NA)
  )
}

fmt <- function(x, d = 3) formatC(as.numeric(x), format = "f", digits = d, big.mark = ",")

build_summary_table <- function(res) {
  el <- res$elasticity; pw <- res$power; sh <- res$shape
  vol <- res$volume; cal_t <- res$cal_tweedie; cal_n <- res$cal_nbg
  p_shape <- p_from_shape(sh$alpha_mom)
  comps <- best_mixture_components(res$mixture)
  mix_str <- sprintf("k = %d: %s at %s", nrow(comps),
                     paste0(fmt(100 * comps$weight, 1), "%", collapse = " / "),
                     paste(fmt(comps$median_xg, 3), collapse = " / "))

  rows <- list(
    c("Shots analysed (StatsBomb: open play; Understat: non-penalty)", fmt(res$n_shots, 0), UNDERSTAT_REF[["shots"]]),
    c("Team-match rows", fmt(res$n_team_match, 0), UNDERSTAT_REF[["team_matches"]]),
    c("Team-match rows with zero shots", fmt(res$n_zero, 0), UNDERSTAT_REF[["zero_shot"]]),
    c("Mean shots per team-match", fmt(res$mean_shots, 1), UNDERSTAT_REF[["mean_shots"]]),
    c("Mean xG per shot (team-match average)", fmt(res$mean_quality, 3), UNDERSTAT_REF[["mean_quality"]]),
    c("Mean xG per team-match", fmt(res$mean_npxg, 2), UNDERSTAT_REF[["mean_npxg"]]),
    c("Tweedie p (SE)", sprintf("%s (%s)", fmt(pw[["p"]]), fmt(pw[["se"]])), UNDERSTAT_REF[["p_fit"]]),
    c("Implied split, volume / quality (%)",
      sprintf("%.0f / %.0f", 100 * (2 - pw[["p"]]), 100 * (pw[["p"]] - 1)), UNDERSTAT_REF[["split_implied"]]),
    c("Observed elasticities, volume / quality (SE)",
      sprintf("%s (%s) / %s (%s)", fmt(el$e_vol, 2), fmt(el$se_vol), fmt(el$e_sev, 2), fmt(el$se_sev)),
      UNDERSTAT_REF[["elasticities"]]),
    c("Sum of elasticities", fmt(el$sum), UNDERSTAT_REF[["elasticity_sum"]]),
    c("Shot xG gamma shape, moments", fmt(sh$alpha_mom, 2), UNDERSTAT_REF[["alpha_mom"]]),
    c("Shot xG gamma shape, MLE", fmt(sh$alpha_mle, 2), UNDERSTAT_REF[["alpha_mle"]]),
    c("p implied by shot shape", fmt(p_shape, 2), UNDERSTAT_REF[["p_shape"]]),
    c("Shape-implied p minus fitted p, in SEs", fmt((p_shape - pw[["p"]]) / pw[["se"]], 1), UNDERSTAT_REF[["p_gap_se"]]),
    c("Quasi-Poisson dispersion (volume)", fmt(vol$quasi_dispersion, 2), UNDERSTAT_REF[["quasi_disp"]]),
    c("NB theta (SE)", sprintf("%s (%s)", fmt(vol$theta, 1), fmt(vol$theta_se, 1)), UNDERSTAT_REF[["theta"]]),
    c("AIC improvement, NB over Poisson", fmt(vol$aic_pois - vol$aic_nb, 0), UNDERSTAT_REF[["aic_gain"]]),
    c("NB residual deviance / df", fmt(vol$nb_dev_df, 2), UNDERSTAT_REF[["nb_dev_df"]]),
    c("Gamma dispersion vs 1/alpha benchmark",
      sprintf("%s vs %s", fmt(res$quality$dispersion, 2), fmt(1 / sh$alpha_mom, 2)), UNDERSTAT_REF[["gamma_disp"]]),
    c("Shot xG mixture (BIC-best)", mix_str, UNDERSTAT_REF[["mixture"]]),
    c("Calibration slope (Tweedie)", fmt(cal_t[["slope"]], 2), UNDERSTAT_REF[["cal_slope"]]),
    c("r, Tweedie", fmt(cal_t[["r"]]), UNDERSTAT_REF[["r_tweedie"]]),
    c("r, NB-G", fmt(cal_n[["r"]]), UNDERSTAT_REF[["r_nbg"]]),
    c("RMSE, Tweedie", fmt(cal_t[["rmse"]]), UNDERSTAT_REF[["rmse"]]),
    c("Top decile, predicted vs actual (95% CI)",
      sprintf("%s vs %s (%s to %s)", fmt(cal_t[["top_pred"]], 2), fmt(cal_t[["top_actual"]], 2),
              fmt(cal_t[["top_lo"]], 2), fmt(cal_t[["top_hi"]], 2)),
      UNDERSTAT_REF[["top_decile"]])
  )

  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("Metric", "StatsBomb 2015/16 (4 leagues)", "Understat EPL 2014/15 to 2024/25")
  out
}

# ---- Tables accompanying each figure ------------------------------------------------

# p-compare: each estimate of p, its interval, and its distance from the fitted p
p_compare_table <- function(res) {
  df <- p_estimates(res)
  data.frame(
    Estimate = df$method,
    p = fmt(df$p, 3),
    `95% interval` = ifelse(is.na(df$lo), "-", sprintf("%s to %s", fmt(df$lo, 3), fmt(df$hi, 3))),
    `Gap from fitted p (in fitted-p SEs)` = fmt((df$p - res$power[["p"]]) / res$power[["se"]], 1),
    check.names = FALSE
  )
}

# Shape: mixture components, BIC gain, and where the trough falls (if any)
shape_table <- function(shots, res) {
  mix <- res$mixture
  comps <- best_mixture_components(mix)
  ft <- mix$fit_table
  bic_gain <- ft$BIC[ft$k == 1] - min(ft$BIC)

  grid <- seq(log(min(comps$median_xg)), log(max(comps$median_xg)), length.out = 2000)
  dens <- Reduce(`+`, lapply(seq_len(nrow(comps)), function(j) {
    comps$weight[j] * stats::dnorm(grid, log(comps$median_xg[j]), comps$sdlog[j])
  }))
  i_min <- which.min(dens)
  trough <- if (nrow(comps) > 1 && i_min > 1 && i_min < length(grid)) exp(grid[i_min]) else NA_real_
  p_big <- big_chance_prob(mix)

  rbind(
    data.frame(Quantity = c("Shots", "Components chosen by BIC", "BIC improvement over one component"),
               Value = c(fmt(nrow(shots), 0), as.character(nrow(comps)), fmt(bic_gain, 0))),
    data.frame(Quantity = sprintf("Component %d: weight / median xG / sdlog", comps$component),
               Value = sprintf("%s%% / %s / %s", fmt(100 * comps$weight, 1),
                               fmt(comps$median_xg, 3), fmt(comps$sdlog, 2))),
    data.frame(Quantity = c("Trough in mixture density (xG)",
                            "Shots most likely in top component"),
               Value = c(if (is.na(trough)) "none (no interior minimum)" else fmt(trough, 3),
                         if (is.null(p_big)) "-" else sprintf("%s%%", fmt(100 * mean(p_big > 0.5), 1))))
  )
}

# Calibration: fit, both tails, and the worst decile
calibration_table <- function(tm, pred) {
  actual <- tm$npxg
  fit <- stats::lm(actual ~ pred)
  dec <- dplyr::ntile(pred, 10)
  dm_pred <- as.numeric(tapply(pred, dec, mean))
  dm_act <- as.numeric(tapply(actual, dec, mean))
  ci_bot <- stats::t.test(actual[dec == 1])$conf.int
  ci_top <- stats::t.test(actual[dec == 10])$conf.int
  gap <- dm_act - dm_pred
  i_gap <- which.max(abs(gap))

  data.frame(
    Quantity = c("Slope / intercept",
                 "r / RMSE",
                 "Bottom decile: predicted vs actual (95% CI)",
                 "Top decile: predicted vs actual (95% CI)",
                 "Largest decile gap, actual minus predicted"),
    Value = c(
      sprintf("%s / %s", fmt(stats::coef(fit)[2], 2), fmt(stats::coef(fit)[1], 3)),
      sprintf("%s / %s", fmt(stats::cor(actual, pred), 3), fmt(sqrt(mean((actual - pred)^2)), 3)),
      sprintf("%s vs %s (%s to %s)", fmt(dm_pred[1], 2), fmt(dm_act[1], 2), fmt(ci_bot[1], 2), fmt(ci_bot[2], 2)),
      sprintf("%s vs %s (%s to %s)", fmt(dm_pred[10], 2), fmt(dm_act[10], 2), fmt(ci_top[1], 2), fmt(ci_top[2], 2)),
      sprintf("%s (decile %d)", fmt(gap[i_gap], 3), i_gap)
    )
  )
}

# Render a table for PDF or HTML. In PDF, writes a plain LaTeX tabular
# (no extra LaTeX packages, no longtable), in a slightly smaller font.
show_table <- function(df) {
  if (knitr::is_latex_output()) {
    tab <- knitr::kable(df, format = "latex", booktabs = FALSE)
    knitr::asis_output(paste0("\\begin{small}\n", tab, "\n\\end{small}\n"))
  } else {
    knitr::kable(df)
  }
}
