# The three figures, in a plain style. Requires models.R and diagnostics.R.

plain_theme <- function() {
  ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

# 1. Do the estimates of the Tweedie index agree?
plot_p_compare <- function(res) {
  df <- p_estimates(res)
  df$method <- factor(df$method, levels = rev(df$method))
  ggplot2::ggplot(df, ggplot2::aes(x = p, y = method)) +
    ggplot2::geom_vline(xintercept = res$power[["p"]], linetype = "dashed", color = "gray50") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lo, xmax = hi), na.rm = TRUE) +
    ggplot2::geom_point() +
    ggplot2::coord_cartesian(xlim = c(1, 2)) +
    ggplot2::labs(title = "Estimates of the Tweedie index",
                  x = "p (dashed line = fitted value; bars = 95% intervals)", y = NULL) +
    plain_theme()
}

# 2. Shot xG on a log scale with the BIC-best mixture and its components.
# comps can be passed directly (e.g. the components saved in stage1_outputs.rds).
plot_shape <- function(shots, res, comps = best_mixture_components(res$mixture)) {
  y <- log(shots$xg[shots$xg > 0])
  grid <- seq(min(y), max(y), length.out = 500)

  comp_dens <- do.call(rbind, lapply(seq_len(nrow(comps)), function(j) {
    data.frame(y = grid, component = factor(j),
               density = comps$weight[j] * stats::dnorm(grid, log(comps$median_xg[j]), comps$sdlog[j]))
  }))
  total <- data.frame(y = grid,
                      density = Reduce(`+`, split(comp_dens$density, comp_dens$component)))
  breaks <- c(0.01, 0.03, 0.1, 0.3, 0.9)

  ggplot2::ggplot(data.frame(y = y), ggplot2::aes(x = y)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 60, fill = "gray80", color = NA) +
    ggplot2::geom_line(data = comp_dens, ggplot2::aes(x = y, y = density, group = component),
                       linetype = "dashed", color = "gray40") +
    ggplot2::geom_line(data = total, ggplot2::aes(x = y, y = density)) +
    ggplot2::scale_x_continuous(breaks = log(breaks), labels = breaks) +
    ggplot2::labs(title = "Distribution of xG per shot",
                  x = sprintf("xG per shot, log scale (solid = %d-component mixture, dashed = components)",
                              nrow(comps)),
                  y = "Density") +
    plain_theme()
}

# 3. Recombined NB-G predictions vs actual team-match xG
plot_calibration <- function(tm, pred, cal) {
  df <- data.frame(pred = pred, actual = tm$npxg)
  dec <- df |>
    dplyr::mutate(bin = dplyr::ntile(pred, 10)) |>
    dplyr::group_by(bin) |>
    dplyr::summarise(pred = mean(pred), actual = mean(actual), .groups = "drop")
  lim <- c(0, unname(stats::quantile(c(df$pred, df$actual), 0.995)))

  ggplot2::ggplot(df, ggplot2::aes(x = pred, y = actual)) +
    ggplot2::geom_point(alpha = 0.15, size = 0.8, color = "gray40") +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ggplot2::geom_point(data = dec, size = 2) +
    ggplot2::coord_cartesian(xlim = lim, ylim = lim) +
    ggplot2::labs(title = sprintf("NB-G calibration (slope %.2f, r = %.3f)", cal[["slope"]], cal[["r"]]),
                  x = "Predicted xG (dark points = decile means)", y = "Actual xG") +
    plain_theme()
}
