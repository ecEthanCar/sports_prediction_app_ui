# ==============================================================================
# plots.R  -- ggplot builders
# Take data frames, return ggplot objects. No reactives, no inputs.
# ==============================================================================

#' Attack vs defence quadrant map.
#'
#' `ratings` must already carry a `gd` column (realised goal difference) and a
#' `team_label` column. Fill encodes goal difference rather than att + def,
#' because att + def is a linear combination of the two axes and would just be
#' position restated as colour. The disagreement between the two is the lesson:
#' the model adjusts for opponent quality and venue, goal difference does not.
plot_quadrant <- function(ratings, title, subtitle = NULL, caption = NULL) {
  pad   <- 0.15
  lim_x <- range(ratings$def) + c(-pad, pad)
  lim_y <- range(ratings$att) + c(-pad, pad)

  quad_fill <- tibble::tibble(
    xmin = c(0, -Inf, -Inf, 0),
    xmax = c(Inf, 0, 0, Inf),
    ymin = c(0, 0, -Inf, -Inf),
    ymax = c(Inf, Inf, 0, 0),
    fill = c("#e8f5e9", "#fff3e0", "#ffebee", "#e1f5fe")
  )

  quad_text <- tibble::tibble(
    x     = c(lim_x[2], lim_x[1], lim_x[1], lim_x[2]) * 0.98,
    y     = c(lim_y[2], lim_y[2], lim_y[1], lim_y[1]) * 0.98,
    hjust = c(1, 0, 0, 1),
    vjust = c(1, 1, 0, 0),
    col   = c("#1b5e20", "#e65100", "#b71c1c", "#01579b"),
    label = c("strong attack, strong defense",
              "high attack, weak defense",
              "weak attack, weak defense",
              "strong defense, weak attack")
  )

  # Guard the degenerate case (every team level on goal difference).
  gd_limit <- max(abs(ratings$gd), na.rm = TRUE)
  if (!is.finite(gd_limit) || gd_limit == 0) gd_limit <- 1

  ggplot2::ggplot(ratings, ggplot2::aes(x = def, y = att)) +
    ggplot2::geom_rect(
      data = quad_fill, inherit.aes = FALSE,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = quad_fill$fill, alpha = 0.45
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.5) +
    ggplot2::geom_text(
      data = quad_text, inherit.aes = FALSE,
      ggplot2::aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      color = quad_text$col, size = 3.4, fontface = "italic"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = gd), shape = 21, size = 4.4,
      color = "white", stroke = 0.7
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#d32f2f", mid = "#fbc02d", high = "#388e3c",
      midpoint = 0, limits = c(-gd_limit, gd_limit),
      name = "Actual goal difference"
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = team_label),
      size = 3.6, fontface = "bold", color = "grey15",
      box.padding = 0.35, point.padding = 0.25,
      force = 1, min.segment.length = 0,
      max.overlaps = Inf, seed = 42,
      segment.color = "grey55", segment.size = 0.35
    ) +
    ggplot2::scale_x_continuous(
      breaks = round(seq(-1, 1, 0.2), 1),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    ggplot2::scale_y_continuous(
      breaks = round(seq(-1, 1, 0.2), 1),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    ggplot2::coord_cartesian(xlim = lim_x, ylim = lim_y, expand = FALSE) +
    ggplot2::labs(
      title = title, subtitle = subtitle, caption = caption,
      x = "Defensive strength (+ = concedes fewer goals)",
      y = "Attacking strength (+ = scores more goals)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.margin     = ggplot2::margin(t = -4),
      legend.key.width  = ggplot2::unit(2.2, "cm"),
      legend.key.height = ggplot2::unit(0.32, "cm"),
      legend.title      = ggplot2::element_text(size = 10),
      legend.text       = ggplot2::element_text(size = 9),
      plot.title        = ggplot2::element_text(face = "bold", size = 15,
                                                margin = ggplot2::margin(b = 4)),
      plot.subtitle     = ggplot2::element_text(color = "grey30", size = 11,
                                                margin = ggplot2::margin(b = 12)),
      plot.caption      = ggplot2::element_text(color = "grey45", size = 9),
      axis.title.x      = ggplot2::element_text(face = "bold",
                                                margin = ggplot2::margin(t = 6)),
      axis.title.y      = ggplot2::element_text(face = "bold",
                                                margin = ggplot2::margin(r = 8)),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.border      = ggplot2::element_rect(color = "grey85", fill = NA),
      plot.margin       = ggplot2::margin(t = 10, r = 12, b = 6, l = 10)
    )
}

#' Scoreline probability heatmap.
#'
#' `actual` is an optional length-2 numeric c(home_goals, away_goals). When the
#' result falls inside the displayed grid it gets a dark outline. That is the
#' whole visual footprint of the real-world comparison: one cell border, no
#' extra panel, no shifted layout. Results outside the grid are reported in the
#' head-to-head panel instead.
plot_scoreline <- function(score_mat, home_name, away_name,
                           display_max = 5, actual = NULL) {
  grid <- dplyr::filter(score_mat, Home <= display_max, Away <= display_max)
  lev  <- as.character(0:display_max)

  grid <- dplyr::mutate(
    grid,
    Home_f = factor(Home, levels = lev),
    Away_f = factor(Away, levels = lev)
  )

  p <- ggplot2::ggplot(grid, ggplot2::aes(x = Away_f, y = Home_f, fill = prob)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", prob * 100)),
      fontface = "bold", size = 3.6, color = "grey15"
    ) +
    ggplot2::scale_fill_gradient(
      low = "#f1f8e9", high = "#2e7d32",
      labels = scales::percent_format(accuracy = 1)
    )

  marked <- FALSE
  if (!is.null(actual) && all(is.finite(actual)) &&
      actual[1] <= display_max && actual[2] <= display_max) {
    cell <- tibble::tibble(
      Home_f = factor(actual[1], levels = lev),
      Away_f = factor(actual[2], levels = lev)
    )
    p <- p + ggplot2::geom_tile(
      data = cell, inherit.aes = FALSE,
      ggplot2::aes(x = Away_f, y = Home_f),
      fill = NA, color = "#111827", linewidth = 1.4
    )
    marked <- TRUE
  }

  p +
    ggplot2::labs(
      x = paste(away_name, "goals"),
      y = paste(home_name, "goals"),
      fill = "Probability",
      caption = if (marked) "Outlined cell: what actually happened in this fixture." else NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid      = ggplot2::element_blank(),
      axis.title      = ggplot2::element_text(face = "bold"),
      plot.caption    = ggplot2::element_text(color = "grey45", size = 9, hjust = 0),
      legend.position = "right"
    )
}
