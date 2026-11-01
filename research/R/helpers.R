# Small formatting helpers shared by the tables and plots.

fmt <- function(x, d = 3) formatC(as.numeric(x), format = "f", digits = d, big.mark = ",")

# Plain table for HTML (kable) or PDF (LaTeX tabular)
show_table <- function(df) {
  if (knitr::is_latex_output()) {
    tab <- knitr::kable(df, format = "latex", booktabs = FALSE)
    knitr::asis_output(paste0("\\begin{small}\n", tab, "\n\\end{small}\n"))
  } else {
    knitr::kable(df)
  }
}

plain_theme <- function() {
  ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
