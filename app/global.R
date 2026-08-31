# ==============================================================================
# global.R  -- runs once, before the app starts
# ==============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(scales)

# Shiny >= 1.5 auto-sources R/, but the load order relative to global.R is not
# worth relying on, and the lesson script sources these files the same way.
# Re-sourcing plain function definitions is harmless.
SUPPORT_FILES <- c("data.R", "model.R", "labels.R", "plots.R",
                   "theme.R", "docs.R")

for (f in SUPPORT_FILES) {
  path <- if (file.exists(file.path("R", f))) file.path("R", f)
          else if (file.exists(f)) f
          else stop("Cannot find ", f, ". Working directory is ", getwd(), call. = FALSE)
  source(path)
}

LEAGUE_TABLE <- available_leagues()

LEAGUE_NAMES <- if (nrow(LEAGUE_TABLE) > 0) {
  LEAGUE_TABLE$league
} else {
  # footBayes is not installed: fall back to a simulated league so the app,
  # and the lesson, still run end to end.
  SYNTHETIC_LEAGUE_NAME
}
