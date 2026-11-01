# Part D: identifiability simulation. On the real lineup design, plant player
# effects of known size, simulate outcomes, and run the same estimation as the
# real analysis, to see what reliability the design allows and how it changes
# with event frequency, effect size, and seasons.
# Requires player_models.R (fit_glm_matrix, split_half_effects), helpers.R
# (fmt, plain_theme), dplyr.
#
# The prior SD is set to the true effect SD (the best case for ridge); the real
# analysis chooses it by cross-validation, which can only do slightly worse.
# "More seasons" repeats the same lineups, which is conservative: real seasons
# have roster turnover, which breaks teammate confounding.

SIM_SIGMA <- c(0.03, 0.06)              # true player-effect SD (log scale): about 3% and 6% per 90
SIM_RATES <- c(1, 11, 44, 150, 1000)    # events per team per 90
SIM_SEASONS <- c(1L, 2L, 4L)
SIM_REPS <- 3L
SIM_FAM <- list(family = stats::poisson(), glmnet_family = "poisson")

# Every sigma x rate at one season, plus 2 and 4 seasons at the shot and entry
# rates with the realistic sigma; each setting replicated SIM_REPS times
sim_grid <- function() {
  one <- expand.grid(sigma = SIM_SIGMA, rate = SIM_RATES, seasons = 1L)
  more <- expand.grid(sigma = SIM_SIGMA[1], rate = c(11, 44), seasons = setdiff(SIM_SEASONS, 1L))
  g <- rbind(one, more)
  g <- g[rep(seq_len(nrow(g)), each = SIM_REPS), ]
  g$rep <- rep(seq_len(SIM_REPS), times = nrow(g) / SIM_REPS)
  g$id <- seq_len(nrow(g))
  g$seed <- 2026L * 100L + g$id
  rownames(g) <- NULL
  g
}

# Non-player linear predictor (intercept, league, home, game state, player
# counts, teams) from M1 fit on the real outcome, without the offset
sim_fixed <- function(md, fam = SIM_FAM) {
  X <- cbind(md$X0, md$XT)
  b <- fit_glm_matrix(X, md$y, md$offset, fam$family)
  as.numeric(cbind(1, as.matrix(X)) %*% b)
}

# The design repeated `seasons` times with new game ids (same lineups)
stack_seasons <- function(md, seasons) {
  if (seasons == 1L) return(md)
  rep_rows <- function(M) do.call(rbind, rep(list(M), seasons))
  out <- md
  for (nm in c("X0", "XT", "XP", "XPos")) if (!is.null(md[[nm]])) out[[nm]] <- rep_rows(md[[nm]])
  for (nm in c("y", "offset", "minutes")) out[[nm]] <- rep(md[[nm]], seasons)
  step <- 10^ceiling(log10(max(md$game_id) + 1))
  out$game_id <- unlist(lapply(seq_len(seasons) - 1L, function(k) md$game_id + k * step))
  out
}

# One simulated dataset: plant N(0, sigma^2) effects on every individual player
# column (pooled replacement columns get 0), scale the intercept to the target
# event rate, draw Poisson outcomes, fit M3 on each half of the matches.
# Returns one row per side: reliability (half vs half, as in the real analysis)
# and accuracy (half-season estimates vs the true effects, only knowable here).
sim_one <- function(md, eta_fixed, setting) {
  set.seed(setting$seed)
  S <- as.integer(setting$seasons)
  mds <- stack_seasons(md, S)
  beta <- numeric(ncol(md$XP))
  players <- which(md$pfP == 1)
  beta[players] <- stats::rnorm(length(players), 0, setting$sigma)
  lin <- rep(eta_fixed, S) + as.numeric(mds$XP %*% beta)
  shift <- log(setting$rate * sum(mds$minutes) / 90 / sum(exp(lin + mds$offset)))
  mds$y <- stats::rpois(length(lin), exp(lin + shift + mds$offset))

  taus <- setting$sigma * c(4, 2, 1)                  # short path ending at the true SD
  sh <- split_half_effects(mds, SIM_FAM, setting$sigma, "M3", taus = taus)
  truth <- stats::setNames(beta, colnames(md$XP))
  t <- truth[paste0(ifelse(sh$side == "Offense", "off_", "def_"), sh$player_id)]
  do.call(rbind, lapply(c("Offense", "Defense"), function(s) {
    i <- sh$side == s
    data.frame(id = setting$id, sigma = setting$sigma, rate = setting$rate, seasons = S,
               rep = setting$rep, side = s, players = sum(i),
               reliability = stats::cor(sh$half1[i], sh$half2[i]),
               accuracy = mean(c(stats::cor(sh$half1[i], t[i]), stats::cor(sh$half2[i], t[i]))))
  }))
}

# ---- Summaries -------------------------------------------------------------------------

sim_summary <- function(res) {
  res |>
    dplyr::group_by(sigma, rate, seasons, side) |>
    dplyr::summarise(rel_sd = stats::sd(reliability), reliability = mean(reliability),
                     accuracy = mean(accuracy), reps = dplyr::n(), .groups = "drop") |>
    as.data.frame()
}

# Where each real outcome's label goes (events per 90, reliability): in the
# empty space above the curves (upper left) or below them (lower right), joined
# to its point by a thin line, so labels never sit on the curves or each other
SIM_LABEL_AT <- data.frame(outcome = c("goals", "shots", "box_entries", "final_third_entries"),
                           lx = c(1.3, 4, 60, 250),
                           ly = c(0.30, 0.42, -0.05, 0.05))

# Simulated reliability against events per 90 (one season), one line per true
# effect SD, with the real outcomes from Part B as points
plot_simulation <- function(sim, real) {
  d <- sim[sim$seasons == 1L, ]
  d$effect <- paste0("True SD ", fmt(100 * d$sigma, 0), "%")
  pts <- rbind(data.frame(outcome = real$outcome, per90 = real$per90, rel = real$rel_off, side = "Offense",
                          label = real$label),
               data.frame(outcome = real$outcome, per90 = real$per90, rel = real$rel_def, side = "Defense",
                          label = real$label))
  at <- match(pts$outcome, SIM_LABEL_AT$outcome)
  pts$lx <- ifelse(is.na(at), pts$per90, SIM_LABEL_AT$lx[at])            # other outcomes: just above
  pts$ly <- ifelse(is.na(at), pts$rel + 0.1, SIM_LABEL_AT$ly[at])
  ggplot2::ggplot(d, ggplot2::aes(x = rate, y = reliability)) +
    ggplot2::geom_line(ggplot2::aes(linetype = effect)) +
    ggplot2::geom_point(ggplot2::aes(shape = effect), size = 1.5) +
    ggplot2::geom_point(data = pts, ggplot2::aes(x = per90, y = rel), inherit.aes = FALSE,
                        size = 2.5, color = "gray20") +
    ggplot2::geom_segment(data = pts, ggplot2::aes(x = per90, y = rel, xend = lx, yend = ly),
                          inherit.aes = FALSE, color = "gray60", linewidth = 0.3) +
    ggplot2::geom_text(data = pts, ggplot2::aes(x = lx, y = ly, label = label), inherit.aes = FALSE,
                       size = 2.8, color = "gray20", vjust = ifelse(pts$ly >= pts$rel, -0.3, 1.3)) +
    ggplot2::facet_wrap(~ side) +
    ggplot2::scale_x_log10(expand = ggplot2::expansion(mult = 0.1)) +
    ggplot2::scale_y_continuous(limits = c(min(0, d$reliability, pts$rel, pts$ly) - 0.05, 1)) +
    ggplot2::labs(title = "Reliability the lineup design allows, one season",
                  x = "Events per team per 90 (log scale)",
                  y = "Split-half reliability (dark points = real outcomes)",
                  linetype = NULL, shape = NULL) +
    plain_theme()
}

# Reliability and accuracy by number of seasons at the shot and entry rates
simulation_table <- function(sim) {
  d <- sim[sim$sigma == SIM_SIGMA[1] & sim$rate %in% c(11, 44), ]
  d <- d[order(d$side, d$rate, d$seasons), ]
  data.frame(Side = d$side, `Events per 90` = fmt(d$rate, 0), Seasons = d$seasons,
             Reliability = fmt(d$reliability, 3), `Accuracy (vs truth)` = fmt(d$accuracy, 3),
             check.names = FALSE, row.names = NULL)
}
