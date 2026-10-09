# Stage 2 identification diagnostics: can stint data separate individual
# players? Measurement only; no player effects are estimated here.
# Requires stints.R, models.R (fit_volume), diagnostics.R (fmt), and dplyr.

IDENT_THRESHOLDS <- c(300, 900, 1800)
IDENT_DEFAULT_THRESHOLD <- 900
RIDGE_PRIOR_SD <- c(0.02, 0.05, 0.10, 0.20, 0.50)   # log scale

# ---- Long format and minutes ---------------------------------------------------------

# One row per (stint-side row, player) for the attacking or defending side
long_players <- function(st, side = c("att", "def")) {
  side <- match.arg(side)
  lst <- if (side == "att") st$att_players else st$def_players
  n <- lengths(lst)
  data.frame(row = rep(seq_len(nrow(st)), n),
             player_id = unlist(lst, use.names = FALSE),
             minutes = rep(st$minutes, n),
             team = rep(if (side == "att") st$att_team else st$def_team, n),
             league = rep(st$league, n),
             stringsAsFactors = FALSE)
}

# Minutes per player, counted from attacking rows so each stint counts once.
# One effect per player across teams; main team = team with the most minutes.
player_minutes <- function(st, players) {
  gk <- players |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(is_goalkeeper = any(is_goalkeeper == 1L, na.rm = TRUE),
                     player_name = dplyr::first(player_name), .groups = "drop")
  long_players(st, "att") |>
    dplyr::group_by(player_id, team, league) |>
    dplyr::summarise(minutes = sum(minutes), .groups = "drop") |>
    dplyr::arrange(player_id, dplyr::desc(minutes)) |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(total_minutes = sum(minutes), n_teams = dplyr::n(),
                     main_team = dplyr::first(team), main_league = dplyr::first(league),
                     .groups = "drop") |>
    dplyr::rename(minutes = total_minutes) |>
    dplyr::left_join(gk, by = "player_id") |>
    dplyr::mutate(is_goalkeeper = dplyr::coalesce(is_goalkeeper, FALSE))
}

minutes_table <- function(pm, thresholds = IDENT_THRESHOLDS) {
  total <- sum(pm$minutes)
  data.frame(
    `Minutes threshold` = c("All players", paste0(">= ", thresholds)),
    Players = c(nrow(pm), vapply(thresholds, function(t) sum(pm$minutes >= t), integer(1))),
    `Share of player-minutes` = c("100.0%", vapply(thresholds, function(t) {
      sprintf("%.1f%%", 100 * sum(pm$minutes[pm$minutes >= t]) / total)
    }, character(1))),
    `Players on more than one team` = c(sum(pm$n_teams > 1),
                                        vapply(thresholds, function(t) sum(pm$n_teams > 1 & pm$minutes >= t), integer(1))),
    check.names = FALSE
  )
}

# Goalkeepers: how close to always-on they are for their main team
goalkeeper_table <- function(st, pm, threshold = IDENT_DEFAULT_THRESHOLD) {
  team_minutes <- st |>
    dplyr::group_by(att_team) |>
    dplyr::summarise(team_minutes = sum(minutes), .groups = "drop")
  gk <- pm |>
    dplyr::filter(is_goalkeeper) |>
    dplyr::left_join(team_minutes, by = c(main_team = "att_team")) |>
    dplyr::mutate(share = minutes / team_minutes)
  regular <- gk[gk$minutes >= threshold, ]
  data.frame(
    Quantity = c("Goalkeepers appearing",
                 sprintf("Goalkeepers with >= %d minutes", threshold),
                 "Median share of team minutes (those goalkeepers)",
                 "Goalkeepers playing more than 90% of team minutes"),
    Value = c(fmt(nrow(gk), 0), fmt(nrow(regular), 0),
              sprintf("%s%%", fmt(100 * stats::median(regular$share), 1)),
              fmt(sum(gk$share > 0.9), 0))
  )
}

# ---- Teammate overlap ------------------------------------------------------------------

# Sparse rows x players incidence matrix (1 if the player is on the pitch)
incidence <- function(long, ids, n_rows) {
  keep <- long$player_id %in% ids
  Matrix::sparseMatrix(i = long$row[keep], j = match(long$player_id[keep], ids),
                       x = 1, dims = c(n_rows, length(ids)))
}

# For outfield players above the threshold: minutes each pair spends on the
# pitch together, and overlap = shared / min(minutes_i, minutes_j). An overlap
# of 1 means the lower-minute player is never on without the other.
teammate_overlap <- function(st, pm, threshold = IDENT_DEFAULT_THRESHOLD) {
  ids <- pm$player_id[pm$minutes >= threshold & !pm$is_goalkeeper]
  A <- incidence(long_players(st, "att"), ids, nrow(st))
  shared <- Matrix::crossprod(A, Matrix::Diagonal(x = st$minutes) %*% A)
  mins <- as.numeric(Matrix::crossprod(A, st$minutes))
  shared <- methods::as(shared, "generalMatrix")
  up <- methods::as(Matrix::triu(shared, k = 1), "TsparseMatrix")
  i <- up@i + 1L; j <- up@j + 1L; x <- up@x
  keep <- x > 0
  i <- i[keep]; j <- j[keep]; x <- x[keep]
  data.frame(player_i = ids[i], player_j = ids[j], shared = x,
             overlap = x / pmin(mins[i], mins[j]))
}

overlap_summary <- function(ov) {
  data.frame(
    Quantity = c("Teammate pairs sharing any minutes",
                 "Pairs with overlap above 0.90",
                 "Pairs with overlap above 0.95",
                 "Median overlap"),
    Value = c(fmt(nrow(ov), 0),
              sprintf("%s (%s%%)", fmt(sum(ov$overlap > 0.90), 0), fmt(100 * mean(ov$overlap > 0.90), 1)),
              sprintf("%s (%s%%)", fmt(sum(ov$overlap > 0.95), 0), fmt(100 * mean(ov$overlap > 0.95), 1)),
              fmt(stats::median(ov$overlap), 2))
  )
}

top_overlap_pairs <- function(ov, pm, n = 10) {
  nm <- stats::setNames(pm$player_name, pm$player_id)
  tm <- stats::setNames(pm$main_team, pm$player_id)
  top <- ov[order(-ov$overlap, -ov$shared), ][seq_len(min(n, nrow(ov))), ]
  data.frame(Team = tm[as.character(top$player_i)],
             `Player 1` = nm[as.character(top$player_i)],
             `Player 2` = nm[as.character(top$player_j)],
             `Shared minutes` = round(top$shared),
             Overlap = round(top$overlap, 3),
             check.names = FALSE, row.names = NULL)
}

# ---- Design matrices -------------------------------------------------------------------

# Intercept, league (if more than one), and home
fixed_design <- function(st) {
  d <- data.frame(league = factor(st$league), home = st$home)
  form <- if (nlevels(d$league) > 1L) ~ league + home else ~ home
  Matrix::sparse.model.matrix(form, data = d)
}

# Player design: offense indicators for outfield players above the threshold,
# defense indicators for all players above it (goalkeepers included), pooled
# below-threshold counts per league, and league + home. Defense is coded +1
# (a positive effect means more shots conceded).
player_design <- function(st, pm, threshold = IDENT_DEFAULT_THRESHOLD) {
  leagues <- sort(unique(st$league))
  off_ids <- pm$player_id[pm$minutes >= threshold & !pm$is_goalkeeper]
  def_ids <- pm$player_id[pm$minutes >= threshold]
  gk_ids <- pm$player_id[pm$is_goalkeeper]
  n <- nrow(st)
  la <- long_players(st, "att")
  ld <- long_players(st, "def")

  pooled <- function(rows) {
    Matrix::sparseMatrix(i = rows, j = match(st$league[rows], leagues), x = 1,
                         dims = c(n, length(leagues)))
  }
  P_off <- pooled(la$row[!(la$player_id %in% off_ids) & !(la$player_id %in% gk_ids)])
  P_def <- pooled(ld$row[!(ld$player_id %in% def_ids)])
  fixed <- fixed_design(st)

  X <- cbind(fixed, incidence(la, off_ids, n), incidence(ld, def_ids, n), P_off, P_def)
  colnames(X) <- c(colnames(fixed), paste0("off_", off_ids), paste0("def_", def_ids),
                   paste0("off_pool_", leagues), paste0("def_pool_", leagues))
  list(X = X, player_cols = grep("^(off|def)_[0-9]+$", colnames(X)),
       n_off = length(off_ids), n_def = length(def_ids))
}

# Team design, as a sanity check: team attack and defense indicators + league + home
team_design <- function(st) {
  teams <- sort(unique(c(st$att_team, st$def_team)))
  n <- nrow(st)
  att <- Matrix::sparseMatrix(i = seq_len(n), j = match(st$att_team, teams), x = 1, dims = c(n, length(teams)))
  def <- Matrix::sparseMatrix(i = seq_len(n), j = match(st$def_team, teams), x = 1, dims = c(n, length(teams)))
  fixed <- fixed_design(st)
  X <- cbind(fixed, att, def)
  colnames(X) <- c(colnames(fixed), paste0("att_", teams), paste0("def_", teams))
  list(X = X, player_cols = grep("^(att|def)_", colnames(X)))
}

# ---- Precision -------------------------------------------------------------------------

# Fisher information of a Poisson shot-volume model at the league-average rate:
# X' diag(mu) X, with mu = stint minutes x league shots per minute.
fisher_info <- function(X, st) {
  rate <- tapply(st$n_shots, st$league, sum) / tapply(st$minutes, st$league, sum)
  mu <- st$minutes * as.numeric(rate[st$league])
  as.matrix(Matrix::crossprod(X, Matrix::Diagonal(x = mu) %*% X))
}

# Rank, aliased columns, condition number, and standard errors (log scale) for
# the columns that are not aliased.
estimability <- function(info) {
  q <- qr(info)
  keep <- sort(q$pivot[seq_len(q$rank)])
  sub <- info[keep, keep, drop = FALSE]
  inv <- tryCatch(solve(sub), error = function(e) MASS::ginv(sub))
  se <- rep(NA_real_, ncol(info))
  se[keep] <- sqrt(pmax(diag(inv), 0))
  ev <- eigen(sub, symmetric = TRUE, only.values = TRUE)$values
  list(rank = q$rank, n_cols = ncol(info),
       aliased = setdiff(seq_len(ncol(info)), keep),
       condition = max(ev) / min(ev), se = se)
}

pct <- function(log_effect) 100 * (exp(log_effect) - 1)

# Spread of team attack effects from the stage 1 volume model (log scale)
team_attack_sd <- function(team_match) {
  nb <- fit_volume(team_match)$nb
  b <- stats::coef(nb)
  b <- b[grepl("^ATT", names(b))]
  C <- attr(team_match, "contrasts_team")
  if (is.null(C) || ncol(C) != length(b)) stop("Team contrasts missing from team_match; rerun targets::tar_make().")
  stats::sd(as.numeric(C %*% b))
}

# How precisely player and team effects could be estimated (no shrinkage), plus
# the spread of team attack effects as the benchmark. Used by precision_table()
# and by the player-SE figure.
precision_data <- function(st, pm, team_match, threshold = IDENT_DEFAULT_THRESHOLD) {
  pd <- player_design(st, pm, threshold)
  ep <- estimability(fisher_info(pd$X, st))
  td <- team_design(st)
  et <- estimability(fisher_info(td$X, st))
  aliased <- intersect(ep$aliased, pd$player_cols)
  list(n_off = pd$n_off, n_def = pd$n_def,
       aliased_cols = colnames(pd$X)[aliased],
       condition = ep$condition,
       se_player = ep$se[pd$player_cols],
       se_team = et$se[td$player_cols],
       team_sd = team_attack_sd(team_match))
}

precision_table <- function(pdat) {
  se_p <- pdat$se_player
  data.frame(
    Quantity = c("Player effects (offense / defense)",
                 "Aliased player columns (never apart from others)",
                 "Condition number, player design (aliased columns removed)",
                 "Player effect SE, median (% shots per 90)",
                 "Player effect SE, 90th percentile (% shots per 90)",
                 "Team effect SE, median (% shots per 90)",
                 "Spread of team attack effects, SD (% shots per 90)"),
    Value = c(sprintf("%d / %d", pdat$n_off, pdat$n_def),
              sprintf("%d (%s)", length(pdat$aliased_cols),
                      if (length(pdat$aliased_cols)) paste(pdat$aliased_cols, collapse = ", ") else "none"),
              formatC(pdat$condition, format = "e", digits = 1),
              fmt(pct(stats::median(se_p, na.rm = TRUE)), 1),
              fmt(pct(stats::quantile(se_p, 0.9, na.rm = TRUE)), 1),
              fmt(pct(stats::median(pdat$se_team, na.rm = TRUE)), 1),
              fmt(pct(pdat$team_sd), 1))
  )
}

# ---- Shrinkage and threshold sensitivity -----------------------------------------------

# Ridge with prior SD tau on player effects (penalty 1 / tau^2; other columns
# unpenalised). Effective df = how many player parameters the data supports.
# If the posterior SD is close to tau, the data adds little beyond the prior.
ridge_table <- function(st, pm, threshold = IDENT_DEFAULT_THRESHOLD, prior_sd = RIDGE_PRIOR_SD) {
  pd <- player_design(st, pm, threshold)
  info <- fisher_info(pd$X, st)
  pc <- pd$player_cols
  do.call(rbind, lapply(prior_sd, function(tau) {
    lam <- numeric(ncol(info))
    lam[pc] <- 1 / tau^2
    M <- info + diag(lam)
    Minv <- tryCatch(solve(M), error = function(e) MASS::ginv(M))
    edf <- sum(rowSums(Minv[pc, , drop = FALSE] * t(info[, pc, drop = FALSE])))
    post_sd <- sqrt(pmax(diag(Minv)[pc], 0))
    data.frame(`Prior SD (% shots per 90)` = fmt(pct(tau), 1),
               `Player parameters` = length(pc),
               `Effective df` = fmt(edf, 0),
               `Median posterior SD (%)` = fmt(pct(stats::median(post_sd)), 1),
               `Share with posterior SD > 90% of prior` = sprintf("%s%%", fmt(100 * mean(post_sd > 0.9 * tau), 1)),
               check.names = FALSE)
  }))
}

threshold_table <- function(st, pm, thresholds = IDENT_THRESHOLDS) {
  do.call(rbind, lapply(thresholds, function(t) {
    pd <- player_design(st, pm, t)
    ep <- estimability(fisher_info(pd$X, st))
    se_p <- ep$se[pd$player_cols]
    data.frame(`Threshold (minutes)` = t,
               `Offense / defense effects` = sprintf("%d / %d", pd$n_off, pd$n_def),
               `Aliased player columns` = length(intersect(ep$aliased, pd$player_cols)),
               `Condition number` = formatC(ep$condition, format = "e", digits = 1),
               `Median SE (%)` = fmt(pct(stats::median(se_p, na.rm = TRUE)), 1),
               `90th pct SE (%)` = fmt(pct(stats::quantile(se_p, 0.9, na.rm = TRUE)), 1),
               check.names = FALSE)
  }))
}

# ---- Figure ----------------------------------------------------------------------------

plot_overlap <- function(ov) {
  ggplot2::ggplot(ov, ggplot2::aes(x = overlap)) +
    ggplot2::geom_histogram(bins = 40, fill = "gray70", color = NA) +
    ggplot2::labs(title = "Teammate overlap",
                  x = "Share of the lower-minute player's minutes spent with the other",
                  y = "Teammate pairs") +
    plain_theme()
}
