# Model fitting. Requires aggregate.R (model_rhs) to be sourced first.

TWEEDIE_CONTROL <- list(tuning = 0.5, max_iter = 100, correct = TRUE)

# ---- Benchmark: Tweedie on the team-match total --------------------------------
# Kept as a benchmark because its index p is the test that motivates modelling
# volume and quality separately (see the p-compare figure and table).

fit_tweedie <- function(tm, control = TWEEDIE_CONTROL) {
  form <- stats::reformulate(model_rhs(tm), response = "npxg")
  Z0 <- mcglm::mc_id(tm)
  Z1 <- list(Matrix::tcrossprod(Matrix::sparse.model.matrix(~ 0 + match_f, data = tm)))
  mcglm::mcglm(
    linear_pred = c(form), matrix_pred = list(c(Z0, Z1)),
    link = c("log"), variance = c("tweedie"), power_fixed = c(FALSE),
    data = tm, control_algorithm = control
  )
}

tweedie_fitted <- function(fit) as.numeric(stats::fitted(fit))

tweedie_power <- function(fit) {
  tb <- stats::coef(fit, std.error = TRUE)
  i <- which(tb$Type == "power")[1]
  if (is.na(i)) stop("No power parameter found in mcglm coefficients.")
  se_col <- grep("std", names(tb), ignore.case = TRUE, value = TRUE)[1]
  c(p = tb$Estimates[i], se = if (is.na(se_col)) NA_real_ else tb[[se_col]][i])
}

# Volume and quality elasticities with respect to the fitted mean. Under a
# compound Poisson-gamma, they equal 2 - p and p - 1 and sum to 1.
fit_elasticities <- function(tm, mu) {
  keep <- tm$n_shots > 0 & mu > 0
  d <- data.frame(n_shots = tm$n_shots[keep],
                  xg_mean_shot = tm$xg_mean_shot[keep],
                  log_mu = log(mu[keep]))
  glm_n <- stats::glm(n_shots ~ log_mu, family = stats::quasipoisson(link = "log"), data = d)
  glm_q <- stats::glm(xg_mean_shot ~ log_mu, family = stats::Gamma(link = "log"),
                      weights = n_shots, data = d)
  cn <- summary(glm_n)$coefficients["log_mu", ]
  cq <- summary(glm_q)$coefficients["log_mu", ]
  list(e_vol = unname(cn["Estimate"]), se_vol = unname(cn["Std. Error"]),
       e_sev = unname(cq["Estimate"]), se_sev = unname(cq["Std. Error"]),
       sum = unname(cn["Estimate"] + cq["Estimate"]),
       glm_n = glm_n, glm_q = glm_q)
}

# Gamma shape of shot xG by moments and by MLE. If shots were gamma, the two
# would agree; the shape-implied Tweedie index is (alpha + 2) / (alpha + 1).
shape_stats <- function(x) {
  x <- x[is.finite(x) & x > 0]
  m <- mean(x); v <- stats::var(x)
  a_mom <- m^2 / v
  a_mle <- unname(MASS::fitdistr(x, "gamma",
                                 start = list(shape = a_mom, rate = a_mom / m),
                                 lower = 1e-6)$estimate["shape"])
  data.frame(n = length(x), mean = m, var = v,
             alpha_mom = a_mom, alpha_mle = a_mle)
}

p_from_shape <- function(alpha) (alpha + 2) / (alpha + 1)

# ---- Volume: Poisson vs negative binomial ---------------------------------------

fit_volume <- function(tm) {
  form <- stats::reformulate(model_rhs(tm), response = "n_shots")
  pois <- stats::glm(form, family = stats::poisson(link = "log"), data = tm)
  nb <- MASS::glm.nb(form, data = tm)
  list(
    pois = pois, nb = nb,
    quasi_dispersion = sum(stats::residuals(pois, type = "pearson")^2) / pois$df.residual,
    pois_dev_df = pois$deviance / pois$df.residual,
    nb_dev_df = nb$deviance / nb$df.residual,
    theta = nb$theta, theta_se = nb$SE.theta,
    aic_pois = stats::AIC(pois), aic_nb = stats::AIC(nb)
  )
}

# ---- Quality: Gamma on mean xG per shot, weighted by shot count ----------------

fit_quality <- function(tm) {
  d <- tm[tm$n_shots > 0, ]
  form <- stats::reformulate(model_rhs(tm), response = "xg_mean_shot")
  fit <- stats::glm(form, family = stats::Gamma(link = "log"),
                    weights = n_shots, data = d)
  list(fit = fit, dispersion = summary(fit)$dispersion)
}

# Predicted volume x predicted quality for every team-match, including zero-shot
# rows. The "contrasts dropped" warning from predict() is harmless (the model's
# stored contrasts are still applied) and is muffled; other warnings pass through.
recombine_nbg <- function(tm, volume, quality) {
  pv <- as.numeric(stats::fitted(volume$nb))
  pq <- withCallingHandlers(
    as.numeric(stats::predict(quality$fit, newdata = tm, type = "response")),
    warning = function(w) {
      if (grepl("contrasts dropped", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  if (length(pv) != nrow(tm) || length(pq) != nrow(tm)) stop("Prediction length does not match team-match rows.")
  pv * pq
}

# ---- Shot-quality mixture ---------------------------------------------------------
# Lognormal mixture = normal mixture on log(xG). Fit once on the pooled shots and
# held fixed: in smaller samples (e.g. half the data) competing 3-component
# solutions can tie in likelihood, so the mixture is never refit per team.

# Starting values near the structure found in the full data (long-range
# cluster, core, high-value tail on the log scale).
mixture_start <- function(y, k) {
  if (k == 2) {
    list(lambda = c(0.9, 0.1), mu = unname(stats::quantile(y, c(0.5, 0.95))), sigma = c(0.9, 0.4))
  } else if (k == 3) {
    list(lambda = c(0.05, 0.85, 0.10), mu = unname(stats::quantile(y, c(0.03, 0.5, 0.95))),
         sigma = c(0.2, 0.8, 0.5))
  } else {
    list(lambda = rep(1 / k, k), mu = unname(stats::quantile(y, seq(0.03, 0.97, length.out = k))),
         sigma = rep(stats::sd(y) / 2, k))
  }
}

# One EM run. Returns NULL instead of erroring if the run fails (for example,
# mixtools giving up after a component's variance collapses) or does not converge.
run_em <- function(y, k, start = NULL, maxit = 10000, epsilon = 1e-6) {
  f <- NULL
  utils::capture.output(f <- tryCatch(
    if (is.null(start)) {
      mixtools::normalmixEM(y, k = k, maxit = maxit, epsilon = epsilon)
    } else {
      mixtools::normalmixEM(y, k = k, lambda = start$lambda, mu = start$mu,
                            sigma = start$sigma, maxit = maxit, epsilon = epsilon)
    },
    error = function(e) NULL
  ))
  if (is.null(f) || length(f$all.loglik) - 1 >= maxit) return(NULL)
  f
}

# For each k: one informed start plus random starts; the best converged fit is
# kept. The log-likelihood is converted to the xG scale (Jacobian) so AIC/BIC are
# comparable with single-distribution fits on raw xG. fit_table reports how many
# starts converged and how many reached the best solution (a stability check).
fit_xg_mixture <- function(x, k_max = 3, seed = 1, n_starts = 10) {
  x <- x[is.finite(x) & x > 0]
  y <- log(x)
  jac <- -sum(y)
  fits <- list(); fit_rows <- list(); comp_rows <- list()
  for (k in seq_len(k_max)) {
    if (k == 1) {
      mu <- mean(y); s <- sqrt(mean((y - mu)^2))
      ll <- sum(stats::dnorm(y, mu, s, log = TRUE))
      comp <- data.frame(k = 1, component = 1, weight = 1, median_xg = exp(mu), sdlog = s)
      n_ok <- NA_integer_; n_best <- NA_integer_
    } else {
      starts <- c(list(mixture_start(y, k)), rep(list(NULL), n_starts - 1))
      f <- NULL; lls <- numeric(0)
      for (s in seq_along(starts)) {
        set.seed(seed + s)
        f_s <- run_em(y, k, starts[[s]])
        if (is.null(f_s)) next
        lls <- c(lls, f_s$loglik)
        if (is.null(f) || f_s$loglik > f$loglik) f <- f_s
      }
      if (is.null(f)) stop("No converged mixture fit for k = ", k)
      n_ok <- length(lls)
      n_best <- sum(abs(lls - max(lls)) < 1)
      ll <- f$loglik
      o <- order(f$mu)
      comp <- data.frame(k = k, component = seq_len(k), weight = f$lambda[o],
                         median_xg = exp(f$mu[o]), sdlog = f$sigma[o])
      fits[[as.character(k)]] <- f
    }
    npar <- 3 * k - 1
    ll_xg <- ll + jac
    fit_rows[[k]] <- data.frame(k = k, loglik_xg = ll_xg,
                                AIC = -2 * ll_xg + 2 * npar,
                                BIC = -2 * ll_xg + log(length(y)) * npar,
                                converged_starts = n_ok, starts_at_best = n_best)
    comp_rows[[k]] <- comp
  }
  list(fit_table = do.call(rbind, fit_rows),
       components = do.call(rbind, comp_rows),
       fits = fits)
}

best_mixture_k <- function(mix) mix$fit_table$k[which.min(mix$fit_table$BIC)]

best_mixture_components <- function(mix) {
  mix$components[mix$components$k == best_mixture_k(mix), ]
}

# Posterior probability of the highest-median component in the BIC-best fit:
# the definition of a "big chance". NULL if the best fit has one component.
big_chance_prob <- function(mix) {
  k <- best_mixture_k(mix)
  if (k == 1) return(NULL)
  f <- mix$fits[[as.character(k)]]
  f$posterior[, which.max(f$mu)]
}

# ---- Calibration -------------------------------------------------------------------

calibration_stats <- function(actual, pred) {
  slope <- unname(stats::coef(stats::lm(actual ~ pred))[2])
  dec <- dplyr::ntile(pred, 10)
  top <- actual[dec == 10]
  ci <- stats::t.test(top)$conf.int
  c(slope = slope,
    r = stats::cor(actual, pred),
    rmse = sqrt(mean((actual - pred)^2)),
    top_pred = mean(pred[dec == 10]),
    top_actual = mean(top),
    top_lo = ci[1], top_hi = ci[2])
}
