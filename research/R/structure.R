# Structured versions of the baseline player model (Part A).
# Requires player_models.R (model_matrix, cv_gain, sh_reliability),
# helpers.R (fmt), Matrix.
#
#   M4      position-group counts on the pitch (unpenalized)
#   MRoles  M4 + pairwise products of the counts: role interactions
#   MGraph  ridge plus a teammate-graph penalty on offensive player effects

GRAPH_KAPPA <- 1

# ---- Role interactions -----------------------------------------------------------------

# Pairwise products of the six position-count columns (attacking and defending
# CB / FB / FW, midfield the reference): within the attacking side (3), within
# the defending side (3), and attacking x defending (9). Unpenalized, like the
# counts. Each product asks whether one role's effect depends on how many of
# another role are on the pitch.
add_role_interactions <- function(md) {
  if (is.null(md$XPos)) stop("Role interactions need position counts: player_model_data(positions = ).")
  P <- md$XPos
  pairs <- utils::combn(colnames(P), 2, simplify = FALSE)
  cols <- lapply(pairs, function(pr) P[, pr[1]] * P[, pr[2]])
  XRole <- Matrix::Matrix(do.call(cbind, cols), sparse = TRUE)
  colnames(XRole) <- vapply(pairs, function(pr) paste(sub("^pos_", "", pr), collapse = "_x_"), character(1))
  md$XRole <- XRole
  md
}

# ---- Teammate graph --------------------------------------------------------------------

# Normalized Laplacian I - D^(-1/2) W D^(-1/2) over the offensive player
# columns of the design, W = minutes each pair of teammates shared on the
# pitch (from teammate_overlap()). Players with no edges get an all-zero row;
# the identity term in build_graph_penalty() still shrinks them.
build_laplacian <- function(md, overlap) {
  off <- grep("^off_[0-9]+$", colnames(md$XP), value = TRUE)
  ids <- as.integer(sub("^off_", "", off))
  i <- match(overlap$player_i, ids); j <- match(overlap$player_j, ids)
  keep <- !is.na(i) & !is.na(j) & overlap$shared > 0
  p <- length(ids)
  W <- Matrix::sparseMatrix(i = c(i[keep], j[keep]), j = c(j[keep], i[keep]),
                            x = rep(overlap$shared[keep], 2), dims = c(p, p))
  d <- Matrix::rowSums(W)
  s <- ifelse(d > 0, 1 / sqrt(d), 0)
  L <- Matrix::Diagonal(p, x = as.numeric(d > 0)) - Matrix::Diagonal(x = s) %*% W %*% Matrix::Diagonal(x = s)
  L <- Matrix::forceSymmetric(L)
  dimnames(L) <- list(off, off)
  L
}

# Penalty matrix over the M3 columns: 0 for unpenalized columns, identity for
# penalized player columns, plus kappa * L on the offensive block. Prior on
# offensive effects: N(0, tau^2 (I + kappa L)^-1), so every player is shrunk
# toward zero and teammates who share minutes toward each other. kappa = 0
# reproduces M3.
build_graph_penalty <- function(md, L, kappa = GRAPH_KAPPA) {
  mm <- model_matrix(md, "M3")
  cn <- colnames(mm$X)
  p <- length(cn)
  idx <- match(rownames(L), cn)
  if (anyNA(idx)) stop("Laplacian players missing from the design.")
  if (any(mm$pf[idx] == 0)) stop("Laplacian includes unpenalized columns.")
  tl <- methods::as(methods::as(L, "generalMatrix"), "TsparseMatrix")   # both triangles
  P <- Matrix::Diagonal(p, x = as.numeric(mm$pf > 0)) +
    Matrix::sparseMatrix(i = idx[tl@i + 1L], j = idx[tl@j + 1L], x = kappa * tl@x, dims = c(p, p))
  P <- Matrix::forceSymmetric(P)
  dimnames(P) <- list(cn, cn)
  P
}

# Poisson regression with penalty beta' P beta / (2 tau^2) and an unpenalized
# intercept, fit by penalized IRLS with warm starts along the tau grid. Same
# objective scale as fit_penalized(): a column with P = I has a N(0, tau^2)
# prior. Each iteration refactors a sparse Cholesky; cheap at this size.
fit_graph_ridge <- function(X, y, offset, P, taus, tol = 1e-9, maxit = 50) {
  taus <- sort(taus)                                  # largest penalty first
  n <- nrow(X)
  Xi <- cbind(Matrix::sparseMatrix(i = seq_len(n), j = rep(1L, n), x = 1, dims = c(n, 1)), X)
  P1 <- Matrix::forceSymmetric(Matrix::bdiag(Matrix::Matrix(0, 1, 1), P))
  pois <- stats::poisson()

  objective <- function(b, lam) {
    mu <- exp(pmin(as.numeric(Xi %*% b) + offset, 30))
    sum(pois$dev.resids(y, mu, 1)) / 2 + lam / 2 * sum(b * as.numeric(P1 %*% b))
  }

  beta <- c(log(sum(y) / sum(exp(offset))), rep(0, ncol(X)))
  B <- matrix(NA_real_, length(beta), length(taus),
              dimnames = list(c("(Intercept)", colnames(X)), NULL))

  for (k in seq_along(taus)) {
    lam <- 1 / taus[k]^2
    obj <- objective(beta, lam)
    done <- FALSE
    for (it in seq_len(maxit)) {
      eta <- pmin(as.numeric(Xi %*% beta) + offset, 30)
      mu <- exp(eta)
      rhs <- Matrix::crossprod(Xi, mu * (eta - offset) + (y - mu))     # X'Wz
      A <- Matrix::forceSymmetric(
        Matrix::crossprod(Xi, Matrix::Diagonal(x = mu) %*% Xi) + lam * P1)
      chol_fac <- Matrix::Cholesky(A, perm = TRUE, LDL = FALSE)
      beta_new <- as.numeric(Matrix::solve(chol_fac, rhs, system = "A"))
      obj_new <- objective(beta_new, lam)
      h <- 0
      while (obj_new > obj + 1e-10 && h < 10) {        # step halving if IRLS overshoots
        beta_new <- (beta + beta_new) / 2
        obj_new <- objective(beta_new, lam)
        h <- h + 1
      }
      done <- abs(obj - obj_new) < tol * (abs(obj_new) + 0.1)
      beta <- beta_new
      obj <- obj_new
      if (done) break
    }
    if (!done) warning("IRLS did not converge at tau = ", signif(taus[k], 3))
    B[, k] <- beta
  }
  list(beta = B, taus = taus)
}

predict_graph_ridge <- function(pfit, X, offset) {
  eta <- as.matrix(X %*% pfit$beta[-1, , drop = FALSE])
  exp(sweep(eta, 2, pfit$beta[1, ], "+") + offset)
}

# ---- Summary ---------------------------------------------------------------------------

# Each structured model against M3: held-out gain on the same folds and
# split-half reliability. fits: named list of list(cv = , model = , sh = ).
structure_table <- function(base_cv, base_sh, fits) {
  rel0 <- sh_reliability(base_sh)
  rows <- lapply(names(fits), function(nm) {
    f <- fits[[nm]]
    g <- cv_gain(base_cv, "M3", f$cv, f$model)
    r <- sh_reliability(f$sh)
    data.frame(Model = nm, `Gain over M3 (SE)` = sprintf("%s (%s)", fmt(g[["gain"]], 1), fmt(g[["se"]], 1)),
               `Reliability: offense` = fmt(r[["Offense"]], 3), `Reliability: defense` = fmt(r[["Defense"]], 3),
               check.names = FALSE)
  })
  rbind(data.frame(Model = "M3: plain ridge (reference)", `Gain over M3 (SE)` = "-",
                   `Reliability: offense` = fmt(rel0[["Offense"]], 3),
                   `Reliability: defense` = fmt(rel0[["Defense"]], 3), check.names = FALSE),
        do.call(rbind, rows))
}

# ---- Role coefficients -----------------------------------------------------------------

# Position-count and role-interaction coefficients from an MRoles fit on all
# rows or on a training subset; the pipeline refits once per left-out fold and
# role_coef_table() turns those into jackknife SEs
role_coefs <- function(md, fam, tau, rows = NULL, taus = PRIOR_SD_GRID) {
  if (is.null(rows)) rows <- rep(TRUE, length(md$y))
  mm <- model_matrix(md, "MRoles")
  b <- coef_at(fit_path(md, "MRoles", rows, fam, taus), tau)
  names(b) <- c("(Intercept)", colnames(mm$X))
  b[c(colnames(md$XPos), colnames(md$XRole))]
}

# Counts: % change per extra player of the role (midfield the reference).
# Products: % change per unit of the product, i.e. how much one role's effect
# shifts for each extra player of the other role.
role_coef_table <- function(full, jk) {
  jk <- do.call(rbind, jk)
  k <- nrow(jk)
  se <- sqrt((k - 1) / k * colSums(sweep(jk, 2, colMeans(jk))^2))
  data.frame(Term = sub("^pos_", "", names(full)),
             `Effect (% per 90)` = fmt(100 * (exp(full) - 1), 1),
             `SE (%)` = fmt(100 * se, 1),
             `Effect / SE` = fmt(full / se, 1),
             check.names = FALSE, row.names = NULL)
}

