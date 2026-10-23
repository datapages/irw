# dutch_identity_helpers.R
#
# Shared helpers for the Dutch Identity vignette's loglinear models and for the
# Γ rank-1 diagnostics. Sourced by dutch_identity_compute.R; also intended for
# any other script that fits the same three nested models to 2^J pattern counts.
#
# Nothing here fetches data or touches the filesystem.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# ==============================================================================
# Log-likelihoods
#
# Every log-likelihood in Table 1 is computed by these two helpers from a
# model's fitted log-mean. Do not mix logLik() for one model with a hand-rolled
# objective for another: that is what produced the impossible +41,572.6
# log-likelihood in the published Table 1. The optim objective omitted the
# Poisson normalising constant −Σ log(y!), which logLik.glm() includes.
# ==============================================================================

# Poisson log-likelihood for pattern counts, including the −Σ log(y!) constant,
# so it is on exactly the scale logLik() reports for a Poisson GLM.
pois_ll <- function(log_mu, counts) {
  sum(counts * log_mu - exp(log_mu) - lgamma(counts + 1))
}

# Multinomial log-likelihood kernel, Σ y log p̂ with p̂ = μ̂ / Σ μ̂. Always ≤ 0.
# It differs from pois_ll by a model-free constant whenever Σ μ̂ = N, which holds
# for every model here because each has a free intercept.
mult_ll <- function(log_mu, counts) {
  log_p <- log_mu - log(sum(exp(log_mu)))
  sum(counts * log_p)
}

# Multinomial log-likelihood of held-out counts under fitted log-means from a
# model fitted elsewhere. `floor_p` guards the case the fitted probability of a
# cell is numerically zero: a single held-out respondent in such a cell would
# otherwise dominate the fold. Returns the raw value and the floored one.
mult_ll_heldout <- function(log_mu_fitted, counts_heldout, floor_p = NULL) {
  log_p <- log_mu_fitted - log(sum(exp(log_mu_fitted)))
  raw   <- sum(counts_heldout * log_p)
  n_at_floor <- NA_integer_
  floored <- NA_real_
  if (!is.null(floor_p)) {
    lp <- pmax(log_p, log(floor_p))
    lp <- lp - log(sum(exp(lp)))
    floored    <- sum(counts_heldout * lp)
    n_at_floor <- sum(counts_heldout[log_p < log(floor_p)])
  }
  list(raw = raw, floored = floored, n_at_floor = n_at_floor,
       n_cells_below_floor = if (is.null(floor_p)) NA_integer_
                            else sum(log_p < log(floor_p)))
}

# ==============================================================================
# Restricted log-quadratic model (rank-1 Γ)
#
#   log μ(x) = α + x'β + (1/2)(x'γ)²
#
# Expanding the quadratic term,
#   (1/2)(x'γ)² = (1/2) Σ_j γ_j² x_j + Σ_{j<k} γ_j γ_k x_j x_k
# because x_j² = x_j for binary items. So the rank-1 prediction is about the
# OFF-diagonal entries of Γ only: Γ_jk = γ_j γ_k for j ≠ k. The diagonal is
# absorbed into the main effects and Γ_jj = 0 by construction.
# ==============================================================================

log_quad_log_mu <- function(params, X) {
  J <- ncol(X)
  as.numeric(
    params[1] +
      X %*% params[2:(J + 1)] +
      0.5 * (X %*% params[(J + 2):(2 * J + 1)])^2
  )
}

log_quad_nll <- function(params, X, counts) {
  nll <- -pois_ll(log_quad_log_mu(params, X), counts)
  if (!is.finite(nll)) 1e10 else nll
}

# Fit it by direct Poisson ML with multiple starts. γ enters only through
# (x'γ)², so its overall sign is unidentified and the surface has at least two
# equivalent optima; use several starts and keep the best, then report how many
# starts reached it.
fit_log_quad <- function(X, counts, alpha_start, beta_start, gamma_lead,
                         n_random = 15, maxit = 10000) {
  J <- ncol(X)
  starts <- list(
    c(alpha_start, beta_start,  gamma_lead),
    c(alpha_start, beta_start, -gamma_lead),
    c(alpha_start, beta_start, rep( 0.0, J)),
    c(alpha_start, beta_start, rep( 0.3, J)),
    c(alpha_start, beta_start, rep(-0.3, J))
  )
  for (i in seq_len(n_random)) {
    starts[[length(starts) + 1]] <-
      c(alpha_start, beta_start, gamma_lead + rnorm(J, 0, 0.5))
  }

  opts <- lapply(starts, function(st) tryCatch(
    optim(st, log_quad_nll, X = X, counts = counts,
          method = "BFGS", control = list(maxit = maxit, reltol = 1e-12)),
    error = function(e) NULL
  ))
  opts <- Filter(Negate(is.null), opts)
  if (!length(opts)) return(NULL)

  nll_vals <- vapply(opts, function(o) o$value, numeric(1))
  opt      <- opts[[which.min(nll_vals)]]

  list(
    opt         = opt,
    convergence = list(
      n_starts   = length(starts),
      n_fitted   = length(opts),
      n_at_best  = sum(nll_vals < min(nll_vals) + 1e-4),
      best_nll   = min(nll_vals),
      optim_code = opt$convergence,
      gamma      = opt$par[(J + 2):(2 * J + 1)]
    )
  )
}

# ==============================================================================
# Γ from a full SOE fit
# ==============================================================================

# Symmetric J×J matrix of pairwise interaction coefficients. The diagonal is
# zero: x_j² = x_j, so there is no x_j² term to estimate.
gamma_from_soe <- function(soe_model, item_cols) {
  J <- length(item_cols)
  G <- matrix(0, J, J, dimnames = list(item_cols, item_cols))
  co <- coef(soe_model)
  for (nm in names(co)[grepl(":", names(co))]) {
    parts <- strsplit(nm, ":")[[1]]
    if (length(parts) == 2 && all(parts %in% item_cols)) {
      G[parts[1], parts[2]] <- co[nm]
      G[parts[2], parts[1]] <- co[nm]
    }
  }
  G
}

# Matching matrix of standard errors for the off-diagonal entries (diagonal NA).
gamma_se_from_soe <- function(soe_model, item_cols) {
  J <- length(item_cols)
  S <- matrix(NA_real_, J, J, dimnames = list(item_cols, item_cols))
  V <- tryCatch(vcov(soe_model), error = function(e) NULL)
  if (is.null(V)) return(S)
  se <- sqrt(pmax(diag(V), 0))
  names(se) <- rownames(V)
  for (nm in names(se)[grepl(":", names(se))]) {
    parts <- strsplit(nm, ":")[[1]]
    if (length(parts) == 2 && all(parts %in% item_cols)) {
      S[parts[1], parts[2]] <- se[nm]
      S[parts[2], parts[1]] <- se[nm]
    }
  }
  S
}

# ==============================================================================
# Rank-1 fit to the OFF-diagonal entries of Γ
#
# Minimise Σ_{j<k} w_jk (Γ_jk − g_j g_k)², treating the diagonal as free.
# Coordinate (alternating least squares) update, holding g_{-j} fixed:
#   g_j = Σ_{k≠j} w_jk Γ_jk g_k / Σ_{k≠j} w_jk g_k²
# The explained share is measured against an uncentred total, Σ_{j<k} w_jk Γ_jk²,
# because the model has no intercept: Γ_jk = 0 is the null it improves on.
#
# g and −g give the same fit; the returned g is sign-normalised so that its
# largest-magnitude entry is positive.
# ==============================================================================

rank1_offdiag_fit <- function(G, W = NULL, start = NULL,
                              n_random = 10, tol = 1e-12, maxit = 2000) {
  J  <- nrow(G)
  if (is.null(W)) W <- matrix(1, J, J)
  diag(W) <- 0
  W[!is.finite(W)] <- 0
  od <- which(upper.tri(G))

  one_run <- function(g0) {
    g <- g0
    for (it in seq_len(maxit)) {
      g_old <- g
      for (j in seq_len(J)) {
        k   <- setdiff(seq_len(J), j)
        num <- sum(W[j, k] * G[j, k] * g[k])
        den <- sum(W[j, k] * g[k]^2)
        g[j] <- if (den > 0) num / den else 0
      }
      if (max(abs(g - g_old)) < tol) break
    }
    rss <- sum(W[od] * (G[od] - outer(g, g)[od])^2)
    list(g = g, rss = rss, iter = it,
         converged = max(abs(g - g_old)) < tol)
  }

  if (is.null(start)) {
    e     <- eigen(G, symmetric = TRUE)
    start <- sqrt(abs(e$values[1])) * e$vectors[, 1]
  }
  starts <- c(list(start, -start, rep(0.3, J)),
              replicate(n_random, start + rnorm(J, 0, 0.5), simplify = FALSE))

  runs <- lapply(starts, one_run)
  best <- runs[[which.min(vapply(runs, function(r) r$rss, numeric(1)))]]

  tss <- sum(W[od] * G[od]^2)
  g   <- best$g
  if (length(g) && g[which.max(abs(g))] < 0) g <- -g

  resid <- G - outer(g, g)
  diag(resid) <- 0

  list(
    g            = g,
    share        = if (tss > 0) 1 - best$rss / tss else NA_real_,
    rss          = best$rss,
    tss          = tss,
    resid        = resid,
    n_at_best    = sum(vapply(runs, function(r) r$rss, numeric(1)) <
                       best$rss + 1e-10 * max(1, best$rss)),
    n_starts     = length(starts),
    iter         = best$iter,
    converged    = best$converged
  )
}

# The eigenvalue facts the diagnostics rest on, in one place.
#
# Γ has a zero diagonal, so trace(Γ) = 0 and its eigenvalues sum to zero: large
# negative eigenvalues are forced by that constraint and carry no information
# about dimensionality. Moreover, for ANY γ the exactly rank-1 structure
# γγ' − diag(γ²) has at most one positive eigenvalue (Weyl: λ₂(A + B) ≤
# λ₂(A) + λ₁(B), with A = γγ' of rank 1 and B = −diag(γ²) ≼ 0). Hence
#   λ₁ / Σ|λ| ≤ 1/2,  with equality iff exactly one eigenvalue is positive,
# and "the leading eigenvalue explains 50% of the absolute eigenvalue sum" is
# the CEILING of that statistic, attained by the rank-1 ideal — not a shortfall.
gamma_eigen_facts <- function(G) {
  ev <- eigen(G, symmetric = TRUE)$values
  list(
    eigenvalues    = ev,
    trace          = sum(diag(G)),
    eig_sum        = sum(ev),
    n_positive     = sum(ev > 0),
    leading_abs_share = abs(ev[1]) / sum(abs(ev))
  )
}

# Eigenvalues of the exactly rank-1 reference structure: γγ' with the diagonal
# zeroed, i.e. what Γ would be if the Dutch Identity held exactly with this γ.
rank1_reference_eigen <- function(g) {
  G0 <- outer(g, g)
  diag(G0) <- 0
  eigen(G0, symmetric = TRUE)$values
}

# ==============================================================================
# The Γ rank-1 diagnostic, with a parametric-bootstrap reference distribution
#
# Observed statistics:
#   - weighted and unweighted share of the off-diagonal sum of squares that the
#     best rank-1 structure explains (weights w_jk = 1/SE(Γ_jk)² from the SOE
#     GLM's vcov)
#   - number of positive eigenvalues of Γ (the rank-1 prediction is exactly 1)
#   - λ₁ and λ₁/Σ|λ| (the latter is capped at 1/2; see gamma_eigen_facts)
#
# Reference distribution: simulate pattern counts from the FITTED restricted
# log-quadratic model at the same N (multinomial over the 2^J cells), refit the
# SOE, and recompute. Under that generating process the Dutch Identity holds
# exactly, so the spread of these statistics is what a test of this length and
# this N can achieve at best.
# ==============================================================================

gamma_rank1_diagnostics <- function(pats, item_cols, soe_model, log_mu_rq,
                                    gamma_rq, N, n_boot = 200, seed = 20260101) {
  G  <- gamma_from_soe(soe_model, item_cols)
  SE <- gamma_se_from_soe(soe_model, item_cols)
  W  <- 1 / SE^2
  W[!is.finite(W)] <- 0
  diag(W) <- 0

  facts <- gamma_eigen_facts(G)

  # trace(Γ) = 0 by construction: no x_j² term exists for binary items.
  stopifnot(abs(facts$trace) < 1e-12)
  stopifnot(abs(facts$eig_sum) < 1e-8)
  # the identity the prose rests on
  stopifnot(facts$leading_abs_share <= 0.5 + 1e-9)

  fit_w <- rank1_offdiag_fit(G, W = W)
  fit_u <- rank1_offdiag_fit(G, W = NULL)
  stopifnot(fit_w$converged, fit_u$converged)

  # γ from the restricted log-quadratic, for the Γ_jk vs γ_jγ_k scatter
  g_rq <- gamma_rq
  if (length(g_rq) && g_rq[which.max(abs(g_rq))] < 0) g_rq <- -g_rq
  od   <- which(upper.tri(G))
  tss  <- sum(G[od]^2)
  share_rq <- 1 - sum((G[od] - outer(g_rq, g_rq)[od])^2) / tss

  scatter <- data.frame(
    item1 = rownames(G)[row(G)[od]],
    item2 = colnames(G)[col(G)[od]],
    gamma_soe  = G[od],
    gamma_pred = outer(g_rq, g_rq)[od],
    se         = SE[od]
  )
  scatter$resid <- scatter$gamma_soe - scatter$gamma_pred
  scatter$z     <- scatter$resid / scatter$se
  worst <- scatter[which.max(abs(scatter$z)), ]

  # ---- parametric bootstrap under the fitted rank-1 model -------------------
  p_rq <- exp(log_mu_rq - max(log_mu_rq))
  p_rq <- p_rq / sum(p_rq)
  stopifnot(abs(sum(p_rq) - 1) < 1e-12, all(p_rq >= 0))

  soe_f <- formula(soe_model)
  boot_pats <- pats

  set.seed(seed)
  rep_one <- function() {
    boot_pats$count <- as.integer(rmultinom(1, size = N, prob = p_rq))
    warned <- FALSE
    m <- withCallingHandlers(
      tryCatch(glm(soe_f, data = boot_pats, family = poisson()),
               error = function(e) NULL),
      warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") }
    )
    if (is.null(m) || !m$converged) {
      return(list(ok = FALSE, warned = warned))
    }
    Gb  <- gamma_from_soe(m, item_cols)
    SEb <- gamma_se_from_soe(m, item_cols)
    Wb  <- 1 / SEb^2
    Wb[!is.finite(Wb)] <- 0
    fb_w <- rank1_offdiag_fit(Gb, W = Wb, n_random = 4)
    fb_u <- rank1_offdiag_fit(Gb, W = NULL, n_random = 4)
    fa   <- gamma_eigen_facts(Gb)
    list(ok = TRUE, warned = warned,
         share_w = fb_w$share, share_u = fb_u$share,
         n_positive = fa$n_positive, lambda1 = fa$eigenvalues[1],
         leading_abs_share = fa$leading_abs_share,
         max_abs_coef = max(abs(coef(m)), na.rm = TRUE))
  }

  reps  <- replicate(n_boot, rep_one(), simplify = FALSE)
  ok    <- vapply(reps, function(r) isTRUE(r$ok), logical(1))
  n_fail <- sum(!ok)
  good  <- reps[ok]
  pull  <- function(nm) vapply(good, function(r) r[[nm]], numeric(1))

  boot <- data.frame(
    share_w           = pull("share_w"),
    share_u           = pull("share_u"),
    n_positive        = pull("n_positive"),
    lambda1           = pull("lambda1"),
    leading_abs_share = pull("leading_abs_share"),
    max_abs_coef      = pull("max_abs_coef"),
    warned            = vapply(good, function(r) isTRUE(r$warned), logical(1))
  )

  # the bootstrap must mostly converge, or the reference is not usable
  stopifnot(n_fail <= 0.05 * n_boot)
  # and it must satisfy the same identity
  stopifnot(all(boot$leading_abs_share <= 0.5 + 1e-9))

  pctl <- function(obs, x) mean(x < obs)

  list(
    gamma_matrix      = G,
    gamma_se          = SE,
    weights           = W,
    eigen_facts       = facts,
    rank1_weighted    = fit_w,
    rank1_unweighted  = fit_u,
    share_from_rq_gamma = share_rq,
    gamma_rq          = g_rq,
    reference_eigen   = rank1_reference_eigen(g_rq),
    scatter           = scatter,
    worst_resid       = worst,
    n_resid_z_gt_3    = sum(abs(scatter$z) > 3, na.rm = TRUE),
    n_offdiag         = nrow(scatter),
    boot              = boot,
    boot_summary      = list(
      share_w_median = median(boot$share_w),
      share_w_lo     = unname(quantile(boot$share_w, 0.025)),
      share_w_hi     = unname(quantile(boot$share_w, 0.975)),
      share_u_median = median(boot$share_u),
      share_u_lo     = unname(quantile(boot$share_u, 0.025)),
      share_u_hi     = unname(quantile(boot$share_u, 0.975))
    ),
    n_boot            = n_boot,
    n_boot_failed     = n_fail,
    n_boot_warned     = sum(boot$warned),
    boot_seed         = seed,
    pctl = list(
      share_w           = pctl(fit_w$share, boot$share_w),
      share_u           = pctl(fit_u$share, boot$share_u),
      lambda1           = pctl(facts$eigenvalues[1], boot$lambda1),
      leading_abs_share = pctl(facts$leading_abs_share, boot$leading_abs_share)
    ),
    boot_prop_one_positive = mean(boot$n_positive == 1)
  )
}
