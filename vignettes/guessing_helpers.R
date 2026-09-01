# guessing_helpers.R
#
# Standalone estimators used by guessing_compute.R for models not available in
# mirt: the 1PL-AG (San Martin, Del Pino, & De Boeck, 2006), the two-class
# Mixture / "safety valve" model (Xiao, Ulitzsch, Zhang, Frank, & Domingue,
# 2026), and Method A person-level purification (Torres Irribarra,
# Echeverria, & Espinoza, 2026). Also provides compute_imv(), a direct port
# of imv::imv.binary() for use with model predictions that don't come from
# mirt objects.
#
# All three custom models are fit by direct quasi-Newton maximization of the
# marginal (quadrature-integrated) log-likelihood, using analytic gradients.
# This is mathematically equivalent to EM at convergence -- the gradient of
# a log-sum-exp mixture likelihood is exactly the EM-posterior-weighted
# complete-data gradient (the same identity underlying the EM algorithm's
# M-step) -- but avoids hand-rolling a nested E-step/M-step loop.

library(mirt)

# ------------------------------------------------------------------------------
# compute_imv(): direct port of imv::imv.binary(), for two probability
# vectors and observed outcomes. Validated to match imv.binary() exactly
# (see vignettes/guessing_compute.R validation block).
# ------------------------------------------------------------------------------

compute_imv <- function(p0, p1, y, sigma = 1e-4) {
  p0 <- pmin(pmax(p0, sigma), 1 - sigma)
  p1 <- pmin(pmax(p1, sigma), 1 - sigma)
  ll <- function(y, p) {
    z <- log(p) * y + log(1 - p) * (1 - y)
    exp(sum(z) / length(y))
  }
  loglik0 <- ll(y, p0)
  loglik1 <- ll(y, p1)
  getcoin <- function(a) {
    f <- function(p, a) abs(p * log(p) + (1 - p) * log(1 - p) - log(a))
    stats::nlminb(0.5, f, lower = 0.001, upper = 0.999, a = a)$par
  }
  c0 <- getcoin(loglik0)
  c1 <- getcoin(loglik1)
  (c1 - c0) / c0
}

# ------------------------------------------------------------------------------
# Quadrature and shared internals
# ------------------------------------------------------------------------------

# Equally-spaced Bock-Aitkin quadrature (not literal polynomial Gauss-Hermite
# nodes, but the standard discretization used in MML-EM IRT implementations;
# with n_nodes=41 over [-6,6] this is a fine approximation to the theta prior).
build_quadrature <- function(n_nodes = 41, range = 6) {
  nodes <- seq(-range, range, length.out = n_nodes)
  w <- dnorm(nodes)
  w <- w / sum(w)
  list(nodes = nodes, weights = w, z = nodes, sd = 1)
}

# Rescale a standard-normal z-grid to a N(0, sd^2) prior. The weights are the
# standard-normal weights attached to the z nodes and do not change; only the
# node locations move, so the grid always spans +/- `range` SDs of theta.
#
# Why this exists: mirt's itemtype = "Rasch" fixes all slopes at 1 and
# ESTIMATES the latent variance, while the custom estimators below originally
# pinned theta ~ N(0,1). That asymmetry gives the Rasch baseline a free
# parameter the custom models did not have, so the two are not nested and the
# Mixture cannot collapse onto Rasch as pi_hat -> 1 -- which is precisely the
# safety-valve property the comparison is meant to test. With no guessing
# present at all, that mismatch alone drives IMV(Rasch, Mixture) to about
# -0.002 at sd(theta) = 1.6 (see guessing_vignette_checks.R, block 2).
.scale_quad <- function(quad, sd) {
  z <- if (!is.null(quad$z)) quad$z else quad$nodes
  list(nodes = z * sd, weights = quad$weights, z = z, sd = sd)
}

# Latent SD from a fitted mirt model (slopes fixed at 1), for use as a
# starting value or to put a mirt-based fit on its own estimated prior.
# The latent SD is bounded during estimation. Without a bound the 1PLg can
# drive sd -> Inf when the assumed g is badly over-specified (observed at
# g_fit = 0.5 on gilbert_meta_1: sd = 1e15, reported by optim as converged),
# which yields a meaningless model with a perfectly finite-looking IMV. The
# range below is far wider than any plausible latent SD, so it binds only on
# degenerate fits; `sd_at_bound` in the return value flags when it does.
LS_BOUNDS <- c(lower = log(0.1), upper = log(10))

.at_bound <- function(ls) isTRUE(abs(ls - LS_BOUNDS[["lower"]]) < 1e-6 ||
                                 abs(ls - LS_BOUNDS[["upper"]]) < 1e-6)

.mirt_sd <- function(mod) {
  v <- tryCatch(coef(mod, simplify = TRUE)$cov[1, 1], error = function(e) 1)
  if (!is.finite(v) || v <= 0) 1 else sqrt(v)
}

# log-sum-exp over quadrature nodes, weighted, per row of `loglik` (N x K).
# Returns list(post = N x K posterior weights, marg_ll = length-N marginal
# log-likelihood contributions).
.node_posterior <- function(loglik, weights) {
  K <- length(weights)
  m <- apply(loglik, 1, max)
  ex <- exp(loglik - m) * matrix(weights, nrow(loglik), K, byrow = TRUE)
  denom <- rowSums(ex)
  denom <- pmax(denom, .Machine$double.xmin)
  list(post = ex / denom, marg_ll = log(denom) + m)
}

.logsumexp2 <- function(a, b) {
  m <- pmax(a, b)
  m + log(exp(a - m) + exp(b - m))
}

# Rasch per-node log-likelihood matrix (N x K) given item difficulties b and
# quadrature nodes theta. Y may contain NA for missing/held-out cells.
.rasch_node_loglik <- function(Y, b, theta) {
  J <- ncol(Y)
  P <- t(outer(theta, b, function(th, bb) plogis(th - bb)))  # J x K
  P <- pmin(pmax(P, 1e-10), 1 - 1e-10)
  logP <- log(P); log1mP <- log(1 - P)
  M <- !is.na(Y)
  Ym <- Y; Ym[!M] <- 0
  Mnum <- matrix(as.numeric(M), nrow(Y), J)
  loglik <- Ym %*% logP + (Mnum - Ym) %*% log1mP
  list(loglik = loglik, P = P, M = Mnum, Ym = Ym)
}

# ------------------------------------------------------------------------------
# fit_1pl_ag(): San Martin, Del Pino, & De Boeck (2006) 1PL-AG.
#
#   P(Y_ij=1|theta_i) = expit(theta_i-beta_j) +
#                        [1-expit(theta_i-beta_j)] * expit(alpha*theta_i+gamma_j)
#
# Fits 1PL-G (alpha fixed at 0, per-item gamma free) first for starting
# values, then 1PL-AG (alpha free); reports the LR test of alpha=0.
# Y: N x J 0/1 matrix, NA for missing/held-out cells.
# ------------------------------------------------------------------------------

# alpha_start exists so the identification check in
# guessing_alpha_identification.R can restart the AG stage away from zero
# without duplicating this estimator; it does not change the default fit.
fit_1pl_ag <- function(Y, quad = build_quadrature(41), start_beta = NULL,
                       free_var = TRUE, alpha_start = 0) {
  N <- nrow(Y); J <- ncol(Y)
  z <- if (!is.null(quad$z)) quad$z else quad$nodes
  w <- quad$weights; K <- length(z)
  M <- !is.na(Y); Mnum <- matrix(as.numeric(M), N, J); Ym <- Y; Ym[!M] <- 0
  z_row <- matrix(z, J, K, byrow = TRUE)

  # Parameter layout, with the log latent SD always last:
  #   1PL-G  : c(beta[1:J], gamma[1:J], [ls])
  #   1PL-AG : c(beta[1:J], gamma[1:J], alpha, [ls])
  # theta_k = z_k * sd, so freeing sd here matches the free latent variance
  # that mirt's itemtype = "Rasch" baseline estimates (see .scale_quad).
  make_obj_grad <- function(with_alpha) {
    np_par <- 2 * J + as.integer(with_alpha)
    parts <- function(par) {
      sd <- if (free_var) exp(par[np_par + 1]) else 1
      list(beta = par[1:J], gamma = par[(J + 1):(2 * J)],
           alpha = if (with_alpha) par[2 * J + 1] else 0,
           sd = sd, theta = z * sd)
    }
    pieces <- function(pp) {
      r <- t(outer(pp$theta, pp$beta,  function(th, b) plogis(th - b)))       # J x K
      s <- t(outer(pp$theta, pp$gamma, function(th, g) plogis(pp$alpha * th + g)))
      P <- pmin(pmax(r + (1 - r) * s, 1e-10), 1 - 1e-10)
      loglik <- Ym %*% log(P) + (Mnum - Ym) %*% log(1 - P)
      list(r = r, s = s, P = P, np = .node_posterior(loglik, w))
    }
    obj <- function(par) -sum(pieces(parts(par))$np$marg_ll)
    grad <- function(par) {
      pp <- parts(par); pc <- pieces(pp)
      r <- pc$r; s <- pc$s; P <- pc$P
      post <- pc$np$post
      A  <- t(Ym) %*% post
      Bc <- t(Mnum) %*% post
      D  <- A - P * Bc                       # J x K: sum_i post_ik*(y_ij-P_jk)*M_ij
      denomP <- P * (1 - P)
      coef_beta  <- -r * (1 - r) * (1 - s) / denomP
      coef_gamma <-  (1 - r) * s * (1 - s) / denomP
      g <- c(rowSums(coef_beta * D), rowSums(coef_gamma * D))
      theta_row <- matrix(pp$theta, J, K, byrow = TRUE)
      if (with_alpha) {
        g <- c(g, sum(((1 - r) * s * (1 - s) * theta_row / denomP) * D))
      }
      if (free_var) {
        # dP/dtheta = r(1-r)(1-s) + (1-r)s(1-s)*alpha, and dtheta_k/dsd = z_k
        dP_dtheta <- r * (1 - r) * (1 - s) + (1 - r) * s * (1 - s) * pp$alpha
        g <- c(g, pp$sd * sum((dP_dtheta / denomP) * D * z_row))
      }
      -g
    }
    list(obj = obj, grad = grad)
  }

  if (is.null(start_beta)) {
    p_j <- colMeans(Y, na.rm = TRUE)
    p_j <- pmin(pmax(p_j, 0.02), 0.98)
    start_beta <- -qlogis(p_j)
  }
  start_gamma <- rep(qlogis(0.2), J)
  start_ls <- if (free_var) 0 else numeric(0)

  bounded <- function(par, fns) {
    if (!free_var) {
      return(optim(par, fns$obj, fns$grad, method = "BFGS",
                   control = list(maxit = 300, reltol = 1e-10), hessian = TRUE))
    }
    n <- length(par)
    optim(par, fns$obj, fns$grad, method = "L-BFGS-B",
          lower = c(rep(-Inf, n - 1), LS_BOUNDS[["lower"]]),
          upper = c(rep( Inf, n - 1), LS_BOUNDS[["upper"]]),
          control = list(maxit = 300, factr = 1e2), hessian = TRUE)
  }

  fg <- make_obj_grad(with_alpha = FALSE)
  fit_g <- bounded(c(start_beta, start_gamma, start_ls), fg)

  # carry beta/gamma across, insert alpha = 0, keep ls last
  start_ag <- c(fit_g$par[1:(2 * J)], alpha_start,
                if (free_var) fit_g$par[2 * J + 1] else numeric(0))
  fag <- make_obj_grad(with_alpha = TRUE)
  fit_ag <- bounded(start_ag, fag)

  alpha_hat <- fit_ag$par[2 * J + 1]
  se_alpha <- tryCatch({
    v <- solve(fit_ag$hessian)[2 * J + 1, 2 * J + 1]
    if (is.finite(v) && v > 0) sqrt(v) else NA_real_
  }, error = function(e) NA_real_)

  sd_hat <- if (free_var) exp(fit_ag$par[2 * J + 2]) else 1

  lr_stat <- 2 * (fit_g$value - fit_ag$value)   # value = -loglik, so G - AG in loglik terms
  lr_stat <- max(lr_stat, 0)
  lr_p <- stats::pchisq(lr_stat, df = 1, lower.tail = FALSE)

  # Is alpha actually identified on this data?
  #
  # alpha enters the likelihood only through the guessing branch,
  # dP/dalpha = (1-r)*s*(1-s)*theta with s = expit(alpha*theta + gamma). If the
  # 1PL-G stage drives every gamma to -Inf -- i.e. the data prefer NO guessing
  # floor at all -- then s ~ 0, that derivative vanishes, and the log-likelihood
  # is exactly flat in alpha. The optimizer then returns whatever alpha it was
  # started at (0, by construction above), with LR ~ 0 and p ~ 1.
  #
  # That is not "we tested for ability-dependent guessing and found none"; it is
  # "the model found no guessing, so the alpha test has nothing to test." The two
  # are indistinguishable in the reported numbers, so flag the second case here
  # and let callers suppress alpha rather than print a spurious p = 1.000.
  # Observed on the five ENEM 2013/2014 tables, where gamma_G lands near -30
  # (s ~ 1e-14) and refits from alpha in {+-0.1, +-0.3} return the start to 1e-11
  # with the log-likelihood unchanged to 1e-13 relative.
  s_max <- max(stats::plogis(fit_g$par[(J + 1):(2 * J)]))
  alpha_identified <- s_max > 1e-6

  list(
    beta = fit_ag$par[1:J], gamma = fit_ag$par[(J + 1):(2 * J)], alpha = alpha_hat,
    se_alpha = se_alpha, lr_stat = lr_stat, lr_p = lr_p, sd = sd_hat,
    loglik_ag = -fit_ag$value, loglik_g = -fit_g$value,
    beta_g = fit_g$par[1:J], gamma_g = fit_g$par[(J + 1):(2 * J)],
    converged_ag = fit_ag$convergence == 0, converged_g = fit_g$convergence == 0,
    free_var = free_var, sd_at_bound = free_var && .at_bound(fit_ag$par[2 * J + 2]),
    alpha_identified = alpha_identified, max_guess_floor = s_max,
    quad = .scale_quad(quad, sd_hat)
  )
}

# Posterior-predictive P(y_ij=1) for every cell, using only Y_train's
# observed cells to form each person's ability posterior (matches the
# "fully integrated posterior predictions" evaluation used throughout).
# Returns a full N x J matrix; caller subsets to the held-out mask, e.g.
# pred_matrix[held_out_mask].
predict_1pl_ag <- function(fit, Y_train) {
  theta <- fit$quad$nodes; w <- fit$quad$weights
  J <- ncol(Y_train)
  beta <- fit$beta; gamma <- fit$gamma; alpha <- fit$alpha
  r <- t(outer(theta, beta,  function(th, b) plogis(th - b)))
  s <- t(outer(theta, gamma, function(th, g) plogis(alpha * th + g)))
  P <- pmin(pmax(r + (1 - r) * s, 1e-10), 1 - 1e-10)
  logP <- log(P); log1mP <- log(1 - P)
  M <- !is.na(Y_train); Mnum <- matrix(as.numeric(M), nrow(Y_train), J)
  Ym <- Y_train; Ym[!M] <- 0
  loglik <- Ym %*% logP + (Mnum - Ym) %*% log1mP
  post <- .node_posterior(loglik, w)$post          # N x K posterior given training items
  post %*% t(P)                                    # N x J predicted P(correct) per item
}

# ------------------------------------------------------------------------------
# fit_mixture(): two-class Rasch-vs-flat-guessing mixture ("safety valve").
#   Pr(y_ij=1|Z_i=1) = expit(theta_i-b_j),  Pr(y_ij=1|Z_i=0) = g_fit (fixed)
# Multi-start over pi0 in pi_starts; keeps the run with highest log-likelihood.
# ------------------------------------------------------------------------------

fit_mixture <- function(Y, g_fit, quad = build_quadrature(41),
                         pi_starts = c(0.6, 0.75, 0.9, 0.97),
                         free_var = TRUE) {
  N <- nrow(Y); J <- ncol(Y)
  z <- if (!is.null(quad$z)) quad$z else quad$nodes
  w <- quad$weights; K <- length(z)
  M <- !is.na(Y); Mnum <- matrix(as.numeric(M), N, J); Ym <- Y; Ym[!M] <- 0
  S <- rowSums(Ym)                       # observed score per person, for d/dsd
  zmat <- matrix(z, N, K, byrow = TRUE)

  logg <- log(g_fit); log1mg <- log(1 - g_fit)
  loglik0_i <- rowSums(Ym * logg + (Mnum - Ym) * log1mg)

  # par = c(b[1:J], eta, ls); pi = expit(eta), sd(theta) = exp(ls).
  # With free_var = FALSE, ls is absent and sd is pinned at 1 (the original
  # behaviour, kept so the fixed-prior version can still be reproduced).
  unpack <- function(par) {
    sd <- if (free_var) exp(par[J + 2]) else 1
    list(b = par[1:J], pi = plogis(par[J + 1]), sd = sd, theta = z * sd)
  }

  # Shared core: per-node Rasch probabilities, the within-class-1 posterior
  # over theta, and the posterior probability of engagement.
  core <- function(pp) {
    r <- t(outer(pp$theta, pp$b, function(th, bb) plogis(th - bb)))
    P <- pmin(pmax(r, 1e-10), 1 - 1e-10)
    loglik1 <- Ym %*% log(P) + (Mnum - Ym) %*% log(1 - P)
    np <- .node_posterior(loglik1, w)
    combined <- .logsumexp2(log(pp$pi) + np$marg_ll, log(1 - pp$pi) + loglik0_i)
    list(P = P, np = np, combined = combined,
         post_z1 = exp(log(pp$pi) + np$marg_ll - combined))
  }

  obj <- function(par) -sum(core(unpack(par))$combined)

  grad <- function(par) {
    pp <- unpack(par); cc <- core(pp)
    P <- cc$P; post_z1 <- cc$post_z1

    post1w <- cc$np$post * post_z1                    # N x K, row-scaled
    A  <- t(Ym) %*% post1w
    Bc <- t(Mnum) %*% post1w
    # dLL/db_j = -sum_k post1w_ik*(y_ij-P_jk)*M_ij; d(obj)/db_j flips the sign
    grad_b_obj <- rowSums(A - P * Bc)
    # dLL/deta = sum(post_z1) - N*pi
    grad_eta_obj <- -(sum(post_z1) - N * pp$pi)
    g <- c(grad_b_obj, grad_eta_obj)

    if (free_var) {
      # theta_k = z_k*sd, and d/dtheta of the Rasch node loglik is (y - P), so
      #   dLL/dsd = sum_ik post1w_ik * z_k * sum_j M_ij*(y_ij - P_jk).
      resid <- S - Mnum %*% P                          # N x K
      grad_sd <- sum(post1w * resid * zmat)
      g <- c(g, -pp$sd * grad_sd)                      # chain rule for ls
    }
    g
  }

  # Rasch starting values, fit once rather than once per pi start. mirt's
  # estimated latent SD is the natural start for ls.
  mod0 <- tryCatch(
    mirt(as.data.frame(Y), 1, itemtype = "Rasch", verbose = FALSE,
         technical = list(NCYCLES = 200)),
    error = function(e) NULL
  )
  b0 <- if (is.null(mod0)) rep(0, J) else -coef(mod0, simplify = TRUE)$items[, "d"]
  ls0 <- if (is.null(mod0)) 0 else log(.mirt_sd(mod0))

  best <- NULL
  for (pi0 in pi_starts) {
    par0 <- c(b0, qlogis(pi0))
    if (free_var) par0 <- c(par0, ls0)
    fit <- tryCatch(
      if (free_var) {
        optim(par0, obj, grad, method = "L-BFGS-B",
              lower = c(rep(-Inf, J + 1), LS_BOUNDS[["lower"]]),
              upper = c(rep( Inf, J + 1), LS_BOUNDS[["upper"]]),
              control = list(maxit = 300, factr = 1e2))
      } else {
        optim(par0, obj, grad, method = "BFGS",
              control = list(maxit = 300, reltol = 1e-10))
      },
      error = function(e) NULL
    )
    if (!is.null(fit) && (is.null(best) || fit$value < best$value)) best <- fit
  }

  pp <- unpack(best$par)
  list(b = pp$b, pi = pp$pi, sd = pp$sd, loglik = -best$value,
       converged = best$convergence == 0, g_fit = g_fit, free_var = free_var,
       sd_at_bound = free_var && .at_bound(best$par[J + 2]),
       quad = .scale_quad(quad, pp$sd))
}

predict_mixture <- function(fit, Y_train) {
  theta <- fit$quad$nodes; w <- fit$quad$weights
  J <- ncol(Y_train)
  b <- fit$b; g_fit <- fit$g_fit; pi_ <- fit$pi
  r <- t(outer(theta, b, function(th, bb) plogis(th - bb)))
  P <- pmin(pmax(r, 1e-10), 1 - 1e-10)
  logP <- log(P); log1mP <- log(1 - P)
  M <- !is.na(Y_train); Mnum <- matrix(as.numeric(M), nrow(Y_train), J)
  Ym <- Y_train; Ym[!M] <- 0
  loglik1 <- Ym %*% logP + (Mnum - Ym) %*% log1mP
  np <- .node_posterior(loglik1, w)
  logg <- log(g_fit); log1mg <- log(1 - g_fit)
  loglik0_i <- rowSums(Ym * logg + (Mnum - Ym) * log1mg)
  combined <- .logsumexp2(log(pi_) + np$marg_ll, log(1 - pi_) + loglik0_i)
  post_z1 <- exp(log(pi_) + np$marg_ll - combined)

  pred_engaged <- np$post %*% t(P)                 # N x J, E[Rasch prob | class 1, training data]
  post_z1 * pred_engaged + (1 - post_z1) * g_fit   # N x J; caller subsets to held-out mask
}

# ------------------------------------------------------------------------------
# purify_rasch(): Torres Irribarra, Echeverria, & Espinoza (2026) Method A.
# Flags examinees whose response-pattern likelihood favors a flat-guessing
# process over Rasch, removes them, and refits Rasch on the retained sample.
# Returns purified item difficulties for evaluation on the FULL sample.
# ------------------------------------------------------------------------------

purify_rasch <- function(Y, g_fit, quad = build_quadrature(41)) {
  J <- ncol(Y)
  mod0 <- mirt(as.data.frame(Y), 1, itemtype = "Rasch", verbose = FALSE,
               technical = list(NCYCLES = 200))
  b0 <- -coef(mod0, simplify = TRUE)$items[, "d"]

  # mirt estimated the latent variance when it calibrated b0, so the flagging
  # step has to score each person against that same prior: comparing a
  # marginal Rasch likelihood computed under N(0,1) against item difficulties
  # calibrated under N(0,sd0^2) puts the two on different scales. The purified
  # refit below gets its own sd for the same reason.
  quad0 <- .scale_quad(quad, .mirt_sd(mod0))

  rl <- .rasch_node_loglik(Y, b0, quad0$nodes)
  marg_ll_rasch <- .node_posterior(rl$loglik, quad0$weights)$marg_ll

  M <- !is.na(Y); Mnum <- matrix(as.numeric(M), nrow(Y), J)
  Ym <- Y; Ym[!M] <- 0
  logg <- log(g_fit); log1mg <- log(1 - g_fit)
  loglik_guess <- rowSums(Ym * logg + (Mnum - Ym) * log1mg)

  flagged <- loglik_guess > marg_ll_rasch
  n_flag <- sum(flagged)

  if (n_flag == 0 || n_flag == nrow(Y)) {
    return(list(b = b0, b_baseline = b0, flagged = flagged,
                n_flagged = n_flag, frac_flagged = mean(flagged),
                sd = quad0$sd, sd_baseline = quad0$sd, quad = quad0,
                note = "no purification applied (0 or all flagged)"))
  }

  mod1 <- mirt(as.data.frame(Y[!flagged, , drop = FALSE]), 1, itemtype = "Rasch",
               verbose = FALSE, technical = list(NCYCLES = 200))
  b1 <- -coef(mod1, simplify = TRUE)$items[, "d"]
  quad1 <- .scale_quad(quad, .mirt_sd(mod1))

  list(b = b1, b_baseline = b0, flagged = flagged,
       n_flagged = n_flag, frac_flagged = mean(flagged),
       sd = quad1$sd, sd_baseline = quad0$sd, quad = quad1, note = "purified")
}

predict_purified_rasch <- function(fit, Y_train, quad = build_quadrature(41)) {
  # use the prior the purified refit was actually calibrated under
  q <- if (!is.null(fit$quad)) fit$quad else quad
  rl <- .rasch_node_loglik(Y_train, fit$b, q$nodes)
  np <- .node_posterior(rl$loglik, q$weights)
  np$post %*% t(rl$P)   # N x J; caller subsets to held-out mask
}

# ------------------------------------------------------------------------------
# Holdout mask -- per-person cell holdout, always leaving >=1 response.
# Same convention as asymmetric_irt_compute.R. Shared by guessing_compute.R,
# guessing_sim_compute.R and guessing_gsweep_compute.R so all three evaluate
# on the same holdout definition.
# ------------------------------------------------------------------------------

mask_holdout <- function(resp, frac = 0.2) {
  resp_train <- resp
  mat <- as.matrix(resp)
  n_obs_per_person <- rowSums(!is.na(mat))
  mask_list <- vector("list", nrow(mat))
  for (i in seq_len(nrow(mat))) {
    k <- n_obs_per_person[i]
    if (k < 2) next
    obs_cols <- which(!is.na(mat[i, ]))
    n_mask_i <- min(floor(frac * k), k - 1)
    if (n_mask_i < 1) next
    mask_list[[i]] <- cbind(row = i, col = sample(obs_cols, n_mask_i))
  }
  mask_idx <- do.call(rbind, mask_list)
  true_vals <- mat[mask_idx]
  for (k in seq_len(nrow(mask_idx))) resp_train[mask_idx[k, 1], mask_idx[k, 2]] <- NA
  list(train = resp_train, mask_idx = mask_idx, true_vals = true_vals)
}

# Held-out predicted P(response=1) for a fitted mirt model, via EAP theta
# plug-in + extract.item()/probtrace() -- repo convention (asymmetric_irt).
heldout_preds_mirt <- function(fit, mask_idx) {
  theta_vec <- fscores(fit, method = "EAP")[, 1]
  preds <- numeric(nrow(mask_idx))
  for (j in unique(mask_idx[, 2])) {
    rows <- which(mask_idx[, 2] == j)
    persons <- mask_idx[rows, 1]
    it <- extract.item(fit, j)
    preds[rows] <- probtrace(it, matrix(theta_vec[persons], ncol = 1))[, "P.1"]
  }
  preds
}

# 1PLg via constrained 3PL: a1 fixed at 1, g fixed at 1/m, d free.
fit_1plg <- function(train_df, m, em_cycles = EM_CYCLES) {
  g_val <- 1 / m
  base <- mirt(train_df, 1, itemtype = "3PL", pars = "values", verbose = FALSE)
  base$value[base$name == "a1"] <- 1
  base$est[base$name == "a1"]   <- FALSE
  base$value[base$name == "g"]  <- g_val
  base$est[base$name == "g"]    <- FALSE
  mirt(train_df, 1, itemtype = "3PL", pars = base, verbose = FALSE,
       technical = list(NCYCLES = em_cycles))
}
