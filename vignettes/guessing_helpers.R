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
  list(nodes = nodes, weights = w)
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

fit_1pl_ag <- function(Y, quad = build_quadrature(41), start_beta = NULL) {
  N <- nrow(Y); J <- ncol(Y)
  theta <- quad$nodes; w <- quad$weights; K <- length(theta)
  M <- !is.na(Y); Mnum <- matrix(as.numeric(M), N, J); Ym <- Y; Ym[!M] <- 0
  theta_row <- matrix(theta, J, K, byrow = TRUE)

  make_obj_grad <- function(with_alpha) {
    obj <- function(par) {
      beta <- par[1:J]; gamma <- par[(J + 1):(2 * J)]
      alpha <- if (with_alpha) par[2 * J + 1] else 0
      r <- t(outer(theta, beta,  function(th, b) plogis(th - b)))            # J x K
      s <- t(outer(theta, gamma, function(th, g) plogis(alpha * th + g)))    # J x K
      P <- pmin(pmax(r + (1 - r) * s, 1e-10), 1 - 1e-10)
      logP <- log(P); log1mP <- log(1 - P)
      loglik <- Ym %*% logP + (Mnum - Ym) %*% log1mP
      np <- .node_posterior(loglik, w)
      -sum(np$marg_ll)
    }
    grad <- function(par) {
      beta <- par[1:J]; gamma <- par[(J + 1):(2 * J)]
      alpha <- if (with_alpha) par[2 * J + 1] else 0
      r <- t(outer(theta, beta,  function(th, b) plogis(th - b)))
      s <- t(outer(theta, gamma, function(th, g) plogis(alpha * th + g)))
      P <- pmin(pmax(r + (1 - r) * s, 1e-10), 1 - 1e-10)
      logP <- log(P); log1mP <- log(1 - P)
      loglik <- Ym %*% logP + (Mnum - Ym) %*% log1mP
      np <- .node_posterior(loglik, w)
      post <- np$post
      A  <- t(Ym) %*% post
      Bc <- t(Mnum) %*% post
      D  <- A - P * Bc                       # J x K: sum_i post_ik*(y_ij-P_jk)*M_ij
      denomP <- P * (1 - P)
      coef_beta  <- -r * (1 - r) * (1 - s) / denomP
      coef_gamma <-  (1 - r) * s * (1 - s) / denomP
      grad_beta  <- rowSums(coef_beta  * D)
      grad_gamma <- rowSums(coef_gamma * D)
      g <- c(grad_beta, grad_gamma)
      if (with_alpha) {
        coef_alpha <- (1 - r) * s * (1 - s) * theta_row / denomP
        g <- c(g, sum(coef_alpha * D))
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

  fg <- make_obj_grad(with_alpha = FALSE)
  fit_g <- optim(c(start_beta, start_gamma), fg$obj, fg$grad,
                  method = "BFGS", control = list(maxit = 300, reltol = 1e-10))

  fag <- make_obj_grad(with_alpha = TRUE)
  fit_ag <- optim(c(fit_g$par, 0), fag$obj, fag$grad,
                   method = "BFGS", control = list(maxit = 300, reltol = 1e-10),
                   hessian = TRUE)

  alpha_hat <- fit_ag$par[2 * J + 1]
  se_alpha <- tryCatch({
    v <- solve(fit_ag$hessian)
    sqrt(v[2 * J + 1, 2 * J + 1])
  }, error = function(e) NA_real_)

  lr_stat <- 2 * (fit_g$value - fit_ag$value)   # value = -loglik, so G - AG in loglik terms
  lr_stat <- max(lr_stat, 0)
  lr_p <- stats::pchisq(lr_stat, df = 1, lower.tail = FALSE)

  list(
    beta = fit_ag$par[1:J], gamma = fit_ag$par[(J + 1):(2 * J)], alpha = alpha_hat,
    se_alpha = se_alpha, lr_stat = lr_stat, lr_p = lr_p,
    loglik_ag = -fit_ag$value, loglik_g = -fit_g$value,
    beta_g = fit_g$par[1:J], gamma_g = fit_g$par[(J + 1):(2 * J)],
    converged_ag = fit_ag$convergence == 0, converged_g = fit_g$convergence == 0,
    quad = quad
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
                         pi_starts = c(0.6, 0.75, 0.9, 0.97)) {
  N <- nrow(Y); J <- ncol(Y)
  theta <- quad$nodes; w <- quad$weights; K <- length(theta)
  M <- !is.na(Y); Mnum <- matrix(as.numeric(M), N, J); Ym <- Y; Ym[!M] <- 0

  logg <- log(g_fit); log1mg <- log(1 - g_fit)
  loglik0_i <- rowSums(Ym * logg + (Mnum - Ym) * log1mg)

  obj <- function(par) {
    b <- par[1:J]; eta <- par[J + 1]; pi_ <- plogis(eta)
    r <- t(outer(theta, b, function(th, bb) plogis(th - bb)))
    P <- pmin(pmax(r, 1e-10), 1 - 1e-10)
    logP <- log(P); log1mP <- log(1 - P)
    loglik1 <- Ym %*% logP + (Mnum - Ym) %*% log1mP
    np <- .node_posterior(loglik1, w)
    combined <- .logsumexp2(log(pi_) + np$marg_ll, log(1 - pi_) + loglik0_i)
    -sum(combined)
  }
  grad <- function(par) {
    b <- par[1:J]; eta <- par[J + 1]; pi_ <- plogis(eta)
    r <- t(outer(theta, b, function(th, bb) plogis(th - bb)))
    P <- pmin(pmax(r, 1e-10), 1 - 1e-10)
    logP <- log(P); log1mP <- log(1 - P)
    loglik1 <- Ym %*% logP + (Mnum - Ym) %*% log1mP
    np <- .node_posterior(loglik1, w)                # posterior over theta WITHIN class 1
    combined <- .logsumexp2(log(pi_) + np$marg_ll, log(1 - pi_) + loglik0_i)
    post_z1 <- exp(log(pi_) + np$marg_ll - combined)  # posterior P(engaged | training data)

    post1w <- np$post * post_z1                       # N x K, row-scaled
    A  <- t(Ym) %*% post1w
    Bc <- t(Mnum) %*% post1w
    # dLL/db_j = sum_k post1w_ik*(P_jk-y_ij)*M_ij = -(rowSums(A - P*Bc)); d(obj)/db_j = -dLL/db_j
    grad_b_obj <- rowSums(A - P * Bc)
    # dLL/deta = sum(post_z1) - N*pi_ ; d(obj)/deta = -dLL/deta
    grad_eta_obj <- -(sum(post_z1) - N * pi_)
    c(grad_b_obj, grad_eta_obj)
  }

  best <- NULL
  for (pi0 in pi_starts) {
    b0 <- tryCatch({
      mod <- mirt(as.data.frame(Y), 1, itemtype = "Rasch", verbose = FALSE,
                  technical = list(NCYCLES = 200))
      cf <- coef(mod, simplify = TRUE)$items
      -cf[, "d"]
    }, error = function(e) rep(0, J))
    par0 <- c(b0, qlogis(pi0))
    fit <- tryCatch(
      optim(par0, obj, grad, method = "BFGS",
            control = list(maxit = 300, reltol = 1e-10)),
      error = function(e) NULL
    )
    if (!is.null(fit) && (is.null(best) || fit$value < best$value)) best <- fit
  }

  b_hat <- best$par[1:J]; pi_hat <- plogis(best$par[J + 1])
  list(b = b_hat, pi = pi_hat, loglik = -best$value,
       converged = best$convergence == 0, g_fit = g_fit, quad = quad)
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

  rl <- .rasch_node_loglik(Y, b0, quad$nodes)
  marg_ll_rasch <- .node_posterior(rl$loglik, quad$weights)$marg_ll

  M <- !is.na(Y); Mnum <- matrix(as.numeric(M), nrow(Y), J)
  Ym <- Y; Ym[!M] <- 0
  logg <- log(g_fit); log1mg <- log(1 - g_fit)
  loglik_guess <- rowSums(Ym * logg + (Mnum - Ym) * log1mg)

  flagged <- loglik_guess > marg_ll_rasch
  n_flag <- sum(flagged)

  if (n_flag == 0 || n_flag == nrow(Y)) {
    return(list(b = b0, b_baseline = b0, flagged = flagged,
                n_flagged = n_flag, frac_flagged = mean(flagged),
                note = "no purification applied (0 or all flagged)"))
  }

  mod1 <- mirt(as.data.frame(Y[!flagged, , drop = FALSE]), 1, itemtype = "Rasch",
               verbose = FALSE, technical = list(NCYCLES = 200))
  b1 <- -coef(mod1, simplify = TRUE)$items[, "d"]

  list(b = b1, b_baseline = b0, flagged = flagged,
       n_flagged = n_flag, frac_flagged = mean(flagged), note = "purified")
}

predict_purified_rasch <- function(fit, Y_train, quad = build_quadrature(41)) {
  b <- fit$b
  rl <- .rasch_node_loglik(Y_train, b, quad$nodes)
  np <- .node_posterior(rl$loglik, quad$weights)
  np$post %*% t(rl$P)   # N x J; caller subsets to held-out mask
}
