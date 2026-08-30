# What happens to the 1PLg and the Mixture as the assumed g moves away from
# the truth. The page fixes g at 1/m everywhere, so this is the axis it doesn't
# vary; it's the axis Studies 1 and 3 of the paper are built on.
#
#   source("guessing_g_sweep.R")
#   res <- g_sweep(Y_train, mask_idx, true_vals)
#   plot_g_sweep(res)
#
# Run the file directly and it does the above on simulated data. ~4 min.
# All three models sit on the same fixed N(0,1) quadrature on purpose; using
# mirt's Rasch as baseline would mix the g effect up with a variance effect.

suppressMessages(library(mirt))
if (!exists(".node_posterior")) source("guessing_helpers.R")

# 1PLg on a fixed quadrature: P = g + (1-g)*expit(theta-b), b free, g fixed.
# g = 0 is the Rasch model, so this doubles as the baseline.

.plg_nodes <- function(b, theta, g) {
  P <- t(outer(theta, b, function(th, bb) g + (1 - g) * plogis(th - bb)))  # J x K
  pmin(pmax(P, 1e-10), 1 - 1e-10)
}

# par = c(b[1:J], ls); sd(theta) = exp(ls), estimated like every other model
# on the page (see guessing_helpers.R::.scale_quad). The original version of
# this script pinned sd = 1 here so that the sweep could not be contaminated by
# a variance effect; now that the Mixture estimates its own sd, pinning the
# baseline would reintroduce exactly the mismatch it was guarding against, so
# both sides free it instead.
fit_1plg_fixedq <- function(Y, g_fit, quad = build_quadrature(41), start_b = NULL,
                            free_var = TRUE) {
  J <- ncol(Y)
  z <- if (!is.null(quad$z)) quad$z else quad$nodes
  w <- quad$weights; K <- length(z)
  M <- !is.na(Y); Mnum <- matrix(as.numeric(M), nrow(Y), J); Ym <- Y; Ym[!M] <- 0
  z_row <- matrix(z, J, K, byrow = TRUE)

  unpack <- function(par) {
    sd <- if (free_var) exp(par[J + 1]) else 1
    list(b = par[1:J], sd = sd, theta = z * sd)
  }

  obj <- function(par) {
    pp <- unpack(par)
    P <- .plg_nodes(pp$b, pp$theta, g_fit)
    ll <- Ym %*% log(P) + (Mnum - Ym) %*% log(1 - P)
    -sum(.node_posterior(ll, w)$marg_ll)
  }
  grad <- function(par) {
    pp <- unpack(par)
    P <- .plg_nodes(pp$b, pp$theta, g_fit)
    ll <- Ym %*% log(P) + (Mnum - Ym) %*% log(1 - P)
    post <- .node_posterior(ll, w)$post
    A <- t(Ym) %*% post; Bc <- t(Mnum) %*% post
    D <- A - P * Bc                                   # J x K
    r <- t(outer(pp$theta, pp$b, function(th, bb) plogis(th - bb)))
    # dP/db = -(1-g)*r*(1-r);  dP/dtheta = +(1-g)*r*(1-r), dtheta_k/dsd = z_k
    coef_b <- ((1 - g_fit) * r * (1 - r)) / (P * (1 - P))
    g <- rowSums(D * coef_b)
    if (free_var) g <- c(g, -pp$sd * sum(D * coef_b * z_row))
    g
  }

  if (is.null(start_b)) {
    p_j <- pmin(pmax(colMeans(Y, na.rm = TRUE), 0.02), 0.98)
    start_b <- -qlogis(p_j)
  }
  par0 <- if (free_var) c(start_b, 0) else start_b
  fit <- if (free_var) {
    optim(par0, obj, grad, method = "L-BFGS-B",
          lower = c(rep(-Inf, J), LS_BOUNDS[["lower"]]),
          upper = c(rep( Inf, J), LS_BOUNDS[["upper"]]),
          control = list(maxit = 300, factr = 1e2))
  } else {
    optim(par0, obj, grad, method = "BFGS",
          control = list(maxit = 300, reltol = 1e-10))
  }
  pp <- unpack(fit$par)
  list(b = pp$b, sd = pp$sd, g_fit = g_fit, loglik = -fit$value,
       converged = fit$convergence == 0,
       sd_at_bound = free_var && .at_bound(fit$par[J + 1]),
       quad = .scale_quad(quad, pp$sd))
}

predict_1plg_fixedq <- function(fit, Y_train) {
  theta <- fit$quad$nodes; w <- fit$quad$weights; J <- ncol(Y_train)
  P <- .plg_nodes(fit$b, theta, fit$g_fit)
  M <- !is.na(Y_train); Mnum <- matrix(as.numeric(M), nrow(Y_train), J)
  Ym <- Y_train; Ym[!M] <- 0
  ll <- Ym %*% log(P) + (Mnum - Ym) %*% log(1 - P)
  .node_posterior(ll, w)$post %*% t(P)
}

# Rasch (g = 0) is fit once and used as the common baseline, so every point on
# both curves is on one scale.

g_sweep <- function(Y_train, mask_idx, true_vals,
                    g_grid = c(0.10, 0.20, 0.25, 0.30, 0.40, 0.50),
                    quad = build_quadrature(41), verbose = TRUE) {
  Y_train <- as.matrix(Y_train)
  fit_r <- fit_1plg_fixedq(Y_train, 0, quad)
  p_rasch <- predict_1plg_fixedq(fit_r, Y_train)[mask_idx]

  out <- lapply(g_grid, function(g) {
    if (verbose) cat(sprintf("  g_fit = %.2f ...", g))
    plg <- fit_1plg_fixedq(Y_train, g, quad, start_b = fit_r$b)
    p_plg <- predict_1plg_fixedq(plg, Y_train)[mask_idx]
    mix <- fit_mixture(Y_train, g_fit = g, quad = quad)
    p_mix <- predict_mixture(mix, Y_train)[mask_idx]
    if (verbose) cat(sprintf(" pi-hat = %.3f\n", mix$pi))
    data.frame(g_fit = g, pi_hat = mix$pi, sd_plg = plg$sd, sd_mix = mix$sd,
               bound_1plg = isTRUE(plg$sd_at_bound), bound_mix = isTRUE(mix$sd_at_bound),
               imv_1plg = compute_imv(p_rasch, p_plg, true_vals),
               imv_mix  = compute_imv(p_rasch, p_mix, true_vals),
               conv_1plg = plg$converged, conv_mix = mix$converged)
  })
  res <- do.call(rbind, out)
  attr(res, "b_rasch") <- fit_r$b
  res
}

plot_g_sweep <- function(res, main = "Sensitivity to the assumed guessing level",
                         g_true = NA) {
  yl <- range(c(res$imv_1plg, res$imv_mix, 0), na.rm = TRUE)
  plot(res$g_fit, res$imv_1plg, type = "b", pch = 19, col = "#eb6834",
       ylim = yl, xlab = "assumed g_fit", ylab = "IMV vs Rasch", main = main)
  lines(res$g_fit, res$imv_mix, type = "b", pch = 17, col = "#4a3aa7")
  abline(h = 0, col = "grey60", lty = 3)
  if (!is.na(g_true)) abline(v = g_true, col = "grey40", lty = 2)
  legend("bottomleft", c("1PLg", "Mixture"), col = c("#eb6834", "#4a3aa7"),
         pch = c(19, 17), lty = 1, bty = "n")
}

# Demo: mixture DGM, 15% guessers at g_true = 0.25, so 0.25 is the correct
# specification and everything else on the grid is a mismatch.

if (sys.nframe() == 0L) {
  set.seed(20260722)
  N <- 1500; J <- 25; PI <- 0.85; G_TRUE <- 0.25
  b_true <- seq(-2, 2, length.out = J)
  theta <- rnorm(N); eng <- rbinom(N, 1, PI)
  P <- plogis(outer(theta, b_true, "-")); P[eng == 0, ] <- G_TRUE
  Y <- matrix(rbinom(N * J, 1, P), N, J); colnames(Y) <- paste0("i", 1:J)

  Ytr <- Y; mask <- NULL
  for (i in 1:N) {
    cols <- sample(J, floor(0.2 * J))
    mask <- rbind(mask, cbind(row = i, col = cols))
  }
  true_vals <- Y[mask]
  Ytr[mask] <- NA

  cat(sprintf("\nDGM: mixture, pi = %.2f, g_true = %.2f, N = %d, J = %d\n\n",
              PI, G_TRUE, N, J))
  res <- g_sweep(Ytr, mask, true_vals)
  cat("\n")
  print(res, row.names = FALSE, digits = 4)

  cat("\nRange of IMV across the g_fit grid:\n")
  cat(sprintf("  1PLg    : %+.4f to %+.4f  (spread %.4f)\n",
              min(res$imv_1plg), max(res$imv_1plg), diff(range(res$imv_1plg))))
  cat(sprintf("  Mixture : %+.4f to %+.4f  (spread %.4f)\n",
              min(res$imv_mix), max(res$imv_mix), diff(range(res$imv_mix))))

  png("g_sweep_demo.png", width = 1500, height = 1050, res = 200)
  plot_g_sweep(res, g_true = G_TRUE)
  dev.off()
  cat("\nWrote g_sweep_demo.png\n")
}
