# asymmetric_irt_helpers.R
#
# Custom mirt item definitions for the LPE, RH, and AO asymmetric IRT models
# from Shim & Bonifay (2026, BJMSP, doi:10.1111/bmsp.70069). The P/dP/d2P
# math in this file is copied verbatim (not re-derived) from the paper's own
# OSF supplement, "Estimation Infrastructure for Asymmetric IRT Models"
# (https://osf.io/qnyex/, file "Asymmetry & Complexity of IRT Models_MMLE.Rmd"),
# a copy of which is kept alongside this file at
# asymmetric_irt_data/shim_bonifay_reference.Rmd for provenance. Verified to
# run against the paper's own LSAT7 illustration in this project's R
# environment before being adapted here -- see [[project_asymmetric_irt_vignette]]
# memory / commit history for that validation step.
#
# Model equations (paper Eqs 1, 3, 5), with z = a*theta + d (mirt's native
# intercept form, d = -a*b):
#   LPE: P(theta) = [Psi(z)]^kappa,                        kappa > 0
#   RH:  P(theta) = Phi(z / sigma(theta)),
#        sigma(theta) = sqrt(2) * (1 + exp(-kappa*theta))^(-1/2)
#   AO:  P(theta) = 1 - [1 + kappa*exp(z)]^(-1/kappa),      kappa > 0
#
# Shape-parameter reparameterization (unconstrained scale used for
# estimation; kappa > 0 for AO/LPE is enforced via exp()):
#   AO:  par name "eta",   kappa = exp(eta)
#   LPE: par name "S",     kappa = exp(S)
#   RH:  par name "delta", kappa = delta directly (kappa can be negative)
# kappa = 1 (AO/LPE) or kappa = 0 (RH) is the symmetric baseline -- AO/LPE
# reduce exactly to the logistic 2PLM there; RH reduces to the 2-parameter
# *normal-ogive* model (it's probit-linked throughout, not logistic).
#
# Gradients/Hessians are supplied analytically (dP/d2P below) and wrapped
# through mirt's own internal symbolic-parameter-gradient machinery
# (mirt:::symbolicGrad_par / symbolicHessian_par) rather than hand-derived
# parameter gradients -- this is what the reference implementation does, and
# it avoids re-deriving four-parameter gradients by hand. Both internal
# functions were confirmed present in the installed mirt version before use.

library(mirt)
library(statmod)

# ------------------------------------------------------------------------------
# Fixed 50-point Gauss-Hermite (probability-scale) quadrature, N(0,1).
# The reference implementation locks estimation to this exact quadrature via
# mirt's customTheta/customPriorFun technical options rather than mirt's
# default adaptive quadrature -- kept identical here for fidelity to the
# paper's estimation setup.
# ------------------------------------------------------------------------------

.gh <- statmod::gauss.quad.prob(50, dist = "normal")
.ord <- order(.gh$nodes)
theta_ref <- .gh$nodes[.ord]
w_theta_ref <- .gh$weights[.ord]
Theta_mat_ref <- matrix(theta_ref, ncol = 1)

prior_GH50 <- local({
  theta_r <- as.numeric(theta_ref)
  w_r <- as.numeric(w_theta_ref)
  function(Theta, Etable) {
    Theta <- as.matrix(Theta)
    th <- as.numeric(Theta[, 1])
    if (length(th) != length(theta_r) || max(abs(th - theta_r)) > 1e-12) {
      stop("prior_GH50(): Theta mismatch. Use customTheta = Theta_mat_ref")
    }
    w_r
  }
})

# ------------------------------------------------------------------------------
# Numerical helpers
# ------------------------------------------------------------------------------

eps_prob_clip_lower <- 1e-32
eps_prob_clip_upper <- .Machine$double.eps

# local() closures capture eps_lo/eps_hi as fixed values at definition time,
# so these functions don't need to look up any global by name when called --
# required for them to survive export to future::multisession workers.
bound_prob_mirt <- local({
  eps_lo <- eps_prob_clip_lower
  eps_hi <- eps_prob_clip_upper
  function(P) {
    P[P <= eps_lo] <- eps_lo
    P[P >= 1 - eps_hi] <- 1 - eps_hi
    P
  }
})
clip_mask_mirt <- local({
  eps_lo <- eps_prob_clip_lower
  eps_hi <- eps_prob_clip_upper
  function(P) {
    (P <= eps_lo) | (P >= 1 - eps_hi)
  }
})

shape_par_name <- function(model_type) {
  switch(model_type, "AO" = "eta", "LPE" = "S", "RH" = "delta",
         stop("Unknown model_type: ", model_type))
}

set_norm_prior_in_values <- function(sv, par_names, mean = 0, sd = 1) {
  nmcol <- if ("name" %in% names(sv)) "name" else "parname"
  if (!("prior.type" %in% names(sv))) sv$prior.type <- NA_character_
  if (!("prior_1" %in% names(sv))) sv$prior_1 <- NA_real_
  if (!("prior_2" %in% names(sv))) sv$prior_2 <- NA_real_
  sv$prior.type <- as.character(sv$prior.type)
  idx <- sv[[nmcol]] %in% par_names
  if (any(idx)) {
    sv$prior.type[idx] <- "norm"
    sv$prior_1[idx] <- mean
    sv$prior_2[idx] <- sd
  }
  sv
}

convert_ad_logit_to_probit <- function(ad_df, D = 1.702) {
  out <- ad_df
  out$a1 <- out$a1 / D
  out$d  <- out$d  / D
  out
}

# NOTE: mod2values()$item is already the exact, unique column name passed to
# mirt() (guaranteed unique by R's data.frame column-name rules) -- no need
# to extract a numeric id from it. An earlier version of this file tried to
# recover a per-item integer by stripping non-digit characters from the item
# name, which silently collapsed distinct items whose names embedded the
# same digits (e.g. real IRW item ids are not always "I1".."I5"-style) into
# one row via pivot_wider(), producing list-columns and breaking every
# downstream numeric computation. Matching on the item name string directly
# avoids that class of bug entirely.

extract_ad <- function(mod) {
  tab <- mirt::mod2values(mod)
  nmcol <- if ("name" %in% names(tab)) "name" else "parname"
  tab <- tab[!is.na(tab$item) & tab$item != "GROUP", , drop = FALSE]
  out_long <- tab[, c("item", nmcol, "value"), drop = FALSE]
  names(out_long) <- c("item", "par", "value")
  out_long$item <- as.character(out_long$item)
  out_long$par <- as.character(out_long$par)
  out_long$value <- suppressWarnings(as.numeric(out_long$value))
  out_wide <- tidyr::pivot_wider(out_long, names_from = par, values_from = value)
  if (!("a1" %in% names(out_wide)) && "a" %in% names(out_wide)) out_wide$a1 <- out_wide$a
  if (!("d" %in% names(out_wide)) && "d1" %in% names(out_wide)) out_wide$d <- out_wide$d1
  out_wide[, c("item", "a1", "d"), drop = FALSE]
}

extract_param_table <- function(mod) {
  tab <- mirt::mod2values(mod)
  nmcol <- if ("name" %in% names(tab)) "name" else "parname"
  tab <- tab[!is.na(tab$item) & tab$item != "GROUP", , drop = FALSE]
  out_long <- tab[, c("item", nmcol, "value"), drop = FALSE]
  names(out_long) <- c("item", "par", "value")
  out_long$item <- as.character(out_long$item)
  out_long$par <- as.character(out_long$par)
  out_long$value <- suppressWarnings(as.numeric(out_long$value))
  out_wide <- tidyr::pivot_wider(out_long, names_from = par, values_from = value)
  if (!("a1" %in% names(out_wide)) && "a" %in% names(out_wide)) out_wide$a1 <- out_wide$a
  if (!("d" %in% names(out_wide)) && "d1" %in% names(out_wide)) out_wide$d <- out_wide$d1
  out_wide[order(out_wide$item), , drop = FALSE]
}

has_valid_mod <- function(fit_obj, require_converged = FALSE) {
  ok <- !is.null(fit_obj) && !isTRUE(fit_obj$error) && !is.null(fit_obj$mod)
  if (require_converged && ok) {
    ok <- tryCatch(isTRUE(fit_obj$mod@OptimInfo$converged), error = function(e) FALSE)
  }
  ok
}

# ------------------------------------------------------------------------------
# AO: P(theta) = 1 - [1 + kappa*exp(z)]^(-1/kappa), kappa = exp(eta)
# ------------------------------------------------------------------------------

AO_def <- local({
  log1pexp <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))

  P_AO <- function(par, Theta, ncat) {
    a <- par[1]; d <- par[2]; eta <- par[3]
    Theta <- as.matrix(Theta); th <- Theta[, 1]
    z <- a * th + d
    lambda <- exp(eta)
    if (lambda == 0) {
      logP0 <- -exp(z); P0 <- exp(logP0); P1 <- -expm1(logP0)
      return(bound_prob_mirt(cbind(P0, P1)))
    }
    if (is.infinite(lambda)) {
      return(bound_prob_mirt(cbind(rep(1, length(z)), rep(0, length(z)))))
    }
    logA <- log1pexp(eta + z)
    logP0 <- -logA / lambda
    P0 <- exp(logP0); P1 <- -expm1(logP0)
    bound_prob_mirt(cbind(P0, P1))
  }

  dP_AO <- function(par, Theta, ncat) {
    a <- par[1]; d <- par[2]; eta <- par[3]
    Theta <- as.matrix(Theta); th <- Theta[, 1]
    z <- a * th + d
    lambda <- exp(eta)
    n <- length(z); npar <- length(par)
    dp1 <- array(0, dim = c(n, ncat, npar))
    if (!(lambda == 0 || is.infinite(lambda))) {
      logA <- log1pexp(eta + z); g <- exp(z - logA); P0 <- exp(-logA / lambda)
      dP_dz <- P0 * g
      dP_da <- th * dP_dz; dP_dd <- dP_dz
      dP_deta <- P0 * (g - logA / lambda)
      dP_da[!is.finite(dP_da)] <- 0
      dP_dd[!is.finite(dP_dd)] <- 0
      dP_deta[!is.finite(dP_deta)] <- 0
      dp1[, 2, 1] <- dP_da; dp1[, 2, 2] <- dP_dd; dp1[, 2, 3] <- dP_deta
      dp1[, 1, ] <- -dp1[, 2, ]
    }
    Pbound <- P_AO(par, Theta, ncat); clip <- clip_mask_mirt(Pbound)
    for (cat in 1:ncat) if (any(clip[, cat])) dp1[clip[, cat], cat, ] <- 0
    as.vector(dp1)
  }

  d2P_AO <- function(par, Theta, ncat) {
    a <- par[1]; d <- par[2]; eta <- par[3]
    Theta <- as.matrix(Theta); th <- Theta[, 1]
    z <- a * th + d
    lambda <- exp(eta)
    n <- length(z); npar <- length(par)
    dp2 <- array(0, dim = c(n, ncat, npar, npar))
    if (!(lambda == 0 || is.infinite(lambda))) {
      logA <- log1pexp(eta + z); g <- exp(z - logA); P0 <- exp(-logA / lambda)
      h <- g - logA / lambda
      d2_dz2 <- P0 * (g - (1 + lambda) * g^2)
      d2_dz_deta <- -P0 * g * (h + lambda * g)
      dh_deta <- (logA / lambda) - g - lambda * g^2
      d2_deta2 <- P0 * (dh_deta - h^2)
      d2_dz2[!is.finite(d2_dz2)] <- 0
      d2_dz_deta[!is.finite(d2_dz_deta)] <- 0
      d2_deta2[!is.finite(d2_deta2)] <- 0
      dp2[, 2, 1, 1] <- th^2 * d2_dz2
      dp2[, 2, 2, 2] <- d2_dz2
      dp2[, 2, 1, 2] <- th * d2_dz2; dp2[, 2, 2, 1] <- dp2[, 2, 1, 2]
      dp2[, 2, 1, 3] <- th * d2_dz_deta; dp2[, 2, 3, 1] <- dp2[, 2, 1, 3]
      dp2[, 2, 2, 3] <- d2_dz_deta; dp2[, 2, 3, 2] <- dp2[, 2, 2, 3]
      dp2[, 2, 3, 3] <- d2_deta2
      dp2[, 1, , ] <- -dp2[, 2, , ]
    }
    Pbound <- P_AO(par, Theta, ncat); clip <- clip_mask_mirt(Pbound)
    for (cat in 1:ncat) if (any(clip[, cat])) dp2[clip[, cat], cat, , ] <- 0
    as.vector(dp2)
  }

  gr_AO <- function(x, Theta) {
    dp1 <- array(dP_AO(x@par, Theta, x@ncat), dim = c(nrow(Theta), x@ncat, length(x@par)))
    g <- mirt:::symbolicGrad_par(x, Theta, dp1 = dp1)
    g[x@est]
  }
  hss_AO <- function(x, Theta) {
    dp1 <- array(dP_AO(x@par, Theta, x@ncat), dim = c(nrow(Theta), x@ncat, length(x@par)))
    dp2 <- array(d2P_AO(x@par, Theta, x@ncat), dim = c(nrow(Theta), x@ncat, length(x@par), length(x@par)))
    H <- mirt:::symbolicHessian_par(x, Theta, dp1 = dp1, dp2 = dp2)
    H[x@est, x@est, drop = FALSE]
  }

  list(P = P_AO, dP = dP_AO, d2P = d2P_AO, gr = gr_AO, hss = hss_AO)
})

# ------------------------------------------------------------------------------
# LPE: P(theta) = [Psi(z)]^kappa, kappa = exp(S)
# ------------------------------------------------------------------------------

LPE_def <- local({
  P_LPE <- function(par, Theta, ncat) {
    a <- par[1]; d <- par[2]; S <- par[3]
    Theta <- as.matrix(Theta); th <- Theta[, 1]
    z <- a * th + d
    lambda <- exp(S)
    logPsi <- plogis(z, log.p = TRUE)
    if (lambda == 0) return(bound_prob_mirt(cbind(rep(0, length(z)), rep(1, length(z)))))
    if (is.infinite(lambda)) {
      P1 <- ifelse(logPsi == 0, 1, 0); P0 <- 1 - P1
      return(bound_prob_mirt(cbind(P0, P1)))
    }
    t <- lambda * logPsi
    P1 <- exp(t); P0 <- -expm1(t)
    bound_prob_mirt(cbind(P0, P1))
  }

  dP_LPE <- function(par, Theta, ncat) {
    a <- par[1]; d <- par[2]; S <- par[3]
    Theta <- as.matrix(Theta); th <- Theta[, 1]
    z <- a * th + d
    lambda <- exp(S)
    n <- length(z); npar <- length(par)
    dp1 <- array(0, dim = c(n, ncat, npar))
    if (!(lambda == 0 || is.infinite(lambda))) {
      Psi <- plogis(z); logPsi <- plogis(z, log.p = TRUE); Q <- 1 - Psi
      P1 <- P_LPE(par, Theta, ncat)[, 2]
      dP_dz <- lambda * P1 * Q
      dP_da <- th * dP_dz; dP_dd <- dP_dz
      dP_dS <- lambda * P1 * logPsi
      dP_da[!is.finite(dP_da)] <- 0
      dP_dd[!is.finite(dP_dd)] <- 0
      dP_dS[!is.finite(dP_dS)] <- 0
      dp1[, 2, 1] <- dP_da; dp1[, 2, 2] <- dP_dd; dp1[, 2, 3] <- dP_dS
      dp1[, 1, ] <- -dp1[, 2, ]
    }
    Pbound <- P_LPE(par, Theta, ncat); clip <- clip_mask_mirt(Pbound)
    for (cat in 1:ncat) if (any(clip[, cat])) dp1[clip[, cat], cat, ] <- 0
    as.vector(dp1)
  }

  d2P_LPE <- function(par, Theta, ncat) {
    a <- par[1]; d <- par[2]; S <- par[3]
    Theta <- as.matrix(Theta); th <- Theta[, 1]
    z <- a * th + d
    lambda <- exp(S)
    n <- length(z); npar <- length(par)
    dp2 <- array(0, dim = c(n, ncat, npar, npar))
    if (!(lambda == 0 || is.infinite(lambda))) {
      Psi <- plogis(z); logPsi <- plogis(z, log.p = TRUE); Q <- 1 - Psi
      P1 <- P_LPE(par, Theta, ncat)[, 2]
      d2_dz2 <- lambda * P1 * Q * (lambda * Q - Psi)
      d2_dz_dS <- lambda * P1 * Q * (lambda * logPsi + 1)
      d2_dS2 <- lambda * P1 * logPsi * (1 + lambda * logPsi)
      d2_dz2[!is.finite(d2_dz2)] <- 0
      d2_dz_dS[!is.finite(d2_dz_dS)] <- 0
      d2_dS2[!is.finite(d2_dS2)] <- 0
      dp2[, 2, 1, 1] <- th^2 * d2_dz2
      dp2[, 2, 2, 2] <- d2_dz2
      dp2[, 2, 1, 2] <- th * d2_dz2; dp2[, 2, 2, 1] <- dp2[, 2, 1, 2]
      dp2[, 2, 1, 3] <- th * d2_dz_dS; dp2[, 2, 3, 1] <- dp2[, 2, 1, 3]
      dp2[, 2, 2, 3] <- d2_dz_dS; dp2[, 2, 3, 2] <- dp2[, 2, 2, 3]
      dp2[, 2, 3, 3] <- d2_dS2
      dp2[, 1, , ] <- -dp2[, 2, , ]
    }
    Pbound <- P_LPE(par, Theta, ncat); clip <- clip_mask_mirt(Pbound)
    for (cat in 1:ncat) if (any(clip[, cat])) dp2[clip[, cat], cat, , ] <- 0
    as.vector(dp2)
  }

  gr_LPE <- function(x, Theta) {
    dp1 <- array(dP_LPE(x@par, Theta, x@ncat), dim = c(nrow(Theta), x@ncat, length(x@par)))
    g <- mirt:::symbolicGrad_par(x, Theta, dp1 = dp1)
    g[x@est]
  }
  hss_LPE <- function(x, Theta) {
    dp1 <- array(dP_LPE(x@par, Theta, x@ncat), dim = c(nrow(Theta), x@ncat, length(x@par)))
    dp2 <- array(d2P_LPE(x@par, Theta, x@ncat), dim = c(nrow(Theta), x@ncat, length(x@par), length(x@par)))
    H <- mirt:::symbolicHessian_par(x, Theta, dp1 = dp1, dp2 = dp2)
    H[x@est, x@est, drop = FALSE]
  }

  list(P = P_LPE, dP = dP_LPE, d2P = d2P_LPE, gr = gr_LPE, hss = hss_LPE)
})

# ------------------------------------------------------------------------------
# RH: P(theta) = Phi(z / sigma(theta)), sigma(theta) = sqrt(2)*(1+exp(-delta*theta))^(-1/2)
# ------------------------------------------------------------------------------

RH_def <- local({
  P_RH <- function(par, Theta, ncat) {
    a <- par[1]; d <- par[2]; delta <- par[3]
    Theta <- as.matrix(Theta); th <- Theta[, 1]
    z <- a * th + d
    s <- plogis(delta * th); sigma <- sqrt(2) * sqrt(s); u <- z / sigma
    u[(is.nan(u)) & (z == 0) & (sigma == 0)] <- 0
    P0 <- numeric(length(u)); P1 <- numeric(length(u)); pos <- (u >= 0)
    P0[pos] <- pnorm(u[pos], lower.tail = FALSE); P1[pos] <- 1 - P0[pos]
    P1[!pos] <- pnorm(u[!pos]); P0[!pos] <- 1 - P1[!pos]
    bound_prob_mirt(cbind(P0, P1))
  }

  dP_RH <- function(par, Theta, ncat) {
    a <- par[1]; d <- par[2]; delta <- par[3]
    Theta <- as.matrix(Theta); th <- Theta[, 1]
    z <- a * th + d
    s <- plogis(delta * th); sigma <- sqrt(2) * sqrt(s); u <- z / sigma
    u[(is.nan(u)) & (z == 0) & (sigma == 0)] <- 0
    phi <- dnorm(u)
    n <- length(u); npar <- length(par)
    dp1 <- array(0, dim = c(n, ncat, npar))
    u_a <- th / sigma; u_d <- 1 / sigma
    cc <- th * (1 - s) / 2; u_delta <- -u * cc
    dP_da <- phi * u_a; dP_dd <- phi * u_d; dP_ddelta <- phi * u_delta
    dP_da[!is.finite(dP_da)] <- 0
    dP_dd[!is.finite(dP_dd)] <- 0
    dP_ddelta[!is.finite(dP_ddelta)] <- 0
    dp1[, 2, 1] <- dP_da; dp1[, 2, 2] <- dP_dd; dp1[, 2, 3] <- dP_ddelta
    dp1[, 1, ] <- -dp1[, 2, ]
    Pbound <- P_RH(par, Theta, ncat); clip <- clip_mask_mirt(Pbound)
    for (cat in 1:ncat) if (any(clip[, cat])) dp1[clip[, cat], cat, ] <- 0
    as.vector(dp1)
  }

  d2P_RH <- function(par, Theta, ncat) {
    a <- par[1]; d <- par[2]; delta <- par[3]
    Theta <- as.matrix(Theta); th <- Theta[, 1]
    z <- a * th + d
    s <- plogis(delta * th); sigma <- sqrt(2) * sqrt(s); u <- z / sigma
    u[(is.nan(u)) & (z == 0) & (sigma == 0)] <- 0
    phi <- dnorm(u)
    n <- length(u); npar <- length(par)
    dp2 <- array(0, dim = c(n, ncat, npar, npar))
    u_a <- th / sigma; u_d <- 1 / sigma
    cc <- th * (1 - s) / 2; u_delta <- -u * cc
    P_aa <- -phi * u * (u_a^2); P_dd <- -phi * u * (u_d^2); P_ad <- -phi * u * (u_a * u_d)
    P_ad2 <- phi * u_a * cc * (u^2 - 1); P_dd2 <- phi * u_d * cc * (u^2 - 1)
    u_dd2 <- u * th^2 * (1 - s^2) / 4
    P_d2d2 <- phi * (u_dd2 - u * (u_delta^2))
    P_aa[!is.finite(P_aa)] <- 0; P_dd[!is.finite(P_dd)] <- 0; P_ad[!is.finite(P_ad)] <- 0
    P_ad2[!is.finite(P_ad2)] <- 0; P_dd2[!is.finite(P_dd2)] <- 0; P_d2d2[!is.finite(P_d2d2)] <- 0
    dp2[, 2, 1, 1] <- P_aa; dp2[, 2, 2, 2] <- P_dd
    dp2[, 2, 1, 2] <- P_ad; dp2[, 2, 2, 1] <- P_ad
    dp2[, 2, 1, 3] <- P_ad2; dp2[, 2, 3, 1] <- P_ad2
    dp2[, 2, 2, 3] <- P_dd2; dp2[, 2, 3, 2] <- P_dd2
    dp2[, 2, 3, 3] <- P_d2d2
    dp2[, 1, , ] <- -dp2[, 2, , ]
    Pbound <- P_RH(par, Theta, ncat); clip <- clip_mask_mirt(Pbound)
    for (cat in 1:ncat) if (any(clip[, cat])) dp2[clip[, cat], cat, , ] <- 0
    as.vector(dp2)
  }

  gr_RH <- function(x, Theta) {
    dp1 <- array(dP_RH(x@par, Theta, x@ncat), dim = c(nrow(Theta), x@ncat, length(x@par)))
    g <- mirt:::symbolicGrad_par(x, Theta, dp1 = dp1)
    g[x@est]
  }
  hss_RH <- function(x, Theta) {
    dp1 <- array(dP_RH(x@par, Theta, x@ncat), dim = c(nrow(Theta), x@ncat, length(x@par)))
    dp2 <- array(d2P_RH(x@par, Theta, x@ncat), dim = c(nrow(Theta), x@ncat, length(x@par), length(x@par)))
    H <- mirt:::symbolicHessian_par(x, Theta, dp1 = dp1, dp2 = dp2)
    H[x@est, x@est, drop = FALSE]
  }

  list(P = P_RH, dP = dP_RH, d2P = d2P_RH, gr = gr_RH, hss = hss_RH)
})

# The P/dP/d2P/gr/hss closures above were each created inside local({...}),
# so they resolve bound_prob_mirt/clip_mask_mirt by ordinary lexical scoping
# in this (single) R session -- but createItem() objects get sent to fresh
# worker processes under future::multisession, and future's automatic
# global-detection can miss helpers referenced only inside a nested closure
# environment, not the fitting function's own environment. Binding the
# helpers directly into each function's environment makes every custom item
# self-contained so it survives that export. (This step exists in the
# authors' own reference implementation for the same reason.)
inject_helpers_into_def <- function(def) {
  for (nm in c("P", "dP", "d2P", "gr", "hss")) {
    environment(def[[nm]])$bound_prob_mirt <- bound_prob_mirt
    environment(def[[nm]])$clip_mask_mirt  <- clip_mask_mirt
  }
  def
}
AO_def  <- inject_helpers_into_def(AO_def)
LPE_def <- inject_helpers_into_def(LPE_def)
RH_def  <- inject_helpers_into_def(RH_def)

MODEL_DEFS <- list(AO = AO_def, LPE = LPE_def, RH = RH_def)

make_custom_item <- function(model_type) {
  def <- MODEL_DEFS[[model_type]]
  par <- switch(model_type,
                AO  = c(a1 = 1, d = 0, eta = 0),
                LPE = c(a1 = 1, d = 0, S = 0),
                RH  = c(a1 = 1, d = 0, delta = 0))
  createItem(name = model_type, par = par, est = c(TRUE, TRUE, TRUE),
             P = def$P, gr = def$gr, hss = def$hss)
}

make_custom_bank <- function() {
  list(AO = make_custom_item("AO"), LPE = make_custom_item("LPE"), RH = make_custom_item("RH"))
}

# ------------------------------------------------------------------------------
# fit_custom(): fit a single asymmetric model to a data set, using a 2PLM
# warm-start for a/d (converted to probit scale for RH) and a N(0, prior_sd^2)
# MAP prior on the shape parameter, matching the reference implementation.
# ------------------------------------------------------------------------------

fit_custom <- function(dat_items, model_type, custom_item_obj, wts = NULL,
                        ad_start = NULL, shape_init = 0, prior_sd = 1,
                        em_cycles = 5000, optimizer_fit = "nlminb") {
  if (is.null(wts)) wts <- rep(1, nrow(dat_items))
  fit_technical <- list(NCYCLES = em_cycles, customTheta = Theta_mat_ref, customPriorFun = prior_GH50)

  tryCatch({
    sv_raw <- mirt(dat_items, model = 1, itemtype = model_type,
                    customItems = setNames(list(custom_item_obj), model_type),
                    pars = "values", method = "EM", survey.weights = wts,
                    technical = fit_technical, optimizer = optimizer_fit, verbose = FALSE)

    nmcol <- if ("name" %in% names(sv_raw)) "name" else "parname"
    item_str <- as.character(sv_raw$item)

    if (!is.null(ad_start) && nrow(ad_start) > 0) {
      for (i in ad_start$item) {
        a_i <- ad_start$a1[ad_start$item == i][1]
        d_i <- ad_start$d[ad_start$item == i][1]
        if (is.finite(a_i)) sv_raw$value[(item_str == i) & (sv_raw[[nmcol]] %in% c("a1", "a"))] <- a_i
        if (is.finite(d_i)) sv_raw$value[(item_str == i) & (sv_raw[[nmcol]] %in% c("d", "d1"))] <- d_i
      }
    }
    shape_name <- shape_par_name(model_type)
    idxs <- (item_str != "GROUP") & (sv_raw[[nmcol]] == shape_name)
    sv_raw$value[idxs] <- shape_init
    sv_raw$est[idxs] <- TRUE
    sv_raw <- set_norm_prior_in_values(sv_raw, par_names = shape_name, mean = 0, sd = prior_sd)

    mod <- mirt(dat_items, model = 1, itemtype = model_type,
                customItems = setNames(list(custom_item_obj), model_type),
                pars = sv_raw, method = "EM", survey.weights = wts,
                technical = fit_technical, optimizer = optimizer_fit, verbose = FALSE)

    list(mod = mod, error = FALSE, message = NA_character_)
  }, error = function(e) list(mod = NULL, error = TRUE, message = conditionMessage(e)))
}
