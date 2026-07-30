# continuous_bounded_helpers.R
#
# Fit/simulate/score helpers for the bounded continuous-response vignette
# (continuous_bounded_compute.R / continuous_bounded.qmd). Four core specs:
#   1. naive linear baseline          -- lavaan (identity link, Gaussian)
#   2. Beta IRT (Noel & Dauvier 2007) -- sirt::brm.irf as a custom mirt itemtype
#   3. Samejima CRM (1973)            -- EstCRM::EstCRMitem / EstCRMperson
#   4. Mueller CRSM (1987)            -- pcIRT::CRSM / person_par (patched, see below)
#
# ==============================================================================
# Package-bug notes (found building this vignette; not fixed upstream as of
# the CRAN versions available when this was written)
# ==============================================================================
#
# - pcIRT::CRSM (Mueller's model) was DROPPED from the package entirely in
#   the last CRAN release (0.2.4, 2019-07-15); it only exists in pcIRT
#   <= 0.2.3. pcIRT itself was later archived by CRAN on 2025-11-11.
#   Installed here from the CRAN archive (0.2.3), NOT the final CRAN
#   version. The prompt that specified this vignette named the function
#   `pcIRT::crsm()` (lowercase) -- the real exported name is `CRSM()`
#   (uppercase); this appears to have been a misremembered detail rather
#   than a real blocker, since the uppercase function does exist in 0.2.3.
#
# - pcIRT 0.2.3's `CRSM()` has a real bug at its Newton-Raphson convergence
#   check: `while (is.na(para) || max(abs(para1 - para)) > conv)` -- `para`
#   becomes a length-2 vector after the first iteration, and a length>1
#   condition inside `||` is a hard error under R >= 4.3 (it used to just
#   warn and silently use the first element). This means CRSM() cannot
#   converge at all under a modern R install. Patched locally to
#   `any(is.na(para)) || ...` before rebuilding/installing; see
#   continuous_bounded_data/pcIRT_0.2.3_patched.tar.gz for the exact patched
#   source used. `person_par.CRSM()` has the superficially similar
#   `!exists("para") || ...` pattern but does NOT have this bug (exists()
#   is always scalar), so it was left untouched.
#
# - sirt::brm.sim() hardcodes `colnames(dat) <- paste0("I", 1L:9)`
#   regardless of the actual item count, so it only runs without error for
#   exactly 9 items. Rather than patch sirt, sim_beta_irt() below
#   reimplements its (otherwise correct) generative logic directly for an
#   arbitrary number of items. sirt::brm.irf() (used for *fitting*, not
#   simulating) has no such bug and is used as-is.
#
# ==============================================================================
# Cross-model likelihood-scale note (the comparability gate the prompt asked
# to resolve before comparing any metric across models)
# ==============================================================================
#
# EstCRMitem() (Samejima CRM) transforms the raw bounded response to
# Z = ln(X / (K-X)) and fits/report its EM log-likelihood entirely on that
# Z-scale (confirmed by reading the function body: `data[,i] =
# log(data[,i]/(max.item[i]-data[,i]))` happens *before* the loglikelihood()
# closure is ever defined, and that closure never re-introduces a Jacobian
# term). A raw log-likelihood from EstCRM is therefore NOT on the same scale
# as, e.g., the Beta-IRT model's density on the raw response.
#
# Rather than hand-deriving a per-model Jacobian correction (error-prone, and
# the prompt itself flagged this as the thing to get right), every model's
# held-out score here is instead a BINNED INTERVAL log-probability:
# log P(bin_lo < X_raw < bin_hi), using the same bin edges (on the raw,
# rescaled-to-[0,1] response scale) for all 4 models. Interval probability is
# invariant to a monotonic reparameterization of the intervening likelihood
# -- the Jacobian exactly cancels under integration -- so this sidesteps the
# Jacobian bookkeeping entirely instead of risking an error in a manual
# derivation. This is a deliberate substitution for "compare raw densities
# with a Jacobian correction" and is flagged here as such, since the prompt
# didn't spell out this exact resolution.
#
# ==============================================================================
# Comparison metric note
# ==============================================================================
#
# Uses held-out log-likelihood (binned, as above), NOT IMV: imv4sem/imv.mirt
# are not on CRAN and no citable generalization of IMV to continuous/beta-
# type outcomes was found. Held-out log-likelihood is a reasonable default
# but is an ad hoc choice for this setting (unlike IMV's binary/count case,
# there's no natural "coin flip" baseline for a continuous response) --
# flagged here as a decision to revisit, not a settled one.

suppressMessages({
  library(mirt)
  library(lavaan)
  library(EstCRM)
  library(pcIRT)
  library(sirt)
})

# ------------------------------------------------------------------------------
# Rescaling / binning utilities. All 4 models are fit and scored on a common
# [0,1] rescaling of the raw response (raw -> (raw-low)/(high-low)); "low"
# and "high" are the fixed, interpretable response-format bounds (e.g. 0/100
# for a VAS), not the sample min/max.
# ------------------------------------------------------------------------------

rescale01 <- function(x, low, high) (x - low) / (high - low)

# Smithson & Verkuilen (2006) squeeze: keeps values strictly inside (0,1).
# Needed for any model whose density/transform is undefined at the exact
# boundary (EstCRM's log(x/(1-x)); the continuous Beta density itself).
# n is the sample size used to set the squeeze amount (smaller squeeze for
# larger n).
squeeze01 <- function(u01, n) (u01 * (n - 1) + 0.5) / n

make_bins <- function(K) list(edges = seq(0, 1, length.out = K + 1), K = K)

# 1-indexed bin number (1..K) for each value in u01.
bin_of <- function(u01, bins) {
  idx <- findInterval(u01, bins$edges, rightmost.closed = TRUE, all.inside = TRUE)
  pmin(pmax(idx, 1), bins$K)
}

# ==============================================================================
# 1. Naive linear baseline (lavaan, identity link, Gaussian residual)
# ==============================================================================

fit_naive_linear <- function(Y01) {
  # lavaan's own mini-language doesn't support backtick-quoted names, so
  # callers must pass column names that are already valid bare identifiers
  # (compute.R renames real IRW item names like "17.1.R" to "item1" etc.
  # before calling any of the 4 fit_*() functions, for this reason).
  items <- colnames(Y01)
  model <- paste0("F1 =~ ", paste(items, collapse = " + "))
  lavaan::cfa(model, data = as.data.frame(Y01), std.lv = TRUE, missing = "fiml")
}

# ------------------------------------------------------------------------------
# Held-out design note: EstCRMitem() and pcIRT::CRSM() (the fitting functions
# for specs 3-4) have NO missing-data support at all in their fitting code --
# confirmed by reading both (CRSM's combn-pairwise loop does rowSums() with no
# na.rm, EstCRMitem's core EM matrix algebra likewise). So a cell-level
# "missing-response paradigm" holdout (this project's usual convention, see
# feedback_cv_holdout memory / guessing_compute.R) is not viable across all 4
# specs with the tools actually available. Substituted instead: a PERSON-level
# train/test split -- fit item parameters on a complete training-person
# matrix (zero missingness), then estimate theta for held-out test persons
# (from their own full response vector, which none of the 4 fitting calls
# ever saw) and score the model's implied probability for those same
# responses. This is a legitimate, standard "do item parameters generalize to
# new examinees" cross-validation design, but it is weaker/different evidence
# than true cell-level held-out-response prediction -- flagged here as a
# deviation driven by package constraints, not a silent substitution.
#
# Each fit_*() below trains on a complete matrix. Each theta_new_*() estimates
# person locations for NEW persons given the fixed, already-fitted item
# parameters. Each bin_logprob_*() computes log P(bin_lo < X < bin_hi | theta,
# item params) -- the common currency across all 4 models (see file header
# for why interval probability, not point density, is what's compared).
# ------------------------------------------------------------------------------

theta_new_linear <- function(fit, Y01_new) {
  as.vector(lavaan::lavPredict(fit, newdata = as.data.frame(Y01_new)))
}

bin_logprob_linear <- function(fit, item_name, theta, lo, hi) {
  pe <- lavaan::parameterEstimates(fit)
  loading   <- pe$est[pe$op == "=~" & pe$rhs == item_name]
  intercept <- pe$est[pe$op == "~1" & pe$lhs == item_name]
  resvar    <- pe$est[pe$op == "~~" & pe$lhs == item_name & pe$rhs == item_name]
  mu <- intercept + loading * theta
  sd_j <- sqrt(max(resvar, 1e-8))
  log(pmax(pnorm(hi, mu, sd_j) - pnorm(lo, mu, sd_j), 1e-12))
}

# ==============================================================================
# 2. Beta IRT (Noel & Dauvier, 2007), discretized via sirt::brm.irf as a
#    custom mirt itemtype (mirrors sirt's own documented mirt-estimation
#    recipe, see ?sirt::brm.sim).
# ==============================================================================

# Bug-fixed reimplementation of sirt::brm.sim(..., K=NULL)'s generative logic
# (see file header: upstream hardcodes a 9-item colnames() call).
sim_beta_irt <- function(theta, delta, tau) {
  N <- length(theta); J <- length(delta)
  if (length(tau) == 1) tau <- rep(tau, J)
  Y <- matrix(0, N, J)
  for (j in seq_len(J)) {
    m1 <- exp((theta - delta[j] + tau[j]) / 2)
    m2 <- exp((-theta + delta[j] + tau[j]) / 2)
    Y[, j] <- rbeta(N, shape1 = m1, shape2 = m2)
  }
  colnames(Y) <- paste0("item", seq_len(J))
  Y
}

# 0..K-1 integer categories for mirt, from [0,1] continuous responses.
discretize01 <- function(Y01, K) {
  bins <- make_bins(K)
  out <- apply(Y01, 2, function(col) bin_of(col, bins) - 1)
  out[is.na(Y01)] <- NA
  out
}

.make_brm_customItem <- function() {
  par <- c(delta = 0, tau = 0, thdim = 1)
  est <- c(TRUE, TRUE, FALSE)
  icc <- function(par, Theta, ncat) {
    sirt::brm.irf(Theta = Theta, delta = par[1], tau = par[2], ncat = ncat, thdim = par[3])
  }
  mirt::createItem("brm", par = par, est = est, P = icc)
}

fit_beta_irt <- function(Ycat, dispersion = c("fixed", "item")) {
  dispersion <- match.arg(dispersion)
  J <- ncol(Ycat)
  brm_item <- .make_brm_customItem()
  itemtype <- rep("brm", J)
  customItems <- list(brm = brm_item)

  if (dispersion == "fixed") {
    mp <- mirt::mirt(as.data.frame(Ycat), 1, itemtype = itemtype,
                      customItems = customItems, pars = "values")
    tau_rows <- mp$parnum[mp$name == "tau"]
    constrain <- list(tau_rows)
  } else {
    constrain <- NULL
  }

  mirt::mirt(as.data.frame(Ycat), 1, itemtype = itemtype,
             customItems = customItems, constrain = constrain,
             verbose = FALSE, technical = list(NCYCLES = 300))
}

theta_new_beta_irt <- function(fit, Ycat_new) {
  as.vector(mirt::fscores(fit, method = "EAP", response.pattern = as.data.frame(Ycat_new)))
}

bin_logprob_beta_irt <- function(fit, item_idx, K, theta, bin) {
  cf <- mirt::coef(fit, simplify = TRUE)$items
  delta <- cf[item_idx, "delta"]; tau <- cf[item_idx, "tau"]
  probs <- sirt::brm.irf(Theta = matrix(theta, ncol = 1), delta = delta, tau = tau, ncat = K)
  log(pmax(probs[1, bin], 1e-12))
}

# ==============================================================================
# 3. Samejima's Continuous Response Model (1973), via EstCRM
# ==============================================================================

fit_samejima_crm <- function(Y01, max_em = 200, converge = 0.01) {
  J <- ncol(Y01)
  EstCRM::EstCRMitem(as.data.frame(Y01), max.item = rep(1, J), min.item = rep(0, J),
                     max.EMCycle = max_em, converge = converge)
}

# EstCRMperson() is already NA-tolerant per item (skips columns with data[k,i]
# NA when forming a person's theta estimate -- confirmed by reading its
# source), so it can be called directly on new persons with missing items.
theta_new_samejima_crm <- function(fit, Y01_new) {
  J <- ncol(Y01_new)
  theta_est <- EstCRM::EstCRMperson(as.data.frame(Y01_new), fit$param, rep(0, J), rep(1, J))
  theta_est$thetas[, "Theta Est."]
}

# EstCRM's own (theta, a, b, alpha) parameterization mixes an EM shrinkage
# term into its precision; rather than reverse-engineer that exactly, we
# estimate each item's residual SD on the Z = ln(x/(1-x)) scale directly and
# empirically from the TRAINING fit (fitted Z_hat = alpha_j*(theta-b_j) vs.
# observed Z), a transparent, defensible predictive residual.
.crm_resid_sd <- function(fit, Y01_train, theta_train) {
  ipar <- fit$param
  J <- ncol(Y01_train)
  Ztr <- log(pmin(pmax(Y01_train, 1e-6), 1 - 1e-6) / (1 - pmin(pmax(Y01_train, 1e-6), 1 - 1e-6)))
  vapply(seq_len(J), function(j) {
    zhat <- ipar[j, "alpha"] * (theta_train - ipar[j, "b"])
    max(sd(Ztr[, j] - zhat, na.rm = TRUE), 1e-3)
  }, numeric(1))
}

bin_logprob_samejima_crm <- function(fit, item_idx, resid_sd, theta, lo, hi) {
  ipar <- fit$param
  z_of <- function(x) log(pmin(pmax(x, 1e-6), 1 - 1e-6) / (1 - pmin(pmax(x, 1e-6), 1 - 1e-6)))
  mu <- ipar[item_idx, "alpha"] * (theta - ipar[item_idx, "b"])
  log(pmax(pnorm(z_of(hi), mu, resid_sd[item_idx]) - pnorm(z_of(lo), mu, resid_sd[item_idx]), 1e-12))
}

# ==============================================================================
# 4. Mueller's Continuous Rating Scale Model (1987), via (patched) pcIRT::CRSM
# ==============================================================================

fit_muller_corsm <- function(Y01) {
  pcIRT::CRSM(as.data.frame(Y01), low = 0, high = 1)
}

# pcIRT::person_par.CRSM() only scores the SAME persons/data used in the
# original CRSM() fit (it reads object$data_p internally) and has no
# missing-data handling -- so it cannot be reused for new/held-out persons.
# Reimplements the same estimating equation (see person_par.CRSM source:
# S0n/S1n/S2n integrals of exp(t*(theta-beta_j)+t*(1-t)*lambda)) generalized
# to (a) accept an arbitrary new person's item vector and (b) skip NA items.
theta_new_muller_corsm <- function(fit, Y01_new) {
  itempar <- fit$itempar; lambda <- fit$disppar
  S0n <- function(t, th, b) exp(t * (th - b) + t * (1 - t) * lambda)
  S1n <- function(t, th, b) t * exp(t * (th - b) + t * (1 - t) * lambda)
  S2n <- function(t, th, b) t^2 * exp(t * (th - b) + t * (1 - t) * lambda)
  estimate_one <- function(y_row) {
    obs <- which(!is.na(y_row))
    if (length(obs) == 0) return(NA_real_)
    th <- 0; th1 <- Inf; iter <- 0
    while (is.na(th1) || (abs(th1 - th) > 1e-4 && iter < 100)) {
      if (iter > 0) th <- th1
      s0 <- vapply(obs, function(j) stats::integrate(S0n, 0, 1, th = th, b = itempar[j], stop.on.error = FALSE)$value, numeric(1))
      s1 <- vapply(obs, function(j) stats::integrate(S1n, 0, 1, th = th, b = itempar[j], stop.on.error = FALSE)$value, numeric(1))
      s2 <- vapply(obs, function(j) stats::integrate(S2n, 0, 1, th = th, b = itempar[j], stop.on.error = FALSE)$value, numeric(1))
      su1 <- sum(s1 / s0); su2 <- -sum(s2 / s0 - (s1 / s0)^2)
      th1 <- th - (sum(y_row[obs]) - su1) / su2
      iter <- iter + 1
    }
    th1
  }
  apply(Y01_new, 1, estimate_one)
}

bin_logprob_muller_corsm <- function(fit, item_idx, theta, lo, hi) {
  lambda <- fit$disppar; b <- fit$itempar[item_idx]
  S0n <- function(t) exp(t * (theta - b) + t * (1 - t) * lambda)
  num <- tryCatch(stats::integrate(S0n, lower = lo, upper = hi, stop.on.error = FALSE)$value, error = function(e) NA_real_)
  den <- tryCatch(stats::integrate(S0n, lower = 0, upper = 1, stop.on.error = FALSE)$value, error = function(e) NA_real_)
  if (is.na(num) || is.na(den) || den <= 0) return(NA_real_)
  log(pmax(num / den, 1e-12))
}
