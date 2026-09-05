# continuous_bounded_info_compute.R
#
# Empirical item-information curves for the primary real table
# (lsbq_maleki_2025_non_persian_proficiency), added in response to reviewer
# feedback (Dylan Molenaar) that the vignette's information-curve comparison
# should show something computed from real fitted items, not just the
# interactive widget's illustrative sliders.
#
# Reuses the raw response matrix already cached in
# continuous_bounded_results.rds (res$real$Y_raw / low / high) -- no Redivis
# re-fetch needed. Refits all 4 core specs (5 counting both Beta IRT
# dispersion variants) on the FULL primary-table matrix (no train/test split
# -- this is about item-parameter-implied information, not held-out
# prediction), then computes each fitted item's Fisher information curve
# I(theta) = E[(d/dtheta log f(X|theta))^2] numerically: central finite
# difference on the score, Simpson's rule over X -- the same numerical
# recipe as the .qmd's OJS widget, just in R instead of JS, so the two are
# directly comparable.
#
# Output: vignettes/continuous_bounded_data/continuous_bounded_info_results.rds
#
# Usage:
#   Rscript vignettes/continuous_bounded_info_compute.R   # from project root

source("vignettes/continuous_bounded_helpers.R")

out_dir <- "vignettes/continuous_bounded_data"
out_rds <- file.path(out_dir, "continuous_bounded_info_results.rds")
results_rds <- file.path(out_dir, "continuous_bounded_results.rds")

if (!file.exists(out_rds)) {

  stopifnot(file.exists(results_rds))
  res <- readRDS(results_rds)

  Y_raw <- res$real$Y_raw
  low <- res$real$low; high <- res$real$high
  orig_items <- res$real$orig_items
  N <- nrow(Y_raw); J <- ncol(Y_raw)

  Y01 <- rescale01(Y_raw, low, high)
  colnames(Y01) <- paste0("item", seq_len(J))
  Y01_sq <- Y01; Y01_sq[] <- squeeze01(as.matrix(Y01), N)

  message("=== Fitting all 4 specs on full primary table (", res$real$table, ", N=", N, ", J=", J, ") ===")

  fit_lin <- fit_naive_linear(Y01)

  Ycat <- discretize01(Y01, K_BINS)
  fit_beta_fixed <- tryCatch(fit_beta_irt(Ycat, "fixed"), error = function(e) e)
  fit_beta_item  <- tryCatch(fit_beta_irt(Ycat, "item"),  error = function(e) e)

  fit_crm <- tryCatch(fit_samejima_crm(Y01_sq), error = function(e) e)
  crm_resid_sd <- NULL
  if (!inherits(fit_crm, "error")) {
    theta_tr_crm <- theta_new_samejima_crm(fit_crm, Y01_sq)
    crm_resid_sd <- .crm_resid_sd(fit_crm, Y01_sq, theta_tr_crm)
  }

  fit_mu <- tryCatch(fit_muller_corsm(Y01_sq), error = function(e) e)

  # ============================================================================
  # Density functions, vectorized in x for a fixed scalar theta -- mirrors the
  # *Density() functions in the .qmd's OJS block, so the R and JS numbers are
  # computed the same way.
  # ============================================================================

  naive_density_of <- function(intercept, loading, sd_j) {
    function(x, theta) dnorm(x, intercept + loading * theta, sd_j)
  }

  beta_density_of <- function(delta, tau) {
    function(x, theta) dbeta(x, exp((theta - delta + tau) / 2), exp((-theta + delta + tau) / 2))
  }

  samejima_density_of <- function(alpha, b, sigma) {
    function(x, theta) {
      z <- log(x / (1 - x))
      dnorm(z, alpha * (theta - b), sigma) / (x * (1 - x))
    }
  }

  mueller_density_of <- function(b, lambda) {
    function(x, theta) {
      g <- function(xx) exp(xx * (theta - b) + xx * (1 - xx) * lambda)
      norm <- stats::integrate(g, 0, 1, stop.on.error = FALSE)$value
      g(x) / norm
    }
  }

  # Fisher information via central-difference score + adaptive quadrature.
  # For naive linear (unbounded X), integrate directly over x on a window
  # that tracks the mean.
  fisher_info <- function(density_fn, theta, lo, hi, h = 1e-3) {
    integrand <- function(x) {
      fC <- density_fn(x, theta)
      fP <- density_fn(x, theta + h)
      fM <- density_fn(x, theta - h)
      score <- (log(pmax(fP, 1e-300)) - log(pmax(fM, 1e-300))) / (2 * h)
      ifelse(fC <= 1e-12, 0, score^2 * fC)
    }
    tryCatch(
      stats::integrate(integrand, lo, hi, stop.on.error = FALSE,
                        subdivisions = 500L, rel.tol = 1e-8)$value,
      error = function(e) NA_real_
    )
  }

  # For the 3 bounded-response models (X in (0,1)), integrating directly over
  # x on a fixed domain fails at steep model-implied slopes: a large alpha or
  # delta can concentrate almost all response density into an x-window far
  # narrower than adaptive quadrature reliably locates from a coarse global
  # scan, silently under-integrating and making the curve collapse toward 0
  # at extreme theta. Caught here by checking against Samejima CRM's own
  # closed form (Fisher info is analytically alpha^2/sigma^2, exactly
  # constant in theta, since Z is a Gaussian location family and Fisher
  # information is invariant under the 1-to-1 Z->X transform): this table's
  # fit for item 17.1.W (alpha=3.52) showed a false collapse-at-the-edges
  # pattern under direct x-integration where the true curve is flat.
  # Substituting z = logit(x) as the integration variable fixes this
  # generically -- a spike near x=0 or x=1 on the raw scale becomes a
  # normal-width bump on the z scale, which quadrature over z resolves
  # correctly regardless of how extreme the x-concentration is. Verified
  # against the alpha^2/sigma^2 closed form directly (matches to 10
  # significant figures after the switch, vs. collapsing to exactly 0 at
  # several theta values before it).
  fisher_info_bounded <- function(density_fn, theta, h = 1e-3, zlo = -30, zhi = 30) {
    integrand <- function(z) {
      x <- plogis(z)
      jac <- x * (1 - x) # dx/dz
      fC <- density_fn(x, theta) * jac
      fP <- density_fn(x, theta + h) * jac
      fM <- density_fn(x, theta - h) * jac
      score <- (log(pmax(fP, 1e-300)) - log(pmax(fM, 1e-300))) / (2 * h)
      ifelse(fC <= 1e-12, 0, score^2 * fC)
    }
    tryCatch(
      stats::integrate(integrand, zlo, zhi, stop.on.error = FALSE,
                        subdivisions = 500L, rel.tol = 1e-8)$value,
      error = function(e) NA_real_
    )
  }

  theta_grid <- seq(-3, 3, by = 0.25)
  rows <- list()

  # --- naive linear ------------------------------------------------------------
  pe <- lavaan::parameterEstimates(fit_lin)
  for (j in seq_len(J)) {
    item_name <- colnames(Y01)[j]
    loading   <- pe$est[pe$op == "=~" & pe$rhs == item_name]
    intercept <- pe$est[pe$op == "~1" & pe$lhs == item_name]
    resvar    <- pe$est[pe$op == "~~" & pe$lhs == item_name & pe$rhs == item_name]
    sd_j <- sqrt(max(resvar, 1e-8))
    dens <- naive_density_of(intercept, loading, sd_j)
    for (theta in theta_grid) {
      mu <- intercept + loading * theta
      lo <- mu - 8 * sd_j; hi <- mu + 8 * sd_j
      rows[[length(rows) + 1]] <- data.frame(
        model = "naive_linear", item = orig_items[j], theta = theta,
        info = fisher_info(dens, theta, lo, hi)
      )
    }
  }

  # --- Beta IRT, both dispersion variants --------------------------------------
  for (variant in c("fixed", "item")) {
    fit_b <- if (variant == "fixed") fit_beta_fixed else fit_beta_item
    key <- paste0("beta_irt_", variant, "_dispersion")
    if (!inherits(fit_b, "error")) {
      cf <- mirt::coef(fit_b, simplify = TRUE)$items
      for (j in seq_len(J)) {
        dens <- beta_density_of(cf[j, "delta"], cf[j, "tau"])
        for (theta in theta_grid) {
          rows[[length(rows) + 1]] <- data.frame(
            model = key, item = orig_items[j], theta = theta,
            info = fisher_info_bounded(dens, theta)
          )
        }
      }
    } else {
      message("  Beta IRT (", variant, " dispersion) failed to fit: ", conditionMessage(fit_b))
    }
  }

  # --- Samejima CRM --------------------------------------------------------------
  if (!inherits(fit_crm, "error")) {
    ipar <- fit_crm$param
    for (j in seq_len(J)) {
      dens <- samejima_density_of(ipar[j, "alpha"], ipar[j, "b"], crm_resid_sd[j])
      for (theta in theta_grid) {
        rows[[length(rows) + 1]] <- data.frame(
          model = "samejima_crm", item = orig_items[j], theta = theta,
          info = fisher_info_bounded(dens, theta)
        )
      }
    }
  } else {
    message("  Samejima CRM failed to fit: ", conditionMessage(fit_crm))
  }

  # --- Mueller CRSM ----------------------------------------------------------------
  if (!inherits(fit_mu, "error")) {
    for (j in seq_len(J)) {
      dens <- mueller_density_of(fit_mu$itempar[j], fit_mu$disppar)
      for (theta in theta_grid) {
        rows[[length(rows) + 1]] <- data.frame(
          model = "muller_corsm", item = orig_items[j], theta = theta,
          info = fisher_info_bounded(dens, theta)
        )
      }
    }
  } else {
    message("  Mueller CRSM failed to fit: ", conditionMessage(fit_mu))
  }

  info_curves <- do.call(rbind, rows)
  results <- list(table = res$real$table, N = N, J = J, items = orig_items,
                   info_curves = info_curves, date_run = Sys.Date())
  saveRDS(results, out_rds)
  message("Saved empirical information curves to ", out_rds)
} else {
  message(out_rds, " already exists -- skipping (delete it to force a rerun)")
}
