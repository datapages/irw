# dutch_identity_compute.R
#
# Produces the precomputed results loaded by dutch_identity.qmd at render time.
# Run once locally before rendering the vignette.
#
# Output: dutchdata/dutch_identity_results.rds
#
# Usage:
#   Rscript vignettes/dutch_identity_compute.R   # from project root

library(irw)
library(mirt)
library(imv)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

set.seed(20240101)

out_dir  <- "vignettes/dutchdata"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# Helper: fetch, optionally downsample, reshape to wide, drop zero-variance items
# ==============================================================================

fetch_resp <- function(table_name, max_n = Inf) {
  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e)); NULL
  })
  if (is.null(df)) return(NULL)

  ids <- unique(df$id)
  if (length(ids) > max_n) {
    df <- df[df$id %in% sample(ids, max_n), ]
  }

  resp <- irw_long2resp(df)
  resp$id <- NULL
  resp <- resp[, sapply(resp, function(x) length(unique(na.omit(x))) > 1), drop = FALSE]
  resp
}

# Helper: build a unique string key for each response pattern row
pattern_key <- function(mat) apply(mat, 1, paste, collapse = "")

# ==============================================================================
# Sections 1–3: Manifest probabilities and loglinear models
#
# Needs: J ∈ [5, 10] items, N ≥ 3000 respondents, density ≥ 0.90, dichotomous.
# We enumerate all 2^J response patterns (zero-count patterns included) and fit
# three nested models to those same counts:
#   (1) independence loglinear model (main effects only)
#   (2) restricted log-quadratic model (rank-1 Γ, fit by direct Poisson ML)
#   (3) full second-order exponential (SOE) model (all pairwise interactions)
#
# Every log-likelihood is computed by the shared helpers below from a model's
# fitted log-mean. Do not mix logLik() for one model with a hand-rolled
# objective for another: that is what produced the impossible +41,572.6
# log-likelihood in the published Table 1. The optim objective omitted the
# Poisson normalising constant −Σ log(y!) — 41,771.9 for this dataset — which
# logLik.glm() includes, so the rank-1 row was inflated by that amount and
# appeared to beat the model it is nested inside.
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

# Fitted log-mean of the restricted log-quadratic model:
#   log μ(x) = α + x'β + (1/2)(x'γ)²
log_quad_log_mu <- function(params, X) {
  J <- ncol(X)
  as.numeric(
    params[1] +
      X %*% params[2:(J + 1)] +
      0.5 * (X %*% params[(J + 2):(2 * J + 1)])^2
  )
}

# Negative Poisson log-likelihood for that model, on the pois_ll scale
log_quad_nll <- function(params, X, counts) {
  nll <- -pois_ll(log_quad_log_mu(params, X), counts)
  if (!is.finite(nll)) 1e10 else nll
}

small_tables <- irw_filter(
  n_categories   = 2,
  n_items        = c(5, 10),
  n_participants = c(3000, Inf),
  density        = c(0.90, 1.0)
)
message("Small-J candidates: ", length(small_tables))

s1_3_result <- NULL

for (tbl in small_tables) {
  message("Trying: ", tbl)

  resp <- fetch_resp(tbl, max_n = 20000)
  if (is.null(resp)) next

  resp_c <- resp[complete.cases(resp), ]
  J <- ncol(resp_c)
  N <- nrow(resp_c)
  if (J < 5 || J > 10 || N < 2000 || 2^J > 1024) next
  message("  J=", J, "  N=", N, "  2^J=", 2^J)

  item_cols <- colnames(resp_c)

  # Enumerate all 2^J patterns and count occurrences. Zero-count patterns are
  # kept: all three models are fitted to this same full table of 2^J cells.
  all_pats <- expand.grid(replicate(J, 0:1, simplify = FALSE), KEEP.OUT.ATTRS = FALSE)
  colnames(all_pats) <- item_cols
  obs_tab  <- table(pattern_key(resp_c))
  pat_keys <- pattern_key(all_pats)
  all_pats$count <- as.integer(obs_tab[pat_keys])
  all_pats$count[is.na(all_pats$count)] <- 0L
  all_pats$log_p_obs <- ifelse(all_pats$count > 0, log(all_pats$count / N), NA_real_)
  all_pats$total     <- rowSums(all_pats[, item_cols])

  X <- as.matrix(all_pats[, item_cols])

  # Independence model: log E[count] ~ main effects
  ind_formula <- as.formula(paste("count ~", paste(item_cols, collapse = " + ")))
  ind_model   <- tryCatch(
    glm(ind_formula, data = all_pats, family = poisson()),
    error = function(e) { message("  ind glm failed"); NULL }
  )
  if (is.null(ind_model)) next

  # Full SOE model: add all pairwise interactions
  soe_formula <- as.formula(
    paste0("count ~ (", paste(item_cols, collapse = " + "), ")^2")
  )
  soe_model <- tryCatch(
    glm(soe_formula, data = all_pats, family = poisson()),
    error = function(e) { message("  SOE glm failed"); NULL }
  )
  if (is.null(soe_model)) next

  # Extract J×J Γ matrix from SOE coefficients
  soe_coefs    <- coef(soe_model)
  gamma_matrix <- matrix(0, J, J, dimnames = list(item_cols, item_cols))
  for (nm in names(soe_coefs)[grepl(":", names(soe_coefs))]) {
    parts <- strsplit(nm, ":")[[1]]
    if (length(parts) == 2 && all(parts %in% item_cols)) {
      gamma_matrix[parts[1], parts[2]] <- soe_coefs[nm]
      gamma_matrix[parts[2], parts[1]] <- soe_coefs[nm]
    }
  }

  eig     <- eigen(gamma_matrix, symmetric = TRUE)
  eig_val <- eig$values
  eig_vec <- eig$vectors

  # Restricted log-quadratic, by direct Poisson ML on the same 2^J counts.
  # γ enters only through (x'γ)², so its overall sign is unidentified and the
  # surface has at least two equivalent optima; use several starts and keep the
  # best, then report how many starts reached it.
  gamma_lead  <- sqrt(abs(eig_val[1])) * eig_vec[, 1]
  alpha_start <- coef(ind_model)[1]
  beta_start  <- coef(ind_model)[-1]

  starts <- list(
    c(alpha_start, beta_start,  gamma_lead),
    c(alpha_start, beta_start, -gamma_lead),
    c(alpha_start, beta_start, rep( 0.0, J)),
    c(alpha_start, beta_start, rep( 0.3, J)),
    c(alpha_start, beta_start, rep(-0.3, J))
  )
  for (i in seq_len(15)) {
    starts[[length(starts) + 1]] <-
      c(alpha_start, beta_start, gamma_lead + rnorm(J, 0, 0.5))
  }

  opts <- lapply(starts, function(st) tryCatch(
    optim(st, log_quad_nll, X = X, counts = all_pats$count,
          method = "BFGS", control = list(maxit = 10000, reltol = 1e-12)),
    error = function(e) NULL
  ))
  opts <- Filter(Negate(is.null), opts)
  if (!length(opts)) { message("  all log-quadratic starts failed"); next }

  nll_vals <- vapply(opts, function(o) o$value, numeric(1))
  opt      <- opts[[which.min(nll_vals)]]

  rq_convergence <- list(
    n_starts    = length(starts),
    n_fitted    = length(opts),
    n_at_best   = sum(nll_vals < min(nll_vals) + 1e-4),
    best_nll    = min(nll_vals),
    optim_code  = opt$convergence,
    gamma       = opt$par[(J + 2):(2 * J + 1)]
  )
  message("  log-quadratic: ", rq_convergence$n_at_best, "/",
          rq_convergence$n_fitted, " starts reached the best optimum (nll = ",
          round(rq_convergence$best_nll, 3), ", optim code ",
          opt$convergence, ")")

  # Fitted log-means, then log-likelihoods for all three through the same
  # helpers. No logLik() calls feed the table.
  log_mu_ind <- as.numeric(predict(ind_model, type = "link"))
  log_mu_soe <- as.numeric(predict(soe_model, type = "link"))
  log_mu_rq  <- log_quad_log_mu(opt$par, X)

  ll_ind <- pois_ll(log_mu_ind, all_pats$count)
  ll_rq  <- pois_ll(log_mu_rq,  all_pats$count)
  ll_soe <- pois_ll(log_mu_soe, all_pats$count)

  mll_ind <- mult_ll(log_mu_ind, all_pats$count)
  mll_rq  <- mult_ll(log_mu_rq,  all_pats$count)
  mll_soe <- mult_ll(log_mu_soe, all_pats$count)

  df_ind <- J + 1
  df_rq  <- 2 * J + 1
  df_soe <- 1 + J + choose(J, 2)

  # BIC uses n = N, the number of respondents, for all three models.
  bic_n <- N

  # ---- Invariants Table 1 must satisfy ---------------------------------------
  log_mus <- list(ind = log_mu_ind, rq = log_mu_rq, soe = log_mu_soe)

  # fitted probabilities sum to 1 for each model
  p_sums <- vapply(log_mus, function(lm) sum(exp(lm - log(sum(exp(lm))))),
                   numeric(1))
  stopifnot(all(abs(p_sums - 1) < 1e-8))

  # each model's fitted counts sum to N (free intercept), so the Poisson and
  # multinomial log-likelihoods differ only by a model-free constant
  mu_sums <- vapply(log_mus, function(lm) sum(exp(lm)), numeric(1))
  stopifnot(all(abs(mu_sums - N) < 1e-6 * N))

  # nesting: independence ⊂ rank-1 log-quadratic ⊂ full SOE
  ll_tol <- 1e-6 * max(1, abs(ll_soe))
  stopifnot(ll_ind  <= ll_rq  + ll_tol, ll_rq  <= ll_soe  + ll_tol)
  stopifnot(mll_ind <= mll_rq + ll_tol, mll_rq <= mll_soe + ll_tol)

  # multinomial log-likelihoods are log-probabilities: never positive
  stopifnot(mll_ind <= 0, mll_rq <= 0, mll_soe <= 0)

  # parameter counts
  stopifnot(df_ind == J + 1, df_rq == 2 * J + 1,
            df_soe == 1 + J + choose(J, 2))
  if (J == 5) stopifnot(all(c(df_ind, df_rq, df_soe) == c(6, 11, 16)))

  # the shared helper must agree with logLik() for the two GLMs; if it does not,
  # the hand-rolled and built-in likelihoods are on different scales again
  stopifnot(abs(ll_ind - as.numeric(logLik(ind_model))) < 1e-6,
            abs(ll_soe - as.numeric(logLik(soe_model))) < 1e-6)
  # ---------------------------------------------------------------------------

  bic_table <- tibble(
    Model      = c("Independence", "Log-quadratic (rank-1 Γ)", "Full SOE"),
    Parameters = c(df_ind, df_rq, df_soe),
    Log_lik    = c(ll_ind, ll_rq, ll_soe),
    BIC        = c(
      -2 * ll_ind + df_ind * log(bic_n),
      -2 * ll_rq  + df_rq  * log(bic_n),
      -2 * ll_soe + df_soe * log(bic_n)
    )
  )

  # Share of the independence → SOE log-likelihood gap the rank-1 model recovers.
  # Identical on the Poisson and multinomial scales (they differ by a constant).
  rq_gap_share <- (ll_rq - ll_ind) / (ll_soe - ll_ind)

  # Fitted log-p for the scatter plots, normalised within each model
  all_pats$log_p_ind <- log_mu_ind - log(sum(exp(log_mu_ind)))
  all_pats$log_p_rq  <- log_mu_rq  - log(sum(exp(log_mu_rq)))
  all_pats$log_p_soe <- log_mu_soe - log(sum(exp(log_mu_soe)))

  s1_3_result <- list(
    table_name     = tbl,
    J              = J,
    N              = N,
    item_cols      = item_cols,
    patterns       = all_pats,
    gamma_matrix   = gamma_matrix,
    eigenvalues    = eig_val,
    bic_table      = bic_table,
    bic_n          = bic_n,
    ll_multinomial = c(ind = mll_ind, rq = mll_rq, soe = mll_soe),
    rq_gap_share   = rq_gap_share,
    rq_convergence = rq_convergence,
    opt_rq         = opt
  )

  saveRDS(s1_3_result, file.path(fits_dir, "s1_3_result.rds"))
  message("  Saved s1-3 result for: ", tbl)
  break
}

if (is.null(s1_3_result)) stop("No suitable dataset found for Sections 1-3.")

# ==============================================================================
# Section 4: γ_k diagnostic — quadratic total-score log-frequencies
#
# Under the Rasch model with θ ~ N(μ, σ²):
#   log π_k ≈ C + μ(k − ȳ₊) + (σ²/2)(k − ȳ₊)²
# We compute empirical log π_k, fit a quadratic, and compare a dataset that
# fits Rasch well to one that does not.
# ==============================================================================

rasch_tables <- irw_filter(
  n_categories   = 2,
  n_items        = c(8, 30),
  n_participants = c(1000, Inf),
  density        = c(0.75, 1.0)
)
message("Rasch candidates: ", length(rasch_tables))

fit_rasch_diag <- function(table_name, max_n = 10000) {
  message("  Rasch diag: ", table_name)
  out_file <- file.path(fits_dir, paste0("rasch_", table_name, ".rds"))
  if (file.exists(out_file)) { message("    cached"); return(readRDS(out_file)) }

  resp <- fetch_resp(table_name, max_n = max_n)
  if (is.null(resp)) return(NULL)
  resp_c <- resp[complete.cases(resp), ]
  J <- ncol(resp_c); N <- nrow(resp_c)
  if (J < 5 || N < 500) return(NULL)

  m1pl <- tryCatch(
    mirt(resp_c, 1, "Rasch", verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(m1pl)) return(NULL)

  s2 <- paste0("F = 1-", J, "\nPRIOR = (1-", J, ", a1, lnorm, 0.0, 1.0)")
  m2pl <- tryCatch(
    mirt(resp_c, mirt.model(s2), itemtype = rep("2PL", J),
         method = "EM", technical = list(NCYCLES = 2000), verbose = FALSE),
    error = function(e) NULL
  )

  total <- rowSums(resp_c)
  tab   <- table(total)
  k_vals    <- as.integer(names(tab))
  pi_k      <- as.numeric(tab) / N
  gamma_k   <- log(pi_k)
  k_bar     <- weighted.mean(k_vals, pi_k)
  k_c       <- k_vals - k_bar
  quad_fit  <- lm(gamma_k ~ k_c + I(k_c^2))
  r2_quad   <- summary(quad_fit)$r.squared

  ll_diff <- if (!is.null(m2pl))
    as.numeric(logLik(m2pl)) - as.numeric(logLik(m1pl))
  else NA_real_

  result <- list(
    table_name   = table_name,
    J            = J,
    N            = N,
    gamma_k_data = tibble(
      k             = k_vals,
      pi_k          = pi_k,
      gamma_k       = gamma_k,
      k_c           = k_c,
      gamma_k_fitted = fitted(quad_fit)
    ),
    quad_coef  = coef(quad_fit),
    r2_quad    = r2_quad,
    ll_diff    = ll_diff,
    # Δℓ per response: the comparable version of ll_diff across N and J
    ll_diff_per_resp = ll_diff / (N * J)
  )
  saveRDS(result, out_file)
  result
}

rasch_raw <- map(head(rasch_tables, 10), safely(fit_rasch_diag)) |>
  map("result") |> compact()

# Raw Δℓ (2PL vs. Rasch) is NOT comparable across datasets: it grows with the
# number of responses, N × J, so a bigger dataset looks like worse Rasch fit for
# free. Wherever Rasch misfit is quoted, quote it per response.
rasch_meta <- map_dfr(rasch_raw, \(r) tibble(
  table_name = r$table_name, J = r$J, N = r$N,
  r2_quad = r$r2_quad, ll_diff = r$ll_diff,
  ll_diff_per_resp = r$ll_diff / (r$N * r$J)
)) |> arrange(desc(r2_quad))

# Exemplars for the γ_k figures are chosen on the quantity those figures
# actually show -- the quadratic R² of log π_k -- not on Δℓ. The two need not
# agree: Holland's Corollary 2 is a joint prediction about Rasch fit *and*
# normality of the ability distribution, so a dataset can fit Rasch tolerably
# per response and still have a badly non-quadratic log π_k.
best_idx  <- which.max(rasch_meta$r2_quad)
worst_idx <- which.min(rasch_meta$r2_quad)
get_by_name <- function(lst, nm) lst[[which(map_chr(lst, "table_name") == nm)]]

s4_result <- list(
  rasch_meta  = rasch_meta,
  good_rasch  = get_by_name(rasch_raw, rasch_meta$table_name[best_idx]),
  poor_rasch  = get_by_name(rasch_raw, rasch_meta$table_name[worst_idx])
)

# the "good quadratic" exemplar must actually have the better quadratic fit
stopifnot(s4_result$good_rasch$r2_quad >= s4_result$poor_rasch$r2_quad)
saveRDS(s4_result, file.path(fits_dir, "s4_result.rds"))
message("Saved s4 result")

# ==============================================================================
# Section 5: Dimensionality via eigendecomposition of Γ
#
# For a D-dimensional test, the J×J second-order interaction matrix Γ (from the
# full SOE fit) has at most D dominant eigenvalues. We compare its scree plot to
# the standard approach (eigenvalues of the inter-item correlation matrix) for
# one dataset expected to be unidimensional and one that is not.
# ==============================================================================

fit_dim_analysis <- function(table_name, max_n = 15000) {
  message("  Dim: ", table_name)
  out_file <- file.path(fits_dir, paste0("dim_", table_name, ".rds"))
  if (file.exists(out_file)) { message("    cached"); return(readRDS(out_file)) }

  resp <- fetch_resp(table_name, max_n = max_n)
  if (is.null(resp)) return(NULL)
  resp_c <- resp[complete.cases(resp), ]
  J <- ncol(resp_c); N <- nrow(resp_c)
  if (J < 5 || N < 500) return(NULL)

  # Eigenvalues of Pearson correlation matrix (standard scree plot)
  cor_mat    <- cor(resp_c, use = "pairwise.complete.obs")
  cor_eigval <- eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values

  # SOE Γ matrix eigenvalues — feasible only for small J (2^J ≤ 4096)
  soe_eigval <- NULL
  if (J <= 12) {
    pats <- expand.grid(replicate(J, 0:1, simplify = FALSE), KEEP.OUT.ATTRS = FALSE)
    colnames(pats) <- colnames(resp_c)
    obs_tab  <- table(pattern_key(resp_c))
    pats$count <- as.integer(obs_tab[pattern_key(pats)])
    pats$count[is.na(pats$count)] <- 0L

    soe_f <- as.formula(paste0("count ~ (", paste(colnames(resp_c), collapse = " + "), ")^2"))
    soe_m <- tryCatch(
      glm(soe_f, data = pats, family = poisson()),
      error = function(e) NULL
    )
    if (!is.null(soe_m)) {
      gmat <- matrix(0, J, J, dimnames = list(colnames(resp_c), colnames(resp_c)))
      sc   <- coef(soe_m)
      for (nm in names(sc)[grepl(":", names(sc))]) {
        pts <- strsplit(nm, ":")[[1]]
        if (length(pts) == 2 && all(pts %in% colnames(resp_c))) {
          gmat[pts[1], pts[2]] <- sc[nm]
          gmat[pts[2], pts[1]] <- sc[nm]
        }
      }
      soe_eigval <- eigen(gmat, symmetric = TRUE, only.values = TRUE)$values
    }
  }

  result <- list(
    table_name  = table_name,
    J           = J,
    N           = N,
    cor_eigval  = cor_eigval,
    soe_eigval  = soe_eigval
  )
  saveRDS(result, out_file)
  result
}

# Unidimensional candidate: reuse the small-J dataset from Sections 1–3
dim_unidim <- fit_dim_analysis(s1_3_result$table_name)

# Multidimensional candidate: look for a dataset where the 2nd correlation
# eigenvalue is a large fraction of the 1st (less dominant first factor)
multi_cands <- irw_filter(
  n_categories   = 2,
  n_items        = c(10, 20),
  n_participants = c(1000, Inf),
  density        = c(0.75, 1.0)
)
multi_cands <- setdiff(multi_cands, s1_3_result$table_name)

dim_cand_raw <- map(head(multi_cands, 10), safely(fit_dim_analysis)) |>
  map("result") |> compact()

dim_cand_meta <- map_dfr(dim_cand_raw, \(r) {
  ev <- r$cor_eigval
  tibble(table_name = r$table_name, J = r$J,
         ev1 = ev[1], ev2 = ev[2],
         ev2_frac = ev[2] / ev[1])  # larger = more multidimensional
}) |> arrange(desc(ev2_frac))

dim_multidim <- get_by_name(dim_cand_raw, dim_cand_meta$table_name[1])

s5_result <- list(
  unidim   = dim_unidim,
  multidim = dim_multidim
)
saveRDS(s5_result, file.path(fits_dir, "s5_result.rds"))
message("Saved s5 result")

# ==============================================================================
# Section 6: Conservation law — how many parameters does a long test support?
#
# A D-dimensional test of J items supports ≤ (D+1)J parameters (Holland 1990).
# For D = 1: the 2PL saturates the limit; 3PL exceeds it.
# Prediction: IMV gain from 2PL → 3PL should shrink as J grows.
# ==============================================================================

range_tables <- irw_filter(
  n_categories   = 2,
  n_items        = c(5, 60),
  n_participants = c(1000, Inf),
  density        = c(0.80, 1.0)
)
message("Conservation law candidates: ", length(range_tables))

fit_conservation <- function(table_name, max_n = 10000) {
  message("  Conservation: ", table_name)
  out_file <- file.path(fits_dir, paste0("cons_", table_name, ".rds"))
  if (file.exists(out_file)) { message("    cached"); return(readRDS(out_file)) }

  resp <- fetch_resp(table_name, max_n = max_n)
  if (is.null(resp)) return(NULL)
  resp_c <- resp[complete.cases(resp), ]
  J <- ncol(resp_c); N <- nrow(resp_c)
  if (J < 4 || N < 300) return(NULL)

  m1pl <- tryCatch(
    mirt(resp_c, 1, "Rasch", verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(m1pl)) return(NULL)

  s2 <- paste0("F = 1-", J, "\nPRIOR = (1-", J, ", a1, lnorm, 0.0, 1.0)")
  m2pl <- tryCatch(
    mirt(resp_c, mirt.model(s2), itemtype = rep("2PL", J),
         method = "EM", technical = list(NCYCLES = 2000), verbose = FALSE),
    error = function(e) NULL
  )

  # 3PL only for shorter tests — fitting is expensive and unstable for large J
  m3pl <- if (!is.null(m2pl) && J <= 20) {
    tryCatch(
      mirt(resp_c, 1, itemtype = rep("3PL", J),
           method = "EM", technical = list(NCYCLES = 2000), verbose = FALSE),
      error = function(e) NULL
    )
  } else NULL

  set.seed(42)
  imv_1v2 <- if (!is.null(m2pl)) {
    tryCatch(imv(m1pl, m2pl)$mean, error = function(e) NA_real_)
  } else NA_real_

  imv_2v3 <- if (!is.null(m3pl)) {
    tryCatch(imv(m2pl, m3pl)$mean, error = function(e) NA_real_)
  } else NA_real_

  result <- list(
    table_name = table_name,
    J          = J,
    N          = N,
    imv_1v2    = imv_1v2,
    imv_2v3    = imv_2v3
  )
  saveRDS(result, out_file)
  result
}

cons_raw <- map(head(range_tables, 40), safely(fit_conservation)) |>
  map("result") |> compact()

s6_result <- map_dfr(cons_raw, \(r) tibble(
  table_name = r$table_name,
  J          = r$J,
  N          = r$N,
  imv_1v2    = r$imv_1v2,
  imv_2v3    = r$imv_2v3
))
saveRDS(s6_result, file.path(fits_dir, "s6_result.rds"))
message("Saved s6 result")

# ==============================================================================
# Combine all sections and save
# ==============================================================================

saveRDS(
  list(
    s1_3     = s1_3_result,
    s4       = s4_result,
    s5       = s5_result,
    s6       = s6_result,
    date_run = Sys.Date(),
    session  = sessionInfo()
  ),
  file = file.path(out_dir, "dutch_identity_results.rds")
)
message("\nAll results saved to ", out_dir, "/dutch_identity_results.rds")

# ==============================================================================
# Bibtex citations
# ==============================================================================

all_used <- c(
  s1_3_result$table_name,
  s4_result$rasch_meta$table_name,
  s5_result$unidim$table_name,
  s5_result$multidim$table_name,
  s6_result$table_name
) |> unique() |> na.omit()

bib_file <- file.path(out_dir, "irw_references.bib")
walk(all_used, \(tbl) tryCatch(
  irw_save_bibtex(tbl, output_file = bib_file, append = TRUE),
  error = function(e) message("bibtex failed for: ", tbl)
))
message("IRW citations saved to ", bib_file)
