# continuous_bounded_compute.R
#
# PILOT / DRAFT scope (2026-07-30): this is a minimal, single-shot run meant
# to produce a first working draft of continuous_bounded.qmd for review by a
# collaborator before investing in the full study design. It is NOT the full
# simulation/recovery grid described in the original vignette spec. Still
# missing, deliberately, pending sign-off on this draft:
#   - the full DGP x N x J x dispersion cross-fitting grid (currently: one
#     simulated dataset, one DGP, one N/J combination)
#   - the boundary-inflation (zero-one-piling) condition
#   - Ferrando's (2001) nonlinear congeneric model (optional stretch spec;
#     not attempted at all yet, per the prompt's own sequencing rule that it
#     only gets attempted after specs 1-4 are complete and validated)
#   - future_map/plan(multisession) parallelization (not needed yet at this
#     scale)
#
# What IS included: real fits of all 4 core specs --
#   1. naive linear baseline (lavaan)
#   2. Beta IRT, fixed AND item-varying dispersion (sirt+mirt custom itemtype)
#   3. Samejima CRM (EstCRM)
#   4. Mueller CRSM (patched pcIRT)
# -- on (a) one simulated Beta-IRT dataset and (b) one real IRW table, each
# scored via a person-level held-out log-likelihood (see
# continuous_bounded_helpers.R's "Held-out design note" for why cell-level
# masking -- the project's usual convention -- isn't viable here: EstCRM and
# pcIRT's fitting code has no missing-data support at all). See the helpers
# file header for the package-bug fixes, the cross-model likelihood-scale
# resolution, and the IMV-vs-log-likelihood decision -- all flagged there as
# they were found/made, not assumed silently.
#
# Real-data table: lsbq_maleki_2025_non_persian_proficiency (3 items, N=312,
# 0-10 scale). Chosen over the 0-100 "Slider/continuous"-tagged tables in the
# scout output (emoji_scheffler_2024, ai_fear_dong_2026_*, etc.) because
# those came back 100% integer-valued on inspection -- weaker evidence of
# genuine continuity than this table's ~40% non-integer responses. Two other
# scout candidates were checked and rejected before landing on this one:
# eammi_grahe_2018_marriage_identity_allocation (row sums are a constant 100
# -- an ipsative/compositional allocation task, which violates the
# conditional-independence assumption every one of these 4 models makes) and
# much_tte_2025_currentmotivation / test_taking_much_2025_cm (person x item
# cells are not unique -- a repeated-measures/longitudinal design, not a
# single-occasion multi-item battery).
#
# Output: vignettes/continuous_bounded_data/continuous_bounded_results.rds
#
# Usage:
#   Rscript vignettes/continuous_bounded_compute.R   # from project root

library(irw)
source("vignettes/continuous_bounded_helpers.R")
library(dplyr)
library(tidyr)

set.seed(20260730)

out_dir <- "vignettes/continuous_bounded_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_rds <- file.path(out_dir, "continuous_bounded_results.rds")

K_BINS <- 12          # shared bin count for held-out interval scoring, all models
TEST_FRAC <- 0.25     # person-level test-set fraction

# ==============================================================================
# Shared person-split fit/score routine. Y01: [0,1]-rescaled response matrix,
# NO missing cells (complete cases only, this pilot). Columns must already be
# safe bare identifiers (e.g. "item1", not "17.1.R" or "CM01_02") -- lavaan's
# model-string parser doesn't support arbitrary/backtick-quoted names.
# ==============================================================================

fit_score_all <- function(Y01, label, K = K_BINS, test_frac = TEST_FRAC, seed) {
  set.seed(seed)
  N <- nrow(Y01); J <- ncol(Y01)
  bins <- make_bins(K)
  test_idx <- sample(seq_len(N), floor(N * test_frac))
  train_idx <- setdiff(seq_len(N), test_idx)

  Ytr <- Y01[train_idx, , drop = FALSE]
  Yte <- Y01[test_idx, , drop = FALSE]
  n_tr <- nrow(Ytr)

  # held-out cells to score: every (test person, item) combination
  held <- expand.grid(person = seq_len(nrow(Yte)), item = seq_len(J))
  held$value <- Yte[cbind(held$person, held$item)]
  held$bin <- bin_of(held$value, bins)
  held$lo <- bins$edges[held$bin]
  held$hi <- bins$edges[held$bin + 1]

  out <- list()
  fits <- list()

  # --- 1. naive linear --------------------------------------------------------
  fit_lin <- tryCatch(fit_naive_linear(Ytr), error = function(e) e)
  if (!inherits(fit_lin, "error")) {
    th <- theta_new_linear(fit_lin, Yte)
    ll <- mapply(function(i, p, lo, hi) bin_logprob_linear(fit_lin, colnames(Yte)[i], th[p], lo, hi),
                 held$item, held$person, held$lo, held$hi)
    out$naive_linear <- data.frame(model = "naive_linear", label = label,
                                    n_held = sum(!is.na(ll)), mean_ll = mean(ll, na.rm = TRUE))
  } else message("  [", label, "] naive linear failed: ", conditionMessage(fit_lin))
  fits$naive_linear <- fit_lin

  # --- 2/2b. Beta IRT, item-varying and fixed dispersion ----------------------
  Ycat_tr <- discretize01(Ytr, K)
  Ycat_te <- discretize01(Yte, K)
  for (disp in c("item", "fixed")) {
    fit_b <- tryCatch(fit_beta_irt(Ycat_tr, dispersion = disp), error = function(e) e)
    key <- paste0("beta_irt_", disp, if (disp == "item") "_dispersion" else "_dispersion")
    if (!inherits(fit_b, "error")) {
      th <- theta_new_beta_irt(fit_b, Ycat_te)
      ll <- mapply(function(i, p, b) bin_logprob_beta_irt(fit_b, i, K, th[p], b),
                   held$item, held$person, held$bin)
      out[[key]] <- data.frame(model = paste0("beta_irt_", disp, "_dispersion"), label = label,
                                n_held = sum(!is.na(ll)), mean_ll = mean(ll, na.rm = TRUE))
    } else message("  [", label, "] beta IRT (", disp, " dispersion) failed: ", conditionMessage(fit_b))
    fits[[key]] <- fit_b
  }

  # --- 3. Samejima CRM ---------------------------------------------------------
  Ytr_sq <- Ytr; Ytr_sq[] <- squeeze01(as.matrix(Ytr), n_tr)
  Yte_sq <- Yte; Yte_sq[] <- squeeze01(as.matrix(Yte), n_tr)
  fit_crm <- tryCatch(fit_samejima_crm(Ytr_sq, max_em = 200), error = function(e) e)
  if (!inherits(fit_crm, "error")) {
    th_tr <- tryCatch(theta_new_samejima_crm(fit_crm, Ytr_sq), error = function(e) e)
    th_te <- tryCatch(theta_new_samejima_crm(fit_crm, Yte_sq), error = function(e) e)
    if (!inherits(th_tr, "error") && !inherits(th_te, "error")) {
      rs <- .crm_resid_sd(fit_crm, Ytr_sq, th_tr)
      ll <- mapply(function(i, p, lo, hi) bin_logprob_samejima_crm(fit_crm, i, rs, th_te[p], lo, hi),
                   held$item, held$person, held$lo, held$hi)
      out$samejima_crm <- data.frame(model = "samejima_crm", label = label,
                                      n_held = sum(!is.na(ll)), mean_ll = mean(ll, na.rm = TRUE))
    } else message("  [", label, "] Samejima CRM theta estimation failed")
  } else message("  [", label, "] Samejima CRM fit failed: ", conditionMessage(fit_crm))
  fits$samejima_crm <- fit_crm

  # --- 4. Mueller CRSM ----------------------------------------------------------
  fit_mu <- tryCatch(fit_muller_corsm(Ytr_sq), error = function(e) e)
  if (!inherits(fit_mu, "error")) {
    th_te <- tryCatch(theta_new_muller_corsm(fit_mu, Yte_sq), error = function(e) e)
    if (!inherits(th_te, "error")) {
      ll <- mapply(function(i, p, lo, hi) bin_logprob_muller_corsm(fit_mu, i, th_te[p], lo, hi),
                   held$item, held$person, held$lo, held$hi)
      out$muller_corsm <- data.frame(model = "muller_corsm", label = label,
                                      n_held = sum(!is.na(ll)), mean_ll = mean(ll, na.rm = TRUE))
    } else message("  [", label, "] Mueller CRSM theta estimation failed")
  } else message("  [", label, "] Mueller CRSM fit failed: ", conditionMessage(fit_mu))
  fits$muller_corsm <- fit_mu

  list(table = bind_rows(out), fits = fits, train_idx = train_idx, test_idx = test_idx)
}

if (!file.exists(out_rds)) {

  # ============================================================================
  # A. Simulated data (Beta-IRT DGP, single condition -- recovery pilot only)
  # ============================================================================
  message("=== Simulated Beta-IRT dataset ===")
  N_sim <- 300; J_sim <- 10
  theta_true <- rnorm(N_sim)
  delta_true <- seq(-1.5, 1.5, length.out = J_sim)
  tau_true <- rep(0.6, J_sim)
  Y_sim <- sim_beta_irt(theta_true, delta_true, tau_true)  # already (0,1), cols "item1".."item10"

  sim_out <- fit_score_all(Y_sim, label = "sim_beta_irt", seed = 1)

  recovery_cor <- if (!inherits(sim_out$fits$beta_irt_item_dispersion, "error")) {
    cf <- mirt::coef(sim_out$fits$beta_irt_item_dispersion, simplify = TRUE)$items
    cor(cf[, "delta"], delta_true[match(rownames(cf), colnames(Y_sim))])
  } else NA_real_

  # ============================================================================
  # B. Real data: lsbq_maleki_2025_non_persian_proficiency
  # ============================================================================
  message("=== Real data: lsbq_maleki_2025_non_persian_proficiency ===")
  real_table <- "lsbq_maleki_2025_non_persian_proficiency"
  real_low <- 0; real_high <- 10

  df_real <- irw_fetch(real_table)
  wide_real <- df_real |>
    select(id, item, resp) |>
    distinct(id, item, .keep_all = TRUE) |>
    pivot_wider(names_from = item, values_from = resp) |>
    as.data.frame()
  rownames(wide_real) <- wide_real$id
  orig_items <- setdiff(colnames(wide_real), "id")
  Y_real_raw <- as.matrix(wide_real[, orig_items])
  storage.mode(Y_real_raw) <- "double"
  # complete cases only for this pilot (3 items; partial-missing handling is
  # future work, not attempted here)
  Y_real_raw <- Y_real_raw[complete.cases(Y_real_raw), , drop = FALSE]
  Y_real01 <- rescale01(Y_real_raw, real_low, real_high)
  colnames(Y_real01) <- paste0("item", seq_len(ncol(Y_real01)))  # lavaan-safe names

  real_out <- fit_score_all(Y_real01, label = real_table, seed = 2)

  tryCatch(
    irw_save_bibtex(real_table, output_file = file.path(out_dir, "irw_references.bib")),
    error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
  )

  # ============================================================================
  # Save
  # ============================================================================
  results <- list(
    scope_note = "PILOT/DRAFT run -- see header of continuous_bounded_compute.R",
    sim = list(theta_true = theta_true, delta_true = delta_true, tau_true = tau_true,
               Y = Y_sim, train_idx = sim_out$train_idx, test_idx = sim_out$test_idx,
               scores = sim_out$table, recovery_cor_delta = recovery_cor),
    real = list(table = real_table, low = real_low, high = real_high,
                orig_items = orig_items,
                N = nrow(Y_real_raw), J = ncol(Y_real_raw),
                Y_raw = Y_real_raw, train_idx = real_out$train_idx, test_idx = real_out$test_idx,
                scores = real_out$table)
  )
  saveRDS(results, out_rds)
  message("Saved results to ", out_rds)
} else {
  message(out_rds, " already exists -- skipping (delete it to force a rerun)")
}
