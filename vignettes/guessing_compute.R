# guessing_compute.R
#
# When guessing is present in multiple-choice data, which correction actually
# helps out-of-sample, and when does the correction itself become a
# liability? Compares three published lenses on guessing --
#   - 1PL-AG (San Martin, Del Pino, & De Boeck, 2006): ability-based guessing,
#     person x item interaction
#   - Mixture / "safety valve" (Xiao, Ulitzsch, Zhang, Frank, & Domingue, 2026):
#     latent guessing-class membership, person-level
#   - Method A purification (Torres Irribarra, Echeverria, & Espinoza, 2026):
#     person-level calibration-sample cleanup, no new item response function
# plus the standard Rasch / 2PL / 1PLg / 3PL baselines, on real IRW tables,
# using out-of-sample IMV (Stenhaug & Domingue, 2022; Domingue et al., 2025b).
#
# Data: 8 ENEM tables (Brazil's national HS exam; always 5 response options,
# per documented exam structure -- IRW carries no item text for these tables,
# so m=5 is an assumed-from-documentation value, NOT verified from IRW item
# text) + 3 gilbert_meta_* tables (m verified directly from IRW item text).
# See guessing.qmd for the full data-selection writeup and the discovery
# process that produced this list (candidate pool audited by hand -- several
# metadata-plausible tables were excluded after inspection revealed
# rubric-coded "options" or choose-two items rather than single-answer MC).
#
# Holdout: single 80/20 response-level (cell) holdout per table, masked once
# and reused across all 7 model fits, per project convention
# ([[feedback_cv_holdout]] / Stenhaug & Domingue, 2022) -- masking is
# per-person, always leaving >=1 retained response. NOT 20 repeated splits
# (an earlier draft of this script used the Safety Valve paper's 20-split
# protocol; simplified to one split for the first run).
#
# Priors: 2PL and 3PL both use lognormal(0,1) on a1 (repo convention, see
# imv.qmd), NOT the Safety Valve paper's tighter lnorm(0,0.5) -- unified for
# consistency within this vignette. 3PL's g uses Beta(5,17) (mean ~0.23),
# per the Safety Valve paper's Study 5.
#
# Output: vignettes/guessingdata/guessing_results.rds
#
# Usage:
#   Rscript vignettes/guessing_compute.R   # from project root

library(irw)
source("vignettes/guessing_helpers.R")
library(mirt)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)
library(imv)

`%||%` <- function(a, b) if (is.null(a)) b else a

set.seed(20260722)

out_dir  <- "vignettes/guessingdata"
fits_dir <- file.path(out_dir, "fits")
prep_dir <- file.path(out_dir, "prepared")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(prep_dir, recursive = TRUE, showWarnings = FALSE)

HOLDOUT_FRAC <- 0.2
SUBSAMPLE_N  <- 3000   # ENEM tables only; gilbert_meta_* used at full N
EM_CYCLES    <- 2000  # matches repo convention (asymmetric_irt/local_dependence/
                       # dutch_identity compute scripts); 3PL non-convergence at
                       # this budget is treated as a real, gracefully-skipped
                       # result -- 3PL instability at moderate J is a documented
                       # phenomenon (Lord, 2012; Thissen & Wainer, 1982; see
                       # dutch_identity.qmd's own J<=20 restriction), not a bug
QUAD         <- build_quadrature(41)

# ==============================================================================
# 1. Table list (locked in with Ben 2026-07-22; see guessing.qmd Data section
#    for the full discovery/exclusion writeup)
# ==============================================================================

TABLES <- tribble(
  ~table,               ~m, ~m_verified,
  "enem_2013_1mil_mt",   5, FALSE,
  "enem_2013_1mil_lc",   5, FALSE,
  "enem_2013_1mil_ch",   5, FALSE,
  "enem_2013_1mil_cn",   5, FALSE,
  "enem_2014_1mil_ch",   5, FALSE,
  "enem_2019_1mil_ch",   5, FALSE,
  "enem_2019_1mil_lc",   5, FALSE,
  "enem_2024_1mil_ch",   5, FALSE,
  "gilbert_meta_1",      4, TRUE,
  "gilbert_meta_102",    5, TRUE,
  "gilbert_meta_103",    5, TRUE,
)

# ==============================================================================
# 2. Holdout mask and mirt held-out predictions now live in
#    guessing_helpers.R (mask_holdout(), heldout_preds_mirt()), so that
#    guessing_sim_compute.R and guessing_gsweep_compute.R evaluate on exactly
#    the same holdout convention this script uses.
# ==============================================================================

# ==============================================================================
# 3. Fetch + prepare one table (wide 0/1 matrix, subsampled if large)
# ==============================================================================

prepare_table <- function(table_name, m) {
  df <- irw_fetch(table_name)

  # Peak memory lives in this function, not in the model fits: the enem_*_1mil_*
  # tables are ~1M respondents x ~45 items, so the fetched long frame is ~45M
  # rows, while everything downstream works on a SUBSAMPLE_N x ~45 matrix. Keep
  # the fetched object as small as possible and drop it as soon as it is no
  # longer needed, rather than letting several full-size copies coexist.
  df <- df[, c("id", "item", "resp"), drop = FALSE]   # drop unused columns first
  df$resp <- suppressWarnings(as.numeric(df$resp))
  df <- df[!is.na(df$resp) & df$resp %in% c(0, 1), , drop = FALSE]

  ids <- unique(df$id)
  if (length(ids) > SUBSAMPLE_N) {
    ids <- sample(ids, SUBSAMPLE_N)
    df  <- df[df$id %in% ids, , drop = FALSE]
    # The subset is now ~0.3% of the fetched rows; release the rest before the
    # reshape, which would otherwise allocate on top of the full-size frame.
    gc(verbose = FALSE)
  }

  wide <- df |>
    distinct(id, item, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = item, values_from = resp)
  rm(df); gc(verbose = FALSE)

  mat <- as.matrix(wide[, -1])
  rownames(mat) <- wide$id
  storage.mode(mat) <- "numeric"
  rm(wide); gc(verbose = FALSE)
  as.data.frame(mat)
}

# Sequential fetch/subsample pass, so the memory-bound fetch and the CPU-bound
# fitting never overlap. Measured on enem_2013_1mil_mt: RSS 3.1 GB after the
# fetch, 5.1 GB after the type/validity filter, 6.1 GB by the time the
# 3,000-person subsample (135,000 rows, ~4 MB) exists -- and R does not return
# those pages to the OS, so a worker that fetched stayed multi-GB for the whole
# fitting phase. The mitigations in prepare_table() shrink that, but the fetch
# itself is irreducible: irw_fetch() has no server-side row or person limit, so
# all ~45M rows must be materialized to keep 0.3% of them.
#
# Running the fetches one at a time caps peak at a single table's fetch rather
# than one per worker, and leaves the parallel pass reading ~4 MB files.
# Tables already prepared or already fitted are skipped, so this resumes.
prepare_to_disk <- function() {
  for (i in seq_len(nrow(TABLES))) {
    table_name <- TABLES$table[i]
    prep_file <- file.path(prep_dir, paste0(table_name, ".rds"))
    fit_file  <- file.path(fits_dir, paste0(table_name, ".rds"))
    if (file.exists(prep_file) || file.exists(fit_file)) {
      message("  Already prepared or fitted, skipping fetch: ", table_name)
      next
    }
    message("  Fetching: ", table_name)
    resp <- tryCatch(prepare_table(table_name, TABLES$m[i]), error = function(e) {
      message("    fetch/prepare failed: ", conditionMessage(e)); NULL
    })
    if (!is.null(resp)) {
      saveRDS(resp, prep_file)
      message("    -> ", nrow(resp), " x ", ncol(resp), " saved")
    }
    rm(resp); gc(verbose = FALSE)
  }
}

# ==============================================================================
# 4. Fit all 7 specs on one table, cache held-out predictions + diagnostics
# ==============================================================================

fit_one_table <- function(table_name, m, m_verified) {
  message("  Processing: ", table_name)

  # Read the matrix prepared by prepare_to_disk(); only fall back to fetching
  # here if fit_one_table() is called directly, outside the two-pass driver.
  prep_file <- file.path(prep_dir, paste0(table_name, ".rds"))
  resp <- if (file.exists(prep_file)) {
    readRDS(prep_file)
  } else {
    tryCatch(prepare_table(table_name, m), error = function(e) {
      message("    fetch/prepare failed: ", conditionMessage(e)); NULL
    })
  }
  if (is.null(resp)) return(NULL)

  # Drop items with no observed variance. mirt cannot estimate a difficulty
  # for an item everyone gets right or everyone gets wrong, so a single such
  # item fails the Rasch fit and takes the whole table with it.
  #
  # This is not hypothetical: the enem_*_1mil_lc tables carry elective
  # English/Spanish blocks that only a subset of candidates sit, and in one
  # 3,000-person draw of enem_2019_1mil_lc five of those items came back with
  # ~1,500 observed responses that were *all* zero -- a coding artifact of the
  # block the candidate did not take, not 1,500 people getting an item wrong.
  # Which elective items appear at all depends on who is sampled, so without
  # this screen the table's usability varies with the subsample draw.
  item_p <- colMeans(resp, na.rm = TRUE)
  degenerate <- is.na(item_p) | item_p %in% c(0, 1)
  n_dropped <- sum(degenerate)
  if (n_dropped > 0) {
    message("    dropping ", n_dropped, " zero-variance item(s): ",
            paste(names(resp)[degenerate], collapse = ", "))
    resp <- resp[, !degenerate, drop = FALSE]
  }
  if (ncol(resp) < 5) {
    message("    fewer than 5 usable items after screening; skipping table")
    return(NULL)
  }

  ni <- ncol(resp); np <- nrow(resp)
  ho <- mask_holdout(resp, HOLDOUT_FRAC)
  Y_full  <- as.matrix(resp)         # ground truth, all cells
  Y_train <- as.matrix(ho$train)     # NA at held-out cells

  converged <- list()
  preds <- list()

  # --- Rasch ---
  fit_rasch <- tryCatch(mirt(ho$train, 1, itemtype = "Rasch", verbose = FALSE,
                              technical = list(NCYCLES = EM_CYCLES)),
                         error = function(e) NULL)
  if (is.null(fit_rasch)) { message("    Rasch failed; skipping table"); return(NULL) }
  converged$rasch <- isTRUE(extract.mirt(fit_rasch, "converged"))
  preds$rasch <- heldout_preds_mirt(fit_rasch, ho$mask_idx)

  # --- 2PL, lnorm(0,1) prior on a1 (imv.qmd convention) ---
  s2 <- paste0("F = 1-", ni, "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)")
  fit_2pl <- tryCatch(mirt(ho$train, mirt.model(s2), itemtype = "2PL", verbose = FALSE,
                            technical = list(NCYCLES = EM_CYCLES)),
                       error = function(e) NULL)
  converged$pl2 <- !is.null(fit_2pl) && isTRUE(extract.mirt(fit_2pl, "converged"))
  preds$pl2 <- if (converged$pl2) heldout_preds_mirt(fit_2pl, ho$mask_idx) else NA

  # --- 1PLg, fixed g=1/m ---
  fit_plg <- tryCatch(fit_1plg(ho$train, m), error = function(e) NULL)
  converged$plg <- !is.null(fit_plg) && isTRUE(extract.mirt(fit_plg, "converged"))
  preds$plg <- if (converged$plg) heldout_preds_mirt(fit_plg, ho$mask_idx) else NA

  # --- 3PL, lnorm(0,1) on a1 + Beta(5,17) on g ---
  # NOTE: mirt's 3PL stores g on a transformed (unconstrained) internal scale,
  # not the raw [0,1] scale -- a plain "beta" PRIOR evaluates the Beta density
  # on that wrong scale, badly miscalibrating the effective prior on g (this
  # was diagnosed after an earlier run collapsed every table's 3PL guessing
  # parameter to ~0.5 regardless of true chance level). "expbeta" applies
  # plogis() first, exactly as mirt's own docs specify for 3PL/4PL lower
  # asymptotes: "(items, expbeta, alpha, beta) for the beta distribution
  # after applying the function plogis to the input value (note, this is
  # specifically for applying a beta prior to the lower-bound parameters in
  # 3/4PL models)".
  s3 <- paste0("F = 1-", ni,
               "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0), (1-", ni, ", g, expbeta, 5, 17)")
  fit_3pl <- tryCatch(mirt(ho$train, mirt.model(s3), itemtype = "3PL", verbose = FALSE,
                            technical = list(NCYCLES = EM_CYCLES)),
                       error = function(e) NULL)
  converged$pl3 <- !is.null(fit_3pl) && isTRUE(extract.mirt(fit_3pl, "converged"))
  preds$pl3 <- if (converged$pl3) heldout_preds_mirt(fit_3pl, ho$mask_idx) else NA

  # --- 1PL-AG ---
  fit_ag <- tryCatch(fit_1pl_ag(Y_train, quad = QUAD), error = function(e) {
    message("    1PL-AG failed: ", conditionMessage(e)); NULL
  })
  converged$ag <- !is.null(fit_ag) && fit_ag$converged_ag
  if (!is.null(fit_ag)) {
    pred_mat <- predict_1pl_ag(fit_ag, Y_train)
    preds$ag <- pred_mat[ho$mask_idx]
  } else preds$ag <- NA

  # --- Mixture (safety valve), g_fit = 1/m ---
  fit_mix <- tryCatch(fit_mixture(Y_train, g_fit = 1 / m, quad = QUAD),
                       error = function(e) { message("    Mixture failed: ", conditionMessage(e)); NULL })
  converged$mix <- !is.null(fit_mix) && fit_mix$converged
  if (!is.null(fit_mix)) {
    pred_mat <- predict_mixture(fit_mix, Y_train)
    preds$mix <- pred_mat[ho$mask_idx]
  } else preds$mix <- NA

  # --- Purified Rasch (Method A) ---
  fit_pur <- tryCatch(purify_rasch(Y_train, g_fit = 1 / m, quad = QUAD),
                       error = function(e) { message("    Purification failed: ", conditionMessage(e)); NULL })
  if (!is.null(fit_pur)) {
    pred_mat <- predict_purified_rasch(fit_pur, Y_train, quad = QUAD)
    preds$purified <- pred_mat[ho$mask_idx]
  } else preds$purified <- NA

  list(
    table = table_name, m = m, m_verified = m_verified,
    n_items = ni, n_participants = np, n_heldout = nrow(ho$mask_idx),
    n_items_dropped = n_dropped,
    true_vals = ho$true_vals,
    preds = preds,
    converged = converged,
    pi_hat = if (!is.null(fit_mix)) fit_mix$pi else NA_real_,
    alpha_hat = if (!is.null(fit_ag)) fit_ag$alpha else NA_real_,
    se_alpha = if (!is.null(fit_ag)) fit_ag$se_alpha else NA_real_,
    lr_stat = if (!is.null(fit_ag)) fit_ag$lr_stat else NA_real_,
    lr_p = if (!is.null(fit_ag)) fit_ag$lr_p else NA_real_,
    frac_flagged = if (!is.null(fit_pur)) fit_pur$frac_flagged else NA_real_,
    # estimated latent SDs: every model on the page now estimates the theta
    # variance, matching mirt's itemtype = "Rasch" baseline (see
    # guessing_helpers.R::.scale_quad for why this matters)
    sd_rasch = if (converged$rasch) sqrt(coef(fit_rasch, simplify = TRUE)$cov[1, 1]) else NA_real_,
    sd_mix = if (!is.null(fit_mix)) fit_mix$sd else NA_real_,
    sd_ag  = if (!is.null(fit_ag)) fit_ag$sd else NA_real_,
    sd_pur = if (!is.null(fit_pur)) fit_pur$sd else NA_real_
  )
}

fit_to_disk <- function(i) {
  table_name <- TABLES$table[i]; m <- TABLES$m[i]; m_verified <- TABLES$m_verified[i]
  out_file <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
  }
  result <- tryCatch(fit_one_table(table_name, m, m_verified), error = function(e) {
    message("    unexpected error for ", table_name, ": ", conditionMessage(e)); NULL
  })
  if (!is.null(result)) saveRDS(result, out_file)
}

if (!isTRUE(getOption("guessing.testmode"))) {
  # 2 workers, not the repo's usual min(4, detectCores() %/% 2). Peak memory
  # here is dominated by the fetched long-format table, not by model fitting:
  # eight of the eleven tables are enem_*_1mil_* (~1M respondents x ~45 items,
  # so ~45M long rows) and prepare_table() has to fetch the whole thing before
  # it can subsample to SUBSAMPLE_N. A previous run at 4 workers reached 4-5 GB
  # RSS each, filled 30 GB and drove the machine into swap. Worker count is the
  # blunt lever on concurrent peak; see prepare_table() for the per-worker one.
  # Pass 1: fetch + subsample, one table at a time (memory-bound).
  message("\nPreparing tables (sequential fetch) ...")
  prepare_to_disk()

  # Pass 2: fit, in parallel over the small prepared matrices (CPU-bound).
  # Nothing nests underneath furrr here: mirt is serial (no mirtCluster()
  # call anywhere in this project) and R is linked against reference BLAS,
  # not a threaded one. OMP_NUM_THREADS is pinned to 1 by the launcher as a
  # belt-and-braces guard in case that linkage ever changes.
  plan(multisession, workers = 2)
  message("\nFitting ", nrow(TABLES), " tables...")
  future_map(seq_len(nrow(TABLES)), fit_to_disk, .options = furrr_options(seed = TRUE))
  plan(sequential)
}

# ==============================================================================
# 5. Combine: derive the five headline IMV comparisons per table from the
#    cached held-out predictions (compute_imv validated against imv::imv.binary
#    -- see recovery/gradient checks in guessing_helpers.R development notes).
# ==============================================================================

if (!isTRUE(getOption("guessing.testmode"))) {

all_raw <- map(TABLES$table, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

summarize_one <- function(r) {
  y <- r$true_vals
  imv_safe <- function(p0, p1) {
    if (any(is.na(p0)) || any(is.na(p1))) return(NA_real_)
    compute_imv(p0, p1, y)
  }
  tibble(
    table = r$table, m = r$m, m_verified = r$m_verified,
    n_items = r$n_items, n_participants = r$n_participants, n_heldout = r$n_heldout,
    n_items_dropped = r$n_items_dropped %||% 0L,
    converged_rasch = r$converged$rasch, converged_2pl = r$converged$pl2,
    converged_1plg = r$converged$plg, converged_3pl = r$converged$pl3,
    converged_ag = r$converged$ag, converged_mix = r$converged$mix,
    pi_hat = r$pi_hat, alpha_hat = r$alpha_hat, se_alpha = r$se_alpha,
    lr_stat = r$lr_stat, lr_p = r$lr_p, frac_flagged = r$frac_flagged,
    sd_rasch = r$sd_rasch %||% NA_real_, sd_mix = r$sd_mix %||% NA_real_,
    sd_ag = r$sd_ag %||% NA_real_, sd_pur = r$sd_pur %||% NA_real_,
    imv_1plg_mix   = imv_safe(r$preds$plg, r$preds$mix),
    imv_1plg_ag    = imv_safe(r$preds$plg, r$preds$ag),
    imv_rasch_purified = imv_safe(r$preds$rasch, r$preds$purified),
    imv_3pl_mix    = imv_safe(r$preds$pl3, r$preds$mix),
    imv_3pl_ag     = imv_safe(r$preds$pl3, r$preds$ag)
  )
}

all_summary <- map(all_raw, summarize_one) |> compact() |> bind_rows()

message("\nDone. ", nrow(all_summary), " tables with usable results out of ", nrow(TABLES), " candidates.")

saveRDS(
  list(summary = all_summary, raw = all_raw, tables = TABLES,
       holdout_frac = HOLDOUT_FRAC, subsample_n = SUBSAMPLE_N,
       date_run = Sys.Date(), session = sessionInfo()),
  file = file.path(out_dir, "guessing_results.rds")
)

message("Saved to ", out_dir, "/guessing_results.rds")

tryCatch(
  irw_save_bibtex(unique(all_summary$table), output_file = file.path(out_dir, "irw_references.bib")),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)

}  # end !guessing.testmode
