# asymmetric_irt_compute.R
#
# Do the asymmetric IRT models from Shim & Bonifay (2026, BJMSP) -- LPE, RH,
# AO -- actually improve on a standard 2PLM across real IRW binary tables,
# and does the direction of asymmetry (leniency vs. strictness, via their
# Л_P index) relate to construct_type?
#
# Model comparison metric: out-of-sample InterModel Vigorish (IMV) on a
# response-level held-out test set -- NOT in-sample BIC, and NOT k-fold CV.
# Per project convention (see [[feedback_cv_holdout]] / Stenhaug & Domingue,
# 2022), holdout masks individual person-item CELLS, not whole persons:
# for each table, ~20% of observed cells are set to NA before fitting; each
# model's own EAP theta is estimated from a person's *retained* responses
# only; held-out predicted probabilities come from that model's own fitted
# item parameters via extract.item()/probtrace(); imv::imv.binary() compares
# each asymmetric model's held-out predictions against the 2PLM reference's,
# using the identical held-out cells for both (same mask, reused across all
# four model fits on a given table). BIC was the original metric here; see
# [[project_asymmetric_irt_vignette]] memory for why this switched -- all
# three asymmetric models have the same per-item parameter count, so a BIC
# comparison among LPE/RH/AO is just log-likelihood, and a large BIC edge
# over 2PLM does not guarantee any real predictive gain out of sample (the
# validation-gate synthetic check in validation_gate.R section (d) found
# cases where it doesn't).
#
# Requires vignettes/asymmetric_irt_data/validation_gate_log.rds to already
# show a passing gate (see validation_gate.R) -- this script does not re-run
# those checks.
#
# Produces the precomputed results loaded by asymmetric_irt.qmd.
#
# Output: asymmetric_irt_data/asymmetric_irt_results.rds
#         asymmetric_irt_data/irw_references.bib
#
# Usage:
#   Rscript vignettes/asymmetric_irt_data/validation_gate.R   # gate, once
#   Rscript vignettes/asymmetric_irt_compute.R                # from project root
#
# NOTE: this rewrites the fitting methodology (masked-training fits, not
# full-data fits), so any existing vignettes/asymmetric_irt_data/fits/*.rds
# from a prior BIC-based run are stale and must be deleted before re-running
# -- fit_to_disk() below skips any table whose output file already exists.

library(irw)
source("vignettes/asymmetric_irt_helpers.R")
library(dplyr)
library(purrr)
library(furrr)
library(tibble)
library(imv)

set.seed(20260720)

out_dir  <- "vignettes/asymmetric_irt_data"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

MIN_ITEMS        <- 5
MAX_ITEMS        <- 40
MIN_PARTICIPANTS <- 2000  # raised from an initial 1000 -- the validation-gate
                           # simulation showed LPE shape-parameter recovery is
                           # still noisy well above 1000; see validation_gate.R
MAX_N            <- 10000
EM_CYCLES        <- 2000  # lower than the 5000 used for the small validation
                           # gate/reference LSAT7 example -- tractable at
                           # batch scale; convergence is checked per model
HOLDOUT_FRAC     <- 0.2   # response-level (cell) holdout fraction, per
                           # [[feedback_cv_holdout]]; masked once per table,
                           # reused across all four model fits so comparisons
                           # are on identical held-out cells
PILOT            <- FALSE # TRUE: run on a random subset first; check results
                           # before committing to the full candidate list
PILOT_N_TABLES   <- 10

# ==============================================================================
# 1. Select datasets: binary items only (LPE/RH/AO polytomous extensions are
#    future work, not attempted here)
# ==============================================================================

all_candidates <- irw_filter(
  n_categories   = 2,
  n_items        = c(MIN_ITEMS, MAX_ITEMS),
  n_participants = c(MIN_PARTICIPANTS, Inf)
)

message("Candidate binary tables (n_participants >= ", MIN_PARTICIPANTS, "): ", length(all_candidates))

if (PILOT) {
  tables <- sample(all_candidates, min(PILOT_N_TABLES, length(all_candidates)))
  message("PILOT run: ", length(tables), " tables selected.")
} else {
  tables <- all_candidates
}

tags_meta <- tryCatch(irw_tags(tables = tables), error = function(e) {
  message("irw_tags() failed: ", conditionMessage(e))
  NULL
})

# ==============================================================================
# 2. Per-table computation
# ==============================================================================

lambda_from_par <- function(model_type, shape_val) {
  switch(model_type,
         AO  = exp(shape_val),
         LPE = exp(shape_val),
         RH  = shape_val)
}

# Л_P (paper Eq 7): P(b+x) + P(b-x) - 1 at a fixed x = 1 theta-unit from the
# item's own difficulty (b = -d/a1). Positive = leniency, negative = strictness.
lambda_P <- function(def, par_row, x = 1) {
  a <- par_row[["a1"]]; d <- par_row[["d"]]; shape <- par_row[[3]]
  b <- -d / a
  par_vec <- c(a1 = a, d = d, setNames(shape, names(par_row)[3]))
  p_hi <- def$P(par_vec, b + x, 2)[1, 2]
  p_lo <- def$P(par_vec, b - x, 2)[1, 2]
  p_hi + p_lo - 1
}

# Response-level (cell) holdout: mask HOLDOUT_FRAC of the *observed* cells
# (never touches cells that were already NA in the source data), returning
# the masked training matrix plus the held-out cell indices and true values.
mask_holdout <- function(resp, frac = HOLDOUT_FRAC) {
  resp_train <- resp
  obs_idx  <- which(!is.na(as.matrix(resp)), arr.ind = TRUE)
  n_mask   <- floor(frac * nrow(obs_idx))
  mask_idx <- obs_idx[sample(seq_len(nrow(obs_idx)), n_mask), , drop = FALSE]
  true_vals <- as.matrix(resp)[mask_idx]
  for (k in seq_len(nrow(mask_idx))) resp_train[mask_idx[k, 1], mask_idx[k, 2]] <- NA
  list(train = resp_train, mask_idx = mask_idx, true_vals = true_vals)
}

# Held-out predicted P(response = 1) for a fitted model, one probtrace()
# call per item (vectorized across that item's held-out persons) rather
# than one call per cell.
heldout_preds <- function(fit, theta_vec, mask_idx) {
  preds <- numeric(nrow(mask_idx))
  for (j in unique(mask_idx[, 2])) {
    rows    <- which(mask_idx[, 2] == j)
    persons <- mask_idx[rows, 1]
    it <- extract.item(fit, j)
    preds[rows] <- probtrace(it, matrix(theta_vec[persons], ncol = 1))[, "P.1"]
  }
  preds
}

fit_asymmetric_irt <- function(table_name) {
  message("  Processing: ", table_name)

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(df)) return(NULL)

  unique_ids <- unique(df$id)
  if (length(unique_ids) > MAX_N) {
    df <- df[df$id %in% sample(unique_ids, MAX_N), ]
  }

  resp <- irw_long2resp(df)
  resp$id <- NULL
  resp <- resp[, sapply(resp, function(x) length(unique(na.omit(x))) > 1), drop = FALSE]
  if (ncol(resp) < MIN_ITEMS) {
    message("    skipped: fewer than ", MIN_ITEMS, " usable items")
    return(NULL)
  }
  n_categories <- length(unique(na.omit(unlist(resp))))
  if (n_categories != 2) {
    message("    skipped: n_categories = ", n_categories, " (expected binary)")
    return(NULL)
  }

  # Response-level holdout, masked once per table and reused for every
  # model fit below so all comparisons are on identical held-out cells.
  ho <- mask_holdout(resp, HOLDOUT_FRAC)

  # Reference 2PLM, fit on the masked training matrix and on the same fixed
  # GH50 quadrature as the custom models below.
  fit_2pl <- tryCatch(
    mirt(ho$train, 1, itemtype = "2PL", verbose = FALSE,
         technical = list(NCYCLES = EM_CYCLES, customTheta = Theta_mat_ref, customPriorFun = prior_GH50)),
    error = function(e) { message("    2PL failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(fit_2pl)) return(NULL)
  if (!mirt::extract.mirt(fit_2pl, "converged")) {
    message("    skipped: 2PL reference did not converge")
    return(NULL)
  }
  ad_2pl <- extract_ad(fit_2pl)
  th_2pl <- fscores(fit_2pl, method = "EAP")[, 1]
  p_2pl_heldout <- heldout_preds(fit_2pl, th_2pl, ho$mask_idx)

  fit_one_asym <- function(model_type) {
    ad_start <- if (model_type == "RH") convert_ad_logit_to_probit(ad_2pl, D = 1.702) else ad_2pl
    out <- tryCatch(
      fit_custom(ho$train, model_type, make_custom_item(model_type),
                 ad_start = ad_start, shape_init = 0, prior_sd = 1,
                 em_cycles = EM_CYCLES),
      error = function(e) list(mod = NULL, error = TRUE, message = conditionMessage(e))
    )
    if (!has_valid_mod(out)) {
      message("    ", model_type, " failed: ", out$message %||% "unknown error")
      return(NULL)
    }
    if (!isTRUE(out$mod@OptimInfo$converged)) {
      message("    ", model_type, " did not converge; skipping")
      return(NULL)
    }
    out$mod
  }

  ni <- ncol(resp)
  models_fit <- list(AO = fit_one_asym("AO"), LPE = fit_one_asym("LPE"), RH = fit_one_asym("RH"))
  models_fit <- compact(models_fit)
  if (length(models_fit) == 0) {
    message("    skipped: no asymmetric model converged")
    return(NULL)
  }

  # Out-of-sample IMV: each asymmetric model's held-out predictions vs. the
  # 2PLM reference's, on the identical held-out cells. Positive = the
  # asymmetric model predicts held-out responses better than 2PLM.
  imv_val <- map_dbl(models_fit, function(m) {
    th_m <- fscores(m, method = "EAP")[, 1]
    p_m_heldout <- heldout_preds(m, th_m, ho$mask_idx)
    imv.binary(ho$true_vals, p_2pl_heldout, p_m_heldout)
  })
  winning_model <- if (max(imv_val) > 0) names(which.max(imv_val)) else "2PLM"

  item_level <- map_dfr(names(models_fit), function(model_type) {
    par_tab <- extract_param_table(models_fit[[model_type]])
    def <- MODEL_DEFS[[model_type]]
    shape_name <- shape_par_name(model_type)
    tibble(
      table = table_name, model = model_type, item = par_tab$item,
      a1 = par_tab$a1, d = par_tab$d,
      shape_est = par_tab[[shape_name]],
      kappa = lambda_from_par(model_type, par_tab[[shape_name]]),
      lambda_P = pmap_dbl(par_tab, function(...) {
        row <- list(...)
        lambda_P(def, row[c("a1", "d", shape_name)])
      })
    )
  })

  meta_row <- if (!is.null(tags_meta)) tags_meta[tags_meta$table == table_name, ] else NULL
  construct_type <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$construct_type[1] else NA_character_

  list(
    summary = tibble(
      table = table_name, n_items = ni, n_participants = nrow(resp),
      construct_type = construct_type,
      n_heldout = nrow(ho$mask_idx),
      imv_AO  = imv_val[["AO"]]  %||% NA_real_,
      imv_LPE = imv_val[["LPE"]] %||% NA_real_,
      imv_RH  = imv_val[["RH"]]  %||% NA_real_,
      winning_model = winning_model
    ),
    item_level = item_level
  )
}

# ==============================================================================
# 3. Run, writing each result to disk as it completes
# ==============================================================================

fit_to_disk <- function(table_name) {
  out_file <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
  }
  result <- tryCatch(fit_asymmetric_irt(table_name), error = function(e) {
    message("    unexpected error for ", table_name, ": ", conditionMessage(e))
    NULL
  })
  if (!is.null(result)) saveRDS(result, out_file)
}

plan(multisession, workers = min(4, parallel::detectCores() %/% 2))
message("\nFitting ", length(tables), " candidate tables...")
future_map(tables, fit_to_disk, .options = furrr_options(seed = TRUE))
plan(sequential)

# ==============================================================================
# 4. Combine + save
# ==============================================================================

all_raw <- map(tables, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

all_summary    <- map(all_raw, "summary") |> compact() |> bind_rows()
all_item_level <- map(all_raw, "item_level") |> compact() |> bind_rows()

message("\nDone. ", nrow(all_summary), " tables with usable results out of ", length(tables), " candidates.")

saveRDS(
  list(
    summary = all_summary, item_level = all_item_level,
    candidate_tables = tables, n_all_candidates = length(all_candidates),
    pilot = PILOT, min_participants = MIN_PARTICIPANTS,
    date_run = Sys.Date(), session = sessionInfo()
  ),
  file = file.path(out_dir, "asymmetric_irt_results.rds")
)

message("Saved to ", out_dir, "/asymmetric_irt_results.rds")

tryCatch(
  irw_save_bibtex(unique(all_summary$table), output_file = file.path(out_dir, "irw_references.bib")),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)
