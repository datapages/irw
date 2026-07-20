# asymmetric_irt_compute.R
#
# Do the asymmetric IRT models from Shim & Bonifay (2026, BJMSP) -- LPE, RH,
# AO -- actually improve on a standard 2PLM across real IRW binary tables,
# and does the direction of asymmetry (leniency vs. strictness, via their
# Л_P index) relate to construct_type?
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

library(irw)
source("vignettes/asymmetric_irt_helpers.R")
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

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

  # Reference 2PLM, fit on the same fixed GH50 quadrature as the custom
  # models below so BIC comparisons aren't confounded by different
  # integration grids.
  fit_2pl <- tryCatch(
    mirt(resp, 1, itemtype = "2PL", verbose = FALSE,
         technical = list(NCYCLES = EM_CYCLES, customTheta = Theta_mat_ref, customPriorFun = prior_GH50)),
    error = function(e) { message("    2PL failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(fit_2pl)) return(NULL)
  if (!mirt::extract.mirt(fit_2pl, "converged")) {
    message("    skipped: 2PL reference did not converge")
    return(NULL)
  }
  ad_2pl <- extract_ad(fit_2pl)
  bic_2pl <- mirt::extract.mirt(fit_2pl, "BIC")

  fit_one_asym <- function(model_type) {
    ad_start <- if (model_type == "RH") convert_ad_logit_to_probit(ad_2pl, D = 1.702) else ad_2pl
    out <- tryCatch(
      fit_custom(resp, model_type, make_custom_item(model_type),
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

  bic_delta <- map_dbl(models_fit, function(m) mirt::extract.mirt(m, "BIC") - bic_2pl)
  winning_model <- if (min(bic_delta) < 0) names(which.min(bic_delta)) else "2PLM"

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
      construct_type = construct_type, bic_2pl = bic_2pl,
      bic_delta_AO  = bic_delta[["AO"]]  %||% NA_real_,
      bic_delta_LPE = bic_delta[["LPE"]] %||% NA_real_,
      bic_delta_RH  = bic_delta[["RH"]]  %||% NA_real_,
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
