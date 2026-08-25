# lsirm_interaction_maps_compute.R
#
# Produces the precomputed results loaded by lsirm_interaction_maps.qmd at
# render time.
#
# Research question: the Rasch model assumes local independence -- once you
# condition on theta, item responses are unrelated. The Latent Space Item
# Response Model (LSIRM; Jeon, Jin, Schweinberger & Baugh, 2021, Psychometrika)
# adds a respondent-item distance term in a shared 2D "interaction map": items
# and respondents that violate local independence together are pulled close
# together in that space. This script fits 1PL/GRM LSIRM (with the package's
# spike-and-slab extension for testing gamma > 0) to a small scouting set of
# IRW tables chosen to bracket the diagnostic: tables with known item_family
# (testlet) structure, tables suspected of speededness (candidates for the
# "stragglers cluster near the last items" pattern in the paper's discussion
# section), and a presumed-Rasch-like negative control.
#
# Output: vignettes/lsirmdata/lsirm_interaction_maps_results.rds
#
# Usage:
#   Rscript vignettes/lsirm_interaction_maps_compute.R          # scouting pass (fast MCMC settings)
#   Rscript vignettes/lsirm_interaction_maps_compute.R --full    # full MCMC settings

library(irw)
library(lsirm12pl)
library(mirt)
library(dplyr)
library(purrr)
library(furrr)
library(future)
library(tibble)

set.seed(20260722)

SCOUT <- !("--full" %in% commandArgs(trailingOnly = TRUE))

out_dir  <- "vignettes/lsirmdata"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Scouting-set table list
#
#    Original 5-table scouting pass (see task spec / vignette Motivation
#    section for detail):
#    - g308_sirt, chakraborty2026_IWAH_IRW: the only two IRW tables found (out
#      of ~800 checked for the local-dependence vignette) to carry a usable
#      item_family grouping -- i.e. ground truth on which items *should*
#      cluster if LSIRM is picking up real structure. g308_sirt is dichotomous
#      (1PL LSIRM); chakraborty2026_IWAH_IRW is graded (GRM LSIRM), included
#      as a second confirmatory case even though the task spec's item_family
#      shortlist criterion was dichotomous-only -- there was only one
#      dichotomous item_family table available.
#    - credentialform_lnirt, nature_relatedness: flagged "likely speeded" by
#      speededness_compute.R (branch speededness-vignette, cached results at
#      /tmp/speededness_results.rds during scouting -- see table_summary).
#      credentialform_lnirt is dichotomous but has 200 items, well above the
#      paper's tractable range (N~200-700, I~7-24), so items are downsampled
#      (see downsample_items() below) keeping the true last few positions so
#      the "stragglers near the end" diagnostic stays meaningful.
#      nature_relatedness is polytomous (8 categories) -- GRM LSIRM.
#    - blum_2018_imak_bin: no known item_family/testlet structure and
#      classified "likely power" (not speeded) by the same screen; dichotomous,
#      23 items, 317 participants -- a presumed-Rasch-like negative control,
#      sized well within the paper's tractable range.
#
#    Expansion (2026-07-23), after the scouting pass cleared the go/no-go bar
#    and raised a "what predicts a positive finding" question worth more data
#    points to check:
#    - brain_hemisphere, artistic_preferences, depression_anxiety_stress,
#      fisher_temperment, face_memory_test: the remaining 5 tables classified
#      "likely speeded" by the same speededness screen (completes that bucket
#      -- all 7 "likely speeded" tables from the original classification are
#      now included, not just the 2 that happened to be dichotomous/small).
#    - sd3ypl_klimczak_2019_ses, BPAQ_Christopher_2024_PSS10,
#      prpt_hellmann_2021_conscientiousness, chen2026_sc, disgust_berger2014,
#      autism_blotner_2025_s1_aq: a *blind* random sample (seed 20260723) from
#      irw_filter(n_items = c(5, 60), n_participants = c(100, 5000)), excluding
#      every table already in the study set -- deliberately not hand-picked
#      for any hypothesis, to see what typical, unremarkable IRW tables look
#      like under LSIRM without the selection bias of picking known-structure
#      or known-diagnostic tables.
#
#    16 tables total. Downsampling (below) keeps each table's MCMC cost
#    bounded regardless of raw size.

scouting_tables <- c(
  # original scouting pass
  "g308_sirt",
  "chakraborty2026_IWAH_IRW",
  "credentialform_lnirt",
  "nature_relatedness",
  "blum_2018_imak_bin",
  # remainder of the "likely speeded" bucket
  "brain_hemisphere",
  "artistic_preferences",
  "depression_anxiety_stress",
  "fisher_temperment",
  "face_memory_test",
  # blind random exploratory sample
  "sd3ypl_klimczak_2019_ses",
  "BPAQ_Christopher_2024_PSS10",
  "prpt_hellmann_2021_conscientiousness",
  "chen2026_sc",
  "disgust_berger2014",
  "autism_blotner_2025_s1_aq"
)

message("Study set: ", length(scouting_tables), " tables")

# ------------------------------------------------------------------------------
# 2. Helpers
# ------------------------------------------------------------------------------

MAX_RESP  <- 700   # paper's own examples top out around N~700
MAX_ITEMS <- 40    # paper's own examples top out around I~24; allow some headroom

# Mirrors find_order_col() in speededness_compute.R (branch speededness-vignette):
# looks for a within-person presentation-order column; falls back to
# first-appearance order if none is found.
find_order_col <- function(df) {
  order_col_candidates <- c("itemcov_order", "position", "item_position",
                             "order", "seq", "sequence")
  for (col in order_col_candidates) {
    if (!(col %in% names(df)) || all(is.na(df[[col]]))) next
    within_id_var <- df |>
      group_by(id) |>
      summarise(n_distinct_val = n_distinct(.data[[col]][!is.na(.data[[col]])]), .groups = "drop")
    if (mean(within_id_var$n_distinct_val > 1, na.rm = TRUE) > 0.5) return(col)
  }
  NULL
}

item_position_order <- function(df) {
  order_col <- find_order_col(df)
  if (!is.null(order_col)) {
    df <- df |>
      group_by(id) |>
      mutate(.rank = rank(.data[[order_col]], ties.method = "first")) |>
      ungroup()
    df |>
      group_by(item) |>
      summarise(position = median(.rank, na.rm = TRUE), .groups = "drop") |>
      arrange(position) |>
      mutate(position = row_number())
  } else {
    tibble(item = unique(df$item)) |> mutate(position = row_number())
  }
}

# Keep the true last `tail_n` items by position (for the "stragglers near the
# end" diagnostic), plus a systematic sample of the rest, up to `max_items`.
downsample_items <- function(item_cols, item_order_df, max_items, tail_n = 10) {
  if (length(item_cols) <= max_items) return(item_cols)
  ord <- item_order_df |> filter(item %in% item_cols) |> arrange(position)
  tail_items <- tail(ord$item, tail_n)
  rest       <- setdiff(ord$item, tail_items)
  n_rest     <- max_items - length(tail_items)
  keep_rest  <- rest[round(seq(1, length(rest), length.out = n_rest))]
  union(keep_rest, tail_items)
}

# ------------------------------------------------------------------------------
# 3. Fit function: fetch -> build response matrix -> fit LSIRM (1PL or GRM,
#    spike-and-slab) -> extract positions/gamma/inclusion probability
# ------------------------------------------------------------------------------

fit_lsirm <- function(table_name, scout = SCOUT) {
  message("  Fitting: ", table_name, if (scout) " [scout]" else " [full]")

  long <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    long fetch failed: ", conditionMessage(e)); NULL
  })
  if (is.null(long)) return(NULL)
  long <- long |> mutate(id = as.character(id), item = as.character(item))

  # item_family grouping, if present (see local_dependence_compute.R)
  item_family_map <- NULL
  if ("item_family" %in% names(long)) {
    fam <- long |> distinct(item, item_family) |> filter(!is.na(item_family))
    fam_counts <- fam |> count(item_family)
    if (any(fam_counts$n >= 2)) {
      item_family_map <- fam |> semi_join(filter(fam_counts, n >= 2), by = "item_family")
    }
  }

  item_order_df <- item_position_order(long)

  wide <- tryCatch(irw_fetch(table_name, resp = TRUE), error = function(e) {
    message("    wide fetch failed: ", conditionMessage(e)); NULL
  })
  if (is.null(wide)) return(NULL)

  ids <- as.character(wide$id)
  mat <- as.matrix(wide[, setdiff(names(wide), "id"), drop = FALSE])
  rownames(mat) <- ids
  colnames(mat) <- sub("^item_", "", colnames(mat))
  storage.mode(mat) <- "numeric"

  # Downsample respondents
  if (nrow(mat) > MAX_RESP) {
    message("    downsampling respondents from ", nrow(mat), " to ", MAX_RESP)
    mat <- mat[sample(nrow(mat), MAX_RESP), , drop = FALSE]
  }

  # Downsample items, preserving true last-position items
  if (ncol(mat) > MAX_ITEMS) {
    keep <- downsample_items(colnames(mat), item_order_df, MAX_ITEMS)
    message("    downsampling items from ", ncol(mat), " to ", length(keep))
    mat <- mat[, keep, drop = FALSE]
  }

  # Drop zero-variance items
  mat <- mat[, apply(mat, 2, function(x) length(unique(na.omit(x))) > 1), drop = FALSE]
  if (ncol(mat) < 5 || nrow(mat) < 50) {
    message("    skipped: too few usable items/respondents after cleaning")
    return(NULL)
  }

  n_categories <- length(unique(na.omit(as.vector(mat))))
  is_dichotomous <- n_categories == 2

  mcmc_args <- if (scout) {
    list(niter = 3000, nburn = 1000, nthin = 2)
  } else {
    list(niter = 15000, nburn = 2500, nthin = 5)
  }

  fit <- tryCatch({
    if (is_dichotomous) {
      do.call(lsirm1pl_ss, c(list(data = mat, ndim = 2, verbose = FALSE), mcmc_args))
    } else {
      do.call(lsirmgrm_ss, c(list(data = mat, ndim = 2, verbose = FALSE), mcmc_args))
    }
  }, error = function(e) {
    message("    lsirm fit failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(fit)) return(NULL)

  z <- fit$z_estimate
  w <- fit$w_estimate
  rownames(z) <- rownames(mat)
  rownames(w) <- colnames(mat)

  item_order_df <- item_order_df |> filter(item %in% colnames(mat))

  list(
    table            = table_name,
    is_dichotomous    = is_dichotomous,
    n_categories     = n_categories,
    n_items          = ncol(mat),
    n_participants   = nrow(mat),
    gamma_estimate   = fit$gamma_estimate,
    gamma_samples    = fit$gamma,
    pi_estimate      = fit$pi_estimate,
    bic              = fit$bic,
    z_estimate       = z,
    w_estimate       = w,
    item_order       = item_order_df,
    item_family      = item_family_map,
    scout            = scout
  )
}

# ------------------------------------------------------------------------------
# 4. Run in parallel, writing each result to disk as it completes
#    If the script crashes, re-running skips already-completed tables.
#    Scout and full-run outputs are cached separately (suffix) so re-running
#    with --full doesn't skip on a stale scout cache.
# ------------------------------------------------------------------------------

fit_to_disk <- function(table_name) {
  suffix   <- if (SCOUT) "_scout" else "_full"
  out_file <- file.path(fits_dir, paste0(table_name, suffix, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
  }
  result <- fit_lsirm(table_name)
  if (!is.null(result)) saveRDS(result, out_file)
}

plan(multisession, workers = min(8, parallel::detectCores() %/% 2))
message("\nFitting ", length(scouting_tables), " tables (", if (SCOUT) "scout" else "full", " settings)...")
future_map(scouting_tables, fit_to_disk)
plan(sequential)

# ------------------------------------------------------------------------------
# 5. Combine results
# ------------------------------------------------------------------------------

suffix <- if (SCOUT) "_scout" else "_full"
all_raw <- map(scouting_tables, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, suffix, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

message("\nDone. ", length(all_raw), " of ", length(scouting_tables), " tables produced usable fits.")

# ------------------------------------------------------------------------------
# 4.5. Null-calibration check
#
#    Every polytomous/graded table in the study set came back with a
#    decisively high inclusion probability for gamma > 0 (>0.96 across all
#    13), while the 3 dichotomous tables split as expected (2 null, 1
#    strong). Before trusting 13 "positive" findings at face value, check
#    whether lsirmgrm_ss() is simply biased toward gamma > 0 on well-behaved
#    graded-response data with *no* true interaction structure: simulate a
#    clean single-factor GRM dataset (realistic discrimination and
#    thresholds, N/I matched to the study set, gamma = 0 by construction)
#    and confirm the estimator recovers a low inclusion probability.
# ------------------------------------------------------------------------------

null_calibration_file <- file.path(fits_dir, paste0("null_calibration_check", suffix, ".rds"))
if (!file.exists(null_calibration_file)) {
  message("\nRunning null-calibration check (simulated GRM data, no true interaction)...")
  set.seed(1)
  a <- matrix(rlnorm(20, 0, 0.3), ncol = 1)
  d <- matrix(t(apply(matrix(rnorm(20 * 4), ncol = 4), 1, function(x) sort(x, decreasing = TRUE))), ncol = 4)
  sim_dat <- simdata(a, d, N = 700, itemtype = "graded")
  storage.mode(sim_dat) <- "numeric"

  mcmc_args <- if (SCOUT) list(niter = 3000, nburn = 1000, nthin = 2) else list(niter = 15000, nburn = 2500, nthin = 5)
  sim_fit <- do.call(lsirmgrm_ss, c(list(data = sim_dat, ndim = 2, verbose = FALSE), mcmc_args))

  saveRDS(list(pi_estimate = sim_fit$pi_estimate, gamma_mean = mean(sim_fit$gamma_estimate)),
          null_calibration_file)
}
null_calibration <- readRDS(null_calibration_file)
message("Null-calibration check: pi_estimate = ", round(null_calibration$pi_estimate, 4),
        ", gamma_mean = ", round(null_calibration$gamma_mean, 4))

summary_df <- map_dfr(all_raw, function(r) {
  gamma_ci <- quantile(r$gamma_samples, c(0.025, 0.975), na.rm = TRUE)
  tibble(
    table          = r$table,
    is_dichotomous = r$is_dichotomous,
    n_categories   = r$n_categories,
    n_items        = r$n_items,
    n_participants = r$n_participants,
    gamma_mean     = mean(r$gamma_estimate),
    gamma_lo       = gamma_ci[[1]],
    gamma_hi       = gamma_ci[[2]],
    pi_estimate    = r$pi_estimate,
    bic            = r$bic
  )
})

print(summary_df)

positions        <- set_names(map(all_raw, function(r) list(z = r$z_estimate, w = r$w_estimate)),
                               map_chr(all_raw, "table"))
item_order_list  <- set_names(map(all_raw, "item_order"), map_chr(all_raw, "table"))
item_family_list <- set_names(map(all_raw, "item_family"), map_chr(all_raw, "table")) |> compact()

# ------------------------------------------------------------------------------
# 6. Save combined output
# ------------------------------------------------------------------------------

saveRDS(
  list(
    summary          = summary_df,
    positions        = positions,
    item_order       = item_order_list,
    item_family      = item_family_list,
    scouting_tables  = scouting_tables,
    scout            = SCOUT,
    null_calibration = null_calibration,
    date_run         = Sys.Date(),
    session          = sessionInfo()
  ),
  file = file.path(out_dir, paste0("lsirm_interaction_maps_results", suffix, ".rds"))
)

message("Saved to ", out_dir, "/lsirm_interaction_maps_results", suffix, ".rds")

# ------------------------------------------------------------------------------
# 7. Generate citations
#    irw_save_bibtex() takes the full vector of table names in one call and
#    writes/overwrites output_file (no append argument in the current API).
# ------------------------------------------------------------------------------

tryCatch(
  irw_save_bibtex(scouting_tables, output_file = file.path(out_dir, "irw_references.bib")),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)

message("Citations saved to ", out_dir, "/irw_references.bib")
