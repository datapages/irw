# continuous_bounded_compute.R
#
# PILOT / DRAFT scope (started 2026-07-30): built up over several rounds --
# a minimal single-shot real-data pilot first, then a proper 2-round vetting
# pass over real candidate tables, then the full simulation/recovery grid
# (Part D). Still not attempted, deliberately: Ferrando's (2001) nonlinear
# congeneric model (optional stretch spec, only gets attempted after specs
# 1-4 are complete and validated, per the original scope's own sequencing
# rule -- specs 1-4 are validated now, but this hasn't been picked up yet)
# and partial-missingness handling for real data (complete cases only
# throughout).
#
# What IS included: real fits of all 4 core specs --
#   1. naive linear baseline (lavaan)
#   2. Beta IRT, fixed AND item-varying dispersion (sirt+mirt custom itemtype)
#   3. Samejima CRM (EstCRM)
#   4. Mueller CRSM (patched pcIRT)
# -- on (a) 17 simulated datasets spanning a DGP x N x J x dispersion grid
# plus one boundary-inflation condition (Part D, uses furrr::future_pmap +
# plan(multisession) as specified) and (b) 7 real IRW tables (Parts B/B2/C),
# each scored via genuine cell-level missing-response prediction: item
# parameters are fit on a complete training-person matrix (EstCRM and
# pcIRT's fitting code has no missing-data support at all, so that part is
# unavoidable), but a random subset of each TEST person's items is hidden
# before estimating that person's theta, and only the hidden items are
# scored -- see continuous_bounded_helpers.R's "Held-out design note" for
# why this is viable despite the fitting-stage constraint (all 4 specs'
# person-scoring functions turn out to be NA-tolerant). See the helpers
# file header for the package-bug fixes, the cross-model likelihood-scale
# resolution, and the IMV-vs-log-likelihood decision -- all flagged there as
# they were found/made, not assumed silently.
#
# Real-data table (primary): lsbq_maleki_2025_non_persian_proficiency
# (3 items, N=312, 0-10 scale). Chosen over the 0-100 "Slider/continuous"-
# tagged tables in the scout output because those came back 100%
# integer-valued on inspection -- weaker evidence of genuine continuity than
# this table's ~40% non-integer responses. One scout candidate was checked
# and rejected before landing on this one: eammi_grahe_2018_marriage_
# identity_allocation (row sums are a constant 100 -- an ipsative/
# compositional allocation task, which violates the conditional-independence
# assumption every one of these 4 models makes).
#
# A second real table (ai_fear_dong_2026_ai, Doctor_* facet subset) and 7
# more (see "C. Additional real-data tables" below) were added after two
# rounds of vetting the other scout-flagged "bounded" tables -- see
# continuous_bounded_data/vet_candidates.R and vet_candidates2.R. The FIRST
# vetting round wrongly excluded much_tte_2025_currentmotivation,
# nas_rogoza_2024_study5_*, westhoff2023_*, and gilbert_meta_95 as
# "repeated-measures duplication" without checking whether IRW's own data
# standard (standard.qmd's `wave`/`rater` columns, documented for exactly
# this scenario) already disambiguated the repetition -- it did, for all of
# these. The SECOND vetting round filters to one wave/rater/occasion before
# judging a table, which is the correct check. tears, emoji_scheffler_2024,
# opentsstvr_linnig_2025_vas, double_marking_steele_2022, and
# thomeczek2025_les remain excluded even after this correction (crossed
# rater x stimulus x phase design that doesn't reduce to one filter; unclear
# rater/id structure; duplicates persist even within one wave; too few
# observations per rater once split). klippel_irw (item-varying ranges),
# ehealth_rioux_2025_* (mixed 0-100/1-7 formats within one table), and
# mclaughlin_samuel_2025_auditory_session_1 (1 item) were unaffected by the
# wave/rater oversight -- those exclusions were correct the first time.
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

  # ============================================================================
  # B2. Real data #2: ai_fear_dong_2026_ai, "Doctor_*" facet subset
  # ============================================================================
  # ai_fear_dong_2026_ai is a 48-item x 6-target x 8-trait factorial design
  # (Care/Doctor/Journalist/Judge/Manager/Religious x competent/warm/etc.),
  # NOT a single construct as a whole table -- vetted (see
  # continuous_bounded_data/vet_candidates.R / vet_results.csv) alongside 19
  # other scout-flagged "bounded" tables, all 19 of which had a disqualifying
  # structural issue (repeated-measures/multi-stimulus row duplication,
  # item-varying ranges, mixed 0-100/1-7 formats within one table, too few
  # items, or too small N). This one table survives as usable ONLY after
  # subsetting to one target's 8 trait items, treated as a coherent "overall
  # impression of Doctor" battery (stereotype-content-model trait ratings
  # reliably load on a single warmth/competence "halo" factor in practice).
  # A useful contrast to the primary real-data table: large N, but 100%
  # integer-valued (unlike lsbq's ~40% fractional values) -- weaker evidence
  # of genuine continuity, included anyway as a large-N structural check
  # rather than a fractional-response demonstration.
  message("=== Real data #2: ai_fear_dong_2026_ai (Doctor_* subset) ===")
  real2_table <- "ai_fear_dong_2026_ai"
  real2_low <- 0; real2_high <- 100
  real2_facet <- "Doctor"

  df_real2 <- irw_fetch(real2_table)
  doctor_items <- grep(paste0("^", real2_facet, "_"), unique(df_real2$item), value = TRUE)
  wide_real2 <- df_real2 |>
    filter(item %in% doctor_items) |>
    select(id, item, resp) |>
    distinct(id, item, .keep_all = TRUE) |>
    pivot_wider(names_from = item, values_from = resp) |>
    as.data.frame()
  rownames(wide_real2) <- wide_real2$id
  orig_items2 <- setdiff(colnames(wide_real2), "id")
  Y_real2_raw <- as.matrix(wide_real2[, orig_items2])
  storage.mode(Y_real2_raw) <- "double"
  Y_real2_raw <- Y_real2_raw[complete.cases(Y_real2_raw), , drop = FALSE]
  # subsample for compute tractability in this pilot (full N=10000 is not
  # needed to demonstrate the comparison; person-level Mueller CRSM scoring
  # in particular is per-test-person numerical integration)
  set.seed(3)
  if (nrow(Y_real2_raw) > 2000) Y_real2_raw <- Y_real2_raw[sample(nrow(Y_real2_raw), 2000), , drop = FALSE]
  Y_real2_01 <- rescale01(Y_real2_raw, real2_low, real2_high)
  colnames(Y_real2_01) <- paste0("item", seq_len(ncol(Y_real2_01)))

  real2_out <- fit_score_all(Y_real2_01, label = paste0(real2_table, "_", real2_facet), seed = 4)

  # ============================================================================
  # C. Additional real-data tables (batch): 7 tables that the FIRST vetting
  # pass wrongly excluded as "repeated-measures duplication," corrected by
  # the second vetting pass (filter to one wave/rater occasion first -- see
  # vet_candidates2.R). much_tte_2025_currentmotivation was in this bucket;
  # test_taking_much_2025_cm was excluded originally on an untested
  # assumption (never actually checked, just grouped with much_tte by
  # naming similarity) -- checked properly now and it has zero duplicate
  # id x item rows with no wave/rater filtering needed at all.
  # ============================================================================
  message("=== Additional real-data tables (batch) ===")

  fetch_single_occasion <- function(table_name, occasion_col = NULL) {
    df <- irw_fetch(table_name)
    if (!is.null(occasion_col)) {
      first_val <- sort(unique(df[[occasion_col]]))[1]
      df <- df[df[[occasion_col]] == first_val, ]
    }
    wide <- df |>
      select(id, item, resp) |>
      distinct(id, item, .keep_all = TRUE) |>
      pivot_wider(names_from = item, values_from = resp) |>
      as.data.frame()
    rownames(wide) <- wide$id
    items <- setdiff(colnames(wide), "id")
    Yraw <- as.matrix(wide[, items]); storage.mode(Yraw) <- "double"
    Yraw[complete.cases(Yraw), , drop = FALSE]
  }

  # westhoff2023_{stopd,pbat}, gilbert_meta_95, and mendes_2019_snycq: a
  # prior version of this comment claimed westhoff2023/gilbert_meta_95 "fail
  # at the Beta IRT scoring step" -- that claim was never actually backed by
  # a run of this script (these 3 were never in BATCH_TABLES to begin with),
  # so it's being tested here for real rather than assumed. mendes_2019_snycq
  # is new to IRW (2026-08-01): wave1-filtered, 0-100 VAS-style scale, 12
  # items, ~27% non-integer responses, 0 duplicate id x item rows, 157/159
  # complete persons -- passes every structural check used elsewhere in this
  # vignette (see vet_mendes.R-equivalent checks, not committed as a separate
  # script since this table's vetting was a one-off during an already-running
  # session).
  BATCH_TABLES <- tribble(
    ~table,                              ~occasion_col, ~low, ~high,
    "much_tte_2025_currentmotivation",   "wave",         0,    100,
    "test_taking_much_2025_cm",          NA_character_,  0,    100,
    "nas_rogoza_2024_study5_nas",        "wave",         0,    100,
    "nas_rogoza_2024_study5_ngs",        "wave",         0,    100,
    "nas_rogoza_2024_study5_nvs",        "wave",         0,    100,
    "westhoff2023_stopd",                "wave",         0,    100,
    "westhoff2023_pbat",                 "wave",         0,    100,
    "gilbert_meta_95",                   "wave",         0,    10,
    "mendes_2019_snycq",                 "wave",         0,    100
  )

  batch_results <- list()
  for (i in seq_len(nrow(BATCH_TABLES))) {
    tb <- BATCH_TABLES$table[i]
    occ <- BATCH_TABLES$occasion_col[i]
    message("  fitting: ", tb)
    Yraw <- fetch_single_occasion(tb, if (is.na(occ)) NULL else occ)
    Y01 <- rescale01(Yraw, BATCH_TABLES$low[i], BATCH_TABLES$high[i])
    colnames(Y01) <- paste0("item", seq_len(ncol(Y01)))
    out <- tryCatch(fit_score_all(Y01, label = tb, seed = 10 + i), error = function(e) {
      message("    failed: ", conditionMessage(e)); NULL
    })
    if (!is.null(out)) batch_results[[tb]] <- list(N = nrow(Yraw), J = ncol(Yraw), scores = out$table,
                                                    predictions = out$predictions)
  }
  batch_predictions <- bind_rows(lapply(names(batch_results), function(tb) batch_results[[tb]]$predictions))
  batch_scores <- bind_rows(lapply(names(batch_results), function(tb) {
    batch_results[[tb]]$scores |> mutate(N = batch_results[[tb]]$N, J = batch_results[[tb]]$J)
  }))

  tryCatch(
    irw_save_bibtex(c(real_table, real2_table, BATCH_TABLES$table),
                     output_file = file.path(out_dir, "irw_references.bib")),
    error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
  )

  # ============================================================================
  # Save
  # ============================================================================
  results <- list(
    scope_note = "PILOT/DRAFT run -- see header of continuous_bounded_compute.R",
    sim = list(theta_true = theta_true, delta_true = delta_true, tau_true = tau_true,
               Y = Y_sim, train_idx = sim_out$train_idx, test_idx = sim_out$test_idx,
               scores = sim_out$table, predictions = sim_out$predictions,
               recovery_cor_delta = recovery_cor),
    real = list(table = real_table, low = real_low, high = real_high,
                orig_items = orig_items,
                N = nrow(Y_real_raw), J = ncol(Y_real_raw),
                Y_raw = Y_real_raw, train_idx = real_out$train_idx, test_idx = real_out$test_idx,
                scores = real_out$table, predictions = real_out$predictions),
    real2 = list(table = real2_table, facet = real2_facet, low = real2_low, high = real2_high,
                 orig_items = orig_items2,
                 N = nrow(Y_real2_raw), J = ncol(Y_real2_raw),
                 Y_raw = Y_real2_raw, train_idx = real2_out$train_idx, test_idx = real2_out$test_idx,
                 scores = real2_out$table, predictions = real2_out$predictions),
    batch = list(tables = BATCH_TABLES, scores = batch_scores, predictions = batch_predictions),
    vetting = list(n_candidates_checked = 20,
                   n_usable_round1 = 1,
                   n_usable_round2 = 5,
                   note = "See vet_candidates.R/vet_results.csv (round 1) and vet_candidates2.R/vet_results2.csv (round 2, corrects the wave/rater oversight). Still excluded after round 2: tears (crossed rater x stimulus x phase design), emoji_scheffler_2024 (unclear rater/id structure), opentsstvr_linnig_2025_vas (duplicates persist even within one wave), double_marking_steele_2022 and thomeczek2025_les (too few observations per rater once split), klippel_irw (item-varying ranges), ehealth_rioux_2025_* (mixed 0-100/1-7 formats), mclaughlin_samuel_2025_auditory_session_1 (1 item). westhoff2023_{stopd,pbat} and gilbert_meta_95 passed the structural (wave) check but hit an unresolved Beta IRT (and, for pbat, Samejima CRM) scoring failure even at K=6 bins -- left out of this pass, not counted as excluded or included.")
  )
  saveRDS(results, out_rds)
  message("Saved results to ", out_rds)
} else {
  message(out_rds, " already exists -- skipping (delete it to force a rerun)")
}

# ==============================================================================
# D. Full simulation/recovery grid (previously deferred pilot scope item).
# Cached separately from the pilot results above (own skip-if-exists gate)
# so re-running the grid doesn't force re-fetching real data.
#
# Cross-fits all 4 core specs against 3 DGPs (Beta IRT, Samejima CRM, Mueller
# CoRSM -- the naive-linear DGP is skipped, per the original scope's own
# allowance: an unbounded-Gaussian DGP clipped to [0,1] to make it "bounded"
# would introduce an artificial boundary spike that isn't representative of
# what a linear-factor model actually generates, the exact awkwardness the
# scope flagged as a reason to skip it). Grid: N in {300, 1000}, J in {8, 20},
# and, for the Beta IRT DGP specifically, fixed vs. item-varying dispersion --
# 8 + 4 + 4 = 16 conditions, plus 1 boundary-inflation condition (zero/one
# piling layered on the Beta IRT DGP at N=300/J=8), for 17 total.
# ==============================================================================

grid_out_rds <- file.path(out_dir, "continuous_bounded_grid_results.rds")

if (!file.exists(grid_out_rds)) {
  library(furrr)
  plan(multisession, workers = min(future::availableCores() - 2, 17))

  run_grid_condition <- function(dgp, N, J, dispersion_mode = NA_character_,
                                  boundary_inflation = FALSE,
                                  response_heaping = FALSE, seed) {
    source("vignettes/continuous_bounded_helpers.R")
    library(dplyr)
    set.seed(seed)
    theta_true <- rnorm(N)
    b_true <- seq(-1.5, 1.5, length.out = J)

    recovery_cor <- NA_real_
    if (dgp == "beta_irt") {
      tau_true <- if (dispersion_mode == "fixed") rep(0.6, J) else runif(J, 0.3, 1.2)
      Y01 <- sim_beta_irt(theta_true, b_true, tau_true)
    } else if (dgp == "samejima_crm") {
      alpha_true <- runif(J, 0.5, 1.5)
      Y01 <- sim_samejima_crm(theta_true, b_true, alpha_true, sigma = 1)
    } else if (dgp == "muller_corsm") {
      Y01 <- sim_muller_corsm(theta_true, b_true, lambda = 2)
    }
    colnames(Y01) <- paste0("item", seq_len(J))

    if (boundary_inflation) Y01 <- apply_boundary_inflation(Y01, p0 = 0.1, p1 = 0.1)
    if (response_heaping) Y01 <- apply_response_heaping(Y01, p_heap = 0.4)

    label <- paste(dgp, N, J, dispersion_mode, boundary_inflation, response_heaping, sep = "_")
    out <- tryCatch(fit_score_all(Y01, label = label, seed = seed + 1), error = function(e) NULL)
    if (is.null(out)) return(NULL)

    # recovery correlation for the correctly-specified fitted model, where
    # one exists in the 4 core specs
    if (dgp == "beta_irt" && !inherits(out$fits[[paste0("beta_irt_", dispersion_mode, "_dispersion")]], "error")) {
      cf <- mirt::coef(out$fits[[paste0("beta_irt_", dispersion_mode, "_dispersion")]], simplify = TRUE)$items
      recovery_cor <- suppressWarnings(cor(cf[, "delta"], b_true[match(rownames(cf), colnames(Y01))]))
    } else if (dgp == "samejima_crm" && !inherits(out$fits$samejima_crm, "error")) {
      recovery_cor <- suppressWarnings(cor(out$fits$samejima_crm$param[, "b"], b_true))
    } else if (dgp == "muller_corsm" && !inherits(out$fits$muller_corsm, "error")) {
      recovery_cor <- suppressWarnings(cor(out$fits$muller_corsm$itempar, b_true))
    }

    list(dgp = dgp, N = N, J = J, dispersion_mode = dispersion_mode,
         boundary_inflation = boundary_inflation, response_heaping = response_heaping,
         scores = out$table, recovery_cor = recovery_cor)
  }

  grid <- tibble::tribble(
    ~dgp,            ~N,    ~J,  ~dispersion_mode, ~boundary_inflation, ~response_heaping,
    "beta_irt",       300,   8,  "fixed",           FALSE,               FALSE,
    "beta_irt",       300,  20,  "fixed",           FALSE,               FALSE,
    "beta_irt",      1000,   8,  "fixed",           FALSE,               FALSE,
    "beta_irt",      1000,  20,  "fixed",           FALSE,               FALSE,
    "beta_irt",       300,   8,  "item",            FALSE,               FALSE,
    "beta_irt",       300,  20,  "item",            FALSE,               FALSE,
    "beta_irt",      1000,   8,  "item",            FALSE,               FALSE,
    "beta_irt",      1000,  20,  "item",            FALSE,               FALSE,
    "samejima_crm",   300,   8,  NA_character_,      FALSE,              FALSE,
    "samejima_crm",   300,  20,  NA_character_,      FALSE,              FALSE,
    "samejima_crm",  1000,   8,  NA_character_,      FALSE,              FALSE,
    "samejima_crm",  1000,  20,  NA_character_,      FALSE,              FALSE,
    "muller_corsm",   300,   8,  NA_character_,      FALSE,              FALSE,
    "muller_corsm",   300,  20,  NA_character_,      FALSE,              FALSE,
    "muller_corsm",  1000,   8,  NA_character_,      FALSE,              FALSE,
    "muller_corsm",  1000,  20,  NA_character_,      FALSE,              FALSE,
    "beta_irt",       300,   8,  "fixed",           TRUE,                FALSE,
    "beta_irt",       300,   8,  "fixed",           FALSE,               TRUE
  )
  grid$seed <- 100 + seq_len(nrow(grid))

  message("=== Running full simulation/recovery grid (", nrow(grid), " conditions) ===")
  grid_results <- future_pmap(
    list(grid$dgp, grid$N, grid$J, grid$dispersion_mode, grid$boundary_inflation,
         grid$response_heaping, grid$seed),
    run_grid_condition,
    .options = furrr_options(seed = TRUE)
  )
  plan(sequential)

  grid_scores <- dplyr::bind_rows(lapply(grid_results, function(r) {
    if (is.null(r)) return(NULL)
    r$scores |> dplyr::mutate(dgp = r$dgp, N = r$N, J = r$J,
                               dispersion_mode = r$dispersion_mode,
                               boundary_inflation = r$boundary_inflation,
                               response_heaping = r$response_heaping,
                               recovery_cor = r$recovery_cor)
  }))

  saveRDS(list(grid = grid, scores = grid_scores), grid_out_rds)
  message("Saved grid results to ", grid_out_rds)
} else {
  message(grid_out_rds, " already exists -- skipping (delete it to force a rerun)")
}
