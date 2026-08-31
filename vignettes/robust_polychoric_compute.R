# robust_polychoric_compute.R
#
# Does the robust "C-estimator" for polychoric correlation (Welz, Mair & Alfons,
# Psychometrika) disagree with maximum likelihood on real IRW data -- and if so,
# where?
#
# Why the comparison is informative, in the method author's own framing: the
# robust estimator is designed for data where the modelling assumptions fail for
# SOME observations (careless responding, bot responses, an attentive but
# heterogeneous subgroup, or any process that violates latent normality). If the
# model is correctly specified for ALL observations, the robust estimator is
# asymptotically equivalent to ML. So agreement is the null and DIVERGENCE IS THE
# SIGNAL: it flags item pairs whose contingency table is not well described by a
# single underlying bivariate normal. That makes this a screen for assumption
# violation across IRW, not a contest between two estimators.
#
# Follows the analysis pattern in the paper's own replication material:
#   https://github.com/mwelz/robust-polycor-replication
#   applications/arias2020/arias2020_analyze.R
# which uses polycormat_mle()/polycormat() for scale-level correlation matrices
# and polycor() on a single pair for the cell-level residual diagnostic.
#
# ---------------------------------------------------------------------------
# Feasibility, established by benchmark 2026-08-30 (see NOTES below the header):
#
# robcat's cost is driven overwhelmingly by CATEGORY COUNT, not sample size:
#
#     categories     robust      psych::polychoric (ML)     ratio
#          3         0.18 s            0.012 s               15x
#          4         0.59 s            0.014 s               42x
#          5         1.11 s            0.020 s               56x
#          7        50.37 s            0.040 s             1259x
#
# An earlier smoke test (2026-07-27) declared the whole vignette infeasible after
# timing a 7-category table -- the worst case in the range, ~45x more expensive
# than 5 categories. Restricting to <= MAX_CATEGORIES makes the scan tractable.
#
# Two further benchmark results shape the design below:
#   * Sample size barely matters, and SMALL samples are WORSE: at 5 categories,
#     N=500 took 8.4 s vs ~1.3 s at N=1000/2000/5000. Subsampling respondents
#     makes the estimator slower and less stable, so this script uses full N.
#   * variance = TRUE is free for a single pair (1.31 s vs 1.30 s), so the
#     deep-dive keeps standard errors. It is NOT free for a whole matrix (that
#     computes the full asymptotic covariance), so the scan uses variance=FALSE,
#     matching the replication script.
# ---------------------------------------------------------------------------
#
# Output: vignettes/robust_polychoricdata/robust_polychoric_results.rds
#         vignettes/robust_polychoricdata/fits/<table>.rds
#
# Usage:
#   Rscript vignettes/robust_polychoric_compute.R          # pilot (few tables)
#   PILOT=FALSE Rscript vignettes/robust_polychoric_compute.R   # full run

suppressPackageStartupMessages({
  library(irw)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(psych)
  library(robcat)
})

set.seed(20260830)

out_dir  <- "vignettes/robust_polychoricdata"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

PILOT          <- !identical(toupper(Sys.getenv("PILOT", "TRUE")), "FALSE")
PILOT_N_TABLES <- 3

MIN_CATEGORIES <- 3     # binary items are tetrachoric, out of scope
MAX_CATEGORIES <- 5     # the feasibility cliff: 7 categories costs ~45x more
MIN_ITEMS      <- 6
ITEMS_CAP      <- 15    # items sampled per table -> at most 105 pairs
TUNING_C       <- 0.6   # robcat default, and the value used in the paper

# Full N is always used -- never subsample respondents. The floor below is
# high for a reason that is the opposite of the usual one: SMALL samples are
# EXPENSIVE here, not cheap. Benchmark at 5 categories gave 8.4 s/pair at
# N=500 against ~1.3 s/pair at N=1000/2000/5000, and a first pilot confirmed
# it on real data -- a 751-respondent table was still running after 21 minutes
# while a 3150-respondent table of the same shape finished in 4. The estimator
# has trouble converging when cells are sparse, so tables just above 500 cost
# far more than large ones and give less stable estimates besides.
MIN_RESPONDENTS <- 1000

# The candidate pool is ~575 tables; at ~4 min/table a full sweep is ~38 hours.
# Sample instead, and say so on the page.
MAX_TABLES     <- 40

# Hard ceiling per table so one pathological table cannot stall the run. A
# table that trips this is recorded as a timeout rather than silently dropped.
TABLE_TIMEOUT_SECS <- 900

# Conservative by design: this repo's other compute scripts use
# plan(multisession, workers = min(4, detectCores() %/% 2)), but the whole run
# is feasible serially (~1.8 s/pair, ~3 min/table) and a long job should leave
# the machine usable. If you do parallelise, do it at ONE level only --
# future_map over tables AND polycormat(parallel = TRUE) would multiply.
N_WORKERS <- 1L

# ==============================================================================
# 1. Select tables
# ==============================================================================

all_candidates <- irw_filter(
  n_categories   = c(MIN_CATEGORIES, MAX_CATEGORIES),
  n_items        = c(MIN_ITEMS, Inf),
  n_participants = c(MIN_RESPONDENTS, Inf)
)

message("Candidate tables (", MIN_CATEGORIES, "-", MAX_CATEGORIES,
        " categories, >=", MIN_ITEMS, " items, >=", MIN_RESPONDENTS,
        " respondents): ", length(all_candidates))

n_take <- if (PILOT) PILOT_N_TABLES else MAX_TABLES
tables <- if (length(all_candidates) > n_take) {
  sample(all_candidates, n_take)
} else {
  all_candidates
}
message(if (PILOT) "PILOT run: " else "FULL run: ", length(tables), " of ",
        length(all_candidates), " candidates.")

tags_meta <- tryCatch(irw_tags(tables = tables), error = function(e) NULL)

# ==============================================================================
# 2. Fetch + shape
# ==============================================================================

fetch_wide <- function(table_name) {
  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e)); NULL
  })
  if (is.null(df) || !all(c("id", "item", "resp") %in% names(df))) return(NULL)
  df |>
    select(id, item, resp) |>
    distinct(id, item, .keep_all = TRUE) |>
    pivot_wider(names_from = item, values_from = resp)
}

# ==============================================================================
# 3. Per-table scan
#
# Both estimators are run on the SAME complete-case matrix so any difference is
# the estimator and not the data. Categories are recoded to consecutive integers
# starting at 1, which robcat expects; items whose observed category count falls
# outside [MIN_CATEGORIES, MAX_CATEGORIES] after complete-casing are dropped.
# ==============================================================================

analyze_table <- function(table_name, wide) {
  if (is.null(wide) || !"id" %in% names(wide)) return(NULL)

  item_cols <- setdiff(names(wide), "id")
  mat <- as.matrix(wide[, item_cols, drop = FALSE])
  storage.mode(mat) <- "numeric"
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  if (nrow(mat) < MIN_RESPONDENTS) {
    message("    skipped: ", nrow(mat), " complete rows < ", MIN_RESPONDENTS)
    return(NULL)
  }

  # Per-item category counts on the complete-case matrix
  k_obs <- apply(mat, 2, function(v) length(unique(v)))
  keep  <- k_obs >= MIN_CATEGORIES & k_obs <= MAX_CATEGORIES
  if (sum(keep) < MIN_ITEMS) {
    message("    skipped: ", sum(keep), " items in the ", MIN_CATEGORIES, "-",
            MAX_CATEGORIES, " category range")
    return(NULL)
  }
  mat <- mat[, keep, drop = FALSE]

  if (ncol(mat) > ITEMS_CAP) {
    mat <- mat[, sort(sample(ncol(mat), ITEMS_CAP)), drop = FALSE]
  }

  # robcat wants consecutive integer codes starting at 1
  mat <- apply(mat, 2, function(v) as.integer(factor(v, levels = sort(unique(v)))))
  items  <- colnames(mat)
  n_pair <- choose(ncol(mat), 2)

  message("    ", ncol(mat), " items x ", nrow(mat), " respondents -> ",
          n_pair, " pairs; categories ",
          paste(range(apply(mat, 2, function(v) length(unique(v)))), collapse = "-"))

  # Loop item pairs in R rather than calling polycormat() on the whole matrix.
  # polycormat() is the natural call and is what the paper's replication script
  # uses, but it stays inside compiled code for the entire matrix, so an R-level
  # time limit cannot interrupt it: a first run had one table burn 6869 s
  # against a 900 s setTimeLimit() because the limit could only fire once
  # polycormat() finally returned. Looping here costs a little R overhead and
  # buys a timeout that actually bounds the table, per-pair timings, and partial
  # results from a table that runs long instead of losing all of its pairs.
  combos <- utils::combn(ncol(mat), 2)
  k_obs2 <- apply(mat, 2, function(v) length(unique(v)))
  t0 <- Sys.time()
  timed_out <- FALSE
  rows <- vector("list", ncol(combos))

  for (j in seq_len(ncol(combos))) {
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > TABLE_TIMEOUT_SECS) {
      timed_out <- TRUE
      message("    timeout after ", j - 1L, " of ", ncol(combos), " pairs")
      break
    }
    a <- combos[1, j]; b <- combos[2, j]
    x <- mat[, a]; y <- mat[, b]
    p0 <- Sys.time()
    fm <- tryCatch(polycor_mle(x, y, variance = FALSE), error = function(e) NULL)
    fr <- tryCatch(polycor(x, y, c = TUNING_C, variance = FALSE),
                   error = function(e) NULL)
    if (is.null(fm) || is.null(fr)) next
    rows[[j]] <- tibble(
      table     = table_name,
      item_x    = items[a],
      item_y    = items[b],
      k_x       = k_obs2[a],
      k_y       = k_obs2[b],
      rho_mle   = unname(fm$thetahat[1]),
      rho_rob   = unname(fr$thetahat[1]),
      pair_secs = as.numeric(difftime(Sys.time(), p0, units = "secs"))
    )
  }

  secs     <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  pairs_df <- bind_rows(rows)

  if (nrow(pairs_df) == 0) {
    saveRDS(list(summary = tibble(table = table_name, n_items = ncol(mat),
                                  n_respondents = nrow(mat),
                                  timed_out = timed_out, secs = secs),
                 pairs = NULL, failed = TRUE),
            file.path(fits_dir, paste0(table_name, "__FAILED.rds")))
    return(NULL)
  }

  pairs_df <- pairs_df |> mutate(diff = rho_rob - rho_mle, abs_diff = abs(diff))

  construct_type <- NA_character_
  if (!is.null(tags_meta) && "construct_type" %in% names(tags_meta)) {
    ct <- tags_meta$construct_type[tags_meta$table == table_name]
    if (length(ct) == 1) construct_type <- ct
  }

  summary_row <- tibble(
    table          = table_name,
    construct_type = construct_type,
    n_items        = ncol(mat),
    n_respondents  = nrow(mat),
    n_pairs         = nrow(pairs_df),   # pairs actually fit
    n_pairs_possible = n_pair,          # differs if the table timed out part-way
    timed_out       = timed_out,
    max_categories = max(k_obs2),
    median_abs_diff = median(pairs_df$abs_diff, na.rm = TRUE),
    max_abs_diff    = max(pairs_df$abs_diff, na.rm = TRUE),
    mean_diff       = mean(pairs_df$diff, na.rm = TRUE),   # signed: is one systematically larger?
    prop_rob_above  = mean(pairs_df$diff > 0, na.rm = TRUE),
    secs            = secs,
    secs_per_pair   = secs / nrow(pairs_df)
  )

  list(summary = summary_row, pairs = pairs_df)
}

fit_to_disk <- function(table_name) {
  out_file    <- file.path(fits_dir, paste0(table_name, ".rds"))
  failed_file <- file.path(fits_dir, paste0(table_name, "__FAILED.rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name); return(invisible(NULL))
  }
  # A table that already timed out or errored is not retried, so a resumed run
  # doesn't burn the timeout again on the same table. Delete its __FAILED.rds
  # to force a retry after changing the settings that made it fail.
  if (file.exists(failed_file)) {
    message("  Skipping (failed previously): ", table_name); return(invisible(NULL))
  }
  message("  Processing: ", table_name)
  res <- tryCatch(analyze_table(table_name, fetch_wide(table_name)),
                  error = function(e) { message("    error: ", conditionMessage(e)); NULL })
  if (!is.null(res)) saveRDS(res, out_file)
}

message("\nScanning ", length(tables), " tables (serial)...")
invisible(lapply(tables, fit_to_disk))

# ==============================================================================
# 4. Combine
# ==============================================================================

all_raw <- map(tables, function(tb) {
  f <- file.path(fits_dir, paste0(tb, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

all_summary <- map(all_raw, "summary") |> compact() |> bind_rows()
all_pairs   <- map(all_raw, "pairs")   |> compact() |> bind_rows()

# Failures/timeouts, so the page can report what was attempted rather than only
# what succeeded.
failed <- map(tables, function(tb) {
  f <- file.path(fits_dir, paste0(tb, "__FAILED.rds"))
  if (file.exists(f)) readRDS(f)$summary else NULL
}) |> compact() |> bind_rows()

message("\nDone. ", nrow(all_summary), " tables usable of ", length(tables),
        " attempted; ", nrow(all_pairs), " item pairs.")
if (nrow(failed) > 0) {
  message(nrow(failed), " table(s) failed, of which ",
          sum(failed$timed_out, na.rm = TRUE), " hit the ",
          TABLE_TIMEOUT_SECS, "s timeout: ",
          paste(failed$table, collapse = ", "))
}
if (nrow(all_summary) > 0) {
  message("Mean seconds per pair: ", round(mean(all_summary$secs_per_pair), 2))
  message("Total compute: ", round(sum(all_summary$secs) / 60, 1), " minutes")
}

saveRDS(
  list(
    summary          = all_summary,
    pairs            = all_pairs,
    failed           = failed,
    candidate_tables = tables,
    n_all_candidates = length(all_candidates),
    tuning_c         = TUNING_C,
    max_categories   = MAX_CATEGORIES,
    items_cap        = ITEMS_CAP,
    pilot            = PILOT,
    date_run         = Sys.Date(),
    session          = sessionInfo()
  ),
  file = file.path(out_dir, "robust_polychoric_results.rds")
)

message("Saved to ", out_dir, "/robust_polychoric_results.rds")

tryCatch(
  irw_save_bibtex(unique(all_summary$table),
                  output_file = file.path(out_dir, "irw_references.bib")),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)
