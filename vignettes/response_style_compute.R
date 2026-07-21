# response_style_compute.R
#
# How prevalent are extreme responding (ERS) and midpoint responding (MRS) in
# Likert-format IRW data, and do these response styles distort substantive
# conclusions (factor loadings) if left unmodeled?
#
# For each candidate table: build a wide response matrix, compute per-person
# ERS/MRS/mean-response-level indices, correlate ERS/MRS with the person's
# raw score, then fit a single-factor CFA on raw responses and again on
# person-mean-centered ("ipsatized") responses to see how much standardized
# loadings move once a simple style control is applied.
#
# Produces the precomputed results loaded by response_style.qmd.
#
# Output: response_style_data/response_style_results.rds
#         response_style_data/irw_references.bib
#
# Usage:
#   Rscript vignettes/response_style_compute.R   # from project root

library(irw)
library(lavaan)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

set.seed(20260717)

out_dir  <- "vignettes/response_style_data"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

MAX_N            <- 10000  # downsample respondents before CFA to keep fitting tractable
MIN_ITEMS        <- 5      # matches the n_items floor used in dataset selection below
MIN_PARTICIPANTS <- 100    # dataset-selection floor -- "not too small": below this,
                           # person-level style indices and CFA fits are too unstable
                           # to be worth including in the sample
MIN_RESPONDENTS  <- 30     # per-table floor after cleaning, for stable style indices/CFA
MAX_TABLES       <- 150    # cap on how many candidate tables to actually run per pass

# ==============================================================================
# 1. Select datasets
#
# Likert-format tables with 4-7 response categories -- this range covers both
# odd category counts with a true midpoint (5, 7) and even counts without one
# (4, 6) -- at least 5 items per person, so person-level style indices
# (proportions computed over items) are reasonably stable -- and at least
# MIN_PARTICIPANTS respondents, so table-level ERS/MRS means and CFA fits
# aren't dominated by sampling noise from a handful of people. Tables where
# per-item category range turns out to be inconsistent (e.g. some items
# 1-4, others 1-7) are dropped later, in fit_response_style(), since that
# can't be detected from table-level metadata alone. Very large tables are
# not excluded here -- fit_response_style() subsets down to MAX_N
# respondents per table instead, so a handful of huge tables don't dominate
# runtime or bias the sample toward whichever instruments happen to have
# the most respondents.
# ==============================================================================

all_candidates <- irw_filter(
  item_format    = "Likert Scale/selected response",
  n_categories   = c(4, 7),
  n_items        = c(MIN_ITEMS, Inf),
  n_participants = c(MIN_PARTICIPANTS, Inf)
)

message("Candidate Likert tables (n_participants >= ", MIN_PARTICIPANTS, "): ",
        length(all_candidates))

# Random sample of a manageable subset for this pass -- rerunning the full
# candidate list (hundreds of tables, some with 100+ items and huge N) is
# impractical in one pass; fit_to_disk()'s cache means later passes with a
# different/larger sample just add to what's already on disk rather than
# redoing this work.
tables <- if (length(all_candidates) > MAX_TABLES) {
  sample(all_candidates, MAX_TABLES)
} else {
  all_candidates
}

message("Sampled ", length(tables), " of ", length(all_candidates), " candidates for this run.")

# Table-level construct_type for the final summary, fetched once for all
# candidate tables rather than per-table.
tags_meta <- tryCatch(irw_tags(tables = tables), error = function(e) {
  message("irw_tags() failed: ", conditionMessage(e))
  NULL
})

# ==============================================================================
# 2. Fetch helper
#
# irw_fetch(table, resp = TRUE) converts to a wide response matrix internally
# via irw_long2resp(); if that internal conversion fails, irw_fetch() silently
# falls back to returning the long-format data with a warning. Detect that
# case and retry the conversion directly rather than silently analysing long
# data as if it were wide.
# ==============================================================================

fetch_wide <- function(table_name) {
  out <- tryCatch(irw_fetch(table_name, resp = TRUE), error = function(e) {
    message("    fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(out)) return(NULL)

  if (all(c("item", "resp") %in% names(out))) {
    message("    irw_fetch(resp = TRUE) returned long format; retrying irw_long2resp() directly")
    out <- tryCatch(irw_long2resp(out), error = function(e) {
      message("    irw_long2resp() also failed: ", conditionMessage(e))
      NULL
    })
  }
  out
}

# ==============================================================================
# 3. Per-table response-style analysis
#
# Split into a pure analysis function (analyze_response_style) that operates
# on an already-built wide response matrix, and a thin network-fetching
# wrapper (fit_response_style) -- the analysis function alone is exercised
# against local .Rdata fixtures during development, without hitting Redivis.
# ==============================================================================

analyze_response_style <- function(table_name, wide, tags_meta = NULL) {
  if (is.null(wide) || !"id" %in% names(wide)) {
    message("    skipped: could not build a response matrix")
    return(NULL)
  }

  item_cols <- setdiff(names(wide), "id")
  resp_mat  <- as.matrix(wide[, item_cols, drop = FALSE])
  storage.mode(resp_mat) <- "numeric"

  # Drop items with no responses at all
  all_na <- apply(resp_mat, 2, function(x) all(is.na(x)))
  resp_mat <- resp_mat[, !all_na, drop = FALSE]
  if (ncol(resp_mat) < MIN_ITEMS) {
    message("    skipped: fewer than ", MIN_ITEMS, " usable items")
    return(NULL)
  }

  # Downsample respondents before any fitting
  if (nrow(resp_mat) > MAX_N) {
    message("    downsampling from ", nrow(resp_mat), " to ", MAX_N, " respondents")
    keep <- sample(nrow(resp_mat), MAX_N)
    resp_mat <- resp_mat[keep, , drop = FALSE]
  }

  # Per-item category range; skip tables with an inconsistent response
  # format across items (e.g. some items 1-4, others 1-7) rather than
  # guessing which items to trust.
  item_min <- unname(apply(resp_mat, 2, min, na.rm = TRUE))
  item_max <- unname(apply(resp_mat, 2, max, na.rm = TRUE))
  if (length(unique(item_min)) > 1 || length(unique(item_max)) > 1) {
    message("    skipped: inconsistent category range across items (min: ",
            paste(sort(unique(item_min)), collapse = ","), "; max: ",
            paste(sort(unique(item_max)), collapse = ","), ")")
    return(NULL)
  }
  min_cat <- item_min[1]
  max_cat <- item_max[1]
  n_categories <- max_cat - min_cat + 1
  has_midpoint <- (n_categories %% 2) == 1
  midpoint <- if (has_midpoint) min_cat + (n_categories - 1) / 2 else NA_real_

  # ---- Person-level response-style indices ----
  n_answered  <- rowSums(!is.na(resp_mat))
  keep_person <- n_answered >= MIN_ITEMS
  resp_mat    <- resp_mat[keep_person, , drop = FALSE]
  n_answered  <- n_answered[keep_person]
  if (nrow(resp_mat) < MIN_RESPONDENTS) {
    message("    skipped: fewer than ", MIN_RESPONDENTS, " respondents with >=",
            MIN_ITEMS, " answered items")
    return(NULL)
  }

  ers <- rowSums(resp_mat == min_cat | resp_mat == max_cat, na.rm = TRUE) / n_answered
  mrs <- if (has_midpoint) {
    rowSums(resp_mat == midpoint, na.rm = TRUE) / n_answered
  } else {
    rep(NA_real_, nrow(resp_mat))
  }
  person_mean <- rowMeans(resp_mat, na.rm = TRUE)

  # A substantial ERS/MRS-score correlation suggests style is confounded
  # with the construct rather than a pure nuisance factor. The person's raw
  # mean item response *is* the raw/substantive score for a single-factor
  # Likert battery, so the same statistic serves both roles here.
  ers_score_cor <- suppressWarnings(cor(ers, person_mean, use = "pairwise.complete.obs"))
  mrs_score_cor <- if (has_midpoint) {
    suppressWarnings(cor(mrs, person_mean, use = "pairwise.complete.obs"))
  } else {
    NA_real_
  }

  # ---- Single-factor CFA: raw vs. person-mean-centered (ipsatized) ----
  cfa_items <- colnames(resp_mat)[
    apply(resp_mat, 2, function(x) length(unique(na.omit(x))) > 1)
  ]

  loadings          <- NULL
  style_sensitivity <- NA_real_
  cfa_raw_ok        <- FALSE
  cfa_ips_ok        <- FALSE

  if (length(cfa_items) >= MIN_ITEMS) {
    lav_names  <- paste0("i", make.names(cfa_items))
    model_str  <- paste0("F =~ ", paste(lav_names, collapse = " + "))

    df_raw <- as.data.frame(resp_mat[, cfa_items, drop = FALSE])
    colnames(df_raw) <- lav_names
    fit_raw <- tryCatch(
      lavaan::cfa(model_str, data = df_raw, missing = "fiml", std.lv = TRUE),
      error = function(e) { message("    raw CFA failed: ", conditionMessage(e)); NULL }
    )
    cfa_raw_ok <- !is.null(fit_raw) && isTRUE(lavaan::lavInspect(fit_raw, "converged"))

    df_ips <- as.data.frame(resp_mat[, cfa_items, drop = FALSE] - person_mean)
    colnames(df_ips) <- lav_names
    fit_ips <- tryCatch(
      lavaan::cfa(model_str, data = df_ips, missing = "fiml", std.lv = TRUE),
      error = function(e) { message("    ipsatized CFA failed: ", conditionMessage(e)); NULL }
    )
    cfa_ips_ok <- !is.null(fit_ips) && isTRUE(lavaan::lavInspect(fit_ips, "converged"))

    if (cfa_raw_ok && cfa_ips_ok) {
      std_raw <- lavaan::standardizedSolution(fit_raw)
      std_ips <- lavaan::standardizedSolution(fit_ips)
      l_raw <- std_raw[std_raw$op == "=~", c("rhs", "est.std")]
      l_ips <- std_ips[std_ips$op == "=~", c("rhs", "est.std")]
      names(l_raw)[2] <- "loading_raw"
      names(l_ips)[2] <- "loading_ipsatized"
      merged <- merge(l_raw, l_ips, by = "rhs")
      style_sensitivity <- mean(abs(merged$loading_raw - merged$loading_ipsatized))
      loadings <- tibble(
        table             = table_name,
        item              = sub("^i", "", merged$rhs),
        loading_raw       = merged$loading_raw,
        loading_ipsatized = merged$loading_ipsatized
      )
    } else {
      message("    CFA did not converge cleanly on both raw and ipsatized data; style_sensitivity = NA")
    }
  } else {
    message("    skipped CFA: fewer than ", MIN_ITEMS, " non-degenerate items")
  }

  construct_type <- NA_character_
  if (!is.null(tags_meta) && "construct_type" %in% names(tags_meta)) {
    ct <- tags_meta$construct_type[tags_meta$table == table_name]
    if (length(ct) == 1) construct_type <- ct
  }

  summary_row <- tibble(
    table          = table_name,
    construct_type = construct_type,
    n_items        = length(cfa_items),
    n_participants = nrow(resp_mat),
    min_cat        = min_cat,
    max_cat        = max_cat,
    n_categories   = n_categories,
    has_midpoint   = has_midpoint,
    mean_ers       = mean(ers, na.rm = TRUE),
    sd_ers         = sd(ers, na.rm = TRUE),
    mean_mrs       = if (has_midpoint) mean(mrs, na.rm = TRUE) else NA_real_,
    sd_mrs         = if (has_midpoint) sd(mrs, na.rm = TRUE) else NA_real_,
    ers_score_cor  = ers_score_cor,
    mrs_score_cor  = mrs_score_cor,
    style_sensitivity = style_sensitivity,
    cfa_raw_converged = cfa_raw_ok,
    cfa_ips_converged = cfa_ips_ok
  )

  list(summary = summary_row, loadings = loadings)
}

fit_response_style <- function(table_name) {
  message("  Processing: ", table_name)
  wide <- fetch_wide(table_name)
  analyze_response_style(table_name, wide, tags_meta)
}

# ==============================================================================
# 4. Run across candidates, writing each result to disk as it completes
#    If the script crashes, re-running skips already-completed tables
# ==============================================================================

fit_to_disk <- function(table_name) {
  out_file <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
  }
  result <- tryCatch(fit_response_style(table_name), error = function(e) {
    message("    unexpected error for ", table_name, ": ", conditionMessage(e))
    NULL
  })
  if (!is.null(result)) saveRDS(result, out_file)
}

plan(multisession, workers = min(4, parallel::detectCores() %/% 2))
message("\nAnalyzing ", length(tables), " candidate tables...")
future_map(tables, fit_to_disk)
plan(sequential)

# ==============================================================================
# 5. Combine results
# ==============================================================================

all_raw <- map(tables, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

all_summary  <- map(all_raw, "summary")  |> compact() |> bind_rows()
all_loadings <- map(all_raw, "loadings") |> compact() |> bind_rows()

message("\nDone. ", nrow(all_summary), " tables with usable response-style results out of ",
        length(tables), " candidates.")

# ==============================================================================
# 6. Save combined output
# ==============================================================================

saveRDS(
  list(
    summary           = all_summary,
    loadings          = all_loadings,
    candidate_tables  = tables,
    n_all_candidates  = length(all_candidates),
    date_run          = Sys.Date(),
    session           = sessionInfo()
  ),
  file = file.path(out_dir, "response_style_results.rds")
)

message("Saved to ", out_dir, "/response_style_results.rds")

# ==============================================================================
# 7. Generate citations (only for tables that produced usable results)
#    irw_save_bibtex() takes the full vector of table names in one call
#    (it has no append argument -- it writes the whole bibliography at once)
# ==============================================================================

tryCatch(
  irw_save_bibtex(unique(all_summary$table), output_file = file.path(out_dir, "irw_references.bib")),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)
