# response_style_compute.R
#
# How prevalent are extreme responding (ERS) and midpoint responding (MRS) in
# Likert-format IRW data, and can sum-score-based style indices be separated
# from the substantive content the scale is trying to measure?
#
# For each candidate table: build a wide response matrix, compute per-person
# ERS/MRS/mean-response-level indices, and characterise the *shape* of the
# relationship between each style index and the person's mean score -- not
# just its Pearson correlation. The distinction matters: the relationship
# between a sum-score style index and the substantive score is typically
# strong but non-monotone (U-shaped for ERS, inverted-U for MRS), so a
# Pearson correlation near zero is exactly what a strong, symmetric
# dependence produces. See Falk & Ju (2020, Front. Psychol. 11:72), Fig. 4.
#
# Deliberately NOT computed here: a raw-vs-ipsatized CFA comparison. It is the
# obvious thing to reach for, so the reasons against it are worth recording.
# Person-mean-centering is a control for acquiescence, not for ERS/MRS
# (Savalei & Falk, 2014), and ipsatized item scores have a covariance matrix of
# rank p-1 by construction, so a p-item CFA fit to them is not identified --
# it returns out-of-bounds and sign-flipped loadings that reflect the rank
# deficiency rather than anything about response style. The vignette
# demonstrates that failure on simulated data instead, and defers a principled
# style model (MNRM / IRTree / mixture; see Bolt & Meng, 2025) to a later pass.
#
# Produces the precomputed results loaded by response_style.qmd.
#
# Output: response_style_data/response_style_results.rds
#         response_style_data/irw_references.bib
#
# Usage:
#   Rscript vignettes/response_style_compute.R   # from project root

library(irw)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)
library(splines)

set.seed(20260717)

out_dir  <- "vignettes/response_style_data"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

MAX_N            <- 10000  # downsample respondents per table to keep runtime bounded
MIN_ITEMS        <- 5      # matches the n_items floor used in dataset selection below
MIN_PARTICIPANTS <- 100    # dataset-selection floor -- "not too small": below this,
                           # person-level style indices are too unstable to be worth
                           # including in the sample
MIN_RESPONDENTS  <- 30     # per-table floor after cleaning
MAX_TABLES       <- 150    # cap on how many candidate tables to actually run per pass
N_KEEP_PERSON    <- 1500   # per-table cap on person-level rows retained in the cache
N_BINS           <- 20     # quantile bins of person mean score, for the binned curves

# ==============================================================================
# 1. Select datasets
#
# Likert-format tables with 4-7 response categories -- this range covers both
# odd category counts with a true midpoint (5, 7) and even counts without one
# (4, 6) -- at least 5 items per person, so person-level style indices
# (proportions computed over items) are reasonably stable -- and at least
# MIN_PARTICIPANTS respondents. Tables where the per-item category range turns
# out to be inconsistent (e.g. some items 1-4, others 1-7) are dropped later,
# in fit_response_style(), since that can't be detected from table-level
# metadata alone.
# ==============================================================================

all_candidates <- irw_filter(
  item_format    = "Likert Scale/selected response",
  n_categories   = c(4, 7),
  n_items        = c(MIN_ITEMS, Inf),
  n_participants = c(MIN_PARTICIPANTS, Inf)
)

message("Candidate Likert tables (n_participants >= ", MIN_PARTICIPANTS, "): ",
        length(all_candidates))

tables <- if (length(all_candidates) > MAX_TABLES) {
  sample(all_candidates, MAX_TABLES)
} else {
  all_candidates
}

message("Sampled ", length(tables), " of ", length(all_candidates), " candidates for this run.")

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
# 3. Shape of the style-score relationship
#
# Three summaries of how a person-level style index relates to that person's
# mean response level, in increasing order of what they can detect:
#
#   pearson  -- linear association only. Near zero for a symmetric U.
#   eta2     -- proportion of style variance explained by N_BINS quantile bins
#               of the mean score. Assumption-free: detects any shape, at the
#               cost of discretising the predictor.
#   r2_spline -- R^2 from regressing the style index on a natural spline of
#               the mean score. Smooth analogue of eta2.
#
# The gap between pearson^2 and these two is the quantity of interest: it is
# how much of the style-content dependence a correlation-based screen misses.
# ==============================================================================

assoc_shapes <- function(style, score) {
  ok <- is.finite(style) & is.finite(score)
  style <- style[ok]; score <- score[ok]

  out <- list(pearson = NA_real_, eta2 = NA_real_, r2_spline = NA_real_)
  if (length(style) < MIN_RESPONDENTS ||
      length(unique(style)) < 2 || length(unique(score)) < 3) return(out)

  out$pearson <- suppressWarnings(cor(style, score))

  # Binned eta^2. Quantile breaks collapse when the score is coarse (few items,
  # few categories), so use however many distinct bins the data actually support.
  brks <- unique(quantile(score, probs = seq(0, 1, length.out = N_BINS + 1),
                          na.rm = TRUE, type = 7))
  if (length(brks) >= 3) {
    bin <- cut(score, breaks = brks, include.lowest = TRUE)
    if (nlevels(droplevels(bin)) >= 2) {
      grand <- mean(style)
      ss_tot <- sum((style - grand)^2)
      ss_bet <- sum(tapply(style, bin, function(v) length(v) * (mean(v) - grand)^2),
                    na.rm = TRUE)
      if (ss_tot > 0) out$eta2 <- ss_bet / ss_tot
    }
  }

  # Natural-spline R^2, backing off the df when the score has few distinct values.
  n_uniq <- length(unique(score))
  df_try <- min(4L, max(2L, n_uniq - 1L))
  fit <- tryCatch(
    stats::lm(style ~ splines::ns(score, df = df_try)),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    s <- summary(fit)
    if (is.finite(s$r.squared)) out$r2_spline <- s$r.squared
  }

  out
}

# Deterministic lower bound on ERS given a person's mean response.
#
# On a K-category scale with categories min_cat..max_cat, a person can avoid
# the endpoints entirely only while their mean stays inside the range spanned
# by the interior categories, [min_cat + 1, max_cat - 1]. Outside that range,
# endpoint responses are arithmetically forced: to reach a mean m < min_cat + 1
# with everything else at the lowest interior category, a fraction
# (min_cat + 1 - m) of responses must sit at min_cat. Symmetrically at the top.
#
# This bound is a property of the index, not of any respondent's psychology --
# it is the floor under the U-shape that any sum-score ERS index inherits.
ers_lower_bound <- function(m, min_cat, max_cat) {
  lo <- pmax(0, (min_cat + 1) - m)
  hi <- pmax(0, m - (max_cat - 1))
  pmin(1, pmax(lo, hi))
}

# ==============================================================================
# 4. Per-table response-style analysis
#
# Split into a pure analysis function (analyze_response_style) that operates on
# an already-built wide response matrix, and a thin network-fetching wrapper
# (fit_response_style) -- the analysis function alone can be exercised against
# local .Rdata fixtures during development, without hitting Redivis.
# ==============================================================================

analyze_response_style <- function(table_name, wide, tags_meta = NULL) {
  if (is.null(wide) || !"id" %in% names(wide)) {
    message("    skipped: could not build a response matrix")
    return(NULL)
  }

  item_cols <- setdiff(names(wide), "id")
  resp_mat  <- as.matrix(wide[, item_cols, drop = FALSE])
  storage.mode(resp_mat) <- "numeric"

  all_na <- apply(resp_mat, 2, function(x) all(is.na(x)))
  resp_mat <- resp_mat[, !all_na, drop = FALSE]
  if (ncol(resp_mat) < MIN_ITEMS) {
    message("    skipped: fewer than ", MIN_ITEMS, " usable items")
    return(NULL)
  }

  if (nrow(resp_mat) > MAX_N) {
    message("    downsampling from ", nrow(resp_mat), " to ", MAX_N, " respondents")
    keep <- sample(nrow(resp_mat), MAX_N)
    resp_mat <- resp_mat[keep, , drop = FALSE]
  }

  # Per-item category range; skip tables with an inconsistent response format
  # across items (e.g. some items 1-4, others 1-7) rather than guessing which
  # items to trust.
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

  # ---- Shape of the style-score relationship ----
  # The person's mean item response is the raw/substantive score for a
  # single-factor Likert battery, so the same statistic serves both roles.
  ers_shape <- assoc_shapes(ers, person_mean)
  mrs_shape <- if (has_midpoint) assoc_shapes(mrs, person_mean) else
    list(pearson = NA_real_, eta2 = NA_real_, r2_spline = NA_real_)

  # ---- Binned style-vs-score curves, for plotting every table at once ----
  brks <- unique(quantile(person_mean, probs = seq(0, 1, length.out = N_BINS + 1),
                          na.rm = TRUE, type = 7))
  binned <- NULL
  if (length(brks) >= 3) {
    bin <- cut(person_mean, breaks = brks, include.lowest = TRUE)
    binned <- tibble(
      table       = table_name,
      bin         = as.integer(bin),
      n           = as.integer(tapply(person_mean, bin, length))[as.integer(bin)],
      score_mid   = as.numeric(tapply(person_mean, bin, mean))[as.integer(bin)],
      ers_mean    = as.numeric(tapply(ers, bin, mean))[as.integer(bin)],
      mrs_mean    = if (has_midpoint)
                      as.numeric(tapply(mrs, bin, mean))[as.integer(bin)]
                    else NA_real_
    ) |>
      distinct(bin, .keep_all = TRUE) |>
      filter(!is.na(bin)) |>
      arrange(bin) |>
      mutate(
        # Rescale the score onto [0, 1] within the table's own category range so
        # curves from 4-, 5-, 6- and 7-category scales are comparable on one axis.
        score_rel = (score_mid - min_cat) / (max_cat - min_cat),
        ers_bound = ers_lower_bound(score_mid, min_cat, max_cat)
      )
  }

  # ---- Person-level sample retained for exemplar scatterplots ----
  idx <- if (length(person_mean) > N_KEEP_PERSON) {
    sort(sample(length(person_mean), N_KEEP_PERSON))
  } else {
    seq_along(person_mean)
  }
  persons <- tibble(
    table       = table_name,
    person_mean = person_mean[idx],
    ers         = ers[idx],
    mrs         = mrs[idx],
    n_answered  = n_answered[idx]
  )

  construct_type <- NA_character_
  if (!is.null(tags_meta) && "construct_type" %in% names(tags_meta)) {
    ct <- tags_meta$construct_type[tags_meta$table == table_name]
    if (length(ct) == 1) construct_type <- ct
  }

  summary_row <- tibble(
    table          = table_name,
    construct_type = construct_type,
    n_items        = ncol(resp_mat),
    n_participants = nrow(resp_mat),
    min_cat        = min_cat,
    max_cat        = max_cat,
    n_categories   = n_categories,
    has_midpoint   = has_midpoint,
    mean_ers       = mean(ers, na.rm = TRUE),
    sd_ers         = sd(ers, na.rm = TRUE),
    mean_mrs       = if (has_midpoint) mean(mrs, na.rm = TRUE) else NA_real_,
    sd_mrs         = if (has_midpoint) sd(mrs, na.rm = TRUE) else NA_real_,
    ers_score_cor       = ers_shape$pearson,
    ers_score_eta2      = ers_shape$eta2,
    ers_score_r2_spline = ers_shape$r2_spline,
    mrs_score_cor       = mrs_shape$pearson,
    mrs_score_eta2      = mrs_shape$eta2,
    mrs_score_r2_spline = mrs_shape$r2_spline
  )

  list(summary = summary_row, binned = binned, persons = persons)
}

fit_response_style <- function(table_name) {
  message("  Processing: ", table_name)
  wide <- fetch_wide(table_name)
  analyze_response_style(table_name, wide, tags_meta)
}

# ==============================================================================
# 5. Run across candidates, writing each result to disk as it completes
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
future_map(tables, fit_to_disk, .options = furrr_options(seed = TRUE))
plan(sequential)

# ==============================================================================
# 6. Combine results
# ==============================================================================

all_raw <- map(tables, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

all_summary <- map(all_raw, "summary") |> compact() |> bind_rows()
all_binned  <- map(all_raw, "binned")  |> compact() |> bind_rows()
all_persons <- map(all_raw, "persons") |> compact() |> bind_rows()

message("\nDone. ", nrow(all_summary), " tables with usable response-style results out of ",
        length(tables), " candidates.")

# ==============================================================================
# 7. Save combined output
# ==============================================================================

saveRDS(
  list(
    summary           = all_summary,
    binned            = all_binned,
    persons           = all_persons,
    candidate_tables  = tables,
    n_all_candidates  = length(all_candidates),
    date_run          = Sys.Date(),
    session           = sessionInfo()
  ),
  file = file.path(out_dir, "response_style_results.rds")
)

message("Saved to ", out_dir, "/response_style_results.rds")

# ==============================================================================
# 8. Generate citations (only for tables that produced usable results)
#    irw_save_bibtex() takes the full vector of table names in one call
#    (it has no append argument -- it writes the whole bibliography at once)
# ==============================================================================

tryCatch(
  irw_save_bibtex(unique(all_summary$table), output_file = file.path(out_dir, "irw_references.bib")),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)
