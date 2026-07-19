# speededness_compute.R
#
# Produces the precomputed results loaded by speededness.qmd at render time.
# Run once locally before rendering the vignette.
#
# Research question: which IRW instruments show evidence of a fixed time
# limit (speeded tests) versus untimed administration (power tests), and
# what does that imply for how their item response data should be modeled?
#
# Output: vignettes/speededness_data/speededness_results.rds
#
# Usage:
#   Rscript vignettes/speededness_compute.R   # from project root

library(irw)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

set.seed(20260717)

out_dir  <- "vignettes/speededness_data"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Select candidate tables
#
#    var = "rt" restricts to tables that carry a response-time column at all.
#    n_items >= 10 is needed for a position trend to be meaningful.
#    density = NULL disables irw_filter()'s default 0.5-1 density filter.
#    Speededness *is* elevated missingness (low density), so filtering on
#    density up front would systematically exclude the tables most likely
#    to show the signature we're screening for -- e.g. it would silently
#    drop rapm_poulton_2022_timed/untimed and the PISA tables entirely.
#
#    Note: irw_filter() only knows metadata-level facts (which vars exist,
#    item/participant counts). It cannot tell us whether `rt` is recorded
#    per-response or is really a single total-test-time value copied onto
#    every row -- that can only be checked after fetching each table, so the
#    per-table function below re-checks and drops any table that fails this.
# ------------------------------------------------------------------------------

candidate_tables <- irw_filter(var = "rt", n_items = c(10, Inf), density = NULL)
message("Candidate RT tables: ", length(candidate_tables))

# ------------------------------------------------------------------------------
# 2. Per-table speededness diagnostics
# ------------------------------------------------------------------------------

fit_speededness <- function(table_name) {
  message("  Processing: ", table_name)

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(df)) return(NULL)

  df <- df |>
    mutate(
      id   = as.character(id),
      item = as.character(item),
      resp = suppressWarnings(as.numeric(resp)),
      rt   = suppressWarnings(as.numeric(rt))
    )

  n_items_tot <- n_distinct(df$item)
  n_persons   <- n_distinct(df$id)
  if (n_items_tot < 10 || n_persons < 100) {
    message("    skipped: too small (", n_items_tot, " items, ", n_persons, " persons)")
    return(NULL)
  }

  # --- rt plausibility checks operate on a *separate* valid-rt subset
  #     (df_rt). They must not drop rows from the main `df` used for the
  #     omission/position analysis: a not-reached item is exactly the case
  #     where rt is missing alongside resp, and dropping those rows up front
  #     would erase the very signal we're trying to detect (and, for tables
  #     where rt is only sparsely recorded, can wipe out items entirely). ---
  df_rt <- df |> filter(!is.na(rt), rt > 0)
  if (nrow(df_rt) == 0) {
    message("    skipped: no usable (positive, non-missing) rt values")
    return(NULL)
  }

  med_rt <- median(df_rt$rt, na.rm = TRUE)
  if (med_rt > 1000) {
    message("    NOTE: median rt = ", round(med_rt, 1),
            " -- looks like milliseconds rather than seconds; converting")
    df$rt    <- df$rt / 1000
    df_rt$rt <- df_rt$rt / 1000
    med_rt   <- median(df_rt$rt, na.rm = TRUE)
  }
  if (med_rt > 600) {
    message("    skipped: median rt = ", round(med_rt, 1),
            "s is implausible for a single item response (likely total-test time)")
    return(NULL)
  }

  # --- Confirm rt is genuinely per-response, not one total-test-time value
  #     copied onto every row for a given person ---
  rt_per_person  <- df_rt |> group_by(id) |> summarise(n_distinct_rt = n_distinct(rt), .groups = "drop")
  frac_single_rt <- mean(rt_per_person$n_distinct_rt <= 1)
  if (frac_single_rt > 0.9) {
    message("    skipped: rt looks like a single total-test-time value per person (",
            round(frac_single_rt * 100, 1), "% of persons have <= 1 distinct rt)")
    return(NULL)
  }

  # --- Item position: use `date` as a within-person presentation order when
  #     it varies meaningfully across items for most people; otherwise fall
  #     back to order of first appearance in the fetched data (a known
  #     limitation -- flagged in the vignette rather than hidden) ---
  if ("date" %in% names(df) && any(!is.na(df$date))) {
    within_id_var <- df |>
      group_by(id) |>
      summarise(n_distinct_date = n_distinct(date[!is.na(date)]), .groups = "drop")
    has_natural_order <- mean(within_id_var$n_distinct_date > 1, na.rm = TRUE) > 0.5
  } else {
    has_natural_order <- FALSE
  }

  if (has_natural_order) {
    df <- df |>
      group_by(id) |>
      mutate(.rank = rank(date, ties.method = "first")) |>
      ungroup()
    item_order <- df |>
      group_by(item) |>
      summarise(position = median(.rank, na.rm = TRUE), .groups = "drop") |>
      arrange(position) |>
      mutate(position = row_number())
    order_source <- "date (within-person presentation order)"
  } else {
    item_order <- tibble(item = unique(df$item)) |>
      mutate(position = row_number())
    order_source <- "first-appearance (no natural presentation order available)"
  }

  df    <- df    |> left_join(item_order, by = "item")
  df_rt <- df_rt |> left_join(item_order, by = "item")
  ni <- nrow(item_order)

  # --- Omission ("not-reached") trend: proportion NA response by position ---
  omit_by_pos <- df |>
    group_by(position) |>
    summarise(
      n_total   = n(),
      n_missing = sum(is.na(resp)),
      p_missing = n_missing / n_total,
      .groups   = "drop"
    )

  omit_slope <- coef(lm(p_missing ~ position, data = omit_by_pos))[["position"]]

  # Permutation test: shuffle position labels, refit, compare slope magnitude
  n_perm <- 500
  perm_slopes <- vapply(seq_len(n_perm), function(i) {
    shuffled <- omit_by_pos
    shuffled$position <- sample(shuffled$position)
    coef(lm(p_missing ~ position, data = shuffled))[["position"]]
  }, numeric(1))
  omit_perm_p <- mean(abs(perm_slopes) >= abs(omit_slope))

  classification <- if (omit_slope > 0 && omit_perm_p < 0.05) "likely speeded" else "likely power"

  # --- RT-by-position trend (secondary signature: rushing near the end) ---
  #     Uses df_rt (valid-rt subset), not df, since positions with sparse or
  #     absent rt recording shouldn't be treated as "rt = 0 at this position".
  rt_by_pos <- df_rt |>
    group_by(position) |>
    summarise(mean_rt = mean(rt, na.rm = TRUE), .groups = "drop")
  rt_slope <- tryCatch(
    coef(lm(mean_rt ~ position, data = rt_by_pos))[["position"]],
    error = function(e) NA_real_
  )

  # --- Accuracy-by-position trend: diagnostic only, confounded with any
  #     difficulty-ordering of items; reported as-is, flagged in the vignette ---
  acc_by_pos <- df |>
    filter(resp %in% c(0, 1)) |>
    group_by(position) |>
    summarise(p_correct = mean(resp, na.rm = TRUE), n = n(), .groups = "drop")
  acc_slope <- if (nrow(acc_by_pos) >= 3) {
    tryCatch(coef(lm(p_correct ~ position, data = acc_by_pos))[["position"]],
             error = function(e) NA_real_)
  } else {
    NA_real_
  }

  # --- Person-level speed-accuracy relationship ---
  #     total_rt is summed over df_rt (valid rt only); total_score is summed
  #     over the full df so persons aren't penalized for missing rt coverage.
  person_rt <- df_rt |>
    group_by(id) |>
    summarise(total_rt = sum(rt, na.rm = TRUE), .groups = "drop")
  person_score <- df |>
    group_by(id) |>
    summarise(total_score = sum(resp, na.rm = TRUE), .groups = "drop")
  person_summary <- left_join(person_score, person_rt, by = "id")
  speed_acc_cor <- tryCatch(
    cor(person_summary$total_rt, person_summary$total_score, use = "pairwise.complete.obs"),
    error = function(e) NA_real_
  )

  item_level <- item_order |>
    left_join(omit_by_pos, by = "position") |>
    left_join(rt_by_pos, by = "position") |>
    left_join(acc_by_pos |> select(position, p_correct), by = "position") |>
    mutate(table = table_name, .before = 1)

  table_summary <- tibble(
    table          = table_name,
    n_items        = ni,
    n_participants = n_persons,
    order_source   = order_source,
    omit_slope     = omit_slope,
    omit_perm_p    = omit_perm_p,
    rt_slope       = rt_slope,
    acc_slope      = acc_slope,
    speed_acc_cor  = speed_acc_cor,
    classification = classification,
    median_rt_sec  = med_rt
  )

  list(item_level = item_level, table_summary = table_summary)
}

# ------------------------------------------------------------------------------
# 3. Run in parallel, writing each result to disk as it completes
#    If the script crashes, re-running skips already-completed tables
# ------------------------------------------------------------------------------

fit_to_disk <- function(table_name) {
  out_file <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
  }
  result <- fit_speededness(table_name)
  if (!is.null(result)) saveRDS(result, out_file)
}

plan(multisession, workers = min(4, parallel::detectCores() %/% 2))
message("\nProcessing ", length(candidate_tables), " candidate tables...")
future_map(candidate_tables, fit_to_disk)
plan(sequential)

# ------------------------------------------------------------------------------
# 4. Combine results
# ------------------------------------------------------------------------------

all_fits <- map(candidate_tables, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |>
  compact()

item_level_results <- map(all_fits, "item_level") |> bind_rows()
table_summary       <- map(all_fits, "table_summary") |> bind_rows()

message("\nDone. ", nrow(table_summary), " tables classified out of ",
        length(candidate_tables), " candidates.")

# ------------------------------------------------------------------------------
# 5. Join construct_type from IRW tags
#    (irw_metadata() does not carry construct_type; it lives in irw_tags())
# ------------------------------------------------------------------------------

table_summary <- tryCatch({
  tg <- irw_tags()
  table_summary |> left_join(tg |> select(table, construct_type), by = "table")
}, error = function(e) {
  message("  construct_type join failed: ", conditionMessage(e))
  table_summary |> mutate(construct_type = NA_character_)
})

# ------------------------------------------------------------------------------
# 6. Save combined output
# ------------------------------------------------------------------------------

saveRDS(
  list(
    table_summary      = table_summary,
    item_level_results = item_level_results,
    candidate_tables   = candidate_tables,
    date_run           = Sys.Date(),
    session            = sessionInfo()
  ),
  file = file.path(out_dir, "speededness_results.rds")
)

message("Saved to ", out_dir, "/speededness_results.rds")

# ------------------------------------------------------------------------------
# 7. Generate citations
#    irw_save_bibtex() takes the full vector of table names in one call
#    (it has no append argument -- it writes the whole bibliography at once)
# ------------------------------------------------------------------------------

tryCatch(
  irw_save_bibtex(table_summary$table, output_file = file.path(out_dir, "irw_references.bib")),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)

message("Citations saved to ", out_dir, "/irw_references.bib")
