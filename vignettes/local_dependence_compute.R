# local_dependence_compute.R
#
# After fitting the standard unidimensional IRT model this site uses
# elsewhere (2PL for binary items, graded response model for polytomous),
# how often are pairs of items still residually correlated (local
# dependence / testlet effects), and does that concentrate in particular
# item formats or construct types?
#
# Produces the precomputed results loaded by local_dependence.qmd.
#
# Output: local_dependence_data/local_dependence_results.rds
#         local_dependence_data/irw_references.bib
#
# Usage:
#   Rscript vignettes/local_dependence_compute.R   # from project root

library(irw)
library(mirt)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

set.seed(20260720)

out_dir  <- "vignettes/local_dependence_data"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

MIN_ITEMS        <- 5
MAX_ITEMS        <- 40     # Q3 matrices scale quadratically with item count
MIN_PARTICIPANTS <- 200
MAX_N            <- 10000  # downsample respondents before fitting, mirrors 2pl_across_datasets_compute.R
Q3_THRESHOLD     <- 0.2    # conventional flagging threshold (Yen, 1984) -- a heuristic, not a hard rule
PILOT            <- FALSE  # TRUE: run on a small hand-picked/sampled subset for a draft page
PILOT_N_TABLES   <- 12

# ==============================================================================
# 1. Select datasets
# ==============================================================================

all_candidates <- irw_filter(
  n_items        = c(MIN_ITEMS, MAX_ITEMS),
  n_participants = c(MIN_PARTICIPANTS, Inf)
)

message("Candidate tables (n_items ", MIN_ITEMS, "-", MAX_ITEMS,
        ", n_participants >= ", MIN_PARTICIPANTS, "): ", length(all_candidates))

if (PILOT) {
  # Small draft run: a few known-good tables (already used elsewhere on the
  # site, so their model behavior is known) plus a random top-up to get a
  # a mix of binary/polytomous and construct types for the draft qmd.
  known_good <- intersect(c("5personalityfactors", "gilbert_meta_2"), all_candidates)
  remainder  <- setdiff(all_candidates, known_good)
  tables <- c(known_good, sample(remainder, min(PILOT_N_TABLES - length(known_good), length(remainder))))
  message("PILOT run: ", length(tables), " tables selected.")
} else {
  tables <- all_candidates
}

# Table-level construct_type/item_format for the final summary, fetched once
# for all candidate tables rather than per-table.
tags_meta <- tryCatch(irw_tags(tables = tables), error = function(e) {
  message("irw_tags() failed: ", conditionMessage(e))
  NULL
})

# ==============================================================================
# 2. Per-table computation
# ==============================================================================

fit_local_dependence <- function(table_name) {
  message("  Processing: ", table_name)

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(df)) return(NULL)

  # IRW standard optional columns that flag known item-grouping structure
  # (see standard.qmd): item_family is an explicit, dataset-author-asserted
  # testlet/clone-item grouping (non-NA family id shared by >=2 items);
  # itemcov_* are per-item covariates (constant within item) that may also
  # reveal shared structure even without an explicit family id. Captured
  # here (before irw_long2resp() drops non-response columns) for the
  # local-dependence-vs-known-structure case study.
  item_family_map <- NULL
  if ("item_family" %in% names(df)) {
    fam <- df %>%
      distinct(item, item_family) %>%
      filter(!is.na(item_family))
    fam_counts <- fam %>% count(item_family)
    if (any(fam_counts$n >= 2)) {
      item_family_map <- fam %>% semi_join(filter(fam_counts, n >= 2), by = "item_family")
    }
  }

  itemcov_cols <- grep("^itemcov_", names(df), value = TRUE)
  itemcov_df <- if (length(itemcov_cols) > 0) {
    distinct(df, across(all_of(c("item", itemcov_cols))))
  } else NULL

  item_text_df <- suppressMessages(tryCatch(irw_itemtext(table_name), error = function(e) NULL))
  if (!is.data.frame(item_text_df)) item_text_df <- NULL

  unique_ids <- unique(df$id)
  if (length(unique_ids) > MAX_N) {
    df <- df[df$id %in% sample(unique_ids, MAX_N), ]
  }

  resp <- irw_long2resp(df)
  resp$id <- NULL

  # Drop zero-variance items
  resp <- resp[, sapply(resp, function(x) length(unique(na.omit(x))) > 1), drop = FALSE]
  if (ncol(resp) < MIN_ITEMS) {
    message("    skipped: fewer than ", MIN_ITEMS, " usable items")
    return(NULL)
  }

  n_categories <- length(unique(na.omit(unlist(resp))))
  if (n_categories == 2) {
    itemtype <- "2PL"
  } else if (n_categories >= 3 && n_categories <= 10) {
    itemtype <- "graded"
  } else {
    message("    skipped: n_categories = ", n_categories, " outside 2-10 range")
    return(NULL)
  }

  ni <- ncol(resp)

  fit <- tryCatch(
    mirt(resp, 1, itemtype = itemtype, verbose = FALSE,
         technical = list(NCYCLES = 2000)),
    error = function(e) {
      message("    mirt failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)
  if (!mirt::extract.mirt(fit, "converged")) {
    message("    skipped: did not converge")
    return(NULL)
  }

  q3 <- tryCatch(
    residuals(fit, type = "Q3", verbose = FALSE),
    error = function(e) {
      message("    Q3 computation failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(q3)) return(NULL)

  # A handful of item pairs can have undefined Q3 (e.g. near-degenerate
  # residuals for a sparsely-endorsed category after downsampling); drop
  # those cells rather than letting NA propagate into every summary stat.
  q3_upper <- q3[upper.tri(q3)]
  q3_upper <- q3_upper[!is.na(q3_upper)]
  n_pairs   <- length(q3_upper)
  n_flagged <- sum(abs(q3_upper) > Q3_THRESHOLD)

  meta_row <- if (!is.null(tags_meta)) tags_meta[tags_meta$table == table_name, ] else NULL
  construct_type <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$construct_type[1] else NA_character_
  item_format    <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$item_format[1] else NA_character_

  list(
    summary = tibble(
      table              = table_name,
      n_items            = ni,
      n_participants     = nrow(resp),
      n_categories       = n_categories,
      itemtype           = itemtype,
      n_pairs            = n_pairs,
      n_flagged          = n_flagged,
      prop_flagged       = n_flagged / n_pairs,
      max_abs_q3         = max(abs(q3_upper)),
      mean_abs_q3        = mean(abs(q3_upper)),
      construct_type     = construct_type,
      item_format        = item_format,
      has_item_text      = !is.null(item_text_df),
      has_item_family    = !is.null(item_family_map),
      has_itemcov        = !is.null(itemcov_df),
      n_itemcov_cols     = length(itemcov_cols)
    ),
    q3_matrix    = q3,
    item_text    = item_text_df,
    item_family  = item_family_map,
    itemcov      = itemcov_df
  )
}

# ==============================================================================
# 3. Run, writing each result to disk as it completes
#    If the script crashes, re-running skips already-completed tables
# ==============================================================================

fit_to_disk <- function(table_name) {
  out_file <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
  }
  result <- tryCatch(fit_local_dependence(table_name), error = function(e) {
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
# 4. Combine results
# ==============================================================================

all_raw <- map(tables, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

all_summary <- map(all_raw, "summary") |> compact() |> bind_rows()

result_names <- map_chr(all_raw, function(r) r$summary$table)
all_q3          <- set_names(map(all_raw, "q3_matrix"), result_names)
all_item_text   <- set_names(map(all_raw, "item_text"), result_names)   |> compact()
all_item_family <- set_names(map(all_raw, "item_family"), result_names) |> compact()
all_itemcov     <- set_names(map(all_raw, "itemcov"), result_names)     |> compact()

message("\nDone. ", nrow(all_summary), " tables with usable local-dependence results out of ",
        length(tables), " candidates.")
message("  ", sum(all_summary$has_item_text), " with item text, ",
        sum(all_summary$has_item_family), " with a usable item_family grouping, ",
        sum(all_summary$has_itemcov), " with itemcov_* columns.")

# ==============================================================================
# 5. Save combined output
# ==============================================================================

saveRDS(
  list(
    summary          = all_summary,
    q3_matrices      = all_q3,
    item_text        = all_item_text,
    item_family      = all_item_family,
    itemcov          = all_itemcov,
    candidate_tables = tables,
    n_all_candidates = length(all_candidates),
    pilot            = PILOT,
    date_run         = Sys.Date(),
    session          = sessionInfo()
  ),
  file = file.path(out_dir, "local_dependence_results.rds")
)

message("Saved to ", out_dir, "/local_dependence_results.rds")

# ==============================================================================
# 6. Generate citations
#    irw_save_bibtex() takes the full vector of table names in one call
#    (it has no append argument -- it writes the whole bibliography at once)
# ==============================================================================

tryCatch(
  irw_save_bibtex(unique(all_summary$table), output_file = file.path(out_dir, "irw_references.bib")),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)
