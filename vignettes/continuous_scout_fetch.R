# continuous_scout_fetch.R
#
# Scoping script (fetch stage) for a possible IRW vignette on continuous
# item responses (candidate models: Noel & Dauvier beta-IRT for bounded
# [0,1] responses, Samejima's continuous response model for unbounded
# responses, Mellenbergh's GLIRT as theoretical framing). Before writing any
# model-fitting code, we need to know whether IRW actually contains real
# datasets with genuinely continuous item responses (slider/VAS positions,
# continuous probability/percentage estimates), as opposed to long ordinal
# scales that just happen to have many categories.
#
# This is a SCOUTING script only: no model fitting, no LLM/API calls, no
# writing to Redivis/GitHub/any public artifact.
#
# Split into two stages so the classification logic (continuous_scout_
# classify.R) can be tinkered with repeatedly without re-hitting Redivis:
#   1. This script (expensive): n_categories filter from irw_metadata()
#      (no fetching), then fetch resp for every surviving table and cache
#      raw per-table and per-item summaries -- no bounded/unbounded
#      judgment calls made here.
#   2. continuous_scout_classify.R (cheap): loads the cache below and does
#      all classification, thresholds, and output.
#
# Output: vignettes/continuous_scout_data/continuous_scout_raw.rds
#
# Usage:
#   Rscript vignettes/continuous_scout_fetch.R   # from project root

library(irw)
library(dplyr)
library(purrr)
library(tibble)

N_CATEGORIES_CAP <- 12   # standard site summaries exclude/cap items at >=12
                         # categories (see dimensionality.qmd, local_dependence_compute.R
                         # etc., which all treat n_categories <= 10 as the
                         # "cleanly ordinal/polytomous" regime) -- this is exactly
                         # the boundary where many-category and continuous
                         # candidates live

out_dir <- "vignettes/continuous_scout_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_rds <- file.path(out_dir, "continuous_scout_raw.rds")

# ==============================================================================
# Step 1: Filter using existing metadata (no fetching)
# ==============================================================================

md <- irw_metadata()
message("irw_metadata() columns: ", paste(colnames(md), collapse = ", "))
message("irw_metadata() rows: ", nrow(md))

if (!"n_categories" %in% colnames(md)) {
  stop("irw_metadata() has no 'n_categories' field. Available fields: ",
       paste(colnames(md), collapse = ", "),
       ". Cannot do the cheap Step 1 filter described in the scouting prompt ",
       "without substituting a different field -- stopping rather than guessing.")
}

n_na <- sum(is.na(md$n_categories))
message("n_categories: class = ", class(md$n_categories),
        ", NA count = ", n_na, " (prompt anticipated missingness might flag ",
        "broken standard computations; as of this run there are none, but the ",
        "NA branch of the filter below is kept in case that changes)")

candidates <- md |>
  filter(n_categories >= N_CATEGORIES_CAP | is.na(n_categories)) |>
  arrange(desc(n_categories))

message("Step 1 filtered set (n_categories >= ", N_CATEGORIES_CAP,
        " or NA): ", nrow(candidates), " of ", nrow(md), " tables")

if (nrow(candidates) == 0) {
  stop("No tables survived the Step 1 filter -- nothing to scout.")
}

# ==============================================================================
# Step 2: For filtered tables, fetch resp and cache raw per-table/per-item
#          summaries. No bounded/unbounded classification here -- just the
#          numbers a classifier will need.
# ==============================================================================

fetch_stats <- function(table_name) {
  message("  Fetching: ", table_name)

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(df) || !"resp" %in% colnames(df) || !"item" %in% colnames(df)) {
    return(NULL)
  }

  resp_num <- suppressWarnings(as.numeric(df$resp))
  if (all(is.na(resp_num))) {
    message("    skipped: resp is non-numeric")
    return(NULL)
  }
  df <- df |> mutate(resp_num = resp_num)

  table_summary <- tibble(
    table = table_name,
    n_unique = length(unique(na.omit(resp_num))),
    min_resp = min(resp_num, na.rm = TRUE),
    max_resp = max(resp_num, na.rm = TRUE),
    pct_noninteger = mean(resp_num != round(resp_num), na.rm = TRUE)
  ) |>
    mutate(range_resp = max_resp - min_resp)

  item_summary <- df |>
    mutate(item = as.character(item)) |>
    group_by(item) |>
    summarise(
      item_n = n(),
      item_min = min(resp_num, na.rm = TRUE),
      item_max = max(resp_num, na.rm = TRUE),
      item_pct_noninteger = mean(resp_num != round(resp_num), na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(table = table_name, .before = 1)

  list(table_summary = table_summary, item_summary = item_summary)
}

raw <- map(candidates$table, fetch_stats) |> compact()

table_summary <- map(raw, "table_summary") |> list_rbind()
item_summary  <- map(raw, "item_summary") |> list_rbind()

message("Fetched and cached raw stats for ", nrow(table_summary),
        " of ", nrow(candidates), " candidate tables")

saveRDS(
  list(
    candidates    = candidates,     # Step 1 metadata filter result (table, n_categories, etc.)
    table_summary = table_summary,  # one row per table: n_unique/min/max/range/pct_noninteger
    item_summary  = item_summary    # one row per table x item: item_n/item_min/item_max/item_pct_noninteger
  ),
  out_rds
)

message("Saved raw cache to ", out_rds,
        " -- run continuous_scout_classify.R to produce the candidates CSV")
