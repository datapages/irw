# 2pl_across_datasets_compute.R
#
# Run this script once locally to produce the precomputed results object
# loaded by 2pl_across_datasets.qmd at render time.
#
# Output: 2pldata/2pl_across_datasets_results.rds
#
# Usage:
#   Rscript 2pl_across_datasets_compute.R   # from project root
#   -- or run interactively in RStudio --

library(irw)
library(mirt)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

# ------------------------------------------------------------------------------
# 1. Select datasets
# ------------------------------------------------------------------------------

cognitive_tables <- irw_filter(
  construct_type = "Cognitive/educational",
  n_categories   = 2,
  n_items        = c(10, 60),
  n_participants = c(500, Inf),
  density        = c(0.8, 1.0)
)

message("Cognitive datasets: ", length(cognitive_tables))

# ------------------------------------------------------------------------------
# 2. Fit 2PL to each dataset
#    Prior on a: lognormal(0, 1.0) — weak, centres near a=1,
#    allows roughly 0.1-7 range, prevents runaway estimates
# ------------------------------------------------------------------------------

fit_2pl <- function(table_name) {
  message("  Fitting: ", table_name)

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(df)) return(NULL)

  # Downsample to at most 10000 unique respondents before reshaping
  max_ids    <- 10000
  unique_ids <- unique(df$id)
  if (length(unique_ids) > max_ids) {
    message("    downsampling from ", length(unique_ids), " to ", max_ids, " respondents")
    df <- df[df$id %in% sample(unique_ids, max_ids), ]
  }

  resp <- irw_long2resp(df)
  resp$id <- NULL

  # Drop zero-variance items
  resp <- resp[, sapply(resp, function(x) length(unique(na.omit(x))) > 1), drop = FALSE]
  if (ncol(resp) < 5) {
    message("    skipped: fewer than 5 usable items")
    return(NULL)
  }

  ni <- ncol(resp)

  model_spec <- mirt.model(paste0(
    "F = 1-", ni, "\n",
    "PRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)"
  ))

  fit <- tryCatch(
    mirt(resp, model_spec, itemtype = rep("2PL", ni),
         method = "EM", technical = list(NCYCLES = 2000), verbose = FALSE),
    error = function(e) {
      message("    mirt failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)

  params <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  tibble(
    table = table_name,
    item  = rownames(params),
    a     = params[, "a"],
    b     = params[, "b"]
  )
}

# ------------------------------------------------------------------------------
# 3. Run in parallel, writing each result to disk as it completes
#    If the script crashes, re-running skips already-completed datasets
# ------------------------------------------------------------------------------

out_dir <- "./2pldata/fits"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fit_to_disk <- function(table_name) {
  out_file <- file.path(out_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
  }
  result <- fit_2pl(table_name)
  if (!is.null(result)) saveRDS(result, out_file)
}

plan(multisession, workers = min(4, parallel::detectCores() %/% 2))
message("\nFitting ", length(cognitive_tables), " datasets...")
future_map(cognitive_tables, fit_to_disk)
plan(sequential)

# ------------------------------------------------------------------------------
# 4. Combine results
# ------------------------------------------------------------------------------

all_results <- map(cognitive_tables, function(tbl) {
  f <- file.path(out_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |>
  compact() |>
  bind_rows()

message("\nDone. ", nrow(all_results), " item-level estimates across ",
        n_distinct(all_results$table), " datasets.")

# ------------------------------------------------------------------------------
# 5. Save combined output
# ------------------------------------------------------------------------------

saveRDS(
  list(
    all_results      = all_results,
    cognitive_tables = cognitive_tables,
    date_run         = Sys.Date(),
    session          = sessionInfo()
  ),
  file = "2pldata/2pl_across_datasets_results.rds"
)

message("Saved to 2pldata/2pl_across_datasets_results.rds")

# ------------------------------------------------------------------------------
# 6. Generate citations
# ------------------------------------------------------------------------------

for (tbl in cognitive_tables) {
  tryCatch(
    irw_save_bibtex(tbl, output_file = "2pldata/irw_references.bib", append = TRUE),
    error = function(e) message("  bibtex failed for: ", tbl)
  )
}

message("Citations saved to 2pldata/irw_references.bib")
