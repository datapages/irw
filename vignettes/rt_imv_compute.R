# rt_imv_compute.R
#
# Quantifies how much response time (RT) improves predictions of binary
# accuracy beyond IRT alone, using IMV as the evaluation metric.
# Motivated by Domingue et al. (2022, JEBS).
#
# Usage: Rscript vignettes/rt_imv_compute.R   # from project root

library(irw)
library(lme4)
library(imv)
library(dplyr)
library(purrr)
library(tibble)
library(splines)
library(tidyr)

set.seed(20240101)

out_dir  <- "vignettes/rtimvdata"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

N_DATASETS <- Inf    # run all available RT datasets

# ------------------------------------------------------------------------------
# Dataset selection — sample broadly to avoid alphabetical personality-scale bias
# ------------------------------------------------------------------------------

all_rt_tables <- irw_filter(var = "rt")
message("Total RT datasets: ", length(all_rt_tables))

set.seed(42)
tables <- sample(all_rt_tables, min(N_DATASETS, length(all_rt_tables)), replace = FALSE)
message("Trying: ", length(tables), " datasets")

# ------------------------------------------------------------------------------
# Per-dataset fitting
# ------------------------------------------------------------------------------

fit_dataset <- function(table_name) {
  message("\n=== ", table_name, " ===")
  out_file <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) { message("  cached"); return(readRDS(out_file)) }

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("  fetch failed: ", conditionMessage(e)); NULL
  })
  if (is.null(df)) return(NULL)

  df <- df |>
    mutate(rt   = suppressWarnings(as.numeric(rt)),
           item = as.character(item),
           id   = as.character(id)) |>
    filter(resp %in% c(0, 1), !is.na(rt), rt > 0) |>
    mutate(resp = as.integer(resp))

  n_items   <- n_distinct(df$item)
  n_persons <- n_distinct(df$id)
  if (nrow(df) < 200 || n_items < 5 || n_persons < 150) {
    message("  skipped: too small (", nrow(df), " rows, ", n_items, " items, ", n_persons, " persons)")
    return(NULL)
  }

  # Drop items/persons with no response variance (glmer needs both 0s and 1s)
  df <- df |>
    group_by(item) |> filter(n_distinct(resp) > 1) |> ungroup() |>
    group_by(id)   |> filter(n_distinct(resp) > 1) |> ungroup()

  n_items <- n_distinct(df$item)
  if (n_items < 5) { message("  skipped: < 5 variable items"); return(NULL) }

  # Cap at 500 responses per item (subsample respondents)
  target_rows <- 500L * n_items
  if (nrow(df) > target_rows) {
    frac     <- target_rows / nrow(df)
    keep_ids <- sample(unique(df$id), round(frac * n_distinct(df$id)))
    df       <- df[df$id %in% keep_ids, ]
  }

  df <- df |>
    group_by(item) |>
    mutate(rt_cwi = log(rt) - mean(log(rt), na.rm = TRUE)) |>
    ungroup()

  # Precompute spline columns on the full df so imv()'s internal update() calls
  # reuse fixed columns rather than re-evaluating bs() with different knots per fold.
  spl <- bs(df$rt_cwi, df = 4)
  df$spl1 <- spl[, 1]; df$spl2 <- spl[, 2]
  df$spl3 <- spl[, 3]; df$spl4 <- spl[, 4]

  message("  N=", n_distinct(df$id), " | J=", n_items, " | rows=", nrow(df))

  # --- Fit glmer models: crossed random effects absorb item/person effects ---
  # Note: glmerControl() must be inlined (not stored in a variable) so that
  # imv()'s internal update() calls can re-evaluate the model call correctly.
  m0 <- tryCatch(
    glmer(resp ~                                    (1|item) + (1|id), family = binomial, data = df,
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))),
    error = function(e) { message("  m0 failed: ", e$message); NULL })
  m1 <- tryCatch(
    glmer(resp ~ rt_cwi                           + (1|item) + (1|id), family = binomial, data = df,
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))),
    error = function(e) { message("  m1 failed: ", e$message); NULL })
  m2 <- tryCatch(
    glmer(resp ~ spl1 + spl2 + spl3 + spl4       + (1|item) + (1|id), family = binomial, data = df,
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))),
    error = function(e) { message("  m2 failed: ", e$message); NULL })

  if (is.null(m0) || is.null(m1) || is.null(m2)) {
    message("  glmer fitting failed"); return(NULL)
  }

  # --- IMV comparisons --------------------------------------------------------
  set.seed(42)
  imv_linear <- tryCatch(imv(m0, m1)$mean, error = function(e) NA_real_)
  imv_spline <- tryCatch(imv(m0, m2)$mean, error = function(e) NA_real_)

  message(sprintf("  IMV linear=%.4f | spline=%.4f", imv_linear, imv_spline))

  result <- list(
    table_name = table_name,
    N          = n_distinct(df$id),
    J          = n_items,
    n_obs      = nrow(df),
    imv_linear = imv_linear,
    imv_spline = imv_spline
  )
  saveRDS(result, out_file)
  result
}

# ------------------------------------------------------------------------------
# Run
# ------------------------------------------------------------------------------

raw_safe <- map(tables, safely(fit_dataset))
raw <- map(raw_safe, \(x) {
  if (!is.null(x$error)) message("  ERROR: ", conditionMessage(x$error))
  x$result
}) |> compact()

results <- map_dfr(raw, \(r) tibble(
  table_name = r$table_name,
  N          = r$N,
  J          = r$J,
  n_obs      = r$n_obs,
  imv_linear = r$imv_linear,
  imv_spline = r$imv_spline
))

message("\n=== Summary ===")
print(results)

saveRDS(
  list(results = results, date_run = Sys.Date(), session = sessionInfo()),
  file.path(out_dir, "rt_imv_results.rds")
)
message("\nSaved to ", out_dir, "/rt_imv_results.rds")
