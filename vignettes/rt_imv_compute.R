# rt_imv_compute.R
#
# Quantifies how much response time (RT) improves predictions of binary
# accuracy beyond IRT alone, using IMV as the evaluation metric.
# Motivated by Domingue et al. (2022, JEBS).
#
# Usage: Rscript vignettes/rt_imv_compute.R   # from project root

library(irw)
library(mirt)
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

MAX_N     <- 10000L   # cap respondents per dataset
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

  # Keep binary resp with non-missing RT; coerce types
  df <- df |>
    mutate(rt   = suppressWarnings(as.numeric(rt)),
           item = as.character(item)) |>
    filter(resp %in% c(0, 1), !is.na(rt), rt > 0) |>
    mutate(resp = as.integer(resp))

  # Minimum size and item count
  n_items <- n_distinct(df$item)
  if (nrow(df) < 200 || n_items < 5) {
    message("  skipped: too small (", nrow(df), " rows, ", n_items, " items)")
    return(NULL)
  }

  # Downsample respondents if needed
  ids <- unique(df$id)
  if (length(ids) > MAX_N) {
    ids <- sample(ids, MAX_N)
    df  <- df[df$id %in% ids, ]
  }
  message("  N=", length(unique(df$id)), " | J=", n_items, " | rows=", nrow(df))

  # --- Fit Rasch model (wide format via pivot_wider to preserve IDs/names) ---
  resp_wide <- tryCatch({
    w <- df |>
      select(id, item, resp) |>
      tidyr::pivot_wider(names_from = item, values_from = resp)
    person_ids_wide <<- as.character(w$id)
    w <- as.data.frame(w[, -1])                          # drop id column
    vary <- apply(w, 2, function(x) length(unique(na.omit(x))) > 1)
    w[, vary, drop = FALSE]
  }, error = function(e) { message("  pivot failed: ", conditionMessage(e)); NULL })
  if (is.null(resp_wide) || ncol(resp_wide) < 5) {
    message("  skipped: < 5 variable items")
    return(NULL)
  }

  m_rasch <- tryCatch(
    mirt(resp_wide, 1, "Rasch", verbose = FALSE),
    error = function(e) { message("  Rasch failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(m_rasch)) return(NULL)

  # Extract p0: outer product of person theta × item difficulty
  theta  <- tryCatch(as.numeric(fscores(m_rasch, method = "EAP")),
                     error = function(e) { message("  fscores failed: ", conditionMessage(e)); NULL })
  if (is.null(theta)) return(NULL)
  diffs <- coef(m_rasch, simplify = TRUE)$items[, "d"]   # d = intercept = -delta

  p0_long <- tibble(
    id   = rep(person_ids_wide, times = ncol(resp_wide)),
    item = rep(colnames(resp_wide), each  = nrow(resp_wide)),
    p0   = as.vector(outer(theta, diffs, function(t, d) plogis(t + d)))
  )

  df <- df |>
    mutate(id = as.character(id)) |>
    left_join(p0_long, by = c("id", "item")) |>
    mutate(log_rt = log(rt)) |>
    group_by(item) |>
    mutate(rt_cwi = log_rt - mean(log_rt, na.rm = TRUE)) |>
    ungroup() |>
    filter(!is.na(p0), !is.na(rt_cwi))

  if (nrow(df) < 100) { message("  skipped: too few rows after merge"); return(NULL) }

  # --- Fit GLMs ---------------------------------------------------------------
  m0 <- tryCatch(glm(resp ~ p0,                       family = binomial, data = df),
                 error = function(e) NULL)
  m1 <- tryCatch(glm(resp ~ p0 + rt_cwi,             family = binomial, data = df),
                 error = function(e) NULL)
  m2 <- tryCatch(glm(resp ~ p0 + bs(rt_cwi, df = 4), family = binomial, data = df),
                 error = function(e) NULL)

  if (is.null(m0) || is.null(m1) || is.null(m2)) {
    message("  GLM fitting failed"); return(NULL)
  }

  # --- IMV comparisons --------------------------------------------------------
  set.seed(42)
  imv_linear <- tryCatch(imv(m0, m1)$mean, error = function(e) NA_real_)
  imv_spline <- tryCatch(imv(m0, m2)$mean, error = function(e) NA_real_)
  imv_flex   <- tryCatch(imv(m1, m2)$mean, error = function(e) NA_real_)

  message(sprintf("  IMV linear=%.4f | spline=%.4f | flex=%.4f",
                  imv_linear, imv_spline, imv_flex))

  result <- list(
    table_name = table_name,
    N          = length(unique(df$id)),
    J          = n_items,
    n_obs      = nrow(df),
    imv_linear = imv_linear,
    imv_spline = imv_spline,
    imv_flex   = imv_flex
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
  imv_spline = r$imv_spline,
  imv_flex   = r$imv_flex
))

message("\n=== Summary ===")
print(results)

saveRDS(
  list(results = results, date_run = Sys.Date(), session = sessionInfo()),
  file.path(out_dir, "rt_imv_results.rds")
)
message("\nSaved to ", out_dir, "/rt_imv_results.rds")
