# network_psych_prior_sensitivity.R
#
# Ad hoc robustness check (not part of the main compute pipeline / not
# fit-to-disk cached the same way) for a reviewer question: Option A's
# Bayesian edge evidence (network_psych_compute.R) uses the matrix-F prior
# default prior_sd = 0.25 (BGGM_PRIOR_SD there), matching Huth et al.'s own
# default rather than anything tuned for IRW's data. Huth et al. themselves
# ran a prior-sensitivity check across 3 SD values on their own data; this
# does the same on a small handful of IRW tables, not the full ~610-table
# candidate pool (that would be a second multi-hour batch for a check whose
# whole point is a handful of illustrative tables, not a population-level
# estimate).
#
# NOTE ON PRIOR_SDS BELOW: Huth et al.'s own paper reports which 3 SD values
# they used; this script picks a similarly-spread illustrative default
# (0.1 / 0.25 / 0.5, i.e. the paper's default flanked by a tighter and a
# looser prior) since the paper's exact values weren't independently
# re-confirmed while writing this script. Check against the paper before
# treating this as a literal replication of their sensitivity check, not
# just an analogous one -- worth 5 minutes before the real run, cheap to
# fix, changes nothing else about the script.
#
# Status: prepared, NOT yet run at the intended scale. A one-table,
# short-timeout smoke test was run to confirm the code path works; that
# result is not saved/treated as final.
#
# Usage:
#   REDIVIS_API_TOKEN=$(cat ~/.redivis_api_token) Rscript vignettes/network_psych_prior_sensitivity.R

library(irw)
library(easybgm)
library(dplyr)
library(purrr)
library(tibble)

set.seed(20260722)

MAIN_CACHE <- "vignettes/network_psych_data/network_psych_results.rds"
OUT_FILE   <- "vignettes/network_psych_data/network_psych_prior_sensitivity_results.rds"

PRIOR_SDS               <- c(0.1, 0.25, 0.5)   # see NOTE above before running for real
N_PRIOR_SENSITIVITY_TABLES <- 6                # "a handful" -- half binary, half polytomous
MAX_N              <- 10000
MIN_BGGM_N         <- 30
BGGM_TIME_BUDGET_SEC <- 120

if (!file.exists(MAIN_CACHE)) {
  stop("Missing prerequisite cache: ", MAIN_CACHE,
       " -- run vignettes/network_psych_compute.R first.")
}
main_res <- readRDS(MAIN_CACHE)

# Reuse the same stratified binary/polytomous subset already drawn for
# Option B (network_psych_compute.R's option_b_tables), just take the first
# few of each type, rather than drawing a fresh sample -- keeps this check
# anchored to tables already known to produce a usable Bayesian fit.
option_b_summary <- main_res$summary %>% filter(table %in% main_res$option_b_tables)
binary_subset  <- option_b_summary %>% filter(n_categories == 2) %>% pull(table)
poly_subset    <- option_b_summary %>% filter(n_categories > 2) %>% pull(table)
n_b <- min(N_PRIOR_SENSITIVITY_TABLES %/% 2, length(binary_subset))
n_p <- min(N_PRIOR_SENSITIVITY_TABLES - n_b, length(poly_subset))
SENSITIVITY_TABLES <- c(head(binary_subset, n_b), head(poly_subset, n_p))
message("Prior-sensitivity subset (", length(SENSITIVITY_TABLES), " tables): ",
        paste(SENSITIVITY_TABLES, collapse = ", "))

# Minimal re-fetch + refit, mirroring fit_network()/fit_bayesian_edge_evidence()
# in network_psych_compute.R without sourcing that script directly (which
# would re-run its entire top-to-bottom batch pipeline as a side effect of
# being sourced -- same reasoning as network_psych_empty_audit.R).
fit_one_prior <- function(table_name, prior_sd) {
  df <- tryCatch(irw_fetch(table_name), error = function(e) NULL)
  if (is.null(df)) return(NULL)

  unique_ids <- unique(df$id)
  if (length(unique_ids) > MAX_N) df <- df[df$id %in% sample(unique_ids, MAX_N), ]

  resp <- irw_long2resp(df)
  resp$id <- NULL
  resp <- resp[, sapply(resp, function(x) length(unique(na.omit(x))) > 1), drop = FALSE]

  resp_num <- as.matrix(sapply(resp, as.numeric))
  resp_num <- na.omit(resp_num)
  n_used <- nrow(resp_num)
  if (n_used < MIN_BGGM_N) return(NULL)

  fit <- tryCatch(
    R.utils::withTimeout(
      easybgm(data = resp_num, type = "continuous", package = "BGGM",
              prior_sd = prior_sd, progress = FALSE),
      timeout = BGGM_TIME_BUDGET_SEC, onTimeout = "error"
    ),
    error = function(e) {
      message("    prior_sd=", prior_sd, " failed/timed out for ", table_name, ": ",
              conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)

  bf <- fit$inc_BF   # confirmed against edges_from_bggm_fit() in network_psych_compute.R
  ut <- upper.tri(bf)
  tibble(
    table    = table_name,
    prior_sd = prior_sd,
    n_used   = n_used,
    n_edges  = sum(ut),
    edge_i   = row(bf)[ut],
    edge_j   = col(bf)[ut],
    bf10     = bf[ut]
  )
}

results <- map_dfr(SENSITIVITY_TABLES, function(tbl) {
  message("Table: ", tbl)
  map_dfr(PRIOR_SDS, function(sd) {
    message("  prior_sd = ", sd)
    fit_one_prior(tbl, sd)
  })
})

# Per-table comparison: how much does the evidence-category classification
# (strong/weak presence, inconclusive, weak/strong absence) move when the
# prior gets tighter or looser than the paper's default? Breakpoints copied
# exactly from classify_bf_evidence() in network_psych_compute.R so
# categories here are directly comparable to the main results.
classify_bf <- function(bf10) {
  dplyr::case_when(
    bf10 > 10    ~ "strong_presence",
    bf10 > 3     ~ "weak_presence",
    bf10 >= 1/3  ~ "inconclusive",
    bf10 >= 1/10 ~ "weak_absence",
    bf10 < 1/10  ~ "strong_absence",
    TRUE         ~ NA_character_
  )
}

results <- results %>% mutate(evidence_category = classify_bf(bf10))

comparison <- results %>%
  select(table, prior_sd, edge_i, edge_j, bf10, evidence_category) %>%
  tidyr::pivot_wider(
    id_cols = c(table, edge_i, edge_j),
    names_from = prior_sd,
    values_from = c(bf10, evidence_category),
    names_glue = "{.value}_sd{prior_sd}"
  )

reference_col <- paste0("evidence_category_sd", PRIOR_SDS[PRIOR_SDS == 0.25])
prop_category_flip <- comparison %>%
  mutate(across(starts_with("evidence_category_sd") & !all_of(reference_col),
                ~ .x != .data[[reference_col]], .names = "flip_{.col}")) %>%
  summarise(across(starts_with("flip_"), ~ mean(.x, na.rm = TRUE)))

message("\nProportion of edges whose evidence category flips relative to prior_sd = 0.25:")
print(prop_category_flip)

saveRDS(list(results = results, comparison = comparison,
             prop_category_flip = prop_category_flip,
             prior_sds = PRIOR_SDS, tables = SENSITIVITY_TABLES,
             date_run = Sys.Date()),
        OUT_FILE)
message("Saved: ", OUT_FILE)
