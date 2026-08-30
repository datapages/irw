# network_psych_sbm_check.R
#
# Ad hoc robustness check (not part of the main compute pipeline) for a
# reviewer suggestion (Karoline Huth): `bgms` includes a stochastic block
# model (SBM) test that can call a table's cluster structure directly from
# the network side, instead of relying on the dimensionality vignette's
# eigenvalue-ratio/parallel-analysis screen as a proxy. For tables where the
# SBM favors a single cluster, does the strength-discrimination
# correspondence look tighter than for tables where it favors more than
# one? A more direct, network-native version of the existing
# "Cross-vignette payoff 1" dimensionality check in network_psych.qmd.
#
# Runs on the same 20-table Option B stratified subset already used for the
# ordinal-MRF robustness check (network_psych_compute.R's option_b_tables)
# -- bgms is the same 20-50x-slower-than-BGGM model used there, so this
# reuses that already-justified sample rather than drawing a new one or
# attempting the full ~660-table candidate pool.
#
# Calls bgms::bgm() directly (edge_prior = "Stochastic-Block"), not through
# easybgm's wrapper -- extract_sbm() dispatches on class "bgms", and it's
# not confirmed that easybgm's wrapper preserves that class or the
# SBM-specific fields when it re-wraps a bgms fit. Calling bgms directly
# sidesteps that uncertainty entirely.
#
# Status: prepared, NOT yet run. No smoke test executed either (per-table
# bgm() cost is the same order of magnitude as the existing Option B pass,
# ~138-192 sec for a 30-item table) -- code was checked by reading bgms's
# own Rd documentation for exact field names (see comments below), not by
# executing it.
#
# Usage:
#   REDIVIS_API_TOKEN=$(cat ~/.redivis_api_token) Rscript vignettes/network_psych_sbm_check.R

library(irw)
library(bgms)
library(dplyr)
library(purrr)
library(tibble)

set.seed(20260722)

MAIN_CACHE <- "vignettes/network_psych_data/network_psych_results.rds"
OUT_FILE   <- "vignettes/network_psych_data/network_psych_sbm_results.rds"

MAX_N          <- 10000
SBM_ITER       <- 1000   # bgm()'s own default; not reduced, to match Option B's existing rigor
SBM_WARMUP     <- 1000
SBM_CHAINS     <- 4
SBM_LAMBDA     <- 1      # bgm()'s default rate for the zero-truncated Poisson prior on # clusters

if (!file.exists(MAIN_CACHE)) {
  stop("Missing prerequisite cache: ", MAIN_CACHE,
       " -- run vignettes/network_psych_compute.R first.")
}
main_res <- readRDS(MAIN_CACHE)
SBM_TABLES <- main_res$option_b_tables
message("SBM check subset (", length(SBM_TABLES), " tables, reusing option_b_tables): ",
        paste(SBM_TABLES, collapse = ", "))

# Prior odds of K=1 (single cluster) vs K>1 under bgm()'s own zero-truncated
# Poisson(lambda) prior on the number of clusters, used below to convert a
# posterior P(K=1) into a Bayes factor via posterior odds / prior odds
# (Savage-Dickey-style). Zero-truncated Poisson pmf at k=1:
#   P(K=1) = lambda * exp(-lambda) / (1 - exp(-lambda))
prior_p_k1 <- function(lambda) {
  (lambda * exp(-lambda)) / (1 - exp(-lambda))
}
PRIOR_P_K1 <- prior_p_k1(SBM_LAMBDA)
PRIOR_ODDS_K1 <- PRIOR_P_K1 / (1 - PRIOR_P_K1)

fit_one_sbm <- function(table_name) {
  df <- tryCatch(irw_fetch(table_name), error = function(e) NULL)
  if (is.null(df)) return(NULL)

  unique_ids <- unique(df$id)
  if (length(unique_ids) > MAX_N) df <- df[df$id %in% sample(unique_ids, MAX_N), ]

  resp <- tryCatch(irw_long2resp(df), error = function(e) {
    message("    irw_long2resp() failed for ", table_name, ": ", conditionMessage(e))
    NULL
  })
  if (is.null(resp)) return(NULL)
  resp$id <- NULL
  resp <- resp[, sapply(resp, function(x) length(unique(na.omit(x))) > 1), drop = FALSE]
  resp_num <- as.matrix(sapply(resp, as.numeric))

  fit <- tryCatch(
    # variable_type = "ordinal" is correct for binary items too -- bgm()'s
    # own docs: "Binary variables are automatically treated as ordinal."
    bgm(resp_num, variable_type = "ordinal", edge_prior = "Stochastic-Block",
        iter = SBM_ITER, warmup = SBM_WARMUP, chains = SBM_CHAINS,
        lambda = SBM_LAMBDA, display_progress = "none", seed = 20260722),
    error = function(e) {
      message("    SBM fit failed for ", table_name, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)

  sbm <- tryCatch(extract_sbm(fit), error = function(e) {
    message("    extract_sbm() failed for ", table_name, ": ", conditionMessage(e))
    NULL
  })
  if (is.null(sbm)) return(NULL)

  # posterior_num_blocks: single-column data frame ("probability"), row i
  # (by position, 1-indexed) = P(K = i). Confirmed against a live fit on
  # synthetic data (bgm() on an 8-item random matrix): 8 rows, decreasing
  # probability, no separate K/cluster-count column -- the row position IS
  # the cluster count.
  num_blocks_df <- sbm$posterior_num_blocks
  post_p_k1 <- num_blocks_df$probability[1]

  bf_k1 <- if (!is.na(post_p_k1) && post_p_k1 < 1) {
    (post_p_k1 / (1 - post_p_k1)) / PRIOR_ODDS_K1
  } else {
    NA_real_
  }

  tibble(
    table       = table_name,
    n_items     = ncol(resp_num),
    n_used      = nrow(na.omit(resp_num)),
    post_p_k1   = post_p_k1,
    bf_k1       = bf_k1,
    favors_one_cluster = !is.na(bf_k1) && bf_k1 > 10
  )
}

results <- map_dfr(SBM_TABLES, function(tbl) {
  message("Table: ", tbl)
  tryCatch(fit_one_sbm(tbl), error = function(e) {
    message("    unexpected error for ", tbl, ": ", conditionMessage(e))
    NULL
  })
})

# Join to the main results cache's strength-discrimination correlation and
# compare, mirroring the existing dimensionality/local-dependence
# cross-checks in network_psych.qmd.
comparison <- results %>%
  inner_join(main_res$summary %>% select(table, strength_a_cor), by = "table") %>%
  filter(!is.na(strength_a_cor), !is.na(bf_k1))

message("\nTables with usable SBM + strength-discrimination comparison: ", nrow(comparison))
if (nrow(comparison) > 0) {
  print(comparison %>%
    group_by(favors_one_cluster) %>%
    summarise(n = n(), median_strength_a_cor = median(strength_a_cor), .groups = "drop"))
}

saveRDS(list(results = results, comparison = comparison,
             prior_lambda = SBM_LAMBDA, prior_p_k1 = PRIOR_P_K1,
             tables = SBM_TABLES, date_run = Sys.Date()),
        OUT_FILE)
message("Saved: ", OUT_FILE)
