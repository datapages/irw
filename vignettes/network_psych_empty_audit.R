# network_psych_empty_audit.R
#
# Ad hoc diagnostic (not part of the main compute pipeline / not cached in
# the fit-to-disk pattern) for a reviewer question: 7 of 610 tables in
# network_psych_results.rds came back with an empty regularized network
# (network_density == 0, no strength-discrimination comparison possible).
# Is that a genuine null result (no detectable partial-correlation structure
# survives LASSO/EBIC shrinkage) or a data/estimation artifact (too few
# usable respondents, degenerate items, correlation matrix numerically
# unstable)? Re-fetches each of the 7 tables directly and reports the same
# diagnostics fit_network() would have seen, without re-running the full
# batch.
#
# Usage:
#   REDIVIS_API_TOKEN=$(cat ~/.redivis_api_token) Rscript vignettes/network_psych_empty_audit.R

library(irw)
library(bootnet)
library(dplyr)

set.seed(20260722)

EMPTY_TABLES <- c(
  "erf_breuer_2017_frmmc",
  "gilbert_meta_38",
  "lsat",
  "project_kids_wj_ak_grade",
  "Resistance",
  "sun_2025_morality_study2_peoplerespect",
  "sun_2025_morality_study2_peopletrust"
)

MAX_N <- 10000

audit_one <- function(table_name) {
  message("Auditing: ", table_name)

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("  fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(df)) {
    return(tibble(table = table_name, status = "fetch_failed"))
  }

  unique_ids <- unique(df$id)
  n_participants_raw <- length(unique_ids)
  if (n_participants_raw > MAX_N) {
    df <- df[df$id %in% sample(unique_ids, MAX_N), ]
  }

  resp <- irw_long2resp(df)
  resp$id <- NULL

  item_var <- sapply(resp, function(x) length(unique(na.omit(x))))
  n_items_raw <- ncol(resp)
  resp_kept <- resp[, item_var > 1, drop = FALSE]
  n_items_kept <- ncol(resp_kept)
  n_dropped_zero_var <- n_items_raw - n_items_kept

  n_categories <- length(unique(na.omit(unlist(resp_kept))))
  prop_missing <- mean(is.na(resp_kept))

  # Marginal proportions / category spread per item -- extreme base rates
  # (near-0 or near-1 endorsement) are the classic driver of a correlation
  # matrix with too little shared variance to survive EBIC/LASSO shrinkage.
  if (n_categories == 2) {
    marginals <- sapply(resp_kept, function(x) mean(x == max(x, na.rm = TRUE), na.rm = TRUE))
    extreme_marginal <- mean(marginals < 0.05 | marginals > 0.95, na.rm = TRUE)
  } else {
    marginals <- NA_real_
    extreme_marginal <- NA_real_
  }

  cor_method <- if (n_categories == 2) NULL else "cor_auto"
  net <- tryCatch(
    suppressWarnings(suppressMessages(
      if (n_categories == 2) {
        estimateNetwork(resp_kept, default = "IsingFit", verbose = FALSE)
      } else if (n_categories >= 3 && n_categories <= 7) {
        estimateNetwork(resp_kept, default = "EBICglasso", corMethod = "cor_auto",
                         corArgs = list(forcePD = TRUE), verbose = FALSE)
      } else {
        NULL
      }
    )),
    error = function(e) {
      message("  network fit failed: ", conditionMessage(e))
      NULL
    }
  )

  density_reproduced <- if (!is.null(net)) {
    mean(net$graph[upper.tri(net$graph)] != 0)
  } else {
    NA_real_
  }

  cor_mat <- tryCatch(suppressWarnings(cor(resp_kept, use = "pairwise.complete.obs")),
                       error = function(e) NULL)
  max_abs_cor <- if (!is.null(cor_mat)) {
    max(abs(cor_mat[upper.tri(cor_mat)]), na.rm = TRUE)
  } else {
    NA_real_
  }
  mean_abs_cor <- if (!is.null(cor_mat)) {
    mean(abs(cor_mat[upper.tri(cor_mat)]), na.rm = TRUE)
  } else {
    NA_real_
  }

  tibble(
    table               = table_name,
    status              = "ok",
    n_participants_raw  = n_participants_raw,
    n_items_raw         = n_items_raw,
    n_items_kept        = n_items_kept,
    n_dropped_zero_var  = n_dropped_zero_var,
    n_categories        = n_categories,
    prop_missing        = prop_missing,
    extreme_marginal    = extreme_marginal,
    max_abs_cor         = max_abs_cor,
    mean_abs_cor        = mean_abs_cor,
    density_reproduced  = density_reproduced
  )
}

results <- bind_rows(lapply(EMPTY_TABLES, audit_one))
print(as.data.frame(results))

out_file <- "vignettes/network_psych_data/network_psych_empty_audit.rds"
saveRDS(results, out_file)
message("Saved: ", out_file)
