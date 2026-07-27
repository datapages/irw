# robust_polychoric_smoketest.R
#
# Phase 0 smoke test for the robust_polychoric vignette (see memory:
# project_robust_polychoric_vignette.md for the full spec). Establishes
# real per-pair timing for robcat's robust polychoric estimator (Welz,
# Mair & Alfons 2024) under realistic IRW conditions, and uses it to derive
# RESPONDENT_CAP / ITEMS_CAP for the full scan -- not the other way around.
#
# This script is exploratory/one-shot (not part of the future_map compute
# pipeline). Output: printed timing tables + the derived caps.

suppressPackageStartupMessages({
  library(irw)
  library(dplyr)
  library(tidyr)
  library(psych)
  library(robcat)
})

set.seed(20260722)

# ==============================================================================
# 0.1 -- confirm robcat's actual API (already explored interactively; recorded
# here for reproducibility). Exports: polycor() [robust C-estimator, c=0.6
# default], polycor_mle() [non-robust ML], polycormat()/polycormat_mle()
# [matrix wrappers], polyserial family. Both polycor() and polycor_mle()
# return class "robpolycor" with a `variance` argument (not `cov`/`se`) that
# toggles the asymptotic covariance/SE computation. plot.robpolycor() exists
# and plots cell-level Pearson residuals (confirms Phase 5 is feasible).
# This deviates from the spec's guess of separate "polycor"/"robpolycor"
# classes -- both estimators share the "robpolycor" class here.
# ==============================================================================

cat("=== robcat exports ===\n")
print(ls("package:robcat"))
cat("\n=== formals(robcat::polycor) ===\n")
print(args(robcat::polycor))

# ==============================================================================
# 0.2 -- pick one moderate IRW table already used elsewhere (cfa.qmd uses
# 5personalityfactors: 70 items, 7-category Likert, 8936 respondents).
# ==============================================================================

df <- irw_fetch("5personalityfactors")
wide <- df |>
  select(id, item, resp) |>
  distinct(id, item, .keep_all = TRUE) |>
  pivot_wider(names_from = item, values_from = resp)

items <- setdiff(names(wide), "id")
cat("\nTable: 5personalityfactors --", length(items), "items,", nrow(wide),
    "respondents,", sum(complete.cases(wide)), "complete rows\n")

get_pair_data <- function(item_a, item_b, n_cap = Inf, seed = NULL) {
  d <- wide[, c(item_a, item_b)] |> tidyr::drop_na()
  if (is.finite(n_cap) && nrow(d) > n_cap) {
    if (!is.null(seed)) set.seed(seed)
    d <- d[sample(nrow(d), n_cap), ]
  }
  d
}

# ==============================================================================
# 0.3 -- time psych::polychoric (a), robcat robust variance=TRUE (b), and
# robcat robust variance=FALSE (c), each averaged over 5 distinct item pairs,
# at N = {full, capped 2000, capped 500}. Same subsample across a/b/c within
# a given N level (seed fixed per pair+N combination).
# ==============================================================================

set.seed(20260722)
pair_idx <- combn(items, 2)
sampled_pairs <- pair_idx[, sample(ncol(pair_idx), 5)]

n_levels <- list(full = Inf, cap2000 = 2000, cap500 = 500)

time_one <- function(expr_fn) {
  t0 <- Sys.time()
  res <- tryCatch(expr_fn(), error = function(e) e)
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  list(wall = wall, ok = !inherits(res, "error"))
}

results_03 <- list()
for (n_name in names(n_levels)) {
  n_cap <- n_levels[[n_name]]
  for (p in seq_len(ncol(sampled_pairs))) {
    ia <- sampled_pairs[1, p]; ib <- sampled_pairs[2, p]
    d <- get_pair_data(ia, ib, n_cap, seed = 1000 + p)
    n_used <- nrow(d)

    ta <- time_one(function() suppressMessages(suppressWarnings(
      psych::polychoric(as.matrix(d))
    )))
    tb <- time_one(function() robcat::polycor(d[[1]], d[[2]], variance = TRUE))
    tc <- time_one(function() robcat::polycor(d[[1]], d[[2]], variance = FALSE))

    results_03[[length(results_03) + 1]] <- tibble(
      n_level = n_name, pair = paste(ia, ib, sep = " / "), n_used = n_used,
      time_a_polychoric = ta$wall, time_b_robust_varTRUE = tb$wall,
      time_c_robust_varFALSE = tc$wall,
      ok_a = ta$ok, ok_b = tb$ok, ok_c = tc$ok
    )
  }
}
results_03 <- bind_rows(results_03)

cat("\n=== 0.3 per-pair timing (seconds) ===\n")
print(as.data.frame(results_03), row.names = FALSE)

cat("\n=== 0.3 mean timing by N level ===\n")
summary_03 <- results_03 |>
  group_by(n_level) |>
  summarise(mean_n_used = mean(n_used),
            mean_time_a = mean(time_a_polychoric),
            mean_time_b = mean(time_b_robust_varTRUE),
            mean_time_c = mean(time_c_robust_varFALSE), .groups = "drop")
print(as.data.frame(summary_03), row.names = FALSE)

# ==============================================================================
# 0.4 -- category-count scaling check, config (c) only. 5personalityfactors
# is uniformly 7-category, so there's no natural 3-category pair in this
# table. For this timing-only check we synthetically collapse one pair's
# responses to 3 categories (1-2-3->1, 4->2, 5-6-7->3); this recoded pair is
# used ONLY for the 0.4 timing comparison, never for real estimates.
# ==============================================================================

pair_native <- sampled_pairs[, 1]
d_native <- get_pair_data(pair_native[1], pair_native[2], n_cap = 500, seed = 9001)
cat("\nNative pair categories -- item1:", length(unique(d_native[[1]])),
    "item2:", length(unique(d_native[[2]])), "\n")

recode3 <- function(x) dplyr::case_when(x <= 3 ~ 1L, x == 4 ~ 2L, x >= 5 ~ 3L)
d_3cat <- d_native |> mutate(across(everything(), recode3))

pair_6cat <- sampled_pairs[, 2]
d_6cat <- get_pair_data(pair_6cat[1], pair_6cat[2], n_cap = 500, seed = 9002)
cat("6+-category pair categories -- item1:", length(unique(d_6cat[[1]])),
    "item2:", length(unique(d_6cat[[2]])), "\n")

t_3cat <- time_one(function() robcat::polycor(d_3cat[[1]], d_3cat[[2]], variance = FALSE))
t_6cat <- time_one(function() robcat::polycor(d_6cat[[1]], d_6cat[[2]], variance = FALSE))

cat("\n=== 0.4 category-count scaling (config c, N=500) ===\n")
cat(sprintf("3-category (synthetic recode): %.4f sec\n", t_3cat$wall))
cat(sprintf("%d-category (native):           %.4f sec\n",
            length(unique(d_6cat[[1]])), t_6cat$wall))

# ==============================================================================
# 0.5 -- decision rule
# ==============================================================================

cat("\n=== 0.5 decision rule ===\n")
mean_c_by_n <- summary_03$mean_time_c
names(mean_c_by_n) <- summary_03$n_level
print(mean_c_by_n)

saveRDS(list(results_03 = results_03, summary_03 = summary_03,
             t_3cat = t_3cat, t_6cat = t_6cat),
        "vignettes/robust_polychoricdata/phase0_smoketest.rds")
cat("\nSaved: vignettes/robust_polychoricdata/phase0_smoketest.rds\n")
