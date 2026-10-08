# guessing_imv_benchmark.R -- are this page's IMVs unusually large, and why?
#
# IMV(Rasch, 3PL) on this page runs 0.009-0.024, larger than the IMVs usually
# reported for IRT model comparisons. Domingue et al. (2024, Psychometrika)
# report, over 89 IRW datasets, a mean IMV(1PL, 2PL) of 0.006 and a mean
# IMV(2PL, 3PL) of 0.00016 (max 0.0046). Two questions separate "these tables
# are different" from "this pipeline inflates IMVs":
#
#   1. Pipeline check. Run the page's exact Rasch / 2PL / 3PL specifications,
#      holdout and IMV on ordinary IRW cognitive tables. If they land near the
#      paper's benchmarks, the pipeline is not what makes this page's numbers
#      large.
#
#   2. Prior check. The page's 3PL puts Beta(5, 17) on g (via mirt's `expbeta`,
#      i.e. on the probability scale). The paper states Beta(2, 17). Refit the
#      3PL on three of the page's tables with Beta(2, 17) and with no prior on
#      g, to see how much of IMV(2PL, 3PL) the prior accounts for.
#
# Output: vignettes/guessingdata/guessing_imv_benchmark.rds
#
# Usage (needs REDIVIS_API_TOKEN for part 1; part 2 reads prepared/):
#   Rscript vignettes/guessing_imv_benchmark.R   # from project root

suppressMessages({
  library(mirt); library(irw); library(dplyr); library(purrr); library(tibble)
})
source("vignettes/guessing_helpers.R")

out_dir  <- "vignettes/guessingdata"
prep_dir <- file.path(out_dir, "prepared")
EM_CYCLES <- 2000

spec_2pl <- function(ni) {
  mirt.model(paste0("F = 1-", ni, "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)"))
}
spec_3pl <- function(ni, g_prior = "expbeta, 5, 17") {
  s <- paste0("F = 1-", ni, "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)")
  if (!is.null(g_prior)) s <- paste0(s, ", (1-", ni, ", g, ", g_prior, ")")
  mirt.model(s)
}
fit <- function(train, model, itemtype) {
  mirt(train, model, itemtype = itemtype, verbose = FALSE,
       technical = list(NCYCLES = EM_CYCLES))
}

# --- 1. the page's pipeline on ordinary IRW cognitive tables -------------------
# Candidates: every IRW table with construct_type "Cognitive/educational",
# dichotomous, 800-100,000 people, 15-80 items and density >= 0.9 (IRW
# metadata, 2026-09-19), less
#   - the gilbert_meta_* tables, which the page itself analyses;
#   - longitudinal tables, where one id recurs across waves;
#   - tables whose wide reshape keeps under 80% of their ids. quopl2_forster_
#     2021_{cft,dmat} are listed at density 1 but are ~84% NA in `resp`, so
#     irw_long2resp() drops five in six people.
# preschool_sel_akt (a multi-construct battery) and test_taking_much_2025_mr
# (rater-mediated) are left out as not single-trait ability tests.
BENCH_TABLES <- c(
  "anunciacao_2024_intelligence_gmi", "art", "cdm_ecpe",
  "gcbs_brotherton_2013_vcl", "lexical_difficulty_altgassen_2025_s02_ws",
  "mpsycho_wilmer", "pirlsmissing_sirt", "psychtools_ability", "quantshort",
  "rmet_higgins_2022_tom", "wooly_hartman2022"
)

bench_one <- function(t) {
  message("  benchmark: ", t)
  d <- irw_fetch(t)
  d <- d[!is.na(d$resp), ]
  d <- d[!duplicated(d[, c("id", "item")]), ]
  n_ids <- length(unique(d$id))
  resp <- irw_long2resp(d); resp$id <- NULL
  if (nrow(resp) < 0.8 * n_ids) {
    message("    kept ", nrow(resp), " of ", n_ids, " ids; skipping")
    return(NULL)
  }
  p <- colMeans(resp, na.rm = TRUE)
  resp <- resp[, !(is.na(p) | p %in% c(0, 1)), drop = FALSE]
  set.seed(20260919 + sum(utf8ToInt(t)))   # seeded by name, not position
  ho <- mask_holdout(resp, 0.2); ni <- ncol(resp); y <- ho$true_vals
  fr <- fit(ho$train, 1, "Rasch")
  f2 <- fit(ho$train, spec_2pl(ni), "2PL")
  f3 <- fit(ho$train, spec_3pl(ni), "3PL")
  pr <- heldout_preds_mirt(fr, ho$mask_idx)
  p2 <- heldout_preds_mirt(f2, ho$mask_idx)
  p3 <- heldout_preds_mirt(f3, ho$mask_idx)
  tibble(table = t, n = nrow(resp), j = ni,
         imv_rasch_2pl = compute_imv(pr, p2, y),
         imv_rasch_3pl = compute_imv(pr, p3, y),
         imv_2pl_3pl   = compute_imv(p2, p3, y))
}
bench <- map_dfr(BENCH_TABLES, function(t) {
  tryCatch(bench_one(t), error = function(e) {
    message("    failed: ", conditionMessage(e)); NULL
  })
})

# --- 2. how much of IMV(2PL, 3PL) the guessing prior accounts for -------------
PRIOR_TABLES <- c(enem_2013_1mil_mt = 5, enem_2024_1mil_ch = 5,
                  gilbert_meta_103 = 5)
G_PRIORS <- list(`Beta(5,17) (this page)` = "expbeta, 5, 17",
                 `Beta(2,17)`             = "expbeta, 2, 17",
                 `none`                   = NULL)

prior_one <- function(t, m) {
  message("  prior check: ", t)
  resp <- readRDS(file.path(prep_dir, paste0(t, ".rds")))
  resp <- screen_scored_absences(resp, m)$resp
  p <- colMeans(resp, na.rm = TRUE)
  resp <- resp[, !(is.na(p) | p %in% c(0, 1)), drop = FALSE]
  set.seed(20260919 + sum(utf8ToInt(t)))
  ho <- mask_holdout(resp, 0.2); ni <- ncol(resp); y <- ho$true_vals
  p2 <- heldout_preds_mirt(fit(ho$train, spec_2pl(ni), "2PL"), ho$mask_idx)
  imap_dfr(G_PRIORS, function(gp, label) {
    f3 <- fit(ho$train, spec_3pl(ni, gp), "3PL")
    g <- coef(f3, simplify = TRUE)$items[, "g"]
    tibble(table = t, g_prior = label,
           imv_2pl_3pl = compute_imv(p2, heldout_preds_mirt(f3, ho$mask_idx), y),
           converged = isTRUE(extract.mirt(f3, "converged")),
           g_median = median(g))
  })
}
prior_check <- imap_dfr(PRIOR_TABLES, function(m, t) prior_one(t, m))

print(as.data.frame(bench), digits = 3)
print(as.data.frame(prior_check), digits = 3)

saveRDS(
  list(bench = bench, prior_check = prior_check,
       # Domingue et al. (2024), Section 4: 89 IRW datasets, 4-fold CV
       paper = list(imv_1pl_2pl_mean = 0.006, imv_1pl_2pl_max = 0.068,
                    imv_2pl_3pl_mean = 0.00016, imv_2pl_3pl_max = 0.0046),
       date_run = Sys.Date(), session = sessionInfo()),
  file.path(out_dir, "guessing_imv_benchmark.rds")
)
message("\nWrote ", file.path(out_dir, "guessing_imv_benchmark.rds"))
