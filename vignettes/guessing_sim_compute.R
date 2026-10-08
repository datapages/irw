# guessing_sim_compute.R
#
# Two scorecards for Method A purification (Torres Irribarra, Echeverria, &
# Espinoza, 2026), on simulated data where the generating item difficulties
# are known.
#
# The results table in guessing.qmd scores every model by out-of-sample IMV,
# and Purified Rasch's column is negative on every real table. That is the
# wrong yardstick for it: Method A does not propose a new item response
# function and was never designed to improve response prediction. Its claim is
# that removing guessing-contaminated persons from the calibration sample
# yields better ITEM PARAMETERS. This script measures both things on the same
# fitted runs, so the two scorecards can be put side by side instead of the
# vignette merely asserting that a fairer comparison exists.
#
# DGM: the same two-class mixture the Mixture model assumes -- a share pi of
# engaged Rasch responders, the rest responding at a flat rate g on every item.
# This is deliberately the Mixture model's own home turf; the point is not a
# horse race but to show that Purified Rasch loses on prediction while winning
# on parameter recovery, i.e. that the sign of its IMV column is uninformative
# about whether it works.
#
# b is identified only up to location, so both b_hat and b_true are centred
# before comparison. The recovery slope regresses centred b_true on centred
# b_hat: slope > 1 means b_hat is compressed toward the middle relative to the
# truth, which is what guessing does to a Rasch calibration.
#
# Output: vignettes/guessingdata/guessing_sim_results.rds
#
# Usage:
#   Rscript vignettes/guessing_sim_compute.R   # from project root

suppressMessages({
  library(mirt); library(dplyr); library(purrr); library(furrr); library(tibble)
})
source("vignettes/guessing_helpers.R")

out_dir <- "vignettes/guessingdata"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

N_PERSONS  <- 3000
N_ITEMS    <- 45     # matches the modal ENEM table on the page
G_TRUE     <- 0.20   # m = 5 options
SD_THETA   <- 1
PI_GRID    <- c(1.00, 0.90, 0.80, 0.70)
N_REPS     <- 20     # the reviewer's original pass was a single run per cell
HOLDOUT    <- 0.2
EM_CYCLES  <- 2000
QUAD       <- build_quadrature(41)
B_TRUE     <- seq(-2, 2, length.out = N_ITEMS)

ctr <- function(x) x - mean(x)

# RMSE and compression slope of a difficulty vector against the truth.
recovery <- function(b_hat, b_true) {
  c(rmse  = sqrt(mean((ctr(b_hat) - ctr(b_true))^2)),
    slope = unname(coef(lm(ctr(b_true) ~ 0 + ctr(b_hat)))[1]))
}

sim_responses <- function(N, J, b_true, pi_eng, g, sd_theta) {
  theta <- rnorm(N, 0, sd_theta)
  engaged <- rbinom(N, 1, pi_eng)
  P <- plogis(outer(theta, b_true, "-"))
  P[engaged == 0, ] <- g
  Y <- matrix(rbinom(N * J, 1, P), N, J)
  colnames(Y) <- paste0("i", seq_len(J))
  as.data.frame(Y)
}

run_cell <- function(pi_eng, rep) {
  set.seed(20260830 + 1000 * rep + round(100 * pi_eng))
  resp <- sim_responses(N_PERSONS, N_ITEMS, B_TRUE, pi_eng, G_TRUE, SD_THETA)
  ho <- mask_holdout(resp, HOLDOUT)
  y <- ho$true_vals
  Y_train <- as.matrix(ho$train)

  fit_rasch <- mirt(ho$train, 1, itemtype = "Rasch", verbose = FALSE,
                    technical = list(NCYCLES = EM_CYCLES))
  b_rasch <- -coef(fit_rasch, simplify = TRUE)$items[, "d"]
  p_rasch <- heldout_preds_mirt(fit_rasch, ho$mask_idx)

  fit_pur <- purify_rasch(Y_train, g_fit = G_TRUE, quad = QUAD)
  p_pur   <- predict_purified_rasch(fit_pur, Y_train, quad = QUAD)[ho$mask_idx]

  fit_mix <- fit_mixture(Y_train, g_fit = G_TRUE, quad = QUAD)
  p_mix   <- predict_mixture(fit_mix, Y_train)[ho$mask_idx]

  r_rasch <- recovery(b_rasch, B_TRUE)
  r_pur   <- recovery(fit_pur$b, B_TRUE)
  r_mix   <- recovery(fit_mix$b, B_TRUE)

  tibble(
    pi_true = pi_eng, rep = rep,
    pi_hat = fit_mix$pi, frac_flagged = fit_pur$frac_flagged,
    sd_mix = fit_mix$sd, sd_pur = fit_pur$sd,
    rmse_rasch = r_rasch["rmse"], rmse_pur = r_pur["rmse"], rmse_mix = r_mix["rmse"],
    slope_rasch = r_rasch["slope"], slope_pur = r_pur["slope"], slope_mix = r_mix["slope"],
    imv_pur = compute_imv(p_rasch, p_pur, y),
    imv_mix = compute_imv(p_rasch, p_mix, y)
  )
}

if (!isTRUE(getOption("guessing.testmode"))) {
  grid <- expand.grid(pi_eng = PI_GRID, rep = seq_len(N_REPS))
  message("Running ", nrow(grid), " simulation cells ...")

  plan(multisession, workers = min(4, parallel::detectCores() %/% 2))
  res <- future_map2(grid$pi_eng, grid$rep, run_cell,
                     .options = furrr_options(seed = TRUE)) |> bind_rows()
  plan(sequential)

  summary_sim <- res |>
    group_by(pi_true) |>
    summarise(
      n_reps = n(),
      across(c(pi_hat, frac_flagged, sd_mix, sd_pur,
               rmse_rasch, rmse_pur, rmse_mix,
               slope_rasch, slope_pur, slope_mix,
               imv_pur, imv_mix),
             list(mean = ~mean(.x), sd = ~sd(.x)), .names = "{.col}_{.fn}"),
      .groups = "drop"
    )

  print(as.data.frame(summary_sim[, c("pi_true", "rmse_rasch_mean", "rmse_pur_mean",
                                      "rmse_mix_mean", "imv_pur_mean", "imv_mix_mean")]))

  saveRDS(
    list(reps = res, summary = summary_sim,
         settings = list(N = N_PERSONS, J = N_ITEMS, g_true = G_TRUE,
                         sd_theta = SD_THETA, pi_grid = PI_GRID,
                         n_reps = N_REPS, holdout_frac = HOLDOUT,
                         b_true = B_TRUE),
         date_run = Sys.Date(), session = sessionInfo()),
    file = file.path(out_dir, "guessing_sim_results.rds")
  )
  message("Saved to ", out_dir, "/guessing_sim_results.rds")
}
