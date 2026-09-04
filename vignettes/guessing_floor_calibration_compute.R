# guessing_floor_calibration_compute.R -- what does an implied guessing floor
# of 1/m actually look like when you estimate it?
#
# The per-table diagnostics on this page report the guessing floor the 1PL-G
# stage recovers, expit(gamma_j), summarised over items. An earlier version of
# the page reported the MAXIMUM over items and read values in the 0.3-0.6 range
# as "the floors return to roughly 1/m". That reading does not hold up:
#
#   * the maximum over J items is an extreme order statistic, biased upward as
#     an estimate of a common floor by construction, and
#   * the bias grows as the latent spread narrows, because a floor and a small
#     ability variance explain overlapping features of the data -- both flatten
#     the item response function's lower end. The post-screen ENEM tables sit
#     at SD(theta) 0.39-0.72, the worst part of that range.
#
# So this script calibrates the column against a known truth: generate from the
# 1PL-G with a floor of exactly 1/m on every item, at sample sizes and latent
# spreads matching the tables on the page, and fit with the page's own
# estimator. The result says how far the median and the maximum can be trusted
# in this regime. The design is Doria's (gamma_range_check.R, review of
# 2026-09-03); this version swaps in fit_1pl_ag() so the calibration
# describes the estimator the page actually uses, and the two agree.
#
# Output: vignettes/guessingdata/guessing_floor_calibration.rds
#
# Usage:
#   Rscript vignettes/guessing_floor_calibration_compute.R   # from project root

source("vignettes/guessing_helpers.R")
library(dplyr)
library(tibble)
library(purrr)

set.seed(20260903)

out_dir <- "vignettes/guessingdata"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

N_SIM     <- 1650   # median analysed N over the five screened ENEM tables
J_SIM     <- 45     # J on every ENEM table on the page
M_SIM     <- 5      # so the true floor is 1/m = 0.20 on every item
SD_GRID   <- c(0.4, 0.7, 1.0, 1.4)   # brackets the observed post-screen range
N_REPS    <- 5
QUAD      <- build_quadrature(41)

TRUE_FLOOR <- 1 / M_SIM

# 1PL-G: P = expit(theta - b) + [1 - expit(theta - b)] * floor. Difficulties
# spread over a realistic range rather than drawn, so the only thing varying
# across cells is the latent spread.
gen_1pl_g <- function(n, j, sd_theta, floor) {
  theta <- rnorm(n, 0, sd_theta)
  b <- seq(-1.5, 1.5, length.out = j)
  r <- plogis(outer(theta, b, "-"))
  P <- r + (1 - r) * floor
  matrix(rbinom(n * j, 1, P), n, j)
}

grid <- expand.grid(sd_theta = SD_GRID, rep = seq_len(N_REPS))

results <- pmap_dfr(list(grid$sd_theta, grid$rep, seq_len(nrow(grid))),
                    function(sd_theta, rep, i) {
  Y <- gen_1pl_g(N_SIM, J_SIM, sd_theta, TRUE_FLOOR)
  fit <- tryCatch(fit_1pl_ag(Y, quad = QUAD), error = function(e) {
    message("    fit failed at SD = ", sd_theta, ", rep ", rep, ": ",
            conditionMessage(e))
    NULL
  })
  if (is.null(fit)) return(NULL)
  message(sprintf("[%2d/%d] SD=%.1f rep %d -> floor med %.3f (IQR %.3f-%.3f) max %.3f  SD_hat %.2f",
                  i, nrow(grid), sd_theta, rep, fit$med_guess_floor,
                  fit$q1_guess_floor, fit$q3_guess_floor, fit$max_guess_floor,
                  fit$sd))
  tibble(sd_theta = sd_theta, rep = rep,
         true_floor = TRUE_FLOOR,
         floor_med = fit$med_guess_floor,
         floor_q1 = fit$q1_guess_floor,
         floor_q3 = fit$q3_guess_floor,
         floor_max = fit$max_guess_floor,
         sd_hat = fit$sd,
         alpha_hat = as.numeric(fit$alpha),
         conv_code_g = fit$conv_code_g, conv_code_ag = fit$conv_code_ag)
})

# NB: the range columns are named apart from the means they summarise.
# summarise() evaluates its arguments in order and later ones see the columns
# earlier ones created, so `floor_med = mean(floor_med)` followed by
# `floor_med_lo = min(floor_med)` takes the min of a single mean and silently
# reports a zero-width range.
summary_by_sd <- results |>
  group_by(sd_theta) |>
  summarise(
    n_reps = n(),
    true_floor = first(true_floor),
    floor_med_lo = min(floor_med), floor_med_hi = max(floor_med),
    floor_max_lo = min(floor_max), floor_max_hi = max(floor_max),
    floor_med = mean(floor_med),
    floor_max = mean(floor_max),
    sd_hat = mean(sd_hat),
    .groups = "drop"
  )

print(as.data.frame(summary_by_sd), digits = 3)

saveRDS(
  list(results = results, summary = summary_by_sd,
       n_sim = N_SIM, j_sim = J_SIM, m_sim = M_SIM, n_reps = N_REPS,
       true_floor = TRUE_FLOOR,
       date_run = Sys.Date(), session = sessionInfo()),
  file.path(out_dir, "guessing_floor_calibration.rds")
)
message("\nWrote ", file.path(out_dir, "guessing_floor_calibration.rds"))
