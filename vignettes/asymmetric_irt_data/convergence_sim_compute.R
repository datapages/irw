# convergence_sim_compute.R
#
# Amendment to the validation gate: check (c) in validation_gate.R recovers
# each model's shape parameter at a single sample size (N = 2,000). That
# confirms the estimator is in the right ballpark there, but says nothing
# about whether it's actually *consistent* -- whether bias and variance
# keep shrinking as N grows, the way a well-behaved MLE/MAP estimator
# should. This script answers that directly: same three custom item types
# (AO, LPE, RH), same fit_custom() pipeline, swept across a grid of sample
# sizes with multiple replicates per size.
#
# Design: item true parameters (a, d, shape) are fixed once per model and
# reused across every N and every replicate -- only the simulated persons
# and their response data are redrawn each replicate. That keeps "bias at
# this N" well defined per item rather than conflating sampling noise in
# the estimates with sampling noise in the item difficulty of the day.
#
# Output: asymmetric_irt_data/convergence_sim.rds
#
# Usage (from the project root):
#   Rscript vignettes/asymmetric_irt_data/convergence_sim_compute.R

source("vignettes/asymmetric_irt_helpers.R")
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

set.seed(20260723)

N_GRID    <- c(250, 500, 1000, 2000, 4000, 8000, 16000)
J         <- 12    # fewer items than check (c)'s 20 -- kept small so REPS x N_GRID is tractable
REPS      <- 8
EM_CYCLES <- 1500

# Fixed true item parameters, one set per model, held constant across every
# N and replicate below. Shape values span clearly asymmetric to
# near-symmetric, same spirit as validation_gate.R check (c).
true_pars <- local({
  set.seed(20260723)
  a_true <- runif(J, 0.7, 1.8)
  d_true <- runif(J, -1.5, 1.5)
  list(
    AO  = list(a = a_true, d = d_true, shape = seq(-1.2, 1.2, length.out = J)),
    LPE = list(a = a_true, d = d_true, shape = seq(-1.2, 1.2, length.out = J)),
    RH  = list(a = a_true, d = d_true, shape = seq(-2.5, 2.5, length.out = J))
  )
})

simulate_one <- function(model_type, n_persons, seed) {
  set.seed(seed)
  tp <- true_pars[[model_type]]
  shape_name <- shape_par_name(model_type)
  def <- MODEL_DEFS[[model_type]]
  theta_persons <- rnorm(n_persons)
  dat <- matrix(NA_integer_, n_persons, J)
  for (j in seq_len(J)) {
    par_j <- setNames(c(tp$a[j], tp$d[j], tp$shape[j]), c("a1", "d", shape_name))
    p1 <- def$P(par_j, theta_persons, 2)[, 2]
    dat[, j] <- rbinom(n_persons, 1, p1)
  }
  colnames(dat) <- paste0("item_", seq_len(J))
  as.data.frame(dat)
}

# One (model, N, replicate) cell: simulate, fit 2PL warm start + custom item,
# return per-item recovered a1/d/shape aligned to the fixed truth by name.
fit_one_cell <- function(model_type, n_persons, rep_id, seed) {
  shape_name <- shape_par_name(model_type)
  tp <- true_pars[[model_type]]
  dat <- simulate_one(model_type, n_persons, seed)

  out <- tryCatch({
    fit_2pl <- mirt(dat, 1, itemtype = "2PL", verbose = FALSE,
                     technical = list(NCYCLES = EM_CYCLES))
    ad_2pl   <- extract_ad(fit_2pl)
    ad_start <- if (model_type == "RH") convert_ad_logit_to_probit(ad_2pl, D = 1.702) else ad_2pl
    fit_custom(dat, model_type, make_custom_item(model_type),
               ad_start = ad_start, shape_init = 0, prior_sd = 1, em_cycles = EM_CYCLES)
  }, error = function(e) list(mod = NULL, error = TRUE, message = conditionMessage(e)))

  converged <- has_valid_mod(out, require_converged = TRUE)
  if (!has_valid_mod(out)) {
    return(tibble(model = model_type, n = n_persons, rep = rep_id, item = seq_len(J),
                   converged = FALSE, a_true = tp$a, d_true = tp$d, shape_true = tp$shape,
                   a_est = NA_real_, d_est = NA_real_, shape_est = NA_real_))
  }

  recovered <- extract_param_table(out$mod)
  item_names <- paste0("item_", seq_len(J))
  idx <- match(item_names, recovered$item)
  tibble(
    model = model_type, n = n_persons, rep = rep_id, item = seq_len(J),
    converged = converged,
    a_true = tp$a, d_true = tp$d, shape_true = tp$shape,
    a_est = recovered$a1[idx], d_est = recovered$d[idx],
    shape_est = recovered[[shape_name]][idx]
  )
}

grid <- expand.grid(model = c("AO", "LPE", "RH"), n = N_GRID, rep = seq_len(REPS),
                     stringsAsFactors = FALSE) |>
  as_tibble() |>
  mutate(seed = 20260723 + row_number())

message("Fitting ", nrow(grid), " (model x N x rep) cells...")

plan(multisession, workers = 2)
cell_results <- future_pmap(
  list(grid$model, grid$n, grid$rep, grid$seed),
  fit_one_cell,
  .options = furrr_options(seed = TRUE)
)
plan(sequential)

convergence_df <- bind_rows(cell_results)

message("Convergence rate by model x N:")
convergence_df |>
  group_by(model, n) |>
  summarise(rep_converged = mean(converged[!duplicated(rep)]), .groups = "drop") |>
  print(n = Inf)

# Per (model, N): bias and RMSE of each parameter, pooling across items and
# replicates within converged fits only.
convergence_summary <- convergence_df |>
  filter(converged) |>
  group_by(model, n) |>
  summarise(
    n_fits         = n_distinct(rep),
    bias_shape     = mean(shape_est - shape_true),
    rmse_shape     = sqrt(mean((shape_est - shape_true)^2)),
    bias_a         = mean(a_est - a_true),
    rmse_a         = sqrt(mean((a_est - a_true)^2)),
    bias_d         = mean(d_est - d_true),
    rmse_d         = sqrt(mean((d_est - d_true)^2)),
    .groups = "drop"
  ) |>
  arrange(model, n)

print(convergence_summary)

dir.create("vignettes/asymmetric_irt_data", recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(
    convergence_df      = convergence_df,
    convergence_summary = convergence_summary,
    n_grid = N_GRID, j_items = J, reps = REPS, em_cycles = EM_CYCLES,
    date_run = Sys.Date(), session = sessionInfo()
  ),
  file = "vignettes/asymmetric_irt_data/convergence_sim.rds"
)

message("\nSaved to vignettes/asymmetric_irt_data/convergence_sim.rds")
