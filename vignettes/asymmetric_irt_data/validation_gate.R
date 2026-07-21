# validation_gate.R
#
# Mandatory validation gate for the asymmetric-IRT vignette, run BEFORE any
# real IRW table is touched. Four checks, per the vignette spec:
#   (a) At the symmetric-baseline shape parameter, each custom P(theta)
#       function must reproduce the standard reference model exactly
#       (2PLM for AO/LPE; 2-parameter normal-ogive for RH, since RH is
#       probit-linked throughout).
#   (b) LPE curves at different kappa must never cross (a proved property
#       of the model, paper Section 2.1) -- a cheap, strong correctness
#       check on the P function itself.
#   (c) A small simulation study: simulate data from each model at known,
#       clearly asymmetric shape parameters, fit the corresponding custom
#       item, and confirm recovery is in the right ballpark.
#   (d) Out-of-sample predictive (IMV) sanity check: confirm the
#       response-level holdout + imv::imv.binary pipeline used by
#       asymmetric_irt_compute.R runs correctly for custom item types and
#       is unbiased under a genuinely symmetric truth.
#
# Output: asymmetric_irt_data/validation_gate_log.rds
#
# Usage:
#   Rscript vignettes/asymmetric_irt_data/validation_gate.R   # from project root

source("vignettes/asymmetric_irt_helpers.R")
library(dplyr)
library(tibble)
library(imv)

set.seed(20260720)

# ==============================================================================
# (a) kappa-reduces-to-baseline check
# ==============================================================================

theta_grid <- seq(-6, 6, length.out = 401)
a_check <- 1.3
d_check <- -0.4

p_ao_base  <- AO_def$P(c(a1 = a_check, d = d_check, eta = 0), theta_grid, 2)[, 2]
p_lpe_base <- LPE_def$P(c(a1 = a_check, d = d_check, S = 0), theta_grid, 2)[, 2]
p_rh_base  <- RH_def$P(c(a1 = a_check, d = d_check, delta = 0), theta_grid, 2)[, 2]

p_2pl_ref <- plogis(a_check * theta_grid + d_check)
p_ono_ref <- pnorm(a_check * theta_grid + d_check)

reduction_check <- tibble(
  model = c("AO", "LPE", "RH"),
  reference = c("2PLM (logistic)", "2PLM (logistic)", "2-parameter normal-ogive (probit)"),
  max_abs_diff = c(
    max(abs(p_ao_base - p_2pl_ref)),
    max(abs(p_lpe_base - p_2pl_ref)),
    max(abs(p_rh_base - p_ono_ref))
  )
)
reduction_check$pass <- reduction_check$max_abs_diff < 1e-8

message("=== (a) Baseline reduction check ===")
print(reduction_check)

# ==============================================================================
# (b) LPE non-crossing check
# ==============================================================================

kappa_vals <- c(0.25, 0.5, 0.75, 1, 2, 3)
lpe_curves <- sapply(kappa_vals, function(k) {
  LPE_def$P(c(a1 = 1, d = 0, S = log(k)), theta_grid, 2)[, 2]
})
colnames(lpe_curves) <- paste0("k", kappa_vals)

baseline_col <- which(kappa_vals == 1)
below1 <- kappa_vals < 1
above1 <- kappa_vals > 1

# For kappa < 1, curve must sit >= the kappa=1 curve at every theta point;
# for kappa > 1, curve must sit <= the kappa=1 curve at every theta point.
noncrossing_ok <- TRUE
noncrossing_detail <- list()
for (j in which(below1)) {
  ok <- all(lpe_curves[, j] >= lpe_curves[, baseline_col] - 1e-10)
  noncrossing_detail[[paste0("k", kappa_vals[j], "_vs_k1")]] <- ok
  noncrossing_ok <- noncrossing_ok && ok
}
for (j in which(above1)) {
  ok <- all(lpe_curves[, j] <= lpe_curves[, baseline_col] + 1e-10)
  noncrossing_detail[[paste0("k", kappa_vals[j], "_vs_k1")]] <- ok
  noncrossing_ok <- noncrossing_ok && ok
}

message("\n=== (b) LPE non-crossing check ===")
message("All comparisons pass: ", noncrossing_ok)
print(noncrossing_detail)

# ==============================================================================
# (c) Simulation study: recover known, clearly asymmetric shape parameters
# ==============================================================================

N_PERSONS <- 2000  # matches the n_participants >= 1000 floor planned for real IRW tables
N_ITEMS   <- 20

simulate_one_model <- function(model_type, seed) {
  set.seed(seed)
  theta_persons <- rnorm(N_PERSONS)

  a_true <- runif(N_ITEMS, 0.7, 1.8)
  d_true <- runif(N_ITEMS, -1.5, 1.5)
  # Spread of clearly asymmetric shape values, plus a couple near-symmetric
  # items, on each model's *natural* (unconstrained estimation) scale.
  shape_true <- switch(model_type,
    AO  = c(seq(-1.2, 1.2, length.out = N_ITEMS - 2), 0, 0.05),
    LPE = c(seq(-1.2, 1.2, length.out = N_ITEMS - 2), 0, -0.05),
    RH  = c(seq(-2.5, 2.5, length.out = N_ITEMS - 2), 0, 0.1)
  )
  shape_true <- sample(shape_true)  # don't correlate item order with truth

  def <- MODEL_DEFS[[model_type]]
  dat <- matrix(NA_integer_, N_PERSONS, N_ITEMS)
  for (j in seq_len(N_ITEMS)) {
    par_j <- switch(model_type,
      AO  = c(a1 = a_true[j], d = d_true[j], eta = shape_true[j]),
      LPE = c(a1 = a_true[j], d = d_true[j], S = shape_true[j]),
      RH  = c(a1 = a_true[j], d = d_true[j], delta = shape_true[j])
    )
    p1 <- def$P(par_j, theta_persons, 2)[, 2]
    dat[, j] <- rbinom(N_PERSONS, 1, p1)
  }
  colnames(dat) <- paste0("item_", seq_len(N_ITEMS))

  list(dat = as.data.frame(dat), a_true = a_true, d_true = d_true, shape_true = shape_true)
}

recover_one_model <- function(model_type, seed) {
  message("  Simulating + fitting: ", model_type)
  sim <- simulate_one_model(model_type, seed)

  fit_2pl <- mirt(sim$dat, 1, itemtype = "2PL", verbose = FALSE,
                   technical = list(NCYCLES = 2000))
  ad_2pl <- extract_ad(fit_2pl)
  ad_start <- if (model_type == "RH") convert_ad_logit_to_probit(ad_2pl, D = 1.702) else ad_2pl

  fit <- fit_custom(sim$dat, model_type, make_custom_item(model_type),
                     ad_start = ad_start, shape_init = 0, prior_sd = 1)

  if (!has_valid_mod(fit)) {
    return(list(model = model_type, converged = FALSE, message = fit$message, recovery = NULL))
  }

  recovered <- extract_param_table(fit$mod)
  shape_name <- shape_par_name(model_type)
  recovery <- tibble(
    model = model_type,
    item = seq_len(N_ITEMS),
    a_true = sim$a_true, d_true = sim$d_true, shape_true = sim$shape_true,
    shape_est = recovered[[shape_name]]
  )
  list(model = model_type, converged = isTRUE(fit$mod@OptimInfo$converged),
       message = NA_character_, recovery = recovery)
}

message("\n=== (c) Simulation recovery study ===")
recovery_results <- lapply(c("AO", "LPE", "RH"), recover_one_model, seed = 20260720)
names(recovery_results) <- c("AO", "LPE", "RH")

recovery_df <- bind_rows(lapply(recovery_results, function(r) r$recovery))
recovery_summary <- recovery_df %>%
  group_by(model) %>%
  summarise(
    cor_shape = cor(shape_true, shape_est),
    mean_abs_error = mean(abs(shape_true - shape_est)),
    .groups = "drop"
  )

print(recovery_summary)

# ==============================================================================
# (d) Out-of-sample predictive (IMV) sanity check
#
# The real-data batch (asymmetric_irt_compute.R) now compares models via
# response-level held-out IMV (Stenhaug & Domingue, 2022 -- mask individual
# person-item cells, not whole persons, not k-fold) rather than in-sample
# BIC. Before trusting that pipeline at batch scale, confirm two things on
# synthetic data where the truth is known:
#   (i)  it runs end to end for a custom item type with NA-masked training
#        data (fit -> fscores on masked data -> extract.item/probtrace on
#        held-out cells -> imv::imv.binary);
#   (ii) when the data-generating truth is genuinely symmetric (kappa = the
#        model's own baseline value), the resulting held-out IMV is close to
#        zero on average -- i.e. the estimator isn't systematically biased
#        toward finding an advantage (or penalty) that isn't there.
# This does NOT assert that a real asymmetric effect must show a positive
# held-out IMV -- whether it does is an empirical question for real data,
# not a property to enforce here. (In fact, synthetic runs with genuine,
# realistic-magnitude asymmetry show held-out IMV can be flat or even
# *negative* despite a large in-sample BIC preference for the asymmetric
# model -- consistent with the extra shape parameter adding estimation
# noise that partly or fully offsets its true signal at this data scale.
# That tension is exactly the motivation for using IMV instead of BIC on
# the real batch, not a failure of this check.)
# ==============================================================================

mask_holdout <- function(resp, frac = 0.2) {
  resp_train <- resp
  mat <- as.matrix(resp)
  n_obs_per_person <- rowSums(!is.na(mat))
  # Mask per person, always leaving at least one observed response -- see
  # the matching comment in asymmetric_irt_compute.R's mask_holdout() for
  # why (real tables can strand a person with zero retained responses).
  mask_list <- vector("list", nrow(mat))
  for (i in seq_len(nrow(mat))) {
    k <- n_obs_per_person[i]
    if (k < 2) next
    obs_cols <- which(!is.na(mat[i, ]))
    n_mask_i <- min(floor(frac * k), k - 1)
    if (n_mask_i < 1) next
    mask_list[[i]] <- cbind(row = i, col = sample(obs_cols, n_mask_i))
  }
  mask_idx <- do.call(rbind, mask_list)
  true_vals <- mat[mask_idx]
  for (k in seq_len(nrow(mask_idx))) resp_train[mask_idx[k, 1], mask_idx[k, 2]] <- NA
  list(train = resp_train, mask_idx = mask_idx, true_vals = true_vals)
}

# Vectorized per-item (not per-cell) held-out prediction: one probtrace()
# call per item across all held-out persons for that item.
heldout_preds <- function(fit, theta_vec, mask_idx) {
  preds <- numeric(nrow(mask_idx))
  for (j in unique(mask_idx[, 2])) {
    rows    <- which(mask_idx[, 2] == j)
    persons <- mask_idx[rows, 1]
    it <- extract.item(fit, j)
    preds[rows] <- probtrace(it, matrix(theta_vec[persons], ncol = 1))[, "P.1"]
  }
  preds
}

imv_sanity_one <- function(model_type, truth = c("null", "asymmetric"), seed, N = 6000, J = 15) {
  truth <- match.arg(truth)
  set.seed(seed)
  def <- MODEL_DEFS[[model_type]]
  shape_name <- shape_par_name(model_type)
  baseline_shape <- if (model_type == "RH") 0 else 1  # kappa=0 (RH) / kappa=1 (LPE,AO) = symmetric
  # Estimation-scale shape values: log(kappa) for AO/LPE, kappa directly for RH
  shape_est_scale <- function(kappa) if (model_type == "RH") kappa else log(kappa)

  theta <- rnorm(N)
  a_true <- runif(J, 0.8, 1.6)
  d_true <- runif(J, -1, 1)
  kappa_true <- if (truth == "null") {
    rep(baseline_shape, J)
  } else {
    switch(model_type,
           RH  = runif(J, -3, -1.5),
           LPE = runif(J, 1.5, 3),
           AO  = runif(J, 1.5, 3))
  }
  dat <- matrix(NA_integer_, N, J)
  for (j in seq_len(J)) {
    par_j <- setNames(c(a_true[j], d_true[j], shape_est_scale(kappa_true[j])),
                       c("a1", "d", shape_name))
    p1 <- def$P(par_j, theta, 2)[, 2]
    dat[, j] <- rbinom(N, 1, p1)
  }
  colnames(dat) <- paste0("item_", seq_len(J))
  resp <- as.data.frame(dat)

  ho <- mask_holdout(resp, 0.2)

  fit_2pl <- mirt(ho$train, 1, itemtype = "2PL", verbose = FALSE,
                   technical = list(NCYCLES = 1000, customTheta = Theta_mat_ref, customPriorFun = prior_GH50))
  ad_2pl   <- extract_ad(fit_2pl)
  ad_start <- if (model_type == "RH") convert_ad_logit_to_probit(ad_2pl, D = 1.702) else ad_2pl
  out <- tryCatch(
    fit_custom(ho$train, model_type, make_custom_item(model_type),
               ad_start = ad_start, shape_init = 0, prior_sd = 1, em_cycles = 1000),
    error = function(e) list(mod = NULL, error = TRUE, message = conditionMessage(e))
  )
  if (!has_valid_mod(out)) return(c(imv = NA_real_, converged = FALSE))
  fit_asym <- out$mod

  th_2pl  <- fscores(fit_2pl, method = "EAP")[, 1]
  th_asym <- fscores(fit_asym, method = "EAP")[, 1]

  p_2pl  <- heldout_preds(fit_2pl, th_2pl, ho$mask_idx)
  p_asym <- heldout_preds(fit_asym, th_asym, ho$mask_idx)

  c(imv = imv.binary(ho$true_vals, p_2pl, p_asym),
    converged = isTRUE(fit_asym@OptimInfo$converged))
}

message("\n=== (d) Out-of-sample predictive (IMV) sanity check ===")
N_IMV_REPS <- 5
imv_sanity <- expand.grid(model = c("AO", "LPE", "RH"), truth = c("null", "asymmetric"),
                           stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(reps = list(sapply(seq_len(N_IMV_REPS), function(s) {
    imv_sanity_one(model, truth, seed = 20260720 + s)["imv"]
  }))) %>%
  ungroup() %>%
  mutate(mean_imv = sapply(reps, mean, na.rm = TRUE),
         sd_imv   = sapply(reps, sd, na.rm = TRUE))

print(imv_sanity %>% select(model, truth, mean_imv, sd_imv))

# Calibration check: under a genuinely symmetric truth, mean held-out IMV
# for each model vs. 2PLM should be small in magnitude (no systematic bias
# in the holdout/prediction pipeline itself). This threshold is descriptive,
# not a precision claim -- it's checking "not obviously broken", not
# certifying a specific effect size.
imv_null_ok <- all(abs(imv_sanity$mean_imv[imv_sanity$truth == "null"]) < 0.01)
message("Null-truth calibration OK (|mean IMV| < 0.01 for all models): ", imv_null_ok)

# ==============================================================================
# Save the full gate log
# ==============================================================================

dir.create("vignettes/asymmetric_irt_data", recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(
    reduction_check    = reduction_check,
    noncrossing_ok     = noncrossing_ok,
    noncrossing_detail = noncrossing_detail,
    lpe_curves         = lpe_curves,
    theta_grid          = theta_grid,
    kappa_vals         = kappa_vals,
    recovery_df        = recovery_df,
    recovery_summary   = recovery_summary,
    convergence        = sapply(recovery_results, function(r) r$converged),
    imv_sanity         = imv_sanity,
    imv_null_ok        = imv_null_ok,
    date_run           = Sys.Date(),
    session            = sessionInfo()
  ),
  file = "vignettes/asymmetric_irt_data/validation_gate_log.rds"
)

message("\nSaved to vignettes/asymmetric_irt_data/validation_gate_log.rds")

gate_pass <- all(reduction_check$pass) && noncrossing_ok && all(recovery_summary$cor_shape > 0.8) && imv_null_ok
message("\n=== GATE RESULT: ", if (gate_pass) "PASS" else "FAIL", " ===")
