# mixed_subjects_irt_compute.R
#
# Proof-of-concept: text-feature-predicted item difficulties as synthetic
# calibration subjects on preschool_sel_box, using the mixed-subjects IRT
# framework (Kanopka et al.).
#
# Connects to the item_text_difficulty vignette: text features (word count,
# avg word length) predict item difficulty across IRW datasets. Here we test
# whether those predictions can serve as synthetic calibration data to augment
# a small human pilot.
#
# Requires: REDIVIS_API_TOKEN (for irw_fetch)
# Output:   vignettes/mixed_subjects_data/results.rds
#
# Usage (from project root):
#   Rscript vignettes/mixed_subjects_irt_compute.R

library(irw)
library(mixedsubjectsirt)
library(dplyr)

TARGET <- "preschool_sel_box"

# ------------------------------------------------------------------------------
# Part A: Simulation showing λ>0 is achievable in ideal conditions
# ------------------------------------------------------------------------------

message("=== Part A: Simulation (ideal conditions) ===")
set.seed(1)

n_items_sim  <- 20
n_human_sim  <- 200
n_synth_sim  <- 2000
b_sim        <- seq(-2, 2, length.out = n_items_sim)   # wide, even spread
theta_h      <- rnorm(n_human_sim, 0, 1)
theta_s      <- rnorm(n_synth_sim, 0.5, 1)             # slight ability offset

sim_human <- matrix(0L, nrow = n_human_sim, ncol = n_items_sim,
                    dimnames = list(NULL, paste0("i", seq_len(n_items_sim))))
sim_synth <- matrix(0L, nrow = n_synth_sim, ncol = n_items_sim,
                    dimnames = list(NULL, paste0("i", seq_len(n_items_sim))))
for (j in seq_len(n_items_sim)) {
  sim_human[, j] <- as.integer(rbinom(n_human_sim, 1, plogis(theta_h - b_sim[j])))
  sim_synth[, j] <- as.integer(rbinom(n_synth_sim, 1, plogis(theta_s - b_sim[j])))
}

sim_result <- tune_lambda_ability_risk_1pl(
  lambda_grid = seq(0, 1, by = 0.05),
  observed    = sim_human,
  predicted   = sim_synth[seq_len(n_human_sim), ],
  generated   = sim_synth
)
message("Simulation best lambda: ", round(sim_result$best_lambda, 4))

# ------------------------------------------------------------------------------
# Part B: Text-feature difficulty predictions (from itemtext vignette)
# ------------------------------------------------------------------------------

message("\n=== Part B: Text-feature predictions for ", TARGET, " ===")
cache    <- readRDS("vignettes/itemtextdata/item_text_difficulty_results.rds")
all_data <- cache$all_data

# Standardize features within each dataset so the regression generalises
# across domains (raw word counts mean different things in different datasets)
all_data_std <- all_data |>
  group_by(table) |>
  mutate(
    wc_z  = (word_count   - mean(word_count))   / (sd(word_count)   + 1e-6),
    awl_z = (avg_word_len - mean(avg_word_len)) / (sd(avg_word_len) + 1e-6)
  ) |>
  ungroup()

train_data  <- all_data_std |> filter(table != TARGET)
target_data <- all_data_std |> filter(table == TARGET)

lm_fit <- lm(prop_correct ~ wc_z + awl_z, data = train_data)
r_text <- cor(predict(lm_fit, newdata = target_data), target_data$prop_correct)
message(sprintf("Cross-dataset regression r=%.3f on target items", r_text))

target_data <- target_data |>
  mutate(
    pred_p = pmin(pmax(predict(lm_fit, newdata = target_data), 0.05), 0.95),
    pred_b = -log(pred_p / (1 - pred_p))
  )

# ------------------------------------------------------------------------------
# Part C: Human pilot + synthetic subjects
# ------------------------------------------------------------------------------

message("Fetching ", TARGET, " from Redivis...")
df <- irw_fetch(TARGET)
human_wide   <- irw_long2resp(df)
human_wide   <- human_wide[, !colnames(human_wide) %in% c("id", "ID"), drop = FALSE]
colnames(human_wide) <- sub("^item_", "", colnames(human_wide))
human_matrix <- as.matrix(human_wide)
storage.mode(human_matrix) <- "integer"
message("Human data: ", nrow(human_matrix), " x ", ncol(human_matrix))

set.seed(42)
pilot_n      <- 100
pilot_idx    <- sample(nrow(human_matrix), pilot_n)
pilot_matrix <- human_matrix[pilot_idx, ]

# Simulate synthetic subjects using text-predicted b values
set.seed(2024)
n_synth   <- 300
theta_syn <- rnorm(n_synth, mean = 0, sd = 1)

shared_items <- intersect(target_data$item, colnames(human_matrix))
b_pred       <- setNames(target_data$pred_b, target_data$item)[shared_items]

synth_matrix <- matrix(0L, nrow = n_synth, ncol = length(shared_items),
                       dimnames = list(NULL, shared_items))
for (j in seq_along(shared_items)) {
  synth_matrix[, j] <- as.integer(rbinom(n_synth, 1, plogis(theta_syn - b_pred[j])))
}

human_matrix  <- human_matrix[,  shared_items, drop = FALSE]
pilot_matrix  <- pilot_matrix[,  shared_items, drop = FALSE]

# ------------------------------------------------------------------------------
# Part D: Fit IRT models
# ------------------------------------------------------------------------------

message("Fitting ground-truth model (full human data, 1PL)...")
gt_fit <- fit_1pl(human_matrix)

message("Fitting pilot-only model (n=", pilot_n, ", 1PL)...")
pilot_fit <- fit_1pl(pilot_matrix)

message("Tuning lambda (1PL, ability-risk criterion)...")
mixed_result <- tune_lambda_ability_risk_1pl(
  lambda_grid = seq(0, 1, by = 0.05),
  observed    = pilot_matrix,
  predicted   = synth_matrix[seq_len(pilot_n), ],
  generated   = synth_matrix
)
lambda_final <- mixed_result$best_lambda
message("Tuned lambda: ", round(lambda_final, 4))

# ------------------------------------------------------------------------------
# Part E: Scale linking and RMSE
# ------------------------------------------------------------------------------

pilot_linked <- link_item_parameters(pilot_fit$pars,                  gt_fit$pars)$pars
mixed_linked <- link_item_parameters(mixed_result$best_fit$item_pars, gt_fit$pars)$pars

gt_b    <- gt_fit$pars  |> arrange(item) |> pull(b)
pilot_b <- pilot_linked |> arrange(item) |> pull(b)
mixed_b <- mixed_linked |> arrange(item) |> pull(b)

rmse_pilot <- sqrt(mean((pilot_b - gt_b)^2, na.rm = TRUE))
rmse_mixed <- sqrt(mean((mixed_b - gt_b)^2, na.rm = TRUE))
message(sprintf("RMSE — pilot-only: %.3f  mixed: %.3f", rmse_pilot, rmse_mixed))

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

out_dir <- "vignettes/mixed_subjects_data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(
  list(
    target              = TARGET,
    # Ground truth
    gt_pars             = gt_fit$pars,
    # Pilot-only
    pilot_pars          = pilot_linked,
    # Mixed-subjects
    mixed_pars          = mixed_linked,
    lambda_final        = lambda_final,
    lambda_summary      = mixed_result$summary,
    # Text predictions
    target_data         = target_data,
    lm_fit              = lm_fit,
    r_text              = r_text,
    # Accuracy comparison
    human_accuracy      = colMeans(human_matrix, na.rm = TRUE),
    synth_accuracy      = colMeans(synth_matrix, na.rm = TRUE),
    # Model comparison
    rmse_pilot          = rmse_pilot,
    rmse_mixed          = rmse_mixed,
    # Sample sizes
    pilot_n             = pilot_n,
    n_synth             = n_synth,
    n_human_total       = nrow(human_matrix),
    # Simulation (ideal conditions, λ>0)
    sim_lambda_final    = sim_result$best_lambda,
    sim_lambda_summary  = sim_result$summary,
    sim_b_true          = b_sim,
    date_run            = Sys.Date()
  ),
  file = file.path(out_dir, "results.rds")
)
message("Saved to ", file.path(out_dir, "results.rds"))
