# mixed_subjects_irt_test_candidates.R
#
# Test whether gilbert_meta_7 and gilbert_meta_11 produce lambda > 0
# with a sparse pilot (n=60). Run before updating the vignette.
#
# Requires: REDIVIS_API_TOKEN
# Usage (from project root):
#   Rscript vignettes/mixed_subjects_irt_test_meta11.R

library(irw)
library(mixedsubjectsirt)
library(dplyr)

N_SYNTH <- 1000

# ── Text-feature predictions (leave-one-out from cached item text data) ────────
cache    <- readRDS("vignettes/itemtextdata/item_text_difficulty_results.rds")
all_data <- cache$all_data

all_data_std <- all_data |>
  group_by(table) |>
  mutate(
    wc_z  = (word_count   - mean(word_count))   / (sd(word_count)   + 1e-6),
    awl_z = (avg_word_len - mean(avg_word_len)) / (sd(avg_word_len) + 1e-6)
  ) |>
  ungroup()

run_target <- function(TARGET, pilot_n = 60) {
  message("\n====== ", TARGET, " | pilot_n=", pilot_n, " ======")
  PILOT_N <- pilot_n
  tryCatch(run_target_inner(TARGET, PILOT_N), error = function(e) message("ERROR: ", e$message))
}

run_target_inner <- function(TARGET, PILOT_N) {

  train_data  <- all_data_std |> filter(table != TARGET)
  target_data <- all_data_std |> filter(table == TARGET)

  lm_fit  <- lm(prop_correct ~ wc_z + awl_z, data = train_data)
  r_text  <- cor(predict(lm_fit, newdata = target_data), target_data$prop_correct)
  message(sprintf("Text-feature r: %.3f", r_text))

  target_data <- target_data |>
    mutate(
      pred_p = pmin(pmax(predict(lm_fit, newdata = target_data), 0.05), 0.95),
      pred_b = -log(pred_p / (1 - pred_p))
    )

  # ── Fetch real response data ─────────────────────────────────────────────────
  message("Fetching from Redivis...")
  df           <- irw_fetch(TARGET)
  human_wide   <- irw_long2resp(df)
  human_wide   <- human_wide[, !colnames(human_wide) %in% c("id", "ID"), drop = FALSE]
  colnames(human_wide) <- sub("^item_", "", colnames(human_wide))
  human_matrix <- as.matrix(human_wide)
  storage.mode(human_matrix) <- "integer"
  message("Human data: ", nrow(human_matrix), " x ", ncol(human_matrix), " items")

  shared_items <- intersect(target_data$item, colnames(human_matrix))
  human_matrix <- human_matrix[, shared_items, drop = FALSE]

  # ── Pilot + synthetic subjects ───────────────────────────────────────────────
  set.seed(42)
  pilot_matrix <- human_matrix[sample(nrow(human_matrix), PILOT_N), ]

  set.seed(2024)
  theta_syn    <- rnorm(N_SYNTH, 0, 1)
  b_pred       <- setNames(target_data$pred_b, target_data$item)[shared_items]
  synth_matrix <- matrix(0L, nrow = N_SYNTH, ncol = length(shared_items),
                         dimnames = list(NULL, shared_items))
  for (j in seq_along(shared_items))
    synth_matrix[, j] <- as.integer(rbinom(N_SYNTH, 1, plogis(theta_syn - b_pred[j])))

  # ── Lambda tuning ────────────────────────────────────────────────────────────
  result <- tune_lambda_ability_risk_1pl(
    lambda_grid = seq(0, 1, by = 0.05),
    observed    = pilot_matrix,
    predicted   = synth_matrix[seq_len(PILOT_N), ],
    generated   = synth_matrix
  )
  message(sprintf("Best lambda: %.3f", result$best_lambda))

  # ── RMSE ─────────────────────────────────────────────────────────────────────
  gt_fit       <- fit_1pl(human_matrix)
  pilot_fit    <- fit_1pl(pilot_matrix)
  pilot_linked <- link_item_parameters(pilot_fit$pars,                   gt_fit$pars)$pars
  mixed_linked <- link_item_parameters(result$best_fit$item_pars, gt_fit$pars)$pars

  gt_b    <- gt_fit$pars   |> arrange(item) |> pull(b)
  pilot_b <- pilot_linked  |> arrange(item) |> pull(b)
  mixed_b <- mixed_linked  |> arrange(item) |> pull(b)
  rmse_p  <- sqrt(mean((pilot_b - gt_b)^2, na.rm = TRUE))
  rmse_m  <- sqrt(mean((mixed_b - gt_b)^2, na.rm = TRUE))
  message(sprintf("RMSE — pilot-only: %.3f   mixed: %.3f   (improvement: %.1f%%)",
                  rmse_p, rmse_m, 100 * (rmse_p - rmse_m) / rmse_p))

  print(result$summary |> select(lambda, selection_risk))
} # end run_target_inner

run_target("florida_twins_auth", pilot_n = 40)
run_target("florida_twins_auth", pilot_n = 60)
run_target("gilbert_meta_78",    pilot_n = 40)
run_target("gilbert_meta_78",    pilot_n = 60)
