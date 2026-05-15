## cfa_irt_imv_compute.R
## Pre-computes CFA vs. IRT IMV cross-validation results for 10 IRW datasets.
## Run from project root: Rscript vignettes/cfa_irt_imv_compute.R
## Output: vignettes/cfairtdata/cfa_irt_imv_results.rds

suppressPackageStartupMessages({
  library(irw)
  library(lavaan)
  library(mirt)
  library(imv)
  library(dplyr)
})

# ----- Datasets -------------------------------------------------------------
# Binary IRW datasets (n_categories=2, 10-30 items, N>=500, density>=0.8)
# selected to span cognitive and ability measurement contexts.
table_names <- c(
  "gilbert_meta_2",
  "gilbert_meta_1",
  "gilbert_meta_8",
  "gilbert_meta_12",
  "gilbert_meta_15",
  "gilbert_meta_37",
  "fims_tam",
  "psychtools_ability",
  "much_tte_2025_matrixreasoning",
  "naep_multilcirt"
)

# ----- CV parameters --------------------------------------------------------
nfold <- 5
seed  <- 1234

# ----- Prediction helpers ---------------------------------------------------
predict_cfa <- function(train, test, model, vary) {
  fit         <- cfa(model, data = train, ordered = vary,
                     parameterization = "delta", estimator = "DWLS")
  ov_pred     <- lavPredict(fit, newdata = test, type = "ov")
  y_threshold <- inspect(fit, what = "est")$tau
  y_residual  <- inspect(fit, what = "est")$theta
  yprob <- matrix(NA, nrow = nrow(test), ncol = length(vary),
                  dimnames = list(NULL, vary))
  for (j in seq_along(vary)) {
    num        <- ov_pred[, j] - y_threshold[j]
    yprob[, j] <- pnorm(num / sqrt(y_residual[j, j]))
  }
  yprob
}

predict_irt <- function(train_resp, test_resp, mirt_model) {
  fit <- mirt(train_resp, mirt_model,
              itemtype  = rep("2PL", ncol(train_resp)),
              method    = "EM",
              technical = list(NCYCLES = 10000),
              verbose   = FALSE)
  theta_test <- fscores(fit, response.pattern = test_resp, method = "EAP")[, 1]
  probs      <- probtrace(fit, theta_test)
  p1_cols    <- seq(2, ncol(probs), by = 2)
  yprob      <- probs[, p1_cols, drop = FALSE]
  colnames(yprob) <- colnames(train_resp)
  yprob
}

# ----- Per-dataset CV -------------------------------------------------------
run_dataset <- function(table_name) {
  cat(sprintf("\n=== %s ===\n", table_name))

  df <- tryCatch(irw::irw_fetch(table_name), error = function(e) {
    cat("  fetch failed:", conditionMessage(e), "\n"); NULL
  })
  if (is.null(df)) return(NULL)

  resp <- tryCatch({
    r <- irw::irw_long2resp(df)
    r$id <- NULL
    r <- r[, apply(r, 2, function(x) all(x %in% c(0, 1, NA))), drop = FALSE]
    r[complete.cases(r), , drop = FALSE]
  }, error = function(e) {
    cat("  prep failed:", conditionMessage(e), "\n"); NULL
  })
  if (is.null(resp) || nrow(resp) < 200 || ncol(resp) < 5) {
    cat("  skipped: insufficient data after prep\n"); return(NULL)
  }

  N     <- nrow(resp)
  items <- colnames(resp)
  ni    <- length(items)
  prev  <- colMeans(resp)
  cat(sprintf("  N=%d | J=%d | prev range: %.2f-%.2f\n",
              N, ni, min(prev), max(prev)))

  cfa_model <- paste0("F =~ NA*", paste(items, collapse = " + "), "\nF ~~ 1*F")
  mirt_model <- mirt::mirt.model(paste0(
    "F = 1-", ni, "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)"
  ))

  set.seed(seed)
  folds <- sample(rep(1:nfold, length.out = N))

  # imv(cfa, irt): IRT vs. CFA as baseline — the primary comparison
  # imv(prev, cfa) and imv(prev, irt): each model vs. prevalence — context
  imv_cfa_v_irt_folds <- matrix(NA, nrow = nfold, ncol = ni + 1,
                                  dimnames = list(NULL, c(items, "scale")))
  imv_cfa_v_prev_folds <- imv_cfa_v_irt_folds
  imv_irt_v_prev_folds <- imv_cfa_v_irt_folds
  fold_errors <- character(0)

  for (k in 1:nfold) {
    cat(sprintf("  Fold %d/%d ... ", k, nfold))
    test_idx  <- which(folds == k)
    train_idx <- which(folds != k)
    train_df  <- resp[train_idx, , drop = FALSE]
    test_df   <- resp[test_idx,  , drop = FALSE]

    yprob_cfa <- tryCatch(
      predict_cfa(train_df, test_df, cfa_model, items),
      error = function(e) {
        msg <- sprintf("Fold %d CFA: %s", k, conditionMessage(e))
        fold_errors <<- c(fold_errors, msg); NULL
      }
    )
    yprob_irt <- tryCatch(
      predict_irt(train_df, test_df, mirt_model),
      error = function(e) {
        msg <- sprintf("Fold %d IRT: %s", k, conditionMessage(e))
        fold_errors <<- c(fold_errors, msg); NULL
      }
    )

    if (is.null(yprob_cfa) || is.null(yprob_irt)) {
      cat("SKIPPED\n"); next
    }

    prev_train       <- colMeans(train_df, na.rm = TRUE)
    item_cfa_v_irt   <- numeric(ni)
    item_cfa_v_prev  <- numeric(ni)
    item_irt_v_prev  <- numeric(ni)

    for (j in seq_along(items)) {
      y_j              <- test_df[, j]
      prev_j           <- rep(prev_train[j], length(y_j))
      item_cfa_v_irt[j]  <- imv.binary(y_j, yprob_cfa[, j], yprob_irt[, j])
      item_cfa_v_prev[j] <- imv.binary(y_j, prev_j, yprob_cfa[, j])
      item_irt_v_prev[j] <- imv.binary(y_j, prev_j, yprob_irt[, j])
    }

    imv_cfa_v_irt_folds[k, ]  <- c(item_cfa_v_irt,  mean(item_cfa_v_irt))
    imv_cfa_v_prev_folds[k, ] <- c(item_cfa_v_prev, mean(item_cfa_v_prev))
    imv_irt_v_prev_folds[k, ] <- c(item_irt_v_prev, mean(item_irt_v_prev))
    cat(sprintf("IMV(CFA,IRT)=%.4f | CFA vs prev=%.4f | IRT vs prev=%.4f\n",
                mean(item_cfa_v_irt), mean(item_cfa_v_prev), mean(item_irt_v_prev)))
  }

  valid_folds <- which(!is.na(imv_cfa_v_irt_folds[, "scale"]))
  if (length(valid_folds) == 0) {
    cat("  all folds failed\n"); return(NULL)
  }

  K <- length(valid_folds)
  scale_cfa_v_irt_mean  <- mean(imv_cfa_v_irt_folds[valid_folds,  "scale"])
  scale_cfa_v_prev_mean <- mean(imv_cfa_v_prev_folds[valid_folds, "scale"])
  scale_irt_v_prev_mean <- mean(imv_irt_v_prev_folds[valid_folds, "scale"])
  scale_cfa_v_irt_sd    <- sd(imv_cfa_v_irt_folds[valid_folds,   "scale"])
  scale_cfa_v_prev_sd   <- sd(imv_cfa_v_prev_folds[valid_folds,  "scale"])
  scale_irt_v_prev_sd   <- sd(imv_irt_v_prev_folds[valid_folds,  "scale"])

  # Discrimination parameters from full-data 2PL
  a_params <- tryCatch({
    fit_full <- mirt(resp, mirt_model, itemtype = rep("2PL", ni),
                     method = "EM", technical = list(NCYCLES = 10000),
                     verbose = FALSE)
    coef(fit_full, IRTpars = TRUE, simplify = TRUE)$items[, "a"]
  }, error = function(e) rep(NA_real_, ni))

  list(
    table_name            = table_name,
    N                     = N,
    J                     = ni,
    items                 = items,
    prev_range            = range(prev),
    nfold                 = nfold,
    valid_folds           = valid_folds,
    fold_errors           = fold_errors,
    imv_cfa_v_irt_folds   = imv_cfa_v_irt_folds,
    imv_cfa_v_prev_folds  = imv_cfa_v_prev_folds,
    imv_irt_v_prev_folds  = imv_irt_v_prev_folds,
    a_params              = a_params,
    scale_cfa_v_irt_mean  = scale_cfa_v_irt_mean,
    scale_cfa_v_prev_mean = scale_cfa_v_prev_mean,
    scale_irt_v_prev_mean = scale_irt_v_prev_mean,
    scale_cfa_v_irt_sd    = scale_cfa_v_irt_sd,
    scale_cfa_v_prev_sd   = scale_cfa_v_prev_sd,
    scale_irt_v_prev_sd   = scale_irt_v_prev_sd
  )
}

# ----- Run all datasets -----------------------------------------------------
out_dir <- "vignettes/cfairtdata/fits"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (tbl in table_names) {
  out_file <- file.path(out_dir, paste0(tbl, ".rds"))
  if (file.exists(out_file)) {
    cat(sprintf("Skipping %s (cached)\n", tbl)); next
  }
  result <- run_dataset(tbl)
  if (!is.null(result)) saveRDS(result, out_file)
}

# ----- Combine results ------------------------------------------------------
dataset_results <- lapply(table_names, function(tbl) {
  f <- file.path(out_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
})
names(dataset_results) <- table_names
dataset_results <- Filter(Negate(is.null), dataset_results)

# Dataset-level summary table
dataset_summary <- do.call(rbind, lapply(dataset_results, function(r) {
  K <- length(r$valid_folds)
  data.frame(
    table_name            = r$table_name,
    N                     = r$N,
    J                     = r$J,
    scale_cfa_v_irt_mean  = r$scale_cfa_v_irt_mean,
    scale_cfa_v_irt_sd    = r$scale_cfa_v_irt_sd,
    scale_cfa_v_prev_mean = r$scale_cfa_v_prev_mean,
    scale_irt_v_prev_mean = r$scale_irt_v_prev_mean,
    scale_cfa_v_prev_sd   = r$scale_cfa_v_prev_sd,
    scale_irt_v_prev_sd   = r$scale_irt_v_prev_sd,
    K                     = K,
    stringsAsFactors = FALSE
  )
}))

cat("\n=== Summary across datasets ===\n")
print(dataset_summary[, c("table_name", "N", "J",
                           "scale_cfa_v_irt_mean",
                           "scale_cfa_v_prev_mean",
                           "scale_irt_v_prev_mean")],
      digits = 4, row.names = FALSE)

# ----- Save -----------------------------------------------------------------
saveRDS(
  list(
    table_names      = names(dataset_results),
    dataset_results  = dataset_results,
    dataset_summary  = dataset_summary,
    nfold            = nfold,
    seed             = seed,
    date_run         = Sys.Date()
  ),
  "vignettes/cfairtdata/cfa_irt_imv_results.rds"
)
cat("\nSaved to vignettes/cfairtdata/cfa_irt_imv_results.rds\n")
