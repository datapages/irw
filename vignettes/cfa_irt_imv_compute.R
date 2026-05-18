## cfa_irt_imv_compute.R
## Pre-computes all results for vignettes/cfa_irt_imv.qmd.
## Run from project root: Rscript vignettes/cfa_irt_imv_compute.R
## Output: vignettes/cfairtdata/cfa_irt_imv_results.rds

suppressPackageStartupMessages({
  library(irw)
  library(lavaan)
  library(mirt)
  library(imv)
  library(dplyr)
})

# ----- Configuration --------------------------------------------------------
table_name <- "gilbert_meta_2"
nfold      <- 5
seed       <- 1234
out_path   <- "vignettes/cfairtdata/cfa_irt_imv_results.rds"

# ----- Data -----------------------------------------------------------------
cat("Fetching", table_name, "...\n")
df   <- irw::irw_fetch(table_name)
resp <- irw::irw_long2resp(df)
resp$id <- NULL

resp <- resp[, apply(resp, 2, function(x) all(x %in% c(0, 1, NA)))]
resp <- resp[complete.cases(resp), ]

N     <- nrow(resp)
items <- colnames(resp)
ni    <- length(items)
prev  <- colMeans(resp)

cat(sprintf("N = %d | J = %d | prevalence range: %.3f – %.3f\n",
            N, ni, min(prev), max(prev)))

# ----- Model strings --------------------------------------------------------
cfa_model <- paste0(
  "F =~ NA*", paste(items, collapse = " + "), "\n",
  "F ~~ 1*F"
)
mirt_model_str <- paste0(
  "F = 1-", ni, "\n",
  "PRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)"
)
mirt_model <- mirt::mirt.model(mirt_model_str)

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
  probs   <- probtrace(fit, theta_test)
  p1_cols <- seq(2, ncol(probs), by = 2)
  yprob   <- probs[, p1_cols, drop = FALSE]
  colnames(yprob) <- colnames(train_resp)
  yprob
}

# ----- Cross-validation -----------------------------------------------------
set.seed(seed)
folds <- sample(rep(1:nfold, length.out = N))

imv_cfa_folds <- matrix(NA, nrow = nfold, ncol = ni + 1,
                         dimnames = list(NULL, c(items, "scale")))
imv_irt_folds <- imv_cfa_folds
fold_errors   <- character(0)

for (k in 1:nfold) {
  cat(sprintf("Fold %d/%d ... ", k, nfold))
  test_idx  <- which(folds == k)
  train_idx <- which(folds != k)
  train_df  <- resp[train_idx, , drop = FALSE]
  test_df   <- resp[test_idx,  , drop = FALSE]

  yprob_cfa <- tryCatch(
    predict_cfa(train_df, test_df, cfa_model, items),
    error = function(e) {
      msg <- paste0("Fold ", k, " CFA error: ", conditionMessage(e))
      fold_errors <<- c(fold_errors, msg)
      NULL
    }
  )
  yprob_irt <- tryCatch(
    predict_irt(train_df, test_df, mirt_model),
    error = function(e) {
      msg <- paste0("Fold ", k, " IRT error: ", conditionMessage(e))
      fold_errors <<- c(fold_errors, msg)
      NULL
    }
  )

  if (is.null(yprob_cfa) || is.null(yprob_irt)) {
    cat("SKIPPED\n"); next
  }

  prev_train   <- colMeans(train_df, na.rm = TRUE)
  item_imv_cfa <- numeric(ni)
  item_imv_irt <- numeric(ni)

  for (j in seq_along(items)) {
    y_j <- test_df[, j]
    item_imv_cfa[j] <- imv.binary(y_j, rep(prev_train[j], length(y_j)), yprob_cfa[, j])
    item_imv_irt[j] <- imv.binary(y_j, rep(prev_train[j], length(y_j)), yprob_irt[, j])
  }

  imv_cfa_folds[k, ] <- c(item_imv_cfa, mean(item_imv_cfa))
  imv_irt_folds[k, ] <- c(item_imv_irt, mean(item_imv_irt))
  cat(sprintf("CFA = %.4f | IRT = %.4f\n",
              mean(item_imv_cfa), mean(item_imv_irt)))
}

# ----- Discrimination parameters (full-data 2PL) ----------------------------
cat("Fitting 2PL on full dataset for discrimination analysis ...\n")
fit_full <- mirt(resp, mirt_model,
                 itemtype  = rep("2PL", ni),
                 method    = "EM",
                 technical = list(NCYCLES = 10000),
                 verbose   = FALSE)
a_params <- coef(fit_full, IRTpars = TRUE, simplify = TRUE)$items[, "a"]

# ----- Summary stats --------------------------------------------------------
valid_folds <- which(!is.na(imv_cfa_folds[, "scale"]))
K           <- length(valid_folds)

scale_cfa_mean <- mean(imv_cfa_folds[valid_folds, "scale"])
scale_irt_mean <- mean(imv_irt_folds[valid_folds, "scale"])
scale_cfa_sd   <- sd(imv_cfa_folds[valid_folds, "scale"])
scale_irt_sd   <- sd(imv_irt_folds[valid_folds, "scale"])

cat(sprintf("\nScale-level IMV  |  CFA: %.4f (±%.4f)  |  IRT: %.4f (±%.4f)\n",
            scale_cfa_mean, 1.96 * scale_cfa_sd / sqrt(K),
            scale_irt_mean, 1.96 * scale_irt_sd / sqrt(K)))
cat(sprintf("IRT advantage: %.4f\n", scale_irt_mean - scale_cfa_mean))

# ----- Save -----------------------------------------------------------------
results <- list(
  table_name     = table_name,
  N              = N,
  J              = ni,
  items          = items,
  prev_range     = range(prev),
  nfold          = nfold,
  seed           = seed,
  imv_cfa_folds  = imv_cfa_folds,
  imv_irt_folds  = imv_irt_folds,
  valid_folds    = valid_folds,
  fold_errors    = fold_errors,
  a_params       = a_params,
  scale_cfa_mean = scale_cfa_mean,
  scale_irt_mean = scale_irt_mean,
  scale_cfa_sd   = scale_cfa_sd,
  scale_irt_sd   = scale_irt_sd,
  date_run       = Sys.Date()
)

saveRDS(results, out_path)
cat("Saved to", out_path, "\n")
