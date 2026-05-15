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
nfold    <- 5
seed     <- 1234
nfold_mr <- 5
seed_mr  <- 5678

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

# ----- MR prediction helpers ------------------------------------------------
# CFA: fit on data_with_nas (DWLS handles pairwise NAs via pairwise polychoric
# correlations), then compute factor scores for each person using only their
# held-in items via the probit regression formula — avoids lavPredict with NAs.
predict_cfa_mr <- function(data_with_nas, items, cfa_model) {
  df  <- as.data.frame(data_with_nas)
  fit <- tryCatch(
    cfa(cfa_model, data = df, ordered = items,
        parameterization = "delta", estimator = "DWLS",
        missing = "pairwise"),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  est <- tryCatch(inspect(fit, what = "est"), error = function(e) NULL)
  if (is.null(est)) return(NULL)

  lam <- as.numeric(est$lambda[, 1])  # factor loadings
  tau <- as.numeric(est$tau)          # thresholds
  psi <- diag(est$theta)             # residual variances

  N <- nrow(df)
  J <- length(items)

  # For each person, regress factor score onto their observed (held-in) items.
  # The probit conditional expectation of y*_j given binary y_j is the
  # standard truncated-normal formula (Mills ratio).
  theta_hat <- vapply(seq_len(N), function(i) {
    y_i <- as.numeric(df[i, ])
    obs <- which(!is.na(y_i))
    if (length(obs) == 0L) return(0)
    tau_o <- tau[obs]; psi_o <- psi[obs]; lam_o <- lam[obs]
    phi_t  <- dnorm(tau_o)
    Phi_t  <- pnorm(tau_o)
    y_star <- ifelse(y_i[obs] == 1,
                     tau_o + phi_t / pmax(Phi_t, 1e-8),
                     tau_o - phi_t / pmax(1 - Phi_t, 1e-8))
    A <- sum(lam_o^2 / psi_o)
    if (A < 1e-10) return(0)
    sum(lam_o * (y_star - tau_o) / psi_o) / A
  }, numeric(1L))

  # Predicted P(Y=1) for all items from the factor score
  probs <- matrix(NA_real_, N, J, dimnames = list(NULL, items))
  for (j in seq_len(J)) {
    probs[, j] <- pnorm((lam[j] * theta_hat - tau[j]) / sqrt(psi[j]))
  }
  probs
}

# IRT: fit on data_with_nas (mirt EM marginalises over NAs), estimate EAP
# from each person's held-in items, predict all items.
predict_irt_mr <- function(data_with_nas, items, mirt_model) {
  fit <- tryCatch(
    mirt(as.data.frame(data_with_nas), mirt_model,
         itemtype  = rep("2PL", length(items)),
         method    = "EM",
         technical = list(NCYCLES = 10000),
         verbose   = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  theta     <- fscores(fit, response.pattern = as.matrix(data_with_nas),
                       method = "EAP")[, 1]
  probs_all <- probtrace(fit, theta)
  p1_cols   <- seq(2, ncol(probs_all), by = 2)
  probs     <- probs_all[, p1_cols, drop = FALSE]
  colnames(probs) <- items
  probs
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

# ----- Missing-response (item-response-level) CV ----------------------------
# Fold assignment is at the (person × item) cell level, stratified by person
# so every person contributes to every fold.  Within each fold, held-out cells
# are set to NA; both models are refit on that training matrix.
run_dataset_mr <- function(table_name) {
  cat(sprintf("\n=== MR: %s ===\n", table_name))

  df <- tryCatch(irw::irw_fetch(table_name), error = function(e) {
    cat("  fetch failed:", conditionMessage(e), "\n"); NULL
  })
  if (is.null(df)) return(NULL)

  resp <- tryCatch({
    r <- irw::irw_long2resp(df)
    r$id <- NULL
    r <- r[, apply(r, 2, function(x) all(x %in% c(0, 1, NA))), drop = FALSE]
    r[complete.cases(r), , drop = FALSE]
  }, error = function(e) NULL)
  if (is.null(resp) || nrow(resp) < 200 || ncol(resp) < 5) {
    cat("  skipped: insufficient data\n"); return(NULL)
  }

  N     <- nrow(resp)
  items <- colnames(resp)
  ni    <- length(items)
  cat(sprintf("  N=%d | J=%d\n", N, ni))

  # Simple dimensionality check: ratio of first two correlation eigenvalues
  ev       <- eigen(cor(resp), symmetric = TRUE, only.values = TRUE)$values
  ev_ratio <- ev[1] / ev[2]
  ev_pct1  <- ev[1] / sum(ev)

  cfa_model  <- paste0("F =~ NA*", paste(items, collapse = " + "), "\nF ~~ 1*F")
  mirt_model <- mirt::mirt.model(paste0(
    "F = 1-", ni, "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)"
  ))

  set.seed(seed_mr)
  fold_mat <- t(replicate(N, sample(rep_len(seq_len(nfold_mr), ni))))

  imv_mr_cfa_v_irt_folds  <- matrix(NA, nrow = nfold_mr, ncol = ni + 1,
                                     dimnames = list(NULL, c(items, "scale")))
  imv_mr_cfa_v_prev_folds <- imv_mr_cfa_v_irt_folds
  imv_mr_irt_v_prev_folds <- imv_mr_cfa_v_irt_folds
  fold_errors <- character(0)

  for (k in seq_len(nfold_mr)) {
    cat(sprintf("  MR fold %d/%d ... ", k, nfold_mr))
    train_mr          <- resp
    train_mr[fold_mat == k] <- NA

    yprob_cfa <- tryCatch(
      predict_cfa_mr(train_mr, items, cfa_model),
      error = function(e) {
        fold_errors <<- c(fold_errors, sprintf("fold %d CFA: %s", k, conditionMessage(e)))
        NULL
      }
    )
    yprob_irt <- tryCatch(
      predict_irt_mr(train_mr, items, mirt_model),
      error = function(e) {
        fold_errors <<- c(fold_errors, sprintf("fold %d IRT: %s", k, conditionMessage(e)))
        NULL
      }
    )

    if (is.null(yprob_cfa) || is.null(yprob_irt)) {
      why <- c(if (is.null(yprob_cfa)) "CFA=NULL" else NULL,
               if (is.null(yprob_irt)) "IRT=NULL" else NULL)
      cat(paste("SKIPPED (", paste(why, collapse = ", "), ")\n"))
      next
    }

    item_mr_cfa_v_irt  <- numeric(ni)
    item_mr_cfa_v_prev <- numeric(ni)
    item_mr_irt_v_prev <- numeric(ni)

    for (j in seq_along(items)) {
      held_rows <- which(fold_mat[, j] == k)
      y_j       <- resp[held_rows, j]
      p_cfa_j   <- yprob_cfa[held_rows, j]
      p_irt_j   <- yprob_irt[held_rows, j]
      prev_j    <- mean(train_mr[, j], na.rm = TRUE)

      item_mr_cfa_v_irt[j]  <- imv.binary(y_j, p_cfa_j, p_irt_j)
      item_mr_cfa_v_prev[j] <- imv.binary(y_j, rep(prev_j, length(y_j)), p_cfa_j)
      item_mr_irt_v_prev[j] <- imv.binary(y_j, rep(prev_j, length(y_j)), p_irt_j)
    }

    imv_mr_cfa_v_irt_folds[k, ]  <- c(item_mr_cfa_v_irt,  mean(item_mr_cfa_v_irt))
    imv_mr_cfa_v_prev_folds[k, ] <- c(item_mr_cfa_v_prev, mean(item_mr_cfa_v_prev))
    imv_mr_irt_v_prev_folds[k, ] <- c(item_mr_irt_v_prev, mean(item_mr_irt_v_prev))
    cat(sprintf("IMV_MR(CFA,IRT)=%.4f | CFA vs prev=%.4f | IRT vs prev=%.4f\n",
                mean(item_mr_cfa_v_irt), mean(item_mr_cfa_v_prev), mean(item_mr_irt_v_prev)))
  }

  valid_folds <- which(!is.na(imv_mr_cfa_v_irt_folds[, "scale"]))
  if (length(valid_folds) == 0) {
    cat("  all MR folds failed\n")
    if (length(fold_errors) > 0) cat("  errors:\n", paste0("    ", fold_errors, collapse = "\n"), "\n")
    return(NULL)
  }
  K_mr <- length(valid_folds)

  list(
    table_name               = table_name,
    N                        = N,
    J                        = ni,
    items                    = items,
    ev_ratio                 = ev_ratio,
    ev_pct1                  = ev_pct1,
    nfold_mr                 = nfold_mr,
    valid_folds_mr           = valid_folds,
    fold_errors_mr           = fold_errors,
    imv_mr_cfa_v_irt_folds   = imv_mr_cfa_v_irt_folds,
    imv_mr_cfa_v_prev_folds  = imv_mr_cfa_v_prev_folds,
    imv_mr_irt_v_prev_folds  = imv_mr_irt_v_prev_folds,
    scale_mr_cfa_v_irt_mean  = mean(imv_mr_cfa_v_irt_folds[valid_folds, "scale"]),
    scale_mr_cfa_v_prev_mean = mean(imv_mr_cfa_v_prev_folds[valid_folds, "scale"]),
    scale_mr_irt_v_prev_mean = mean(imv_mr_irt_v_prev_folds[valid_folds, "scale"]),
    scale_mr_cfa_v_irt_sd    = if (K_mr > 1) sd(imv_mr_cfa_v_irt_folds[valid_folds, "scale"]) else NA_real_,
    scale_mr_cfa_v_prev_sd   = if (K_mr > 1) sd(imv_mr_cfa_v_prev_folds[valid_folds, "scale"]) else NA_real_,
    scale_mr_irt_v_prev_sd   = if (K_mr > 1) sd(imv_mr_irt_v_prev_folds[valid_folds, "scale"]) else NA_real_
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

# ----- Run MR (item-response-level) CV -------------------------------------
out_dir_mr <- "vignettes/cfairtdata/fits_mr"
dir.create(out_dir_mr, recursive = TRUE, showWarnings = FALSE)

for (tbl in table_names) {
  out_file_mr <- file.path(out_dir_mr, paste0(tbl, ".rds"))
  if (file.exists(out_file_mr)) {
    cat(sprintf("Skipping MR %s (cached)\n", tbl)); next
  }
  result_mr <- run_dataset_mr(tbl)
  if (!is.null(result_mr)) saveRDS(result_mr, out_file_mr)
}

# ----- Combine results ------------------------------------------------------
dataset_results <- lapply(table_names, function(tbl) {
  f <- file.path(out_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
})
names(dataset_results) <- table_names
dataset_results <- Filter(Negate(is.null), dataset_results)

dataset_results_mr <- lapply(table_names, function(tbl) {
  f <- file.path(out_dir_mr, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
})
names(dataset_results_mr) <- table_names
dataset_results_mr <- Filter(Negate(is.null), dataset_results_mr)

# Dataset-level summary table (MP + MR + eigenvalue columns)
dataset_summary <- do.call(rbind, lapply(names(dataset_results), function(nm) {
  r    <- dataset_results[[nm]]
  r_mr <- dataset_results_mr[[nm]]
  K    <- length(r$valid_folds)
  K_mr <- if (!is.null(r_mr)) length(r_mr$valid_folds_mr) else NA_integer_
  data.frame(
    table_name               = r$table_name,
    N                        = r$N,
    J                        = r$J,
    # Missing-person (MP) columns
    scale_cfa_v_irt_mean     = r$scale_cfa_v_irt_mean,
    scale_cfa_v_irt_sd       = r$scale_cfa_v_irt_sd,
    scale_cfa_v_prev_mean    = r$scale_cfa_v_prev_mean,
    scale_irt_v_prev_mean    = r$scale_irt_v_prev_mean,
    scale_cfa_v_prev_sd      = r$scale_cfa_v_prev_sd,
    scale_irt_v_prev_sd      = r$scale_irt_v_prev_sd,
    K                        = K,
    # Missing-response (MR) columns
    scale_mr_cfa_v_irt_mean  = if (!is.null(r_mr)) r_mr$scale_mr_cfa_v_irt_mean  else NA_real_,
    scale_mr_cfa_v_irt_sd    = if (!is.null(r_mr)) r_mr$scale_mr_cfa_v_irt_sd    else NA_real_,
    scale_mr_cfa_v_prev_mean = if (!is.null(r_mr)) r_mr$scale_mr_cfa_v_prev_mean else NA_real_,
    scale_mr_irt_v_prev_mean = if (!is.null(r_mr)) r_mr$scale_mr_irt_v_prev_mean else NA_real_,
    K_mr                     = K_mr,
    # Dimensionality check
    ev_ratio                 = if (!is.null(r_mr)) r_mr$ev_ratio else NA_real_,
    ev_pct1                  = if (!is.null(r_mr)) r_mr$ev_pct1  else NA_real_,
    stringsAsFactors = FALSE
  )
}))

cat("\n=== Summary across datasets ===\n")
print(dataset_summary[, c("table_name", "N", "J",
                           "scale_cfa_v_irt_mean",
                           "scale_mr_cfa_v_irt_mean",
                           "ev_ratio")],
      digits = 4, row.names = FALSE)

# ----- Save -----------------------------------------------------------------
saveRDS(
  list(
    table_names         = names(dataset_results),
    dataset_results     = dataset_results,
    dataset_results_mr  = dataset_results_mr,
    dataset_summary     = dataset_summary,
    nfold               = nfold,
    nfold_mr            = nfold_mr,
    seed                = seed,
    seed_mr             = seed_mr,
    date_run            = Sys.Date()
  ),
  "vignettes/cfairtdata/cfa_irt_imv_results.rds"
)
cat("\nSaved to vignettes/cfairtdata/cfa_irt_imv_results.rds\n")
