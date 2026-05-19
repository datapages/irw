## cfa_irt_imv_compute.R
## Pre-computes CFA vs. IRT IMV cross-validation results for 10 IRW datasets,
## using the dimensionality (K) determined per dataset via parallel analysis.
## K=1 results reuse existing caches; K>1 results go to fits_multi/ and fits_mr_multi/.
## Run from project root: Rscript vignettes/cfa_irt_imv_compute.R
## Output: vignettes/cfairtdata/cfa_irt_imv_results.rds

suppressPackageStartupMessages({
  library(irw)
  library(lavaan)
  library(mirt)
  library(imv)
  library(psych)
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ----- Datasets -------------------------------------------------------------
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

# ----- Cache directories ----------------------------------------------------
out_dir          <- "vignettes/cfairtdata/fits"
out_dir_mr       <- "vignettes/cfairtdata/fits_mr"
out_dir_multi    <- "vignettes/cfairtdata/fits_multi"
out_dir_mr_multi <- "vignettes/cfairtdata/fits_mr_multi"
for (d in c(out_dir, out_dir_mr, out_dir_multi, out_dir_mr_multi)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ----- Dimensionality -------------------------------------------------------
# Parallel analysis on tetrachoric correlation matrix → number of factors K.
determine_k <- function(resp) {
  tc <- tryCatch(
    psych::polychoric(resp, correct = 0)$rho,
    error = function(e) cor(resp)
  )
  pa <- tryCatch(
    suppressMessages(suppressWarnings(
      psych::fa.parallel(tc, n.obs = nrow(resp), fa = "fa", plot = FALSE)
    )),
    error = function(e) list(nfact = 1L)
  )
  min(3L, max(1L, as.integer(pa$nfact %||% 1L)))
}

# ----- K=1 prediction helpers -----------------------------------------------
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

# CFA MR: fit with pairwise missing, compute scalar factor scores via Mills ratio.
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

  lam <- as.numeric(est$lambda[, 1])
  tau <- as.numeric(est$tau)
  psi <- diag(est$theta)

  N <- nrow(df)
  J <- length(items)

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

  probs <- matrix(NA_real_, N, J, dimnames = list(NULL, items))
  for (j in seq_len(J)) {
    probs[, j] <- pnorm((lam[j] * theta_hat - tau[j]) / sqrt(psi[j]))
  }
  probs
}

# IRT MR: fit with NAs (mirt marginalises), EAP from held-in items.
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

# ----- K>1 prediction helpers -----------------------------------------------
# Helper: solve linear system with SVD fallback.
safe_solve <- function(A, b) {
  tryCatch(solve(A, b), error = function(e) {
    s <- svd(A)
    s$v %*% diag(1 / pmax(s$d, 1e-10), nrow = length(s$d)) %*% t(s$u) %*% b
  })
}

# CFA MP (K>1): exploratory K-factor DWLS; vectorised factor scores.
predict_cfa_kfactor <- function(train, test, items, K) {
  fit <- tryCatch(
    lavaan::efa(nfactors = K, data = as.data.frame(train), ordered = items,
                estimator = "DWLS", rotation = "oblimin",
                parameterization = "delta"),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  if (inherits(fit, "efaList")) fit <- fit[[1L]]
  est <- tryCatch(inspect(fit, "est"), error = function(e) NULL)
  if (is.null(est)) return(NULL)

  Lambda <- as.matrix(est$lambda)   # J × K
  tau    <- as.numeric(est$tau)     # J
  Theta  <- diag(est$theta)         # J (residual variances)
  J      <- length(items)
  N_test <- nrow(test)

  # Probit residuals y* (N × J)
  phi_t  <- dnorm(tau);  Phi_t <- pnorm(tau)
  y_mat  <- as.matrix(test)
  y_star <- matrix(NA_real_, N_test, J)
  for (j in seq_len(J)) {
    y_star[, j] <- ifelse(y_mat[, j] == 1,
                          tau[j] + phi_t[j] / max(Phi_t[j], 1e-8),
                          tau[j] - phi_t[j] / max(1 - Phi_t[j], 1e-8))
  }

  # OLS factor scores (A is same for all persons — test data complete)
  w      <- 1 / Theta                                   # J weights
  A      <- crossprod(Lambda * sqrt(w))                 # K × K = Λ'diag(w)Λ
  A_inv  <- safe_solve(A, diag(K))
  b_mat  <- sweep(y_star, 2, tau, "-")                  # N × J  (y* - τ)
  b_mat  <- sweep(b_mat, 2, w, "*") %*% Lambda          # N × K  Λ'diag(w)(y*-τ)
  theta_hat <- b_mat %*% A_inv                          # N × K

  # Predicted P(Y=1)
  lin_pred <- theta_hat %*% t(Lambda)                   # N × J
  lin_pred <- sweep(lin_pred, 2, tau, "-")
  probs    <- pnorm(sweep(lin_pred, 2, sqrt(Theta), "/"))
  colnames(probs) <- items
  probs
}

# Build a mirt.model with lognormal priors on all K discrimination parameters.
make_irt_model <- function(ni, K) {
  factor_str <- if (K == 1L) {
    paste0("F = 1-", ni)
  } else {
    paste(paste0("F", seq_len(K), " = 1-", ni), collapse = "\n")
  }
  prior_str <- paste(
    paste0("(1-", ni, ", a", seq_len(K), ", lnorm, 0.0, 1.0)"),
    collapse = ", "
  )
  mirt::mirt.model(paste0(factor_str, "\nPRIOR = ", prior_str))
}

# IRT MP (K>1): fit on train; score test via fixed-parameter re-fit to avoid
# fscores(response.pattern) returning wrong shape for K>1 models.
predict_irt_kfactor <- function(train, test, items, K) {
  ni <- length(items)
  fit <- tryCatch(
    mirt(as.data.frame(train), make_irt_model(ni, K),
         itemtype  = rep("2PL", ni),
         method    = "EM",
         technical = list(NCYCLES = 10000),
         verbose   = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  fit_test <- tryCatch(
    mirt(as.data.frame(test), K, pars = coef(fit), TOL = NaN, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit_test)) return(NULL)
  theta_test <- fscores(fit_test, method = "EAP")
  probs_all  <- probtrace(fit_test, theta_test)
  p1_cols    <- seq(2, ncol(probs_all), by = 2)
  yprob      <- probs_all[, p1_cols, drop = FALSE]
  colnames(yprob) <- items
  yprob
}

# CFA MR (K>1): EFA with pairwise missing; per-person K-dim factor scores.
predict_cfa_mr_kfactor <- function(data_with_nas, items, K) {
  df <- as.data.frame(data_with_nas)
  fit <- tryCatch(
    lavaan::efa(nfactors = K, data = df, ordered = items,
                estimator = "DWLS", rotation = "oblimin",
                parameterization = "delta", missing = "pairwise"),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  if (inherits(fit, "efaList")) fit <- fit[[1L]]
  est <- tryCatch(inspect(fit, "est"), error = function(e) NULL)
  if (is.null(est)) return(NULL)

  Lambda <- as.matrix(est$lambda)
  tau    <- as.numeric(est$tau)
  Theta  <- diag(est$theta)
  N <- nrow(df);  J <- length(items)

  theta_hat <- matrix(0, N, K)
  for (i in seq_len(N)) {
    y_i <- as.numeric(df[i, ])
    obs <- which(!is.na(y_i))
    if (length(obs) < K + 1L) next
    tau_o   <- tau[obs];  Theta_o <- Theta[obs]
    Lam_o   <- Lambda[obs, , drop = FALSE]
    phi_t   <- dnorm(tau_o);  Phi_t <- pnorm(tau_o)
    y_star  <- ifelse(y_i[obs] == 1,
                      tau_o + phi_t / pmax(Phi_t, 1e-8),
                      tau_o - phi_t / pmax(1 - Phi_t, 1e-8))
    w  <- 1 / Theta_o
    A  <- crossprod(Lam_o * sqrt(w))                    # K × K
    b  <- colSums(Lam_o * (w * (y_star - tau_o)))       # K
    theta_hat[i, ] <- safe_solve(A, b)
  }

  lin_pred <- theta_hat %*% t(Lambda)
  lin_pred <- sweep(lin_pred, 2, tau, "-")
  probs    <- pnorm(sweep(lin_pred, 2, sqrt(Theta), "/"))
  colnames(probs) <- items
  probs
}

# IRT MR (K>1): fit with NA data; score via fscores() directly (no response.pattern)
# since data is already in the fit object — avoids K>1 shape issue.
predict_irt_mr_kfactor <- function(data_with_nas, items, K) {
  ni <- length(items)
  fit <- tryCatch(
    mirt(as.data.frame(data_with_nas), make_irt_model(ni, K),
         itemtype  = rep("2PL", ni),
         method    = "EM",
         technical = list(NCYCLES = 10000),
         verbose   = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  theta     <- fscores(fit, method = "EAP")
  probs_all <- probtrace(fit, theta)
  p1_cols   <- seq(2, ncol(probs_all), by = 2)
  probs     <- probs_all[, p1_cols, drop = FALSE]
  colnames(probs) <- items
  probs
}

# ----- CV runners -----------------------------------------------------------
# Shared data-prep helper used by all four runners.
fetch_resp <- function(table_name) {
  df <- tryCatch(irw::irw_fetch(table_name), error = function(e) {
    cat("  fetch failed:", conditionMessage(e), "\n"); NULL
  })
  if (is.null(df)) return(NULL)
  tryCatch({
    r <- irw::irw_long2resp(df)
    r$id <- NULL
    r <- r[, apply(r, 2, function(x) all(x %in% c(0, 1, NA))), drop = FALSE]
    r[complete.cases(r), , drop = FALSE]
  }, error = function(e) {
    cat("  prep failed:", conditionMessage(e), "\n"); NULL
  })
}

# Shared IMV computation across items for one fold.
fold_imv <- function(y_true, yprob_cfa, yprob_irt, prev_train, items) {
  ni <- length(items)
  item_cfa_v_irt  <- numeric(ni)
  item_cfa_v_prev <- numeric(ni)
  item_irt_v_prev <- numeric(ni)
  for (j in seq_along(items)) {
    y_j <- y_true[, j]
    p_c <- yprob_cfa[, j];  p_i <- yprob_irt[, j]
    pv  <- rep(prev_train[j], length(y_j))
    item_cfa_v_irt[j]  <- imv.binary(y_j, p_c, p_i)
    item_cfa_v_prev[j] <- imv.binary(y_j, pv, p_c)
    item_irt_v_prev[j] <- imv.binary(y_j, pv, p_i)
  }
  list(cfa_v_irt = item_cfa_v_irt, cfa_v_prev = item_cfa_v_prev,
       irt_v_prev = item_irt_v_prev)
}

# ----- K=1 MP CV ------------------------------------------------------------
run_dataset <- function(table_name) {
  cat(sprintf("\n=== MP K=1: %s ===\n", table_name))
  resp <- fetch_resp(table_name)
  if (is.null(resp) || nrow(resp) < 200 || ncol(resp) < 5) {
    cat("  skipped: insufficient data\n"); return(NULL)
  }
  N <- nrow(resp);  items <- colnames(resp);  ni <- length(items)
  prev <- colMeans(resp)
  cat(sprintf("  N=%d | J=%d | prev range: %.2f-%.2f\n", N, ni, min(prev), max(prev)))

  cfa_model  <- paste0("F =~ NA*", paste(items, collapse = " + "), "\nF ~~ 1*F")
  mirt_model <- mirt::mirt.model(paste0(
    "F = 1-", ni, "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)"
  ))

  set.seed(seed)
  folds <- sample(rep(1:nfold, length.out = N))
  imv_cfa_v_irt_folds  <- matrix(NA, nrow = nfold, ncol = ni + 1,
                                   dimnames = list(NULL, c(items, "scale")))
  imv_cfa_v_prev_folds <- imv_cfa_v_irt_folds
  imv_irt_v_prev_folds <- imv_cfa_v_irt_folds
  fold_errors <- character(0)

  for (k in 1:nfold) {
    cat(sprintf("  Fold %d/%d ... ", k, nfold))
    train_df <- resp[folds != k, , drop = FALSE]
    test_df  <- resp[folds == k, , drop = FALSE]

    yprob_cfa <- tryCatch(predict_cfa(train_df, test_df, cfa_model, items),
      error = function(e) { fold_errors <<- c(fold_errors, sprintf("fold %d CFA: %s", k, conditionMessage(e))); NULL })
    yprob_irt <- tryCatch(predict_irt(train_df, test_df, mirt_model),
      error = function(e) { fold_errors <<- c(fold_errors, sprintf("fold %d IRT: %s", k, conditionMessage(e))); NULL })
    if (is.null(yprob_cfa) || is.null(yprob_irt)) { cat("SKIPPED\n"); next }

    imv_f <- fold_imv(test_df, yprob_cfa, yprob_irt, colMeans(train_df), items)
    imv_cfa_v_irt_folds[k, ]  <- c(imv_f$cfa_v_irt,  mean(imv_f$cfa_v_irt))
    imv_cfa_v_prev_folds[k, ] <- c(imv_f$cfa_v_prev, mean(imv_f$cfa_v_prev))
    imv_irt_v_prev_folds[k, ] <- c(imv_f$irt_v_prev, mean(imv_f$irt_v_prev))
    cat(sprintf("IMV(CFA,IRT)=%.4f | CFA vs prev=%.4f | IRT vs prev=%.4f\n",
                mean(imv_f$cfa_v_irt), mean(imv_f$cfa_v_prev), mean(imv_f$irt_v_prev)))
  }

  vf <- which(!is.na(imv_cfa_v_irt_folds[, "scale"]))
  if (length(vf) == 0) { cat("  all folds failed\n"); return(NULL) }

  a_params <- tryCatch({
    fit_full <- mirt(resp, mirt_model, itemtype = rep("2PL", ni),
                     method = "EM", technical = list(NCYCLES = 10000), verbose = FALSE)
    coef(fit_full, IRTpars = TRUE, simplify = TRUE)$items[, "a"]
  }, error = function(e) rep(NA_real_, ni))

  list(table_name = table_name, n_dim = 1L, N = N, J = ni, items = items,
       prev_range = range(prev), nfold = nfold, valid_folds = vf,
       fold_errors = fold_errors, a_params = a_params,
       imv_cfa_v_irt_folds  = imv_cfa_v_irt_folds,
       imv_cfa_v_prev_folds = imv_cfa_v_prev_folds,
       imv_irt_v_prev_folds = imv_irt_v_prev_folds,
       scale_cfa_v_irt_mean  = mean(imv_cfa_v_irt_folds[vf,  "scale"]),
       scale_cfa_v_prev_mean = mean(imv_cfa_v_prev_folds[vf, "scale"]),
       scale_irt_v_prev_mean = mean(imv_irt_v_prev_folds[vf, "scale"]),
       scale_cfa_v_irt_sd    = sd(imv_cfa_v_irt_folds[vf,  "scale"]),
       scale_cfa_v_prev_sd   = sd(imv_cfa_v_prev_folds[vf, "scale"]),
       scale_irt_v_prev_sd   = sd(imv_irt_v_prev_folds[vf, "scale"]))
}

# ----- K>1 MP CV ------------------------------------------------------------
run_dataset_kfactor <- function(table_name, K) {
  cat(sprintf("\n=== MP K=%d: %s ===\n", K, table_name))
  resp <- fetch_resp(table_name)
  if (is.null(resp) || nrow(resp) < 200 || ncol(resp) < 5) {
    cat("  skipped: insufficient data\n"); return(NULL)
  }
  N <- nrow(resp);  items <- colnames(resp);  ni <- length(items)
  prev <- colMeans(resp)
  cat(sprintf("  N=%d | J=%d | K=%d\n", N, ni, K))

  set.seed(seed)
  folds <- sample(rep(1:nfold, length.out = N))
  imv_cfa_v_irt_folds  <- matrix(NA, nrow = nfold, ncol = ni + 1,
                                   dimnames = list(NULL, c(items, "scale")))
  imv_cfa_v_prev_folds <- imv_cfa_v_irt_folds
  imv_irt_v_prev_folds <- imv_cfa_v_irt_folds
  fold_errors <- character(0)

  for (k in 1:nfold) {
    cat(sprintf("  Fold %d/%d ... ", k, nfold))
    train_df <- resp[folds != k, , drop = FALSE]
    test_df  <- resp[folds == k, , drop = FALSE]

    yprob_cfa <- tryCatch(predict_cfa_kfactor(train_df, test_df, items, K),
      error = function(e) { fold_errors <<- c(fold_errors, sprintf("fold %d CFA: %s", k, conditionMessage(e))); NULL })
    yprob_irt <- tryCatch(predict_irt_kfactor(train_df, test_df, items, K),
      error = function(e) { fold_errors <<- c(fold_errors, sprintf("fold %d IRT: %s", k, conditionMessage(e))); NULL })
    if (is.null(yprob_cfa) || is.null(yprob_irt)) { cat("SKIPPED\n"); next }

    imv_f <- fold_imv(test_df, yprob_cfa, yprob_irt, colMeans(train_df), items)
    imv_cfa_v_irt_folds[k, ]  <- c(imv_f$cfa_v_irt,  mean(imv_f$cfa_v_irt))
    imv_cfa_v_prev_folds[k, ] <- c(imv_f$cfa_v_prev, mean(imv_f$cfa_v_prev))
    imv_irt_v_prev_folds[k, ] <- c(imv_f$irt_v_prev, mean(imv_f$irt_v_prev))
    cat(sprintf("IMV(CFA,IRT)=%.4f | CFA vs prev=%.4f | IRT vs prev=%.4f\n",
                mean(imv_f$cfa_v_irt), mean(imv_f$cfa_v_prev), mean(imv_f$irt_v_prev)))
  }

  if (length(fold_errors) > 0) cat("  errors:", paste(fold_errors, collapse = " | "), "\n")
  vf <- which(!is.na(imv_cfa_v_irt_folds[, "scale"]))
  if (length(vf) == 0) { cat("  all folds failed\n"); return(NULL) }

  list(table_name = table_name, n_dim = K, N = N, J = ni, items = items,
       prev_range = range(prev), nfold = nfold, valid_folds = vf,
       fold_errors = fold_errors,
       imv_cfa_v_irt_folds  = imv_cfa_v_irt_folds,
       imv_cfa_v_prev_folds = imv_cfa_v_prev_folds,
       imv_irt_v_prev_folds = imv_irt_v_prev_folds,
       scale_cfa_v_irt_mean  = mean(imv_cfa_v_irt_folds[vf,  "scale"]),
       scale_cfa_v_prev_mean = mean(imv_cfa_v_prev_folds[vf, "scale"]),
       scale_irt_v_prev_mean = mean(imv_irt_v_prev_folds[vf, "scale"]),
       scale_cfa_v_irt_sd    = sd(imv_cfa_v_irt_folds[vf,  "scale"]),
       scale_cfa_v_prev_sd   = sd(imv_cfa_v_prev_folds[vf, "scale"]),
       scale_irt_v_prev_sd   = sd(imv_irt_v_prev_folds[vf, "scale"]))
}

# ----- K=1 MR CV ------------------------------------------------------------
run_dataset_mr <- function(table_name) {
  cat(sprintf("\n=== MR K=1: %s ===\n", table_name))
  resp <- fetch_resp(table_name)
  if (is.null(resp) || nrow(resp) < 200 || ncol(resp) < 5) {
    cat("  skipped: insufficient data\n"); return(NULL)
  }
  N <- nrow(resp);  items <- colnames(resp);  ni <- length(items)
  cat(sprintf("  N=%d | J=%d\n", N, ni))

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
    train_mr <- resp;  train_mr[fold_mat == k] <- NA

    yprob_cfa <- tryCatch(predict_cfa_mr(train_mr, items, cfa_model),
      error = function(e) { fold_errors <<- c(fold_errors, sprintf("fold %d CFA: %s", k, conditionMessage(e))); NULL })
    yprob_irt <- tryCatch(predict_irt_mr(train_mr, items, mirt_model),
      error = function(e) { fold_errors <<- c(fold_errors, sprintf("fold %d IRT: %s", k, conditionMessage(e))); NULL })
    if (is.null(yprob_cfa) || is.null(yprob_irt)) {
      why <- c(if (is.null(yprob_cfa)) "CFA=NULL", if (is.null(yprob_irt)) "IRT=NULL")
      cat(paste("SKIPPED (", paste(why, collapse = ", "), ")\n")); next
    }

    item_mr_cfa_v_irt  <- numeric(ni)
    item_mr_cfa_v_prev <- numeric(ni)
    item_mr_irt_v_prev <- numeric(ni)
    for (j in seq_along(items)) {
      hr <- which(fold_mat[, j] == k)
      y_j <- resp[hr, j];  pv <- rep(mean(train_mr[, j], na.rm = TRUE), length(y_j))
      item_mr_cfa_v_irt[j]  <- imv.binary(y_j, yprob_cfa[hr, j], yprob_irt[hr, j])
      item_mr_cfa_v_prev[j] <- imv.binary(y_j, pv, yprob_cfa[hr, j])
      item_mr_irt_v_prev[j] <- imv.binary(y_j, pv, yprob_irt[hr, j])
    }
    imv_mr_cfa_v_irt_folds[k, ]  <- c(item_mr_cfa_v_irt,  mean(item_mr_cfa_v_irt))
    imv_mr_cfa_v_prev_folds[k, ] <- c(item_mr_cfa_v_prev, mean(item_mr_cfa_v_prev))
    imv_mr_irt_v_prev_folds[k, ] <- c(item_mr_irt_v_prev, mean(item_mr_irt_v_prev))
    cat(sprintf("IMV_MR(CFA,IRT)=%.4f | CFA vs prev=%.4f | IRT vs prev=%.4f\n",
                mean(item_mr_cfa_v_irt), mean(item_mr_cfa_v_prev), mean(item_mr_irt_v_prev)))
  }

  vf <- which(!is.na(imv_mr_cfa_v_irt_folds[, "scale"]))
  if (length(vf) == 0) { cat("  all MR folds failed\n"); return(NULL) }
  n_vf <- length(vf)

  list(table_name = table_name, n_dim = 1L, N = N, J = ni, items = items,
       ev_ratio = ev_ratio, ev_pct1 = ev_pct1,
       nfold_mr = nfold_mr, valid_folds_mr = vf, fold_errors_mr = fold_errors,
       imv_mr_cfa_v_irt_folds  = imv_mr_cfa_v_irt_folds,
       imv_mr_cfa_v_prev_folds = imv_mr_cfa_v_prev_folds,
       imv_mr_irt_v_prev_folds = imv_mr_irt_v_prev_folds,
       scale_mr_cfa_v_irt_mean  = mean(imv_mr_cfa_v_irt_folds[vf, "scale"]),
       scale_mr_cfa_v_prev_mean = mean(imv_mr_cfa_v_prev_folds[vf, "scale"]),
       scale_mr_irt_v_prev_mean = mean(imv_mr_irt_v_prev_folds[vf, "scale"]),
       scale_mr_cfa_v_irt_sd  = if (n_vf > 1) sd(imv_mr_cfa_v_irt_folds[vf,  "scale"]) else NA_real_,
       scale_mr_cfa_v_prev_sd = if (n_vf > 1) sd(imv_mr_cfa_v_prev_folds[vf, "scale"]) else NA_real_,
       scale_mr_irt_v_prev_sd = if (n_vf > 1) sd(imv_mr_irt_v_prev_folds[vf, "scale"]) else NA_real_)
}

# ----- K>1 MR CV ------------------------------------------------------------
run_dataset_mr_kfactor <- function(table_name, K) {
  cat(sprintf("\n=== MR K=%d: %s ===\n", K, table_name))
  resp <- fetch_resp(table_name)
  if (is.null(resp) || nrow(resp) < 200 || ncol(resp) < 5) {
    cat("  skipped: insufficient data\n"); return(NULL)
  }
  N <- nrow(resp);  items <- colnames(resp);  ni <- length(items)
  cat(sprintf("  N=%d | J=%d | K=%d\n", N, ni, K))

  ev       <- eigen(cor(resp), symmetric = TRUE, only.values = TRUE)$values
  ev_ratio <- ev[1] / ev[2]
  ev_pct1  <- ev[1] / sum(ev)

  set.seed(seed_mr)
  fold_mat <- t(replicate(N, sample(rep_len(seq_len(nfold_mr), ni))))
  imv_mr_cfa_v_irt_folds  <- matrix(NA, nrow = nfold_mr, ncol = ni + 1,
                                     dimnames = list(NULL, c(items, "scale")))
  imv_mr_cfa_v_prev_folds <- imv_mr_cfa_v_irt_folds
  imv_mr_irt_v_prev_folds <- imv_mr_cfa_v_irt_folds
  fold_errors <- character(0)

  for (k in seq_len(nfold_mr)) {
    cat(sprintf("  MR fold %d/%d ... ", k, nfold_mr))
    train_mr <- resp;  train_mr[fold_mat == k] <- NA

    yprob_cfa <- tryCatch(predict_cfa_mr_kfactor(train_mr, items, K),
      error = function(e) { fold_errors <<- c(fold_errors, sprintf("fold %d CFA: %s", k, conditionMessage(e))); NULL })
    yprob_irt <- tryCatch(predict_irt_mr_kfactor(train_mr, items, K),
      error = function(e) { fold_errors <<- c(fold_errors, sprintf("fold %d IRT: %s", k, conditionMessage(e))); NULL })
    if (is.null(yprob_cfa) || is.null(yprob_irt)) {
      why <- c(if (is.null(yprob_cfa)) "CFA=NULL", if (is.null(yprob_irt)) "IRT=NULL")
      cat(paste("SKIPPED (", paste(why, collapse = ", "), ")\n")); next
    }

    item_mr_cfa_v_irt  <- numeric(ni)
    item_mr_cfa_v_prev <- numeric(ni)
    item_mr_irt_v_prev <- numeric(ni)
    for (j in seq_along(items)) {
      hr <- which(fold_mat[, j] == k)
      y_j <- resp[hr, j];  pv <- rep(mean(train_mr[, j], na.rm = TRUE), length(y_j))
      item_mr_cfa_v_irt[j]  <- imv.binary(y_j, yprob_cfa[hr, j], yprob_irt[hr, j])
      item_mr_cfa_v_prev[j] <- imv.binary(y_j, pv, yprob_cfa[hr, j])
      item_mr_irt_v_prev[j] <- imv.binary(y_j, pv, yprob_irt[hr, j])
    }
    imv_mr_cfa_v_irt_folds[k, ]  <- c(item_mr_cfa_v_irt,  mean(item_mr_cfa_v_irt))
    imv_mr_cfa_v_prev_folds[k, ] <- c(item_mr_cfa_v_prev, mean(item_mr_cfa_v_prev))
    imv_mr_irt_v_prev_folds[k, ] <- c(item_mr_irt_v_prev, mean(item_mr_irt_v_prev))
    cat(sprintf("IMV_MR(CFA,IRT)=%.4f | CFA vs prev=%.4f | IRT vs prev=%.4f\n",
                mean(item_mr_cfa_v_irt), mean(item_mr_cfa_v_prev), mean(item_mr_irt_v_prev)))
  }

  if (length(fold_errors) > 0) cat("  errors:", paste(fold_errors, collapse = " | "), "\n")
  vf <- which(!is.na(imv_mr_cfa_v_irt_folds[, "scale"]))
  if (length(vf) == 0) { cat("  all MR folds failed\n"); return(NULL) }
  n_vf <- length(vf)

  list(table_name = table_name, n_dim = K, N = N, J = ni, items = items,
       ev_ratio = ev_ratio, ev_pct1 = ev_pct1,
       nfold_mr = nfold_mr, valid_folds_mr = vf, fold_errors_mr = fold_errors,
       imv_mr_cfa_v_irt_folds  = imv_mr_cfa_v_irt_folds,
       imv_mr_cfa_v_prev_folds = imv_mr_cfa_v_prev_folds,
       imv_mr_irt_v_prev_folds = imv_mr_irt_v_prev_folds,
       scale_mr_cfa_v_irt_mean  = mean(imv_mr_cfa_v_irt_folds[vf, "scale"]),
       scale_mr_cfa_v_prev_mean = mean(imv_mr_cfa_v_prev_folds[vf, "scale"]),
       scale_mr_irt_v_prev_mean = mean(imv_mr_irt_v_prev_folds[vf, "scale"]),
       scale_mr_cfa_v_irt_sd  = if (n_vf > 1) sd(imv_mr_cfa_v_irt_folds[vf,  "scale"]) else NA_real_,
       scale_mr_cfa_v_prev_sd = if (n_vf > 1) sd(imv_mr_cfa_v_prev_folds[vf, "scale"]) else NA_real_,
       scale_mr_irt_v_prev_sd = if (n_vf > 1) sd(imv_mr_irt_v_prev_folds[vf, "scale"]) else NA_real_)
}

# ----- Determine K for all datasets -----------------------------------------
cat("\n=== Determining dimensionality via parallel analysis ===\n")
k_cache_file <- "vignettes/cfairtdata/k_estimates.rds"
k_estimates  <- if (file.exists(k_cache_file)) readRDS(k_cache_file) else list()

for (tbl in table_names) {
  if (!is.null(k_estimates[[tbl]])) {
    cat(sprintf("K[%s] = %d (cached)\n", tbl, k_estimates[[tbl]])); next
  }
  resp <- fetch_resp(tbl)
  if (is.null(resp) || nrow(resp) < 200 || ncol(resp) < 5) {
    k_estimates[[tbl]] <- 1L; next
  }
  k_estimates[[tbl]] <- determine_k(resp)
  cat(sprintf("K[%s] = %d\n", tbl, k_estimates[[tbl]]))
}
saveRDS(k_estimates, k_cache_file)
cat("\nDimensionality estimates:\n")
print(unlist(k_estimates[table_names]))

# ----- Run K=1 MP CV (all datasets) -----------------------------------------
for (tbl in table_names) {
  out_file <- file.path(out_dir, paste0(tbl, ".rds"))
  if (file.exists(out_file)) { cat(sprintf("Skipping MP K=1 %s (cached)\n", tbl)); next }
  result <- run_dataset(tbl)
  if (!is.null(result)) saveRDS(result, out_file)
}

# ----- Run K>1 MP CV (multidimensional datasets only) -----------------------
for (tbl in table_names) {
  K <- k_estimates[[tbl]] %||% 1L
  if (K == 1L) next
  out_file <- file.path(out_dir_multi, paste0(tbl, ".rds"))
  if (file.exists(out_file)) { cat(sprintf("Skipping MP K=%d %s (cached)\n", K, tbl)); next }
  result <- run_dataset_kfactor(tbl, K)
  if (!is.null(result)) saveRDS(result, out_file)
}

# ----- Run K=1 MR CV (all datasets) -----------------------------------------
for (tbl in table_names) {
  out_file <- file.path(out_dir_mr, paste0(tbl, ".rds"))
  if (file.exists(out_file)) { cat(sprintf("Skipping MR K=1 %s (cached)\n", tbl)); next }
  result <- run_dataset_mr(tbl)
  if (!is.null(result)) saveRDS(result, out_file)
}

# ----- Run K>1 MR CV (multidimensional datasets only) -----------------------
for (tbl in table_names) {
  K <- k_estimates[[tbl]] %||% 1L
  if (K == 1L) next
  out_file <- file.path(out_dir_mr_multi, paste0(tbl, ".rds"))
  if (file.exists(out_file)) { cat(sprintf("Skipping MR K=%d %s (cached)\n", K, tbl)); next }
  result <- run_dataset_mr_kfactor(tbl, K)
  if (!is.null(result)) saveRDS(result, out_file)
}

# ----- Combine results ------------------------------------------------------
# K=1 results for all datasets (used for figures)
dataset_results <- lapply(table_names, function(tbl) {
  f <- file.path(out_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
})
names(dataset_results) <- table_names
dataset_results <- Filter(Negate(is.null), dataset_results)

# K=1 MR results for all datasets
dataset_results_mr <- lapply(table_names, function(tbl) {
  f <- file.path(out_dir_mr, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
})
names(dataset_results_mr) <- table_names
dataset_results_mr <- Filter(Negate(is.null), dataset_results_mr)

# K>1 results for multidimensional datasets (used for table only)
dataset_results_multi <- lapply(table_names, function(tbl) {
  f <- file.path(out_dir_multi, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
})
names(dataset_results_multi) <- table_names
dataset_results_multi <- Filter(Negate(is.null), dataset_results_multi)

dataset_results_mr_multi <- lapply(table_names, function(tbl) {
  f <- file.path(out_dir_mr_multi, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
})
names(dataset_results_mr_multi) <- table_names
dataset_results_mr_multi <- Filter(Negate(is.null), dataset_results_mr_multi)

dataset_summary <- do.call(rbind, lapply(names(dataset_results), function(nm) {
  r      <- dataset_results[[nm]]
  r_mr   <- dataset_results_mr[[nm]]
  r_md   <- dataset_results_multi[[nm]]
  r_mr_md <- dataset_results_mr_multi[[nm]]
  n_vf    <- length(r$valid_folds)
  n_vf_mr <- if (!is.null(r_mr)) length(r_mr$valid_folds_mr) else NA_integer_
  n_dim   <- k_estimates[[nm]] %||% 1L
  data.frame(
    table_name               = r$table_name,
    n_dim                    = n_dim,
    N                        = r$N,
    J                        = r$J,
    # K=1 MP columns (all datasets)
    scale_cfa_v_irt_mean     = r$scale_cfa_v_irt_mean,
    scale_cfa_v_irt_sd       = r$scale_cfa_v_irt_sd,
    scale_cfa_v_prev_mean    = r$scale_cfa_v_prev_mean,
    scale_irt_v_prev_mean    = r$scale_irt_v_prev_mean,
    K_mp                     = n_vf,
    # K=1 MR columns (all datasets)
    scale_mr_cfa_v_irt_mean  = if (!is.null(r_mr)) r_mr$scale_mr_cfa_v_irt_mean  else NA_real_,
    scale_mr_cfa_v_irt_sd    = if (!is.null(r_mr)) r_mr$scale_mr_cfa_v_irt_sd    else NA_real_,
    scale_mr_cfa_v_prev_mean = if (!is.null(r_mr)) r_mr$scale_mr_cfa_v_prev_mean else NA_real_,
    scale_mr_irt_v_prev_mean = if (!is.null(r_mr)) r_mr$scale_mr_irt_v_prev_mean else NA_real_,
    K_mr                     = n_vf_mr,
    # K>1 MP columns (multidimensional datasets only)
    scale_md_cfa_v_irt_mean  = if (!is.null(r_md)) r_md$scale_cfa_v_irt_mean  else NA_real_,
    scale_md_cfa_v_irt_sd    = if (!is.null(r_md)) r_md$scale_cfa_v_irt_sd    else NA_real_,
    scale_md_cfa_v_prev_mean = if (!is.null(r_md)) r_md$scale_cfa_v_prev_mean else NA_real_,
    scale_md_irt_v_prev_mean = if (!is.null(r_md)) r_md$scale_irt_v_prev_mean else NA_real_,
    # K>1 MR columns (multidimensional datasets only)
    scale_mr_md_cfa_v_irt_mean  = if (!is.null(r_mr_md)) r_mr_md$scale_mr_cfa_v_irt_mean  else NA_real_,
    scale_mr_md_cfa_v_irt_sd    = if (!is.null(r_mr_md)) r_mr_md$scale_mr_cfa_v_irt_sd    else NA_real_,
    scale_mr_md_cfa_v_prev_mean = if (!is.null(r_mr_md)) r_mr_md$scale_mr_cfa_v_prev_mean else NA_real_,
    scale_mr_md_irt_v_prev_mean = if (!is.null(r_mr_md)) r_mr_md$scale_mr_irt_v_prev_mean else NA_real_,
    # Eigenvalue diagnostics
    ev_ratio                 = if (!is.null(r_mr)) r_mr$ev_ratio else NA_real_,
    ev_pct1                  = if (!is.null(r_mr)) r_mr$ev_pct1  else NA_real_,
    stringsAsFactors = FALSE
  )
}))

cat("\n=== Summary across datasets ===\n")
print(dataset_summary[, c("table_name", "n_dim", "N", "J",
                           "scale_cfa_v_irt_mean",
                           "scale_md_cfa_v_irt_mean",
                           "scale_mr_cfa_v_irt_mean",
                           "scale_mr_md_cfa_v_irt_mean",
                           "ev_ratio")],
      digits = 4, row.names = FALSE)

# ----- Save -----------------------------------------------------------------
saveRDS(
  list(
    table_names              = names(dataset_results),
    k_estimates              = k_estimates,
    dataset_results          = dataset_results,
    dataset_results_mr       = dataset_results_mr,
    dataset_results_multi    = dataset_results_multi,
    dataset_results_mr_multi = dataset_results_mr_multi,
    dataset_summary          = dataset_summary,
    nfold                    = nfold,
    nfold_mr                 = nfold_mr,
    seed                     = seed,
    seed_mr                  = seed_mr,
    date_run                 = Sys.Date()
  ),
  "vignettes/cfairtdata/cfa_irt_imv_results.rds"
)
cat("\nSaved to vignettes/cfairtdata/cfa_irt_imv_results.rds\n")
