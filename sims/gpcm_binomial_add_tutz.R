# sims/gpcm_binomial_add_tutz.R
#
# Adds Tutz (sequential) model rows to gpcm_binomial_results.rds.
# Reproduces the exact same jobs (seeds → asym values) used in the original
# sweep and extension runs, so the simulated data and holdouts are identical.
# Fits only GPCM + Binom 1PL (logit) + Tutz per job (GPCM and 1PL are needed
# for the ceiling and floor IMV comparisons respectively).
#
# Usage: Rscript sims/gpcm_binomial_add_tutz.R

suppressPackageStartupMessages(library(imv))
suppressPackageStartupMessages(library(mirt))
suppressPackageStartupMessages(library(parallel))

N_PERSONS       <- 1000L
N_ITEMS         <- 10L
N_REPS          <- 100L
N_CORES         <- 4L
LNORM_SDLOG     <- 0.5
THRESH_NOISE_SD <- 0.3
existing_file   <- "sims/gpcm_binomial_results.rds"
out_file        <- "sims/gpcm_binomial_results.rds"

# ── Helpers (kept in sync with gpcm_binomial_sweep.R) ─────────────────────────
make_thresholds <- function(K, asym = 0) {
  n <- K - 1L; total_range <- 3.0
  sym <- seq(-total_range / 2, total_range / 2, length.out = n)
  if (asym == 0 || n <= 2L) return(sym)
  gaps <- exp(asym * 0:(n - 2L) / (n - 2L))
  gaps <- gaps / sum(gaps) * total_range
  b    <- cumsum(c(-total_range / 2, gaps[-length(gaps)]))
  c(b, b[n - 1L] + gaps[n - 1L])
}

gpcm_probs <- function(theta, a, b) {
  K  <- length(b) + 1L
  ln <- matrix(0, nrow = length(theta), ncol = K)
  for (k in seq_len(K - 1L)) ln[, k + 1L] <- ln[, k] + a * (theta - b[k])
  ln <- ln - apply(ln, 1, max); ex <- exp(ln); ex / rowSums(ex)
}
sim_gpcm <- function(theta, a, b) {
  probs <- gpcm_probs(theta, a, b)
  apply(probs, 1, function(p) sample.int(length(p), 1L, prob = p) - 1L)
}
binom_probs <- function(theta, a, b, K) {
  p <- pmin(pmax(plogis(a * theta - b), 1e-10), 1 - 1e-10)
  outer(p, 0:(K - 1L), function(pp, k) dbinom(k, K - 1L, pp))
}
tutz_probs <- function(theta, a, b) {
  K <- length(b) + 1L
  s <- plogis(outer(theta, b, function(th, bk) a * (th - bk)))  # N x (K-1)
  probs <- matrix(NA_real_, length(theta), K)
  cprod <- matrix(1, length(theta), K)
  for (k in seq_len(K - 1L)) cprod[, k + 1L] <- cprod[, k] * s[, k]
  for (k in seq_len(K - 1L)) probs[, k] <- cprod[, k] * (1 - s[, k])
  probs[, K] <- cprod[, K]
  pmax(probs, 1e-15)
}
item_ll <- function(probs, X) sum(log(pmax(probs[cbind(seq_along(X), X + 1L)], 1e-15)))
fit_binom_1pl_logit <- function(X, theta) {
  K <- max(X) + 1L; size <- K - 1L
  mod <- glm(cbind(X, size - X) ~ offset(theta), family = binomial("logit"))
  list(a = 1.0, b = -coef(mod)[["(Intercept)"]])
}

imv_c <- function(y, pctt.tab, p1, p2) {
  nn <- length(pctt.tab); iis <- 0:(nn - 1L); om <- numeric(nn)
  for (ii in iis) {
    ns <- om.tmp <- numeric()
    for (jj in iis[-match(ii, iis)]) {
      y2 <- y[y$resp %in% c(ii, jj), ]
      resp <- ifelse(y2$resp == ii, 1, 0)
      z <- data.frame(resp = resp,
                      p1 = y2[[paste0(p1, ii)]] / (y2[[paste0(p1, ii)]] + y2[[paste0(p1, jj)]]),
                      p2 = y2[[paste0(p2, ii)]] / (y2[[paste0(p2, ii)]] + y2[[paste0(p2, jj)]]))
      om.tmp[as.character(jj)] <- imv.binary(z$resp, z$p1, z$p2)
      ns[as.character(jj)]     <- nrow(z)
    }
    om[ii + 1L] <- sum(om.tmp * ns) / sum(ns)
  }
  sum(om * pctt.tab) / sum(pctt.tab)
}
imv_t <- function(y, pctt.tab, p1, p2) {
  nn <- length(pctt.tab); om <- numeric(nn - 1L)
  for (ii in 0:(nn - 2L)) {
    resp <- ifelse(y$resp <= ii, 1, 0)
    pr1  <- rowSums(y[, paste0(p1, 0:ii), drop = FALSE])
    pr2  <- rowSums(y[, paste0(p2, 0:ii), drop = FALSE])
    om[ii + 1L] <- imv.binary(resp, pr1, pr2)
  }
  wts <- pctt.tab[1:(nn - 1L)] / (1 - pctt.tab[nn])
  sum(om * wts) / sum(wts)
}
make_pair_df <- function(probs_base, probs_enh, X, K) {
  df <- as.data.frame(probs_base); colnames(df) <- paste0("p1", 0:(K - 1L))
  df$resp <- X
  for (k in 0:(K - 1L)) df[[paste0("p2", k)]] <- probs_enh[, k + 1L]
  df[complete.cases(df), ]
}
rmse_fn <- function(probs, X, K)
  sqrt(mean((as.numeric(probs %*% 0:(K - 1L)) / (K - 1L) - X / (K - 1L))^2))

extract_params_mirt <- function(mod, K) {
  lapply(seq_len(N_ITEMS), function(j) {
    p <- coef(mod, IRTpars = TRUE)[[j]]
    list(a = as.numeric(p[, "a"]),
         b = as.numeric(p[, paste0("b", seq_len(K - 1L))]))
  })
}

# Item-level Tutz fit given EAP thetas (more robust than mirt sequential).
fit_tutz_item <- function(X, theta, K) {
  neg <- function(par) {
    a <- exp(par[1]); b <- par[-1]
    -sum(log(pmax(tutz_probs(theta, a, b)[cbind(seq_along(X), X + 1L)], 1e-15)))
  }
  opt <- tryCatch(
    optim(c(log(1.5), seq(-1.5, 1.5, length.out = K - 1L)), neg,
          method = "BFGS", control = list(maxit = 3000, reltol = 1e-9)),
    error = function(e) NULL
  )
  if (is.null(opt)) return(NULL)
  list(a = exp(opt$par[1]), b = opt$par[-1])
}

# ── run_one_tutz: reproduce same data + holdout, fit only 3 models ─────────────
run_one_tutz <- function(K, asym, rep_idx) {
  set.seed(rep_idx * 997L + as.integer(K) * 31L + as.integer(round(asym * 100)))

  theta  <- rnorm(N_PERSONS)
  b_base <- make_thresholds(K, asym)
  shifts <- runif(N_ITEMS, -1, 1)
  X_mat  <- matrix(NA_integer_, N_PERSONS, N_ITEMS)
  for (j in seq_len(N_ITEMS)) {
    a_j <- rlnorm(1L, meanlog = 0, sdlog = LNORM_SDLOG)
    b_j <- b_base + shifts[j] + rnorm(K - 1L, 0, THRESH_NOISE_SD)
    X_j <- sim_gpcm(theta, a_j, b_j); tries <- 0L
    while (length(unique(X_j)) < K && tries < 5L) {
      shifts[j] <- runif(1, -1, 1)
      b_j <- b_base + shifts[j] + rnorm(K - 1L, 0, THRESH_NOISE_SD)
      X_j <- sim_gpcm(theta, a_j, b_j); tries <- tries + 1L
    }
    if (length(unique(X_j)) < K) return(NULL)
    X_mat[, j] <- X_j
  }

  n_ho    <- as.integer(0.2 * N_PERSONS * N_ITEMS)
  ho_lin  <- sample(N_PERSONS * N_ITEMS, n_ho)
  ho_mat  <- matrix(FALSE, N_PERSONS, N_ITEMS); ho_mat[ho_lin] <- TRUE
  ho_row  <- row(ho_mat)[ho_lin]; ho_col <- col(ho_mat)[ho_lin]
  all_X_ho <- X_mat[ho_lin]
  pctt.tab <- as.numeric(table(factor(all_X_ho, levels = 0L:(K - 1L)))) / length(all_X_ho)
  X_tr_df  <- as.data.frame(X_mat); X_tr_df[ho_mat] <- NA_integer_
  theta_grid <- seq(-4, 4, length.out = 101L)

  # Fit GPCM via mirt (ceiling comparison + EAP thetas for item-level models)
  mod_gpcm <- tryCatch(suppressMessages(mirt(X_tr_df, 1, itemtype = "gpcm", verbose = FALSE)),
                       error = function(e) NULL)
  if (is.null(mod_gpcm)) return(NULL)
  theta_eap <- as.numeric(fscores(mod_gpcm, method = "EAP")[, 1])
  gpcm_par  <- extract_params_mirt(mod_gpcm, K)

  # Fit Tutz per item using EAP thetas (avoids mirt sequential model issues)
  tutz_par <- lapply(seq_len(N_ITEMS), function(j) {
    obs_j <- !ho_mat[, j]
    tryCatch(fit_tutz_item(X_mat[obs_j, j], theta_eap[obs_j], K), error = function(e) NULL)
  })
  if (any(sapply(tutz_par, is.null))) return(NULL)

  # Fit Binom 1PL (logit) per item (needed for 1PL floor comparison)
  binom1pl_par <- lapply(seq_len(N_ITEMS), function(j) {
    obs_j <- !ho_mat[, j]
    tryCatch(fit_binom_1pl_logit(X_mat[obs_j, j], theta_eap[obs_j]), error = function(e) NULL)
  })
  if (any(sapply(binom1pl_par, is.null))) return(NULL)

  # Predict held-out cells for all three models
  predict_ho <- function(par, prob_fn) {
    total_ll <- matrix(0, N_PERSONS, length(theta_grid))
    for (j in seq_len(N_ITEMS)) {
      obs_j   <- which(!ho_mat[, j])
      pr_grid <- tryCatch(prob_fn(theta_grid, par[[j]]$a, par[[j]]$b, K),
                          error = function(e) NULL)
      if (is.null(pr_grid) || anyNA(pr_grid)) return(NULL)
      total_ll[obs_j, ] <- total_ll[obs_j, ] +
        t(log(pmax(pr_grid, 1e-15)))[X_mat[obs_j, j] + 1L, ]
    }
    theta_est <- theta_grid[apply(total_ll, 1, which.max)]
    pr_ho <- matrix(NA_real_, n_ho, K)
    for (j in seq_len(N_ITEMS)) {
      cells_j <- which(ho_col == j)
      if (length(cells_j) == 0L) next
      pr_ho[cells_j, ] <- tryCatch(
        prob_fn(theta_est[ho_row[cells_j]], par[[j]]$a, par[[j]]$b, K),
        error = function(e) matrix(NA_real_, length(cells_j), K)
      )
    }
    if (anyNA(pr_ho)) return(NULL)
    pr_ho
  }

  gpcm_binom_fn  <- function(th, a, b, K) gpcm_probs(th, a, b)
  tutz_fn        <- function(th, a, b, K) tutz_probs(th, a, b)
  binom1pl_fn    <- function(th, a, b, K) binom_probs(th, a, b, K)

  pr_gpcm  <- predict_ho(gpcm_par,    gpcm_binom_fn)
  pr_tutz  <- predict_ho(tutz_par,    tutz_fn)
  pr_1pl   <- predict_ho(binom1pl_par, binom1pl_fn)
  if (is.null(pr_gpcm) || is.null(pr_tutz) || is.null(pr_1pl)) return(NULL)

  # Compute metrics for Tutz only
  ydf_ceil <- make_pair_df(pr_tutz, pr_gpcm, all_X_ho, K)
  ydf_floor <- make_pair_df(pr_1pl,  pr_tutz, all_X_ho, K)

  data.frame(
    K = K, asym = asym, rep = rep_idx, model = "tutz",
    rmse      = rmse_fn(pr_tutz, all_X_ho, K),
    imv_c     = tryCatch(imv_c(ydf_ceil,  pctt.tab, "p1", "p2"), error = function(e) NA_real_),
    imv_t     = tryCatch(imv_t(ydf_ceil,  pctt.tab, "p1", "p2"), error = function(e) NA_real_),
    imv_c_1pl = tryCatch(imv_c(ydf_floor, pctt.tab, "p1", "p2"), error = function(e) NA_real_),
    imv_t_1pl = tryCatch(imv_t(ydf_floor, pctt.tab, "p1", "p2"), error = function(e) NA_real_),
    stringsAsFactors = FALSE
  )
}

# ── Reproduce exact job lists (same seeds = same asym draws) ──────────────────
# Original sweep: set.seed(2026), K in {5,7}
set.seed(2026)
jobs_orig <- expand.grid(K = c(5L, 7L), rep = seq_len(N_REPS), stringsAsFactors = FALSE)
jobs_orig$asym <- runif(nrow(jobs_orig), -log(8), log(8))

# K extension: set.seed(20260603), K in {4,6}
set.seed(20260603)
jobs_ext <- expand.grid(K = c(4L, 6L), rep = seq_len(N_REPS), stringsAsFactors = FALSE)
jobs_ext$asym <- runif(nrow(jobs_ext), -log(8), log(8))

jobs <- rbind(jobs_orig, jobs_ext)
message(sprintf("Running Tutz for %d jobs on %d cores ...", nrow(jobs), N_CORES))

tutz_results <- mclapply(seq_len(nrow(jobs)), function(i) {
  r <- jobs[i, ]
  tryCatch(run_one_tutz(r$K, r$asym, r$rep),
           error = function(e) { message("job ", i, " failed: ", e$message); NULL })
}, mc.cores = N_CORES, mc.preschedule = FALSE)

tutz_df <- do.call(rbind, Filter(Negate(is.null), tutz_results))
message(sprintf("Tutz results: %d rows", nrow(tutz_df)))

# ── Merge with existing results and save ──────────────────────────────────────
existing <- readRDS(existing_file)
all_model_names <- c("gpcm", "pcm", "grm", "tutz",
                     "binom_1pl_logit", "binom_1pl_scobit",
                     "binom_1pl_cloglog", "binom_1pl_cauchit",
                     "binom_2pl_logit", "np_spline")
combined <- rbind(existing, tutz_df)
combined$model <- factor(combined$model, levels = all_model_names)
saveRDS(combined, out_file)
message(sprintf("Combined: %d rows → %s", nrow(combined), out_file))
