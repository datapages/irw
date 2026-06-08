# sims/gpcm_binomial_sweep.R
#
# Full sweep: GPCM data, compare GPCM vs. binomial approximations.
# Native models (GPCM, PCM, GRM) estimated via mirt MMLE.
# Binomial models calibrated using mirt-estimated training thetas.
# Test theta estimated by grid-search MLE for all models.
#
# The x-axis variable is asym: 0 = symmetric (equal gaps); asym > 0 = wider
# upper gaps; asym < 0 = wider lower gaps. max/min gap ratio = exp(|asym|).
#
# Multiple items per condition: each item shifts the base threshold vector by
# a uniform draw in [-1, 1] (same gap shape, different location).
#
# Saves sims/gpcm_binomial_results.rds for visualization.
#
# Usage: Rscript sims/gpcm_binomial_sweep.R

suppressPackageStartupMessages(library(imv))
suppressPackageStartupMessages(library(mirt))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(scam))

# ── Simulation parameters ──────────────────────────────────────────────────────
set.seed(2026)
N_PERSONS      <- 1000L
N_ITEMS        <- 10L
N_REPS         <- 100L
N_CORES        <- 4L
LNORM_SDLOG    <- 0.5    # sd of log(a); per-item discrimination ~ LogNormal(0, 0.5^2)
K_grid         <- c(4L, 5L, 6L, 7L)
# asym ~ Uniform(-log(8), log(8)) per rep: 0 = symmetric; +log(8) = wider upper
# gaps with max/min ratio of 8; -log(8) = wider lower gaps same magnitude.
# Dropping K=3: with only 1 inter-threshold gap, asymmetry is undefined.
# K=4,6 results are appended to existing K=5,7 via gpcm_binomial_extend.R.
# Per-item threshold noise: each threshold gets iid N(0, THRESH_NOISE_SD) added
# on top of the location shift. Thresholds span ~3 units, so 0.3 ≈ 10% of range.
THRESH_NOISE_SD <- 0.3
out_file       <- "sims/gpcm_binomial_results.rds"

# ── Threshold generation ───────────────────────────────────────────────────────
# asym = 0 → equal gaps (symmetric).
# asym > 0 → gaps widen toward upper end; asym < 0 → toward lower end.
# max/min gap ratio = exp(|asym|), comparable across K values.
# Formula: gap_k ∝ exp(asym * k / (n-2)),  k = 0 .. n-2.
make_thresholds <- function(K, asym = 0) {
  n           <- K - 1L
  total_range <- 3.0
  sym         <- seq(-total_range / 2, total_range / 2, length.out = n)
  if (asym == 0 || n <= 2L) return(sym)
  k_idx       <- 0:(n - 2L)
  gap_weights <- exp(asym * k_idx / (n - 2L))
  gaps        <- gap_weights / sum(gap_weights) * total_range
  b           <- cumsum(c(-total_range / 2, gaps[-length(gaps)]))
  c(b, b[n - 1L] + gaps[n - 1L])
}

# ── Model helpers ──────────────────────────────────────────────────────────────

gpcm_probs <- function(theta, a, b) {
  K  <- length(b) + 1L
  ln <- matrix(0, nrow = length(theta), ncol = K)
  for (k in seq_len(K - 1L))
    ln[, k + 1L] <- ln[, k] + a * (theta - b[k])
  ln <- ln - apply(ln, 1, max)
  ex <- exp(ln); ex / rowSums(ex)
}

sim_gpcm <- function(theta, a, b) {
  probs <- gpcm_probs(theta, a, b)
  apply(probs, 1, function(p) sample.int(length(p), 1L, prob = p) - 1L)
}

grm_probs <- function(theta, a, b) {
  K   <- length(b) + 1L
  cum <- cbind(1, plogis(outer(theta, b, function(th, bk) a * (th - bk))), 0)
  probs <- cum[, 1:K] - cum[, 2:(K + 1L)]
  pmax(probs, 1e-15)
}

binom_probs <- function(theta, a, b, K, link) {
  eta <- a * theta - b
  p   <- switch(link,
    logit   = plogis(eta),
    cloglog = 1 - exp(-exp(eta)),
    cauchit = 0.5 + atan(eta) / pi
  )
  p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
  outer(p, 0:(K - 1L), function(pp, k) dbinom(k, K - 1L, pp))
}

# Scobit (power logistic): p = logis(theta - b)^alpha, alpha > 0.
# alpha=1 → logit; alpha>1 → right-skewed ERF; alpha<1 → left-skewed.
scobit_probs <- function(theta, b, alpha, K) {
  p <- pmin(pmax(plogis(theta - b)^alpha, 1e-10), 1 - 1e-10)
  outer(p, 0:(K - 1L), function(pp, k) dbinom(k, K - 1L, pp))
}

item_ll <- function(probs, X)
  sum(log(pmax(probs[cbind(seq_along(X), X + 1L)], 1e-15)))

fit_gpcm <- function(X, theta) {
  K   <- max(X) + 1L
  neg <- function(par) -item_ll(gpcm_probs(theta, exp(par[1]), par[-1]), X)
  opt <- optim(c(log(1.5), seq(-1.5, 1.5, len = K - 1L)), neg,
               method = "BFGS", control = list(maxit = 3000, reltol = 1e-9))
  list(a = exp(opt$par[1]), b = opt$par[-1])
}

fit_pcm <- function(X, theta) {
  K   <- max(X) + 1L
  neg <- function(b) -item_ll(gpcm_probs(theta, 1.0, b), X)
  opt <- optim(seq(-1.5, 1.5, len = K - 1L), neg,
               method = "BFGS", control = list(maxit = 3000, reltol = 1e-9))
  list(a = 1.0, b = opt$par)
}

fit_grm <- function(X, theta) {
  K   <- max(X) + 1L
  neg <- function(par) {
    a <- exp(par[1])
    b <- if (K == 2L) par[2] else cumsum(c(par[2], exp(par[3:K])))
    -item_ll(grm_probs(theta, a, b), X)
  }
  b_init <- seq(-1.5, 1.5, len = K - 1L)
  init   <- if (K == 2L) c(log(1.5), b_init) else c(log(1.5), b_init[1], log(diff(b_init)))
  opt    <- optim(init, neg, method = "BFGS", control = list(maxit = 3000, reltol = 1e-9))
  b_est  <- if (K == 2L) opt$par[2] else cumsum(c(opt$par[2], exp(opt$par[3:K])))
  list(a = exp(opt$par[1]), b = b_est)
}

fit_binom_1pl <- function(X, theta, link) {
  K    <- max(X) + 1L; size <- K - 1L
  fam  <- switch(link, logit = binomial("logit"), cloglog = binomial("cloglog"), NULL)
  if (!is.null(fam)) {
    mod <- glm(cbind(X, size - X) ~ offset(theta), family = fam)
    return(list(a = 1.0, b = -coef(mod)[["(Intercept)"]]))
  }
  neg <- function(b) -item_ll(binom_probs(theta, 1.0, b, K, link), X)
  opt <- optim(0, neg, method = "Brent", lower = -6, upper = 6)
  list(a = 1.0, b = opt$par)
}

fit_scobit <- function(X, theta) {
  K   <- max(X) + 1L
  neg <- function(par) {
    -item_ll(scobit_probs(theta, par[1], exp(par[2]), K), X)
  }
  opt <- optim(c(0, 0), neg, method = "BFGS",
               control = list(maxit = 3000, reltol = 1e-9))
  list(b = opt$par[1], alpha = exp(opt$par[2]))
}

fit_binom_2pl <- function(X, theta, link = "logit") {
  K    <- max(X) + 1L; size <- K - 1L
  if (link == "logit") {
    mod <- glm(cbind(X, size - X) ~ theta, family = binomial("logit"))
    return(list(a = coef(mod)[["theta"]], b = -coef(mod)[["(Intercept)"]]))
  }
  neg <- function(par) -item_ll(binom_probs(theta, par[1], par[2], K, link), X)
  opt <- optim(c(1, 0), neg, method = "BFGS")
  list(a = opt$par[1], b = opt$par[2])
}

# Fit item parameters given training responses and thetas
fit_model <- function(nm, X_tr, theta_tr) {
  switch(nm,
    gpcm              = fit_gpcm(X_tr, theta_tr),
    pcm               = fit_pcm(X_tr, theta_tr),
    grm               = fit_grm(X_tr, theta_tr),
    binom_1pl_logit   = fit_binom_1pl(X_tr, theta_tr, "logit"),
    binom_1pl_scobit  = fit_scobit(X_tr, theta_tr),
    binom_1pl_cloglog = fit_binom_1pl(X_tr, theta_tr, "cloglog"),
    binom_1pl_cauchit = fit_binom_1pl(X_tr, theta_tr, "cauchit"),
    binom_2pl_logit   = fit_binom_2pl(X_tr, theta_tr, "logit"),
    stop("unknown: ", nm)
  )
}

# Predict on test thetas given fitted parameters
predict_model <- function(nm, f, theta_te, K) {
  switch(nm,
    gpcm = , pcm    = gpcm_probs(theta_te, f$a, f$b),
    grm             = grm_probs(theta_te, f$a, f$b),
    binom_1pl_scobit = scobit_probs(theta_te, f$b, f$alpha, K),
    np_spline = {
      p_hat <- pmin(pmax(
        as.numeric(predict(f$mod, newdata = data.frame(th = theta_te), type = "response")),
        1e-10), 1 - 1e-10)
      outer(p_hat, 0:(K - 1L), function(p, k) dbinom(k, K - 1L, p))
    },
    binom_probs(theta_te, f$a, f$b, K, sub(".*_", "", nm))
  )
}

# ── IMV helpers ────────────────────────────────────────────────────────────────
imv_c <- function(y, pctt.tab, p1, p2) {
  nn  <- length(pctt.tab); iis <- 0:(nn - 1L); om <- numeric(nn)
  for (ii in iis) {
    ns <- om.tmp <- numeric()
    for (jj in iis[-match(ii, iis)]) {
      y2   <- y[y$resp %in% c(ii, jj), ]
      resp <- ifelse(y2$resp == ii, 1, 0)
      p1ii <- y2[[paste0(p1, ii)]]; p1jj <- y2[[paste0(p1, jj)]]
      p2ii <- y2[[paste0(p2, ii)]]; p2jj <- y2[[paste0(p2, jj)]]
      z    <- data.frame(resp = resp,
                         p1   = p1ii / (p1ii + p1jj),
                         p2   = p2ii / (p2ii + p2jj))
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
  df         <- as.data.frame(probs_base); colnames(df) <- paste0("p1", 0:(K - 1L))
  df$resp    <- X
  enh        <- as.data.frame(probs_enh)
  for (k in 0:(K - 1L)) df[[paste0("p2", k)]] <- enh[, k + 1L]
  df[complete.cases(df), ]
}

rmse_fn <- function(probs, X, K) {
  exp_score <- as.numeric(probs %*% 0:(K - 1L)) / (K - 1L)
  sqrt(mean((exp_score - X / (K - 1L))^2))
}

# ── One condition × rep ────────────────────────────────────────────────────────
model_names <- c("gpcm", "pcm", "grm", "tutz",
                 "binom_1pl_logit", "binom_1pl_scobit",
                 "binom_1pl_cloglog", "binom_1pl_cauchit",
                 "binom_2pl_logit", "np_spline")

run_one <- function(K, asym, rep_idx) {
  set.seed(rep_idx * 997L + as.integer(K) * 31L + as.integer(round(asym * 100)))

  theta  <- rnorm(N_PERSONS)
  b_base <- make_thresholds(K, asym)
  shifts <- runif(N_ITEMS, -1, 1)

  # Phase 1: simulate all items, per-item discrimination ~ LogNormal ──────────
  X_mat <- matrix(NA_integer_, N_PERSONS, N_ITEMS)
  for (j in seq_len(N_ITEMS)) {
    a_j <- rlnorm(1L, meanlog = 0, sdlog = LNORM_SDLOG)
    b_j <- b_base + shifts[j] + rnorm(K - 1L, 0, THRESH_NOISE_SD)
    X_j <- sim_gpcm(theta, a_j, b_j)
    tries <- 0L
    while (length(unique(X_j)) < K && tries < 5L) {
      shifts[j] <- runif(1, -1, 1)
      b_j <- b_base + shifts[j] + rnorm(K - 1L, 0, THRESH_NOISE_SD)
      X_j <- sim_gpcm(theta, a_j, b_j); tries <- tries + 1L
    }
    if (length(unique(X_j)) < K) return(NULL)
    X_mat[, j] <- X_j
  }

  # Phase 2: 20% cell-level holdout ─────────────────────────────────────────
  # Randomly mask 20% of person×item cells; fit on observed, evaluate on masked.
  n_ho    <- as.integer(0.2 * N_PERSONS * N_ITEMS)
  ho_lin  <- sample(N_PERSONS * N_ITEMS, n_ho)
  ho_mat  <- matrix(FALSE, N_PERSONS, N_ITEMS)
  ho_mat[ho_lin] <- TRUE
  ho_row  <- row(ho_mat)[ho_lin]   # person index for each held-out cell
  ho_col  <- col(ho_mat)[ho_lin]   # item index for each held-out cell
  all_X_ho <- X_mat[ho_lin]        # actual responses at held-out cells
  pctt.tab <- as.numeric(table(factor(all_X_ho, levels = 0L:(K - 1L)))) / length(all_X_ho)

  # Training matrix: NA at held-out cells
  X_tr_df <- as.data.frame(X_mat)
  X_tr_df[ho_mat] <- NA_integer_
  theta_grid <- seq(-4, 4, length.out = 101L)

  # Phase 3: fit GPCM with mirt MMLE on training data ─────────────────────────
  # mirt handles missing cells naturally. EAP thetas (using observed responses
  # per person) are used to calibrate the binomial models.
  mod_gpcm <- tryCatch(
    suppressMessages(mirt(X_tr_df, 1, itemtype = "gpcm", verbose = FALSE)),
    error = function(e) NULL
  )
  if (is.null(mod_gpcm)) return(NULL)
  theta_eap <- as.numeric(fscores(mod_gpcm, method = "EAP")[, 1])

  extract_params <- function(mod) {
    lapply(seq_len(N_ITEMS), function(j) {
      p <- coef(mod, IRTpars = TRUE)[[j]]
      list(a = as.numeric(p[, "a"]),
           b = as.numeric(p[, paste0("b", seq_len(K - 1L))]))
    })
  }

  # Phase 4: per model — fit, estimate theta on observed cells, predict held-out
  stacked <- lapply(setNames(model_names, model_names),
                    function(.) matrix(numeric(0), ncol = K))

  for (nm in model_names) {
    fitted <- NULL
    if (nm == "gpcm") {
      fitted <- extract_params(mod_gpcm)

    } else if (nm == "pcm") {
      pv <- suppressMessages(
        mirt(X_tr_df, 1, itemtype = "gpcm", pars = "values", verbose = FALSE)
      )
      pv$value[pv$name == "a1"] <- 1
      pv$est[pv$name   == "a1"] <- FALSE
      mod_pcm <- tryCatch(
        suppressMessages(mirt(X_tr_df, 1, itemtype = "gpcm", pars = pv, verbose = FALSE)),
        error = function(e) NULL
      )
      if (!is.null(mod_pcm)) fitted <- extract_params(mod_pcm)

    } else if (nm == "grm") {
      mod_grm <- tryCatch(
        suppressMessages(mirt(X_tr_df, 1, itemtype = "graded", verbose = FALSE)),
        error = function(e) NULL
      )
      if (!is.null(mod_grm)) fitted <- extract_params(mod_grm)

    } else if (nm == "np_spline") {
      # Monotone spline on logit(p) using binomial likelihood — the correct
      # generative model for X ~ Binomial(K-1, p). Fits the same family as
      # 2PL+logit but relaxes the logistic link to any monotone function.
      fit_list <- lapply(seq_len(N_ITEMS), function(j) {
        obs_j <- !ho_mat[, j]
        df_j  <- data.frame(X  = X_mat[obs_j, j],
                            th = theta_eap[obs_j])
        tryCatch(
          list(mod = scam::scam(cbind(X, K - 1L - X) ~ s(th, bs = "mpi"),
                                data = df_j, family = binomial())),
          error = function(e) NULL
        )
      })
      if (!any(sapply(fit_list, is.null))) fitted <- fit_list

    } else {
      # Binomial models: fit per item using only observed cells + EAP thetas
      fit_list <- lapply(seq_len(N_ITEMS), function(j) {
        obs_j <- !ho_mat[, j]
        tryCatch(fit_model(nm, X_mat[obs_j, j], theta_eap[obs_j]),
                 error = function(e) NULL)
      })
      if (!any(sapply(fit_list, is.null))) fitted <- fit_list
    }
    if (is.null(fitted)) next

    # Grid-search MLE theta using each person's observed (non-held-out) responses
    total_ll <- matrix(0, N_PERSONS, length(theta_grid))
    ok <- TRUE
    for (j in seq_len(N_ITEMS)) {
      obs_j <- which(!ho_mat[, j])
      pr_grid <- tryCatch(predict_model(nm, fitted[[j]], theta_grid, K),
                          error = function(e) NULL)
      if (is.null(pr_grid) || anyNA(pr_grid)) { ok <- FALSE; break }
      total_ll[obs_j, ] <- total_ll[obs_j, ] +
        t(log(pmax(pr_grid, 1e-15)))[X_mat[obs_j, j] + 1L, ]
    }
    if (!ok) next
    theta_est <- theta_grid[apply(total_ll, 1, which.max)]

    # Predict held-out cells at estimated theta ────────────────────────────
    pr_ho <- matrix(NA_real_, n_ho, K)
    for (j in seq_len(N_ITEMS)) {
      cells_j <- which(ho_col == j)
      if (length(cells_j) == 0L) next
      pr_j <- tryCatch(
        predict_model(nm, fitted[[j]], theta_est[ho_row[cells_j]], K),
        error = function(e) matrix(NA_real_, length(cells_j), K)
      )
      pr_ho[cells_j, ] <- pr_j
    }
    if (anyNA(pr_ho)) next
    stacked[[nm]] <- pr_ho
  }

  # Phase 5: compute metrics on held-out cells ───────────────────────────────
  pr_1pl  <- stacked[["binom_1pl_logit"]]
  pr_gpcm <- stacked[["gpcm"]]
  rows <- lapply(model_names, function(nm) {
    pr <- stacked[[nm]]
    if (is.null(pr) || nrow(pr) == 0L || anyNA(pr)) return(NULL)

    if (nm == "gpcm" || is.null(pr_gpcm) || nrow(pr_gpcm) == 0L) {
      imv_c_gpcm <- 0; imv_t_gpcm <- 0
    } else {
      ydf <- make_pair_df(pr, pr_gpcm, all_X_ho, K)
      imv_c_gpcm <- tryCatch(imv_c(ydf, pctt.tab, "p1", "p2"), error = function(e) NA_real_)
      imv_t_gpcm <- tryCatch(imv_t(ydf, pctt.tab, "p1", "p2"), error = function(e) NA_real_)
    }

    if (nm == "binom_1pl_logit" || is.null(pr_1pl) || nrow(pr_1pl) == 0L || anyNA(pr_1pl)) {
      imv_c_1pl <- 0; imv_t_1pl <- 0
    } else {
      ydf_1pl <- make_pair_df(pr_1pl, pr, all_X_ho, K)
      imv_c_1pl <- tryCatch(imv_c(ydf_1pl, pctt.tab, "p1", "p2"), error = function(e) NA_real_)
      imv_t_1pl <- tryCatch(imv_t(ydf_1pl, pctt.tab, "p1", "p2"), error = function(e) NA_real_)
    }

    data.frame(
      K = K, asym = asym, rep = rep_idx, model = nm,
      rmse = rmse_fn(pr, all_X_ho, K),
      imv_c = imv_c_gpcm, imv_t = imv_t_gpcm,
      imv_c_1pl = imv_c_1pl, imv_t_1pl = imv_t_1pl,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# ── Build job list and run ─────────────────────────────────────────────────────
jobs <- expand.grid(K = K_grid, rep = seq_len(N_REPS), stringsAsFactors = FALSE)
jobs$asym <- runif(nrow(jobs), -log(8), log(8))

message(sprintf("Running %d jobs (K in {5,7}, %d reps, asym ~ Unif(-log8,log8), a ~ LogNorm) on %d cores ...",
                nrow(jobs), N_REPS, N_CORES))

results_list <- mclapply(seq_len(nrow(jobs)), function(i) {
  r <- jobs[i, ]
  tryCatch(
    run_one(r$K, r$asym, r$rep),
    error = function(e) { message("job ", i, " failed: ", e$message); NULL }
  )
}, mc.cores = N_CORES, mc.preschedule = FALSE)

results <- do.call(rbind, Filter(Negate(is.null), results_list))
results$model <- factor(results$model, levels = model_names)

saveRDS(results, out_file)
message(sprintf("Saved %d rows → %s", nrow(results), out_file))

# ── Quick summary (binned asym for readability) ─────────────────────────
results$asym_bin <- cut(results$asym,
                        breaks = c(-log(8), -0.5, 0.5, log(8)),
                        labels = c("lower-wider", "symmetric", "upper-wider"))
agg <- aggregate(cbind(rmse, imv_c, imv_t, imv_c_1pl, imv_t_1pl) ~ model + K + asym_bin,
                 data = results, FUN = mean, na.rm = TRUE)
agg <- agg[order(agg$K, agg$asym_bin, agg$imv_c), ]
print(agg, digits = 4, row.names = FALSE)
