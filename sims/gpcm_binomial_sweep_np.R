# sims/gpcm_binomial_sweep_np.R
#
# Section 2 simulation: nonparametric polytomous DGP.
# Each item's K-1 category boundary functions P(X >= k | theta) are a mixture of
# a GPCM logistic baseline and independent random monotone splines.
#   flex = 0 -> pure GPCM boundaries  (logistic)
#   flex = 1 -> pure random monotone splines (unconstrained shape)
# flex ~ Uniform(0, 1) per replication.
#
# All fitting models and metrics are identical to gpcm_binomial_sweep.R.
# Saves sims/gpcm_binomial_results_np_oracle.rds.
#
# Usage: Rscript sims/gpcm_binomial_sweep_np.R

suppressPackageStartupMessages(library(imv))
suppressPackageStartupMessages(library(mirt))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(scam))

# ── Parameters ─────────────────────────────────────────────────────────────────
set.seed(2027)
N_PERSONS       <- 1000L
N_ITEMS         <- 10L
N_REPS          <- 100L
N_CORES         <- 4L
LNORM_SDLOG     <- 0.5
K_grid          <- c(5L, 7L)
THRESH_NOISE_SD <- 0.3
out_file        <- "sims/gpcm_binomial_results_np_oracle.rds"

# ── Standard model helpers (shared with gpcm_binomial_sweep.R) ─────────────────

gpcm_probs <- function(theta, a, b) {
  K  <- length(b) + 1L
  ln <- matrix(0, nrow = length(theta), ncol = K)
  for (k in seq_len(K - 1L))
    ln[, k + 1L] <- ln[, k] + a * (theta - b[k])
  ln <- ln - apply(ln, 1, max)
  ex <- exp(ln); ex / rowSums(ex)
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
  neg <- function(par) -item_ll(scobit_probs(theta, par[1], exp(par[2]), K), X)
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

predict_model <- function(nm, f, theta_te, K) {
  switch(nm,
    gpcm = , pcm     = gpcm_probs(theta_te, f$a, f$b),
    grm              = grm_probs(theta_te, f$a, f$b),
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

# ── NP DGP helpers ─────────────────────────────────────────────────────────────

# Random monotone Hermite spline: theta -> [0,1], monotone increasing.
rand_mono_spline <- function(n_knots = 7L) {
  x_k <- seq(-4, 4, length.out = n_knots)
  y_k <- sort(runif(n_knots))
  splinefun(x_k, y_k, method = "monoH.FC")
}

# Category probabilities from mixed NP/GPCM DGP.
# splines_j: list of K-1 random monotone splines (one per boundary).
np_probs <- function(theta, a_j, b_j, K, flex, splines_j) {
  n     <- K - 1L
  theta_c <- pmin(pmax(theta, -4), 4)   # clamp to spline domain

  # GPCM cumulative P(X >= k) for k = 1,...,K-1
  gpcm_cat <- gpcm_probs(theta, a_j, b_j)
  gpcm_cum <- matrix(NA_real_, length(theta), n)
  for (k in seq_len(n))
    gpcm_cum[, k] <- 1 - rowSums(gpcm_cat[, seq_len(k), drop = FALSE])

  # Random spline boundary values (clamped to [0,1])
  rand_cum <- matrix(NA_real_, length(theta), n)
  for (k in seq_len(n))
    rand_cum[, k] <- pmin(pmax(splines_j[[k]](theta_c), 0), 1)

  # Convex mixture; re-sort rows to enforce g_1 > g_2 > ... > g_{K-1}
  cum <- (1 - flex) * gpcm_cum + flex * rand_cum
  if (n > 1L) cum <- t(apply(cum, 1, sort, decreasing = TRUE))

  # Category probabilities from cumulative differences
  probs <- cbind(
    1 - cum[, 1],
    if (n > 1L) cum[, seq_len(n - 1L), drop = FALSE] -
                cum[, seq_len(n - 1L) + 1L, drop = FALSE],
    cum[, n]
  )
  pmax(probs, 1e-10)
}

sim_np_item <- function(theta, a_j, b_j, K, flex, splines_j) {
  probs <- np_probs(theta, a_j, b_j, K, flex, splines_j)
  probs <- probs / rowSums(probs)
  apply(probs, 1, function(p) sample.int(K, 1L, prob = p) - 1L)
}

# ── One NP replication ─────────────────────────────────────────────────────────
model_names <- c("gpcm", "pcm", "grm",
                 "binom_1pl_logit", "binom_1pl_scobit",
                 "binom_1pl_cloglog", "binom_1pl_cauchit",
                 "binom_2pl_logit", "np_spline")

run_one_np <- function(K, flex, rep_idx) {
  set.seed(rep_idx * 997L + as.integer(K) * 31L +
           as.integer(round(flex * 1000)) + 10000L)

  theta   <- rnorm(N_PERSONS)
  n_bound <- K - 1L
  shifts  <- runif(N_ITEMS, -1, 1)

  # Phase 1: NP DGP -----------------------------------------------------------
  X_mat          <- matrix(NA_integer_, N_PERSONS, N_ITEMS)
  true_item_params <- vector("list", N_ITEMS)
  for (j in seq_len(N_ITEMS)) {
    a_j       <- rlnorm(1L, meanlog = 0, sdlog = LNORM_SDLOG)
    b_j       <- seq(-1.5, 1.5, length.out = n_bound) + shifts[j] +
                 rnorm(n_bound, 0, THRESH_NOISE_SD)
    splines_j <- lapply(seq_len(n_bound), function(k) rand_mono_spline())

    X_j   <- sim_np_item(theta, a_j, b_j, K, flex, splines_j)
    tries <- 0L
    while (length(unique(X_j)) < K && tries < 5L) {
      shifts[j]  <- runif(1, -1, 1)
      b_j        <- seq(-1.5, 1.5, length.out = n_bound) + shifts[j] +
                    rnorm(n_bound, 0, THRESH_NOISE_SD)
      splines_j  <- lapply(seq_len(n_bound), function(k) rand_mono_spline())
      X_j        <- sim_np_item(theta, a_j, b_j, K, flex, splines_j)
      tries      <- tries + 1L
    }
    if (length(unique(X_j)) < K) return(NULL)
    X_mat[, j] <- X_j
    true_item_params[[j]] <- list(a = a_j, b = b_j, flex = flex, splines = splines_j)
  }

  # Phase 2: 20% cell holdout -------------------------------------------------
  n_ho     <- as.integer(0.2 * N_PERSONS * N_ITEMS)
  ho_lin   <- sample(N_PERSONS * N_ITEMS, n_ho)
  ho_mat   <- matrix(FALSE, N_PERSONS, N_ITEMS)
  ho_mat[ho_lin] <- TRUE
  ho_row   <- row(ho_mat)[ho_lin]
  ho_col   <- col(ho_mat)[ho_lin]
  all_X_ho <- X_mat[ho_lin]
  pctt.tab <- as.numeric(table(factor(all_X_ho, levels = 0L:(K - 1L)))) /
              length(all_X_ho)

  X_tr_df    <- as.data.frame(X_mat)
  X_tr_df[ho_mat] <- NA_integer_
  theta_grid <- seq(-4, 4, length.out = 101L)

  # Phase 3: mirt GPCM on training data ---------------------------------------
  mod_gpcm <- tryCatch(
    suppressMessages(mirt(X_tr_df, 1, itemtype = "gpcm", verbose = FALSE)),
    error = function(e) NULL
  )
  if (is.null(mod_gpcm)) return(NULL)
  theta_eap <- as.numeric(fscores(mod_gpcm, method = "EAP")[, 1])

  pr_oracle <- matrix(NA_real_, n_ho, K)
  for (j in seq_len(N_ITEMS)) {
    cells_j <- which(ho_col == j)
    if (!length(cells_j)) next
    tp <- true_item_params[[j]]
    pr_oracle[cells_j, ] <- np_probs(theta[ho_row[cells_j]], tp$a, tp$b, K, tp$flex, tp$splines)
  }

  extract_params <- function(mod) {
    lapply(seq_len(N_ITEMS), function(j) {
      p <- coef(mod, IRTpars = TRUE)[[j]]
      list(a = as.numeric(p[, "a"]),
           b = as.numeric(p[, paste0("b", seq_len(K - 1L))]))
    })
  }

  # Phase 4: fit, score theta, predict held-out -------------------------------
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
        suppressMessages(mirt(X_tr_df, 1, itemtype = "gpcm", pars = pv,
                              verbose = FALSE)),
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
      fit_list <- lapply(seq_len(N_ITEMS), function(j) {
        obs_j <- !ho_mat[, j]
        tryCatch(fit_model(nm, X_mat[obs_j, j], theta_eap[obs_j]),
                 error = function(e) NULL)
      })
      if (!any(sapply(fit_list, is.null))) fitted <- fit_list
    }
    if (is.null(fitted)) next

    total_ll <- matrix(0, N_PERSONS, length(theta_grid))
    ok <- TRUE
    for (j in seq_len(N_ITEMS)) {
      obs_j   <- which(!ho_mat[, j])
      pr_grid <- tryCatch(predict_model(nm, fitted[[j]], theta_grid, K),
                          error = function(e) NULL)
      if (is.null(pr_grid) || anyNA(pr_grid)) { ok <- FALSE; break }
      total_ll[obs_j, ] <- total_ll[obs_j, ] +
        t(log(pmax(pr_grid, 1e-15)))[X_mat[obs_j, j] + 1L, ]
    }
    if (!ok) next
    theta_est <- theta_grid[apply(total_ll, 1, which.max)]

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

  # Phase 5: metrics ----------------------------------------------------------
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

    if (nm == "binom_1pl_logit" || is.null(pr_1pl) ||
        nrow(pr_1pl) == 0L || anyNA(pr_1pl)) {
      imv_c_1pl <- 0; imv_t_1pl <- 0
    } else {
      ydf_1pl <- make_pair_df(pr_1pl, pr, all_X_ho, K)
      imv_c_1pl <- tryCatch(imv_c(ydf_1pl, pctt.tab, "p1", "p2"), error = function(e) NA_real_)
      imv_t_1pl <- tryCatch(imv_t(ydf_1pl, pctt.tab, "p1", "p2"), error = function(e) NA_real_)
    }

    if (is.null(pr_oracle) || anyNA(pr_oracle)) {
      imv_c_oracle <- NA_real_; imv_t_oracle <- NA_real_
    } else {
      ydf_oracle   <- make_pair_df(pr, pr_oracle, all_X_ho, K)
      imv_c_oracle <- tryCatch(imv_c(ydf_oracle, pctt.tab, "p1", "p2"), error = function(e) NA_real_)
      imv_t_oracle <- tryCatch(imv_t(ydf_oracle, pctt.tab, "p1", "p2"), error = function(e) NA_real_)
    }

    data.frame(
      K = K, flex = flex, rep = rep_idx, model = nm,
      rmse         = rmse_fn(pr, all_X_ho, K),
      imv_c        = imv_c_gpcm,    imv_t        = imv_t_gpcm,
      imv_c_1pl    = imv_c_1pl,     imv_t_1pl    = imv_t_1pl,
      imv_c_oracle = imv_c_oracle,  imv_t_oracle = imv_t_oracle,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# ── Build jobs and run ─────────────────────────────────────────────────────────
jobs_np <- expand.grid(K = K_grid, rep = seq_len(N_REPS), stringsAsFactors = FALSE)
jobs_np$flex <- runif(nrow(jobs_np), 0, 1)

message(sprintf(
  "Running %d NP jobs (K in {5,7}, %d reps, flex ~ Unif(0,1)) on %d cores ...",
  nrow(jobs_np), N_REPS, N_CORES
))

results_np_list <- mclapply(seq_len(nrow(jobs_np)), function(i) {
  r <- jobs_np[i, ]
  tryCatch(
    run_one_np(r$K, r$flex, r$rep),
    error = function(e) { message("NP job ", i, " failed: ", e$message); NULL }
  )
}, mc.cores = N_CORES, mc.preschedule = FALSE)

results_np <- do.call(rbind, Filter(Negate(is.null), results_np_list))
results_np$model <- factor(results_np$model, levels = model_names)
saveRDS(results_np, out_file)
message(sprintf("Saved %d NP rows → %s", nrow(results_np), out_file))

# ── Quick summary ──────────────────────────────────────────────────────────────
results_np$flex_bin <- cut(results_np$flex,
                           breaks = c(0, 1/3, 2/3, 1),
                           labels = c("low (0-0.33)", "mid (0.33-0.67)", "high (0.67-1)"),
                           include.lowest = TRUE)
agg <- aggregate(cbind(rmse, imv_c, imv_t, imv_c_1pl, imv_t_1pl, imv_c_oracle, imv_t_oracle) ~
                   model + K + flex_bin,
                 data = results_np, FUN = mean, na.rm = TRUE)
agg <- agg[order(agg$K, agg$flex_bin, agg$imv_c_oracle), ]
print(agg, digits = 4, row.names = FALSE)
