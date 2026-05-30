# sims/gpcm_binomial_sweep.R
#
# Full sweep: GPCM data, known theta, compare GPCM vs. binomial approximations.
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
suppressPackageStartupMessages(library(parallel))

# ── Simulation parameters ──────────────────────────────────────────────────────
set.seed(2026)
N_PERSONS   <- 1000L
N_ITEMS     <- 10L
N_REPS      <- 100L
N_CORES     <- 4L
a_grid      <- c(1.0, 1.5)
K_grid      <- c(5L, 7L)
# asym ~ Uniform(-log(8), log(8)) per rep: 0 = symmetric; +log(8) = wider upper
# gaps with max/min ratio of 8; -log(8) = wider lower gaps same magnitude.
# Dropping K=3: with only 1 inter-threshold gap, asymmetry is undefined.
out_file    <- "sims/gpcm_binomial_results.rds"

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
    probit  = pnorm(eta),
    cloglog = 1 - exp(-exp(eta)),
    cauchit = 0.5 + atan(eta) / pi
  )
  p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
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
  fam  <- switch(link, logit = binomial("logit"), probit = binomial("probit"),
                 cloglog = binomial("cloglog"), NULL)
  if (!is.null(fam)) {
    mod <- glm(cbind(X, size - X) ~ offset(theta), family = fam)
    return(list(a = 1.0, b = -coef(mod)[["(Intercept)"]]))
  }
  neg <- function(b) -item_ll(binom_probs(theta, 1.0, b, K, link), X)
  opt <- optim(0, neg, method = "Brent", lower = -6, upper = 6)
  list(a = 1.0, b = opt$par)
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
    binom_1pl_probit  = fit_binom_1pl(X_tr, theta_tr, "probit"),
    binom_1pl_cloglog = fit_binom_1pl(X_tr, theta_tr, "cloglog"),
    binom_1pl_cauchit = fit_binom_1pl(X_tr, theta_tr, "cauchit"),
    binom_2pl_logit   = fit_binom_2pl(X_tr, theta_tr, "logit"),
    stop("unknown: ", nm)
  )
}

# Predict on test thetas given fitted parameters
predict_model <- function(nm, f, theta_te, K) {
  switch(nm,
    gpcm = , pcm = gpcm_probs(theta_te, f$a, f$b),
    grm  = grm_probs(theta_te, f$a, f$b),
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
model_names <- c("gpcm", "pcm", "grm",
                 "binom_1pl_logit", "binom_1pl_probit",
                 "binom_1pl_cloglog", "binom_1pl_cauchit",
                 "binom_2pl_logit")

run_one <- function(a_true, K, asym, rep_idx) {
  set.seed(rep_idx * 997L + as.integer(K) * 31L + as.integer(round(a_true * 7)) +
           as.integer(round(asym * 100)))

  theta  <- rnorm(N_PERSONS)
  b_base <- make_thresholds(K, asym)
  shifts <- runif(N_ITEMS, -1, 1)

  # 80/20 person-level split: fit on train, evaluate on test.
  # Same split for all items so test observations stack cleanly.
  n_tr     <- as.integer(0.8 * N_PERSONS)
  idx_tr   <- sample(seq_len(N_PERSONS), n_tr)
  idx_te   <- setdiff(seq_len(N_PERSONS), idx_tr)
  theta_tr <- theta[idx_tr]; theta_te <- theta[idx_te]

  all_X   <- integer(0)
  stacked <- lapply(setNames(model_names, model_names),
                    function(.) matrix(numeric(0), ncol = K))

  for (j in seq_len(N_ITEMS)) {
    b_j <- b_base + shifts[j]
    X_j <- sim_gpcm(theta, a_true, b_j)
    tries <- 0L
    while (length(unique(X_j)) < K && tries < 5L) {
      shifts[j] <- runif(1, -1, 1); b_j <- b_base + shifts[j]
      X_j       <- sim_gpcm(theta, a_true, b_j); tries <- tries + 1L
    }
    if (length(unique(X_j)) < K) return(NULL)
    all_X <- c(all_X, X_j[idx_te])
    for (nm in model_names) {
      if (nm == "gpcm") {
        # Oracle GPCM: use the true generating parameters.
        # This isolates misspecification from GPCM estimation noise.
        pr <- gpcm_probs(theta_te, a_true, b_j)
      } else {
        f  <- tryCatch(fit_model(nm, X_j[idx_tr], theta_tr), error = function(e) NULL)
        pr <- if (is.null(f)) matrix(NA_real_, length(idx_te), K) else
                tryCatch(predict_model(nm, f, theta_te, K),
                         error = function(e) matrix(NA_real_, length(idx_te), K))
      }
      stacked[[nm]] <- rbind(stacked[[nm]], pr)
    }
  }

  pctt.tab <- as.numeric(table(factor(all_X, levels = 0L:(K - 1L)))) / length(all_X)

  # All IMV computed as IMV(model, GPCM): p1 = model, p2 = GPCM.
  # Positive = GPCM predicts better than the alternative.
  # GPCM self-comparison = 0 by definition.
  rows <- lapply(model_names, function(nm) {
    pr <- stacked[[nm]]
    if (anyNA(pr)) return(NULL)
    if (nm == "gpcm") {
      return(data.frame(
        a_true = a_true, K = K, asym = asym, rep = rep_idx,
        model = nm, rmse = rmse_fn(pr, all_X, K),
        imv_c = 0, imv_t = 0, stringsAsFactors = FALSE
      ))
    }
    ydf <- make_pair_df(pr, stacked[["gpcm"]], all_X, K)
    data.frame(
      a_true     = a_true,
      K          = K,
      asym = asym,
      rep        = rep_idx,
      model          = nm,
      rmse           = rmse_fn(pr, all_X, K),
      imv_c          = tryCatch(imv_c(ydf, pctt.tab, "p1", "p2"), error = function(e) NA_real_),
      imv_t          = tryCatch(imv_t(ydf, pctt.tab, "p1", "p2"), error = function(e) NA_real_),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# ── Build job list and run ─────────────────────────────────────────────────────
# asym ~ Uniform(-log(8), log(8)) per rep: 0 = symmetric, ±2.08 = max asymmetry.
jobs <- expand.grid(a_true = a_grid, K = K_grid, rep = seq_len(N_REPS),
                    stringsAsFactors = FALSE)
jobs$asym <- runif(nrow(jobs), -log(8), log(8))

message(sprintf("Running %d jobs (K in {5,7}, a in {1.0,1.5}, %d reps, asym ~ Unif(-log8,log8)) on %d cores ...",
                nrow(jobs), N_REPS, N_CORES))

results_list <- mclapply(seq_len(nrow(jobs)), function(i) {
  r <- jobs[i, ]
  tryCatch(
    run_one(r$a_true, r$K, r$asym, r$rep),
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
agg <- aggregate(cbind(rmse, imv_c, imv_t) ~ model + a_true + K + asym_bin,
                 data = results, FUN = mean, na.rm = TRUE)
agg <- agg[order(agg$K, agg$a_true, agg$asym_bin, agg$imv_c), ]
print(agg, digits = 4, row.names = FALSE)
