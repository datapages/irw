# sims/gpcm_binomial_known_theta.R
#
# Simulation: GPCM-generated data, abilities treated as known.
# Compares GPCM vs. binomial approximations (various link functions)
# on RMSE and IMV (both imv_c and imv_t).
#
# Item parameters are estimated via MLE with theta held fixed (no joint
# estimation). Predictions also use the true theta, so all differences
# in metrics reflect model misspecification, not theta estimation error.
#
# Mirrors the imv_c / imv_t / pctt.tab baseline logic in
# poly_compare_multi_compute.R.
#
# Usage: Rscript sims/gpcm_binomial_known_theta.R

suppressPackageStartupMessages(library(imv))

# ── Control parameters ─────────────────────────────────────────────────────────
set.seed(42)
N      <- 2000L                       # persons
K      <- 5L                          # response categories (0..K-1)
a_true <- 1.5                         # discrimination; 1 → PCM, >1 → GPCM
b_true <- c(-1.5, -0.5, 0.5, 1.5)    # K-1 category thresholds

stopifnot(length(b_true) == K - 1L)

# ── 1. GPCM helpers ────────────────────────────────────────────────────────────

# Returns N x K matrix of P(X=k | theta) under GPCM
gpcm_probs <- function(theta, a, b) {
  K      <- length(b) + 1L
  ln     <- matrix(0, nrow = length(theta), ncol = K)
  for (k in seq_len(K - 1L))
    ln[, k + 1L] <- ln[, k] + a * (theta - b[k])
  ln <- ln - apply(ln, 1, max)   # numerical stability
  ex <- exp(ln)
  ex / rowSums(ex)
}

sim_gpcm <- function(theta, a, b) {
  probs <- gpcm_probs(theta, a, b)
  apply(probs, 1, function(p) sample.int(length(p), 1L, prob = p) - 1L)
}

# ── 2. Generate data ───────────────────────────────────────────────────────────
theta <- rnorm(N)
X     <- sim_gpcm(theta, a_true, b_true)

cat(sprintf("Data: N=%d  K=%d  a_true=%.2f  b_true=(%s)\n",
            N, K, a_true, paste(round(b_true, 2), collapse = ", ")))
cat("Response distribution:\n"); print(prop.table(table(X))); cat("\n")

# ── 3. Item-parameter MLE with known theta ─────────────────────────────────────

# Log-likelihood for a model that produces a prob matrix (rows=persons, cols=cats)
item_ll <- function(probs, X)
  sum(log(pmax(probs[cbind(seq_along(X), X + 1L)], 1e-15)))

## GPCM (free discrimination)
fit_gpcm <- function(X, theta) {
  K <- max(X) + 1L
  neg <- function(par) {
    a <- exp(par[1]); b <- par[-1]
    -item_ll(gpcm_probs(theta, a, b), X)
  }
  opt <- optim(c(log(1.5), seq(-1.5, 1.5, len = K - 1L)), neg,
               method = "BFGS", control = list(maxit = 5000, reltol = 1e-10))
  list(a = exp(opt$par[1]), b = opt$par[-1], converged = opt$convergence == 0)
}

## PCM (discrimination fixed at 1)
fit_pcm <- function(X, theta) {
  K <- max(X) + 1L
  neg <- function(b) -item_ll(gpcm_probs(theta, 1.0, b), X)
  opt <- optim(seq(-1.5, 1.5, len = K - 1L), neg,
               method = "BFGS", control = list(maxit = 5000, reltol = 1e-10))
  list(a = 1.0, b = opt$par, converged = opt$convergence == 0)
}

## GRM: cumulative logit model with K-1 ordered thresholds
# P(X >= k | theta) = logis(a*(theta - b[k]))
grm_probs <- function(theta, a, b) {
  K        <- length(b) + 1L
  cum      <- matrix(c(rep(1, length(theta)),
                       plogis(outer(theta, b, function(th, bk) a * (th - bk))),
                       rep(0, length(theta))),
                     nrow = length(theta))
  probs <- cum[, 1:K] - cum[, 2:(K + 1L)]
  pmax(probs, 1e-15)
}

fit_grm <- function(X, theta) {
  K <- max(X) + 1L
  # Enforce threshold ordering via log-gaps: b = cumsum(c(b1, exp(gaps)))
  neg <- function(par) {
    a <- exp(par[1])
    b <- if (K == 2L) par[2] else cumsum(c(par[2], exp(par[3:K])))
    -item_ll(grm_probs(theta, a, b), X)
  }
  b_init <- seq(-1.5, 1.5, len = K - 1L)
  init   <- if (K == 2L) c(log(1.5), b_init) else c(log(1.5), b_init[1], log(diff(b_init)))
  opt    <- optim(init, neg, method = "BFGS",
                  control = list(maxit = 5000, reltol = 1e-10))
  b_est  <- if (K == 2L) opt$par[2] else cumsum(c(opt$par[2], exp(opt$par[3:K])))
  list(a = exp(opt$par[1]), b = b_est, converged = opt$convergence == 0)
}

## Binomial model: P(X=k) = dbinom(k, K-1, p)  where p = link^{-1}(a*theta - b)
binom_probs <- function(theta, a, b, K, link) {
  eta <- a * theta - b
  p   <- switch(link,
    logit   = plogis(eta),
    probit  = pnorm(eta),
    cloglog = 1 - exp(-exp(eta)),
    cauchit = 0.5 + atan(eta) / pi,
    stop("unknown link: ", link)
  )
  p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
  outer(p, 0:(K - 1L), function(pp, k) dbinom(k, K - 1L, pp))
}

## 1PL binomial: a=1 fixed.
# For logit/probit/cloglog: GLM with offset so slope is forced to 1.
# For cauchit: optim (no matching base family).
fit_binom_1pl <- function(X, theta, link) {
  K    <- max(X) + 1L
  size <- K - 1L
  glm_fam <- switch(link,
    logit   = binomial("logit"),
    probit  = binomial("probit"),
    cloglog = binomial("cloglog"),
    NULL
  )
  if (!is.null(glm_fam)) {
    mod   <- glm(cbind(X, size - X) ~ offset(theta), family = glm_fam)
    b_est <- -coef(mod)[["(Intercept)"]]
    return(list(a = 1.0, b = b_est, converged = mod$converged))
  }
  neg <- function(b) -item_ll(binom_probs(theta, 1.0, b, K, link), X)
  opt <- optim(0, neg, method = "Brent", lower = -6, upper = 6)
  list(a = 1.0, b = opt$par, converged = opt$convergence == 0)
}

## 2PL binomial: both a and b estimated.
# For logit: GLM directly (slope = a, intercept → b).
# Others: optim.
fit_binom_2pl <- function(X, theta, link = "logit") {
  K    <- max(X) + 1L
  size <- K - 1L
  if (link == "logit") {
    mod   <- glm(cbind(X, size - X) ~ theta, family = binomial("logit"))
    a_est <- coef(mod)[["theta"]]
    b_est <- -coef(mod)[["(Intercept)"]]
    return(list(a = a_est, b = b_est, converged = mod$converged))
  }
  neg <- function(par) -item_ll(binom_probs(theta, par[1], par[2], K, link), X)
  opt <- optim(c(1, 0), neg, method = "BFGS")
  list(a = opt$par[1], b = opt$par[2], converged = opt$convergence == 0)
}

# ── 4. Fit all models ──────────────────────────────────────────────────────────
cat("Fitting models...\n")
fits <- list(
  gpcm          = fit_gpcm(X, theta),
  pcm           = fit_pcm(X, theta),
  grm           = fit_grm(X, theta),
  binom_1pl_logit   = fit_binom_1pl(X, theta, "logit"),
  binom_1pl_probit  = fit_binom_1pl(X, theta, "probit"),
  binom_1pl_cloglog = fit_binom_1pl(X, theta, "cloglog"),
  binom_1pl_cauchit = fit_binom_1pl(X, theta, "cauchit"),
  binom_2pl_logit   = fit_binom_2pl(X, theta, "logit")
)
invisible(lapply(names(fits), function(nm) {
  f <- fits[[nm]]
  cat(sprintf("  %-22s a=%.3f  b=(%s)  converged=%s\n",
              nm, f$a, paste(round(f$b, 3), collapse = ", "), f$converged))
}))
cat("\n")

# ── 5. Predicted probabilities using true theta ────────────────────────────────
get_probs <- function(nm) {
  f <- fits[[nm]]
  if (grepl("^(gpcm|pcm)$", nm))
    return(gpcm_probs(theta, f$a, f$b))
  if (nm == "grm")
    return(grm_probs(theta, f$a, f$b))
  link <- sub(".*_", "", nm)       # e.g. "binom_1pl_logit" → "logit"
  binom_probs(theta, f$a, f$b, K, link)
}

all_probs <- lapply(names(fits), get_probs)
names(all_probs) <- names(fits)

# ── 6. IMV helpers (from poly_compare_multi_compute.R) ────────────────────────
# Baseline: marginal category distribution of observed data
pctt.tab <- as.numeric(table(factor(X, levels = 0L:(K - 1L)))) / N

imv_c <- function(y, pctt.tab, p1, p2) {
  nn  <- length(pctt.tab)
  om  <- numeric(nn)
  iis <- 0:(nn - 1L)
  for (ii in iis) {
    ns <- om.tmp <- numeric()
    jjs <- iis[-match(ii, iis)]
    for (jj in jjs) {
      y2    <- y[y$resp %in% c(ii, jj), ]
      resp  <- ifelse(y2$resp == ii, 1, 0)
      p1.ii <- y2[[paste0(p1, ii)]]; p1.jj <- y2[[paste0(p1, jj)]]
      p2.ii <- y2[[paste0(p2, ii)]]; p2.jj <- y2[[paste0(p2, jj)]]
      z <- data.frame(resp = resp,
                      p1   = p1.ii / (p1.ii + p1.jj),
                      p2   = p2.ii / (p2.ii + p2.jj))
      om.tmp[as.character(jj)] <- imv.binary(z$resp, z$p1, z$p2)
      ns[as.character(jj)]     <- nrow(z)
    }
    om[ii + 1L] <- sum(om.tmp * ns) / sum(ns)
  }
  sum(om * pctt.tab) / sum(pctt.tab)
}

imv_t <- function(y, pctt.tab, p1, p2) {
  nn <- length(pctt.tab)
  om <- numeric(nn - 1L)
  for (ii in 0:(nn - 2L)) {
    resp <- ifelse(y$resp <= ii, 1, 0)
    pr1  <- rowSums(y[, paste0(p1, 0:ii), drop = FALSE])
    pr2  <- rowSums(y[, paste0(p2, 0:ii), drop = FALSE])
    z    <- data.frame(resp = resp, p1 = pr1, p2 = pr2)
    om[ii + 1L] <- imv.binary(z$resp, z$p1, z$p2)
  }
  wts <- pctt.tab[1:(nn - 1L)] / (1 - pctt.tab[nn])
  sum(om * wts) / sum(wts)
}

# Build imv data frame for a given model (baseline = marginal)
make_imv_df <- function(probs_model) {
  df         <- as.data.frame(probs_model)
  colnames(df) <- paste0("p2", 0L:(K - 1L))
  df$resp    <- X
  for (k in 0L:(K - 1L)) df[[paste0("p1", k)]] <- pctt.tab[k + 1L]
  df[complete.cases(df), ]
}

# Pairwise: how much does probs_b improve over probs_a?
make_pair_df <- function(probs_a, probs_b) {
  df         <- as.data.frame(probs_a)
  colnames(df) <- paste0("p1", 0L:(K - 1L))
  df$resp    <- X
  enh        <- as.data.frame(probs_b)
  for (k in 0L:(K - 1L)) df[[paste0("p2", k)]] <- enh[, k + 1L]
  df[complete.cases(df), ]
}

# ── 7. Compute metrics ─────────────────────────────────────────────────────────
# RMSE: rescaled expected score vs. observed (both on [0,1])
rmse_fn <- function(probs) {
  exp_score <- as.numeric(probs %*% 0:(K - 1L)) / (K - 1L)
  sqrt(mean((exp_score - X / (K - 1L))^2))
}

results <- lapply(names(fits), function(nm) {
  pr  <- all_probs[[nm]]
  ydf <- make_imv_df(pr)
  data.frame(
    model     = nm,
    rmse      = round(rmse_fn(pr), 5),
    imv_c_vs_marginal = round(imv_c(ydf, pctt.tab, "p1", "p2"), 5),
    imv_t_vs_marginal = round(imv_t(ydf, pctt.tab, "p1", "p2"), 5),
    stringsAsFactors  = FALSE
  )
})
results_df <- do.call(rbind, results)

cat("── Model metrics (IMV vs. marginal baseline) ─────────────────────────────\n")
print(results_df, row.names = FALSE)
cat("\n")

# Pairwise IMV: each non-GPCM model vs. GPCM (positive = GPCM wins)
binom_models <- grep("^(binom|grm|pcm)", names(fits), value = TRUE)
pair_rows <- lapply(binom_models, function(nm) {
  ydf <- make_pair_df(all_probs[[nm]], all_probs[["gpcm"]])
  data.frame(
    comparison = paste0(nm, " → gpcm"),
    imv_c      = round(imv_c(ydf, pctt.tab, "p1", "p2"), 5),
    imv_t      = round(imv_t(ydf, pctt.tab, "p1", "p2"), 5),
    stringsAsFactors = FALSE
  )
})
pair_df <- do.call(rbind, pair_rows)

cat("── Pairwise IMV: binomial → GPCM (positive = GPCM wins) ─────────────────\n")
print(pair_df, row.names = FALSE)
cat("\n")

# True-parameter predictions (oracle; upper bound)
probs_true <- gpcm_probs(theta, a_true, b_true)
ydf_true   <- make_imv_df(probs_true)
cat(sprintf("Oracle (true params):  RMSE=%.5f  imv_c=%.5f  imv_t=%.5f\n",
            rmse_fn(probs_true),
            imv_c(ydf_true, pctt.tab, "p1", "p2"),
            imv_t(ydf_true, pctt.tab, "p1", "p2")))
