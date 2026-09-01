# Why the 1PL-AG's alpha is reported as "not identified" on five of the eleven
# tables. Drop this next to guessing_helpers.R and run it. ~12 min. R 4.5.1.
#
# Background. An earlier version of the page printed "alpha-hat 0.000 (p=1.000)"
# on the five ENEM 2013/2014 tables and interior estimates on the other six.
# Doria Torres Irribarra pointed out that p = 1.000 needs an LR statistic below
# about 4e-7, that alpha starts at exactly 0 in fit_1pl_ag(), and that the
# pattern therefore looked like an optimizer never leaving its starting value.
# She also showed by simulation -- two independent free-variance implementations,
# analytic gradients checked against central differences -- that alpha IS
# identified at SD(theta) up to 2.8 when the data contain a guessing floor, so
# the high-SD end is the easy end rather than the hard one.
#
# Both halves of that are right. The conclusion is not that the fits are stuck,
# though: it is that alpha is not identified on those five tables at all, for a
# reason her simulation could not produce. Blocks 1 and 2 establish it on the
# real data; block 3 reproduces her result and shows the two findings agree.
#
# Block 1 needs guessingdata/prepared/, the subsampled response matrices written
# by guessing_compute.R. They are gitignored (regenerable, not results), so run
# the compute script first if this is a fresh checkout. Block 3 is self-contained.

suppressMessages(source("guessing_helpers.R"))
QUAD <- build_quadrature(41)
set.seed(20260722)

NOT_IDENTIFIED <- c("enem_2013_1mil_mt", "enem_2013_1mil_lc", "enem_2013_1mil_ch",
                    "enem_2013_1mil_cn", "enem_2014_1mil_ch")
IDENTIFIED     <- c("enem_2019_1mil_ch", "enem_2024_1mil_ch")

prep <- function(tbl) {
  f <- file.path("guessingdata/prepared", paste0(tbl, ".rds"))
  if (!file.exists(f)) return(NULL)
  resp <- readRDS(f)
  degen <- vapply(resp, function(x) length(unique(x[!is.na(x)])) < 2, logical(1))
  as.matrix(mask_holdout(resp[, !degen, drop = FALSE], 0.2)$train)
}

# --- 1. where does the guessing floor end up? --------------------------------
# alpha enters the likelihood only through the guessing branch:
#   dP/dalpha = (1-r) * s * (1-s) * theta,   s = expit(alpha*theta + gamma)
# so if the 1PL-G stage drives every gamma to -Inf, s vanishes, and the
# log-likelihood is exactly flat in alpha. That is a property of the data (no
# guessing floor to speak of), not of the optimizer.

cat("== 1. gamma from the 1PL-G stage, and the alpha gradient at alpha = 0 ==\n")
cat(sprintf("%-20s %7s %8s %11s %14s %6s\n",
            "table", "med", "max", "max expit", "dlogL/dalpha", "flag"))
for (tbl in c(NOT_IDENTIFIED, IDENTIFIED)) {
  Y <- prep(tbl)
  if (is.null(Y)) { cat(sprintf("%-20s  (prepared/ missing -- skipped)\n", tbl)); next }
  f <- fit_1pl_ag(Y, quad = QUAD)
  g <- f$gamma_g
  # analytic dlogL/dalpha at the AG starting point, alpha = 0
  th <- QUAD$nodes * f$sd
  N <- nrow(Y); J <- ncol(Y)
  r <- t(outer(th, f$beta_g, function(t, b) plogis(t - b)))
  s <- t(outer(th, g, function(t, gg) plogis(gg)))
  P <- pmin(pmax(r + (1 - r) * s, 1e-10), 1 - 1e-10)
  M <- !is.na(Y); Mn <- matrix(as.numeric(M), N, J); Ym <- Y; Ym[!M] <- 0
  post <- .node_posterior(Ym %*% log(P) + (Mn - Ym) %*% log(1 - P), QUAD$weights)$post
  D <- (t(Ym) %*% post) - P * (t(Mn) %*% post)
  th_row <- matrix(th, J, length(th), byrow = TRUE)
  dalpha <- sum(((1 - r) * s * (1 - s) * th_row / (P * (1 - P))) * D)
  cat(sprintf("%-20s %7.1f %8.1f %11.2e %14.2e %6s\n",
              tbl, median(g), max(g), max(plogis(g)), dalpha,
              if (f$alpha_identified) "ok" else "FLAT"))
}

# --- 2. does a nonzero start rescue it? --------------------------------------
# The check Doria asked for. If these were stuck fits, starting away from zero
# would find the real optimum. Instead alpha-hat comes back equal to whatever it
# was started at, with the log-likelihood unchanged -- which is what a flat
# likelihood looks like, and is the positive evidence for non-identification.

cat("\n== 2. refitting from nonzero alpha starts ==\n")
starts <- c(0, 0.1, -0.1, 0.3, -0.3)
for (tbl in NOT_IDENTIFIED[1:2]) {          # two tables is enough to show it
  Y <- prep(tbl)
  if (is.null(Y)) { cat(sprintf("%-20s  (prepared/ missing -- skipped)\n", tbl)); next }
  cat(sprintf("%s\n  %11s %16s %12s %12s\n", tbl, "start", "alpha_hat", "LR", "loglik"))
  ll <- numeric(length(starts))
  for (i in seq_along(starts)) {
    f <- fit_1pl_ag(Y, quad = QUAD, alpha_start = starts[i])
    ll[i] <- f$loglik_ag
    cat(sprintf("  %11.1f %16.10f %12.2e %12.1f\n",
                starts[i], f$alpha, f$lr_stat, f$loglik_ag))
  }
  cat(sprintf("  log-likelihood spread across starts: %.2e (relative %.1e)\n",
              diff(range(ll)), diff(range(ll)) / abs(mean(ll))))
}

# --- 3. the same estimator on data that does have a floor --------------------
# Doria's design: N = 3000, J = 45, b in [-1.5, 1.5], gamma = logit(0.2),
# generated from the 1PL-AG itself. The floor is there by construction, so s is
# bounded away from 0 and alpha is identified -- at high SD(theta) especially,
# exactly as she found. This is why the simulation validation on the page could
# never have caught the real-data failure: no 1PL-AG-generated dataset can
# reproduce it.

cat("\n== 3. simulated positive control: a floor exists, so alpha is identified ==\n")
gen <- function(N, J, alpha, sd_theta, gam = qlogis(0.2), seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  th <- rnorm(N, 0, sd_theta); b <- seq(-1.5, 1.5, length.out = J)
  r <- plogis(outer(th, b, "-")); s <- plogis(outer(alpha * th, rep(gam, J), "+"))
  matrix(rbinom(N * J, 1, r + (1 - r) * s), N, J)
}
cat(sprintf("%9s %11s %12s %10s %9s %8s\n",
            "SD(true)", "alpha(true)", "alpha_hat", "SD_hat", "p", "flag"))
i <- 0
for (sd_true in c(1.4, 2.4, 2.8)) for (a_true in c(0, 0.5)) {
  i <- i + 1
  f <- fit_1pl_ag(gen(3000, 45, a_true, sd_true, seed = 1000 + i), quad = QUAD)
  cat(sprintf("%9.1f %11.1f %12.4f %10.2f %9.3f %8s\n",
              sd_true, a_true, f$alpha, f$sd, f$lr_p,
              if (f$alpha_identified) "ok" else "FLAT"))
}
cat("\nExpected: no FLAG anywhere in block 3, alpha recovered near its true\n",
    "value at every SD, and a non-significant p only in the alpha = 0 rows.\n", sep = "")
