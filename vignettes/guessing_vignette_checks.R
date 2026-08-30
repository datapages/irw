# Some checks against the estimators in guessing_helpers.R, on simulated data
# where the truth is known. Drop this next to guessing_helpers.R and run it.
# ~2 min. R 4.5.1, mirt 1.45.1.

suppressMessages(library(mirt))
source("guessing_helpers.R")
QUAD <- build_quadrature(41)
set.seed(20260722)

# lifted from guessing_compute.R so the holdout and the mirt predictions match
# what the page actually does
mask_holdout <- function(resp, frac = 0.2) {
  rt <- resp; mat <- as.matrix(resp)
  no <- rowSums(!is.na(mat)); ml <- vector("list", nrow(mat))
  for (i in seq_len(nrow(mat))) {
    k <- no[i]; if (k < 2) next
    oc <- which(!is.na(mat[i, ])); nm <- min(floor(frac * k), k - 1)
    if (nm < 1) next
    ml[[i]] <- cbind(row = i, col = sample(oc, nm))
  }
  mi <- do.call(rbind, ml); tv <- mat[mi]
  for (k in seq_len(nrow(mi))) rt[mi[k, 1], mi[k, 2]] <- NA
  list(train = rt, mask_idx = mi, true_vals = tv)
}

heldout_preds_mirt <- function(fit, mi) {
  th <- fscores(fit, method = "EAP")[, 1]; p <- numeric(nrow(mi))
  for (j in unique(mi[, 2])) {
    r <- which(mi[, 2] == j)
    p[r] <- probtrace(extract.item(fit, j), matrix(th[mi[r, 1]], ncol = 1))[, "P.1"]
  }
  p
}

ctr <- function(x) x - mean(x)

# b is only identified up to location, so centre both before comparing.
# slope > 1 means b_hat is compressed relative to the truth.
recov <- function(bhat, btrue) c(
  rmse  = sqrt(mean((ctr(bhat) - ctr(btrue))^2)),
  slope = unname(coef(lm(ctr(btrue) ~ 0 + ctr(bhat)))[1]))

sim_table <- function(N, J, b_true, pi_eng = 1, g = 0.2, sd_theta = 1) {
  theta <- rnorm(N, 0, sd_theta)
  eng <- rbinom(N, 1, pi_eng)
  P <- plogis(outer(theta, b_true, "-")); P[eng == 0, ] <- g
  Y <- matrix(rbinom(N * J, 1, P), N, J)
  colnames(Y) <- paste0("i", seq_len(J))
  as.data.frame(Y)
}


# --- 1. item recovery vs held-out IMV, on the same runs --------------------
# The callout on the page says Method A shouldn't be scored on prediction.
# This puts both numbers side by side with b_true known.

cat("\n== recovery vs IMV ==\n")
N <- 2000; J <- 30; g <- 0.2
b_true <- seq(-2, 2, length.out = J)
rows <- list()
for (PI in c(1.00, 0.90, 0.80, 0.70)) {
  set.seed(20260722)
  resp <- sim_table(N, J, b_true, pi_eng = PI, g = g)
  ho <- mask_holdout(resp, 0.2); y <- ho$true_vals; Ytr <- as.matrix(ho$train)

  fR <- mirt(ho$train, 1, itemtype = "Rasch", verbose = FALSE,
             technical = list(NCYCLES = 2000))
  bR <- -coef(fR, simplify = TRUE)$items[, "d"]
  pR <- heldout_preds_mirt(fR, ho$mask_idx)

  pur <- purify_rasch(Ytr, g_fit = g, quad = QUAD)
  pP  <- predict_purified_rasch(pur, Ytr, quad = QUAD)[ho$mask_idx]
  mx  <- fit_mixture(Ytr, g_fit = g, quad = QUAD)
  pM  <- predict_mixture(mx, Ytr)[ho$mask_idx]

  rr <- recov(bR, b_true); rp <- recov(pur$b, b_true); rm_ <- recov(mx$b, b_true)
  rows[[length(rows) + 1]] <- data.frame(
    pi_true = PI, pi_hat = round(mx$pi, 3), flagged = round(pur$frac_flagged, 3),
    rmse_Rasch = round(rr["rmse"], 4), rmse_Pur = round(rp["rmse"], 4),
    rmse_Mix = round(rm_["rmse"], 4),
    slope_Rasch = round(rr["slope"], 3), slope_Pur = round(rp["slope"], 3),
    IMV_Pur = round(compute_imv(pR, pP, y), 5),
    IMV_Mix = round(compute_imv(pR, pM, y), 5), row.names = NULL)
}
print(do.call(rbind, rows), row.names = FALSE)


# --- 2. ability dispersion, with no guessing anywhere ----------------------
# mirt's Rasch estimates the latent variance; fit_mixture / fit_1pl_ag /
# purify_rasch all fix theta ~ N(0,1) through build_quadrature(). There is no
# guessing at all in any row below, so anything away from zero is that.

cat("\n== no guessing; only sd(theta) varies ==\n")
N <- 3000; J <- 45
b_true <- seq(-2, 2, length.out = J)
cat(" sd    var_hat   pi_hat    IMV(R,Mix)   IMV(R,AG)   alpha_hat\n")
for (SD in c(1.0, 1.2, 1.4, 1.6)) {
  set.seed(20260722)
  resp <- sim_table(N, J, b_true, pi_eng = 1, sd_theta = SD)
  ho <- mask_holdout(resp, 0.2); y <- ho$true_vals; Ytr <- as.matrix(ho$train)
  fR <- mirt(ho$train, 1, itemtype = "Rasch", verbose = FALSE,
             technical = list(NCYCLES = 2000))
  pR <- heldout_preds_mirt(fR, ho$mask_idx)
  mx <- fit_mixture(Ytr, g_fit = 0.2, quad = QUAD)
  ag <- fit_1pl_ag(Ytr, quad = QUAD)
  cat(sprintf(" %.1f    %.3f     %.4f    %+.5f     %+.5f     %+.3f\n",
              SD, coef(fR, simplify = TRUE)$cov[1, 1], mx$pi,
              compute_imv(pR, predict_mixture(mx, Ytr)[ho$mask_idx], y),
              compute_imv(pR, predict_1pl_ag(ag, Ytr)[ho$mask_idx], y), ag$alpha))
}
cat(" (that alpha_hat column is on data with no guessing in it at all)\n")


# --- 3. what a negative alpha does to the 1PL-AG curve ---------------------
#   P     = r + (1-r)s,  r = expit(theta-beta), s = expit(alpha*theta+gamma)
#   dP/dt = (1-r)(1-s)(r + alpha*s)
# so the slope takes the sign of (r + alpha*s). r -> 0 and s -> 1 as
# theta -> -Inf, so any alpha < 0 turns the curve over somewhere on the low
# end and sends it to 1, whatever gamma is.

cat("\n== 1PL-AG curve by sign of alpha (beta=1, gamma=logit(0.2)) ==\n")
beta <- 1; gam <- qlogis(0.2)
th <- seq(-3, 3, by = 1)
icc <- function(a) { r <- plogis(th - beta); r + (1 - r) * plogis(a * th + gam) }
print(round(cbind(theta = th, `a=-0.9` = icc(-0.9), `a=-0.2` = icc(-0.2),
                  `a=0` = icc(0), `a=+0.2` = icc(0.2)), 4))


# --- 4. two things it isn't ------------------------------------------------
# I thought each of these might be behind the negative Purified column.
# Neither is, so leaving them here rather than have someone chase them again.

cat("\n== ruled out ==\n")
set.seed(20260722)
N <- 3000; J <- 45; b_true <- seq(-2, 2, length.out = J)
resp <- sim_table(N, J, b_true, pi_eng = 0.80, g = 0.2)
ho <- mask_holdout(resp, 0.2); y <- ho$true_vals; Ytr <- as.matrix(ho$train)
fR <- mirt(ho$train, 1, itemtype = "Rasch", verbose = FALSE,
           technical = list(NCYCLES = 2000))
bR <- -coef(fR, simplify = TRUE)$items[, "d"]
p_plugin <- heldout_preds_mirt(fR, ho$mask_idx)

# (a) the mirt models use an EAP plug-in, the custom ones integrate over the
#     posterior. Is that asymmetry doing anything?
rl <- .rasch_node_loglik(Ytr, bR, QUAD$nodes)
p_int <- (.node_posterior(rl$loglik, QUAD$weights)$post %*% t(rl$P))[ho$mask_idx]
cat(sprintf("(a) plug-in vs integrated, same fit: IMV %+.5f, cor %.5f\n",
            compute_imv(p_plugin, p_int, y), cor(p_plugin, p_int)))

# (b) purify_rasch refits on a subsample, so its b end up on a shifted scale.
#     Re-estimating the prior on the full sample would undo that if it mattered.
pred_with_prior <- function(b, Y, mu, sd) {
  nodes <- seq(-6, 6, length.out = 41) * sd + mu
  w <- dnorm(nodes, mu, sd); w <- w / sum(w)
  rl <- .rasch_node_loglik(as.matrix(Y), b, nodes)
  .node_posterior(rl$loglik, w)$post %*% t(rl$P)
}
pur <- purify_rasch(Ytr, g_fit = 0.2, quad = QUAD)
nll <- function(par) {
  nodes <- seq(-6, 6, length.out = 41) * exp(par[2]) + par[1]
  w <- dnorm(nodes, par[1], exp(par[2])); w <- w / sum(w)
  -sum(.node_posterior(.rasch_node_loglik(Ytr, pur$b, nodes)$loglik, w)$marg_ll)
}
o <- optim(c(0, 0), nll, method = "Nelder-Mead")
cat(sprintf("(b) mean(b_pur) - mean(b_base) = %+.4f; refit prior mu=%+.3f sd=%.3f\n",
            mean(pur$b) - mean(pur$b_baseline), o$par[1], exp(o$par[2])))
cat(sprintf("    IMV vs Rasch: as-is %+.5f, relinked %+.5f\n",
            compute_imv(p_plugin, predict_purified_rasch(pur, Ytr, quad = QUAD)[ho$mask_idx], y),
            compute_imv(p_plugin, pred_with_prior(pur$b, Ytr, o$par[1], exp(o$par[2]))[ho$mask_idx], y)))
