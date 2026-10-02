# intransitivity_helpers.R
#
# Functions for intransitivity_compute.R: are there more cycles (A beats B, B beats
# C, C beats A) in pairwise data than a transitive model produces by chance?
#
# A "unit" is a data.frame of games: a, b (agents), y (a's score: 1, 0.5 for a draw,
# 0), home (1 if a has the home or first-move advantage, -1 if b has it, 0 if
# neither) and, optionally, off (a fixed logit offset, e.g. a pre-game rating
# difference, so that strengths are estimated beyond it).
#
# Two kinds of test, both against a parametric bootstrap that re-simulates the same
# schedule (same pairs, same home sides) from a fitted transitive model:
#   run_unit()  triad statistics on pair-level win proportions
#   lr_test()   likelihood ratio, Bradley-Terry vs Bradley-Terry plus a rank-2
#               skew-symmetric term that can produce cycles among any agents
# In both, the strengths used to simulate are rescaled by sqrt(EB shrink): the
# fitted strengths overstate the true spread, badly so when there are few games
# per pair, and a null simulated from them is too predictable.

suppressPackageStartupMessages(library(MASS))

# ---- transitive model and its empirical-Bayes shrink -------------------------
# Bradley-Terry (glm), or an ordinal Bradley-Terry (polr) when there are draws.
fit_model <- function(d, agents) {
  off <- if (is.null(d$off)) rep(0, nrow(d)) else d$off
  X <- matrix(0, nrow(d), length(agents))
  X[cbind(seq_len(nrow(d)), match(d$a, agents))] <- 1
  X[cbind(seq_len(nrow(d)), match(d$b, agents))] <- -1
  X <- X[, -1, drop = FALSE]
  has_home <- any(d$home != 0)
  draws <- any(d$y == 0.5)
  # with draws, a constant home column is absorbed by polr's cutpoints; h is then
  # recovered from their midpoint below
  home_in_cut <- draws && has_home && length(unique(d$home)) == 1
  if (has_home && !home_in_cut) X <- cbind(home = d$home, X)
  if (!draws) {
    m <- glm(d$y ~ X - 1 + offset(off), family = binomial)
    cf <- coef(m); V <- vcov(m); kappa <- 0
  } else {
    m <- polr(factor(d$y, levels = c(0, 0.5, 1)) ~ X + offset(off), Hess = TRUE)
    cf <- coef(m); V <- solve(m$Hessian)[seq_along(cf), seq_along(cf)]
    kappa <- unname(diff(m$zeta) / 2)    # symmetric draw band around eta
  }
  hcol <- has_home && !home_in_cut
  h <- if (hcol) unname(cf[1]) else if (home_in_cut) -unname(sum(m$zeta)) / 2 * unique(d$home) else 0
  th <- c(0, if (hcol) cf[-1] else cf)
  th <- th - mean(th)
  # EB shrink: true variance = observed variance - mean sampling variance of the
  # CENTRED strengths (reference-coded SEs also carry the reference agent's
  # uncertainty, which would over-shrink)
  k <- length(agents); Vt <- matrix(0, k, k)
  idx <- if (hcol) -1 else seq_len(ncol(V))
  Vt[-1, -1] <- V[idx, idx]
  M <- diag(k) - 1 / k
  v_obs <- var(th); v_err <- mean(diag(M %*% Vt %*% M)) * k / (k - 1)
  # rescale by sqrt(shrink) rather than taking posterior means, which would be
  # under-dispersed: this keeps the simulated spread at the estimated true variance
  shrink <- max(0, (v_obs - v_err) / v_obs)
  names(th) <- agents
  list(theta = th, theta_sim = th * sqrt(shrink), h = h, kappa = kappa, shrink = shrink)
}

# The same shrink from a penalized Bradley-Terry fit (fit_ll below): sampling
# variance from the inverse Fisher information of the centred strengths. Unlike the
# glm/polr fit, it stays finite under (quasi-)separation, where some agents win or
# lose nearly every game and the glm standard errors blow up.
shrink_pen <- function(th, h, ia, ib, home, off, lam = 1e-3) {
  n <- length(th); p <- plogis(off + h * home + th[ia] - th[ib]); w <- p * (1 - p)
  I <- matrix(0, n, n)
  dg <- rowsum(c(w, w), c(ia, ib)); i <- as.integer(rownames(dg)); I[cbind(i, i)] <- dg[, 1]
  od <- rowsum(w, paste(pmin(ia, ib), pmax(ia, ib)))
  ij <- do.call(rbind, lapply(strsplit(rownames(od), " "), as.integer))
  I[ij] <- I[ij] - od[, 1]; I[ij[, 2:1, drop = FALSE]] <- I[ij[, 2:1, drop = FALSE]] - od[, 1]
  V <- solve(I + 2 * lam * diag(n)); M <- diag(n) - 1 / n
  v_err <- mean(diag(M %*% V %*% M)) * n / (n - 1); v_obs <- var(th)
  max(0, (v_obs - v_err) / v_obs)
}

simulate_y <- function(d, f) {
  eta <- f$h * d$home + f$theta_sim[d$a] - f$theta_sim[d$b]
  if (!is.null(d$off)) eta <- eta + d$off
  if (f$kappa == 0) return(as.numeric(runif(nrow(d)) < plogis(eta)))
  u <- runif(nrow(d))
  p_win <- plogis(eta - f$kappa); p_notloss <- plogis(eta + f$kappa)
  ifelse(u < p_win, 1, ifelse(u < p_notloss, 0.5, 0))
}

# ---- triad statistics ---------------------------------------------------------
pair_mats <- function(d, y, agents) {
  n <- length(agents); ia <- match(d$a, agents); ib <- match(d$b, agents)
  W <- as.matrix(xtabs(y ~ factor(ia, 1:n) + factor(ib, 1:n)))          # i's score against j
  W <- W + t(as.matrix(xtabs((1 - y) ~ factor(ia, 1:n) + factor(ib, 1:n))))
  N <- as.matrix(xtabs(~ factor(ia, 1:n) + factor(ib, 1:n))); N <- N + t(N)
  dimnames(W) <- dimnames(N) <- NULL
  list(W = W, N = N)
}

# wst  : share of fully decided triads whose majority directions form a cycle
# sst  : among transitive triads i>j>k, share with p_ik < max(p_ij, p_jk)
# curl : HodgeRank cyclic share, the weighted residual SS / total SS after the
#        least-squares ranking fit to smoothed pair log-odds
stats_from <- function(pm, tri) {
  W <- pm$W; N <- pm$N
  P <- ifelse(N > 0, W / N, NA)
  A <- (P > 0.5) * 1; A[is.na(A)] <- 0
  U <- (!is.na(P) & P != 0.5) * 1
  dec <- sum(diag(U %*% U %*% U)) / 6
  cyc <- sum(diag(A %*% A %*% A)) / 3
  wst <- if (dec > 0) cyc / dec else NA
  # exactly one ordering (x, y, z) of a transitive triad has x>y, y>z and x>z; it
  # violates SST if P(x>z) < max(P(x>y), P(y>z))
  perms <- rbind(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))
  n_tr <- 0; n_viol <- 0
  for (r in seq_len(nrow(perms))) {
    x <- tri[, perms[r, 1]]; y <- tri[, perms[r, 2]]; z <- tri[, perms[r, 3]]
    pxy <- P[cbind(x, y)]; pyz <- P[cbind(y, z)]; pxz <- P[cbind(x, z)]
    ok <- !is.na(pxy) & !is.na(pyz) & !is.na(pxz) & pxy > .5 & pyz > .5 & pxz > .5
    n_tr <- n_tr + sum(ok); n_viol <- n_viol + sum(pxz[ok] < pmax(pxy[ok], pyz[ok]))
  }
  sst <- if (n_tr > 0) n_viol / n_tr else NA
  e <- which(upper.tri(N) & N > 0, arr.ind = TRUE)
  n_e <- N[e]; yv <- qlogis((W[e] + .5) / (n_e + 1))
  G <- matrix(0, nrow(e), nrow(N)); G[cbind(1:nrow(e), e[, 1])] <- 1; G[cbind(1:nrow(e), e[, 2])] <- -1
  G <- G[, -1, drop = FALSE]
  fitl <- lm.wfit(G, yv, n_e)
  curl <- sum(n_e * fitl$residuals^2) / sum(n_e * yv^2)
  c(wst = wst, sst = sst, curl = curl, triads = dec, cycles = cyc)
}

run_unit <- function(name, d, B = 200) {
  agents <- sort(unique(c(d$a, d$b)))
  f <- fit_model(d, agents)
  if (f$shrink < .05) {
    # as in lr_test: under separation the glm/polr SEs blow up and the shrink falls
    # to 0; take strengths and shrink from the penalized fit instead
    ia <- match(d$a, agents); ib <- match(d$b, agents)
    f1 <- fit_ll(ia, ib, d$y, d$home, length(agents), FALSE)
    th <- f1$par[-1] - mean(f1$par[-1])
    s <- tryCatch(shrink_pen(th, f1$par[1], ia, ib, d$home, 0), error = function(e) 0)
    f$theta_sim <- setNames(th * sqrt(s), agents); f$h <- f1$par[1]; f$shrink <- s
  }
  tri <- t(combn(length(agents), 3))
  obs <- stats_from(pair_mats(d, d$y, agents), tri)
  sims <- t(replicate(B, stats_from(pair_mats(d, simulate_y(d, f), agents), tri)))
  out <- data.frame(unit = name, shrink = round(f$shrink, 2), triads = obs[["triads"]], cycles = obs[["cycles"]])
  for (s in c("wst", "sst", "curl")) {
    out[[s]] <- obs[[s]]
    out[[paste0(s, "_null")]] <- mean(sims[, s], na.rm = TRUE)
    out[[paste0(s, "_p")]] <- (1 + sum(sims[, s] >= obs[[s]], na.rm = TRUE)) / (1 + sum(!is.na(sims[, s])))
  }
  out
}

# ---- rank-2 likelihood-ratio test ----------------------------------------------
#   logit P(a beats b) = off + h * home + theta_a - theta_b + (u_a v_b - u_b v_a)
# Each agent is a point (u, v); a beats b more often than strength predicts when b
# sits counter-clockwise of a, so the term can cycle among any agents. Draws enter
# as y = 1/2 (a quasi-binomial likelihood) in both models alike, with a small ridge
# penalty (lam) that keeps the fits finite.
ll_grad <- function(par, ia, ib, y, home, n, lam, rank2, off = 0) {
  h <- par[1]; th <- par[2:(n + 1)]
  eta <- off + h * home + th[ia] - th[ib]
  if (rank2) { u <- par[(n + 2):(2 * n + 1)]; v <- par[(2 * n + 2):(3 * n + 1)]
               eta <- eta + u[ia] * v[ib] - u[ib] * v[ia] }
  p <- plogis(eta); r <- y - p
  ll <- sum(y * log(p) + (1 - y) * log1p(-p)) - lam * sum(par[-1]^2)
  g_th <- rowsum(c(r, -r), c(ia, ib), reorder = TRUE)
  gth <- numeric(n); gth[as.integer(rownames(g_th))] <- g_th[, 1]
  g <- c(sum(r * home), gth)
  if (rank2) {
    gu <- numeric(n); gv <- numeric(n)
    a1 <- rowsum(c(r * v[ib], -r * v[ia]), c(ia, ib)); gu[as.integer(rownames(a1))] <- a1[, 1]
    a2 <- rowsum(c(r * u[ia], -r * u[ib]), c(ib, ia)); gv[as.integer(rownames(a2))] <- a2[, 1]
    g <- c(g, gu, gv)
  }
  g[-1] <- g[-1] - 2 * lam * par[-1]
  list(ll = ll, g = g)
}

fit_ll <- function(ia, ib, y, home, n, rank2, lam = 1e-3, starts = 3, off = 0) {
  best <- NULL
  for (s in seq_len(if (rank2) starts else 1)) {
    p0 <- c(0, rep(0, n), if (rank2) rnorm(2 * n, 0, 0.1))
    o <- optim(p0, function(p) -ll_grad(p, ia, ib, y, home, n, lam, rank2, off)$ll,
               function(p) -ll_grad(p, ia, ib, y, home, n, lam, rank2, off)$g,
               method = "BFGS", control = list(maxit = 2000, reltol = 1e-10))
    if (is.null(best) || o$value < best$value) best <- o
  }
  list(ll = -best$value, par = best$par)
}

# LR = 2 (ll_rank2 - ll_BT), with a parametric-bootstrap null from the fitted BT
# model, its strengths rescaled by sqrt(EB shrink). The shrink comes from
# fit_model; where that fit fails or separates (shrink < .05), from shrink_pen.
# shrink = FALSE simulates from the fitted strengths as they are; it is kept only to
# show, on simulated data, why that null is too lenient. Bootstrap replicates run on
# getOption("lr_cores", 1) cores.
lr_test <- function(d, B = 100, seed = 1, off = 0, shrink = TRUE) {
  set.seed(seed)
  agents <- sort(unique(c(d$a, d$b))); n <- length(agents)
  ia <- match(d$a, agents); ib <- match(d$b, agents)
  f1 <- fit_ll(ia, ib, d$y, d$home, n, FALSE, off = off); f2 <- fit_ll(ia, ib, d$y, d$home, n, TRUE, off = off)
  lr <- 2 * (f2$ll - f1$ll)
  h <- f1$par[1]; th <- f1$par[2:(n + 1)]; pdraw <- mean(d$y == 0.5)
  s <- 1
  if (shrink) {
    s <- tryCatch(fit_model(data.frame(d[, c("a", "b", "y", "home")], off = off), agents)$shrink, error = function(e) NA)
    if (is.na(s) || s < .05) s <- tryCatch(shrink_pen(th - mean(th), h, ia, ib, d$home, off), error = function(e) 1)
  }
  th <- (th - mean(th)) * sqrt(s)
  eta <- off + h * d$home + th[ia] - th[ib]
  # a draw is drawn with the observed rate
  null <- unlist(parallel::mclapply(seq_len(B), function(r) { set.seed(seed * 1000 + r)
    y <- as.numeric(runif(nrow(d)) < plogis(eta))
    if (pdraw > 0) y[runif(nrow(d)) < pdraw] <- 0.5
    2 * (fit_ll(ia, ib, y, d$home, n, TRUE, off = off)$ll - fit_ll(ia, ib, y, d$home, n, FALSE, off = off)$ll)
  }, mc.cores = getOption("lr_cores", 1)))
  # size of the cyclic part: SD over pairs of (u_a v_b - u_b v_a), in logits
  u <- f2$par[(n + 2):(2 * n + 1)]; v <- f2$par[(2 * n + 2):(3 * n + 1)]
  cyc <- outer(u, v) - outer(v, u)
  data.frame(lr = lr, lr_null = mean(null), lr_p = (1 + sum(null >= lr)) / (B + 1),
             cyc_sd = sd(cyc[upper.tri(cyc)]), theta_sd = sd(f2$par[2:(n + 1)]), null_shrink = s)
}
