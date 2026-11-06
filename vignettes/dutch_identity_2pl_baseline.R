# dutch_identity_2pl_baseline.R
#
# A second reference distribution for Section 3's "is Γ rank-1?" test.
#
# The existing bootstrap simulates from the fitted restricted log-quadratic
# model, a world in which Γ is exactly rank-1 by construction. That model is
# Holland's large-J approximation to a unidimensional IRT model, not the model
# itself. At J = 5 a true unidimensional 2PL with normal θ need not produce an
# exactly rank-1 Γ: the posterior of θ given an extreme pattern such as 00000 is
# skewed, and the log-quadratic form is only the second-order term of the
# expansion. So falling outside the log-quadratic null shows a departure from
# the approximation, and might or might not show a departure from
# unidimensionality.
#
# This script asks the second question directly:
#   1. fit a unidimensional 2PL (mirt, normal θ) to the Section 1-3 table;
#   2. compute the 2PL's exact marginal pattern probabilities p(x) over all 2^J
#      cells by quadrature, and check them against mirt's own log-likelihood;
#   3. simulate N_SIM tables of pattern counts at the same N (multinomial on
#      p(x), fixed seed), refit the full SOE to each, and recompute the weighted
#      off-diagonal rank-1 share of Γ exactly as the vignette does;
#   4. also fit the SOE to the 2PL's EXPECTED counts, which gives the share a
#      2PL world produces with no sampling noise at all.
#
# Reads  vignettes/dutchdata/dutch_identity_results.rds (the cached table and
#        the observed Γ diagnostics); fetches nothing.
# Writes vignettes/dutchdata/dutch_identity_2pl_baseline.rds
#
# Usage (from the project root):
#   Rscript vignettes/dutch_identity_2pl_baseline.R

suppressMessages({
  library(mirt)
})
source("vignettes/dutch_identity_helpers.R")

SEED     <- 20261102
N_SIM    <- 500
N_QUAD   <- 61          # mirt's default for one dimension: 61 points on [-6, 6]

cache <- readRDS("vignettes/dutchdata/dutch_identity_results.rds")
s1_3  <- cache$s1_3
item_cols <- s1_3$item_cols
pats  <- s1_3$patterns[, c(item_cols, "count")]
J <- length(item_cols); N <- sum(pats$count)
stopifnot(nrow(pats) == 2^J, N == s1_3$N)
X <- as.matrix(pats[, item_cols])

# ---- the observed statistic, recomputed here as a guard ---------------------
soe_f <- as.formula(paste0("count ~ (", paste(item_cols, collapse = " + "), ")^2"))
w_share <- function(m, n_random = 4) {
  G  <- gamma_from_soe(m, item_cols)
  W  <- 1 / gamma_se_from_soe(m, item_cols)^2
  W[!is.finite(W)] <- 0
  rank1_offdiag_fit(G, W = W, n_random = n_random)$share
}
set.seed(SEED)
obs_share <- w_share(glm(soe_f, data = pats, family = poisson()), n_random = 10)
stopifnot(abs(obs_share - s1_3$gamma_rank1$rank1_weighted$share) < 1e-6)

# ---- 1. the 2PL ------------------------------------------------------------
resp <- X[rep(seq_len(nrow(X)), pats$count), , drop = FALSE]
colnames(resp) <- item_cols
mod <- mirt(as.data.frame(resp), 1, itemtype = "2PL", verbose = FALSE,
            quadpts = N_QUAD, TOL = 1e-6)
stopifnot(extract.mirt(mod, "converged"))
pars <- coef(mod, IRTpars = TRUE, simplify = TRUE)$items[, c("a", "b")]

# ---- 2. exact marginal p(x) by quadrature ----------------------------------
theta <- seq(-6, 6, length.out = N_QUAD)
wq    <- dnorm(theta); wq <- wq / sum(wq)
P1 <- sapply(seq_len(J), function(j)
  probtrace(extract.item(mod, j), matrix(theta))[, 2])          # N_QUAD x J
# p(x) = Σ_q w_q Π_j P_j(θ_q)^x_j (1 − P_j(θ_q))^(1 − x_j), on the log scale
logL <- X %*% t(log(P1)) + (1 - X) %*% t(log(1 - P1))           # 2^J x N_QUAD
p2pl <- drop(exp(logL) %*% wq)
stopifnot(abs(sum(p2pl) - 1) < 1e-10)
# the hand-rolled marginal must reproduce mirt's own log-likelihood
ll_hand <- sum(pats$count * log(p2pl))
ll_mirt <- extract.mirt(mod, "logLik")
stopifnot(abs(ll_hand - ll_mirt) < 1e-4 * abs(ll_mirt))

# how well does the 2PL itself fit, against the three loglinear models?
ll_mult <- s1_3$ll_multinomial                          # ind, rq, soe (multinomial)
ll_2pl  <- ll_hand

# ---- 4. the share in a 2PL world with no sampling noise ---------------------
exp_pats <- pats; exp_pats$count <- N * p2pl
m_exp <- suppressWarnings(glm(soe_f, data = exp_pats, family = poisson()))
set.seed(SEED)
pop_share <- w_share(m_exp, n_random = 10)

# ---- 3. simulate, refit, recompute -----------------------------------------
set.seed(SEED)
sim <- vapply(seq_len(N_SIM), function(i) {
  sp <- pats
  sp$count <- as.integer(rmultinom(1, size = N, prob = p2pl))
  m <- tryCatch(glm(soe_f, data = sp, family = poisson()), error = function(e) NULL,
                warning = function(w) NULL)
  if (is.null(m) || !m$converged) return(NA_real_)
  w_share(m)
}, numeric(1))
n_fail <- sum(is.na(sim))
stopifnot(n_fail <= 0.05 * N_SIM)
sim <- sim[!is.na(sim)]

res <- list(
  seed = SEED, n_sim = N_SIM, n_failed = n_fail, n_quad = N_QUAD,
  item_pars = pars,
  ll_2pl = ll_2pl, ll_multinomial = ll_mult,
  observed_share = obs_share,
  population_share = pop_share,
  sim_share = sim,
  sim_summary = list(median = median(sim),
                     lo = unname(quantile(sim, 0.025)),
                     hi = unname(quantile(sim, 0.975)),
                     min = min(sim)),
  pctl_observed = mean(sim < obs_share),     # share of 2PL replicates below observed
  n_below_observed = sum(sim < obs_share),
  logquad_null = s1_3$gamma_rank1$boot_summary,
  date_run = Sys.Date(),
  mirt_version = as.character(packageVersion("mirt"))
)
saveRDS(res, "vignettes/dutchdata/dutch_identity_2pl_baseline.rds")

cat(sprintf("observed weighted share      %.4f\n", obs_share))
cat(sprintf("2PL, no sampling noise       %.4f\n", pop_share))
cat(sprintf("2PL replicates (n = %d)     median %.4f, 95%% [%.4f, %.4f], min %.4f\n",
            length(sim), res$sim_summary$median, res$sim_summary$lo,
            res$sim_summary$hi, res$sim_summary$min))
cat(sprintf("replicates below observed    %d of %d\n", res$n_below_observed, length(sim)))
cat(sprintf("log-quadratic null           median %.4f, 95%% [%.4f, %.4f]\n",
            res$logquad_null$share_w_median, res$logquad_null$share_w_lo,
            res$logquad_null$share_w_hi))
cat(sprintf("multinomial ll: ind %.1f  rq %.1f  2PL %.1f  soe %.1f\n",
            ll_mult[["ind"]], ll_mult[["rq"]], ll_2pl, ll_mult[["soe"]]))
