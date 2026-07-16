# poly_compare_sim.R
#
# Simulation validation for custom binomial IRT item types.
# For each model (binom_1pl, binom_2pl, binom_1pl_cll):
#   1. Simulate data from known parameters
#   2. Fit the model back to the data
#   3. Check parameter recovery (true vs estimated b, and a for 2PL)
#   4. Confirm RMSE is lower for the generating model than for mis-specified ones
#
# Usage: Rscript vignettes/poly_compare_sim.R   (from project root)

suppressPackageStartupMessages({
  library(mirt)
  library(dplyr)
})

set.seed(42)

N    <- 1000    # persons
J    <- 10      # items
K    <- 5       # response categories (0 .. K-1, so K_range = K-1 = 4)
size <- K - 1L  # binomial trials per response

# ── 1. Custom item types (same as poly_compare_compute.R) ────────────────────
binom_P <- function(prob, ncat) {
  size <- ncat - 1L
  prob <- pmin(pmax(prob, 1e-10), 1 - 1e-10)
  outer(prob, 0:size, function(p, k) dbinom(k, size, p))
}

binom_1pl <- createItem("binom_1pl",
  par = c(a = 1, b = 0), est = c(FALSE, TRUE),
  P = function(par, Theta, ncat)
    binom_P(plogis(par[1] * Theta[, 1] - par[2]), ncat)
)

binom_2pl <- createItem("binom_2pl",
  par = c(a = 1, b = 0), est = c(TRUE, TRUE),
  P = function(par, Theta, ncat)
    binom_P(plogis(par[1] * Theta[, 1] - par[2]), ncat)
)

binom_1pl_cll <- createItem("binom_1pl_cll",
  par = c(a = 1, b = 0), est = c(FALSE, TRUE),
  P = function(par, Theta, ncat)
    binom_P(1 - exp(-exp(par[1] * Theta[, 1] - par[2])), ncat)
)

custom_items <- list(
  binom_1pl     = binom_1pl,
  binom_2pl     = binom_2pl,
  binom_1pl_cll = binom_1pl_cll
)

# ── 2. Simulation helpers ─────────────────────────────────────────────────────
sim_binom <- function(theta, b_true, a_true = 1, link = c("logit", "cll")) {
  link  <- match.arg(link)
  a_vec <- rep(a_true, length.out = length(b_true))
  eta   <- outer(theta, a_vec, `*`) - matrix(b_true, nrow = length(theta),
                                              ncol = length(b_true), byrow = TRUE)
  prob <- if (link == "logit") plogis(eta) else 1 - exp(-exp(eta))
  apply(prob, c(1, 2), function(p) rbinom(1, size, p))
}

# True item parameters
b_true <- seq(-1.5, 1.5, length.out = J)   # item difficulties
a_true <- runif(J, 0.5, 2.0)               # discriminations (2PL only)
theta  <- rnorm(N)                          # person abilities

# ── 3. Simulate and recover ───────────────────────────────────────────────────
recover <- function(sim_label, dat, fit_type, custom = NULL, a_fixed = NULL) {
  itypes <- rep(fit_type, J)
  args   <- list(data = dat, model = 1, itemtype = itypes,
                 verbose = FALSE, technical = list(NCYCLES = 2000))
  if (!is.null(custom)) args$customItems <- custom

  # for binom_2pl, set starting a if provided
  if (!is.null(a_fixed)) {
    pars <- do.call(mirt, c(args, list(pars = "values")))
    pars$value[pars$name == "a"] <- a_fixed
    args$pars <- pars
  }

  mod <- tryCatch(do.call(mirt, args),
                  error = function(e) { message("  fit failed: ", e$message); NULL })
  if (is.null(mod)) return(NULL)

  # extract estimated b (and a for 2pl)
  coefs <- do.call(rbind, lapply(seq_len(J), function(j) {
    p <- coef(mod)[[j]]
    data.frame(item = j, a_est = p[1, "a"], b_est = p[1, "b"])
  }))
  coefs
}

item_names <- paste0("item", seq_len(J))

cat("=== Simulation 1: binom_1pl (logit, a=1) ===\n")
dat_1pl <- sim_binom(theta, b_true, a_true = 1, link = "logit")
colnames(dat_1pl) <- item_names
res_1pl <- recover("binom_1pl", dat_1pl, "binom_1pl", custom = custom_items)
if (!is.null(res_1pl)) {
  cat("b recovery (true vs est):\n")
  print(round(data.frame(b_true, b_est = res_1pl$b_est), 3))
  cat("Correlation:", round(cor(b_true, res_1pl$b_est), 3), "\n\n")
}

cat("=== Simulation 2: binom_2pl (logit, free a) ===\n")
dat_2pl <- sim_binom(theta, b_true, a_true = a_true, link = "logit")
colnames(dat_2pl) <- item_names
res_2pl <- recover("binom_2pl", dat_2pl, "binom_2pl", custom = custom_items)
if (!is.null(res_2pl)) {
  cat("b recovery:\n")
  print(round(data.frame(b_true, b_est = res_2pl$b_est), 3))
  cat("b correlation:", round(cor(b_true, res_2pl$b_est), 3), "\n")
  cat("a recovery:\n")
  print(round(data.frame(a_true, a_est = res_2pl$a_est), 3))
  cat("a correlation:", round(cor(a_true, res_2pl$a_est), 3), "\n\n")
}

cat("=== Simulation 3: binom_1pl_cll (cloglog, a=1) ===\n")
dat_cll <- sim_binom(theta, b_true, a_true = 1, link = "cll")
colnames(dat_cll) <- item_names
res_cll <- recover("binom_1pl_cll", dat_cll, "binom_1pl_cll", custom = custom_items)
if (!is.null(res_cll)) {
  cat("b recovery:\n")
  print(round(data.frame(b_true, b_est = res_cll$b_est), 3))
  cat("Correlation:", round(cor(b_true, res_cll$b_est), 3), "\n\n")
}

# ── 4. Model mis-specification check ─────────────────────────────────────────
# Data from binom_1pl_cll; fit both logit and cll — cll should win on RMSE
cat("=== Model mis-specification: data from CLL, fit logit vs cll ===\n")

train_idx <- sample(N, 800)
test_idx  <- setdiff(seq_len(N), train_idx)

predict_rmse <- function(mod, test_dat, k_range) {
  theta_hat <- fscores(mod, response.pattern = test_dat, method = "EAP")[, 1]
  Theta_hat <- matrix(theta_hat, ncol = 1)
  pred <- sapply(seq_len(J), function(j) {
    pr   <- probtrace(extract.item(mod, j), Theta_hat)
    cats <- 0:(ncol(pr) - 1)
    as.numeric(pr %*% cats) / k_range
  })
  obs  <- test_dat / k_range
  sqrt(mean((pred - obs)^2, na.rm = TRUE))
}

fit_logit <- tryCatch(
  mirt(dat_cll[train_idx, ], 1, itemtype = rep("binom_1pl", J),
       customItems = custom_items, verbose = FALSE,
       technical = list(NCYCLES = 2000)),
  error = function(e) NULL
)
fit_cll <- tryCatch(
  mirt(dat_cll[train_idx, ], 1, itemtype = rep("binom_1pl_cll", J),
       customItems = custom_items, verbose = FALSE,
       technical = list(NCYCLES = 2000)),
  error = function(e) NULL
)

if (!is.null(fit_logit) && !is.null(fit_cll)) {
  rmse_logit <- predict_rmse(fit_logit, dat_cll[test_idx, ], size)
  rmse_cll   <- predict_rmse(fit_cll,   dat_cll[test_idx, ], size)
  cat("RMSE logit (mis-specified):", round(rmse_logit, 4), "\n")
  cat("RMSE cll   (correct):      ", round(rmse_cll,   4), "\n")
  cat("CLL wins?", rmse_cll < rmse_logit, "\n")
}

cat("\nDone.\n")
