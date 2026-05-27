# poly_compare_compute.R
#
# Compare polytomous IRT models and binomial IRT models on a single dataset
# using 80/20 person-level train/test split and RMSE on E[Y|theta]/(K-1).
#
# Models:
#   Native polytomous:
#     1. PCM  — gpcm with a1 fixed to 1
#     2. GPCM — itemtype "gpcm"
#     3. GRM  — itemtype "graded"
#     4. Tutz — itemtype "sequential"
#   Binomial IRT (custom createItem; P(Y=y|theta) = Binom(K-1, link(theta))):
#     5. 1PL+logit   — logit link, a fixed to 1
#     6. 2PL+logit   — logit link, a estimated
#     7. 1PL+cloglog — complementary log-log link, a fixed to 1
#
# Usage: Rscript vignettes/poly_compare_compute.R   (from project root)

suppressPackageStartupMessages({
  library(irw)
  library(mirt)
  library(dplyr)
})

set.seed(20260527)

out_dir <- "vignettes/poly_compare_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── 1. Custom binomial item types ─────────────────────────────────────────────
# P(Y=y|theta) = Binom(K-1, link(a*theta - b)); ncat = K so size = ncat-1.
# outer() gives N x ncat probability matrix cleanly for any N.

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

# ── 2. Data ───────────────────────────────────────────────────────────────────
df    <- irw_fetch("florida_twins_dweck")
df    <- df[df$wave == 2, ]
K_min <- min(df$resp, na.rm = TRUE)
K_max <- max(df$resp, na.rm = TRUE)
K_range <- K_max - K_min                   # = K-1 for 1-based responses

df$resp0  <- df$resp - K_min               # shift to 0-based: 0 .. K_range
resp_wide <- irw_long2resp(df %>% select(-resp) %>% rename(resp = resp0))
item_cols <- setdiff(names(resp_wide), "id")
resp_mat  <- as.matrix(resp_wide[, item_cols])

ni <- ncol(resp_mat)
N  <- nrow(resp_mat)
cat("Dataset: florida_twins_dweck wave 2\n")
cat("N =", N, " items =", ni, " response range 0 ..", K_range, "\n\n")

# ── 3. Train / test split (80/20 person-level) ────────────────────────────────
train_idx <- sample(N, floor(0.8 * N))
test_idx  <- setdiff(seq_len(N), train_idx)
train_mat <- resp_mat[train_idx, ]
test_mat  <- resp_mat[test_idx,  ]

# ── 4. Helpers ────────────────────────────────────────────────────────────────
# Predict E[Y|theta]/(K_range) for test persons from any fitted mirt model.
predict_rescaled <- function(mod, test_data) {
  theta <- fscores(mod, response.pattern = test_data, method = "EAP")[, 1]
  Theta <- matrix(theta, ncol = 1)
  pred  <- sapply(seq_len(ni), function(j) {
    pr   <- probtrace(extract.item(mod, j), Theta)   # N_test x ncat
    cats <- 0:(ncol(pr) - 1)
    as.numeric(pr %*% cats) / K_range
  })
  pred  # N_test x ni matrix
}

compute_rmse <- function(pred, obs) sqrt(mean((pred - obs)^2, na.rm = TRUE))

obs_rescaled <- test_mat / K_range   # target for all models

# ── 5. Fit and evaluate ───────────────────────────────────────────────────────
fit_eval <- function(label, itemtypes, custom = NULL, pars = NULL) {
  cat("Fitting:", label, "...")
  args <- list(data = train_mat, model = 1, itemtype = itemtypes,
               verbose = FALSE, technical = list(NCYCLES = 2000))
  if (!is.null(custom)) args$customItems <- custom
  if (!is.null(pars))   args$pars        <- pars
  mod <- tryCatch(
    do.call(mirt, args),
    error = function(e) { cat(" FAILED:", e$message, "\n"); NULL }
  )
  if (is.null(mod)) return(NULL)
  pred <- tryCatch(
    predict_rescaled(mod, test_mat),
    error = function(e) { cat(" PREDICT FAILED:", e$message, "\n"); NULL }
  )
  if (is.null(pred)) return(NULL)
  rmse <- compute_rmse(pred, obs_rescaled)
  cat(" done.  RMSE =", round(rmse, 4), "\n")
  list(label = label, mod = mod, rmse = rmse)
}

# PCM: gpcm with a1 = 1
pcm_pars <- mirt(train_mat, 1, itemtype = rep("gpcm", ni),
                  verbose = FALSE, pars = "values")
pcm_pars$value[pcm_pars$name == "a1"] <- 1
pcm_pars$est[pcm_pars$name   == "a1"] <- FALSE

results <- list(
  pcm          = fit_eval("PCM",          rep("gpcm",        ni), pars = pcm_pars),
  gpcm         = fit_eval("GPCM",         rep("gpcm",        ni)),
  grm          = fit_eval("GRM",          rep("graded",      ni)),
  tutz         = fit_eval("Tutz",         rep("sequential",  ni)),
  binom_1pl    = fit_eval("1PL+logit",    rep("binom_1pl",   ni), custom = custom_items),
  binom_2pl    = fit_eval("2PL+logit",    rep("binom_2pl",   ni), custom = custom_items),
  binom_1pl_cll= fit_eval("1PL+cloglog",  rep("binom_1pl_cll",ni),custom = custom_items)
)

# ── 6. Summary ────────────────────────────────────────────────────────────────
rmse_df <- do.call(rbind, lapply(results, function(r) {
  if (is.null(r)) return(NULL)
  data.frame(model = r$label, rmse = round(r$rmse, 4))
}))
rmse_df <- rmse_df[order(rmse_df$rmse), ]
cat("\n=== RMSE (lower = better) ===\n")
print(rmse_df, row.names = FALSE)

saveRDS(list(results = results, rmse = rmse_df),
        file.path(out_dir, "florida_twins_dweck_w2.rds"))
cat("\nSaved to", out_dir, "\n")
