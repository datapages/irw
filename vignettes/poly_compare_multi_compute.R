# poly_compare_multi_compute.R
#
# Compare polytomous IRT models vs binomial IRT across N_DATASETS IRW datasets.
# Iterates metadata in order, skips dichotomous/inconsistent-K/too-small datasets,
# stops when N_DATASETS successful fits are collected.
#
# Usage: Rscript vignettes/poly_compare_multi_compute.R

suppressPackageStartupMessages({
  library(irw)
  library(mirt)
  library(dplyr)
})

set.seed(20260527)

N_DATASETS <- 10
MIN_N      <- 200
MIN_ITEMS  <- 3
out_dir    <- "vignettes/poly_compare_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── 1. Custom binomial item types ─────────────────────────────────────────────
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

# ── 2. Per-dataset pipeline ───────────────────────────────────────────────────
process_one <- function(tab) {
  cache <- file.path(out_dir, paste0("multi_", tab, ".rds"))
  if (file.exists(cache)) {
    message("  cached")
    return(readRDS(cache))
  }

  df <- NULL
  for (attempt in 1:3) {
    df <- tryCatch(irw_fetch(tab), error = function(e) {
      message("  fetch attempt ", attempt, " failed: ", e$message); NULL
    })
    if (!is.null(df)) break
    Sys.sleep(5 * attempt)
  }
  if (is.null(df)) return(NULL)

  K_range <- max(df$resp, na.rm = TRUE) - min(df$resp, na.rm = TRUE)
  if (K_range < 2)  { message("  skip: dichotomous");          return(NULL) }
  if (K_range > 10) { message("  skip: K_range=", K_range, " > 10"); return(NULL) }

  K_min    <- min(df$resp, na.rm = TRUE)
  df$resp0 <- df$resp - K_min

  resp_wide <- tryCatch(
    irw_long2resp(df %>% select(-resp) %>% rename(resp = resp0)),
    error = function(e) { message("  long2resp failed: ", e$message); NULL }
  )
  if (is.null(resp_wide)) return(NULL)

  item_cols <- setdiff(names(resp_wide), "id")
  resp_mat  <- as.matrix(resp_wide[, item_cols])

  # drop zero-variance items
  keep <- apply(resp_mat, 2, function(x) length(unique(na.omit(x))) >= 2)
  resp_mat  <- resp_mat[, keep, drop = FALSE]
  item_cols <- item_cols[keep]

  ni <- ncol(resp_mat)
  N  <- nrow(resp_mat)
  if (N < MIN_N) { message("  skip: N=", N, " < ", MIN_N); return(NULL) }
  if (N > 5000) {
    message("  sampling 5000 from N=", N)
    resp_mat <- resp_mat[sample(N, 5000), ]
    N <- 5000L
  }
  if (ni < MIN_ITEMS){ message("  skip: ", ni, " items < ", MIN_ITEMS); return(NULL) }

  # binomial model requires all items share the same K
  item_max <- apply(resp_mat, 2, max, na.rm = TRUE)
  if (!all(item_max == K_range)) {
    message("  skip: inconsistent K across items (range ",
            paste(range(item_max), collapse="-"), ")")
    return(NULL)
  }

  message("  N=", N, "  items=", ni, "  K_range=", K_range)

  train_idx    <- sample(N, floor(0.8 * N))
  test_idx     <- setdiff(seq_len(N), train_idx)
  train_mat    <- resp_mat[train_idx, ]
  test_mat     <- resp_mat[test_idx,  ]
  obs_rescaled <- test_mat / K_range

  predict_rescaled <- function(mod, test_data) {
    theta <- fscores(mod, response.pattern = test_data, method = "EAP")[, 1]
    Theta <- matrix(theta, ncol = 1)
    pred  <- sapply(seq_len(ni), function(j) {
      pr   <- probtrace(extract.item(mod, j), Theta)
      cats <- 0:(ncol(pr) - 1)
      as.numeric(pr %*% cats) / K_range
    })
    pred
  }
  compute_rmse <- function(pred, obs) sqrt(mean((pred - obs)^2, na.rm = TRUE))

  fit_eval <- function(label, itemtypes, custom = NULL, pars = NULL) {
    message("    ", label, " ...")
    args <- list(data = train_mat, model = 1, itemtype = itemtypes,
                 verbose = FALSE, technical = list(NCYCLES = 2000))
    if (!is.null(custom)) args$customItems <- custom
    if (!is.null(pars))   args$pars        <- pars
    mod <- tryCatch(do.call(mirt, args),
                    error = function(e) { message("      FAILED: ", e$message); NULL })
    if (is.null(mod)) return(NULL)
    pred <- tryCatch(predict_rescaled(mod, test_mat),
                     error = function(e) { message("      PREDICT FAILED: ", e$message); NULL })
    if (is.null(pred)) return(NULL)
    rmse <- compute_rmse(pred, obs_rescaled)
    message("      RMSE = ", round(rmse, 4))
    list(label = label, rmse = rmse)
  }

  pcm_pars <- tryCatch(
    {
      p <- mirt(train_mat, 1, itemtype = rep("gpcm", ni),
                verbose = FALSE, pars = "values")
      p$value[p$name == "a1"] <- 1
      p$est[p$name   == "a1"] <- FALSE
      p
    },
    error = function(e) { message("  pcm_pars failed: ", e$message); NULL }
  )
  if (is.null(pcm_pars)) return(NULL)

  res <- list(
    pcm           = fit_eval("PCM",         rep("gpcm",          ni), pars = pcm_pars),
    gpcm          = fit_eval("GPCM",        rep("gpcm",          ni)),
    grm           = fit_eval("GRM",         rep("graded",        ni)),
    tutz          = fit_eval("Tutz",        rep("sequential",    ni)),
    binom_1pl     = fit_eval("1PL+logit",   rep("binom_1pl",     ni), custom = custom_items),
    binom_2pl     = fit_eval("2PL+logit",   rep("binom_2pl",     ni), custom = custom_items),
    binom_1pl_cll = fit_eval("1PL+cloglog", rep("binom_1pl_cll", ni), custom = custom_items)
  )

  rmse_df <- do.call(rbind, lapply(res, function(r) {
    if (is.null(r)) return(NULL)
    data.frame(dataset = tab, model = r$label, rmse = round(r$rmse, 4),
               stringsAsFactors = FALSE)
  }))

  out <- list(tab = tab, K_range = K_range, ni = ni, N = N, rmse = rmse_df)
  saveRDS(out, cache)
  out
}

# ── 3. Run until N_DATASETS successes ─────────────────────────────────────────
combined_path <- file.path(out_dir, "multi_results.rds")
all_results   <- if (file.exists(combined_path)) readRDS(combined_path) else list()

meta       <- irw_metadata()
candidates <- meta$table

for (tab in candidates) {
  if (length(all_results) >= N_DATASETS) break
  if (tab %in% names(all_results)) { message("=== ", tab, " === (cached)"); next }
  message("\n=== ", tab, " ===")
  r <- process_one(tab)
  if (!is.null(r)) {
    all_results[[tab]] <- r
    saveRDS(all_results, combined_path)
    message("  -> success #", length(all_results), "/", N_DATASETS)
  }
}

# ── 4. Summary ────────────────────────────────────────────────────────────────
all_rmse <- do.call(rbind, lapply(all_results, `[[`, "rmse"))

cat("\n=== Per-dataset RMSE (long) ===\n")
print(all_rmse, row.names = FALSE)

wide <- reshape(all_rmse, idvar = "dataset", timevar = "model", direction = "wide")
names(wide) <- gsub("^rmse\\.", "", names(wide))
cat("\n=== Wide format (rows = datasets, cols = models) ===\n")
print(wide, row.names = FALSE)

saveRDS(
  list(all_results = all_results, rmse_long = all_rmse, rmse_wide = wide),
  file.path(out_dir, "multi_summary.rds")
)
cat("\nSaved to", out_dir, "\n")
