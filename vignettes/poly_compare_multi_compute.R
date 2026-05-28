# poly_compare_multi_compute.R
#
# Compare polytomous IRT models vs binomial IRT across IRW datasets.
# Each dataset is processed in parallel via mclapply and written to its own
# cache file. After each batch, completed results are collected from the
# directory (not from mclapply return values, so a crashed worker that still
# wrote its file doesn't block progress).
#
# Run poly_compare_multi_combine.R separately to assemble multi_results.rds.
#
# Usage: Rscript vignettes/poly_compare_multi_compute.R

suppressPackageStartupMessages({
  library(irw)
  library(mirt)
  library(dplyr)
  library(parallel)
})

set.seed(20260527)

N_DATASETS <- 250
MIN_N      <- 200
MIN_ITEMS  <- 3
MAX_ITEMS  <- 60L
N_CORES    <- 2L
out_dir    <- "vignettes/poly_compare_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Using ", N_CORES, " cores")

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

# ── 2a. Sequential fetch + preprocess (one fetch per dataset, no race) ────────
fetch_one <- function(tab) {
  data_cache <- file.path(out_dir, paste0("data_", tab, ".rds"))
  if (file.exists(data_cache)) return(readRDS(data_cache))

  message("  fetching ", tab, " ...")
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
  if (K_range < 2)  { message("  skip: K_range=", K_range, " < 2"); return(NULL) }
  if (K_range > 5)  { message("  skip: K_range=", K_range, " > 5"); return(NULL) }

  K_min    <- min(df$resp, na.rm = TRUE)
  df$resp0 <- df$resp - K_min

  resp_wide <- tryCatch(
    irw_long2resp(df %>% select(-resp) %>% rename(resp = resp0)),
    error = function(e) { message("  long2resp failed: ", e$message); NULL }
  )
  if (is.null(resp_wide)) return(NULL)

  item_cols <- setdiff(names(resp_wide), "id")
  resp_mat  <- as.matrix(resp_wide[, item_cols])

  keep     <- apply(resp_mat, 2, function(x) length(unique(na.omit(x))) >= 2)
  resp_mat <- resp_mat[, keep, drop = FALSE]

  ni <- ncol(resp_mat)
  N  <- nrow(resp_mat)
  if (N < MIN_N)     { message("  skip: N=", N, " < ", MIN_N);          return(NULL) }
  if (ni < MIN_ITEMS){ message("  skip: ", ni, " items < ", MIN_ITEMS); return(NULL) }

  item_max <- apply(resp_mat, 2, max, na.rm = TRUE)
  target_K <- as.integer(names(sort(table(item_max), decreasing = TRUE))[1])
  if (target_K != K_range || !all(item_max == target_K)) {
    drop <- sum(item_max != target_K)
    message("  keeping ", sum(item_max == target_K), " items with K_range=", target_K,
            " (dropping ", drop, " items with different K)")
    keep_K   <- item_max == target_K
    resp_mat <- resp_mat[, keep_K, drop = FALSE]
    K_range  <- target_K
    ni       <- ncol(resp_mat)
    if (ni < MIN_ITEMS) { message("  skip: ", ni, " items < ", MIN_ITEMS, " after K filter"); return(NULL) }
    if (K_range < 2)    { message("  skip: K_range=", K_range, " < 2 after K filter");       return(NULL) }
  }

  if (ni > MAX_ITEMS) {
    message("  sampling ", MAX_ITEMS, " items from ni=", ni)
    resp_mat <- resp_mat[, sample(ni, MAX_ITEMS), drop = FALSE]
    ni       <- MAX_ITEMS
  }
  if (N > 5000) {
    message("  sampling 5000 from N=", N)
    resp_mat <- resp_mat[sample(N, 5000), ]
    N        <- 5000L
  }

  out <- list(tab = tab, K_range = K_range, ni = ni, N = N, resp_mat = resp_mat)
  saveRDS(out, data_cache)
  out
}

# ── 2b. Parallel model fitting (reads preloaded data, no irw_fetch) ───────────
process_one <- function(tab) {
  cache <- file.path(out_dir, paste0("multi_", tab, ".rds"))
  if (file.exists(cache)) {
    message("=== ", tab, " === (cached)")
    return(readRDS(cache))
  }

  data_cache <- file.path(out_dir, paste0("data_", tab, ".rds"))
  if (!file.exists(data_cache)) {
    message("=== ", tab, " === (no data cache, skipping)")
    return(NULL)
  }
  d <- readRDS(data_cache)

  message("\n=== ", tab, " ===")
  K_range  <- d$K_range
  ni       <- d$ni
  N        <- d$N
  resp_mat <- d$resp_mat

  message("  N=", N, "  items=", ni, "  K_range=", K_range)

  # Response-level 80/20 holdout: mask 20% of observed cells.
  # Models fit on training matrix; persons scored on their training responses
  # only; held-out cells predicted from those scores.
  non_missing <- which(!is.na(resp_mat), arr.ind = TRUE)
  holdout_idx <- sample(nrow(non_missing), floor(0.2 * nrow(non_missing)))
  train_mat   <- resp_mat
  train_mat[non_missing[holdout_idx, , drop = FALSE]] <- NA
  test_cells  <- non_missing[holdout_idx, , drop = FALSE]
  obs_vals    <- resp_mat[test_cells]

  # Vectorized: group held-out cells by item, one probtrace call per item.
  predict_rescaled <- function(mod) {
    theta <- fscores(mod, response.pattern = train_mat, method = "EAP")[, 1]
    pred  <- numeric(nrow(test_cells))
    for (j in seq_len(ni)) {
      rows_j <- which(test_cells[, 2] == j)
      if (length(rows_j) == 0L) next
      Theta_j <- matrix(theta[test_cells[rows_j, 1]], ncol = 1)
      pr      <- probtrace(extract.item(mod, j), Theta_j)
      cats    <- 0:(ncol(pr) - 1L)
      pred[rows_j] <- as.numeric(pr %*% cats) / K_range
    }
    pred
  }
  compute_rmse <- function(pred) sqrt(mean((pred - obs_vals / K_range)^2, na.rm = TRUE))

  fit_eval <- function(label, itemtypes, custom = NULL, pars = NULL) {
    message("    ", label, " ...")
    args <- list(data = train_mat, model = 1, itemtype = itemtypes,
                 verbose = FALSE, technical = list(NCYCLES = 2000))
    if (!is.null(custom)) args$customItems <- custom
    if (!is.null(pars))   args$pars        <- pars
    mod <- tryCatch(do.call(mirt, args),
                    error = function(e) { message("      FAILED: ", e$message); NULL })
    if (is.null(mod)) return(NULL)
    pred <- tryCatch(predict_rescaled(mod),
                     error = function(e) { message("      PREDICT FAILED: ", e$message); NULL })
    if (is.null(pred)) return(NULL)
    rmse <- compute_rmse(pred)
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

# ── 3. Build candidate list ───────────────────────────────────────────────────
already_done <- function() {
  fs <- list.files(out_dir, pattern = "^multi_.+\\.rds$", full.names = FALSE)
  fs <- fs[!grepl("^multi_(results|summary)\\.rds$", fs)]
  sub("^multi_", "", sub("\\.rds$", "", fs))
}

done_before <- already_done()
candidates  <- setdiff(irw_filter(n_categories = c(3, 6)), done_before)
candidates  <- sample(candidates)               # randomize so runs aren't alphabetical
n_needed    <- max(0L, N_DATASETS - length(done_before))
candidates  <- head(candidates, n_needed * 3L)  # over-provision to absorb skips

message(length(done_before), " datasets already cached; running up to ",
        length(candidates), " candidates on ", N_CORES, " cores")

# ── 4. Sequential fetch pass (one irw_fetch per dataset, no race condition) ───
message("\nFetching data sequentially ...")
already_data <- {
  fs <- list.files(out_dir, pattern = "^data_.+\\.rds$", full.names = FALSE)
  sub("^data_", "", sub("\\.rds$", "", fs))
}
to_fetch <- setdiff(candidates, already_data)
message(length(already_data), " datasets already fetched; fetching ",
        length(to_fetch), " new datasets")
for (tab in to_fetch) fetch_one(tab)

# ── 5. Parallel model fitting (workers read local data caches, no irw_fetch) ──
# mc.preschedule = FALSE: each worker picks up the next candidate immediately
# when it finishes — no batch barrier.
mclapply(candidates, process_one,
         mc.cores       = N_CORES,
         mc.preschedule = FALSE)

message("\nDone. ", length(already_done()), " datasets cached.")
message("Run poly_compare_multi_combine.R to assemble multi_results.rds.")
