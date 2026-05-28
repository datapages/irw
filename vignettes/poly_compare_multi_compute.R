# poly_compare_multi_compute.R
#
# Compare polytomous IRT models vs binomial IRT across N_DATASETS IRW datasets.
# Datasets are processed in parallel batches via mclapply (dataset-level only).
# Model fitting within each dataset is sequential.
# Only datasets with 3–6 response categories (K_range 2–5) are included.
#
# Usage: Rscript vignettes/poly_compare_multi_compute.R

suppressPackageStartupMessages({
  library(irw)
  library(mirt)
  library(dplyr)
  library(parallel)
})

set.seed(20260527)

N_DATASETS <- 20
MIN_N      <- 200
MIN_ITEMS  <- 3
MAX_ITEMS  <- 60L
N_CORES    <- 6L
BATCH_SIZE <- N_CORES
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

# ── 2. Per-dataset pipeline ───────────────────────────────────────────────────
# Returns list(tab, K_range, ni, N, rmse) on success, NULL on skip/error.
# Inherited via fork: binom_1pl/2pl/cll, custom_items, MIN_N, MIN_ITEMS, out_dir.
process_one <- function(tab) {
  cache <- file.path(out_dir, paste0("multi_", tab, ".rds"))
  if (file.exists(cache)) {
    message("=== ", tab, " === (cached)")
    return(readRDS(cache))
  }
  message("\n=== ", tab, " ===")

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
  if (K_range < 2)  { message("  skip: K_range=", K_range, " < 2 (need K>=3)"); return(NULL) }
  if (K_range > 5)  { message("  skip: K_range=", K_range, " > 5");  return(NULL) }

  K_min    <- min(df$resp, na.rm = TRUE)
  df$resp0 <- df$resp - K_min

  resp_wide <- tryCatch(
    irw_long2resp(df %>% select(-resp) %>% rename(resp = resp0)),
    error = function(e) { message("  long2resp failed: ", e$message); NULL }
  )
  if (is.null(resp_wide)) return(NULL)

  item_cols <- setdiff(names(resp_wide), "id")
  resp_mat  <- as.matrix(resp_wide[, item_cols])

  keep      <- apply(resp_mat, 2, function(x) length(unique(na.omit(x))) >= 2)
  resp_mat  <- resp_mat[, keep, drop = FALSE]
  item_cols <- item_cols[keep]

  ni <- ncol(resp_mat)
  N  <- nrow(resp_mat)
  if (N < MIN_N)    { message("  skip: N=", N, " < ", MIN_N);          return(NULL) }
  if (ni < MIN_ITEMS){ message("  skip: ", ni, " items < ", MIN_ITEMS); return(NULL) }

  item_max  <- apply(resp_mat, 2, max, na.rm = TRUE)
  target_K  <- as.integer(names(sort(table(item_max), decreasing = TRUE))[1])
  if (target_K != K_range || !all(item_max == target_K)) {
    drop <- sum(item_max != target_K)
    message("  keeping ", sum(item_max == target_K), " items with K_range=", target_K,
            " (dropping ", drop, " items with different K)")
    keep_K    <- item_max == target_K
    resp_mat  <- resp_mat[, keep_K, drop = FALSE]
    item_cols <- item_cols[keep_K]
    K_range   <- target_K
    ni        <- ncol(resp_mat)
    if (ni < MIN_ITEMS) { message("  skip: ", ni, " items < ", MIN_ITEMS, " after K filter"); return(NULL) }
  }

  if (ni > MAX_ITEMS) {
    message("  sampling ", MAX_ITEMS, " items from ni=", ni)
    keep_idx  <- sample(ni, MAX_ITEMS)
    resp_mat  <- resp_mat[, keep_idx, drop = FALSE]
    item_cols <- item_cols[keep_idx]
    ni        <- MAX_ITEMS
  }

  if (N > 5000) {
    message("  sampling 5000 from N=", N)
    resp_mat <- resp_mat[sample(N, 5000), ]
    N <- 5000L
  }

  message("  N=", N, "  items=", ni, "  K_range=", K_range)

  train_idx    <- sample(N, floor(0.8 * N))
  test_idx     <- setdiff(seq_len(N), train_idx)
  train_mat    <- resp_mat[train_idx, ]
  test_mat     <- resp_mat[test_idx,  ]

  # Clip test to per-item max seen in training, then set any remaining
  # unseen categories (e.g. intermediate values absent from training) to NA
  # so mirt doesn't encounter unexpected categories during fscores().
  train_max <- apply(train_mat, 2, max, na.rm = TRUE)
  test_mat  <- t(pmin(t(test_mat), train_max))
  for (j in seq_len(ni)) {
    train_vals <- unique(na.omit(train_mat[, j]))
    unseen     <- !is.na(test_mat[, j]) & !(test_mat[, j] %in% train_vals)
    if (any(unseen)) test_mat[unseen, j] <- NA
  }

  obs_rescaled <- test_mat / K_range

  predict_rescaled <- function(mod) {
    theta <- fscores(mod, response.pattern = test_mat, method = "EAP")[, 1]
    Theta <- matrix(theta, ncol = 1)
    sapply(seq_len(ni), function(j) {
      pr   <- probtrace(extract.item(mod, j), Theta)
      cats <- 0:(ncol(pr) - 1)
      as.numeric(pr %*% cats) / K_range
    })
  }
  compute_rmse <- function(pred) sqrt(mean((pred - obs_rescaled)^2, na.rm = TRUE))

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

# ── 3. Run in batches until N_DATASETS successes ───────────────────────────────
combined_path <- file.path(out_dir, "multi_results.rds")
all_results   <- if (file.exists(combined_path)) readRDS(combined_path) else list()

candidates <- setdiff(
  irw_filter(n_categories = c(3, 6)),
  names(all_results)
)

i <- 1L
while (length(all_results) < N_DATASETS && i <= length(candidates)) {
  batch <- candidates[i:min(i + BATCH_SIZE - 1L, length(candidates))]
  i     <- i + BATCH_SIZE
  message("\n--- batch: ", paste(batch, collapse = ", "), " ---")

  batch_results <- mclapply(batch, process_one, mc.cores = min(BATCH_SIZE, N_CORES),
                             mc.preschedule = FALSE)

  for (k in seq_along(batch)) {
    r <- batch_results[[k]]
    if (!is.null(r)) {
      all_results[[batch[k]]] <- r
      saveRDS(all_results, combined_path)
      message("  -> success #", length(all_results), "/", N_DATASETS,
              " (", batch[k], ")")
    }
  }

  if (length(all_results) >= N_DATASETS) break
}

# ── 4. Summary ─────────────────────────────────────────────────────────────────
all_results <- all_results[seq_len(min(N_DATASETS, length(all_results)))]
all_rmse    <- do.call(rbind, lapply(all_results, `[[`, "rmse"))

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
