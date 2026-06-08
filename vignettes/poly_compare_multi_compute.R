# poly_compare_multi_compute.R
#
# Compare polytomous IRT models vs binomial IRT across IRW datasets.
# Each worker is self-contained: fetch → preprocess → fit → write result.
# Re-running skips already-cached results (multi_<tab>.rds).
#
# Run poly_compare_multi_combine.R separately to assemble multi_results.rds.
#
# Usage: Rscript vignettes/poly_compare_multi_compute.R

suppressPackageStartupMessages({
  library(irw)
  library(imv)
  library(mirt)
  library(scam)
  library(dplyr)
  library(parallel)
})

set.seed(20260527)

N_DATASETS <- 500
MIN_N      <- 200
MIN_ITEMS  <- 3
MAX_ITEMS  <- 60L
N_CORES    <- 4L
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

# ── 2. imv_c and imv_t (from intermodelvigorish/shinysite collapse.qmd) ───────
# imv.binary is imported from the imv package.
# y must have: $resp (integer 0..K-1), columns p1{k} and p2{k} for each k.
# pctt.tab: category proportions (length K), used as weights.

imv_c <- function(y, pctt.tab, p1, p2) {
  nn  <- length(pctt.tab)
  om  <- numeric(nn)
  iis <- 0:(nn - 1)
  for (ii in iis) {
    ns <- om.tmp <- numeric()
    jjs <- iis[-match(ii, iis)]
    for (jj in jjs) {
      y2   <- y[y$resp %in% c(ii, jj), ]
      resp <- ifelse(y2$resp == ii, 1, 0)
      p1.ii <- y2[[paste0(p1, ii)]]; p1.jj <- y2[[paste0(p1, jj)]]
      p2.ii <- y2[[paste0(p2, ii)]]; p2.jj <- y2[[paste0(p2, jj)]]
      z <- data.frame(resp = resp,
                      p1   = p1.ii / (p1.ii + p1.jj),
                      p2   = p2.ii / (p2.ii + p2.jj))
      om.tmp[as.character(jj)] <- imv.binary(z$resp, z$p1, z$p2)
      ns[as.character(jj)]     <- nrow(z)
    }
    om[ii + 1] <- sum(om.tmp * ns) / sum(ns)
  }
  sum(om * pctt.tab) / sum(pctt.tab)
}

imv_t <- function(y, pctt.tab, p1, p2) {
  nn <- length(pctt.tab)
  om <- numeric(nn - 1)
  for (ii in 0:(nn - 2)) {
    resp <- ifelse(y$resp <= ii, 1, 0)
    pr1  <- rowSums(y[, paste0(p1, 0:ii), drop = FALSE])
    pr2  <- rowSums(y[, paste0(p2, 0:ii), drop = FALSE])
    z    <- data.frame(resp = resp, p1 = pr1, p2 = pr2)
    om[ii + 1] <- imv.binary(z$resp, z$p1, z$p2)
  }
  wts <- pctt.tab[1:(nn - 1)] / (1 - pctt.tab[nn])
  sum(om * wts) / sum(wts)
}

# ── 2. Process one dataset (fetch → preprocess → fit → write) ─────────────────
process_one <- function(tab) {
  cache <- file.path(out_dir, paste0("multi_", tab, ".rds"))
  if (file.exists(cache)) {
    cached <- readRDS(cache)
    if (!is.null(cached[["imv"]]) && !is.null(cached[["pair_imv"]]) &&
        !is.null(cached[["imv_1pl"]]) && !is.null(cached[["asym"]]) &&
        !is.null(cached[["cat_decomp"]])) {
      message("=== ", tab, " === (cached)")
      return(cached)
    }
    message("=== ", tab, " === (re-running: cache missing required fields)")
  }

  message("\n=== ", tab, " ===")

  rate_limit_patterns <- c("too_many_requests", "rate_limit", "quota_exceeded",
                            "429", "too many", "rate limit", "quota")
  df <- NULL
  for (attempt in 1:3) {
    fetch_err <- NULL
    df <- tryCatch(irw_fetch(tab), error = function(e) {
      fetch_err <<- e$message
      msg <- e$message
      if (any(sapply(rate_limit_patterns, grepl, x = msg, ignore.case = TRUE))) {
        message("!!! REDIVIS RATE LIMIT: ", tab, " — ", msg)
      } else {
        message("  fetch attempt ", attempt, " failed: ", msg)
      }
      NULL
    })
    if (!is.null(df)) break
    Sys.sleep(5 * attempt)
  }
  if (is.null(df)) return(NULL)

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

  # K = number of unique response categories (excluding NA), enforced to be
  # consistent across items by keeping only items with the modal K.
  item_K   <- apply(resp_mat, 2, function(x) length(unique(na.omit(x))))
  target_K <- as.integer(names(sort(table(item_K), decreasing = TRUE))[1])
  if (!all(item_K == target_K)) {
    drop <- sum(item_K != target_K)
    message("  keeping ", sum(item_K == target_K), " items with K=", target_K,
            " (dropping ", drop, " items with different K)")
    resp_mat <- resp_mat[, item_K == target_K, drop = FALSE]
    ni       <- ncol(resp_mat)
    if (ni < MIN_ITEMS) { message("  skip: ", ni, " items < ", MIN_ITEMS, " after K filter"); return(NULL) }
  }
  K <- target_K
  if (K < 3) { message("  skip: K=", K, " < 3"); return(NULL) }
  if (K > 7) { message("  skip: K=", K, " > 7"); return(NULL) }

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

  message("  N=", N, "  items=", ni, "  K=", K)

  # Response-level 80/20 holdout: mask 20% of observed cells.
  non_missing <- which(!is.na(resp_mat), arr.ind = TRUE)
  holdout_idx <- sample(nrow(non_missing), floor(0.2 * nrow(non_missing)))
  train_mat   <- resp_mat
  train_mat[non_missing[holdout_idx, , drop = FALSE]] <- NA
  test_cells  <- non_missing[holdout_idx, , drop = FALSE]
  obs_vals    <- resp_mat[test_cells]

  # Returns list(pred, probs, theta) — theta is EAP for all N persons (needed for NP spline)
  predict_rescaled <- function(mod) {
    theta <- fscores(mod, response.pattern = train_mat, method = "EAP")[, 1]
    pred  <- numeric(nrow(test_cells))
    probs <- matrix(NA_real_, nrow(test_cells), K)
    for (j in seq_len(ni)) {
      rows_j <- which(test_cells[, 2] == j)
      if (length(rows_j) == 0L) next
      Theta_j <- matrix(theta[test_cells[rows_j, 1]], ncol = 1)
      pr      <- probtrace(extract.item(mod, j), Theta_j)
      if (ncol(pr) != K) next
      cats    <- 0:(ncol(pr) - 1L)
      pred[rows_j]    <- as.numeric(pr %*% cats) / (K - 1L)
      probs[rows_j, ] <- pr
    }
    list(pred = pred, probs = probs, theta = theta)
  }
  compute_rmse <- function(pred) sqrt(mean((pred - obs_vals / (K - 1L))^2, na.rm = TRUE))

  # Training category proportions used as the prevalence baseline for IMV
  train_obs <- as.integer(train_mat[!is.na(train_mat)])
  pctt.tab  <- as.numeric(table(factor(train_obs, levels = 0L:(K - 1L)))) / length(train_obs)

  compute_imv <- function(probs) {
    y_imv      <- as.data.frame(probs)
    colnames(y_imv) <- paste0("p2", 0L:(K - 1L))
    y_imv$resp <- obs_vals
    for (k in 0L:(K - 1L)) y_imv[[paste0("p1", k)]] <- pctt.tab[k + 1L]
    y_imv <- y_imv[complete.cases(y_imv), ]
    list(
      imv_c = tryCatch(imv_c(y_imv, pctt.tab, p1 = "p1", p2 = "p2"), error = function(e) NA_real_),
      imv_t = tryCatch(imv_t(y_imv, pctt.tab, p1 = "p1", p2 = "p2"), error = function(e) NA_real_)
    )
  }

  # Pairwise: how much does probs_enh improve over probs_base?
  compute_imv_pair <- function(probs_base, probs_enh) {
    y_imv <- as.data.frame(probs_base)
    colnames(y_imv) <- paste0("p1", 0L:(K - 1L))
    y_imv$resp <- obs_vals
    for (k in 0L:(K - 1L)) y_imv[[paste0("p2", k)]] <- probs_enh[, k + 1L]
    y_imv <- y_imv[complete.cases(y_imv), ]
    list(
      imv_c = tryCatch(imv_c(y_imv, pctt.tab, p1 = "p1", p2 = "p2"), error = function(e) NA_real_),
      imv_t = tryCatch(imv_t(y_imv, pctt.tab, p1 = "p1", p2 = "p2"), error = function(e) NA_real_)
    )
  }

  fit_eval <- function(label, itemtypes, custom = NULL, pars = NULL, extract_bpars = FALSE) {
    message("    ", label, " ...")
    args <- list(data = train_mat, model = 1, itemtype = itemtypes,
                 verbose = FALSE, technical = list(NCYCLES = 2000))
    if (!is.null(custom)) args$customItems <- custom
    if (!is.null(pars))   args$pars        <- pars
    mod <- tryCatch(do.call(mirt, args),
                    error = function(e) { message("      FAILED: ", e$message); NULL })
    if (is.null(mod)) return(NULL)
    out <- tryCatch(predict_rescaled(mod),
                    error = function(e) { message("      PREDICT FAILED: ", e$message); NULL })
    if (is.null(out)) return(NULL)
    rmse     <- compute_rmse(out$pred)
    imv_vals <- compute_imv(out$probs)
    message("      RMSE=", round(rmse, 4),
            "  imv_c=", round(imv_vals$imv_c, 4),
            "  imv_t=", round(imv_vals$imv_t, 4))
    item_bpars <- if (extract_bpars) {
      lapply(seq_len(ni), function(j) {
        p <- tryCatch(coef(mod, IRTpars = TRUE)[[j]], error = function(e) NULL)
        if (is.null(p)) return(NULL)
        as.numeric(p[1L, grep("^b", colnames(p))])
      })
    } else NULL
    list(label = label, rmse = rmse, imv_c = imv_vals$imv_c, imv_t = imv_vals$imv_t,
         probs = out$probs, theta = out$theta, item_bpars = item_bpars)
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
    gpcm          = fit_eval("GPCM",        rep("gpcm",          ni), extract_bpars = TRUE),
    grm           = fit_eval("GRM",         rep("graded",        ni)),
    tutz          = fit_eval("Tutz",        rep("sequential",    ni)),
    binom_1pl     = fit_eval("Binom 1PL (logit)",   rep("binom_1pl",     ni), custom = custom_items),
    binom_2pl     = fit_eval("Binom 2PL (logit)",   rep("binom_2pl",     ni), custom = custom_items),
    binom_1pl_cll = fit_eval("Binom 1PL (cloglog)", rep("binom_1pl_cll", ni), custom = custom_items)
  )

  # ── NP spline (monotone scam, binomial family, EAP thetas from GPCM) ──────────
  res$np_spline <- NULL
  if (!is.null(res$gpcm) && !is.null(res$gpcm$theta)) {
    message("    NP spline ...")
    theta_eap <- res$gpcm$theta
    np_fits <- lapply(seq_len(ni), function(j) {
      obs_rows <- which(!is.na(train_mat[, j]))
      df_j <- data.frame(X = as.integer(train_mat[obs_rows, j]),
                         th = theta_eap[obs_rows])
      tryCatch(
        scam(cbind(X, K - 1L - X) ~ s(th, bs = "mpi"), data = df_j, family = binomial()),
        error = function(e) NULL
      )
    })
    if (!any(sapply(np_fits, is.null))) {
      np_probs <- matrix(NA_real_, nrow(test_cells), K)
      np_pred  <- numeric(nrow(test_cells))
      for (j in seq_len(ni)) {
        rows_j <- which(test_cells[, 2] == j)
        if (length(rows_j) == 0L) next
        th_j  <- theta_eap[test_cells[rows_j, 1]]
        p_hat <- pmin(pmax(
          predict(np_fits[[j]], newdata = data.frame(th = th_j), type = "response"),
          1e-10), 1 - 1e-10)
        pr_j  <- outer(p_hat, 0:(K - 1L), function(p, k) dbinom(k, K - 1L, p))
        np_pred[rows_j]    <- as.numeric(pr_j %*% 0:(K - 1L)) / (K - 1L)
        np_probs[rows_j, ] <- pr_j
      }
      np_imv  <- compute_imv(np_probs)
      np_rmse <- compute_rmse(np_pred)
      message("      RMSE=", round(np_rmse, 4),
              "  imv_c=", round(np_imv$imv_c, 4),
              "  imv_t=", round(np_imv$imv_t, 4))
      res$np_spline <- list(label = "NP spline", rmse = np_rmse,
                            imv_c = np_imv$imv_c, imv_t = np_imv$imv_t,
                            probs = np_probs, theta = NULL, item_bpars = NULL)
    } else {
      message("      NP spline FAILED (scam convergence)")
    }
  }

  rmse_df <- do.call(rbind, lapply(res, function(r) {
    if (is.null(r)) return(NULL)
    data.frame(dataset = tab, model = r$label, rmse = round(r$rmse, 4),
               stringsAsFactors = FALSE)
  }))
  imv_df <- do.call(rbind, lapply(res, function(r) {
    if (is.null(r)) return(NULL)
    data.frame(dataset = tab, model = r$label,
               imv_c = round(r$imv_c, 4), imv_t = round(r$imv_t, 4),
               stringsAsFactors = FALSE)
  }))

  safe_pair <- function(r_base, r_enh) {
    if (is.null(r_base) || is.null(r_enh)) return(list(imv_c = NA_real_, imv_t = NA_real_))
    compute_imv_pair(r_base$probs, r_enh$probs)
  }
  p1 <- safe_pair(res$binom_1pl, res$pcm)
  p2 <- safe_pair(res$binom_2pl, res$gpcm)
  pair_imv_df <- data.frame(
    dataset    = tab,
    comparison = c("Binom 1PL (logit)→PCM", "Binom 2PL (logit)→GPCM"),
    imv_c      = round(c(p1$imv_c, p2$imv_c), 4),
    imv_t      = round(c(p1$imv_t, p2$imv_t), 4),
    stringsAsFactors = FALSE
  )

  # IMV vs. Binom 1PL (logit) floor: IMV(1PL, model) > 0 means model beats 1PL.
  # The 1PL row itself is 0 by definition (self-comparison).
  imv_1pl_df <- do.call(rbind, lapply(names(res), function(nm) {
    r <- res[[nm]]
    if (is.null(r)) return(NULL)
    if (nm == "binom_1pl") {
      vals <- list(imv_c = 0, imv_t = 0)
    } else if (is.null(res$binom_1pl)) {
      vals <- list(imv_c = NA_real_, imv_t = NA_real_)
    } else {
      vals <- tryCatch(
        compute_imv_pair(res$binom_1pl$probs, r$probs),
        error = function(e) list(imv_c = NA_real_, imv_t = NA_real_)
      )
    }
    data.frame(dataset = tab, model = r$label,
               imv_c = round(vals$imv_c, 4), imv_t = round(vals$imv_t, 4),
               stringsAsFactors = FALSE)
  }))

  # ── TODO 11: Asymmetry diagnostic — log(max_gap / min_gap) from GPCM b-params ─
  asym_df <- NULL
  if (!is.null(res$gpcm) && !is.null(res$gpcm$item_bpars)) {
    item_asym <- sapply(res$gpcm$item_bpars, function(b) {
      if (is.null(b) || length(b) < 2L) return(NA_real_)
      gaps <- diff(sort(b))
      gaps <- gaps[gaps > 0]
      if (length(gaps) < 2L) return(NA_real_)
      log(max(gaps) / min(gaps))
    })
    asym_df <- data.frame(
      dataset           = tab, K = K,
      mean_log_gap_ratio = round(mean(item_asym, na.rm = TRUE), 4),
      med_log_gap_ratio  = round(median(item_asym, na.rm = TRUE), 4),
      stringsAsFactors   = FALSE
    )
  }

  # ── TODO 12: Per-category log-likelihood advantage of GPCM over Binom 2PL ────
  # For each true response category k: mean(log P_GPCM(k) - log P_Binom2PL(k))
  # weighted by category frequency. Mirrors fig-kl-by-category in the simulation.
  cat_decomp_df <- NULL
  if (!is.null(res$gpcm) && !is.null(res$binom_2pl)) {
    pr_g <- res$gpcm$probs
    pr_b <- res$binom_2pl$probs
    cat_decomp_df <- do.call(rbind, lapply(0L:(K - 1L), function(k) {
      idx <- which(obs_vals == k)
      if (length(idx) < 5L) return(NULL)
      dll <- mean(log(pmax(pr_g[idx, k + 1L], 1e-15)) -
                  log(pmax(pr_b[idx, k + 1L], 1e-15)), na.rm = TRUE)
      data.frame(dataset = tab, K = K, cat = k,
                 freq = round(length(idx) / length(obs_vals), 4),
                 dll_gpcm_vs_binom2pl = round(dll, 6),
                 stringsAsFactors = FALSE)
    }))
  }

  out <- list(tab = tab, K = K, ni = ni, N = N,
              rmse = rmse_df, imv = imv_df,
              pair_imv = pair_imv_df, imv_1pl = imv_1pl_df,
              asym = asym_df, cat_decomp = cat_decomp_df)
  saveRDS(out, cache)
  out
}

# ── 3. Build candidate list ───────────────────────────────────────────────────
all_cached_names <- function() {
  fs <- list.files(out_dir, pattern = "^multi_.+\\.rds$", full.names = FALSE)
  fs <- fs[!grepl("^multi_(results|summary)\\.rds$", fs)]
  sub("^multi_", "", sub("\\.rds$", "", fs))
}

# Separate truly-complete caches (have imv_1pl) from ones that need updating.
all_cached   <- all_cached_names()
truly_done   <- Filter(function(nm) {
  x <- tryCatch(readRDS(file.path(out_dir, paste0("multi_", nm, ".rds"))),
                error = function(e) NULL)
  !is.null(x[["imv_1pl"]]) && !is.null(x[["asym"]]) && !is.null(x[["cat_decomp"]])
}, all_cached)
needs_update <- setdiff(all_cached, truly_done)

# New datasets to fill up to N_DATASETS beyond those already cached.
new_candidates <- setdiff(irw_filter(n_categories = c(3, 7)), all_cached)
new_candidates <- sample(new_candidates)
n_new          <- max(0L, N_DATASETS - length(all_cached))
new_candidates <- head(new_candidates, n_new)

candidates <- c(needs_update, new_candidates)

message(length(truly_done), " datasets complete; ",
        length(needs_update), " need imv_1pl update; adding up to ",
        length(new_candidates), " new candidates — ",
        length(candidates), " total jobs on ", N_CORES, " cores")

# ── 4. Parallel run ───────────────────────────────────────────────────────────
mclapply(candidates, process_one,
         mc.cores       = N_CORES,
         mc.preschedule = FALSE)

message("\nDone. ", length(all_cached_names()), " datasets cached.")
message("Run poly_compare_multi_combine.R to assemble multi_results.rds.")
