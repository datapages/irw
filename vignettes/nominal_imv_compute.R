# nominal_imv_compute.R
#
# How much is a wrong answer worth? For every multiple-choice table in the
# nominal source, compares out-of-sample predictions of right/wrong from a 2PL
# fitted to the 0/1 scores against two models fitted to the option chosen:
# Bock's nominal response model and the nested logit (Suh & Bolt, 2010). The
# Rasch -> 2PL gain on the same cells is the yardstick.
#
# Design: 5-fold cross-validation over person-item cells. Each fold hides 20%
# of the observed cells from every model; each model's EAP theta then predicts
# P(key) on the hidden cells, and the IMV compares those predictions. The
# outcome is always binary (keyed or not), so the option-level models are
# judged on the same thing the 2PL predicts: they only get to use the extra
# information, which wrong answer was chosen, on the training cells.
#
# Output: vignettes/nominal_imv_data/<table>.rds (one per table, skipped if it
# exists) and vignettes/nominal_imv_data/nominal_imv_results.rds (all tables).
# Subsampled inputs are cached in ~/.cache/irw_nominal_imv (not committed).
#
# Usage: Rscript vignettes/nominal_imv_compute.R   # from project root

library(irw)
library(mirt)
library(imv)
library(furrr)

set.seed(20260930)

out_dir   <- "vignettes/nominal_imv_data"
cache_dir <- file.path(Sys.getenv("HOME"), ".cache", "irw_nominal_imv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

SUBSAMPLE_N <- 3000   # respondents per table
N_FOLDS     <- 5
RARE        <- 0.01   # a wrong option chosen by fewer than 1% is pooled
MIN_ITEMS   <- 10
WORKERS     <- 6

# ------------------------------------------------------------------------------
# 1. Candidate tables: every nominal table with at least MIN_ITEMS items. The
#    metadata's n_categories is not used: it is stale for RMET/MRMET (77 and 40,
#    counted when resp_raw held the option words). Essays and other free text
#    fall out in prep(), as do tables without one keyed option per item.
# ------------------------------------------------------------------------------

meta <- read.csv("https://raw.githubusercontent.com/ben-domingue/irw/main/metadata/nominal_metadata.csv")
cands <- meta$table[meta$n_items >= MIN_ITEMS]
if (nzchar(Sys.getenv("TABLES"))) cands <- strsplit(Sys.getenv("TABLES"), ",")[[1]]
message("Candidates: ", length(cands))

# ------------------------------------------------------------------------------
# 2. Fetch + subsample, one table at a time (the enem_*_1mil_* tables need ~3 GB
#    each while in memory, so this pass is sequential).
# ------------------------------------------------------------------------------

fetch_one <- function(tn) {
  f <- file.path(cache_dir, paste0(tn, ".rds"))
  if (file.exists(f)) return(invisible(f))
  message("fetch ", tn)
  n <- irw_fetch(tn, source = "nom")
  if (!all(c("resp", "text") %in% names(n))) {
    saveRDS(list(table = tn, skipped = "no resp/text columns (unscored free text)",
                 date_run = as.character(Sys.Date())), file.path(out_dir, paste0(tn, ".rds")))
    return(invisible(NULL))
  }
  n <- n[!is.na(n$text) & !is.na(n$resp), c("id", "item", "resp", "text")]
  ids <- unique(n$id)
  n_full <- length(ids)
  if (n_full > SUBSAMPLE_N) n <- n[n$id %in% sample(ids, SUBSAMPLE_N), ]
  attr(n, "n_full") <- n_full
  saveRDS(n, f)
  rm(n); gc()
  invisible(f)
}
for (tn in cands) tryCatch(fetch_one(tn), error = function(e) message("  fetch failed: ", tn, ": ", conditionMessage(e)))

# ------------------------------------------------------------------------------
# 3. Long -> two wide matrices: the option chosen (key = highest code) and 0/1.
# ------------------------------------------------------------------------------

prep <- function(n) {
  n$text <- as.character(n$text)
  keys <- tapply(n$text[n$resp == 1], n$item[n$resp == 1], function(x) unique(x))
  if (any(lengths(keys) != 1)) return("not one keyed option per item")
  if (!all(n$resp %in% 0:1)) return("resp is not 0/1")
  n_text <- tapply(n$text, n$item, function(x) length(unique(x)))
  if (max(n_text) > 12) return("free text (more than 12 distinct answers to an item)")
  ids <- unique(n$id); items <- sort(names(keys))
  opt <- x01 <- matrix(NA_integer_, length(ids), length(items), dimnames = list(as.character(ids), items))
  n_pooled <- 0L; n_cells_dropped <- 0L
  for (it in items) {
    d <- n[n$item == it, ]
    k <- keys[[it]]
    share <- table(d$text) / nrow(d)
    rare <- setdiff(names(share)[share < RARE], k)
    if (length(rare) > 1) n_pooled <- n_pooled + 1L
    d$opt <- ifelse(d$text %in% rare, "_rare", d$text)
    # A pooled category that is itself under RARE (e.g. ENEM's blank and
    # double-mark codes, ~0.5% between them) is too thin to estimate: drop it.
    if (any(d$opt == "_rare") && mean(d$opt == "_rare") < RARE) {
      n_cells_dropped <- n_cells_dropped + sum(d$opt == "_rare")
      d <- d[d$opt != "_rare", ]
    }
    lv <- c(sort(setdiff(unique(d$opt), k)), k)   # key last: sets theta's direction
    r <- match(as.character(d$id), rownames(opt))
    opt[r, it] <- match(d$opt, lv)
    x01[r, it] <- as.integer(d$opt == k)
  }
  p <- colMeans(x01, na.rm = TRUE)
  ncat <- apply(opt, 2, function(z) length(unique(na.omit(z))))
  ok <- ncat >= 3 & p > 0 & p < 1
  opt <- opt[, ok, drop = FALSE]; x01 <- x01[, ok, drop = FALSE]
  # Respondents left with under half their items (ENEM candidates who left
  # nearly everything blank) carry too little to estimate theta.
  keep <- rowMeans(!is.na(opt)) >= 0.5
  if (ncol(opt) < MIN_ITEMS) return("fewer than MIN_ITEMS usable items")
  list(opt = opt[keep, , drop = FALSE], x01 = x01[keep, , drop = FALSE],
       n_items_dropped = sum(!ok), n_persons_dropped = sum(!keep),
       n_cells_dropped = n_cells_dropped)
}

# ------------------------------------------------------------------------------
# 4. Cross-validated comparison for one table.
# ------------------------------------------------------------------------------

fit_one <- function(tn) {
  out_file <- file.path(out_dir, paste0(tn, ".rds"))
  if (file.exists(out_file)) return(readRDS(out_file))
  f <- file.path(cache_dir, paste0(tn, ".rds"))
  if (!file.exists(f)) return(NULL)
  n <- readRDS(f)
  P <- prep(n)
  if (is.character(P)) {
    res <- list(table = tn, skipped = P, date_run = as.character(Sys.Date()))
    saveRDS(res, out_file); return(res)
  }
  opt <- P$opt; x01 <- P$x01
  ncat <- apply(opt, 2, max, na.rm = TRUE)
  obs <- which(!is.na(opt))
  fold <- sample(rep_len(seq_len(N_FOLDS), length(obs)))
  t0 <- Sys.time()

  per_fold <- lapply(seq_len(N_FOLDS), function(k) {
    ho <- obs[fold == k]
    opt_tr <- opt; opt_tr[ho] <- NA
    x_tr <- x01; x_tr[ho] <- NA
    mods <- list(
      rasch   = mirt(x_tr, 1, "Rasch", verbose = FALSE),
      twopl   = mirt(x_tr, 1, "2PL", verbose = FALSE),
      nominal = mirt(opt_tr, 1, "nominal", verbose = FALSE),
      nested  = mirt(opt_tr, 1, "2PLNRM", key = ncat, verbose = FALSE)
    )
    conv <- sapply(mods, function(m) extract.mirt(m, "converged"))
    rc <- arrayInd(ho, dim(opt))
    preds <- sapply(names(mods), function(m) {
      th <- fscores(mods[[m]], method = "EAP")[, 1]
      binary <- m %in% c("rasch", "twopl")
      out <- numeric(length(ho))
      for (j in unique(rc[, 2])) {
        w <- which(rc[, 2] == j)
        pr <- probtrace(extract.item(mods[[m]], j), matrix(th[rc[w, 1]]))
        # The keyed option is always the last category of the option models.
        out[w] <- pr[, if (binary) 2 else ncat[j]]
      }
      out
    })
    th2 <- fscores(mods$twopl, method = "EAP")[, 1]
    list(cells = data.frame(y = x01[ho], preds, theta_2pl = th2[rc[, 1]], fold = k), conv = conv)
  })
  converged <- t(sapply(per_fold, `[[`, "conv"))
  cells <- do.call(rbind, lapply(per_fold, `[[`, "cells"))
  cuts <- quantile(cells$theta_2pl, c(0, 1/3, 2/3, 1))
  cells$tercile <- cut(cells$theta_2pl, cuts, include.lowest = TRUE, labels = c("low", "mid", "high"))

  imvs <- function(d) c(
    rasch_to_2pl   = imv.binary(d$y, d$rasch, d$twopl),
    twopl_to_nom   = imv.binary(d$y, d$twopl, d$nominal),
    twopl_to_nest  = imv.binary(d$y, d$twopl, d$nested),
    nest_to_nom    = imv.binary(d$y, d$nested, d$nominal)
  )
  by_fold    <- t(sapply(split(cells, cells$fold), imvs))
  by_tercile <- t(sapply(split(cells, cells$tercile), imvs))

  res <- list(
    table = tn, skipped = NA_character_,
    n_full = attr(n, "n_full"), n_persons = nrow(opt), n_items = ncol(opt),
    p_correct = mean(x01, na.rm = TRUE),
    n_options = table(ncat),
    n_items_dropped = P$n_items_dropped, n_persons_dropped = P$n_persons_dropped,
    n_cells_dropped = P$n_cells_dropped,
    converged = converged,
    imv_all = imvs(cells), imv_by_fold = by_fold, imv_by_tercile = by_tercile,
    seconds = as.numeric(Sys.time() - t0, units = "secs"),
    date_run = as.character(Sys.Date()),
    irw_pkg = as.character(packageVersion("irw")),
    mirt_pkg = as.character(packageVersion("mirt"))
  )
  saveRDS(res, out_file)
  res
}

plan(multisession, workers = WORKERS)
results <- future_map(cands, function(tn) tryCatch(fit_one(tn), error = function(e)
  list(table = tn, skipped = paste("error:", conditionMessage(e)))),
  .options = furrr_options(seed = TRUE))
plan(sequential)
results <- Filter(Negate(is.null), results)
saveRDS(results, file.path(out_dir, "nominal_imv_results.rds"))
message("Done: ", sum(sapply(results, function(r) is.na(r$skipped))), " tables fitted, ",
        sum(sapply(results, function(r) !is.na(r$skipped))), " skipped")
