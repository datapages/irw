# dutch_identity_gapshare.R
#
# Corpus-wide version of Table 1 of the Dutch Identity vignette: across IRW
# dichotomous tables, how much of the pairwise association structure does a
# single latent dimension account for?
#
# For each table we fit three nested loglinear models to the 2^J pattern counts
# (zero cells included), exactly as the vignette does:
#   independence          J + 1 parameters
#   restricted log-quadratic (rank-1 Γ)   2J + 1
#   full SOE              1 + J + J(J-1)/2
# and report
#   gap share = (ℓ_rq − ℓ_ind) / (ℓ_soe − ℓ_ind).
#
# In sample that share is bounded in [0, 1] and is inflated at small J. The
# held-out version (5-fold CV over respondents) is the honest one, BUT it is
# reported as a ratio only where the denominator is comfortably positive: out of
# sample the SOE can score WORSE than independence, and then the ratio flips
# sign and means nothing. The primary numbers are therefore the three held-out
# log-likelihoods per held-out respondent, in nats; the share is secondary.
#
# Held-out log-likelihoods are also reported with a floor on fitted cell
# probabilities. A model can give a pattern a fitted probability of e^-30; one
# held-out respondent landing there otherwise dominates the fold, and that is a
# property of an empty cell, not of overfitting.
#
# Outputs (vignettes/dutchdata/):
#   gapshare_tables.csv      one row per table (main stratum)
#   gapshare_bigJ.csv        J > 10 stratum, 5 random 10-item draws per table
#   gapshare_excluded.csv    every table considered and why it was dropped
#   gapshare_summary.md      headline numbers
#   gapshare_hist.png, gapshare_by_J.png, gapshare_by_construct.png
#   gapshare_run.rds         full per-table results, including the fits' flags
#
# Does not modify any vignette.
#
# Usage:
#   Rscript vignettes/dutch_identity_gapshare.R        # from project root
#   BIGJ=1 Rscript vignettes/dutch_identity_gapshare.R # J > 10 stratum only
#   CACHED_ONLY=1 BIGJ=1 Rscript ...                   # tabulate the cache, fetch nothing

suppressMessages({
  library(irw)
  library(dplyr)
  library(tibble)
  library(ggplot2)
})

source("vignettes/dutch_identity_helpers.R")

out_dir   <- "vignettes/dutchdata"
cache_dir <- file.path(out_dir, "gapshare_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

SEED       <- 20260222
N_FOLDS    <- 5
N_RANDOM   <- 10     # random starts for the rank-1 fit, on top of 5 fixed ones
MIN_J      <- 4
MAX_J      <- 10
MIN_N_ABS  <- 1000
COEF_FLAG  <- 15     # |coef| above this flags the SOE fit
BIGJ_DRAWS <- 5      # random 10-item draws per J > 10 table
# the held-out ratio is only formed when the SOE beats independence by at least
# this much per held-out respondent; below it the denominator is noise
DENOM_MIN  <- 1e-3

set.seed(SEED)

# Record the exact per-dataset versions and pin them for the run. Note that
# irw_use_version() can only pin datasets the installed package knows about; it
# warns about any it does not, and those are read at whatever version is live.
irw_v <- tryCatch({
  v <- as.data.frame(irw_version())
  paste(paste0(v$dataset, "=", v$version), collapse = "; ")
}, error = function(e) NA_character_)
pin_warn <- NA_character_
pinned <- tryCatch(withCallingHandlers({ irw_use_version(); TRUE },
  warning = function(w) { pin_warn <<- conditionMessage(w); invokeRestart("muffleWarning") }),
  error = function(e) FALSE)
message("IRW versions pinned (", pinned, "): ", irw_v)
if (!is.na(pin_warn)) message("pin warning: ", gsub("\n", " ", pin_warn))

# ==============================================================================
# Selection
# ==============================================================================

lowest <- function(x) {
  u <- unique(x[!is.na(x)])
  num <- suppressWarnings(as.numeric(as.character(u)))
  if (all(!is.na(num))) u[which.min(num)] else sort(as.character(u))[1]
}

# Single-administration rule, as in bits_per_item_scout.R: earliest wave, then
# the control arm, then exclude anything that still has repeated id-item rows.
# Then drop items answered by fewer than half the respondents and items with no
# variance, and keep complete-case respondents on what remains.
# Fetches fail transiently under rate limiting, and a failed fetch can come back
# as an empty frame rather than an error. Both are retried, and neither is ever
# cached: an early version cached them and silently turned 135 readable tables
# into permanent exclusions labelled "no non-missing responses".
fetch_retry <- function(tab, tries = 3, wait = 20) {
  for (k in seq_len(tries)) {
    df <- tryCatch(as.data.frame(irw_fetch(tab)),
                   error = function(e) conditionMessage(e))
    if (!is.character(df) && nrow(df) > 0) return(df)
    if (k < tries) {
      message("    fetch attempt ", k, " gave ",
              if (is.character(df)) "an error" else "0 rows", "; retrying")
      Sys.sleep(wait * k)
    }
  }
  if (is.character(df)) list(transient = TRUE, reason = paste("fetch failed:", df))
  else list(transient = TRUE, reason = "fetch returned 0 rows")
}

prepare_table <- function(tab) {
  df <- fetch_retry(tab)
  if (is.list(df) && isTRUE(df$transient))
    return(list(ok = FALSE, transient = TRUE, reason = df$reason))

  n_waves <- if ("wave" %in% names(df)) length(unique(na.omit(df$wave))) else NA_integer_
  wave_kept <- NA_character_
  if (!is.na(n_waves) && n_waves > 1) {
    wave_kept <- as.character(lowest(df$wave))
    df <- df[!is.na(df$wave) & as.character(df$wave) == wave_kept, ]
  }
  n_treat <- if ("treat" %in% names(df)) length(unique(na.omit(df$treat))) else NA_integer_
  treat_kept <- NA_character_
  if (!is.na(n_treat) && n_treat > 1) {
    treat_kept <- as.character(lowest(df$treat))
    df <- df[!is.na(df$treat) & as.character(df$treat) == treat_kept, ]
  }
  df <- df[!is.na(df$resp), ]
  if (!nrow(df)) return(list(ok = FALSE, reason = "no non-missing responses"))

  if (anyDuplicated(paste(df$id, df$item, sep = "\r")))
    return(list(ok = FALSE, reason = "repeated id-item rows (trial-level)"))

  vals <- sort(unique(df$resp))
  if (length(vals) != 2)
    return(list(ok = FALSE, reason = paste0("not dichotomous after cleaning (",
                                            length(vals), " values)")))
  df$resp01 <- as.integer(df$resp == vals[2])

  n_ids   <- length(unique(df$id))
  item_n  <- table(df$item)
  keep    <- names(item_n)[item_n >= 0.5 * n_ids]
  df      <- df[df$item %in% keep, ]
  if (!nrow(df)) return(list(ok = FALSE, reason = "no item answered by half the respondents"))

  item_sd <- tapply(df$resp01, df$item, function(x) length(unique(x)))
  keep    <- names(item_sd)[item_sd > 1]
  df      <- df[df$item %in% keep, ]
  if (!nrow(df)) return(list(ok = FALSE, reason = "no item with variance"))

  wide <- reshape(df[, c("id", "item", "resp01")], idvar = "id",
                  timevar = "item", direction = "wide")
  rownames(wide) <- NULL
  mat <- as.matrix(wide[, -1, drop = FALSE])
  colnames(mat) <- sub("^resp01\\.", "x_", colnames(mat))
  colnames(mat) <- make.names(colnames(mat), unique = TRUE)
  mat <- mat[complete.cases(mat), , drop = FALSE]
  if (!nrow(mat)) return(list(ok = FALSE, reason = "no complete-case respondents"))

  # zero variance can reappear once incomplete respondents are dropped
  keep <- apply(mat, 2, function(x) length(unique(x))) > 1
  mat  <- mat[, keep, drop = FALSE]
  mat  <- mat[complete.cases(mat), , drop = FALSE]

  list(ok = TRUE, mat = mat, J = ncol(mat), N = nrow(mat),
       n_waves = n_waves, wave_kept = wave_kept,
       n_treat = n_treat, treat_kept = treat_kept)
}

pattern_counts <- function(mat) {
  J <- ncol(mat); item_cols <- colnames(mat)
  pats <- expand.grid(replicate(J, 0:1, simplify = FALSE), KEEP.OUT.ATTRS = FALSE)
  colnames(pats) <- item_cols
  key  <- function(m) apply(m, 1, paste, collapse = "")
  obs  <- table(key(mat))
  pats$count <- as.integer(obs[key(as.matrix(pats))])
  pats$count[is.na(pats$count)] <- 0L
  pats
}

# ==============================================================================
# The three fits on one table of counts
# ==============================================================================

fit_three <- function(pats, item_cols, n_random = N_RANDOM) {
  X <- as.matrix(pats[, item_cols])
  counts <- pats$count
  N <- sum(counts)
  warned <- FALSE
  catch <- function(expr) withCallingHandlers(
    tryCatch(expr, error = function(e) NULL),
    warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") })

  ind <- catch(glm(as.formula(paste("count ~", paste(item_cols, collapse = " + "))),
                   data = pats, family = poisson()))
  soe <- catch(glm(as.formula(paste0("count ~ (", paste(item_cols, collapse = " + "), ")^2")),
                   data = pats, family = poisson()))
  if (is.null(ind) || is.null(soe)) return(NULL)

  G <- gamma_from_soe(soe, item_cols)
  e <- eigen(G, symmetric = TRUE)
  rq <- fit_log_quad(X, counts,
                     alpha_start = coef(ind)[1], beta_start = coef(ind)[-1],
                     gamma_lead  = sqrt(abs(e$values[1])) * e$vectors[, 1],
                     n_random = n_random)
  if (is.null(rq)) return(NULL)

  log_mu <- list(
    ind = as.numeric(predict(ind, newdata = pats, type = "link")),
    rq  = log_quad_log_mu(rq$opt$par, X),
    soe = as.numeric(predict(soe, newdata = pats, type = "link"))
  )
  ll <- vapply(log_mu, pois_ll, numeric(1), counts = counts)
  mll <- vapply(log_mu, mult_ll, numeric(1), counts = counts)

  max_abs_coef <- max(abs(c(coef(ind), coef(soe), rq$opt$par)), na.rm = TRUE)

  list(log_mu = log_mu, ll = ll, mll = mll, N = N,
       rq_conv = rq$convergence,
       flags = list(
         warned        = warned,
         ind_converged = isTRUE(ind$converged),
         soe_converged = isTRUE(soe$converged),
         max_abs_coef  = max_abs_coef,
         big_coef      = isTRUE(max_abs_coef > COEF_FLAG),
         nonfinite     = any(!is.finite(unlist(log_mu)))
       ))
}

# Table 1's invariants, for every table.
check_fit <- function(fit, J, N) {
  ok <- TRUE
  for (lm_ in fit$log_mu) {
    p <- exp(lm_ - log(sum(exp(lm_))))
    if (abs(sum(p) - 1) > 1e-8) ok <- FALSE
  }
  tol <- 1e-6 * max(1, abs(fit$ll[["soe"]]))
  if (!(fit$ll[["ind"]] <= fit$ll[["rq"]] + tol)) ok <- FALSE
  if (!(fit$ll[["rq"]] <= fit$ll[["soe"]] + tol)) ok <- FALSE
  if (any(fit$mll > 0)) ok <- FALSE
  ok
}

# ==============================================================================
# 5-fold CV over respondents
# ==============================================================================

cv_three <- function(mat, n_folds = N_FOLDS, seed = SEED) {
  J <- ncol(mat); N <- nrow(mat); item_cols <- colnames(mat)
  set.seed(seed)
  fold <- sample(rep_len(seq_len(n_folds), N))

  tot <- c(ind = 0, rq = 0, soe = 0)
  tot_floor <- tot
  n_below <- 0L; n_eval <- 0L; n_fail <- 0L

  for (f in seq_len(n_folds)) {
    tr <- mat[fold != f, , drop = FALSE]
    te <- mat[fold == f, , drop = FALSE]
    if (!nrow(te) || !nrow(tr)) next
    pats_tr <- pattern_counts(tr)
    pats_te <- pattern_counts(te)
    stopifnot(identical(pats_tr[, item_cols], pats_te[, item_cols]))

    fit <- fit_three(pats_tr, item_cols)
    if (is.null(fit)) { n_fail <- n_fail + 1L; next }

    floor_p <- 1 / (10 * nrow(tr))
    for (nm in names(tot)) {
      h <- mult_ll_heldout(fit$log_mu[[nm]], pats_te$count, floor_p = floor_p)
      tot[nm]       <- tot[nm] + h$raw
      tot_floor[nm] <- tot_floor[nm] + h$floored
      if (nm == "soe") n_below <- n_below + h$n_at_floor
    }
    n_eval <- n_eval + nrow(te)
  }
  if (n_fail == n_folds || n_eval == 0) return(NULL)
  list(ll = tot, ll_floor = tot_floor, n_eval = n_eval,
       n_folds_failed = n_fail,
       n_heldout_below_floor = n_below)
}

gap_share <- function(ll) unname((ll[["rq"]] - ll[["ind"]]) / (ll[["soe"]] - ll[["ind"]]))

# ==============================================================================
# One table, end to end
# ==============================================================================

# The J > 10 stratum draws five item subsets from the same table, so the cleaned
# matrix is cached rather than refetched once per draw.
prepare_cached <- function(tab) {
  pf <- file.path(cache_dir, paste0("prep__", tab, ".rds"))
  if (file.exists(pf)) return(readRDS(pf))
  prep <- prepare_table(tab)
  if (!isTRUE(prep$transient)) saveRDS(prep, pf)   # never cache a transient failure
  prep
}

run_table <- function(tab, items_subset = NULL, draw = NA_integer_) {
  tag <- if (is.na(draw)) tab else paste0(tab, "__draw", draw)
  cf  <- file.path(cache_dir, paste0(tag, ".rds"))
  if (file.exists(cf)) return(readRDS(cf))

  t0 <- Sys.time()
  prep <- prepare_cached(tab)
  if (!prep$ok) {
    res <- list(table = tab, draw = draw, ok = FALSE, reason = prep$reason,
                transient = isTRUE(prep$transient))
    if (!isTRUE(prep$transient)) saveRDS(res, cf)
    return(res)
  }
  mat <- prep$mat
  if (!is.null(items_subset)) {
    if (ncol(mat) < 10) {
      res <- list(table = tab, draw = draw, ok = FALSE,
                  reason = paste0("only ", ncol(mat), " items after cleaning"))
      saveRDS(res, cf); return(res)
    }
    set.seed(SEED + draw)
    mat <- mat[, sort(sample(seq_len(ncol(mat)), 10)), drop = FALSE]
    mat <- mat[, apply(mat, 2, function(x) length(unique(x))) > 1, drop = FALSE]
  }
  J <- ncol(mat); N <- nrow(mat)
  items_used <- paste(colnames(mat), collapse = "|")

  if (J < MIN_J || J > MAX_J) {
    res <- list(table = tab, draw = draw, ok = FALSE, J = J, N = N,
                reason = paste0("J = ", J, " outside [", MIN_J, ", ", MAX_J, "]"))
    saveRDS(res, cf); return(res)
  }
  need <- max(MIN_N_ABS, 5 * 2^J)
  if (N < need) {
    res <- list(table = tab, draw = draw, ok = FALSE, J = J, N = N,
                reason = paste0("N = ", N, " < max(1000, 5*2^J) = ", need))
    saveRDS(res, cf); return(res)
  }

  pats <- pattern_counts(mat)
  fit  <- fit_three(pats, colnames(mat))
  if (is.null(fit)) {
    res <- list(table = tab, draw = draw, ok = FALSE, J = J, N = N,
                reason = "in-sample fit failed")
    saveRDS(res, cf); return(res)
  }
  stopifnot(check_fit(fit, J, N))
  df_counts <- c(ind = J + 1, rq = 2 * J + 1, soe = 1 + J + choose(J, 2))
  stopifnot(df_counts[["ind"]] < df_counts[["rq"]], df_counts[["rq"]] < df_counts[["soe"]])
  stopifnot(nrow(pats) == 2^J, sum(pats$count) == N)

  cv <- cv_three(mat)

  res <- list(
    table = tab, draw = draw, ok = TRUE, J = J, N = N,
    items_used = items_used,
    n_waves = prep$n_waves, wave_kept = prep$wave_kept,
    n_treat = prep$n_treat, treat_kept = prep$treat_kept,
    ll = fit$ll, mll = fit$mll,
    df = c(ind = J + 1, rq = 2 * J + 1, soe = 1 + J + choose(J, 2)),
    share_in = gap_share(fit$ll),
    rq_conv = fit$rq_conv, flags = fit$flags,
    cv = cv,
    share_out = if (is.null(cv)) NA_real_ else gap_share(cv$ll),
    share_out_floor = if (is.null(cv)) NA_real_ else gap_share(cv$ll_floor),
    secs = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
  saveRDS(res, cf)
  res
}

# ==============================================================================
# Candidates
# ==============================================================================

meta  <- as_tibble(irw_metadata())
tags  <- tryCatch(as_tibble(irw_tags()), error = function(e) NULL)

bigj <- Sys.getenv("BIGJ") == "1"
only <- Sys.getenv("ONLY")
# CACHED_ONLY=1 tabulates whatever the cache already holds and fetches nothing,
# so an interrupted sweep can be reported on without rerunning it. Tables with
# no cached result are skipped outright rather than recorded as exclusions.
cached_only <- Sys.getenv("CACHED_ONLY") == "1"
cache_hit <- function(tab, draw = NA_integer_)
  file.exists(file.path(cache_dir, paste0(
    if (is.na(draw)) tab else paste0(tab, "__draw", draw), ".rds")))
cands <- meta |>
  filter(n_categories == 2) |>
  { \(d) if (bigj) filter(d, n_items > MAX_J, n_participants >= 5 * 2^MAX_J)
         else filter(d, n_items >= MIN_J, n_items <= MAX_J,
                     n_participants >= pmax(MIN_N_ABS, 5 * 2^n_items)) }() |>
  arrange(n_responses)
if (nzchar(only)) cands <- filter(cands, table %in% strsplit(only, ",")[[1]])
message("Candidates (", if (bigj) "J > 10 stratum" else "main stratum", "): ", nrow(cands),
        if (cached_only) " [CACHED_ONLY: no fetching]" else "")

results <- list()
for (i in seq_len(nrow(cands))) {
  tab <- cands$table[i]
  message(i, "/", nrow(cands), "  ", tab)
  if (bigj) {
    for (d in seq_len(BIGJ_DRAWS)) {
      if (cached_only && !cache_hit(tab, d)) next
      results[[length(results) + 1]] <-
        tryCatch(run_table(tab, items_subset = TRUE, draw = d),
                 error = function(e) list(table = tab, draw = d, ok = FALSE,
                                          reason = paste("error:", conditionMessage(e))))
    }
  } else {
    if (!(cached_only && !cache_hit(tab)))
      results[[length(results) + 1]] <-
        tryCatch(run_table(tab),
                 error = function(e) list(table = tab, draw = NA_integer_, ok = FALSE,
                                          reason = paste("error:", conditionMessage(e))))
  }
  gc(verbose = FALSE)
}

saveRDS(list(results = results, irw_version = irw_v, pin_warning = pin_warn, seed = SEED,
             date_run = Sys.Date(), stratum = if (bigj) "bigJ" else "main",
             cached_only = cached_only, n_candidates = nrow(cands)),
        file.path(out_dir, if (bigj) "gapshare_run_bigJ.rds" else "gapshare_run.rds"))

# ==============================================================================
# Tabulate
# ==============================================================================

num <- function(x, nm) if (is.null(x)) NA_real_ else unname(x[[nm]])

rows <- lapply(results, function(r) tibble(
  table = r$table, draw = r$draw %||% NA_integer_, ok = isTRUE(r$ok),
  reason = if (isTRUE(r$ok)) NA_character_ else r$reason %||% NA_character_,
  J = r$J %||% NA_integer_, N = r$N %||% NA_integer_,
  items_used = r$items_used %||% NA_character_,
  ll_ind = num(r$ll, "ind"), ll_rq = num(r$ll, "rq"), ll_soe = num(r$ll, "soe"),
  share_in = r$share_in %||% NA_real_,
  cv_ll_ind = num(r$cv$ll, "ind"), cv_ll_rq = num(r$cv$ll, "rq"),
  cv_ll_soe = num(r$cv$ll, "soe"),
  cv_ll_ind_floor = num(r$cv$ll_floor, "ind"),
  cv_ll_rq_floor  = num(r$cv$ll_floor, "rq"),
  cv_ll_soe_floor = num(r$cv$ll_floor, "soe"),
  cv_n = r$cv$n_eval %||% NA_integer_,
  cv_folds_failed = r$cv$n_folds_failed %||% NA_integer_,
  cv_heldout_below_floor = r$cv$n_heldout_below_floor %||% NA_integer_,
  share_out = r$share_out %||% NA_real_,
  share_out_floor = r$share_out_floor %||% NA_real_,
  rq_starts = r$rq_conv$n_starts %||% NA_integer_,
  rq_at_best = r$rq_conv$n_at_best %||% NA_integer_,
  flag_warning = r$flags$warned %||% NA,
  flag_nonconv = if (is.null(r$flags)) NA else
    !(isTRUE(r$flags$ind_converged) && isTRUE(r$flags$soe_converged)),
  flag_big_coef = r$flags$big_coef %||% NA,
  max_abs_coef = r$flags$max_abs_coef %||% NA_real_,
  secs = r$secs %||% NA_real_
))
tab_all <- bind_rows(rows)

# per-respondent held-out gaps, in nats: the primary numbers
tab_all <- tab_all |>
  mutate(
    cv_gap_rq_nats  = (cv_ll_rq_floor  - cv_ll_ind_floor) / cv_n,
    cv_gap_soe_nats = (cv_ll_soe_floor - cv_ll_ind_floor) / cv_n,
    share_out_usable = !is.na(cv_gap_soe_nats) & cv_gap_soe_nats > DENOM_MIN,
    flag_any = flag_warning | flag_nonconv | flag_big_coef
  )

if (!is.null(tags) && "construct_type" %in% names(tags))
  tab_all <- left_join(tab_all,
                       distinct(select(tags, table, construct_type, measurement_tool)),
                       by = "table")

write.csv(filter(tab_all, ok), file.path(out_dir,
          if (bigj) "gapshare_bigJ.csv" else "gapshare_tables.csv"), row.names = FALSE)
write.csv(select(tab_all, table, draw, ok, reason, J, N),
          file.path(out_dir, if (bigj) "gapshare_excluded_bigJ.csv" else "gapshare_excluded.csv"),
          row.names = FALSE)
message("Wrote CSVs to ", out_dir)

ok <- filter(tab_all, ok)
message("Usable tables: ", nrow(ok),
        "  |  flagged: ", sum(ok$flag_any, na.rm = TRUE),
        "  |  held-out ratio usable: ", sum(ok$share_out_usable, na.rm = TRUE))
if (nrow(ok)) {
  message("in-sample share   median ", round(median(ok$share_in, na.rm = TRUE), 4),
          "  IQR [", paste(round(quantile(ok$share_in, c(.25, .75), na.rm = TRUE), 4),
                           collapse = ", "), "]")
  u <- filter(ok, share_out_usable)
  if (nrow(u))
    message("held-out share    median ", round(median(u$share_out_floor, na.rm = TRUE), 4),
            "  IQR [", paste(round(quantile(u$share_out_floor, c(.25, .75), na.rm = TRUE), 4),
                             collapse = ", "), "]  (n = ", nrow(u), ")")
}
