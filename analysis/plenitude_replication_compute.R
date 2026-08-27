## plenitude_replication_compute.R -------------------------------------------
## Replication of the empirical portion of Sotoudeh & DiMaggio (2021),
## "Coping With Plenitude," Sociological Methods & Research 52(4):1838-1882,
## on a stratified sample of IRW tables.
##
## Fits everything and writes cached .rds; skip-if-exists at the
## table x replicate level, so the run is resumable. Parallel via future_map.
##
##   Rscript analysis/plenitude_replication_compute.R
##
## Requires REDIVIS_API_TOKEN for the fetch stage only; once
## analysis/plenitude_data/tables/ is populated the run is offline.

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(furrr); library(future)
})
suppressWarnings(source("analysis/plenitude_lib.R"))
source("analysis/plenitude_data_prep.R")

OUT      <- "analysis/plenitude_data"
FITDIR   <- file.path(OUT, "fits")
N_CAP    <- 2000L
N_REPS   <- 3L
SEEDS    <- c(101L, 202L, 303L)      # one per replicate, recorded
WORKERS  <- as.integer(Sys.getenv("PLENITUDE_WORKERS", "16"))
SEM_TIME <- 900                       # seconds; guard against lavaan hanging
dir.create(FITDIR, showWarnings = FALSE, recursive = TRUE)

models <- readRDS("analysis/vendor/metafeature_models_cleaned.RDS")
corpus <- read.csv(file.path(OUT, "corpus.csv"), stringsAsFactors = FALSE)

## ---------------------------------------------------------------------------
## Reverse-keying diagnostic.
## Polarity score: each respondent's agreement with the sign of the first
## principal component's item loadings. eta^2 is the share of that score's
## variance explained by class membership -- i.e. how much of the recovered
## partition is nothing more than which way a respondent leans on PC1.
reverse_key_eta2 <- function(m, cl) {
  if (length(unique(cl)) < 2) return(NA_real_)
  z <- scale(m)
  ld <- prcomp(m, scale. = TRUE)$rotation[, 1]
  s <- as.numeric(z %*% sign(ld))
  ss_tot <- sum((s - mean(s))^2)
  if (ss_tot <= 0) return(NA_real_)
  ss_bet <- sum(tapply(s, cl, function(x) length(x) * (mean(x) - mean(s))^2))
  ss_bet / ss_tot
}

## ---------------------------------------------------------------------------
## Their five internal validity criteria.
## Pearson's Gamma, within-class SS, within-between ratio and Calinski-Harabasz
## come from fpc::cluster.stats on a distance matrix built from ABSOLUTE
## response correlations -- their construction, and the reason these criteria
## lean toward correlation, cosine and ACE (their own caveat, p.1866).
## The fifth is the AIC improvement of a class-grouped saturated covariance
## model over the pooled one.
validity_criteria <- function(m, cl, covmodel, semdat, overall_aic) {
  cs <- try(fpc::cluster.stats(abs(cor(t(m))), clustering = cl), silent = TRUE)
  out <- data.frame(pearsongamma = NA_real_, within_ss = NA_real_,
                    wb_ratio = NA_real_, ch = NA_real_,
                    aic_improvement = NA_real_, aic_improvement_trimmed = NA_real_,
                    aic_status = NA_character_)
  if (!inherits(cs, "try-error")) {
    out$pearsongamma <- abs(cs$pearsongamma)
    out$within_ss    <- cs$within.cluster.ss
    out$wb_ratio     <- cs$wb.ratio
    out$ch           <- abs(cs$ch)
  }

  fit_group <- function(dat, grp) {
    dat$.grp <- grp
    setTimeLimit(elapsed = SEM_TIME, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)
    suppressWarnings(try(lavaan::sem(covmodel, data = dat, group = ".grp",
                                     check.gradient = FALSE), silent = TRUE))
  }

  ## as they do it: all classes, try() -> NA on failure
  g <- fit_group(semdat, cl)
  if (!inherits(g, "try-error") && lavaan::lavInspect(g, "converged")) {
    out$aic_improvement <- overall_aic - AIC(g)
    out$aic_status <- "ok"
  } else {
    out$aic_status <- "failed"
  }

  ## trimmed variant: a saturated covariance model is not identified in a class
  ## with fewer members than items, and these methods routinely return such
  ## classes. Pooling every class smaller than p+1 into one keeps the criterion
  ## computable. Reported alongside, never in place of, the untrimmed version.
  p <- ncol(m); tb <- table(cl)
  small <- names(tb)[tb < (p + 1)]
  if (length(small) > 0 && length(setdiff(names(tb), small)) >= 2) {
    cl2 <- ifelse(as.character(cl) %in% small, "pooled_small", as.character(cl))
    if (sum(cl2 == "pooled_small") >= (p + 1)) {
      g2 <- fit_group(semdat, cl2)
      if (!inherits(g2, "try-error") && lavaan::lavInspect(g2, "converged"))
        out$aic_improvement_trimmed <- overall_aic - AIC(g2)
    }
  } else if (out$aic_status == "ok") {
    out$aic_improvement_trimmed <- out$aic_improvement
  }
  out
}

## ---------------------------------------------------------------------------
## One table x replicate.
## Workers get a fresh R session; the Rcpp functions (f_outer, relationalityC)
## are external pointers and cannot be serialized across, so each worker sources
## the library itself, once.
.worker_ready <- FALSE
worker_init <- function() {
  if (isTRUE(get0(".worker_ready", ifnotfound = FALSE))) return(invisible())
  suppressWarnings(source("analysis/plenitude_lib.R"))
  source("analysis/plenitude_data_prep.R")
  assign(".worker_ready", TRUE, envir = globalenv())
  invisible()
}

run_one <- function(tbl, rep_i) {
  f <- file.path(FITDIR, sprintf("%s__rep%d.rds", tbl, rep_i))
  if (file.exists(f)) return(f)
  worker_init()
  seed <- SEEDS[rep_i]

  m0 <- try(get_matrix(tbl), silent = TRUE)
  if (inherits(m0, "try-error") || is.null(dim(m0)) || ncol(m0) < 3 || nrow(m0) < 50) {
    saveRDS(list(table = tbl, replicate = rep_i, status = "no_data"), f); return(f)
  }
  m <- subsample(m0, N_CAP, seed = seed)
  p <- ncol(m); N <- nrow(m)

  ## --- meta-features (CARRY-FORWARD 1: num_vars passed explicitly) ----------
  mf <- try(evaluate_metafeatures(m, num_vars = p, seed = seed), silent = TRUE)
  if (inherits(mf, "try-error")) {
    saveRDS(list(table = tbl, replicate = rep_i, status = "metafeature_error"), f); return(f)
  }

  ## --- predicted accuracy, four readings ------------------------------------
  ## primary  : name-aligned coefficients, released PercentOut, released OverallRightKurt
  ## pct_cells: PercentOut swapped for the cell-outlier proportion (CARRY-FORWARD 2)
  ## rk_corr  : OverallRightKurt swapped for right_half_kurt      (CARRY-FORWARD 3)
  ## positional: their make_prediction() column ordering          (CARRY-FORWARD 5)
  mf_cells <- mf; mf_cells$PercentOut <- mf$PercentOut_cells
  mf_rk    <- mf; mf_rk$OverallRightKurt <- mf$OverallRightKurt_corrected
  pred <- bind_rows(
    data.frame(reading = "primary",    method = names(models), predicted = predict_accuracy(models, mf,       "name")),
    data.frame(reading = "pct_cells",  method = names(models), predicted = predict_accuracy(models, mf_cells, "name")),
    data.frame(reading = "rk_corrected", method = names(models), predicted = predict_accuracy(models, mf_rk,  "name")),
    data.frame(reading = "positional", method = names(models), predicted = predict_accuracy(models, mf,       "positional")))

  ## --- the nine measures + the ensemble -------------------------------------
  memb <- list(); timing <- list()
  for (meas in MEASURES) {
    t0 <- proc.time()[["elapsed"]]
    r <- try(metaRCA(m, meas, seed = seed), silent = TRUE)
    timing[[meas]] <- proc.time()[["elapsed"]] - t0
    memb[[meas]] <- if (inherits(r, "try-error")) NULL else r$membership
  }
  ok_ens <- ENSEMBLE_MEMBERS[!vapply(memb[ENSEMBLE_MEMBERS], is.null, logical(1))]
  if (length(ok_ens) >= 2) {
    t0 <- proc.time()[["elapsed"]]
    e <- try(dyadic_vote_ensemble(memb[ok_ens], seed = seed), silent = TRUE)
    timing[["Ensemble"]] <- proc.time()[["elapsed"]] - t0
    memb[["Ensemble"]] <- if (inherits(e, "try-error")) NULL else e
  }

  ## --- pooled saturated covariance model, fitted once ------------------------
  vc <- t(combn(colnames(m), 2))
  covmodel <- paste(vc[, 1], " ~~ ", vc[, 2], ";", sep = "")
  semdat <- as.data.frame(m)
  o <- suppressWarnings(try(lavaan::sem(covmodel, data = semdat, check.gradient = FALSE), silent = TRUE))
  overall_aic <- if (!inherits(o, "try-error")) AIC(o) else NA_real_

  ## --- solutions, validity, reverse-keying ----------------------------------
  sol <- bind_rows(lapply(names(memb), function(meas) {
    cl <- memb[[meas]]
    if (is.null(cl)) return(data.frame(method = meas, status = "error", k = NA_integer_,
                                       largest_prop = NA_real_, n_substantive = NA_integer_,
                                       secs = timing[[meas]], degenerate = NA))
    tb <- table(cl)
    data.frame(method = meas, status = "ok", k = length(tb),
               largest_prop = max(tb) / length(cl),
               n_substantive = sum(tb >= 0.05 * length(cl)),
               secs = timing[[meas]],
               degenerate = length(tb) == 1 || max(tb) / length(cl) > 0.90)
  }))

  val <- bind_rows(lapply(names(memb), function(meas) {
    cl <- memb[[meas]]
    if (is.null(cl) || length(unique(cl)) < 2)
      return(cbind(data.frame(method = meas), validity_criteria(m, rep(1, nrow(m)), covmodel, semdat, NA_real_)[0, ]))
    cbind(data.frame(method = meas), validity_criteria(m, cl, covmodel, semdat, overall_aic))
  }))

  rk <- bind_rows(lapply(names(memb), function(meas) {
    cl <- memb[[meas]]
    data.frame(method = meas,
               eta2 = if (is.null(cl)) NA_real_ else reverse_key_eta2(m, cl))
  }))

  res <- list(
    table = tbl, replicate = rep_i, seed = seed, status = "ok",
    n_items = p, n_resp = N, n_resp_full = nrow(m0), N_capped = nrow(m0) > N_CAP,
    metafeatures = mf, predicted = pred, solutions = sol,
    validity = val, reverse_key = rk, membership = memb,
    overall_aic = overall_aic)
  saveRDS(res, f)
  f
}

## ---------------------------------------------------------------------------
## Fetch serially first (Redivis is the bottleneck and is not thread-safe here),
## then fit in parallel. Cheapest tables first so the cache fills early.
if (!identical(Sys.getenv("PLENITUDE_SKIP_FETCH"), "1")) {
  for (tbl in corpus$table) {
    if (file.exists(file.path(CACHE, paste0(tbl, ".rds")))) next
    message("fetch: ", tbl)
    try(get_matrix(tbl), silent = TRUE)
  }
}

corpus$cost <- pmin(corpus$n_participants, N_CAP)^2 * corpus$n_items^4
jobs <- expand.grid(table = corpus$table, rep_i = seq_len(N_REPS),
                    stringsAsFactors = FALSE) %>%
  left_join(corpus[, c("table", "cost")], by = "table") %>%
  arrange(cost)

plan(multisession, workers = WORKERS)
message("fitting ", nrow(jobs), " table x replicate jobs on ", WORKERS, " workers")
future_walk2(jobs$table, jobs$rep_i, run_one,
             .options = furrr_options(seed = TRUE,
               globals = c("FITDIR","CACHE","SEEDS","N_CAP","SEM_TIME","models",
                           "worker_init","reverse_key_eta2","validity_criteria")),
             .progress = TRUE)
plan(sequential)

## ---------------------------------------------------------------------------
## Collate to tidy CSVs.
source("analysis/plenitude_collate.R")
