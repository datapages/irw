# Stage 1 of datapages/irw#375: when a two-class mixture splits an IRW table,
# is the second class a different item ordering or a compressed copy of the first?
#
# Kaplan-Damary, Oz & Stern (2026) show that a mixture with a fixed number of
# classes cannot separate heterogeneity in *order* from heterogeneity in *scale*,
# so it spends a class on the respondents who answer less consistently. Under a
# Rasch parameterisation scale has only one place to go -- the spread of the item
# difficulties -- which makes the diagnostic clean: regress the less dispersed
# class's difficulties on the more dispersed class's and read off
#
#   scale_ratio  sd of the smaller over sd of the larger; 1 = no compression
#   shared_r2    how much of the second class is a rescaling of the first
#   resid_sd     what is left after rescaling, in logits; the genuine reordering
#
# A raw correlation means nothing on its own, because two independent samples from
# one population do not give identical item parameters either. So every table also
# gets a RANDOM-SPLIT BASELINE: persons partitioned at random into groups of the
# same sizes as the fitted classes, a Rasch model on each, the same three numbers.
# The baseline is what the mixture has to beat; do not read the mixture column
# without it.
#
# Resumable: one row per table appended to fits/mixture_scale_results.csv.
# Per-class item parameters go to fits/items/<table>.csv. Run from vignettes/.

suppressMessages({
  library(irw); library(psychomix); library(data.table)
})

HERE       <- normalizePath(".")
FITS       <- file.path(HERE, "mixture_scale_data", "fits")
ITEMS      <- file.path(FITS, "items")
OUT        <- file.path(FITS, "mixture_scale_results.csv")
METADATA   <- Sys.getenv("IRW_METADATA",
                         path.expand("~/Dropbox/projects/irw/src/metadata/metadata.csv"))
SEED       <- 20261023
MAX_PERSON <- 2000   # raschmix is CML + EM; 2,000 keeps a table to a few minutes
MAX_ITEMS  <- 80     # CML cost grows fast in the number of items; the pilot holds
                     # four tables of 116-379 items, which would dominate the run
NREP       <- 3      # random starts, because mixture likelihoods have local maxima
KMAX       <- 3      # two classes is the paper's setup; K is a choice, so check 3
PILOT_PER_STRATUM <- 7

dir.create(ITEMS, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------- selection ----
eligible <- function() {
  m <- read.csv(METADATA)
  q <- subset(m, n_categories == 2 & n_participants >= 500 & n_items >= 10 &
                density >= 0.9 & n_responses <= 2e6)
  q[order(q$table), ]
}

pilot_tables <- function(q, n_per = PILOT_PER_STRATUM) {
  q$jb <- cut(q$n_items, c(9, 19, 39, Inf), labels = c("10-19", "20-39", "40+"))
  q$nb <- cut(q$n_participants, c(499, 1500, Inf), labels = c("500-1499", "1500+"))
  set.seed(SEED)
  out <- do.call(rbind, lapply(split(q, list(q$jb, q$nb), drop = TRUE), function(g)
    g[sample(nrow(g), min(n_per, nrow(g))), ]))
  out[order(out$table), ]
}

# ------------------------------------------------------------------ prepare ----
# Returns the person x item matrix a Rasch mixture can actually be fitted to,
# with every exclusion counted rather than silently applied.
prepare <- function(tab) {
  d <- as.data.table(irw_fetch(tab)[, c("id", "item", "resp")])
  d <- d[!is.na(resp)]
  if (!all(d$resp %in% c(0, 1))) return(list(err = "resp not 0/1"))
  d <- unique(d, by = c("id", "item"))          # duplicate id-item rows can be real data
  w <- dcast(d, id ~ item, value.var = "resp")
  m <- as.matrix(w[, -1]); rownames(m) <- as.character(w$id)
  n_raw <- nrow(m); j_raw <- ncol(m)

  m <- m[complete.cases(m), , drop = FALSE]     # CML needs complete rows
  n_complete <- nrow(m)
  if (n_complete < 300 || ncol(m) < 10) return(list(err = "too small after complete.cases"))

  # Response direction is not harmonised in IRW. A reverse-keyed item gets a
  # negative rest correlation and would read as a reordering between classes, so
  # align keys first and record how many items were flipped.
  tot <- rowSums(m)
  rr <- vapply(seq_len(ncol(m)), function(j) {
    x <- m[, j]; rest <- tot - x
    if (length(unique(x)) < 2 || length(unique(rest)) < 2) NA_real_ else cor(x, rest)
  }, numeric(1))
  flip <- !is.na(rr) & rr < 0
  m[, flip] <- 1 - m[, flip]

  const <- apply(m, 2, function(x) length(unique(x)) < 2)
  m <- m[, !const, drop = FALSE]
  if (ncol(m) < 10) return(list(err = "too few non-constant items"))

  # CML conditions on the raw score, so all-0 and all-J people carry no
  # information about item parameters and are dropped by the estimator anyway.
  # They are also exactly the people a "flat" class would recruit, so the count
  # is reported, not buried.
  s <- rowSums(m); keep <- s > 0 & s < ncol(m)
  n_extreme <- sum(!keep)
  m <- m[keep, , drop = FALSE]
  if (nrow(m) < 300) return(list(err = "too few non-extreme persons"))

  set.seed(SEED)
  if (nrow(m) > MAX_PERSON) m <- m[sample(nrow(m), MAX_PERSON), , drop = FALSE]
  if (ncol(m) > MAX_ITEMS)  m <- m[, sort(sample(ncol(m), MAX_ITEMS)), drop = FALSE]

  list(m = m, n_raw = n_raw, j_raw = j_raw, n_complete = n_complete,
       n_flipped = sum(flip), n_extreme = n_extreme)
}

# ---------------------------------------------------------------- diagnostic ----
# Two difficulty vectors in, three numbers out. Orientation is by dispersion, so
# the "scale" direction is always the same way round and the slope is comparable
# across tables.
compare_pair <- function(ba, bb) {
  if (sd(ba) == 0 || sd(bb) == 0) return(list(scale_ratio = NA, shared_r2 = NA, resid_sd = NA))
  big <- if (sd(ba) >= sd(bb)) ba else bb
  sml <- if (sd(ba) >= sd(bb)) bb else ba
  fit <- lm(sml ~ big)
  list(scale_ratio = sd(sml) / sd(big),
       shared_r2   = suppressWarnings(cor(big, sml))^2,
       resid_sd    = sd(residuals(fit)))
}

# The baseline. Persons split at random into groups of the same sizes as the
# fitted classes, one Rasch model each: the ordering difference two samples of
# this size and this test length produce when there is no mixture at all.
split_baseline <- function(m, sizes, reps = 3) {
  res <- lapply(seq_len(reps), function(r) {
    set.seed(SEED + r)
    g <- sample(rep(seq_along(sizes), times = sizes))
    bs <- lapply(seq_along(sizes), function(k) {
      mk <- m[g == k, , drop = FALSE]
      s <- rowSums(mk); mk <- mk[s > 0 & s < ncol(mk), , drop = FALSE]
      if (nrow(mk) < 50) return(NULL)
      f <- try(raschmix(mk, k = 1, verbose = FALSE), silent = TRUE)
      if (inherits(f, "try-error")) NULL else as.numeric(itempar(f))
    })
    if (any(vapply(bs, is.null, logical(1)))) return(NULL)
    compare_pair(bs[[1]], bs[[2]])
  })
  res <- Filter(Negate(is.null), res)
  if (!length(res)) return(list(scale_ratio = NA, shared_r2 = NA, resid_sd = NA))
  list(scale_ratio = mean(vapply(res, `[[`, numeric(1), "scale_ratio")),
       shared_r2   = mean(vapply(res, `[[`, numeric(1), "shared_r2")),
       resid_sd    = mean(vapply(res, `[[`, numeric(1), "resid_sd")))
}

# --------------------------------------------------------------------- table ----
run_table <- function(tab) {
  t0 <- Sys.time()
  p <- prepare(tab)
  if (!is.null(p$err)) return(data.frame(table = tab, status = p$err))
  m <- p$m

  f <- raschmix(m, k = 1:KMAX, nrep = NREP, verbose = FALSE)
  bics <- BIC(f)
  k_bic <- as.integer(names(bics)[which.min(bics)])
  m2 <- getModel(f, which = "2")
  ip <- itempar(m2)                       # items x classes
  stopifnot(nrow(ip) == ncol(m), ncol(ip) == 2)
  cl <- clusters(m2)
  sizes <- as.integer(table(factor(cl, levels = 1:2)))

  cmp  <- compare_pair(ip[, 1], ip[, 2])
  base <- split_baseline(m, sizes)

  write.csv(data.frame(item = rownames(ip), b1 = ip[, 1], b2 = ip[, 2]),
            file.path(ITEMS, paste0(tab, ".csv")), row.names = FALSE)

  data.frame(
    table = tab, status = "ok",
    n_raw = p$n_raw, j_raw = p$j_raw, n_complete = p$n_complete,
    n_fitted = nrow(m), j_fitted = ncol(m),
    n_flipped = p$n_flipped, n_extreme = p$n_extreme,
    mean_resp = round(mean(m), 4),
    bic_k1 = bics[["1"]], bic_k2 = bics[["2"]],
    bic_k3 = if ("3" %in% names(bics)) bics[["3"]] else NA_real_,
    k_bic = k_bic,
    smaller_class = min(sizes) / sum(sizes),
    scale_ratio = cmp$scale_ratio, shared_r2 = cmp$shared_r2, resid_sd = cmp$resid_sd,
    base_scale_ratio = base$scale_ratio, base_shared_r2 = base$shared_r2,
    base_resid_sd = base$resid_sd,
    secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  )
}

# ---------------------------------------------------------------------- main ----
main <- function() {
  q <- eligible()
  tabs <- pilot_tables(q)$table
  cat(sprintf("pool %d eligible, pilot %d tables\n", nrow(q), length(tabs)))
  done <- if (file.exists(OUT)) read.csv(OUT)$table else character(0)
  for (tab in setdiff(tabs, done)) {
    cat(sprintf("[%s] %s ... ", format(Sys.time(), "%H:%M:%S"), tab)); flush.console()
    row <- tryCatch(run_table(tab),
                    error = function(e) data.frame(table = tab, status = paste("error:", conditionMessage(e))))
    write.table(row, OUT, sep = ",", row.names = FALSE,
                col.names = !file.exists(OUT), append = file.exists(OUT))
    cat(if (identical(row$status, "ok"))
      sprintf("r2=%.2f (base %.2f) scale=%.2f k_bic=%d %ds\n",
              row$shared_r2, row$base_shared_r2, row$scale_ratio, row$k_bic, row$secs)
      else paste(row$status, "\n"))
  }
  cat("done\n")
}

if (sys.nframe() == 0) main()
