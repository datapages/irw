# Stage 2 of datapages/irw#375: the fair null for the Stage 1 diagnostic.
#
# Stage 1 compared each fitted two-class mixture against a RANDOM SPLIT of the
# same people. That null is too weak to carry the conclusion. A random split
# optimises nothing, so two halves of a homogeneous table necessarily look alike
# (shared_r2 ~ 0.98 across the pilot); a mixture, by contrast, is choosing the
# partition that maximises the likelihood, and on perfectly homogeneous data it
# will still find *some* partition whose two item-difficulty vectors disagree.
# The question Stage 1 cannot answer is how much.
#
# So: fit a ONE-CLASS Rasch model to the real table, simulate data from it, fit
# the same two-class mixture to the simulated data, and compute the same three
# numbers. Whatever the mixture manufactures from data with no mixture in it is
# the floor the real tables have to clear.
#
# The simulation also refits k = 1:KMAX, which tests the other Stage 1 finding
# directly: if BIC prefers k = 3 on data generated from a single Rasch model,
# then "BIC wants three classes" says nothing about the corpus.
#
# Ability distribution: thetas are estimated by ML from the raw scores given the
# CML difficulties, then the sampling variance is removed by moments before
# simulating -- var(theta_hat) overstates var(theta) by the mean squared standard
# error, and simulating from the raw spread would hand the mixture a wider
# ability range than the table actually has.
#
# Resumable: one row per (table, replicate) appended to fits/bootstrap_results.csv.
# Prepared matrices are cached under fits/prep/ so a resume does not refetch.
# Run from vignettes/, single-threaded, under `capped`.

suppressMessages({
  library(irw); library(psychomix); library(data.table)
})

source("mixture_scale_compute.R")          # prepare(), rasch_b(), compare_pair()

PREP  <- file.path(FITS, "prep")
BOUT  <- file.path(FITS, "bootstrap_results.csv")
NBOOT <- as.integer(Sys.getenv("NBOOT", "5"))

dir.create(PREP, recursive = TRUE, showWarnings = FALSE)

# Stage 1 fetched and cleaned each table; do that once more and keep it, so the
# bootstrap replicates and any rerun work from the identical matrix.
prepare_cached <- function(tab) {
  f <- file.path(PREP, paste0(tab, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  p <- prepare(tab)
  saveRDS(p, f)
  p
}

# ML ability for each attainable raw score, given the item difficulties. Scores
# are conditioned on in CML, so there are at most J-1 distinct values to solve.
mle_theta <- function(scores, b) {
  s_u <- sort(unique(scores))
  tab <- vapply(s_u, function(s) {
    g <- function(th) sum(plogis(th - b)) - s
    if (g(-8) > 0 || g(8) < 0) return(NA_real_)
    uniroot(g, c(-8, 8), tol = 1e-6)$root
  }, numeric(1))
  names(tab) <- as.character(s_u)
  tab[as.character(scores)]
}

# One simulated table from a single-class Rasch fit to m.
simulate_rasch <- function(m, b, mu, sigma) {
  th <- rnorm(nrow(m), mu, sigma)
  p  <- plogis(outer(th, b, "-"))
  x  <- matrix(rbinom(length(p), 1, p), nrow(p), ncol(p),
               dimnames = list(rownames(m), names(b)))
  x
}

# The same cleaning the real matrices got, so the two are comparable: constant
# items and extreme scorers removed to a fixed point.
clean_sim <- function(x) {
  repeat {
    const <- apply(x, 2, function(v) length(unique(v)) < 2)
    if (any(const)) x <- x[, !const, drop = FALSE]
    if (ncol(x) < 10) return(NULL)
    s <- rowSums(x); keep <- s > 0 & s < ncol(x)
    if (all(keep) && !any(const)) break
    x <- x[keep, , drop = FALSE]
    if (nrow(x) < 300) return(NULL)
  }
  x
}

# ------------------------------------------------------------------- table ----
boot_table <- function(tab, reps = NBOOT) {
  p <- prepare_cached(tab)
  if (!is.null(p$err)) return(data.frame(table = tab, rep = NA_integer_, status = p$err))
  m <- p$m

  f1 <- raschmix(m, k = 1, verbose = FALSE)
  pr <- rasch_b(f1)
  b  <- setNames(pr[, 1], sub("^item\\.", "", rownames(pr)))
  b  <- b[is.finite(b)]
  if (length(b) < 10) return(data.frame(table = tab, rep = NA_integer_,
                                        status = "too few estimable items at k=1"))
  m <- m[, names(b), drop = FALSE]

  # abilities, with the estimation variance taken back out
  s  <- rowSums(m)
  th <- mle_theta(s, b)
  ok <- is.finite(th)
  se2 <- vapply(th[ok], function(t) { q <- plogis(t - b); 1 / sum(q * (1 - q)) }, numeric(1))
  mu    <- mean(th[ok])
  sigma <- sqrt(max(var(th[ok]) - mean(se2), 0.04))

  # Each replicate is guarded on its own. A single bad simulated draw -- a
  # k = 2 component that fails to come back from getModel(), say -- must cost
  # that replicate and not the table's other four.
  out <- lapply(seq_len(reps), function(r) tryCatch({
    t0 <- Sys.time()
    set.seed(SEED + 1000 * r + nchar(tab))
    x <- clean_sim(simulate_rasch(m, b, mu, sigma))
    if (is.null(x)) return(data.frame(table = tab, rep = r, status = "simulated table degenerate"))
    f <- try(raschmix(x, k = 1:KMAX, nrep = NREP, verbose = FALSE), silent = TRUE)
    if (inherits(f, "try-error"))
      return(data.frame(table = tab, rep = r, status = paste("error:", conditionMessage(attr(f, "condition")))))
    bics  <- BIC(f)
    k_bic <- as.integer(names(bics)[which.min(bics)])
    m2 <- getModel(f, which = "2")
    ip <- rasch_b(m2)
    rownames(ip) <- sub("^item\\.", "", rownames(ip))
    sizes <- as.integer(table(factor(clusters(m2), levels = 1:2)))
    cmp <- compare_pair(setNames(ip[, 1], rownames(ip)), setNames(ip[, 2], rownames(ip)))
    data.frame(table = tab, rep = r, status = "ok",
               n_sim = nrow(x), j_sim = ncol(x),
               theta_sd = round(sigma, 4), b_sd = round(sd(b), 4),
               bic_k1 = bics[["1"]], bic_k2 = bics[["2"]],
               bic_k3 = if ("3" %in% names(bics)) bics[["3"]] else NA_real_,
               k_bic = k_bic,
               smaller_class = min(sizes) / sum(sizes),
               sim_scale_ratio = cmp$scale_ratio, sim_shared_r2 = cmp$shared_r2,
               sim_resid_sd = cmp$resid_sd, sim_j_used = cmp$j_used,
               secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }, error = function(e) data.frame(table = tab, rep = r,
                                    status = paste("error:", conditionMessage(e)))))
  rbindlist(out, fill = TRUE)
}

# -------------------------------------------------------------------- main ----
boot_main <- function() {
  st <- read.csv(OUT)
  tabs <- st$table[st$status == "ok"]
  cat(sprintf("%d tables x %d replicates\n", length(tabs), NBOOT))
  done <- if (file.exists(BOUT)) unique(read.csv(BOUT)$table) else character(0)
  for (tab in setdiff(tabs, done)) {
    cat(sprintf("[%s] %s ... ", format(Sys.time(), "%H:%M:%S"), tab)); flush.console()
    rows <- tryCatch(boot_table(tab),
                     error = function(e) data.frame(table = tab, rep = NA_integer_,
                                                    status = paste("error:", conditionMessage(e))))
    write.table(rows, BOUT, sep = ",", row.names = FALSE,
                col.names = !file.exists(BOUT), append = file.exists(BOUT))
    okr <- rows[!is.na(rows$status) & rows$status == "ok", ]
    cat(if (nrow(okr))
      sprintf("sim r2=%.2f scale=%.2f resid=%.2f  k_bic=%s  %ds\n",
              mean(okr$sim_shared_r2, na.rm = TRUE), mean(okr$sim_scale_ratio, na.rm = TRUE),
              mean(okr$sim_resid_sd, na.rm = TRUE),
              paste(okr$k_bic, collapse = ""), sum(okr$secs))
      else paste(unique(rows$status), "\n"))
  }
  cat("done\n")
}

if (sys.nframe() == 0) boot_main()
