# speed_accuracy_copula_compute.R
#
# Kwon & Zhang (2026, Psychometrika) replace the bivariate-normal person prior
# of the van der Linden (2007) hierarchical speed-accuracy model with a copula,
# and report that a 90-degree-rotated Joe copula beats the MVN on 28 of 30
# PISA 2018 splits -- read as evidence for asymmetric tail dependence between
# latent ability and latent speed in large-scale educational assessment. Their
# non-PISA evidence is six small cognitive-training datasets. This script asks
# whether that asymmetry is a property of item response data generally or a
# property of PISA, by running a deliberately conservative two-stage screen
# across every IRW table that can support one: fit the plain MVN hierarchical
# model per table (LNIRT), then characterise the empirical copula of the
# resulting (theta-hat, tau-hat) person estimates against the same family set
# the paper selects over.
#
# STAGE A ONLY. This is not their estimator. Joint copula estimation (Stage B)
# is deliberately out of scope -- see the issue thread.
#
# The two-stage design biases the answer in a known direction, which is why it
# is worth running: (theta-hat, tau-hat) are shrunken posterior means, and
# shrinkage pulls the joint distribution toward its centre, biasing estimated
# tail dependence TOWARD ZERO. Asymmetry this screen finds is credible;
# absence of asymmetry is weak evidence.
#
# Produces the precomputed results loaded by speed_accuracy_copula.qmd.
#
# Output: speed_accuracy_copula_data/speed_accuracy_copula_results.rds
#         speed_accuracy_copula_data/references.bib
#
# Usage:
#   nice -n 19 Rscript vignettes/speed_accuracy_copula_compute.R   # from project root

# Single-threaded BLAS: workers are capped low on purpose (this runs on a
# 20-core working desktop, not a compute box) and nested threading there
# produces load spikes without finishing sooner.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")

library(irw)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)
library(LNIRT)       # MVN hierarchical speed-accuracy model (van der Linden 2007)
library(VineCopula)  # bivariate copula selection by AIC over the paper's families
library(copula)      # exchangeability and radial-symmetry tests, pseudo-observations

set.seed(20261022)

out_dir  <- "vignettes/speed_accuracy_copula_data"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
bib_file <- file.path(out_dir, "references.bib")

# ==============================================================================
# Thresholds. Every one of these traces to a stated reason; see the Data and
# methods section of the .qmd, which reports the attrition they cause.
# ==============================================================================

MIN_ITEMS        <- 10      # a person-level speed/ability estimate needs some items
MIN_PARTICIPANTS <- 300     # tail dependence is a tail claim; small N cannot speak to it
MAX_ITEMS        <- 500     # LNIRT ceiling; wider tables get the top-MAX_ITEMS treatment
MAX_N            <- 5000    # downsample persons before fitting (compute, not statistics)
MAX_ROWS         <- 5e6     # refuse to fetch tables larger than this

MIN_RESP_PER_ITEM   <- 50   # item parameters feed theta-hat; thin items poison it
PERSON_FLOOR_SWEEP  <- c(10, 20, 40)  # headline is the first; the rest are the
PERSON_FLOOR        <- PERSON_FLOOR_SWEEP[1]  # sensitivity check the brief asks for

XG     <- 1000  # MCMC iterations for LNIRT
BURNIN <- 10    # percent discarded

SYMTEST_MAX  <- 2000  # symmetry tests use the full sample up to this size
SYMTEST_REPS <- 1000  # bootstrap replicates for the symmetry tests

# Kwon & Zhang select over nine families. These VineCopula codes cover all of
# them -- independence, Gaussian, Student t, and Gumbel and Joe in all four
# rotations, including the 90-degree-rotated Joe (26) that wins on their PISA
# splits.
FAMILY_SET <- c(0, 1, 2, 4, 6, 14, 16, 24, 26, 34, 36)
JOE_90     <- 26

PILOT          <- TRUE
PILOT_TABLES   <- c(
  # Deliberately spans the structural range, and two are known-goods.
  "nomt_hooper_2024_study2",     # one of Kwon & Zhang's own six datasets: direct bridge
  "credentialform_lnirt",        # the LNIRT package's canonical dataset: known-good MVN fit
  "blum_2018_imak_bin",          # smallest complete table: fast smoke test
  "rapm_poulton_2022_untimed",   # density 2.0 via `wave`: exercises the collapsing rule
  "zorowitz_2023_marsib",        # 384 items at density 0.04: sparsity gate at its edge
  "cifar10h"                     # crowdsourced image labels: maximally un-test-like
)

# ==============================================================================
# 1. Select datasets
#
# Built from the metadata directly rather than irw_filter() alone, because the
# two gates that matter here -- repeated trials per id-item, and sparsity that
# makes the person estimates noise -- are density and responses-per-unit
# facts that irw_filter() does not express.
# ==============================================================================

meta <- irw_metadata()

has_rt <- vapply(strsplit(meta$variables %||% "", "|", fixed = TRUE),
                 function(v) "rt" %in% trimws(v), logical(1))

pool <- meta[has_rt &
             !is.na(meta$n_categories) & meta$n_categories == 2 &
             !is.na(meta$n_participants) & meta$n_participants >= MIN_PARTICIPANTS &
             !is.na(meta$n_items) & meta$n_items >= MIN_ITEMS, ]

message("Tables with rt: ", sum(has_rt),
        " | binary, N >= ", MIN_PARTICIPANTS, ", J >= ", MIN_ITEMS, ": ", nrow(pool))

# Gate on size and on the thin-item problem here, where it is cheap. The
# trial-structure gate needs the actual keys, so it runs per table below.
eligible <- pool[pool$n_responses <= MAX_ROWS &
                 pool$responses_per_item >= MIN_RESP_PER_ITEM, ]

all_candidates   <- eligible$table
n_all_candidates <- nrow(pool)

gate_log <- tibble(
  table  = pool$table,
  reason = case_when(
    pool$n_responses > MAX_ROWS                      ~ "too large to fetch",
    pool$responses_per_item < MIN_RESP_PER_ITEM      ~ "fewer than 50 responses per item",
    TRUE                                             ~ "passed metadata gates"
  )
)

message("Candidate tables after metadata gates: ", length(all_candidates))

if (PILOT) {
  tables <- intersect(PILOT_TABLES, all_candidates)
  missing_pilot <- setdiff(PILOT_TABLES, all_candidates)
  if (length(missing_pilot)) {
    message("PILOT: these picks did not survive the metadata gates and are being ",
            "run anyway so the pilot can report why: ",
            paste(missing_pilot, collapse = ", "))
    tables <- c(tables, missing_pilot)
  }
  message("PILOT run: ", length(tables), " tables.")
} else {
  tables <- all_candidates
}

tags_meta <- tryCatch(irw_tags(tables = tables), error = function(e) {
  message("irw_tags() failed: ", conditionMessage(e))
  NULL
})

# ==============================================================================
# 2. Per-table computation
# ==============================================================================

# Nonparametric tail-dependence coefficients, all four corners, at
# k = floor(sqrt(n)) (Schmidt & Stadtmueller 2006). Model-free, so they do not
# inherit the winning family's shape assumption.
#
# All four corners, not just the diagonal, because ability and speed are
# NEGATIVELY dependent on most of these tables. The classical lambda_L and
# lambda_U measure the low-low and high-high corners, which under negative
# dependence are empty by construction -- reporting them alone measures the
# two corners guaranteed to hold nothing. The substantive quantity is the
# OFF-diagonal pair:
#
#   lambda_LU = P(speed high | ability low)  -- fast and wrong
#   lambda_UL = P(speed low  | ability high) -- slow and able
#
# and the asymmetry that Kwon & Zhang's rotated Joe encodes is precisely
# lambda_LU != lambda_UL. A Gaussian or t copula forces them equal.
emp_taildep <- function(u, v) {
  n <- length(u)
  k <- max(2L, floor(sqrt(n)))
  p <- k / n
  Cn <- function(a, b) mean(u <= a & v <= b)
  c(lambda_LL_emp = Cn(p, p) / p,
    lambda_UU_emp = (2 * p - 1 + Cn(1 - p, 1 - p)) / p,
    lambda_LU_emp = (p - Cn(p, 1 - p)) / p,
    lambda_UL_emp = (p - Cn(1 - p, p)) / p,
    taildep_k     = k)
}

# Tail dependence implied by the SELECTED family, placed in the right corner.
# BiCopPar2TailDep reports the rotated copula's diagonal corners, which are
# zero for the 90- and 270-degree rotations by construction, so it cannot
# express what those families actually claim. A 90-degree rotation moves the
# base family's upper tail into the (low u, high v) corner; 270 degrees moves
# it into (high u, low v). Verified against BiCopSim for Joe and Gumbel.
fit_taildep <- function(fam, par, par2) {
  base <- fam %% 10L
  zero <- c(lambda_LU_fit = 0, lambda_UL_fit = 0)
  if (fam %in% c(24L, 26L, 34L, 36L)) {
    bu <- tryCatch(VineCopula::BiCopPar2TailDep(base, abs(par), par2)$upper,
                   error = function(e) NA_real_)
    if (fam %in% c(24L, 26L)) return(c(lambda_LU_fit = bu, lambda_UL_fit = 0))
    return(c(lambda_LU_fit = 0, lambda_UL_fit = bu))
  }
  # Student t with negative rho carries tail dependence in BOTH off-diagonal
  # corners, equally -- it is a symmetric alternative, which is the whole
  # point of the comparison.
  if (fam == 2L && is.finite(par) && par < 0) {
    td <- tryCatch(VineCopula::BiCopPar2TailDep(2, abs(par), par2)$upper,
                   error = function(e) NA_real_)
    return(c(lambda_LU_fit = td, lambda_UL_fit = td))
  }
  zero
}

# One response and one RT per person-item, or an explicit reason why not.
# Density far above 1 means the id-item key repeats. Sometimes that is
# longitudinal (`wave` resolves it and keeping the first wave is a real
# collapsing rule); sometimes `item` names a stimulus TYPE presented many
# times, and then there is no person-by-item matrix to model at all.
resolve_trials <- function(df, table_name) {
  if (sum(duplicated(df[, c("id", "item")])) == 0) {
    return(list(df = df, note = "one response per person-item", collapsed = FALSE))
  }
  if ("wave" %in% names(df)) {
    w1 <- df[!is.na(df$wave) & df$wave == min(df$wave, na.rm = TRUE), ]
    if (nrow(w1) > 0 && sum(duplicated(w1[, c("id", "item")])) == 0) {
      return(list(df = w1,
                  note = paste0("collapsed to first of ",
                                length(unique(df$wave)), " waves"),
                  collapsed = TRUE))
    }
  }
  list(df = NULL,
       note = "repeated trials per person-item; `item` is a stimulus type, not an item",
       collapsed = FALSE)
}

characterise_copula <- function(theta, speed, floor_used, n_items_fit, table_label) {
  ok <- is.finite(theta) & is.finite(speed)
  theta <- theta[ok]; speed <- speed[ok]
  n <- length(theta)
  if (n < 50) return(NULL)

  U <- copula::pobs(cbind(theta = theta, speed = speed))
  u <- U[, 1]; v <- U[, 2]

  tau_emp <- suppressWarnings(cor(theta, speed, method = "kendall"))

  # Tie diagnostic, and it is not cosmetic. On a short complete test, theta-hat
  # is very nearly a monotone function of the sum score, so it takes only about
  # J+1 distinct values while speed-hat stays continuous. The empirical copula
  # is then a lattice of vertical stripes, and a rotated family can win on that
  # structure rather than on any real tail behaviour. Any table whose
  # theta_distinct_frac is small should be read as suggestive at best.
  theta_distinct_frac <- length(unique(theta)) / n
  speed_distinct_frac <- length(unique(speed)) / n

  # Counting distinct values is too weak a test: MCMC noise makes every
  # posterior mean numerically unique while the values still sit in a tight
  # lattice around the J+1 possible sum scores. On rapm_poulton (12 items)
  # 98.9% of theta-hat's variance is BETWEEN 13 clusters. So measure the
  # lattice directly, and then check the asymmetry against it by jittering.
  lattice_share <- NA_real_
  if (n_items_fit <= 50 && n > (n_items_fit + 1)) {
    lattice_share <- tryCatch({
      km <- kmeans(theta, centers = n_items_fit + 1, nstart = 5)
      km$tot.withinss / km$totss   # near 0 = a lattice, near 1 = continuous
    }, error = function(e) NA_real_)
  }

  # Does the asymmetry survive smearing theta-hat across the lattice? If it is
  # an artifact of discreteness, jitter destroys it; if it is real, it does not.
  asym_raw <- function(th, sp) {
    nn <- length(th); kk <- max(2L, floor(sqrt(nn))); pp <- kk / nn
    uu <- rank(th, ties.method = "random") / (nn + 1)
    vv <- rank(sp, ties.method = "random") / (nn + 1)
    Cnn <- function(a, b) mean(uu <= a & vv <= b)
    ((pp - Cnn(pp, 1 - pp)) / pp) - ((pp - Cnn(1 - pp, pp)) / pp)
  }
  sd_theta  <- stats::sd(theta)
  asym_jitter <- tryCatch(
    mean(replicate(20, asym_raw(theta + stats::rnorm(n, 0, 0.10 * sd_theta), speed))),
    error = function(e) NA_real_)

  sel <- tryCatch(
    VineCopula::BiCopSelect(u, v, familyset = FAMILY_SET,
                            selectioncrit = "AIC", rotations = FALSE),
    error = function(e) { message("    BiCopSelect failed: ", conditionMessage(e)); NULL })
  if (is.null(sel)) return(NULL)

  # Full AIC table over the family set, so the page can show the margin the
  # winner wins by rather than only the winner. Families that cannot represent
  # the observed dependence sign fail here; that is informative, not an error.
  aic_tbl <- map_dfr(FAMILY_SET, function(fam) {
    f <- tryCatch(VineCopula::BiCopEst(u, v, family = fam, method = "mle"),
                  error = function(e) NULL)
    if (is.null(f)) return(tibble(family = fam, family_name = NA_character_,
                                  logLik = NA_real_, AIC = NA_real_))
    tibble(family = fam, family_name = VineCopula::BiCopName(fam, short = FALSE),
           logLik = f$logLik, AIC = f$AIC)
  })

  aic_gauss <- aic_tbl$AIC[aic_tbl$family == 1]
  aic_indep <- aic_tbl$AIC[aic_tbl$family == 0]

  td  <- tryCatch(VineCopula::BiCopPar2TailDep(sel), error = function(e) list(lower = NA, upper = NA))
  ftd <- fit_taildep(sel$family, sel$par, sel$par2)
  emp <- emp_taildep(u, v)

  # The symmetry tests are bootstrap-based and scale badly. Radial symmetry is
  # the one that bears on the paper's claim -- an MVN person prior is radially
  # symmetric by construction, so rejecting it rejects the MVN's shape.
  #
  # Use the whole sample up to SYMTEST_MAX. Beyond it, subsample from a seed
  # derived from the table name rather than from the ambient RNG stream: an
  # earlier version drew 1000 of 1636 off the shared stream, and
  # credentialform_lnirt's p moved from 0.26 to 0.04 between two runs that
  # differed only in how much unrelated code had drawn before it.
  sub <- if (n > SYMTEST_MAX) {
    local({
      old_seed <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
      set.seed(sum(utf8ToInt(table_label)) + floor_used)
      idx <- sample.int(n, SYMTEST_MAX)
      if (!is.null(old_seed)) assign(".Random.seed", old_seed, .GlobalEnv)
      U[idx, ]
    })
  } else U
  # 1000 bootstrap replicates, not 200. These p-values carry Monte Carlo error
  # of their own -- at N = 200 credentialform_lnirt's radial-symmetry p ranged
  # over 0.012-0.027 across identical reruns, and at N = 1000 it settles to
  # about 0.02 +/- 0.005. Read which side of a threshold they fall on, never
  # the third digit.
  p_exch <- tryCatch(copula::exchTest(sub, N = SYMTEST_REPS)$p.value,
                     error = function(e) NA_real_)
  p_rad  <- tryCatch(copula::radSymTest(sub, N = SYMTEST_REPS)$p.value,
                     error = function(e) NA_real_)

  tibble(
    person_floor        = floor_used,
    n_persons_copula    = n,
    tau_emp             = tau_emp,
    theta_distinct_frac = theta_distinct_frac,
    speed_distinct_frac = speed_distinct_frac,
    n_theta_distinct    = length(unique(theta)),
    lattice_share       = lattice_share,
    asym_jitter         = asym_jitter,
    best_family      = sel$family,
    best_family_name = VineCopula::BiCopName(sel$family, short = FALSE),
    best_par         = sel$par,
    best_par2        = sel$par2,
    is_joe90         = sel$family == JOE_90,
    lambda_LL_fit    = td$lower %||% NA_real_,
    lambda_UU_fit    = td$upper %||% NA_real_,
    lambda_LU_fit    = unname(ftd["lambda_LU_fit"]),
    lambda_UL_fit    = unname(ftd["lambda_UL_fit"]),
    asym_fit         = unname(ftd["lambda_LU_fit"] - ftd["lambda_UL_fit"]),
    lambda_LL_emp    = unname(emp["lambda_LL_emp"]),
    lambda_UU_emp    = unname(emp["lambda_UU_emp"]),
    lambda_LU_emp    = unname(emp["lambda_LU_emp"]),
    lambda_UL_emp    = unname(emp["lambda_UL_emp"]),
    # Positive means the fast-and-wrong corner is the sharper one.
    asym_emp         = unname(emp["lambda_LU_emp"] - emp["lambda_UL_emp"]),
    taildep_k        = unname(emp["taildep_k"]),
    aic_best         = sel$AIC,
    aic_gaussian     = if (length(aic_gauss)) aic_gauss else NA_real_,
    aic_independence = if (length(aic_indep)) aic_indep else NA_real_,
    daic_vs_gaussian = (if (length(aic_gauss)) aic_gauss else NA_real_) - sel$AIC,
    p_exch           = p_exch,
    p_radsym         = p_rad,
    symtest_n        = nrow(sub),
    symtest_reps     = SYMTEST_REPS,
    aic_table        = list(aic_tbl)
  )
}

fit_one_table <- function(table_name) {
  message("  Processing: ", table_name)

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e)); NULL })
  if (is.null(df)) return(list(excluded = tibble(table = table_name, reason = "fetch failed")))
  if (!all(c("id", "item", "resp", "rt") %in% names(df))) {
    return(list(excluded = tibble(table = table_name, reason = "no rt column in fetched table")))
  }

  n_rows_raw <- nrow(df)

  tr <- resolve_trials(df, table_name)
  if (is.null(tr$df)) {
    message("    excluded: ", tr$note)
    return(list(excluded = tibble(table = table_name, reason = tr$note)))
  }
  df <- tr$df

  # `rt` does not always arrive numeric -- credentialform_lnirt hands back a
  # character column (with real NAs, and 109 literal zeros that log() would
  # send to -Inf). Coerce explicitly rather than trusting the fetched type.
  df <- df[, c("id", "item", "resp", "rt")]
  df$resp <- suppressWarnings(as.numeric(as.character(df$resp)))
  df$rt   <- suppressWarnings(as.numeric(as.character(df$rt)))
  df <- df[is.finite(df$resp) & is.finite(df$rt) & df$rt > 0, ]
  if (nrow(df) == 0) {
    return(list(excluded = tibble(table = table_name, reason = "no usable resp/rt rows")))
  }

  # Wide tables: keep the MAX_ITEMS most-administered items. This changes the
  # object -- it is an item-selected subtable, not the table -- so it is
  # flagged and reported separately on the page, never pooled silently.
  item_selection <- "all items"
  if (length(unique(df$item)) > MAX_ITEMS) {
    keep <- names(sort(table(df$item), decreasing = TRUE))[seq_len(MAX_ITEMS)]
    df <- df[df$item %in% keep, ]
    item_selection <- paste0("top ", MAX_ITEMS, " items by administration count")
  }

  # Item floor then person floor, iterated twice: dropping thin items can push
  # a person below the person floor and vice versa.
  for (i in 1:2) {
    ic <- table(df$item); df <- df[df$item %in% names(ic)[ic >= MIN_RESP_PER_ITEM], ]
    pc <- table(df$id);   df <- df[df$id   %in% names(pc)[pc >= PERSON_FLOOR], ]
  }
  if (nrow(df) == 0 || length(unique(df$item)) < MIN_ITEMS) {
    return(list(excluded = tibble(table = table_name,
      reason = paste0("fewer than ", MIN_ITEMS, " items survive the response floors"))))
  }

  n_persons_pre <- length(unique(df$id))
  if (n_persons_pre > MAX_N) {
    df <- df[df$id %in% sample(unique(df$id), MAX_N), ]
  }
  if (length(unique(df$id)) < MIN_PARTICIPANTS) {
    return(list(excluded = tibble(table = table_name,
      reason = paste0("fewer than ", MIN_PARTICIPANTS, " persons survive the response floors"))))
  }

  ids <- sort(unique(df$id)); its <- sort(unique(df$item))
  Y  <- matrix(NA_real_, length(ids), length(its))
  RT <- Y
  ri <- cbind(match(df$id, ids), match(df$item, its))
  Y[ri]  <- df$resp
  RT[ri] <- log(df$rt)   # LNIRT expects LOG response times

  keep_item <- apply(Y, 2, function(x) length(unique(na.omit(x))) > 1)
  Y <- Y[, keep_item, drop = FALSE]; RT <- RT[, keep_item, drop = FALSE]
  if (ncol(Y) < MIN_ITEMS) {
    return(list(excluded = tibble(table = table_name,
      reason = "fewer than 10 items with response variance")))
  }

  pct_na <- 100 * mean(is.na(Y))

  fit <- tryCatch(
    { invisible(capture.output(
        f <- LNIRT::LNIRT(RT = RT, Y = Y, XG = XG, burnin = BURNIN,
                          residual = FALSE, td = FALSE))); f },
    error = function(e) { message("    LNIRT failed: ", conditionMessage(e)); NULL })
  if (is.null(fit)) {
    return(list(excluded = tibble(table = table_name, reason = "LNIRT fit failed")))
  }

  theta <- as.numeric(fit$Post.Means$Person.Ability)
  speed <- as.numeric(fit$Post.Means$Person.Speed)
  if (length(theta) != nrow(Y) || all(!is.finite(theta)) || all(!is.finite(speed))) {
    return(list(excluded = tibble(table = table_name,
      reason = "LNIRT returned unusable person estimates")))
  }

  # The fitted MVN person prior IS the null the copula replaces, so carry its
  # implied Kendall tau along for comparison on the same scale.
  cv  <- fit$Post.Means$Cov.Person.Ability.Speed
  vth <- fit$Post.Means$Var.Person.Ability
  vsp <- fit$Post.Means$Var.Person.Speed
  rho_mvn <- if (all(is.finite(c(cv, vth, vsp))) && vth > 0 && vsp > 0) cv / sqrt(vth * vsp) else NA_real_
  tau_mvn <- if (is.finite(rho_mvn)) (2 / pi) * asin(max(-1, min(1, rho_mvn))) else NA_real_

  # Characterise at each floor in the sweep. The floor is applied to the
  # responses each person actually has in the matrix that was fitted, so the
  # stricter floors are nested subsets of the same fit -- this is a
  # sensitivity check on the person set, not on the item parameters.
  n_resp_person <- rowSums(!is.na(Y))
  cop <- map_dfr(PERSON_FLOOR_SWEEP, function(fl) {
    sel <- n_resp_person >= fl
    if (sum(sel) < 50) return(NULL)
    characterise_copula(theta[sel], speed[sel], fl, ncol(Y), table_name)
  })
  if (nrow(cop) == 0) {
    return(list(excluded = tibble(table = table_name, reason = "copula characterisation failed")))
  }

  meta_row <- if (!is.null(tags_meta)) tags_meta[tags_meta$table == table_name, ] else NULL
  construct_type <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$construct_type[1] else NA_character_
  item_format    <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$item_format[1] else NA_character_

  summary_tbl <- cop |>
    mutate(table          = table_name,
           n_persons_fit  = nrow(Y),
           n_items_fit    = ncol(Y),
           pct_na         = pct_na,
           n_rows_raw     = n_rows_raw,
           trial_note     = tr$note,
           wave_collapsed = tr$collapsed,
           item_selection = item_selection,
           tau_mvn        = tau_mvn,
           rho_mvn        = rho_mvn,
           construct_type = construct_type,
           item_format    = item_format) |>
    relocate(table)

  list(
    summary = summary_tbl,
    # Full person estimates stay in the per-table fit (which is gitignored and
    # resume-only), so a later diagnostic never costs another LNIRT run.
    person_estimates = tibble(theta = theta, speed = speed,
                              n_resp = as.integer(n_resp_person)),
    # A thinned pseudo-observation sample, for the copula scatter on the page.
    pobs_sample = {
      ok <- is.finite(theta) & is.finite(speed)
      U <- copula::pobs(cbind(theta[ok], speed[ok]))
      if (nrow(U) > 2000) U[sample.int(nrow(U), 2000), ] else U
    }
  )
}

# ==============================================================================
# 3. Run, writing each result to disk as it completes
# ==============================================================================

fit_to_disk <- function(table_name) {
  out_file <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
  }
  result <- tryCatch(fit_one_table(table_name), error = function(e) {
    message("    unexpected error for ", table_name, ": ", conditionMessage(e))
    list(excluded = tibble(table = table_name,
                           reason = paste0("unexpected error: ", conditionMessage(e))))
  })
  if (!is.null(result)) saveRDS(result, out_file)
}

# Two workers, niced launch. This is a working desktop; LNIRT is MCMC and
# irw_fetch forks download children of its own, so a wide `plan` here is how
# you OOM the machine rather than how you finish sooner.
plan(multisession, workers = 2)
message("\nAnalyzing ", length(tables), " candidate tables...")
invisible(future_map(tables, fit_to_disk, .options = furrr_options(seed = TRUE)))
plan(sequential)

# ==============================================================================
# 4. Combine results
# ==============================================================================

all_raw <- map(tables, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

all_summary <- map(all_raw, "summary") |> compact() |> bind_rows()
excluded    <- map(all_raw, "excluded") |> compact() |> bind_rows()

pobs_samples <- all_raw |>
  keep(~ !is.null(.x$summary)) |>
  set_names(map_chr(keep(all_raw, ~ !is.null(.x$summary)), ~ .x$summary$table[1])) |>
  map("pobs_sample")

headline <- all_summary |> filter(person_floor == PERSON_FLOOR)

message("\nDone. ", nrow(headline), " tables with usable copula results out of ",
        length(tables), " attempted; ", nrow(excluded), " excluded by a gate.")
if (nrow(excluded)) {
  message("Exclusions:")
  for (i in seq_len(nrow(excluded))) {
    message("  - ", excluded$table[i], ": ", excluded$reason[i])
  }
}
if (nrow(headline)) {
  message("\nWinning family counts at the headline floor (", PERSON_FLOOR, "):")
  print(sort(table(headline$best_family_name), decreasing = TRUE))
  message("90-degree-rotated Joe wins on ", sum(headline$is_joe90), " of ", nrow(headline), ".")
}

# ==============================================================================
# 5. Save combined output
# ==============================================================================

saveRDS(
  list(
    summary             = all_summary,
    excluded            = excluded,
    gate_log            = gate_log,
    pobs_samples        = pobs_samples,
    candidate_tables    = tables,
    all_candidates      = all_candidates,
    n_all_candidates    = n_all_candidates,
    family_set          = FAMILY_SET,
    person_floor        = PERSON_FLOOR,
    person_floor_sweep  = PERSON_FLOOR_SWEEP,
    min_resp_per_item   = MIN_RESP_PER_ITEM,
    max_items           = MAX_ITEMS,
    max_n               = MAX_N,
    xg                  = XG,
    pilot               = PILOT,
    date_run            = Sys.Date(),
    session             = sessionInfo()
  ),
  file = file.path(out_dir, "speed_accuracy_copula_results.rds")
)

message("Saved to ", out_dir, "/speed_accuracy_copula_results.rds")

# ==============================================================================
# 6. Generate citations
# ==============================================================================

tryCatch(
  irw_save_bibtex(unique(all_summary$table), output_file = bib_file),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)

manual_entries <- c(
  "@article{kwon2026copula,
  title   = {A Copula-Based Joint Model for Speed and Accuracy},
  author  = {Kwon, Sooyong and Zhang, Susu},
  journal = {Psychometrika},
  year    = {2026},
  doi     = {10.1017/psy.2026.10143}
}",
  "@article{vanderlinden2007hierarchical,
  title   = {A hierarchical framework for modeling speed and accuracy on test items},
  author  = {van der Linden, Wim J.},
  journal = {Psychometrika},
  volume  = {72},
  number  = {3},
  pages   = {287--308},
  year    = {2007},
  doi     = {10.1007/s11336-006-1478-z}
}",
  "@article{domingue2022notsofast,
  title   = {Not So Fast: The Limited Validity of Response Time Models},
  author  = {Domingue, Benjamin W. and Kanopka, Klint and Stenhaug, Ben and
             Sulik, Michael J. and Beverly, Tanesia and Brinkhuis, Matthieu and
             Circi, Ruhan and Faul, Jessica and Liao, Dandan and McCandliss, Bruce and
             Obradovi\\'{c}, Jelena and Piech, Chris and Porter, Tenelle and
             Soland, James and Weeks, Jonathan and Wise, Steven L. and Berkowitz, Talia},
  journal = {Journal of Educational and Behavioral Statistics},
  volume  = {47},
  number  = {6},
  pages   = {654--679},
  year    = {2022},
  doi     = {10.3102/10769986221099906}
}",
  "@article{schmidt2006nonparametric,
  title   = {Non-parametric estimation of tail dependence},
  author  = {Schmidt, Rafael and Stadtm\\\"{u}ller, Ulrich},
  journal = {Scandinavian Journal of Statistics},
  volume  = {33},
  number  = {2},
  pages   = {307--335},
  year    = {2006},
  doi     = {10.1111/j.1467-9469.2005.00483.x}
}",
  "@article{fox2021lnirt,
  title   = {{LNIRT}: An {R} Package for Joint Modeling of Response Accuracy and Times},
  author  = {Fox, Jean-Paul and Klotzke, Konrad and Entink, Rinke Klein},
  journal = {Journal of Statistical Software},
  year    = {2021},
  note    = {R package version 0.5.1}
}",
  "@article{genest2012tests,
  title   = {Tests of symmetry for bivariate copulas},
  author  = {Genest, Christian and Neslehov\\'{a}, Johanna},
  journal = {Annals of the Institute of Statistical Mathematics},
  volume  = {64},
  pages   = {811--834},
  year    = {2012},
  doi     = {10.1007/s10463-011-0337-6}
}"
)

entry_key <- function(entry) sub("^@\\w+\\{([^,]+),.*$", "\\1", trimws(entry))
existing_keys <- if (file.exists(bib_file)) {
  key_lines <- grep("^@\\w+\\{", readLines(bib_file), value = TRUE)
  vapply(key_lines, entry_key, character(1), USE.NAMES = FALSE)
} else character(0)
new_entries <- manual_entries[!vapply(manual_entries, entry_key, character(1)) %in% existing_keys]
if (length(new_entries) > 0) {
  cat(paste0(new_entries, "\n"), file = bib_file, append = TRUE, sep = "\n")
  message(length(new_entries), " manual citation(s) appended to ", bib_file)
}
