# network_psych_compute.R
#
# Network psychometrics (Ising models for binary items, Gaussian graphical
# models for polytomous/continuous items) represents item covariation as a
# graph of direct partial-correlation edges rather than a shared latent
# variable. Van der Maas et al. (2006) and Epskamp, Maris, van Borkulo, &
# Borsboom (2018) show that under a single dominant common cause, network
# centrality (node strength) should track IRT discrimination, and the
# network should look densely/uniformly connected. Does this hold across
# real IRW instruments, and do breakdowns line up with tables already
# flagged as multidimensional (dimensionality vignette) or locally
# dependent (local-dependence vignette)?
#
# Produces the precomputed results loaded by network_psych.qmd.
#
# Output: network_psych_data/network_psych_results.rds
#         network_psych_data/references.bib
#
# Usage:
#   Rscript vignettes/network_psych_compute.R   # from project root

library(irw)
library(bootnet)
library(qgraph)
library(mirt)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)
library(easybgm)   # amendment: Bayesian edge evidence (Huth et al. 2026); loads BGGM and bgms

set.seed(20260722)

out_dir  <- "vignettes/network_psych_data"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
bib_file <- file.path(out_dir, "references.bib")

MIN_ITEMS        <- 5
MAX_ITEMS        <- 30      # lower than the dimensionality vignette's 40 -- EBIC-regularized
                             # network estimation scales worse than a single IRT fit or eigen-decomposition
MIN_PARTICIPANTS <- 300
MAX_N            <- 10000   # downsample respondents before fitting, mirrors the other vignettes
Q3_THRESHOLD_REF <- 0.2     # only used to describe joined local-dependence results, not recomputed here
PILOT            <- FALSE   # TRUE: run on a small hand-picked/sampled subset for a draft page
PILOT_N_TABLES   <- 18

DIM_CACHE <- "vignettes/dimensionality_data/dimensionality_results.rds"
LD_CACHE  <- "vignettes/local_dependence_data/local_dependence_results.rds"

# Amendment: Bayesian edge evidence (Huth, Haslbeck, Keetelaar, van Holst, &
# Marsman, 2026, Nature Human Behaviour). Option A (primary) fits every table
# as a GGM via BGGM/easybgm, uniformly across binary/ordinal/continuous items,
# matching the paper's own simplifying choice exactly -- this is what makes
# the headline-number comparison in the vignette apples-to-apples. Option B
# (secondary, small subset only) fits the ordinal Markov random field via
# bgms instead, the model the paper itself calls conceptually more
# appropriate for this data type but didn't use. See "Runtime piloting" below
# for why Option B is capped at a small subset rather than run at full batch.
BGGM_PRIOR_SD        <- 0.25   # paper's matrix-F prior default (BGGM/easybgm default too)
BGGM_TIME_BUDGET_SEC <- 120    # piloted worst case (28 items, ~12,500 respondents): ~10 sec
MRF_TIME_BUDGET_SEC  <- 600    # piloted worst case (30 items, 664 respondents): ~192 sec
MIN_BGGM_N           <- 30     # sanity floor after listwise-deletion of missing rows
N_OPTION_B_TABLES     <- 20    # bgms ordinal MRF piloted at ~20-50x BGGM's runtime per table
                                # (138-192 sec vs 1-10 sec on the same tables) -- full-batch
                                # Option B across ~660 candidates would take 7-27 hours, so it
                                # runs as an illustrative robustness check on a small stratified
                                # subset only, not the full candidate pool.

# ==============================================================================
# 0. Hard prerequisite gate
#    The whole point of this vignette is the cross-vignette join in step 7
#    below. A results file that quietly lacks those comparison columns is
#    worse than not running at all, so refuse to proceed without both caches.
# ==============================================================================

if (!file.exists(DIM_CACHE)) {
  stop("Missing prerequisite cache: ", DIM_CACHE,
       " -- run vignettes/dimensionality_compute.R first.")
}
if (!file.exists(LD_CACHE)) {
  stop("Missing prerequisite cache: ", LD_CACHE,
       " -- run vignettes/local_dependence_compute.R first.")
}

dim_summary <- readRDS(DIM_CACHE)$summary
ld_summary  <- readRDS(LD_CACHE)$summary

message("Prerequisite caches found: ", nrow(dim_summary), " dimensionality rows, ",
        nrow(ld_summary), " local-dependence rows.")

# ==============================================================================
# 1. Select datasets
# ==============================================================================

binary_tables <- irw_filter(n_items = c(MIN_ITEMS, MAX_ITEMS),
                             n_participants = c(MIN_PARTICIPANTS, Inf),
                             n_categories = 2)
poly_tables   <- irw_filter(n_items = c(MIN_ITEMS, MAX_ITEMS),
                             n_participants = c(MIN_PARTICIPANTS, Inf),
                             n_categories = c(3, 7))
all_candidates <- union(binary_tables, poly_tables)

message("Candidate tables (n_items ", MIN_ITEMS, "-", MAX_ITEMS,
        ", n_participants >= ", MIN_PARTICIPANTS, "): ", length(all_candidates),
        " (", length(binary_tables), " binary, ", length(poly_tables), " polytomous)")

if (PILOT) {
  # Small draft run: a couple of tables already confirmed (by hand, during
  # pipeline piloting) to give a clean strength-vs-discrimination signal --
  # one binary (IsingFit), one polytomous (EBICglasso) -- plus a random
  # top-up mixing both item types for the draft page.
  known_good <- intersect(c("gilbert_meta_109", "assessment_time_fournier_2025_gad"), all_candidates)
  remainder  <- setdiff(all_candidates, known_good)
  tables <- c(known_good, sample(remainder, min(PILOT_N_TABLES - length(known_good), length(remainder))))
  message("PILOT run: ", length(tables), " tables selected.")
} else {
  tables <- all_candidates
}

# Table-level construct_type/item_format for the final summary, fetched once
# for all candidate tables rather than per-table.
tags_meta <- tryCatch(irw_tags(tables = tables), error = function(e) {
  message("irw_tags() failed: ", conditionMessage(e))
  NULL
})

# Stratified subset for Option B (ordinal MRF via bgms) -- half binary, half
# polytomous, so the agreement-with-Option-A check below isn't confined to
# one item type. Sampled once here (not per-worker) so re-running the batch
# with the same seed reproduces the same subset.
option_b_tables <- {
  b <- intersect(binary_tables, tables)
  p <- intersect(poly_tables, tables)
  n_b <- min(N_OPTION_B_TABLES %/% 2, length(b))
  n_p <- min(N_OPTION_B_TABLES - n_b, length(p))
  c(if (n_b > 0) sample(b, n_b) else character(0),
    if (n_p > 0) sample(p, n_p) else character(0))
}
message("Option B (ordinal MRF) illustrative subset: ", length(option_b_tables), " tables.")

# ==============================================================================
# 1b. Bayesian edge evidence (Huth et al. 2026 amendment)
# ==============================================================================

# Paper's exact five-way classification, open-interval convention: every BF10
# value falls in exactly one category (no gaps, no overlaps). Note deviation
# from a strictly open-interval read of the paper's stated thresholds: the
# inconclusive band is closed on both ends here so the five categories
# partition the real line; the paper describes the boundary as open (e.g.
# "1/3 < BF10 < 3") without saying which neighboring category owns the
# boundary value itself. Continuous posterior BF10 draws make an edge landing
# exactly on 3, 1/3, 10, or 1/10 a measure-zero event in practice.
#
# Implemented as explicit ordered comparisons rather than cut(): bgms
# (Option B) legitimately returns BF10 = Inf for edges with posterior
# inclusion probability 1, and base R's cut() returns NA for a value equal
# to its own Inf breakpoint (Inf < Inf is FALSE, so it matches no bin) --
# confirmed against real bgms output during piloting, where 15/276 edges on
# one table silently dropped out of every category. Explicit comparisons
# handle Inf/-Inf correctly; genuine NaN (a failed edge estimate, not
# observed in piloting but not ruled out at batch scale) still propagates to
# NA here, and evidence_proportions() below excludes NA from both the
# numerator and denominator rather than silently miscounting it.
classify_bf_evidence <- function(bf10) {
  dplyr::case_when(
    bf10 > 10   ~ "strong_presence",
    bf10 > 3    ~ "weak_presence",
    bf10 >= 1/3 ~ "inconclusive",
    bf10 >= 1/10 ~ "weak_absence",
    bf10 < 1/10 ~ "strong_absence",
    TRUE        ~ NA_character_
  )
}

edges_from_bggm_fit <- function(fit, table_name, item_names, n_used, suffix) {
  pcor <- fit$parameters
  bf   <- fit$inc_BF
  pip  <- fit$inc_probs
  ni   <- length(item_names)
  ut   <- upper.tri(pcor)
  edges <- tibble(
    table    = table_name,
    item_i   = item_names[row(pcor)[ut]],
    item_j   = item_names[col(pcor)[ut]],
    pcor     = pcor[ut],
    BF10     = bf[ut],
    inc_prob = pip[ut]
  )
  edges$evidence_category <- classify_bf_evidence(edges$BF10)
  names(edges)[-(1:3)] <- paste0(names(edges)[-(1:3)], "_", suffix)
  edges
}

evidence_proportions <- function(categories, suffix) {
  levels <- c("strong_presence", "weak_presence", "inconclusive",
              "weak_absence", "strong_absence")
  props <- setNames(as.list(rep(NA_real_, length(levels))),
                     paste0("prop_", levels, "_", suffix))
  # Drop NA categories (a genuinely unclassifiable edge, e.g. a failed BF10
  # estimate) from both the numerator and denominator, so the five
  # proportions still sum to 1 over the edges that *were* classified rather
  # than silently undercounting against the full edge count.
  n_na <- sum(is.na(categories))
  categories <- categories[!is.na(categories)]
  if (n_na > 0) {
    message("    ", n_na, " edge(s) excluded from evidence proportions (unclassifiable BF10)")
  }
  if (length(categories) > 0) {
    tab <- table(factor(categories, levels = levels)) / length(categories)
    props[paste0("prop_", levels, "_", suffix)] <- as.list(as.numeric(tab))
  }
  as_tibble(props)
}

# Option A (primary): GGM via BGGM, forced uniformly across item types. Reuses
# the already-fetched, already-cleaned `resp` from fit_network() rather than
# re-fetching. type = "continuous" is forced regardless of n_categories --
# easybgm() silently redirects package = "BGGM" back to bgms whenever
# type = "binary" is passed (checked against the installed easybgm 0.4.0
# source directly), which would silently defeat Option A's whole point of a
# uniform GGM comparison, so type must never be anything but "continuous"
# here.
fit_bayesian_edge_evidence <- function(resp, table_name, time_budget_sec = BGGM_TIME_BUDGET_SEC) {
  resp_num <- as.matrix(sapply(resp, as.numeric))
  resp_num <- na.omit(resp_num)  # BGGM::explore()'s own default (impute = FALSE) does this
                                  # internally too; done explicitly here so n_used below
                                  # reflects the actual sample the evidence is based on.
  n_used <- nrow(resp_num)
  if (n_used < MIN_BGGM_N) {
    message("    Bayesian edge evidence skipped: only ", n_used, " complete rows")
    return(NULL)
  }

  fit <- tryCatch(
    R.utils::withTimeout(
      easybgm(data = resp_num, type = "continuous", package = "BGGM",
              prior_sd = BGGM_PRIOR_SD, progress = FALSE),
      timeout = time_budget_sec, onTimeout = "error"
    ),
    error = function(e) {
      message("    Bayesian edge evidence (BGGM) failed/timed out: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)

  ni    <- ncol(resp_num)
  edges <- edges_from_bggm_fit(fit, table_name, colnames(resp_num), n_used, "ggm")
  props <- evidence_proportions(edges$evidence_category_ggm, "ggm")

  list(
    edges   = edges,
    summary = bind_cols(
      tibble(n_used_ggm = n_used,
             relative_n_ggm = n_used / (ni * (ni - 1) / 2)),
      props
    )
  )
}

# Option B (secondary, small subset only -- see N_OPTION_B_TABLES above):
# ordinal Markov random field via bgms, easybgm's own default routing for
# binary/ordinal data (no package override, unlike Option A).
fit_ordinal_mrf_edge_evidence <- function(resp, table_name, n_categories,
                                           time_budget_sec = MRF_TIME_BUDGET_SEC) {
  resp_num <- as.matrix(sapply(resp, as.numeric))
  resp_num <- na.omit(resp_num)
  n_used <- nrow(resp_num)
  if (n_used < MIN_BGGM_N) {
    message("    Ordinal MRF edge evidence skipped: only ", n_used, " complete rows")
    return(NULL)
  }

  mrf_type <- if (n_categories == 2) "binary" else "ordinal"
  fit <- tryCatch(
    R.utils::withTimeout(
      easybgm(data = resp_num, type = mrf_type, progress = FALSE),
      timeout = time_budget_sec, onTimeout = "error"
    ),
    error = function(e) {
      message("    Ordinal MRF edge evidence (bgms) failed/timed out: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)

  ni    <- ncol(resp_num)
  edges <- edges_from_bggm_fit(fit, table_name, colnames(resp_num), n_used, "mrf")
  props <- evidence_proportions(edges$evidence_category_mrf, "mrf")

  list(
    edges   = edges,
    summary = bind_cols(
      tibble(n_used_mrf = n_used,
             relative_n_mrf = n_used / (ni * (ni - 1) / 2)),
      props
    )
  )
}

# ==============================================================================
# 2. Per-table computation
# ==============================================================================

fit_network <- function(table_name) {
  message("  Processing: ", table_name)

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(df)) return(NULL)

  unique_ids <- unique(df$id)
  if (length(unique_ids) > MAX_N) {
    df <- df[df$id %in% sample(unique_ids, MAX_N), ]
  }

  resp <- irw_long2resp(df)
  resp$id <- NULL

  # Drop zero-variance items
  resp <- resp[, sapply(resp, function(x) length(unique(na.omit(x))) > 1), drop = FALSE]
  if (ncol(resp) < MIN_ITEMS) {
    message("    skipped: fewer than ", MIN_ITEMS, " usable items")
    return(NULL)
  }

  n_categories <- length(unique(na.omit(unlist(resp))))
  if (n_categories == 2) {
    network_default <- "IsingFit"
    irt_itemtype     <- "2PL"
    cor_method       <- NULL
  } else if (n_categories >= 3 && n_categories <= 7) {
    network_default <- "EBICglasso"
    # GPCM, not GRM: Marsman, van den Bergh, & Haslbeck (2025) derive the
    # network-model <-> IRT-model equivalence for ordinal items via the
    # generalized partial credit model, not the graded response model --
    # so GPCM is the theoretically matched choice for this vignette's core
    # comparison on polytomous tables (see Limitations in network_psych.qmd).
    irt_itemtype     <- "gpcm"
    # bootnet's "EBICglasso" default set stopped calling qgraph::cor_auto()
    # (which estimates polychoric/polyserial correlations for ordinal data)
    # as of bootnet 1.4, falling back to plain Pearson correlations unless
    # corMethod is passed explicitly. Pin it here rather than rely on
    # whatever the installed bootnet version's default happens to be.
    cor_method       <- "cor_auto"
  } else {
    message("    skipped: n_categories = ", n_categories, " outside {2} u {3..7}")
    return(NULL)
  }

  ni <- ncol(resp)

  net <- tryCatch(
    suppressWarnings(suppressMessages(
      if (is.null(cor_method)) {
        estimateNetwork(resp, default = network_default, verbose = FALSE)
      } else {
        # cor_auto()'s polychoric matrix isn't guaranteed positive-definite
        # (common with several ordinal items and pairwise-estimated
        # correlations); bootnet's default is to error out rather than
        # correct it, so force the nearest valid correlation matrix instead
        # of dropping every table that hits this.
        estimateNetwork(resp, default = network_default, corMethod = cor_method,
                         corArgs = list(forcePD = TRUE), verbose = FALSE)
      }
    )),
    error = function(e) {
      message("    ", network_default, " failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(net)) return(NULL)

  graph <- net$graph
  density <- mean(graph[upper.tri(graph)] != 0)

  cent <- tryCatch(centrality_auto(graph), error = function(e) {
    message("    centrality_auto failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(cent)) return(NULL)

  # In a genuine weighted network centrality_auto() names the column
  # "Strength"; an all-zero (fully empty) regularized network gets
  # auto-detected as unweighted and falls back to "Degree" instead -- still
  # informative (an empty network is itself a real result, kept below), but
  # not comparable to discrimination via correlation, so that's left NA.
  strength <- if ("Strength" %in% names(cent$node.centrality)) {
    setNames(cent$node.centrality$Strength, rownames(cent$node.centrality))
  } else {
    setNames(cent$node.centrality$Degree, rownames(cent$node.centrality))
  }

  fit <- tryCatch(
    mirt(resp, 1, itemtype = irt_itemtype, verbose = FALSE,
         technical = list(NCYCLES = 2000)),
    error = function(e) {
      message("    mirt failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)
  if (!mirt::extract.mirt(fit, "converged")) {
    message("    skipped: IRT model did not converge")
    return(NULL)
  }

  a <- tryCatch(coef(fit, simplify = TRUE)$items[, "a1"], error = function(e) NULL)
  if (is.null(a)) {
    message("    skipped: could not extract discrimination parameters")
    return(NULL)
  }
  a <- a[names(strength)]

  strength_a_cor <- if (density > 0 && length(unique(strength)) > 1) {
    tryCatch(cor(strength, a), error = function(e) NA_real_)
  } else {
    NA_real_
  }

  meta_row <- if (!is.null(tags_meta)) tags_meta[tags_meta$table == table_name, ] else NULL
  construct_type <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$construct_type[1] else NA_character_
  item_format    <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$item_format[1] else NA_character_

  # Cross-vignette join (step 7 of the spec): read directly from the
  # already-loaded dimensionality/local-dependence summaries rather than
  # recomputing anything.
  dim_row <- dim_summary[dim_summary$table == table_name, ]
  ld_row  <- ld_summary[ld_summary$table == table_name, ]

  # Amendment: Bayesian edge evidence (Huth et al. 2026). Reuses `resp` --
  # already fetched, downsampled, and zero-variance-dropped above -- rather
  # than fetching again.
  bayes <- tryCatch(fit_bayesian_edge_evidence(resp, table_name), error = function(e) {
    message("    Bayesian edge evidence wrapper failed: ", conditionMessage(e))
    NULL
  })
  mrf <- if (table_name %in% option_b_tables) {
    tryCatch(fit_ordinal_mrf_edge_evidence(resp, table_name, n_categories), error = function(e) {
      message("    Ordinal MRF edge evidence wrapper failed: ", conditionMessage(e))
      NULL
    })
  } else NULL

  bayes_summary_cols <- if (!is.null(bayes)) bayes$summary else tibble(
    n_used_ggm = NA_integer_, relative_n_ggm = NA_real_,
    prop_strong_presence_ggm = NA_real_, prop_weak_presence_ggm = NA_real_,
    prop_inconclusive_ggm = NA_real_, prop_weak_absence_ggm = NA_real_,
    prop_strong_absence_ggm = NA_real_
  )
  mrf_summary_cols <- if (!is.null(mrf)) mrf$summary else tibble(
    n_used_mrf = NA_integer_, relative_n_mrf = NA_real_,
    prop_strong_presence_mrf = NA_real_, prop_weak_presence_mrf = NA_real_,
    prop_inconclusive_mrf = NA_real_, prop_weak_absence_mrf = NA_real_,
    prop_strong_absence_mrf = NA_real_
  )

  list(
    summary = bind_cols(
      tibble(
        table               = table_name,
        n_items             = ni,
        n_participants      = nrow(resp),
        n_categories        = n_categories,
        network_method      = network_default,
        network_density     = density,
        strength_a_cor      = strength_a_cor,
        construct_type      = construct_type,
        item_format         = item_format,
        dim_ratio_12        = if (nrow(dim_row) > 0) dim_row$ratio_12[1] else NA_real_,
        dim_nfact_suggested = if (nrow(dim_row) > 0) dim_row$nfact_suggested[1] else NA_integer_,
        dim_unidimensional  = if (nrow(dim_row) > 0) dim_row$unidimensional_pa[1] else NA,
        ld_prop_flagged     = if (nrow(ld_row) > 0) ld_row$prop_flagged[1] else NA_real_,
        ld_mean_abs_q3      = if (nrow(ld_row) > 0) ld_row$mean_abs_q3[1] else NA_real_,
        has_dim_match       = nrow(dim_row) > 0,
        has_ld_match        = nrow(ld_row) > 0,
        has_bayes_match     = !is.null(bayes),
        has_mrf_match       = !is.null(mrf)
      ),
      bayes_summary_cols,
      mrf_summary_cols
    ),
    graph      = graph,
    strength   = strength,
    a          = a,
    edges_ggm  = if (!is.null(bayes)) bayes$edges else NULL,
    edges_mrf  = if (!is.null(mrf)) mrf$edges else NULL
  )
}

# ==============================================================================
# 3. Run, writing each result to disk as it completes
#    If the script crashes, re-running skips already-completed tables
# ==============================================================================

fit_to_disk <- function(table_name) {
  out_file <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    cached <- tryCatch(readRDS(out_file), error = function(e) NULL)
    # Amendment: pre-existing per-table caches from before the Bayesian edge
    # evidence amendment lack the "edges_ggm" element entirely (not just a
    # NULL value -- a NULL there can also mean the amendment ran but that
    # table's Bayesian fit failed/timed out, which should still count as
    # done). Only a genuinely old-format cache gets re-run.
    if (!is.null(cached) && "edges_ggm" %in% names(cached)) {
      message("  Skipping (already done): ", table_name)
      return(invisible(NULL))
    }
    message("  Re-running (old-format cache predates Bayesian edge evidence amendment): ",
            table_name)
  }
  result <- tryCatch(fit_network(table_name), error = function(e) {
    message("    unexpected error for ", table_name, ": ", conditionMessage(e))
    NULL
  })
  if (!is.null(result)) saveRDS(result, out_file)
}

plan(multisession, workers = min(4, parallel::detectCores() %/% 2))
message("\nAnalyzing ", length(tables), " candidate tables...")
future_map(tables, fit_to_disk, .options = furrr_options(seed = TRUE))
plan(sequential)

# ==============================================================================
# 4. Combine results
# ==============================================================================

all_raw <- map(tables, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

all_summary <- map(all_raw, "summary") |> compact() |> bind_rows()

result_names  <- map_chr(all_raw, function(r) r$summary$table)
all_graphs    <- set_names(map(all_raw, "graph"), result_names)
all_strengths <- set_names(map(all_raw, "strength"), result_names)
all_a         <- set_names(map(all_raw, "a"), result_names)

# Edge-level Bayesian evidence (amendment): one row per item pair per table,
# used for the paper's Fig. 3/Fig. 4 replications and the strong-evidence-only
# cross-vignette re-check in the qmd.
all_edges_ggm <- map(all_raw, "edges_ggm") |> compact() |> bind_rows()
all_edges_mrf <- map(all_raw, "edges_mrf") |> compact() |> bind_rows()

n_dim_matched   <- sum(all_summary$has_dim_match)
n_ld_matched    <- sum(all_summary$has_ld_match)
n_bayes_matched <- sum(all_summary$has_bayes_match)
n_mrf_matched   <- sum(all_summary$has_mrf_match)
message("\nDone. ", nrow(all_summary), " tables with usable network results out of ",
        length(tables), " candidates.")
message("  quality check: ", n_dim_matched, "/", nrow(all_summary),
        " found a matching dimensionality-vignette entry.")
message("  quality check: ", n_ld_matched, "/", nrow(all_summary),
        " found a matching local-dependence-vignette entry.")
message("  quality check: ", n_bayes_matched, "/", nrow(all_summary),
        " produced usable Bayesian edge evidence (Option A, GGM).")
message("  quality check: ", n_mrf_matched, "/", length(option_b_tables),
        " Option B (ordinal MRF) subset tables produced usable results.")

# ==============================================================================
# 5. Save combined output
# ==============================================================================

saveRDS(
  list(
    summary          = all_summary,
    graphs           = all_graphs,
    strengths        = all_strengths,
    discriminations  = all_a,
    edges_ggm        = all_edges_ggm,
    edges_mrf        = all_edges_mrf,
    option_b_tables  = option_b_tables,
    candidate_tables = tables,
    n_all_candidates = length(all_candidates),
    n_binary_all     = length(binary_tables),
    n_poly_all       = length(poly_tables),
    pilot            = PILOT,
    date_run         = Sys.Date(),
    session          = sessionInfo()
  ),
  file = file.path(out_dir, "network_psych_results.rds")
)

message("Saved to ", out_dir, "/network_psych_results.rds")

# ==============================================================================
# 6. Generate citations
# ==============================================================================

tryCatch(
  irw_save_bibtex(unique(all_summary$table), output_file = bib_file),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)

manual_entries <- c(
  "@article{vandermaas2006dynamical,
  title   = {A dynamical model of general intelligence: the positive manifold of intelligence by mutualism},
  author  = {van der Maas, Han L. J. and Dolan, Conor V. and Grasman, Raoul P. P. P. and Wicherts, Jelte M. and Huizenga, Hilde M. and Raijmakers, Maartje E. J.},
  journal = {Psychological Review},
  volume  = {113},
  number  = {4},
  pages   = {842--861},
  year    = {2006},
  doi     = {10.1037/0033-295X.113.4.842}
}",
  "@incollection{epskamp2018testing,
  title     = {Testing the network approach: {A}re psychopathology symptom networks biased structures?},
  author    = {Epskamp, Sacha and Maris, Gunter and van Borkulo, Claudia D. and Borsboom, Denny},
  booktitle = {The {Wiley} Handbook of Psychometric Testing},
  publisher = {Wiley},
  year      = {2018},
  doi       = {10.1002/9781118489772.ch30}
}",
  "@article{marsman2018introduction,
  title   = {An introduction to network psychometrics: {R}elating {I}sing network models to item response theory models},
  author  = {Marsman, Maarten and Borsboom, Denny and Kruis, Jonas and Epskamp, Sacha and van Bork, Ria and Waldorp, Lourens J. and van der Maas, Han L. J. and Maris, Gunter},
  journal = {Multivariate Behavioral Research},
  volume  = {53},
  number  = {1},
  pages   = {15--35},
  year    = {2018},
  doi     = {10.1080/00273171.2017.1379379}
}",
  "@article{marsman2025bayesian,
  title   = {Bayesian analysis of the ordinal {M}arkov random field},
  author  = {Marsman, Maarten and van den Bergh, Don and Haslbeck, Jonas M. B.},
  journal = {Psychometrika},
  volume  = {90},
  number  = {1},
  pages   = {146--182},
  year    = {2025},
  doi     = {10.1017/psy.2024.4}
}",
  "@article{huth2026statistical,
  title   = {Statistical evidence in psychological networks},
  author  = {Huth, Karoline B. S. and Haslbeck, Jonas M. B. and Keetelaar, Sara and van Holst, Ruth J. and Marsman, Maarten},
  journal = {Nature Human Behaviour},
  volume  = {10},
  number  = {2},
  pages   = {333--346},
  year    = {2026},
  doi     = {10.1038/s41562-025-02314-2}
}",
  "@article{williams2020bggm,
  title   = {{BGGM}: {B}ayesian {G}aussian graphical models in {R}},
  author  = {Williams, Donald R. and Mulder, Joris},
  journal = {Journal of Open Source Software},
  volume  = {5},
  number  = {51},
  pages   = {2111},
  year    = {2020},
  doi     = {10.21105/joss.02111}
}",
  "@article{huth2024easybgm,
  title   = {Simplifying {B}ayesian analysis of graphical models for the social sciences with easybgm: {A} user-friendly {R}-package},
  author  = {Huth, Karoline B. S. and Keetelaar, Sara and Sekulovski, Nikola and van den Bergh, Don and Marsman, Maarten},
  journal = {Advances.in/Psychology},
  volume  = {2},
  pages   = {e66366},
  year    = {2024},
  doi     = {10.56296/aip00010}
}"
)

entry_key <- function(entry) sub("^@\\w+\\{([^,]+),.*$", "\\1", trimws(entry))
existing_keys <- if (file.exists(bib_file)) {
  bib_lines <- readLines(bib_file)
  key_lines <- grep("^@\\w+\\{", bib_lines, value = TRUE)
  vapply(key_lines, entry_key, character(1), USE.NAMES = FALSE)
} else character(0)
new_entries <- manual_entries[!vapply(manual_entries, entry_key, character(1)) %in% existing_keys]
if (length(new_entries) > 0) {
  cat(paste0(new_entries, "\n"), file = bib_file, append = TRUE, sep = "\n")
  message(length(new_entries), " manual citation(s) appended to ", bib_file)
}
