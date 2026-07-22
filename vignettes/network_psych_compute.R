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

  list(
    summary = tibble(
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
      has_ld_match        = nrow(ld_row) > 0
    ),
    graph    = graph,
    strength = strength,
    a        = a
  )
}

# ==============================================================================
# 3. Run, writing each result to disk as it completes
#    If the script crashes, re-running skips already-completed tables
# ==============================================================================

fit_to_disk <- function(table_name) {
  out_file <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
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

n_dim_matched <- sum(all_summary$has_dim_match)
n_ld_matched  <- sum(all_summary$has_ld_match)
message("\nDone. ", nrow(all_summary), " tables with usable network results out of ",
        length(tables), " candidates.")
message("  quality check: ", n_dim_matched, "/", nrow(all_summary),
        " found a matching dimensionality-vignette entry.")
message("  quality check: ", n_ld_matched, "/", nrow(all_summary),
        " found a matching local-dependence-vignette entry.")

# ==============================================================================
# 5. Save combined output
# ==============================================================================

saveRDS(
  list(
    summary          = all_summary,
    graphs           = all_graphs,
    strengths        = all_strengths,
    discriminations  = all_a,
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
