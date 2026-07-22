# fit_vs_imv_compute.R
#
# Does a table's statistical fit diagnostics (M2 limited-information test,
# item-level S-X2, person-level Zh) agree with its predictive-accuracy gain
# (IMV) from adding a discrimination parameter (Rasch -> 2PL)? Motivated by
# Sinharay & Monroe (2025)'s review of IRT fit assessment, whose own
# real-data demonstration reports several diagnostics jointly (including a
# person-fit statistic) and whose conclusions explicitly flag predictive-fit
# approaches (Stenhaug & Domingue, 2022 -- the same lineage IMV comes from)
# as not covered in their review. Also serves as a multi-table look at how
# large IMV(Rasch, 2PL) typically is across real IRW instruments (the
# "enhanced IMV vignette" idea folded into this one).
#
# PILOT NOTE: this run is restricted to dichotomous tables only, matching
# imv.qmd's own Rasch-vs-2PL comparison exactly. Polytomous tables need a
# different baseline/richer-model pair (e.g. constrained vs. graded) that
# hasn't been validated here yet -- left for a future extension, not
# attempted in this pilot.
#
# Output: fit_vs_imv_data/fit_vs_imv_results.rds
#         fit_vs_imv_data/references.bib
#
# Usage:
#   Rscript vignettes/fit_vs_imv_compute.R   # from project root

library(irw)
library(mirt)
library(imv)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

set.seed(20260722)

out_dir  <- "vignettes/fit_vs_imv_data"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
bib_file <- file.path(out_dir, "references.bib")

MIN_ITEMS        <- 5
MAX_ITEMS        <- 40
MIN_PARTICIPANTS <- 500
MAX_N            <- 10000  # downsample respondents, mirrors 2pl_across_datasets_compute.R
SIG_ALPHA        <- 0.05   # conventional significance threshold, uncorrected

PILOT          <- TRUE   # TRUE: small hand-picked/random subset for a draft page
PILOT_N_TABLES <- 10

# ==============================================================================
# 1. Select datasets
# ==============================================================================

all_candidates <- irw_filter(
  n_categories   = 2,             # dichotomous only -- see pilot note above
  n_items        = c(MIN_ITEMS, MAX_ITEMS),
  n_participants = c(MIN_PARTICIPANTS, Inf)
)

message("Candidate tables (dichotomous, n_items ", MIN_ITEMS, "-", MAX_ITEMS,
        ", n_participants >= ", MIN_PARTICIPANTS, "): ", length(all_candidates))

if (PILOT) {
  # gilbert_meta_2 is imv.qmd's own baseline table -- include it so the pilot
  # can be sanity-checked directly against that vignette's already-published
  # IMV number, plus a random top-up for variety.
  known_good <- intersect(c("gilbert_meta_2"), all_candidates)
  remainder  <- setdiff(all_candidates, known_good)
  tables <- c(known_good, sample(remainder, min(PILOT_N_TABLES - length(known_good), length(remainder))))
  message("PILOT run: ", length(tables), " tables selected.")
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

fit_and_compare <- function(table_name) {
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

  ni <- ncol(resp)

  # Baseline (Rasch / 1PL) and richer (2PL, lognormal prior on a) models --
  # identical specification to imv.qmd and 2pl_across_datasets_compute.R.
  m0 <- tryCatch(mirt(resp, 1, "Rasch", verbose = FALSE), error = function(e) {
    message("    Rasch fit failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(m0)) return(NULL)

  model_spec <- mirt.model(paste0(
    "F = 1-", ni, "\n",
    "PRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)"
  ))
  m1 <- tryCatch(
    mirt(resp, model_spec, itemtype = rep("2PL", ni),
         method = "EM", technical = list(NCYCLES = 2000), verbose = FALSE),
    error = function(e) {
      message("    2PL fit failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(m1)) return(NULL)

  # IMV(Rasch, 2PL) -- imv() does 5-fold CV internally. whole.matrix = TRUE
  # (the default) can fail for small item/respondent counts with "Sample
  # sizes don't support whole.matrix = TRUE"; fall back to whole.matrix =
  # FALSE (per-cell rather than per-person-vector folds) when that happens.
  imv_res <- tryCatch(imv(m0, m1), error = function(e) {
    if (grepl("whole.matrix", conditionMessage(e))) {
      tryCatch(imv(m0, m1, whole.matrix = FALSE), error = function(e2) {
        message("    imv() failed (both whole.matrix settings): ", conditionMessage(e2))
        NULL
      })
    } else {
      message("    imv() failed: ", conditionMessage(e))
      NULL
    }
  })
  if (is.null(imv_res)) return(NULL)

  # Statistical fit diagnostics on the richer (2PL) model
  m2_res <- tryCatch(M2(m1, calcNull = FALSE), error = function(e) {
    message("    M2() failed: ", conditionMessage(e))
    NULL
  })

  ifit_res <- tryCatch(itemfit(m1, na.rm = TRUE), error = function(e) {
    message("    itemfit() failed: ", conditionMessage(e))
    NULL
  })

  # Person-fit: mirt::personfit()'s Zh column is Drasgow, Levine & Williams'
  # (1985) l_z statistic -- one of the three person-fit statistics Sinharay
  # & Monroe name in their review (alongside Tatsuoka's caution indices and
  # Snijders' (2001) more refined l_z*, which their own appendix code uses
  # via the PerFit package). We use Zh/l_z rather than l_z* to avoid PerFit's
  # heavy transitive dependency chain (kernlab/hdrcde/ks/rainbow/fds/fda);
  # stated explicitly in the vignette text as a substitution, not silently.
  pfit_res <- tryCatch(personfit(m1), error = function(e) {
    message("    personfit() failed: ", conditionMessage(e))
    NULL
  })

  if (is.null(m2_res) && is.null(ifit_res) && is.null(pfit_res)) {
    message("    skipped: all statistical fit diagnostics failed")
    return(NULL)
  }

  n_items_flagged    <- if (!is.null(ifit_res)) sum(ifit_res$p.S_X2 < SIG_ALPHA, na.rm = TRUE) else NA_integer_
  prop_items_flagged <- if (!is.null(ifit_res)) n_items_flagged / nrow(ifit_res) else NA_real_

  # Two-tailed critical value matching SIG_ALPHA (Zh ~ N(0,1) under the null)
  zh_crit <- qnorm(1 - SIG_ALPHA / 2)
  n_persons_flagged    <- if (!is.null(pfit_res)) sum(abs(pfit_res$Zh) > zh_crit, na.rm = TRUE) else NA_integer_
  prop_persons_flagged <- if (!is.null(pfit_res)) n_persons_flagged / nrow(pfit_res) else NA_real_

  meta_row <- if (!is.null(tags_meta)) tags_meta[tags_meta$table == table_name, ] else NULL
  construct_type <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$construct_type[1] else NA_character_

  tibble(
    table               = table_name,
    n_items             = ni,
    n_participants      = nrow(resp),
    construct_type      = construct_type,
    imv_mean            = imv_res$mean,
    imv_sd              = imv_res$sd,
    imv_ci_lower        = imv_res$ci["lower"],
    imv_ci_upper        = imv_res$ci["upper"],
    M2                  = if (!is.null(m2_res)) m2_res$M2[1] else NA_real_,
    M2_df               = if (!is.null(m2_res)) m2_res$df[1] else NA_real_,
    M2_p                = if (!is.null(m2_res)) m2_res$p[1] else NA_real_,
    M2_RMSEA            = if (!is.null(m2_res)) m2_res$RMSEA[1] else NA_real_,
    n_items_flagged      = n_items_flagged,
    prop_items_flagged   = prop_items_flagged,
    n_persons_flagged    = n_persons_flagged,
    prop_persons_flagged = prop_persons_flagged,
    M2_flagged           = if (!is.null(m2_res)) m2_res$p[1] < SIG_ALPHA else NA,
    itemfit_flagged      = if (!is.na(prop_items_flagged)) prop_items_flagged > 0 else NA,
    personfit_flagged    = if (!is.na(prop_persons_flagged)) prop_persons_flagged > 0 else NA
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
  result <- tryCatch(fit_and_compare(table_name), error = function(e) {
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

all_summary <- map(tables, function(tbl) {
  f <- file.path(fits_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact() |> bind_rows()

message("\nDone. ", nrow(all_summary), " tables with usable results out of ",
        length(tables), " candidates.")

# ==============================================================================
# 5. Save combined output
# ==============================================================================

saveRDS(
  list(
    summary          = all_summary,
    candidate_tables = tables,
    n_all_candidates = length(all_candidates),
    sig_alpha        = SIG_ALPHA,
    pilot            = PILOT,
    date_run         = Sys.Date(),
    session          = sessionInfo()
  ),
  file = file.path(out_dir, "fit_vs_imv_results.rds")
)

message("Saved to ", out_dir, "/fit_vs_imv_results.rds")

# ==============================================================================
# 6. Generate citations
# ==============================================================================

tryCatch(
  irw_save_bibtex(unique(all_summary$table), output_file = bib_file),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)

manual_entries <- c(
  "@article{sinharay2025assessment,
  title   = {Assessment of fit of item response theory models: {A} critical review of the status quo and some future directions},
  author  = {Sinharay, Sandip and Monroe, Scott},
  journal = {British Journal of Mathematical and Statistical Psychology},
  volume  = {78},
  pages   = {711--733},
  year    = {2025},
  doi     = {10.1111/bmsp.12378}
}",
  "@article{domingue2025imv,
  title   = {The {InterModel} {Vigorish} as a lens for understanding (and quantifying) the value of item response models for dichotomously coded items},
  author  = {Domingue, Benjamin W. and Kanopka, Klint and Kapoor, Radhika and Pohl, Steffi and Chalmers, R. Philip and Rahal, Charles and Rhemtulla, Mijke},
  journal = {Psychometrika},
  volume  = {89},
  number  = {3},
  pages   = {1034--1054},
  year    = {2025},
  doi     = {10.1007/s11336-024-09977-2}
}",
  "@article{sinharayhaberman2014,
  title   = {How often is the misfit of item response theory models practically significant?},
  author  = {Sinharay, Sandip and Haberman, Shelby J.},
  journal = {Educational Measurement: Issues and Practice},
  volume  = {33},
  number  = {1},
  pages   = {23--35},
  year    = {2014},
  doi     = {10.1111/emip.12024}
}",
  "@article{zhao2017promis,
  title   = {Impact of {IRT} item misfit on score estimates and severity classifications: {An} examination of {PROMIS} depression and pain interference item banks},
  author  = {Zhao, Yue},
  journal = {Quality of Life Research},
  volume  = {26},
  number  = {3},
  pages   = {555--564},
  year    = {2017},
  doi     = {10.1007/s11136-016-1467-3}
}",
  "@article{stenhaug2022predictive,
  title   = {Predictive fit metrics for item response models},
  author  = {Stenhaug, Benjamin A. and Domingue, Benjamin W.},
  journal = {Applied Psychological Measurement},
  volume  = {46},
  number  = {2},
  pages   = {136--155},
  year    = {2022},
  doi     = {10.1177/01466216211066603}
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
