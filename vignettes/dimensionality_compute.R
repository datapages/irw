# dimensionality_compute.R
#
# Every unidimensional IRT model used elsewhere on this site (1PL/2PL,
# IMV comparisons) assumes a single latent trait explains the item
# correlations. How often does that assumption actually hold up across
# real IRW instruments, and does it vary by construct type? For each
# eligible table, this fits a tetrachoric/polychoric correlation matrix
# to the item responses and runs two classic exploratory diagnostics:
# the ratio of the 1st to 2nd eigenvalue, and parallel analysis
# (Horn, 1965) for a suggested number of factors.
#
# Produces the precomputed results loaded by dimensionality.qmd.
#
# Output: dimensionality_data/dimensionality_results.rds
#         dimensionality_data/references.bib
#
# Usage:
#   Rscript vignettes/dimensionality_compute.R   # from project root

library(irw)
library(psych)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

set.seed(20260720)

out_dir  <- "vignettes/dimensionality_data"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
bib_file <- file.path(out_dir, "references.bib")

MIN_ITEMS        <- 5
MAX_ITEMS        <- 40     # polychoric matrices + parallel analysis scale poorly beyond this
MIN_PARTICIPANTS <- 200
MAX_N             <- 10000 # downsample respondents before computing correlations, mirrors 2pl_across_datasets_compute.R
RATIO_CUTOFF      <- 4     # heuristic "clearly unidimensional" reference line -- not a hard rule
PILOT             <- FALSE # TRUE: run on a small hand-picked/sampled subset for a draft page
PILOT_N_TABLES    <- 15

# ==============================================================================
# 1. Select datasets
# ==============================================================================

all_candidates <- irw_filter(
  n_items        = c(MIN_ITEMS, MAX_ITEMS),
  n_participants = c(MIN_PARTICIPANTS, Inf)
)

message("Candidate tables (n_items ", MIN_ITEMS, "-", MAX_ITEMS,
        ", n_participants >= ", MIN_PARTICIPANTS, "): ", length(all_candidates))

if (PILOT) {
  # Small draft run: a couple of known-good tables with an expected answer
  # (a classic multi-factor personality inventory, a single-construct math
  # test) plus a random top-up for a mix of construct types/item formats.
  known_good <- intersect(c("psychtools_bfi", "4thgrade_math_sirt"), all_candidates)
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

fit_dimensionality <- function(table_name) {
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
    cor_method <- "tetrachoric"
    cormat <- tryCatch(
      suppressWarnings(suppressMessages(psych::tetrachoric(resp)$rho)),
      error = function(e) {
        message("    tetrachoric failed: ", conditionMessage(e))
        NULL
      }
    )
  } else if (n_categories >= 3 && n_categories <= 10) {
    cor_method <- "polychoric"
    cormat <- tryCatch(
      suppressWarnings(suppressMessages(psych::polychoric(resp, max.cat = 10, progress = FALSE)$rho)),
      error = function(e) {
        message("    polychoric failed: ", conditionMessage(e))
        NULL
      }
    )
  } else {
    cor_method <- "pearson"
    message("    n_categories = ", n_categories, " outside 2-10; falling back to Pearson correlations")
    cormat <- tryCatch(
      cor(resp, use = "pairwise.complete.obs"),
      error = function(e) {
        message("    Pearson correlation failed: ", conditionMessage(e))
        NULL
      }
    )
  }
  if (is.null(cormat) || anyNA(cormat)) {
    message("    skipped: correlation matrix unusable (NULL or contains NA)")
    return(NULL)
  }

  ni <- ncol(resp)

  ev <- tryCatch(eigen(cormat, symmetric = TRUE, only.values = TRUE)$values, error = function(e) NULL)
  if (is.null(ev) || length(ev) < 2) {
    message("    skipped: eigen decomposition failed")
    return(NULL)
  }
  ratio_12    <- ev[1] / ev[2]
  prop_var_1  <- ev[1] / sum(ev)

  # Parallel analysis against a random-data eigenvalue baseline (Horn, 1965).
  # Wrapped in tryCatch -- can fail on small/ill-conditioned matrices.
  pa <- tryCatch(
    psych::fa.parallel(cormat, n.obs = nrow(resp), fa = "fa", plot = FALSE),
    error = function(e) {
      message("    fa.parallel failed: ", conditionMessage(e))
      NULL
    }
  )
  nfact_suggested <- if (!is.null(pa)) pa$nfact else NA_integer_

  meta_row <- if (!is.null(tags_meta)) tags_meta[tags_meta$table == table_name, ] else NULL
  construct_type <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$construct_type[1] else NA_character_
  item_format    <- if (!is.null(meta_row) && nrow(meta_row) > 0) meta_row$item_format[1] else NA_character_

  list(
    summary = tibble(
      table              = table_name,
      n_items            = ni,
      n_participants     = nrow(resp),
      n_categories       = n_categories,
      cor_method         = cor_method,
      ratio_12           = ratio_12,
      prop_var_1         = prop_var_1,
      nfact_suggested    = nfact_suggested,
      unidimensional_pa  = !is.na(nfact_suggested) && nfact_suggested <= 1,
      construct_type     = construct_type,
      item_format        = item_format
    ),
    eigenvalues = ev
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
  result <- tryCatch(fit_dimensionality(table_name), error = function(e) {
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

result_names    <- map_chr(all_raw, function(r) r$summary$table)
all_eigenvalues <- set_names(map(all_raw, "eigenvalues"), result_names)

message("\nDone. ", nrow(all_summary), " tables with usable dimensionality results out of ",
        length(tables), " candidates.")

# ==============================================================================
# 5. Save combined output
# ==============================================================================

saveRDS(
  list(
    summary          = all_summary,
    eigenvalues      = all_eigenvalues,
    candidate_tables = tables,
    n_all_candidates = length(all_candidates),
    ratio_cutoff     = RATIO_CUTOFF,
    pilot            = PILOT,
    date_run         = Sys.Date(),
    session          = sessionInfo()
  ),
  file = file.path(out_dir, "dimensionality_results.rds")
)

message("Saved to ", out_dir, "/dimensionality_results.rds")

# ==============================================================================
# 6. Generate citations
#    irw_save_bibtex() takes the full vector of table names in one call and
#    overwrites bib_file each run, so the two hand-written methods citations
#    below (Horn's parallel analysis, Garrido et al.'s ordinal-PA study) are
#    appended afterward rather than written into bib_file directly.
# ==============================================================================

tryCatch(
  irw_save_bibtex(unique(all_summary$table), output_file = bib_file),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)

manual_entries <- c(
  "@article{horn1965rationale,
  title   = {A rationale and test for the number of factors in factor analysis},
  author  = {Horn, John L.},
  journal = {Psychometrika},
  volume  = {30},
  number  = {2},
  pages   = {179--185},
  year    = {1965},
  doi     = {10.1007/BF02289447}
}",
  "@book{embretson2000item,
  title     = {Item Response Theory for Psychologists},
  author    = {Embretson, Susan E. and Reise, Steven P.},
  year      = {2000},
  publisher = {Lawrence Erlbaum Associates},
  address   = {Mahwah, NJ},
  isbn      = {0-8058-2818-4}
}",
  "@article{garrido2013new,
  title   = {A new look at {H}orn's parallel analysis with ordinal variables},
  author  = {Garrido, Luis Eduardo and Abad, Francisco Jos\\'{e} and Ponsoda, Vicente},
  journal = {Psychological Methods},
  volume  = {18},
  number  = {4},
  pages   = {454--474},
  year    = {2013},
  doi     = {10.1037/a0030005}
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
