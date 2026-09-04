# guessing_key_diagnostics.R -- per-table answer-key sanity checks.
#
# Motivation: `enem_2019_1mil_lc` behaves unlike the other ten tables (tiny
# estimated latent SD, a median raw score below the chance floor) and the
# scored-absence screen does not touch it -- it carries no zero block at the
# candidate level. This script asks the prior question the model fits cannot:
# do these responses look like they were scored against the right key?
#
# Its answer is what excluded that table from guessing_compute.R's TABLES on
# 2026-09-04, so this script deliberately runs over the full CANDIDATE set,
# including the excluded table, rather than over the tables that survived.
# Take the exclusion out of the diagnostics and the page loses the evidence
# for its own decision.
#
# Two statistics per table, both on the sample the models are actually fit to
# (after the scored-absence screen, before the zero-variance item drop, so the
# drop itself is visible):
#
#   1. Item difficulty against the chance floor. On an m-option multiple-choice
#      test scored with the right key, p_j < 1/m should be rare: a candidate
#      who knows nothing still guesses right 1/m of the time. Items sitting
#      well below 1/m across a whole form are a key problem, not hard items.
#
#   2. Item-total point-biserial. This is the sharper of the two and does not
#      depend on knowing m. A correctly keyed item correlates positively with
#      the rest of the test; an item keyed to the wrong option correlates with
#      it near zero or negatively, however hard or easy it looks.
#
# Neither statistic is a model fit, so neither can be explained away by the
# guessing models on this page being wrong. Runs in seconds off the cached
# `prepared/` matrices.
#
# Output: vignettes/guessingdata/guessing_key_diagnostics.rds
#
# Usage:
#   Rscript vignettes/guessing_key_diagnostics.R   # from project root

source("vignettes/guessing_helpers.R")
library(dplyr)
library(tibble)
library(purrr)

out_dir  <- "vignettes/guessingdata"
prep_dir <- file.path(out_dir, "prepared")

# Same table/m list as guessing_compute.R. Kept as a literal rather than
# sourced from that script because sourcing it would launch the whole fit.
TABLES <- tribble(
  ~table,                ~m,
  "enem_2013_1mil_mt",    5,
  "enem_2013_1mil_lc",    5,
  "enem_2013_1mil_ch",    5,
  "enem_2013_1mil_cn",    5,
  "enem_2014_1mil_ch",    5,
  "enem_2019_1mil_ch",    5,
  "enem_2019_1mil_lc",    5,
  "enem_2024_1mil_ch",    5,
  "gilbert_meta_1",       4,
  "gilbert_meta_102",     5,
  "gilbert_meta_103",     5
)

# Point-biserial of each item against the total of the OTHER items. Correcting
# for the item's own contribution matters at these J: with J = 30 an uncorrected
# item-total correlation carries a floor of roughly 1/sqrt(J) from the item
# being inside its own total, which is enough to hide a dead item.
rest_cor <- function(M) {
  tot <- rowSums(M, na.rm = TRUE)
  vapply(seq_len(ncol(M)), function(j) {
    yj <- M[, j]
    rest <- tot - ifelse(is.na(yj), 0, yj)
    suppressWarnings(stats::cor(yj, rest, use = "pairwise.complete.obs"))
  }, numeric(1))
}

diagnose_one <- function(table_name, m) {
  f <- file.path(prep_dir, paste0(table_name, ".rds"))
  if (!file.exists(f)) {
    message("  no prepared matrix, skipping: ", table_name)
    return(NULL)
  }
  resp <- readRDS(f)

  # Match guessing_compute.R's person screen so these numbers describe the
  # analysis sample, not the raw draw.
  resp <- screen_scored_absences(resp, m)$resp
  M <- as.matrix(resp)

  p_all <- colMeans(M, na.rm = TRUE)
  degenerate <- is.na(p_all) | p_all %in% c(0, 1)

  # Elective blocks: items a large share of candidates never see. ENEM's
  # language paper offers English or Spanish, five items each, and IRW carries
  # the block the candidate did not choose as missing.
  na_rate <- colMeans(is.na(M))
  elective <- na_rate > 0.4

  # Analysis matrix: what the models see.
  A <- M[, !degenerate, drop = FALSE]
  p_a <- colMeans(A, na.rm = TRUE)
  r_a <- rest_cor(A)

  tibble(
    table = table_name, m = m,
    n_persons = nrow(M), n_items_all = ncol(M),
    n_items_degenerate = sum(degenerate),
    degenerate_items = paste(colnames(M)[degenerate], collapse = ", "),
    n_items_elective = sum(elective),
    # Of the elective items, how many are among the all-zero ones: this is the
    # signature of an entire elective block scored wrong for everyone who sat
    # it, as opposed to scattered degenerate items.
    n_elective_degenerate = sum(elective & degenerate),
    # Candidates who sat the elective items that came back with no variance:
    # the size of the block the scoring failed on, where there is one.
    n_sat_degenerate_elective = if (any(elective & degenerate)) {
      max(colSums(!is.na(M[, elective & degenerate, drop = FALSE])))
    } else NA_integer_,
    # Difficulty range of the elective items that survived, for contrast with
    # the common items: a broken block reads as impossible, and the intact
    # block on the same table can read as unusually easy.
    elective_ok_p_min = if (any(elective & !degenerate)) {
      min(p_all[elective & !degenerate])
    } else NA_real_,
    elective_ok_p_max = if (any(elective & !degenerate)) {
      max(p_all[elective & !degenerate])
    } else NA_real_,
    n_items_analysed = ncol(A),
    p_min = min(p_a), p_med = stats::median(p_a), p_max = max(p_a),
    frac_p_below_chance = mean(p_a < 1 / m),
    rpb_med = stats::median(r_a, na.rm = TRUE),
    rpb_min = min(r_a, na.rm = TRUE),
    frac_rpb_below_05 = mean(r_a < 0.05, na.rm = TRUE)
  )
}

diagnostics <- map2(TABLES$table, TABLES$m, diagnose_one) |> compact() |> bind_rows()

print(as.data.frame(diagnostics[, c("table", "n_items_analysed", "p_med",
                                    "frac_p_below_chance", "rpb_med",
                                    "frac_rpb_below_05")]), digits = 3)

saveRDS(
  list(diagnostics = diagnostics, date_run = Sys.Date(), session = sessionInfo()),
  file.path(out_dir, "guessing_key_diagnostics.rds")
)
message("\nWrote ", file.path(out_dir, "guessing_key_diagnostics.rds"))
