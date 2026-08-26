# dimensionality_merge_scout.R
#
# SCOUTING ONLY -- not part of the render path, and not called by
# dimensionality_compute.R. Run this once by hand to find IRW merge groups
# suitable for the "known dimensionality" section of dimensionality.qmd, then
# hand-vet the output and freeze the survivors into a static MERGE_GROUPS
# literal in dimensionality_compute.R.
#
# Why not just call irw_merge() at render time?
#
#   irw_merge() groups tables by shared DOI (falling back to BibTex) and
#   rbinds them. Its consistency checks -- equal N, shared IDs, no item
#   overlap -- are reported as messages and then confirmed via a yes/no
#   prompt. As of the 2026 update that prompt short-circuits in a
#   non-interactive session and returns its default (TRUE), so an unattended
#   run proceeds past every one of those checks. For a vignette claiming
#   "this merged matrix has at least k known dimensions" we want the checks
#   to be hard filters, not warnings. We also want a frozen mapping so the
#   published figure doesn't drift as IRW's bibliography grows.
#
#   So: this script reimplements irw_merge()'s grouping logic, applies the
#   same checks as filters, and reports what survives. The vignette still
#   points readers at irw_merge() as the discovery path.
#
# Two stages:
#   Stage 1 (cheap, metadata only) -- group by DOI/BibTex, filter on group
#     size, equal N, and total item count. Writes the full ranked list.
#   Stage 2 (expensive, fetches data) -- for the top-scoring groups, verify
#     that member tables really share respondents (genuine id intersection)
#     and really have disjoint items. Per-table id/item summaries are cached
#     so reruns are cheap.
#
# Output: dimensionality_data/merge_scout_stage1.csv   (all metadata-eligible groups)
#         dimensionality_data/merge_scout_stage2.csv   (verified subset, for hand review)
#         dimensionality_data/merge_scout_cache/*.rds  (per-table id/item summaries)
#
# Usage:
#   Rscript vignettes/dimensionality_merge_scout.R   # from project root

library(irw)
library(dplyr)
library(purrr)
library(tibble)

set.seed(20260817)

out_dir   <- "vignettes/dimensionality_data"
cache_dir <- file.path(out_dir, "merge_scout_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# --- Stage 1 filters (metadata only) ------------------------------------
MIN_TABLES     <- 3    # need >= 3 instruments for "n dimensions" to say much
MAX_TABLES     <- 12   # beyond this the merge is usually an item bank split by topic
MIN_ITEMS_TOT  <- 20
MAX_ITEMS_TOT  <- 150  # polychoric is O(items^2) pairs; 150 items ~= 11k pairs
MIN_N          <- 200
MAX_N          <- 50000

# --- Stage 2 budget -----------------------------------------------------
N_VERIFY       <- Inf     # how many top-scoring stage-1 groups to actually fetch
MAX_CELLS      <- 30e6    # skip a group whose members are collectively too big to fetch

# ==============================================================================
# 1. Group tables the way irw_merge() does
# ==============================================================================

# Mirrors irw:::generate_doi_bibtex_mapping() + find_merge_candidates():
# DOI takes precedence, BibTex is the fallback for tables with no usable DOI.
bib <- irw:::.fetch_biblio_table()
md  <- irw_metadata()
md  <- md[!duplicated(md$table), ]

present <- function(x) !is.na(x) & nzchar(trimws(x)) & trimws(x) != "NA"

group_key <- ifelse(
  present(bib$DOI__for_paper_),
  paste0("doi:", trimws(bib$DOI__for_paper_)),
  ifelse(present(bib$BibTex), paste0("bib:", trimws(bib$BibTex)), NA_character_)
)

groups <- split(bib$table, group_key)
groups <- groups[lengths(groups) > 1]
groups <- lapply(groups, function(x) unique(x[x %in% md$table]))
groups <- groups[lengths(groups) > 1]

message("Merge groups (size > 1, tables present in metadata): ", length(groups))

# ==============================================================================
# 2. Stage 1 -- metadata-only screen
# ==============================================================================

meta_of <- function(tbls) md[match(tbls, md$table), ]

stage1 <- imap_dfr(groups, function(tbls, key) {
  m <- meta_of(tbls)
  n_vec <- m$n_participants
  tibble(
    group_key   = key,
    anchor      = tbls[1],
    tables      = paste(tbls, collapse = "|"),
    k           = length(tbls),
    items_total = sum(m$n_items, na.rm = TRUE),
    items_min   = suppressWarnings(min(m$n_items, na.rm = TRUE)),
    n_min       = suppressWarnings(min(n_vec, na.rm = TRUE)),
    n_max       = suppressWarnings(max(n_vec, na.rm = TRUE)),
    same_n      = !anyNA(n_vec) && length(unique(n_vec)) == 1,
    cells_est   = sum(as.numeric(m$n_items) * as.numeric(m$n_participants), na.rm = TRUE)
  )
})

stage1 <- stage1 %>%
  mutate(
    eligible = same_n &
      k >= MIN_TABLES & k <= MAX_TABLES &
      items_total >= MIN_ITEMS_TOT & items_total <= MAX_ITEMS_TOT &
      n_min >= MIN_N & n_max <= MAX_N,
    # Prefer more instruments (a sharper lower bound on dimensionality) and a
    # healthy respondent count, penalising groups that are near the item cap.
    score = k + log10(pmax(n_min, 1)) - items_total / MAX_ITEMS_TOT
  ) %>%
  arrange(desc(eligible), desc(score))

write.csv(stage1, file.path(out_dir, "merge_scout_stage1.csv"), row.names = FALSE)
message("Stage 1: ", sum(stage1$eligible), " of ", nrow(stage1),
        " groups pass the metadata screen. Written to merge_scout_stage1.csv")

# ==============================================================================
# 3. Stage 2 -- verify shared respondents and disjoint items
# ==============================================================================

# Cache just what the checks need (id set, item set, counts), not the table
# itself -- the fetch is the expensive part and we only ever need summaries.
table_summary <- function(tbl) {
  f <- file.path(cache_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) return(readRDS(f))

  df <- tryCatch(irw_fetch(tbl), error = function(e) {
    message("    fetch failed for ", tbl, ": ", conditionMessage(e))
    NULL
  })
  if (is.null(df)) return(NULL)

  s <- list(
    table    = tbl,
    ids      = unique(na.omit(df$id)),
    items    = unique(na.omit(df$item)),
    n_resp   = length(unique(na.omit(df$id))),
    n_items  = length(unique(na.omit(df$item))),
    n_cat    = length(unique(na.omit(df$resp)))
  )
  saveRDS(s, f)
  s
}

verify_group <- function(tbls, key) {
  message("  Verifying: ", tbls[1], " (k = ", length(tbls), ")")
  summaries <- compact(map(tbls, table_summary))
  if (length(summaries) < MIN_TABLES) {
    return(tibble(group_key = key, anchor = tbls[1], verdict = "fetch_failed"))
  }

  id_sets   <- map(summaries, "ids")
  item_sets <- map(summaries, "items")

  shared_ids <- reduce(id_sets, intersect)
  all_ids    <- reduce(id_sets, union)
  n_shared   <- length(shared_ids)

  # irw_merge() cautions that 1..n IDs may be different subjects in different
  # studies sharing a numbering scheme. Recorded as a note, not a verdict: IRW
  # re-indexes respondents to 1..n on ingest, so this fires on most groups, and
  # the cross-study collision it guards against is largely ruled out by the
  # members sharing a DOI and having identical ID sets.
  ids_sequential <- FALSE
  if (n_shared > 0 && is.numeric(shared_ids)) {
    srt <- sort(shared_ids)
    ids_sequential <- identical(as.numeric(srt), as.numeric(seq(min(srt), max(srt))))
  }
  if (n_shared > 0 && !is.numeric(shared_ids)) {
    num <- suppressWarnings(as.numeric(shared_ids))
    if (!anyNA(num)) {
      srt <- sort(num)
      ids_sequential <- identical(srt, as.numeric(seq(min(srt), max(srt))))
    }
  }

  n_item_overlap <- 0L
  for (i in seq_len(length(item_sets) - 1)) {
    for (j in seq.int(i + 1, length(item_sets))) {
      n_item_overlap <- n_item_overlap + length(intersect(item_sets[[i]], item_sets[[j]]))
    }
  }

  id_coverage <- if (length(all_ids) > 0) n_shared / length(all_ids) else 0

  verdict <- if (n_shared == 0) {
    "no_shared_respondents"
  } else if (n_item_overlap > 0) {
    "item_overlap"
  } else if (id_coverage < 0.9) {
    "partial_respondent_overlap"
  } else {
    "ok"
  }

  tibble(
    group_key      = key,
    anchor         = tbls[1],
    tables         = paste(map_chr(summaries, "table"), collapse = "|"),
    k              = length(summaries),
    items_total    = sum(map_int(summaries, "n_items")),
    n_shared_ids   = n_shared,
    id_coverage    = round(id_coverage, 3),
    ids_sequential = ids_sequential,
    n_item_overlap = n_item_overlap,
    max_n_cat      = max(map_int(summaries, "n_cat")),
    verdict        = verdict
  )
}

to_verify <- stage1 %>%
  filter(eligible, cells_est <= MAX_CELLS) %>%
  head(N_VERIFY)

message("\nStage 2: fetching members of ", nrow(to_verify), " groups (",
        sum(stage1$eligible) - nrow(to_verify), " eligible groups not verified this run)...")

stage2 <- pmap_dfr(
  list(strsplit(to_verify$tables, "|", fixed = TRUE), to_verify$group_key),
  function(tbls, key) {
    tryCatch(verify_group(tbls, key), error = function(e) {
      message("    unexpected error: ", conditionMessage(e))
      tibble(group_key = key, anchor = tbls[1], verdict = "error")
    })
  }
)

stage2 <- stage2 %>% arrange(factor(verdict, levels = c(
  "ok", "partial_respondent_overlap",
  "item_overlap", "no_shared_respondents", "fetch_failed", "error"
)), desc(k))

write.csv(stage2, file.path(out_dir, "merge_scout_stage2.csv"), row.names = FALSE)

message("\n=== Stage 2 verdicts ===")
print(table(stage2$verdict))
message("\nWritten to merge_scout_stage2.csv. The 'ok' rows are the MERGE_GROUPS pool.")
message("Note: k (number of member tables) is an index of how many instruments were ",
        "stacked, not a ground-truth factor count -- some groups split one inventory ",
        "into per-subscale tables, others repeat a construct pre/post. The question ",
        "the vignette asks is how multidimensional merged batteries get, not whether ",
        "a diagnostic recovers a known k.")
