# landing/hero_stats.R -- pre-render step (see _quarto.yml)
#
# Writes data/hero_stats.json, the homepage hero's numbers, from the PUBLISHED
# irw_meta on Redivis, the same source every other page reads at render time.
# Ruled in ben-domingue/irw#1940 (B3): the banner counts what a user can fetch,
# not the repo's metadata.csv. That keeps the site agreeing with itself, and the
# banner can no longer go stale between hand-run commits: it is rebuilt on every
# render.
#
# Ported from ben-domingue/irw metadata/09_hero_status.R, which used to write
# this file from a local metadata.csv and was committed here by hand. The
# computation is unchanged:
#
#   - totals: n_tables, n_responses, n_participants, n_items -- direct sums
#     across all tables -- plus n_tables_itemtext, the tables with a row in
#     itemtext_metadata (case-insensitive, as in 11_status.R, so the hero and
#     the status page agree).
#   - n_tables_comp, n_tables_nom, n_tables_conj: table counts for the
#     competition, nominal and conjoint families, for one small line on the
#     hero. Omitted on a failed read.
#   - category_breakdown: tables bucketed by their own aggregate n_categories
#     (2/3/4/5/6/7+, excluding <2 and >=12), with n_items and n_responses summed
#     within each bucket. A table-level approximation of the paper's item-level
#     Table 2; see the footer copy in components/_hero_playful.qmd.
#
# Failure: in CI (env CI set) a failed read stops the render, as the landing pages
# do (ben-domingue/irw#2129), so a broken token cannot publish a stale banner.
# Locally it warns and leaves data/hero_stats.json alone; with no file the hero
# renders its fallback values.

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(redivis)
})

out_path <- "data/hero_stats.json"

read_meta <- function() {
  # Tables by NAME, never name:referenceId -- ids rotate on every release.
  irw_meta <- redivis$user("datapages")$dataset("irw_meta:bdxt")
  list(
    version  = irw_meta$get()$properties$version$tag,
    meta     = irw_meta$table("metadata")$to_tibble(),
    itemtext = irw_meta$table("itemtext_metadata")$to_tibble()
  )
}

got <- tryCatch(read_meta(), error = function(e) e)
if (inherits(got, "error")) {
  msg <- paste0("landing/hero_stats.R: could not read irw_meta (", conditionMessage(got), ")")
  if (nzchar(Sys.getenv("CI"))) stop(msg, call. = FALSE)
  warning(msg, " -- leaving ", out_path, " as it is.", call. = FALSE)
  quit(save = "no", status = 0)
}

meta <- as.data.frame(got$meta)

# Table counts for the competition, nominal and conjoint families: a small line
# under the hero's table count, not part of the totals above. Their metadata
# sits in irw_meta beside the core table, where the irw package reads it for
# source = "comp" / "nom" / "conj". Soft on failure -- a missing count just
# drops the family from the line -- since these families should not be able to
# block a publish. One row per table in each, across shards for conjoint.
count_branch <- function(table) {
  tryCatch(
    nrow(redivis$user("datapages")$dataset("irw_meta:bdxt")$table(table)$to_tibble()),
    error = function(e) {
      warning("landing/hero_stats.R: could not count ", table, " (",
              conditionMessage(e), ")", call. = FALSE)
      NULL
    }
  )
}

totals <- list(
  n_tables       = nrow(meta),
  n_responses    = sum(as.numeric(meta$n_responses), na.rm = TRUE),
  n_participants = sum(as.numeric(meta$n_participants), na.rm = TRUE),
  # round(): some tables' n_items are non-integer upstream.
  n_items        = round(sum(as.numeric(meta$n_items), na.rm = TRUE))
)

key <- function(x) tolower(trimws(as.character(x)))
totals$n_tables_itemtext <- sum(key(meta$table) %in% key(got$itemtext$table))
totals$n_tables_comp <- count_branch("comps_metadata")
totals$n_tables_nom  <- count_branch("nominal_metadata")
totals$n_tables_conj <- count_branch("conj_metadata")

MIN_CATEGORIES <- 2   # single-category (no-variance) tables excluded
MAX_CATEGORIES <- 11  # inclusive; >=12 categories excluded, per paper Sec. 2.2

bucket_for <- function(n) {
  if (is.na(n) || n < MIN_CATEGORIES || n > MAX_CATEGORIES) return(NA_character_)
  if (n >= 7) "7+" else as.character(n)
}

classified <- meta |>
  mutate(bucket = vapply(as.numeric(n_categories), bucket_for, character(1))) |>
  filter(!is.na(bucket)) |>
  group_by(bucket) |>
  summarise(n_items = sum(as.numeric(n_items), na.rm = TRUE),
            n_responses = sum(as.numeric(n_responses), na.rm = TRUE), .groups = "drop")

# One row per known bucket, in fixed order; an empty bucket is a zero, and the
# front end hides its badge. Plain numeric, not integer: summed responses
# exceed R's 32-bit integer limit.
all_buckets <- c("2", "3", "4", "5", "6", "7+")
breakdown <- lapply(all_buckets, function(b) {
  row <- classified[classified$bucket == b, ]
  list(
    n_categories = if (b == "7+") "7+" else as.integer(b),
    n_items      = if (nrow(row)) round(row$n_items) else 0,
    n_responses  = if (nrow(row)) as.numeric(row$n_responses) else 0
  )
})

hero <- list(
  generated_at       = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  irw_meta_version   = got$version,
  totals             = totals,
  category_breakdown = breakdown
)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write_json(hero, out_path, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA)
message("landing/hero_stats.R: wrote ", out_path, " from irw_meta ", got$version,
        " (", format(totals$n_tables, big.mark = ","), " tables)")
