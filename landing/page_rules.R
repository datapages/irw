# Which IRW tables get a landing page, and in what state (ben-domingue/irw#1706).
#
# Sourced by BOTH landing/emit_landing_pages.R (which writes the pages, post-render)
# and data.qmd (which links to them from the "Information on selected dataset"
# panel). The emitter runs after data.qmd has rendered and cannot tell it anything,
# so this file is the contract: one rule, two readers, no drift.
#
# The rules, as decided by Ben on 2026-09-19 (see #1706):
#
# - Every live table gets a page, EXCEPT one with no recorded licence. A table
#   whose `Derived_License` is blank or "NA" gets no page until the licence is
#   known; the queue of those tables is ben-domingue/irw#2266.
# - A table with an open data defect still gets a page, carrying a banner that
#   names the issue, and is kept out of search (noindex, no Dataset JSON-LD, no
#   Croissant file, not in the sitemap) until the fix ships. The list is
#   landing/known_issues.tsv; delete a row when its fix is released.
# - A withdrawn table keeps its URL as a tombstone that says "Withdrawn" and the
#   date, and nothing else. The list is landing/withdrawn.tsv; add a row in the
#   same PR that withdraws a table, or its page becomes a 404 on the next publish.

# Shard name -> Redivis scoped reference. Mirrors the map in _load-data-explore.qmd;
# authoritative source is IRW_CORE_DATASETS in ben-domingue/irw metadata/redivis_config.R.
SHARD_REF <- c(
  item_response_warehouse   = "item_response_warehouse:as2e",
  item_response_warehouse_2 = "item_response_warehouse_2:epbx",
  item_response_warehouse_3 = "item_response_warehouse_3:5xaj",
  item_response_warehouse_4 = "item_response_warehouse_4:980f",
  item_response_warehouse_5 = "item_response_warehouse_5:3ykx",
  item_response_warehouse_6 = "item_response_warehouse_6:fpe6"
)

# The dictionary Sheets are hand-edited, so "missing" arrives in several spellings:
# a real NA, an empty cell, or the literal text "NA" / "N/A" / "NULL". All of them
# must count as absent, or they end up rendered as facts -- an early run emitted
# "https://doi.org/NA" as a citation and "NA" as a schema.org keyword.
blank <- function(x) {
  if (is.null(x) || length(x) == 0) return(TRUE)
  if (all(is.na(x))) return(TRUE)
  v <- trimws(as.character(x)[1])
  !nzchar(v) || toupper(v) %in% c("NA", "N/A", "NULL", "NONE", "-", "MISSING (NA)")
}

# URL slug rule: always lowercase. ~300 table names are not lowercase, and a
# case-sensitive host would serve Foo/ and foo/ as two pages while a
# case-insensitive one would collide them. The page displays the true name; only
# the path is folded. The emitter asserts no two names collapse to one slug.
slug_of <- function(x) tolower(x)

# A small tab-separated list with a header row; '#' starts a comment. Missing
# file -> empty frame, so a checkout without the file still renders.
read_landing_list <- function(file, cols) {
  path <- file.path("landing", file)
  empty <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                         stringsAsFactors = FALSE)
  if (!file.exists(path)) return(empty)
  ln <- readLines(path, warn = FALSE)
  ln <- ln[nzchar(trimws(ln)) & !grepl("^\\s*#", ln)]
  if (length(ln) < 2) return(empty)
  df <- utils::read.delim(text = paste(ln, collapse = "\n"), colClasses = "character",
                          stringsAsFactors = FALSE)
  df[] <- lapply(df, trimws)
  df <- df[nzchar(df$table), cols, drop = FALSE]
  df[order(tolower(df$table)), , drop = FALSE]
}

known_issues  <- function() read_landing_list("known_issues.tsv", c("table", "issue"))
withdrawn_tbl <- function() read_landing_list("withdrawn.tsv",    c("table", "date"))

# The tables that get a full landing page. `md` is irw_meta's metadata table,
# `bib` its biblio table; `live` is optionally the table names Redivis actually
# lists, so a table still in metadata after it left Redivis gets no page.
# Returns the true table names, sorted.
page_tables <- function(md, bib, live = NULL) {
  md_names <- trimws(as.character(md$table))
  in_shard <- as.character(md$dataset) %in% names(SHARD_REF)
  lic <- setNames(as.character(bib$Derived_License), tolower(trimws(as.character(bib$table))))
  has_lic <- vapply(tolower(md_names), function(k) !blank(lic[k]), logical(1))
  keep <- in_shard & has_lic & !(tolower(md_names) %in% tolower(withdrawn_tbl()$table))
  if (!is.null(live)) keep <- keep & tolower(md_names) %in% tolower(live)
  out <- unique(md_names[keep])
  out[order(tolower(out))]
}
