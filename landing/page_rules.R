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
#   date, and nothing else (a renamed table also links its new name). The list is
#   the data repo's withdrawal ledger, ben-domingue/irw itemtext/withdrawals.csv,
#   read at build time, plus landing/withdrawn.tsv for anything the ledger lacks.
#   The ledger wins even while Redivis still serves the table (a withdrawal is
#   recorded before its release): on 2026-09-30 that included nine tables withdrawn
#   for publishing personal data. A ledger row applies to the dataset it names, so
#   a name reused in another shard keeps its page. A table rebuilt and re-released
#   after its withdrawal is listed in landing/reinstated.tsv until the ledger can
#   record that itself.

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

# The non-core sources get pages too (ben-domingue/irw#2453, decided 2026-09-26).
# Their facts live in their own irw_meta tables (<prefix>_metadata, _biblio), not
# in irw_meta's `metadata`, and each is fetched in the packages with `source =`.
# Table names are unique across ALL sources -- the 66 nom tables were renamed to
# *_nom for exactly this -- so every source shares the one flat /tables/<slug>/.
# Conjoint (irw_conjoint, ben-domingue/irw#2887) has no item/resp either: its
# metadata counts respondents, tasks and profiles, and carries design fields
# (country, languages, randomization restrictions) plus a conj_outcomes table.
NONCORE <- data.frame(
  dataset = c("irw_simsyn",      "irw_competitions",      "irw_nominal",      "irw_conjoint"),
  ref     = c("irw_simsyn:0btg", "irw_competitions:cmd7", "irw_nominal:614n", "irw_conjoint:5wjx"),
  prefix  = c("simsyn",          "comps",                 "nominal",          "conj"),
  source  = c("sim",             "comp",                  "nom",              "conj"),
  stringsAsFactors = FALSE)
# Conjoint is split across shards, because Redivis caps a dataset at 1,000
# tables (ben-domingue/irw#2934; new tables go to the newest shard). irw_meta's
# conj_metadata has one row per table and no shard column, so the emitter gives
# each conj table the newest shard that lists it -- the packages resolve a name
# the same way. NONCORE keeps only the first shard, so conj_metadata is read once.
CONJ_SHARDS <- c(irw_conjoint = "irw_conjoint:5wjx", irw_conjoint_2 = "irw_conjoint_2:142p")
PAGE_REF <- c(SHARD_REF, setNames(NONCORE$ref, NONCORE$dataset),
              CONJ_SHARDS[setdiff(names(CONJ_SHARDS), NONCORE$dataset)])

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

# Withdrawals are recorded once, in the data repo's ledger, when a table is taken
# down. Keeping a second hand-kept list here is how 112 withdrawn tables became
# 404s in September 2026: none of them reached withdrawn.tsv. So the ledger is read,
# not forked. Only whole-table withdrawals of response data count; irw_text rows
# are item text, which has no page. A ledger note "renamed [to] <name>" gives the
# table's new name.
WITHDRAWALS_URL <- paste0("https://raw.githubusercontent.com/ben-domingue/irw/main/",
                          "itemtext/withdrawals.csv")
.ledger_cache <- NULL
withdrawal_ledger <- function() {
  if (!is.null(.ledger_cache)) return(.ledger_cache)
  w <- tryCatch({
    con <- url(WITHDRAWALS_URL)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    utils::read.csv(con, colClasses = "character", na.strings = character(0),
                    encoding = "UTF-8")
  }, error = function(e) {
    # In CI a missing ledger would turn every tombstone it holds into a 404 on
    # the live site, so it stops the build. Locally it reads as empty.
    if (nzchar(Sys.getenv("CI")))
      stop("[landing] withdrawals.csv unreadable (", conditionMessage(e), "); ",
           "refusing to publish without the tombstones it lists.", call. = FALSE)
    message("[landing] withdrawals.csv unreadable (", conditionMessage(e),
            "); only landing/withdrawn.tsv is used (local preview only)")
    NULL
  })
  out <- data.frame(table = character(0), dataset = character(0), date = character(0),
                    renamed_to = character(0), stringsAsFactors = FALSE)
  if (!is.null(w) && nrow(w)) {
    w[] <- lapply(w, trimws)
    w <- w[w$kind == "whole" & !startsWith(w$dataset, "irw_text") & nzchar(w$table), , drop = FALSE]
    ren <- regmatches(w$note, regexec("renamed (?:to )?([A-Za-z0-9_-]+)", w$note, perl = TRUE))
    out <- data.frame(table = w$table, dataset = w$dataset, date = substr(w$withdrawn, 1, 10),
                      renamed_to = vapply(ren, function(m) if (length(m) > 1) m[2] else "",
                                          character(1)),
                      stringsAsFactors = FALSE)
  }
  assign(".ledger_cache", out, envir = globalenv())
  out
}

# Every withdrawn table: the ledger, then withdrawn.tsv for any table the ledger
# does not have. One row per table (case-folded, as the URL slug is), sorted.
withdrawn_tbl <- function() {
  local <- read_landing_list("withdrawn.tsv", c("table", "date"))
  local$dataset <- rep("", nrow(local))      # "" = whichever dataset holds it
  local$renamed_to <- rep("", nrow(local))
  local <- local[c("table", "dataset", "date", "renamed_to")]
  # A ledger row for a table since rebuilt and re-released (landing/reinstated.tsv)
  # no longer counts. The ledger itself cannot say so yet.
  back <- tolower(read_landing_list("reinstated.tsv", c("table", "ref"))$table)
  ledger <- withdrawal_ledger()
  ledger <- ledger[!(tolower(ledger$table) %in% back), , drop = FALSE]
  w <- rbind(ledger, local)
  w <- w[!duplicated(paste(tolower(w$table), w$dataset)), , drop = FALSE]
  w[order(tolower(w$table)), , drop = FALSE]
}

# TRUE where table `tb` in dataset `ds` is withdrawn. A ledger dataset ending in
# "*" (item_response_warehouse*) covers every dataset with that prefix.
is_withdrawn <- function(tb, ds, w = withdrawn_tbl()) {
  vapply(seq_along(tb), function(i) {
    hit <- w[tolower(w$table) == tolower(tb[i]), , drop = FALSE]
    any(!nzchar(hit$dataset) | hit$dataset == ds[i] |
        (endsWith(hit$dataset, "*") & startsWith(ds[i], sub("\\*$", "", hit$dataset))))
  }, logical(1))
}

# The tables that get a full landing page. `md` is irw_meta's metadata table,
# `bib` its biblio table (the emitter appends the non-core sources' rows to both,
# with `dataset` set; data.qmd passes the core tables alone); `live` is optionally the table names Redivis actually
# lists, so a table still in metadata after it left Redivis gets no page, and a
# withdrawn table (see withdrawn_tbl) never gets one.
# Returns the true table names, sorted.
page_tables <- function(md, bib, live = NULL) {
  md_names <- trimws(as.character(md$table))
  in_shard <- as.character(md$dataset) %in% names(PAGE_REF)
  lic <- setNames(as.character(bib$Derived_License), tolower(trimws(as.character(bib$table))))
  has_lic <- vapply(tolower(md_names), function(k) !blank(lic[k]), logical(1))
  keep <- in_shard & has_lic & !is_withdrawn(md_names, as.character(md$dataset))
  if (!is.null(live)) keep <- keep & tolower(md_names) %in% tolower(live)
  out <- unique(md_names[keep])
  out[order(tolower(out))]
}
