# specimens.R -- writes data/specimens.json, the rotating specimen card on
# preview.qmd (the homepage redesign preview).
#
# NOT a pre-render step and not wired into _quarto.yml: it needs Redivis
# credentials, and the card must never call Redivis from a reader's browser.
# Run it by hand when the specimen list changes, then commit the JSON it writes.
#
#   Rscript specimens.R
#
# Input: specimens_list.csv, hand-edited, one row per specimen, columns
#
#   table   the IRW table name, as the warehouse spells it
#   source  core | nom | comp | rank | conj  (the irw package's `source`)
#   title   the one-line human title shown on the card
#
# Output: data/specimens.json, one object per row, in the row order of the CSV,
# with "placeholder": false. preview.qmd reads it with FileAttachment and tags
# any entry whose "placeholder" is true, which is what the committed
# hand-written file contains until this script has been run.
#
# Each entry's `columns`/`rows` hold 5 rows of that family's standard columns,
# in the family's own order -- the card's headers vary by family because the
# standards do (see comps_standard.qmd, nominal_standard.qmd,
# rankings_standard.qmd, conjoint_standard.qmd).
#
# The script never downloads a whole table. Two of the specimens are enormous
# (enem_2013_1mil_mt_nom is 45M responses, lichess_2013_blitz 1.3M games), and
# pulling either down to print fifteen cells would be absurd. Instead:
#
#   - row and dimension counts come from irw_metadata(source = ...), which is
#     one small table per family, not from the fetched rows;
#   - the display rows come from irw_fetch(..., max_rows = SAMPLE_ROWS), a
#     bounded pushdown, which is cheap even against a 45M-row table.
#
# SAMPLE_ROWS is well above the 5 rows shown because Redivis returns rows in
# arbitrary order (there is no stored row order to rely on), so a 5-row fetch
# would hand back 5 unrelated respondents and the card would show a ballot
# with one mark and a conjoint task with one profile. Sorting a few thousand
# rows by the family's key columns and taking the top 5 gives a coherent
# excerpt -- consecutive rows of one respondent -- at no meaningful cost.

suppressPackageStartupMessages({
  library(irw)
  library(jsonlite)
})

LIST_PATH   <- "specimens_list.csv"
OUT_PATH    <- "data/specimens.json"
N_ROWS      <- 5
SAMPLE_ROWS <- 5000L

# Family label shown in the card's chip and on its buttons, per `source`.
FAMILY <- c(core = "core", nom = "nominal", comp = "competitions",
            rank = "rankings", conj = "conjoint")

# The standard columns to show, per family, in order. Columns absent from a
# given table are dropped rather than shown empty; for conjoint the attr_*
# columns are discovered per table (a design has as many as it has) and the
# first two are kept so the card stays five columns wide.
STANDARD_COLS <- list(
  core = c("id", "item", "resp"),
  nom  = c("id", "item", "text"),
  comp = c("agent_a", "agent_b", "winner"),
  rank = c("id", "task", "item", "resp"),
  conj = c("id", "task", "profile", "choice")
)

# Columns the sample is sorted by before the top 5 are taken, so the excerpt
# reads as consecutive rows of one respondent rather than 5 unrelated ones.
# Competitions have no respondent to group by, so the sample is shown as it
# arrives.
SORT_COLS <- list(
  core = c("id", "item"),
  nom  = c("id", "item"),
  comp = character(0),
  rank = c("id", "task", "resp"),
  conj = c("id", "task", "profile")
)

# Where each family's metadata keeps the figures the card prints. The names
# differ by family: core and nominal count responses, conjoint and rankings
# count rows, competitions counts contests.
META_ROWS <- c(core = "n_responses", nom = "n_responses", comp = "n_responses",
               rank = "n_rows", conj = "n_rows")

stopifnot(file.exists(LIST_PATH))
spec_list <- read.csv(LIST_PATH, colClasses = "character", comment.char = "#")
required <- c("table", "source", "title")
missing <- setdiff(required, names(spec_list))
if (length(missing)) {
  stop("specimens_list.csv is missing column(s): ", paste(missing, collapse = ", "))
}
spec_list <- spec_list[nzchar(trimws(spec_list$table)), , drop = FALSE]
if (!nrow(spec_list)) stop("specimens_list.csv has no rows")

bad_source <- setdiff(unique(spec_list$source), names(FAMILY))
if (length(bad_source)) {
  stop("unknown source(s) in specimens_list.csv: ", paste(bad_source, collapse = ", "),
       " -- expected one of ", paste(names(FAMILY), collapse = ", "))
}

# The columns to show for one fetched table: the family's standard columns that
# are actually present, with conjoint's first two attr_* inserted before the
# outcome so a profile's levels are visible beside the choice.
cols_for <- function(df, source) {
  want <- STANDARD_COLS[[source]]
  if (identical(source, "conj")) {
    attrs <- grep("^attr_", names(df), value = TRUE)
    want <- c(setdiff(want, "choice"), head(sort(attrs), 2), "choice")
  }
  intersect(want, names(df))
}

# The one-line dimensions string under the title, built from the family's
# metadata row rather than from the fetched sample -- the sample is bounded, so
# counting distinct ids in it would understate every large table.
dims_for <- function(meta_row, source) {
  g <- function(col) {
    if (is.null(meta_row) || !col %in% names(meta_row)) return(NA_real_)
    suppressWarnings(as.numeric(meta_row[[col]][1]))
  }
  fmt <- function(x) format(round(x), big.mark = ",", trim = TRUE)
  part <- function(x, unit) if (is.na(x)) NA_character_ else paste(fmt(x), unit)
  parts <- switch(
    source,
    core = c(part(g("n_participants"), "people"), part(g("n_items"), "items"),
             part(g("n_categories"), "categories")),
    nom  = c(part(g("n_participants"), "people"), part(g("n_items"), "items"),
             "unordered"),
    comp = c(part(g("n_actors"), "actors"), part(g("n_responses"), "contests")),
    rank = c(part(g("n_respondents"), "ballots"), part(g("n_items"), "options")),
    conj = c(part(g("n_respondents"), "people"), part(g("n_tasks"), "tasks"),
             part(g("n_attributes"), "attributes"))
  )
  paste(parts[!is.na(parts)], collapse = " · ")
}

# The fetch call printed on the card. Core takes no `source`, as the package's
# own default; the other families name theirs.
fetch_call <- function(table, source) {
  if (identical(source, "core")) sprintf('irw_fetch("%s")', table)
  else sprintf('irw_fetch("%s", source = "%s")', table, source)
}

# One metadata table per family, fetched once and reused, rather than once per
# specimen.
meta_cache <- new.env(parent = emptyenv())
meta_for <- function(source, table) {
  if (is.null(meta_cache[[source]])) {
    meta_cache[[source]] <- as.data.frame(
      if (identical(source, "core")) irw_metadata() else irw_metadata(source = source)
    )
  }
  md <- meta_cache[[source]]
  hit <- md[tolower(trimws(as.character(md$table))) == tolower(table), , drop = FALSE]
  if (!nrow(hit)) {
    warning(table, ": no row in the ", source, " metadata; dimensions and row ",
            "count will be omitted", call. = FALSE)
    return(NULL)
  }
  hit[1, , drop = FALSE]
}

entries <- vector("list", nrow(spec_list))

for (k in seq_len(nrow(spec_list))) {
  row <- spec_list[k, ]
  tbl <- trimws(row$table)
  src <- trimws(row$source)
  message("fetching ", tbl, " (source = ", src, ", max_rows = ", SAMPLE_ROWS, ") ...")

  df <- if (identical(src, "core")) irw_fetch(tbl, max_rows = SAMPLE_ROWS)
        else irw_fetch(tbl, source = src, max_rows = SAMPLE_ROWS)
  df <- as.data.frame(df)
  if (!nrow(df)) stop(tbl, " fetched 0 rows")

  cols <- cols_for(df, src)
  if (!length(cols)) stop(tbl, ": none of the ", src, " standard columns are present")

  # Redivis returns rows in arbitrary order, so sort the sample before taking
  # the top 5; otherwise the excerpt is 5 unrelated respondents.
  sort_by <- intersect(SORT_COLS[[src]], names(df))
  if (length(sort_by)) df <- df[do.call(order, unname(as.list(df[sort_by]))), , drop = FALSE]

  meta_row <- meta_for(src, tbl)
  head_df <- head(df[, cols, drop = FALSE], N_ROWS)
  # Every cell goes out as a string: the card renders text, and a numeric
  # column would otherwise arrive as 1 vs "1" depending on the column's type.
  rows <- lapply(seq_len(nrow(head_df)), function(j) {
    vapply(cols, function(cc) {
      v <- head_df[[cc]][j]
      if (is.na(v)) "" else as.character(v)
    }, character(1), USE.NAMES = FALSE)
  })

  n_rows <- if (is.null(meta_row)) NA_real_ else
    suppressWarnings(as.numeric(meta_row[[META_ROWS[[src]]]][1]))

  entries[[k]] <- list(
    table = tbl,
    family = unname(FAMILY[[src]]),
    source = src,
    title = trimws(row$title),
    dims = dims_for(meta_row, src),
    columns = cols,
    rows = rows,
    # The card hides this line when it is not a positive number.
    n_rows = if (is.na(n_rows)) 0 else round(n_rows),
    fetch = fetch_call(tbl, src),
    # Table pages are emitted at /tables/<lowercased name>/ -- a directory, not
    # a .html file. See slug_of() in landing/page_rules.R.
    page_url = sprintf("tables/%s/", tolower(tbl)),
    placeholder = FALSE
  )
}

dir.create(dirname(OUT_PATH), showWarnings = FALSE, recursive = TRUE)
write_json(entries, OUT_PATH, auto_unbox = TRUE, pretty = TRUE)
message("wrote ", OUT_PATH, ": ", length(entries), " specimens")
