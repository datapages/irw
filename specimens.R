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
# Each entry's `columns`/`rows` hold the first 5 rows of that family's standard
# columns, in the family's own order -- the card's headers vary by family
# because the standards do (see comps_standard.qmd, nominal_standard.qmd,
# rankings_standard.qmd, conjoint_standard.qmd).

suppressPackageStartupMessages({
  library(irw)
  library(jsonlite)
})

LIST_PATH <- "specimens_list.csv"
OUT_PATH  <- "data/specimens.json"
N_ROWS    <- 5

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

# The one-line dimensions string under the title. Counted from the fetched
# table, not from metadata, so it describes exactly the rows shown below it.
dims_for <- function(df, source) {
  n <- function(col) if (col %in% names(df)) length(unique(df[[col]])) else NA_integer_
  fmt <- function(x) format(x, big.mark = ",", trim = TRUE)
  parts <- switch(
    source,
    core = c(sprintf("%s people", fmt(n("id"))), sprintf("%s items", fmt(n("item"))),
             sprintf("%s categories", fmt(n("resp")))),
    nom  = c(sprintf("%s people", fmt(n("id"))), sprintf("%s items", fmt(n("item"))),
             "unordered"),
    comp = c(sprintf("%s actors",
                     fmt(length(unique(c(as.character(df$agent_a), as.character(df$agent_b)))))),
             sprintf("%s contests", fmt(nrow(df)))),
    rank = c(sprintf("%s ballots", fmt(n("id"))), sprintf("%s options", fmt(n("item")))),
    conj = c(sprintf("%s people", fmt(n("id"))), sprintf("%s tasks", fmt(n("task"))),
             sprintf("%s attributes", fmt(length(grep("^attr_", names(df))))))
  )
  paste(parts[!grepl("NA", parts)], collapse = " · ")
}

# The fetch call printed on the card. Core takes no `source`, as the package's
# own default; the other families name theirs.
fetch_call <- function(table, source) {
  if (identical(source, "core")) sprintf('irw_fetch("%s")', table)
  else sprintf('irw_fetch("%s", source = "%s")', table, source)
}

entries <- vector("list", nrow(spec_list))

for (k in seq_len(nrow(spec_list))) {
  row <- spec_list[k, ]
  tbl <- trimws(row$table)
  src <- trimws(row$source)
  message("fetching ", tbl, " (source = ", src, ") ...")

  df <- if (identical(src, "core")) irw_fetch(tbl) else irw_fetch(tbl, source = src)
  df <- as.data.frame(df)
  if (!nrow(df)) stop(tbl, " fetched 0 rows")

  cols <- cols_for(df, src)
  if (!length(cols)) stop(tbl, ": none of the ", src, " standard columns are present")

  head_df <- head(df[, cols, drop = FALSE], N_ROWS)
  # Every cell goes out as a string: the card renders text, and a numeric
  # column would otherwise arrive as 1 vs "1" depending on the column's type.
  rows <- lapply(seq_len(nrow(head_df)), function(j) {
    vapply(cols, function(cc) {
      v <- head_df[[cc]][j]
      if (is.na(v)) "" else as.character(v)
    }, character(1), USE.NAMES = FALSE)
  })

  entries[[k]] <- list(
    table = tbl,
    family = unname(FAMILY[[src]]),
    source = src,
    title = trimws(row$title),
    dims = dims_for(df, src),
    columns = cols,
    rows = rows,
    n_rows = nrow(df),
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
