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
# max_rows IS NOT A SAMPLE. It returns the first N rows in whatever order
# Redivis stores them, and that order can be perfectly correlated with the
# column you care about: the first 20,000 rows of fivethirtyeight_nba are ALL
# agent_a wins (the table overall is 45,158/28,205), and the first 20,000 rows
# of narcissistic_personality_inventory are ALL resp = 1 (the table overall is
# 216,672/231,626). No amount of picking rows out of such a prefix can show
# variation that is not in it. So any table small enough to fetch whole is
# fetched whole (FULL_FETCH_MAX), and the prefix is used only where the table
# is too large for that -- today only enem_2013_1mil_mt_nom at 45M rows.
#
# SAMPLE_ROWS is well above the 5 rows shown because Redivis returns rows in
# arbitrary order (there is no stored row order to rely on), so a 5-row fetch
# would hand back 5 unrelated respondents and the card would show a ballot
# with one mark and a conjoint task with one profile. Sorting a few thousand
# rows by the family's key columns and taking the top 5 gives a coherent
# excerpt -- consecutive rows of one respondent -- at no meaningful cost.
#
# Which respondent is then chosen, rather than simply the first (Ben,
# 2026-10-28). max_rows returns an arbitrary CONTIGUOUS slice, not a random
# one, so taking the lowest id gave excerpts that misrepresent the table: the
# PHQ-9 specimen showed 1,0,0,0,0 under a dimensions line reading "4
# categories", a 5-category risk-perception table came back all 1s, and a
# chess table showed agent_a winning all five games. pick_rows() therefore
# shows the first respondent in the sample whose rows are NOT constant in the
# outcome column.
#
# This is a display choice and worth being honest about: the rows are still
# real, still consecutive, and still all from one real respondent -- the
# excerpt simply does not lead with someone who answered zero to everything.
# It is not a random draw and should not be read as one.

suppressPackageStartupMessages({
  library(irw)
  library(jsonlite)
})

LIST_PATH   <- "specimens_list.csv"
OUT_PATH    <- "data/specimens.json"
N_ROWS      <- 5
SAMPLE_ROWS <- 20000L
# Below this many rows the whole table is fetched instead of a bounded prefix.
# See the note on max_rows bias in the header.
FULL_FETCH_MAX <- 2000000

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

# The respondent key to group an excerpt by, and the column whose variation
# makes the excerpt worth showing. Competitions have no respondent, so their
# rows are chosen a row at a time instead. Conjoint's outcome is whichever of
# choice/rating the table has, which cols_for has already resolved, so it is
# taken from the resolved column list rather than named here.
KEY_COL <- list(core = "id", nom = "id", comp = NULL, rank = "id", conj = "id")
OUTCOME_COL <- list(core = "resp", nom = "text", comp = "winner", rank = "resp",
                    conj = NULL)

# Five rows that show something. For a family with a respondent key: the first
# respondent in the sample whose rows vary in the outcome column, falling back
# to the first respondent when none of them do. For competitions: one row per
# distinct outcome first (a win either way, a draw), then fill in order.
pick_rows <- function(df, cols, src) {
  outcome <- OUTCOME_COL[[src]]
  if (is.null(outcome)) outcome <- tail(cols, 1)
  # Always five rows: showing fewer looks like a broken table, and a short
  # excerpt is worse than an unvaried one.
  fallback <- function() head(df[, cols, drop = FALSE], N_ROWS)
  if (!outcome %in% names(df)) return(fallback())

  key <- KEY_COL[[src]]
  if (!is.null(key) && key %in% names(df)) {
    idx <- split(seq_len(nrow(df)), df[[key]])
    # Groups in the order they appear in the sorted sample, so the pick is
    # deterministic rather than dependent on how split() orders its names.
    idx <- idx[order(vapply(idx, function(i) i[1], integer(1)))]
    # Only respondents with five rows to give. Without this the pick could
    # land on a respondent holding a single row and the card would show one
    # row: ENEM's slice is one item across many candidates, letki is one row
    # per respondent by design, and a bullet-voted ballot has one mark.
    idx <- idx[vapply(idx, length, integer(1)) >= N_ROWS]
    if (!length(idx)) return(fallback())
    n_distinct <- vapply(idx,
      function(i) length(unique(df[[outcome]][head(i, N_ROWS)])), integer(1))
    # which.max takes the first maximum, so a tie resolves to the earliest
    # respondent.
    return(df[head(idx[[which.max(n_distinct)]], N_ROWS), cols, drop = FALSE])
  }

  firsts <- vapply(unique(df[[outcome]]),
                   function(v) which(df[[outcome]] == v)[1], integer(1))
  take <- unique(c(firsts, seq_len(nrow(df))))
  df[head(take, N_ROWS), cols, drop = FALSE]
}

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
    # The conjoint standard's outcome is `choice` OR `rating` -- a design that
    # asked for a rating has no `choice` column at all, and hardcoding `choice`
    # dropped the outcome from the card entirely for those tables.
    outcome <- head(intersect(c("choice", "rating"), names(df)), 1)
    want <- c(setdiff(want, "choice"), head(sort(attrs), 2), outcome)
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
  # scientific = FALSE is load-bearing: format(1e6) is "1e+06", so ENEM's
  # 1,000,000 people rendered as "1e+06 people" without it.
  fmt <- function(x) format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)
  # "1 tasks" reads as a bug on the card, so a count of one drops the plural.
  part <- function(x, unit) {
    if (is.na(x)) return(NA_character_)
    if (x == 1) unit <- sub("s$", "", unit)
    paste(fmt(x), unit)
  }
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

# Redivis download connections fail intermittently, and the client's own retry
# can come back with a 400 ("Unable to parse table reference
# `character(0).<table>`") rather than the rows -- a transient fault, not a
# missing table. Retry the whole fetch a few times before giving up on a
# specimen.
FETCH_TRIES <- 3L

fetch_one <- function(tbl, src, cap) {
  for (attempt in seq_len(FETCH_TRIES)) {
    got <- tryCatch(
      {
        args <- list(tbl)
        if (!identical(src, "core")) args$source <- src
        if (!is.null(cap)) args$max_rows <- cap
        df <- do.call(irw_fetch, args)
        df <- as.data.frame(df)
        if (!nrow(df)) stop("fetched 0 rows", call. = FALSE)
        df
      },
      error = function(e) e
    )
    if (!inherits(got, "error")) return(got)
    if (attempt < FETCH_TRIES) {
      message("  attempt ", attempt, " failed (", conditionMessage(got), "); retrying ...")
      Sys.sleep(5 * attempt)
    } else {
      return(got)
    }
  }
}

entries <- vector("list", nrow(spec_list))
failures <- character(0)

for (k in seq_len(nrow(spec_list))) {
  row <- spec_list[k, ]
  tbl <- trimws(row$table)
  src <- trimws(row$source)
  # Metadata first: its row count decides whether the whole table is fetched.
  meta_row <- meta_for(src, tbl)
  n_rows <- if (is.null(meta_row)) NA_real_ else
    suppressWarnings(as.numeric(meta_row[[META_ROWS[[src]]]][1]))
  cap <- if (!is.na(n_rows) && n_rows > FULL_FETCH_MAX) SAMPLE_ROWS else NULL

  message("fetching ", tbl, " (source = ", src, ", ",
          if (is.null(cap)) "whole table" else paste0("max_rows = ", cap), ") ...")

  df <- fetch_one(tbl, src, cap)
  # Carry on through a failure rather than halting, so one run reports every
  # broken specimen instead of only the first. Nothing is written unless all of
  # them succeed; see the all-or-nothing check below.
  if (inherits(df, "error")) {
    failures <- c(failures, sprintf("%s (%s): %s", tbl, src, conditionMessage(df)))
    message("  GIVING UP on ", tbl)
    next
  }

  cols <- cols_for(df, src)
  if (!length(cols)) stop(tbl, ": none of the ", src, " standard columns are present")

  # Redivis returns rows in arbitrary order, so sort the sample before taking
  # the top 5; otherwise the excerpt is 5 unrelated respondents.
  sort_by <- intersect(SORT_COLS[[src]], names(df))
  if (length(sort_by)) df <- df[do.call(order, unname(as.list(df[sort_by]))), , drop = FALSE]

  head_df <- pick_rows(df, cols, src)
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

# All or nothing: a partial file would quietly drop specimens, and in the worst
# case a whole family, from the homepage card. Leave the committed JSON alone
# and report every failure at once.
if (length(failures)) {
  stop("could not fetch ", length(failures), " of ", nrow(spec_list), " specimens:\n  ",
       paste(failures, collapse = "\n  "),
       "\n", OUT_PATH, " left unchanged.", call. = FALSE)
}

dir.create(dirname(OUT_PATH), showWarnings = FALSE, recursive = TRUE)
write_json(entries, OUT_PATH, auto_unbox = TRUE, pretty = TRUE)
message("wrote ", OUT_PATH, ": ", length(entries), " specimens")
