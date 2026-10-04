#!/usr/bin/env Rscript
#
# Per-table landing pages for the IRW (ben-domingue/irw#1706).
#
# Emits, into _site/tables/, for every table landing/page_rules.R says gets a page:
#   <slug>/index.html       a landing page carrying schema.org/Dataset JSON-LD
#   <slug>/croissant.jsonld a Croissant (MLCommons) description
# plus a tombstone <slug>/index.html for each withdrawn table without a live page
# (ben-domingue/irw itemtext/withdrawals.csv, plus landing/withdrawn.tsv), and a
# banner (and no search presence) for each table in landing/known_issues.tsv.
# Caveats about a table's *source* (ben-domingue/irw metadata/data_notes.csv,
# #2529) are a plain Notes section instead: no banner, and the page stays indexed.
#
# The directory form is deliberate: the public URL is /tables/<slug>/ with no
# file extension. These URLs are meant to be cited, and to be what a release DOI
# resolves to if #1870 lands -- an extension in a citable identifier ages badly,
# and GitHub Pages does not reliably serve /tables/<slug> for a <slug>.html file.
# Same file count either way; only the path shape differs. Changing it after the
# pages are indexed and cited is the expensive move, so it is made up front.
# Also writes _site/tables/index.html and _site/sitemap-tables.xml, which
# landing/write_sitemaps.R (the next post-render step) lists in the sitemap index.
#
# Run as a Quarto post-render step. Skips itself (with a message, exit 0) when
# REDIVIS_API_TOKEN is absent, so a local preview without credentials still works.
# In CI (env CI set) an absent token is a hard error instead -- see the guard below.
#
# CREDENTIAL: this script only ever READS (irw_meta and table metadata). The token
# it expects is a read-scoped Redivis token; it needs no data.edit scope, and a
# write-scoped token should not be used here.
#
# THREE RULES THIS FILE EXISTS TO KEEP -- see the 2026-09-03 scoping comment on
# ben-domingue/irw#1706 for the measurements behind them:
#
# 1. OUTPUT IS DETERMINISTIC. Identical inputs must produce byte-identical files.
#    No timestamps, no build ids, no unordered iteration. Every page is published
#    to the gh-pages branch, which today holds 176 files in a 294MB repo; if a
#    page changes when its table did not, a full corpus emission adds ~34MB of git
#    objects per publish (~5GB/yr). Determinism is what makes ~3/4 of renders free.
#    DO NOT introduce Sys.time(), Sys.Date(), or any nondeterministic ordering.
#
# 2. VERSIONS ARE REPORTED, NEVER RECONCILED. The page states both the IRW version
#    from metadata/version_manifest.tsv and the exact Redivis dataset version the
#    page's facts were read from. When they disagree (the manifest cron lags), the
#    page shows both and the script warns. It never guesses which is right.
#
# 3. THE MANIFEST IS READ, NOT FORKED. metadata/version_manifest.tsv in
#    ben-domingue/irw is authoritative (ARCHITECTURE.md rule 1). This script reads
#    that file over HTTP rather than restating any of it.
#    NOTE: irw::irw_version() does this properly, but renv.lock pins irw at
#    6ebce93a, which predates R/manifest.R. When that pin is next bumped for other
#    reasons, replace .read_manifest() with irw::irw_version().

# The skip below exists for one case only: a local preview by someone without
# Redivis credentials. In CI it must NOT skip. `quarto publish` replaces the
# published site wholesale, so a silent skip on a green build would delete the
# table landing pages and their Croissant files from itemresponsewarehouse.org
# with nothing in the log louder than one message() -- exactly the failure mode
# that makes datapages/irw's REDIVIS_API_TOKEN secret dangerous to touch
# (ben-domingue/irw#2129). A missing credential in CI is a broken build, not a
# reason to publish a smaller site. stop() here fails the post-render step, which
# fails the render, which means nothing is published and the live pages survive.
if (!nzchar(Sys.getenv("REDIVIS_API_TOKEN"))) {
  if (nzchar(Sys.getenv("CI"))) {
    stop("[landing] REDIVIS_API_TOKEN is not set, but CI is. Refusing to publish ",
         "a site without the table landing pages. Set the REDIVIS_API_TOKEN ",
         "secret on this repository to a read-scoped Redivis token.", call. = FALSE)
  }
  message("[landing] REDIVIS_API_TOKEN not set -- skipping landing page emission ",
          "(local preview; this is a hard error in CI).")
  quit(status = 0)
}

suppressWarnings(suppressMessages({
  library(redivis); library(jsonlite)
}))
source(file.path("landing", "page_rules.R"))   # SHARD_REF, blank(), slug_of(), page_tables()

SITE_URL     <- "https://itemresponsewarehouse.org"
OUT_DIR      <- file.path("_site", "tables")
MANIFEST_URL <- paste0("https://raw.githubusercontent.com/ben-domingue/irw/main/",
                       "metadata/version_manifest.tsv")
# Collections the IRW found tables through (openESM, ...), keyed by biblio's
# Source_via: each one's note and BibTeX (ben-domingue/irw#2421).
AGGREGATORS_URL <- paste0("https://raw.githubusercontent.com/ben-domingue/irw/main/",
                          "metadata/aggregators.csv")
# Caveats true of a table's source, not IRW defects (ben-domingue/irw#2529).
DATA_NOTES_URL <- paste0("https://raw.githubusercontent.com/ben-domingue/irw/main/",
                         "metadata/data_notes.csv")
# Per-column codebook rows, built weekly by the data repo's stage 14, and the
# data standard whose schema table defines the IRW's own columns
# (ben-domingue/irw#2763).
COLUMN_DOCS_URL <- paste0("https://raw.githubusercontent.com/ben-domingue/irw/main/",
                          "metadata/column_docs.csv")
# Covariate value labels (ben-domingue/irw#1775), verbatim from each source:
# table, covariate, code, label. The same file irw_meta's covariate_labels
# table is uploaded from.
COVARIATE_LABELS_URL <- paste0("https://raw.githubusercontent.com/ben-domingue/irw/main/",
                               "metadata/covariate_labels.csv")
# The source's own codebook files, found by NAME in each deposit's file list
# (ben-domingue/irw#2766, metadata/find_codebook_links.py): table, url,
# file_name, host, how_found, n_same_kind_in_deposit, deposit_url, checked_at.
CODEBOOK_LINKS_URL <- paste0("https://raw.githubusercontent.com/ben-domingue/irw/main/",
                             "metadata/codebook_links.csv")
STANDARD_RAW_URL <- paste0("https://raw.githubusercontent.com/ben-domingue/irw/main/",
                           "datastandard.md")
# Filled by main() from .read_standard(); empty until then, so a page built
# without it shows codebook rows with no definitions rather than failing.
STANDARD <- list(exact = list(), family = list())
# Data defects are tracked in the data repo, not this one: a known-issue banner
# and the index's flag both point here.
ISSUE_URL    <- "https://github.com/ben-domingue/irw/issues/"
# Redivis' table.listRows endpoint serves a public table as CSV with no token, but
# only up to 100MB: above that it answers 401 "Results larger than 100MB are not
# supported for unauthenticated requests". The cutoff follows the table's own
# numBytes property exactly -- verified 2026-09-19 on 12 tables between 60MB and
# 160MB: every one at <= 95.6MB returned 200, every one at >= 105.2MB returned 401.
ROWS_API       <- "https://redivis.com/api/v1/tables/"
ANON_MAX_BYTES <- 100e6

# ---------------------------------------------------------------- small helpers

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

chr <- function(x) {
  if (blank(x)) return("")
  trimws(as.character(x)[1])
}

esc <- function(x) {
  x <- chr(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

num_fmt <- function(x) {
  if (blank(x)) return("")
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) return(chr(x))
  if (v == round(v) && abs(v) < 1e15) formatC(v, format = "d", big.mark = ",")
  else formatC(v, format = "f", digits = 3)
}

# The same count as num_fmt, unformatted, for the index's data-n sort key: the
# displayed "1,048,576" sorts lexically, which puts 9,912 above a million.
# Never scientific notation, and never NA -- an unknown count sorts to the end.
num_raw <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) "-1" else sprintf("%.0f", v)
}

# The dictionary's paper-DOI cell also holds placeholders ("No DOI",
# "Upcoming", "not yet published"), which were published as the DOI row and
# as a JSON-LD citation of "https://doi.org/No DOI" (ben-domingue/irw#2513).
# Keep only a DOI (10.<registrant>/...) or a URL; anything else is absent.
clean_doi <- function(x) {
  doi <- chr(x)
  if (!grepl("^(https?://|(doi:\\s*)?10\\.[0-9]{4,}/)", doi, ignore.case = TRUE)) "" else doi
}

# ------------------------------------------------------------------ the inputs

.read_manifest <- function() {
  con <- url(MANIFEST_URL)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  m <- utils::read.delim(con, stringsAsFactors = FALSE, colClasses = "character")
  m$irw_version <- as.integer(m$irw_version)
  m
}

# key -> list(note, bibtex). A credit line must never cost a build its pages, so
# any failure reads as no registry, and a page then names the source without
# the collection's own note or citation.
.read_aggregators <- function() {
  tryCatch({
    con <- url(AGGREGATORS_URL)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    a <- utils::read.csv(con, colClasses = "character", check.names = FALSE,
                         encoding = "UTF-8")
    a <- a[!is.na(a$key) & nzchar(trimws(a$key)), , drop = FALSE]
    setNames(lapply(seq_len(nrow(a)), function(i)
      list(note = chr(a[["note text"]][i]), bibtex = chr(a$BibTeX[i]))),
      trimws(a$key))
  }, error = function(e) {
    message("aggregators.csv unreadable (", conditionMessage(e),
            "); Source via pages get the plain note")
    list()
  })
}

# tolower(table) -> data frame of that table's notes (note, issue), oldest
# first. Unlike the aggregator registry, a note is information a user of the
# table needs, and the pages are republished wholesale, so a fetch failure in CI
# stops the build rather than silently strip every note from the live site.
# Locally it reads as no notes, like the missing-token skip above.
.read_data_notes <- function() {
  n <- tryCatch({
    con <- url(DATA_NOTES_URL)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    utils::read.csv(con, colClasses = "character", na.strings = character(0),
                    encoding = "UTF-8")
  }, error = function(e) {
    if (nzchar(Sys.getenv("CI")))
      stop("[landing] data_notes.csv unreadable (", conditionMessage(e), "); ",
           "refusing to publish pages without their notes.", call. = FALSE)
    message("[landing] data_notes.csv unreadable (", conditionMessage(e),
            "); pages get no Notes section (local preview only)")
    NULL
  })
  if (is.null(n) || !nrow(n)) return(list())
  n$table <- trimws(n$table); n$note <- trimws(n$note)
  n <- n[nzchar(n$table) & nzchar(n$note), , drop = FALSE]
  # Rule 1: a fixed order, whatever order the file is in.
  n <- n[order(tolower(n$table), n$date, n$note, method = "radix"), , drop = FALSE]
  split(n[c("note", "issue")], tolower(n$table))
}

# tolower(table) -> that table's rows of column_docs.csv. A codebook is help,
# not a fact the page would be wrong without, so any failure reads as no rows
# and the page falls back to the plain column list (ben-domingue/irw#2763).
.read_column_docs <- function() {
  d <- tryCatch({
    con <- url(COLUMN_DOCS_URL)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    utils::read.csv(con, colClasses = "character", na.strings = character(0),
                    encoding = "UTF-8")
  }, error = function(e) {
    message("[landing] column_docs.csv unreadable (", conditionMessage(e),
            "); pages show the plain column list")
    NULL
  })
  if (is.null(d) || !nrow(d)) return(list())
  split(d, tolower(d$table))
}

# tolower(table) -> that table's covariate labels. Help, not fact, like the
# codebook rows: a failed fetch leaves covariates unlabelled (irw#2763 step 3).
.read_covariate_labels <- function() {
  d <- tryCatch({
    con <- url(COVARIATE_LABELS_URL)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    utils::read.csv(con, colClasses = "character", na.strings = character(0),
                    encoding = "UTF-8")
  }, error = function(e) {
    message("[landing] covariate_labels.csv unreadable (", conditionMessage(e),
            "); codebook covariates carry no value labels")
    NULL
  })
  if (is.null(d) || !nrow(d)) return(list())
  split(d, tolower(d$table))
}

# tolower(table) -> that table's source-codebook links. Help, not fact: a
# failed fetch leaves the page pointing at the deposit, as before (irw#2766).
.read_codebook_links <- function() {
  d <- tryCatch({
    con <- url(CODEBOOK_LINKS_URL)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    utils::read.csv(con, colClasses = "character", na.strings = character(0),
                    encoding = "UTF-8")
  }, error = function(e) {
    message("[landing] codebook_links.csv unreadable (", conditionMessage(e),
            "); pages point at the source deposit")
    NULL
  })
  if (is.null(d) || !nrow(d)) return(list())
  split(d, tolower(d$table))
}

# The data standard's schema table: list(exact = name -> first sentence,
# family = prefix -> first sentence). Same parsing rules as stage 14 and the
# MCP: a backticked `name*` or `nameN` is a family.
.read_standard <- function() {
  lines <- tryCatch(readLines(url(STANDARD_RAW_URL), warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      message("[landing] datastandard.md unreadable (", conditionMessage(e),
              "); codebook rows carry no definitions")
      character(0)
    })
  exact <- list(); family <- list()
  for (ln in grep("^\\| `", lines, value = TRUE)) {
    cells <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", ln)), "|", fixed = TRUE)[[1]])
    if (length(cells) < 3) next
    rule <- gsub("\\*\\*|`", "", paste(cells[-(1:2)], collapse = " | "))
    first <- regmatches(rule, regexpr("^.+?[.](?=\\s|$)", rule, perl = TRUE))
    first <- if (length(first)) first else rule
    names_ <- regmatches(cells[1], gregexpr("`[^`]+`", cells[1]))[[1]]
    for (nm in gsub("`", "", names_)) {
      if (grepl("\\*$", nm) || grepl("^[A-Za-z_]+N$", nm)) {
        key <- sub(".$", "", nm)
        if (is.null(family[[key]])) family[[key]] <- first
      } else if (!grepl("^[A-Za-z_]+1$", nm) && is.null(exact[[nm]])) {
        exact[[nm]] <- first
      }
    }
  }
  list(exact = exact, family = family)
}

as_df <- function(tbl) as.data.frame(tbl$to_tibble(), stringsAsFactors = FALSE)

# rbind for frames whose columns differ (the non-core metadata tables each carry
# their own subset of the core columns); a missing column is NA.
bind_fill <- function(a, b) {
  cols <- union(names(a), names(b))
  for (cn in setdiff(cols, names(a))) a[[cn]] <- rep(NA, nrow(a))
  for (cn in setdiff(cols, names(b))) b[[cn]] <- rep(NA, nrow(b))
  rbind(a[cols], b[cols])
}

# The table's source, as the packages' `source =` argument spells it.
source_of <- function(dataset) {
  i <- match(dataset, NONCORE$dataset)
  if (is.na(i)) "core" else NONCORE$source[i]
}

# ------------------------------------------------------------------ page parts

# Every table's page shows the same sections in the same order; sections with no
# data are omitted rather than rendered empty, so an untagged table
# produces a shorter page, not a page full of blanks.
# A pair may carry a third element, TRUE, to bold its value.
kv_rows <- function(pairs) {
  keep <- vapply(pairs, function(p) !blank(p[[2]]), logical(1))
  pairs <- pairs[keep]
  if (!length(pairs)) return("")
  paste0(
    "<table class=\"kv\">\n",
    paste0(vapply(pairs, function(p)
      paste0("<tr><th>", esc(p[[1]]), "</th><td>",
             if (isTRUE(p[3][[1]])) paste0("<strong>", esc(p[[2]]), "</strong>") else esc(p[[2]]),
             "</td></tr>"),
      character(1)), collapse = "\n"),
    "\n</table>\n")
}

# What the columns mean (ben-domingue/irw#2755). A user asked where the codebook
# for gilbert_meta_10's cluster_id and block_id was; the page listed the names
# and nothing else. Columns the data standard defines mean the same thing in
# every table, so they link there; for everything else -- item codes, cov_
# values -- the codebook is the source's, and the page says so rather than
# implying the IRW holds one.
STANDARD_URL  <- "https://github.com/ben-domingue/irw/blob/main/datastandard.md"
STANDARD_COLS <- c("id", "item", "resp", "resp_raw", "wave", "treat", "rt", "date",
                   "rater", "item_family", "cluster_id", "block_id", "std_baseline")
columns_note <- function(x) {
  # A competitions table's agent_a/agent_b/winner are the IRW's own layout.
  if (identical(x$src, "comp")) return("")
  std <- x$variables[x$variables %in% STANDARD_COLS | startsWith(x$variables, "std_baseline")]
  first <- if (length(std)) paste0(
    paste0("<code>", vapply(std, esc, character(1)), "</code>", collapse = ", "),
    if (length(std) == 1) " is" else " are",
    " defined in the <a href=\"", STANDARD_URL, "\">IRW data standard</a> and mean the same in every table. ") else ""
  # The data URL field also holds "NA" and "Author Permission"; link only a URL.
  url <- if (!blank(x$source_url) && grepl("^https?://", x$source_url)) x$source_url else ""
  others <- if (nzchar(first)) "For the other columns" else "For what these columns mean"
  rest <- if (x$src == "sim") {
    if (nzchar(url)) paste0("The other columns are set by the <a href=\"", esc(url),
                            "\">generating script</a>.") else ""
  } else paste0(others, ", including what item codes and covariate values stand for, ",
    if (nzchar(cbl <- source_codebook_html(x$cblinks))) paste0("see ", cbl, ".")
    else if (nzchar(url)) paste0("any codebook is the one released with the <a href=\"", esc(url),
                            "\">source data</a>.")
    else "see the source cited above.")
  if (!nzchar(first) && !nzchar(rest)) return("")
  paste0("<p class=\"note\">", first, rest, "</p>\n")
}

# The Codebook section (ben-domingue/irw#2763): one row per column, from
# column_docs.csv and the data standard. It is a reconstruction -- what the
# standard says the column is, and the source column the build script renamed
# it from -- and opens by saying so (Ben, 2026-10-02). Nothing here infers a
# meaning from a column's name: a column with neither a definition nor a traced
# source says it is not documented. Returns "" when there are no rows, and the
# page keeps the plain column list.
SCRIPT_BLOB <- "https://github.com/ben-domingue/irw/blob/main/"

# A covariate's value labels, in code order: "1 = Rural · 2 = Urban". More
# than eight fold into a <details>. Labels are the source's own words; when
# every code shares the withheld-institution label, decoding would only merge
# distinct groups, so the row says what the codes are instead.
VALUES_INLINE_MAX <- 8
WITHHELD_LABEL <- "[institution name withheld]"
values_html <- function(labels, column) {
  if (is.null(labels) || !nrow(labels)) return("")
  v <- labels[labels$covariate == column, , drop = FALSE]
  if (!nrow(v)) return("")
  num <- suppressWarnings(as.numeric(v$code))
  v <- v[order(is.na(num), num, v$code, method = "radix"), , drop = FALSE]
  if (all(v$label == WITHHELD_LABEL))
    return(paste0("<span class=\"vals\">", nrow(v),
                  " codes, each an institution; names withheld.</span>"))
  pairs <- paste0("<span class=\"val\"><code>", vapply(v$code, esc, character(1)),
                  "</code>&nbsp;=&nbsp;", vapply(v$label, esc, character(1)), "</span>")
  if (nrow(v) <= VALUES_INLINE_MAX)
    paste0("<span class=\"vals\">", paste(pairs, collapse = " &middot; "), "</span>")
  else paste0("<details class=\"vals\"><summary>", nrow(v), " labelled values</summary>",
              paste(pairs, collapse = " &middot; "), "</details>")
}
# "the source's codebook, <a>file</a>" -- or "" when none was found, and the
# caller keeps its deposit link. These kinds are shown, in this order:
#   recorded_at_ingest    the codebook file whoever built the table named when
#                         staging it (irw#2770): a person read it
#   recorded_by_review    a statistics office's codebook found by review
#                         (irw#2787 step 3): a person or agent opened it and
#                         recorded the evidence. Those codebooks (a CIS
#                         codigo, say) list the columns but often not the
#                         response codes, so the same review's questionnaire
#                         is shown beside it
#   typed_codebook        a document the repository itself types as a codebook
#                         (LDbase "Codebook: ..."; irw#2787)
#   package_doc           a CRAN package's help page for the dataset the
#                         table's build script loads (irw#2787)
#   name_codebook         a file whose NAME says codebook / data dictionary / ...
#   readme_names_columns  a README whose TEXT names at least three of this
#                         table's own column or item names (irw#2766 follow-on;
#                         a sample of README-only hits split about half
#                         describing the variables, half install steps and
#                         folder layouts, so the name alone is not enough)
#   doc_names_columns     a journal supplementary document (PLOS, Europe PMC;
#                         .docx/.pdf/.txt, never the data) whose TEXT names at
#                         least three of this table's names (irw#2792)
#   questionnaire         a supplementary file the authors caption as the
#                         questionnaire or instrument: it shows the items, so
#                         it is worded softly (irw#2792)
# A README is shown only when no codebook-named file was found. Other README
# hits and Dataverse DDI exports stay in the CSV for the MCP. A deposit with
# more than SOURCE_CODEBOOKS_MAX of a kind -- one per scale, say -- gets one
# line pointing at the deposit: picking the right one would mean decoding
# abbreviations, which is a guess.
SOURCE_CODEBOOKS_MAX <- 3
source_codebook_html <- function(links) {
  if (is.null(links) || !nrow(links)) return("")
  for (kind in c("recorded_at_ingest", "recorded_by_review", "typed_codebook", "package_doc", "name_codebook",
                 "readme_names_columns", "doc_names_columns", "questionnaire")) {
    l <- links[links$how_found == kind, , drop = FALSE]
    l <- l[!duplicated(l$url), , drop = FALSE]
    if (!nrow(l)) next
    what <- switch(kind, readme_names_columns = c("README", "READMEs"),
                   package_doc = c("package documentation", "package help pages"),
                   doc_names_columns = c("documentation", "documents"),
                   questionnaire = c("questionnaire", "questionnaires"),
                   c("codebook", "codebook files"))
    # the cap stops the page choosing among a deposit's per-scale files; a
    # reviewed row was chosen for this table (one per wave it pools, say)
    if (nrow(l) > SOURCE_CODEBOOKS_MAX && kind != "recorded_by_review") {
      dep <- l$deposit_url[nzchar(l$deposit_url)][1]
      place <- if (all(l$host %in% c("plos", "epmc"))) "the article" else "the source deposit"
      where <- if (!is.na(dep)) paste0("<a href=\"", esc(dep), "\">", place, "</a>") else place
      return(paste0("one of the ", nrow(l), " ", what[2], " in ", where))
    }
    files <- paste0("<a href=\"", vapply(l$url, esc, character(1)), "\"><code>",
                    vapply(basename(l$file_name), esc, character(1)), "</code></a>")
    whose <- if (kind == "package_doc") "the package&rsquo;s " else "the source&rsquo;s "
    out <- paste0(whose, if (kind == "package_doc") "documentation" else what[1], ", ",
                  paste(files, collapse = ", "),
                  if (kind == "questionnaire") paste0(", which ", if (nrow(l) > 1) "show" else "shows",
                                                       " the item wording") else "")
    if (kind == "recorded_by_review") {
      q <- source_codebook_html(links[links$how_found == "questionnaire", , drop = FALSE])
      if (nzchar(q)) out <- paste0(out, ", and ", sub("^the source&rsquo;s ", "its ", q))
    }
    return(out)
  }
  ""
}

codebook_html <- function(x, standard) {
  d <- x$coldocs
  if (is.null(d) || !nrow(d) || !length(x$variables)) return("")
  d <- d[match(x$variables, d$column, nomatch = 0), , drop = FALSE]  # the table's own order
  if (!nrow(d)) return("")
  meaning_of <- function(col, defined_by) {
    if (defined_by == "standard") return(standard$exact[[col]] %||% "")
    if (defined_by == "standard_family" && !startsWith(col, "cov_")) {
      fam <- names(standard$family)[startsWith(col, names(standard$family))]
      if (length(fam)) return(standard$family[[fam[which.max(nchar(fam))]]])
    }
    ""
  }
  row_html <- function(i) {
    r <- d[i, ]
    line_url <- if (nzchar(r$script) && nzchar(r$script_line))
      paste0(SCRIPT_BLOB, r$script, "#L", r$script_line) else ""
    src <- if (r$basis == "renamed")
      paste0("<a href=\"", esc(line_url), "\"><code>", esc(r$source_column), "</code></a>")
    else if (r$basis == "built")
      paste0("<a href=\"", esc(line_url), "\">made in the build script</a>")
    else "<span class=\"muted\">not traced</span>"
    meaning <- meaning_of(r$column, r$defined_by)
    chip <- if (r$defined_by == "standard") c("std", "IRW standard")
      else if (startsWith(r$column, "cov_")) c("cov", "Covariate")
      else if (r$documented == "true") c("src", "From source")
      else c("none", "Not documented")
    what <- if (nzchar(meaning)) esc(meaning)
      else if (startsWith(r$column, "cov_")) ""
      else if (r$documented == "true") "Not defined by the IRW standard; the source column&rsquo;s codebook entry gives its meaning."
      else "Not documented by the IRW; see the source&rsquo;s codebook."
    what <- paste0(what, values_html(x$covlabels, r$column))
    paste0("<tr><td><code class=\"col\">", esc(r$column), "</code></td>",
           "<td><span class=\"chip ", chip[1], "\">", chip[2], "</span> ", what, "</td>",
           "<td>", src, "</td></tr>")
  }
  group <- ifelse(d$defined_by == "standard", "std",
                  ifelse(startsWith(d$column, "cov_"), "cov", "other"))
  labels <- c(std = "IRW standard columns", cov = "Respondent covariates", other = "Other columns")
  body <- paste(unlist(lapply(names(labels), function(g) {
    idx <- which(group == g)
    if (!length(idx)) return(NULL)
    c(paste0("<tr class=\"grp\"><th colspan=\"3\">", labels[[g]], "</th></tr>"),
      vapply(idx, row_html, character(1)))
  })), collapse = "\n")
  scripts <- unique(d$script[nzchar(d$script)])
  script_link <- if (length(scripts) == 1)
    paste0(" and the script that built it, <a href=\"", SCRIPT_BLOB, esc(scripts), "\"><code>",
           esc(basename(scripts)), "</code></a>") else if (length(scripts) > 1)
    " and the scripts that built it" else ""
  url <- if (!blank(x$source_url) && grepl("^https?://", x$source_url)) x$source_url else ""
  caveat <- paste0(
    "<p class=\"cbnote\">This codebook is reconstructed by the IRW from the ",
    "<a href=\"", STANDARD_URL, "\">IRW data standard</a>", script_link, ". ",
    "It is our best reading, not the source&rsquo;s own codebook, and it may contain mistakes. ",
    "Where the two disagree, the ",
    if (nzchar(url)) paste0("<a href=\"", esc(url), "\">source data</a>") else "source data",
    " are the authority. If you find an error, please ",
    "<a href=\"", ISSUE_URL, "new\">tell us</a>.</p>\n")
  labelled <- !is.null(x$covlabels) && nrow(x$covlabels) > 0
  values <- if (x$src == "sim") "" else paste0(
    "<p class=\"note\">",
    if (labelled) "Covariate value labels are the source&rsquo;s own, as the IRW recorded them. " else "",
    "What item codes", if (labelled) " and any unlabelled values" else " and covariate values",
    " stand for is in ",
    if (nzchar(cbl <- source_codebook_html(x$cblinks))) cbl
    else paste0("the codebook released with the ",
                if (nzchar(url)) paste0("<a href=\"", esc(url), "\">source data</a>") else "source data"),
    ".</p>\n")
  paste0(caveat,
         "<div class=\"scroll\"><table class=\"cb\">\n",
         "<thead><tr><th>Column</th><th>What it is</th><th>Source column</th></tr></thead>\n",
         "<tbody>\n", body, "\n</tbody></table></div>\n", values)
}

section <- function(title, body, id = NULL) {
  if (!nzchar(trimws(body))) return("")
  paste0("<section", if (!is.null(id)) paste0(" id=\"", id, "\"") else "", ">\n",
         "<h2>", esc(title), "</h2>\n", body, "</section>\n")
}

# Tables from the same source: the same paper DOI, or failing that the same data
# URL. Most multi-table sources are one study released as a table per scale
# (c19prc_uk_mcbride_2021_* is 70 of them), so their pages share most of their
# text. Each page says what sets it apart and links its siblings. A large family
# shows the RELATED_MAX siblings nearest in name order, a different window on each
# page, so every page is still linked from its neighbours without every page in a
# family carrying the same long list. `x$family` is a data frame (table, construct,
# items) of the whole family including this table, sorted by name.
RELATED_MAX <- 12

source_key <- function(brow) {
  if (!nrow(brow)) return(NA_character_)
  d <- clean_doi(brow[1, "DOI__for_paper_"])
  if (nzchar(d))
    return(paste0("doi:", tolower(sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", d,
                                      ignore.case = TRUE))))
  u <- tolower(sub("/+$", "", chr(brow[1, "URL__for_data_"])))
  if (nzchar(u)) paste0("url:", u) else NA_character_
}

related_parts <- function(x) {
  fam <- x$family
  if (is.null(fam) || nrow(fam) < 2) return(list(note = "", list = ""))
  n <- nrow(fam)
  self <- match(tolower(x$table), tolower(fam$table))
  # A nom twin is the same responses coded differently, not a different measure,
  # so sharing its construct does not make this table indistinguishable.
  peers <- fam[-self, , drop = FALSE]
  peers <- peers[tolower(peers$table) != tolower(x$twin), , drop = FALSE]
  cn <- fam$construct[self]
  what <- if (nzchar(cn) && !(tolower(cn) %in% tolower(peers$construct))) {
    paste0("this one measures ", esc(cn))
  } else {
    # The part of the name the family does not share: enem_2020_1mil_mt in a
    # family of enem_* tables is the "2020_1mil_mt" table.
    tk <- strsplit(tolower(fam$table), "_", fixed = TRUE)
    i <- 0
    while (i < min(lengths(tk)) - 1 &&
           length(unique(vapply(tk, `[`, character(1), i + 1))) == 1) i <- i + 1
    if (i > 0) paste0("this one is the <code>",
                      esc(paste(strsplit(x$table, "_", fixed = TRUE)[[1]][-(1:i)], collapse = "_")),
                      "</code> table") else ""
  }
  note <- paste0("<p class=\"note\">One of ", n, " tables from the same source",
                 if (nzchar(what)) paste0("; ", what) else "",
                 ". The others are listed under <a href=\"#related\">Related tables</a>.</p>\n")

  shown <- seq_len(n)[-self]
  if (length(shown) > RELATED_MAX) {
    half <- RELATED_MAX %/% 2
    shown <- ((self - 1 + c(-half:-1, 1:half)) %% n) + 1
    twin_i <- match(tolower(x$twin), tolower(fam$table))
    shown <- sort(unique(c(shown, if (!is.na(twin_i)) twin_i)))
  }
  items <- vapply(shown, function(i) paste0(
    "<li><a href=\"", SITE_URL, "/tables/", slug_of(fam$table[i]), "/\">", esc(fam$table[i]), "</a>",
    if (nzchar(fam$construct[i])) paste0(" &mdash; ", esc(fam$construct[i])) else "",
    if (grepl("_nom$", fam$table[i], ignore.case = TRUE)) " (nominal response coding)" else "",
    if (nzchar(fam$items[i])) paste0(" &middot; ", fam$items[i], " items") else "",
    "</li>"), character(1))
  list(note = note, list = paste0(
    if (length(shown) < n - 1) paste0("<p class=\"note\">", length(shown), " of the ", n - 1,
      " other tables from this source, the nearest to this one by name.</p>\n") else "",
    "<ul>\n", paste(items, collapse = "\n"), "\n</ul>\n"))
}

TAG_COLS <- c("age range", "child age (for child-focused studies)", "sample",
              "construct type", "measurement tool", "item format",
              "primary language(s)", "construct name")

# ------------------------------------------------------------------- JSON-LD

# schema.org/Dataset. Field order is fixed and the object is built the same way
# for every table, so two runs over unchanged data serialise byte-identically.
build_jsonld <- function(x) {
  d <- list(
    "@context"    = "https://schema.org/",
    "@type"       = "Dataset",
    name          = x$table,
    url           = x$page_url,
    identifier    = x$page_url,
    version       = paste0("IRW v", x$irw_version),
    datePublished = x$irw_released_date
  )
  d$description <- x$long_description
  if (!blank(x$license))   d$license   <- x$license
  if (!blank(x$doi_url))   d$citation  <- x$doi_url
  if (!blank(x$reference)) d$creditText <- x$reference

  # Google reads the nested isPartOf object as a second Dataset item on the page
  # and holds it to the same required fields as the top-level one, so it needs a
  # description of its own; without it Search Console reports "Missing field
  # description" for every table page (2026-09-19).
  d$isPartOf <- list(
    "@type" = "Dataset",
    name    = "Item Response Warehouse",
    description = paste0("The Item Response Warehouse (IRW), a harmonised collection of ",
                         "item-level response data drawn from public sources and released ",
                         "in a single long format for psychometric research."),
    url     = SITE_URL,
    version = paste0("IRW v", x$irw_version)
  )
  d$includedInDataCatalog <- list(
    "@type" = "DataCatalog", name = "Item Response Warehouse", url = SITE_URL)
  d$creator <- list(
    "@type" = "Organization", name = "Item Response Warehouse", url = SITE_URL)
  d$publisher <- list(
    "@type" = "Organization", name = "Stanford University Redivis", url = "https://redivis.com")

  # A simulated table is based on the script that generated it, not on a study.
  if (x$src == "sim" && !blank(x$source_url)) d$isBasedOn <- x$source_url
  if (length(x$keywords)) d$keywords <- x$keywords
  if (length(x$variables)) {
    d$variableMeasured <- lapply(x$variables, function(v)
      list("@type" = "PropertyValue", name = v))
  }
  # The CSV download is listed only when x$rows_url exists, i.e. the table is
  # small enough for Redivis to serve without a login (see ANON_MAX_BYTES). A
  # larger table gets its Redivis page instead, which is not a data file and is
  # labelled as such rather than as text/csv.
  csv <- if (nzchar(x$rows_url))
    list("@type" = "DataDownload", name = paste0(x$table, " (CSV)"),
         encodingFormat = "text/csv", contentUrl = x$rows_url)
  else
    list("@type" = "DataDownload", name = paste0(x$table, " on Redivis"),
         encodingFormat = "text/html", contentUrl = x$redivis_url)
  d$distribution <- list(
    csv,
    list("@type" = "DataDownload", name = paste0(x$table, " (Croissant)"),
         encodingFormat = "application/ld+json", contentUrl = x$croissant_url)
  )
  d
}

# --------------------------------------------------------------- Croissant 1.0

build_croissant <- function(x) {
  # Only the three standard columns, whose names the data standard fixes. The
  # other column names come from irw_meta, which stores them lowercased, while
  # the table itself may not (chakraborty2026_SELOS_IRW has `cov_Gender`), and a
  # Croissant field naming a column that does not exist makes the whole file fail
  # to load. Reading their true case would take one Redivis call per table. The
  # extra columns are still in the CSV and still listed on the page.
  # A competitions table has no id/item/resp: each row is one comparison between
  # agent_a and agent_b, and `winner` records the outcome. A nom table adds
  # `text`, the option the respondent chose.
  std <- switch(x$src,
    comp = c("agent_a", "agent_b", "winner"),
    nom  = c("id", "item", "resp", "text"),
    c("id", "item", "resp"))
  core <- intersect(std, x$variables)
  fields <- lapply(core, function(v) {
    list("@type" = "cr:Field",
         "@id"   = paste0("responses/", v),
         name    = v,
         description = paste0("The '", v, "' column of the IRW table."),
         dataType = if (v == "resp") "sc:Float" else "sc:Text",
         source  = list("fileObject" = list("@id" = "redivis-table"),
                        "extract"    = list("column" = v)))
  })
  list(
    # The official Croissant 1.0 @context, verbatim. mlcroissant warns on any
    # abridged version of it, so this is copied whole rather than trimmed to the
    # keys we happen to use.
    "@context" = list(
      "@language" = "en", "@vocab" = "https://schema.org/",
      citeAs = "cr:citeAs", column = "cr:column", conformsTo = "dct:conformsTo",
      cr = "http://mlcommons.org/croissant/",
      data = list("@id" = "cr:data", "@type" = "@json"),
      dataBiases = "cr:dataBiases", dataCollection = "cr:dataCollection",
      dataType = list("@id" = "cr:dataType", "@type" = "@vocab"),
      dct = "http://purl.org/dc/terms/", examples = list("@id" = "cr:examples", "@type" = "@json"),
      extract = "cr:extract", field = "cr:field", fileProperty = "cr:fileProperty",
      fileObject = "cr:fileObject", fileSet = "cr:fileSet", format = "cr:format",
      includes = "cr:includes", isLiveDataset = "cr:isLiveDataset",
      jsonPath = "cr:jsonPath", key = "cr:key", md5 = "cr:md5",
      parentField = "cr:parentField", path = "cr:path",
      personalSensitiveInformation = "cr:personalSensitiveInformation",
      recordSet = "cr:recordSet", references = "cr:references", regex = "cr:regex",
      repeated = "cr:repeated", replace = "cr:replace", sc = "https://schema.org/",
      separator = "cr:separator", source = "cr:source", subField = "cr:subField",
      transform = "cr:transform"
    ),
    "@type"      = "sc:Dataset",
    "conformsTo" = "http://mlcommons.org/croissant/1.0",
    name         = gsub("[^A-Za-z0-9_-]", "_", x$table),
    description  = x$long_description,
    url          = x$page_url,
    # Croissant requires MAJOR.MINOR.PATCH. The IRW version is a single counter,
    # so it becomes the MAJOR component; "IRW v332" is what the page and the
    # schema.org block say, and the two must be read as the same fact.
    version      = paste0(x$irw_version, ".0.0"),
    # A real date, taken from the manifest row for this IRW version -- never the
    # current date, which would change the file on every render (rule 1).
    datePublished = x$irw_released_date,
    license      = if (!blank(x$license)) x$license else
                     "See the IRW record for licence terms.",
    citation     = if (!blank(x$reference)) x$reference else
                     paste0("Item Response Warehouse table '", x$table, "', IRW v",
                            x$irw_version, "."),
    citeAs       = if (!blank(x$reference)) x$reference else NULL,
    # TRUE although the data URL is pinned to one Redivis version. A non-live
    # dataset must carry a sha256/md5 per file (mlcroissant rejects it without),
    # and no stable hash exists: Redivis returns rows in arbitrary order, so the
    # same version's CSV differs byte-for-byte between requests.
    isLiveDataset = TRUE,
    distribution = list(list(
      "@type"         = "cr:FileObject",
      "@id"           = "redivis-table",
      name            = "redivis-table",
      # A table over ANON_MAX_BYTES has no anonymous CSV URL. Its file still
      # validates, pointing at the Redivis page, but a loader reads nothing from
      # it; the description says so, since the file itself cannot.
      description     = paste0("The '", x$table, "' table as released on Redivis in ",
                               x$shard, " ", x$shard_version, ".",
                               if (!nzchar(x$rows_url))
                                 paste0(" This table is larger than Redivis serves ",
                                        "without a login, so contentUrl is its Redivis ",
                                        "page; download it with the irw R or Python ",
                                        "package instead.") else ""),
      contentUrl      = if (nzchar(x$rows_url)) x$rows_url else x$redivis_url,
      encodingFormat  = "text/csv",
      sha256          = NULL
    )),
    recordSet = list(list(
      "@type" = "cr:RecordSet", "@id" = "responses", name = "responses",
      description = if (x$src == "comp")
        "One row per comparison between two agents, per the IRW competitions format."
        else "One row per person-item response, per the IRW data standard.",
      field = fields
    ))
  )
}

# --------------------------------------------------------------------- the page

PAGE_CSS <- paste0(
"body{font-family:system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;line-height:1.55;",
"max-width:52rem;margin:0 auto;padding:1.5rem 1.25rem 4rem;color:#1c1c1c}",
"a{color:#8c1515}h1{font-size:1.6rem;margin:.2rem 0 .1rem;word-break:break-word}",
"h2{font-size:1.05rem;margin:1.9rem 0 .5rem;padding-bottom:.25rem;",
"border-bottom:1px solid #e3e3e3;text-transform:uppercase;letter-spacing:.04em;color:#555}",
".sub{color:#666;font-size:.9rem;margin:0 0 1.2rem}",
"table.kv{border-collapse:collapse;width:100%;font-size:.93rem}",
"table.kv th{text-align:left;font-weight:600;padding:.32rem .8rem .32rem 0;",
"vertical-align:top;width:15rem;color:#444}",
"table.kv td{padding:.32rem 0;vertical-align:top}",
"table.kv tr+tr th,table.kv tr+tr td{border-top:1px solid #f0f0f0}",
"pre{background:#f7f7f8;border:1px solid #e6e6e6;border-radius:5px;padding:.7rem .85rem;",
"overflow-x:auto;font-size:.85rem}",
"nav.crumb{font-size:.85rem;color:#777;margin-bottom:1rem}",
"footer{margin-top:2.5rem;padding-top:1rem;border-top:1px solid #e3e3e3;",
"font-size:.82rem;color:#777}",
".pill{display:inline-block;background:#f2f2f4;border-radius:3px;padding:.1rem .45rem;",
"margin:0 .3rem .3rem 0;font-size:.82rem}",
".issue{background:#fff6e5;border:1px solid #f0c987;border-left:5px solid #d98b1f;",
"border-radius:5px;padding:.85rem 1rem;margin:1.2rem 0 1.6rem}",
".issue p{margin:.35rem 0}",
"input.find{width:100%;max-width:24rem;padding:.45rem .6rem;font-size:.95rem;",
"border:1px solid #ccc;border-radius:5px;margin:.2rem 0 1rem}",
".btns{display:flex;flex-wrap:wrap;gap:.6rem;margin:.4rem 0 .5rem}",
".btn{display:inline-flex;flex-direction:column;padding:.55rem .95rem;border-radius:6px;",
"border:1px solid #8c1515;text-decoration:none;font-weight:600;font-size:.93rem;line-height:1.3}",
".btn small{font-weight:400;font-size:.75rem;opacity:.85}",
".btn.primary{background:#8c1515;color:#fff}",
".btn:hover{background:#f7eded}.btn.primary:hover{background:#6f1010}",
".note{font-size:.88rem;color:#555}",
".cbnote{font-size:.88rem;color:#444;background:#f6f7f9;border-left:4px solid #9aa3b2;",
"padding:.5rem .8rem;margin:.2rem 0 .8rem}",
".scroll{overflow-x:auto}",
"table.cb{border-collapse:collapse;width:100%;font-size:.88rem;min-width:34rem}",
"table.cb thead th{text-align:left;font-weight:600;color:#444;padding:.35rem .6rem .35rem 0;",
"border-bottom:1px solid #e3e3e3}",
"table.cb td{padding:.38rem .6rem .38rem 0;vertical-align:top;border-top:1px solid #f0f0f0}",
"table.cb tr.grp th{text-align:left;font-size:.74rem;text-transform:uppercase;letter-spacing:.06em;",
"color:#777;padding:.9rem 0 .2rem;font-weight:600}",
"code.col{background:#f2f2f4;border-radius:3px;padding:.05rem .35rem;font-size:.84rem;white-space:nowrap}",
".chip{display:inline-block;font-size:.7rem;border-radius:3px;padding:0 .35rem;margin-right:.25rem;",
"border:1px solid;white-space:nowrap}",
".chip.std{color:#1d5e3a;border-color:#a9d3b8;background:#eef8f1}",
".chip.cov{color:#3e4a6b;border-color:#c3cbe0;background:#f1f3f9}",
".chip.src{color:#6b4a12;border-color:#e3cd9b;background:#fbf6ea}",
".chip.none{color:#8c1515;border-color:#e6b9b9;background:#fbefef}",
".muted{color:#777;font-style:italic}",
".vals{display:block;font-size:.84rem;color:#444;margin-top:.15rem}",
".val{white-space:nowrap}",
"details.vals summary{cursor:pointer;color:#8c1515}",
"th.s{cursor:pointer;user-select:none}th.s:hover{color:#8c1515}",
"td.restrict{background:#fff6e5}",
".flag{display:inline-block;font-size:.76rem;background:#fff6e5;border:1px solid #f0c987;",
"border-radius:3px;padding:0 .35rem;margin-left:.4rem;white-space:nowrap;text-decoration:none}",
".copy{font-family:inherit;font-size:.8rem;padding:.25rem .6rem;border:1px solid #8c1515;",
"background:#fff;color:#8c1515;border-radius:5px;cursor:pointer;margin:.15rem 0 0}",
".copy:hover{background:#f7eded}",
".licnote{background:#fff6e5;border-left:4px solid #d98b1f;padding:.5rem .8rem;",
"margin:.2rem 0 .7rem;font-size:.93rem}")

# Plain-language restrictions for a licence string, or character(0) when it is
# not restrictive. Only NC and ND count: those limit what a user may do with the
# data. SA is a condition on sharing adaptations, not a restriction on use.
licence_terms <- function(lic) {
  if (blank(lic)) return(character(0))
  u <- toupper(lic)
  c(if (grepl("\\bNC\\b", u)) "non-commercial use only",
    if (grepl("\\bND\\b", u)) "no derivative works may be shared",
    if (grepl("\\bNC\\b|\\bND\\b", u) && grepl("\\bSA\\b", u))
      "adaptations must be shared under the same licence")
}

build_page <- function(x) {
  # A table with an open data defect (landing/known_issues.tsv) keeps its page,
  # but is kept out of search until the fix ships: noindex, no Dataset JSON-LD,
  # no Croissant file, no sitemap entry. The banner is what a visitor sees.
  flagged <- nzchar(x$issue)
  jsonld <- toJSON(build_jsonld(x), auto_unbox = TRUE, pretty = TRUE, null = "null")
  issue_url <- paste0(ISSUE_URL, x$issue)
  banner <- if (flagged) paste0(
    "<div class=\"issue\">\n<p><strong>Known data issue.</strong> This table has an open ",
    "defect that is being fixed; see <a href=\"", esc(issue_url), "\">irw#", esc(x$issue),
    "</a> before relying on it. This notice is removed when the corrected table is ",
    "released.</p>\n</div>\n") else ""

  # Not a warning, a fact about what the rows are: nobody responded to these items.
  sim_note <- if (x$src == "sim") paste0(
    "<div class=\"issue\">\n<p><strong>Simulated data.</strong> These responses were ",
    "generated by a script, not collected from people",
    if (!blank(x$source_url)) paste0("; see the <a href=\"", esc(x$source_url),
                                     "\">generating script</a>") else "",
    ".",
    if (length(x$truth_cols)) paste0(" The ",
      paste0("<code>", vapply(x$truth_cols, esc, character(1)), "</code>", collapse = ", "),
      if (length(x$truth_cols) == 1) " column holds" else " columns hold",
      " the true generating values, which a real dataset would not have.") else "",
    "</p>\n</div>\n") else ""
  banner <- paste0(banner, sim_note)

  # A nom table and the core table it recodes link to each other: the same
  # responses, with and without the option the respondent chose.
  twin <- if (nzchar(x$twin)) {
    href <- paste0(SITE_URL, "/tables/", slug_of(x$twin), "/")
    paste0("<p class=\"note\">",
      if (x$src == "nom") "The scored version of this table is "
      else "A version of this table that keeps the option each respondent chose is ",
      "<a href=\"", esc(href), "\">", esc(x$twin), "</a>.</p>\n")
  } else ""

  related <- related_parts(x)

  size <- kv_rows(list(
    list(if (x$src == "comp") "Comparisons" else "Responses", num_fmt(x$m$n_responses)),
    list("Respondents",               num_fmt(x$m$n_participants)),
    list("Agents",                    num_fmt(x$m$n_actors)),
    list("Items",                     num_fmt(x$m$n_items)),
    list("Response categories",       num_fmt(x$m$n_categories)),
    list("Responses per respondent",  num_fmt(x$m$responses_per_participant)),
    list("Responses per item",        num_fmt(x$m$responses_per_item)),
    list("Density",                   num_fmt(x$m$density)),
    list("Longitudinal",              x$m$longitudinal)))

  about <- kv_rows(list(
    list("Description", x$description),
    list("Reference",   x$reference),
    list("DOI",         x$doi),
    # Bold on every page, so terms like NC or ND are hard to miss (Ben, 2026-09-19).
    list("Licence",     x$license, TRUE),
    list(if (x$src == "sim") "Generating script" else "Source data", x$source_url)))

  # A table found through another collection says so in that collection's own
  # words, linked to its record there (ben-domingue/irw#2421).
  if (!blank(x$source_via)) {
    via <- if (!blank(x$source_url)) paste0("<a href=\"", esc(x$source_url), "\">",
                                            esc(x$source_via), "</a>") else esc(x$source_via)
    note <- if (!blank(x$via_note))
      sub(esc(x$source_via), via, esc(x$via_note), fixed = TRUE) else
      paste0("These data were found via ", via, ".")
    about <- paste0(about, "<p class=\"note\">", note, "</p>\n")
  }

  tagbody <- ""
  if (length(x$tags)) {
    tagbody <- kv_rows(lapply(names(x$tags), function(k) list(k, x$tags[[k]])))
  }

  itext <- ""
  if (!is.null(x$it)) {
    itext <- paste0(
      "<p>This table has item text in the IRW: the wording administered to ",
      "respondents, not just the response codes.</p>",
      kv_rows(list(
        list("Instrument",                    x$it$instrument),
        list("Mean words per item",           num_fmt(x$it$mean_word)),
        list("Mean characters per item",      num_fmt(x$it$mean_character)),
        list("Mean characters per response",  num_fmt(x$it$mean_character_responses)),
        list("Flesch-Kincaid grade level",    num_fmt(x$it$FK_grade)))))
  }

  # Source caveats (data_notes.csv): information, not a warning, so no banner.
  notes <- ""
  if (!is.null(x$notes) && nrow(x$notes)) {
    notes <- paste0("<ul>\n", paste0(vapply(seq_len(nrow(x$notes)), function(i) {
      iss <- trimws(x$notes$issue[i])
      paste0("<li>", gsub("`([^`]+)`", "<code>\\1</code>", esc(x$notes$note[i])),
             if (grepl("^#[0-9]+$", iss)) paste0(" (<a href=\"", esc(paste0(ISSUE_URL, sub("#", "", iss))),
                                                "\">irw", esc(iss), "</a>)") else "",
             "</li>")
    }, character(1)), collapse = "\n"), "\n</ul>\n",
    "<p>These notes describe the source data, which the IRW reproduces as released. ",
    "All notes are listed on the <a href=\"", SITE_URL, "/data_notes.html\">Data Notes</a> page.</p>\n")
  }

  vars <- ""
  if (length(x$variables)) {
    vars <- paste0("<p>",
      paste0("<span class=\"pill\">", vapply(x$variables, esc, character(1)),
             "</span>", collapse = ""), "</p>\n", columns_note(x))
  }

  btn <- function(href, label, hint, cls = "btn")
    paste0("<a class=\"", cls, "\" href=\"", esc(href), "\">", label,
           "<small>", hint, "</small></a>")
  # Restrictive licences (NC or ND) are repeated, in plain words, right above the
  # download buttons -- where someone takes the data, not only in the About box
  # (Ben, 2026-09-19).
  terms <- licence_terms(x$license)
  # A Custom licence has no fixed meaning, so its note says to check the terms,
  # quoting them where the dictionary records them (19 of 313 on 2026-09-19) and
  # pointing at the source otherwise. "Permission via Email" gets no note: the
  # licence shown is the whole story (Ben, 2026-09-19).
  custom <- identical(tolower(chr(x$license)), "custom")
  licnote <- if (length(terms)) paste0(
    "<p class=\"licnote\">Licence: <strong>", esc(x$license), "</strong> &mdash; ",
    paste(terms, collapse = "; "), ".</p>\n") else if (custom) paste0(
    "<p class=\"licnote\"><strong>Custom licence</strong> &mdash; check the terms before reuse",
    if (!blank(x$license_terms)) paste0(": ", esc(x$license_terms))
    else if (!blank(x$source_url)) paste0(" at the <a href=\"", esc(x$source_url),
                                          "\">source data</a>")
    else "",
    if (!blank(x$license_terms) && grepl("[.!?]$", chr(x$license_terms))) "" else ".",
    "</p>\n") else ""
  # The source paper's BibTeX, exactly the string docs.qmd hands out from the
  # dictionary's BibTex column. Nothing here says how to cite the IRW itself:
  # what counts as a release is unsettled (irw#1870, irw#2317), and a citation
  # form invented on 4,000 pages would have to be withdrawn from all of them.
  # The button degrades to a selectable <pre> where clipboard access is refused.
  copyable <- function(bib) paste0(
    "<pre>", esc(bib), "</pre>\n",
    "<button class=\"copy\" type=\"button\" onclick=\"",
    "var b=this,t=b.previousElementSibling.textContent;",
    "navigator.clipboard.writeText(t).then(function(){",
    "b.textContent='Copied';setTimeout(function(){b.textContent='Copy BibTeX'},2000)})",
    "\">Copy BibTeX</button>\n")
  cite <- if (!blank(x$bibtex)) copyable(x$bibtex) else ""
  # The collection the table was found through asks to be cited too (#2421).
  if (!blank(x$via_bibtex)) cite <- paste0(cite,
    "<p class=\"note\">This table was found via ", esc(x$source_via),
    "; please also cite:</p>\n", copyable(x$via_bibtex))

  access <- paste0(licnote,
if (nzchar(x$rows_url)) paste0(
"<div class=\"btns\">",
btn(x$rows_url, "Download CSV", "no account needed", "btn primary"),
btn(x$redivis_url, "Browse on Redivis", "explore and query"),
if (!flagged) btn("croissant.jsonld", "Croissant metadata", "Hugging Face, Kaggle, OpenML") else "",
"</div>\n") else paste0(
"<div class=\"btns\">",
btn(x$redivis_url, "Browse on Redivis", "sign in to download"),
"</div>\n",
"<p class=\"note\">This table is larger than Redivis serves as a CSV without a login. ",
"The R package below downloads it with no account; the Python package and the ",
"Redivis website need you to sign in to Redivis.</p>\n"),
"<p class=\"note\">Or load it directly in R or Python:</p>\n",
"<pre># R (no account needed)\ninstall.packages(\"remotes\")\nremotes::install_github(\"itemresponsewarehouse/Rpkg\")\n",
"library(irw)\ndf &lt;- irw_fetch(\"", esc(x$table), "\"",
if (x$src != "core") paste0(", source = \"", x$src, "\"") else "", ")</pre>\n",
"<pre># Python (needs a free Redivis account)\npip install irw\n\n",
"import irw\ndf = irw.fetch(\"", esc(x$table), "\"",
if (x$src != "core") paste0(", source=\"", x$src, "\"") else "", ")</pre>\n")

  prov <- kv_rows(list(
    list("IRW version",               paste0("v", x$irw_version)),
    list("Redivis dataset",           paste0(x$shard, " ", x$shard_version)),
    list("Redivis dataset DOI",       x$shard_doi),
    list("Manifest pin for this IRW version", x$manifest_pin),
    list("Metadata source",           paste0("irw_meta ", x$meta_version))))

  paste0(
"<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
"<title>", esc(x$table), " &mdash; Item Response Warehouse</title>\n",
"<link rel=\"canonical\" href=\"", esc(x$page_url), "\">\n",
"<meta name=\"description\" content=\"", esc(substr(x$meta_description, 1, 300)), "\">\n",
if (flagged) "<meta name=\"robots\" content=\"noindex\">\n" else paste0(
"<link rel=\"alternate\" type=\"application/ld+json\" href=\"croissant.jsonld\" title=\"Croissant\">\n"),
"<style>", PAGE_CSS, "</style>\n",
if (!flagged) paste0("<script type=\"application/ld+json\">\n", jsonld, "\n</script>\n") else "",
"</head>\n<body>\n",
"<nav class=\"crumb\"><a href=\"", SITE_URL, "/\">Item Response Warehouse</a> / ",
"<a href=\"", SITE_URL, "/tables/\">Tables</a> / ", esc(x$table), "</nav>\n",
"<h1>", esc(x$table), "</h1>\n",
"<p class=\"sub\">", esc(x$size_sentence), "</p>\n",
banner,
twin,
related$note,
section("About this table", about),
section("Notes", notes, id = "notes"),
section("Size and shape", size),
section("Classification", tagbody),
section("Item text", itext),
{ cb <- codebook_html(x, STANDARD)
  if (nzchar(cb)) section("Codebook", cb, id = "codebook") else section("Columns", vars) },
section("Get the data", access),
section("How to cite", cite),
section("Related tables", related$list, id = "related"),
section("Version and provenance", prov),
"<footer>Part of the <a href=\"", SITE_URL, "/\">Item Response Warehouse</a>, ",
"IRW v", esc(x$irw_version), ". ",
"This page describes the table as released in ", esc(x$shard), " ",
esc(x$shard_version), ".</footer>\n",
"</body>\n</html>\n")
}

# ------------------------------------------------------------------ index page

# One inline handler, a fixed literal: it is emitted identically on every build,
# so the index stays byte-deterministic (the file is committed to gh-pages).
# Rows are moved with appendChild, which preserves each row's inline
# style.display -- a sort after a filter must not resurrect the hidden rows.
SORT_JS <- paste0(
"<script>\n",
"var sd={};\n",
"function srt(c,num){\n",
" var rs=[].slice.call(document.querySelectorAll('#tbl tr[data-t]'));\n",
" if(!rs.length)return;\n",
" var d=(c in sd)?!sd[c]:!num;sd[c]=d;\n",
" rs.sort(function(a,b){\n",
"  var x,y;\n",
"  if(num){x=+a.dataset.n;y=+b.dataset.n}\n",
"  else if(c==0){x=a.dataset.t;y=b.dataset.t}\n",
"  else{x=a.cells[c].textContent.toLowerCase();y=b.cells[c].textContent.toLowerCase()}\n",
"  return x<y?(d?-1:1):x>y?(d?1:-1):0});\n",
" var p=rs[0].parentNode;rs.forEach(function(r){p.appendChild(r)})}\n",
"</script>\n")

build_index <- function(rows, irw_version) {
  # Licence on the list, not only on the page: it decides whether a reader may
  # use a table at all, and until now it cost a click to find out. A table with
  # no recorded licence gets no page (irw#2266), so the column is never blank.
  # Restrictive licences (NC, ND) carry the same amber the download note uses.
  items <- paste0(vapply(rows, function(r) {
    flag <- if (nzchar(r$issue)) paste0(
      "<a class=\"flag\" href=\"", ISSUE_URL, esc(r$issue),
      "\" title=\"Open data defect -- see irw#", esc(r$issue), "\">known issue</a>") else ""
    paste0(
    "<tr data-t=\"", esc(r$slug), "\" data-n=\"", num_raw(r$n_responses), "\">",
    "<td><a href=\"", esc(r$slug), "/\">", esc(r$table), "</a>",
    if (nzchar(flag)) paste0(" ", flag) else "", "</td>",
    "<td>", esc(num_fmt(r$n_responses)), "</td>",
    "<td", if (length(licence_terms(r$license))) " class=\"restrict\"" else "", ">",
    esc(r$license), "</td>",
    "<td>", esc(r$shard), "</td></tr>")}, character(1)), collapse = "\n")
  paste0(
"<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
"<title>IRW table pages &mdash; Item Response Warehouse</title>\n",
"<link rel=\"canonical\" href=\"", SITE_URL, "/tables/\">\n",
"<meta name=\"description\" content=\"A page for each of the ",
format(length(rows), big.mark = ","), " tables in the Item Response Warehouse, ",
"each with schema.org and Croissant metadata.\">\n",
"<style>", PAGE_CSS, "</style>\n</head>\n<body>\n",
"<nav class=\"crumb\"><a href=\"", SITE_URL, "/\">Item Response Warehouse</a> / Tables</nav>\n",
"<h1>IRW table pages</h1>\n",
"<p class=\"sub\">", format(length(rows), big.mark = ","), " tables, each with a page ",
"naming the IRW version it describes and carrying schema.org/Dataset and Croissant ",
"metadata. Click a heading to sort. To filter by size, response type or ",
"classification, use ",
"<a href=\"", SITE_URL, "/data.html\">Browse the IRW Data</a>.</p>\n",
"<input class=\"find\" type=\"search\" placeholder=\"Filter by table name\" ",
"aria-label=\"Filter by table name\" oninput=\"",
"var q=this.value.toLowerCase();",
"document.querySelectorAll('#tbl tr[data-t]').forEach(function(r){",
"r.style.display=r.dataset.t.indexOf(q)<0?'none':''})\">\n",
"<table class=\"kv\" id=\"tbl\"><tr>",
"<th class=\"s\" onclick=\"srt(0,0)\">Table</th>",
"<th class=\"s\" onclick=\"srt(1,1)\">Responses</th>",
"<th class=\"s\" onclick=\"srt(2,0)\">Licence</th>",
"<th class=\"s\" onclick=\"srt(3,0)\">Redivis dataset</th></tr>\n",
items,
"\n</table>\n",
SORT_JS,
"<footer>Item Response Warehouse, IRW v", esc(irw_version), ".</footer>\n",
"</body>\n</html>\n")
}

# ----------------------------------------------------------------- tombstones

# A withdrawn table keeps its URL: an indexed or cited address must not become a
# 404. Deliberately vague (Ben, 2026-09-19): "Withdrawn" and the date, no reason.
# A renamed table also links its new name, when that has a page: a pointer, not a
# reason. noindex, no JSON-LD, no Croissant file, no sitemap entry.
build_tombstone <- function(table, date, renamed_to = "") {
  paste0(
"<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
"<title>", esc(table), " (withdrawn) &mdash; Item Response Warehouse</title>\n",
"<meta name=\"robots\" content=\"noindex\">\n",
"<style>", PAGE_CSS, "</style>\n</head>\n<body>\n",
"<nav class=\"crumb\"><a href=\"", SITE_URL, "/\">Item Response Warehouse</a> / ",
"<a href=\"", SITE_URL, "/tables/\">Tables</a> / ", esc(table), "</nav>\n",
"<h1>", esc(table), "</h1>\n",
"<div class=\"issue\"><p><strong>Withdrawn</strong>",
if (!blank(date)) paste0(" on ", esc(date)) else "", ".",
if (nzchar(renamed_to)) paste0(" It continues as <a href=\"", SITE_URL, "/tables/",
  slug_of(renamed_to), "/\">", esc(renamed_to), "</a>.") else "",
"</p></div>\n",
"<footer><a href=\"", SITE_URL, "/tables/\">All IRW table pages</a></footer>\n",
"</body>\n</html>\n")
}

# ------------------------------------------------------------------- sitemap

# The table pages' own sitemap, _site/sitemap-tables.xml. The site's sitemap.xml
# is an index over it and Quarto's pages, written by landing/write_sitemaps.R,
# which runs after this script.
# No <lastmod>. Nothing this script reads dates a table's content: every table's
# Redivis createdAt/updatedAt is the date of its shard's latest release (checked
# 2026-09-30 -- all 982 tables in item_response_warehouse read that day, including
# tables whose content hash had not changed since v60), so it would claim a whole
# shard changed on every release. Omitting it is better than a date that lies.
write_tables_sitemap <- function(urls) {
  writeLines(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
               "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">",
               paste0("  <url><loc>", sort(urls), "</loc></url>"),
               "</urlset>"), file.path("_site", "sitemap-tables.xml"))
  message("[landing] wrote sitemap-tables.xml with ", length(urls), " URLs")
}

.assert_no_slug_collisions <- function(tables) {
  s <- slug_of(tables)
  dup <- unique(s[duplicated(s)])
  if (length(dup)) {
    stop("[landing] slug collision -- these table names differ only by case: ",
         paste(tables[s %in% dup], collapse = ", "),
         "\nThe URL slug rule folds case; two tables cannot share one page.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------ main

main <- function() {
  manifest <- .read_manifest()
  aggregators <- .read_aggregators()
  notes_of <- .read_data_notes()
  coldocs_of <- .read_column_docs()
  covlabels_of <- .read_covariate_labels()
  cblinks_of <- .read_codebook_links()
  STANDARD <<- .read_standard()
  irw_version <- max(manifest$irw_version)
  pins <- manifest[manifest$irw_version == irw_version, ]
  pin_of <- setNames(pins$redivis_tag, pins$dataset)
  # The date this IRW version was released, straight from the manifest. This is
  # the only date any emitted file carries -- see rule 1 at the top.
  irw_released_date <- substr(chr(pins$irw_released_at[1]), 1, 10)

  # Tables are addressed by NAME, never as `name:referenceId`. A reference id
  # belongs to one version's table: `red_up` replaces a table by deleting and
  # recreating it (Redivis uploads append, so that is the only true replace),
  # and the v20.0 cut on 2026-09-04 minted a new id for all thirteen tables at
  # once. Every pinned id in this repo stopped resolving that morning.
  meta_ds  <- redivis$user("datapages")$dataset("irw_meta:bdxt")
  meta_ver <- meta_ds$get()$properties$version$tag
  md    <- as_df(meta_ds$table("metadata"))
  bib   <- as_df(meta_ds$table("biblio"))
  tg    <- as_df(meta_ds$table("tags"))
  itm   <- as_df(meta_ds$table("itemtext_metadata"))
  message("[landing] read irw_meta ", meta_ver, ": ", nrow(md), " metadata rows")
  # The non-core sources keep their own metadata/biblio (and, for nom, tags)
  # tables in irw_meta, with no `dataset` column of their own to rely on.
  for (i in seq_len(nrow(NONCORE))) {
    pf <- NONCORE$prefix[i]
    m2 <- as_df(meta_ds$table(paste0(pf, "_metadata")))
    m2$dataset <- NONCORE$dataset[i]
    md  <- bind_fill(md,  m2)
    bib <- bind_fill(bib, as_df(meta_ds$table(paste0(pf, "_biblio"))))
    tname <- paste0(pf, "_tags")
    if (tname %in% vapply(meta_ds$list_tables(), function(t) t$name, character(1)))
      tg <- bind_fill(tg, as_df(meta_ds$table(tname)))
    message("[landing] read ", pf, "_metadata: ", nrow(m2), " rows")
  }

  key <- function(df) tolower(trimws(as.character(df[[1]])))
  md$.k <- key(md); bib$.k <- key(bib); tg$.k <- key(tg); itm$.k <- key(itm)

  # One listing per shard, not one request per table: at ~4,000 tables the
  # per-table call was the whole runtime. Each listed table carries Redivis' own
  # URL (with the released version as ?v=; the path uses short ids that cannot be
  # derived from the name, and hand-built URLs 404) and numBytes, which decides
  # whether the anonymous CSV URL exists.
  shard_info <- list(); listed <- list()
  for (shard in names(PAGE_REF)) {
    ds <- redivis$user("datapages")$dataset(PAGE_REF[[shard]])
    p <- ds$get()$properties
    shard_info[[shard]] <- list(version = p$version$tag %||% "", doi = p$doi %||% "",
                                url = p$url %||% "")
    tabs <- ds$list_tables()
    listed[[shard]] <- data.frame(
      .k    = tolower(vapply(tabs, function(t) t$name, character(1))),
      url   = vapply(tabs, function(t) t$properties$url %||% "", character(1)),
      bytes = vapply(tabs, function(t) suppressWarnings(as.numeric(t$properties$numBytes %||% NA)),
                     numeric(1)),
      stringsAsFactors = FALSE)
    # Core tables' columns come from irw_meta's `variables`; the non-core
    # metadata tables have none, so their columns are read from Redivis, one call
    # per table (about a hundred in all).
    if (shard %in% NONCORE$dataset) {
      listed[[shard]]$vars <- vapply(tabs, function(t)
        paste(sort(vapply(t$list_variables(), function(v) v$name, character(1))),
              collapse = ","), character(1))
    }
    message("[landing] ", shard, " ", shard_info[[shard]]$version, ": ",
            length(tabs), " tables listed")
  }
  live_names <- unlist(lapply(listed, `[[`, ".k"), use.names = FALSE)

  tables <- page_tables(md, bib, live = live_names)
  # A tombstone for every withdrawn name that has no page. A name withdrawn in
  # one dataset but live in another keeps its page (page_tables), so no tombstone.
  withdrawn <- withdrawn_tbl()
  withdrawn <- withdrawn[!(tolower(withdrawn$table) %in% tolower(tables)), , drop = FALSE]
  withdrawn <- withdrawn[!duplicated(tolower(withdrawn$table)), , drop = FALSE]
  # A rename links on only when the new name has a page.
  withdrawn$renamed_to[!(tolower(withdrawn$renamed_to) %in% tolower(tables))] <- ""
  withdrawn$renamed_to <- tables[match(tolower(withdrawn$renamed_to), tolower(tables))]
  withdrawn$renamed_to[is.na(withdrawn$renamed_to)] <- ""
  issues <- known_issues()
  issue_of <- setNames(issues$issue, tolower(issues$table))
  .assert_no_slug_collisions(c(tables, withdrawn$table))
  in_shard <- md$table[as.character(md$dataset) %in% names(PAGE_REF)]
  held <- sort(in_shard[!(tolower(in_shard) %in% tolower(c(tables, withdrawn$table)))])
  message("[landing] ", length(tables), " tables get a page; ", nrow(withdrawn),
          " tombstones; ", length(held), " held (no licence, or not listed on Redivis)")

  # The anonymous CSV URL for a table, pinned to the dataset version the page
  # reports: datapages.<shard>:v7_0.<table>. Addressed by name, never by
  # reference id (see above). "" when Redivis would refuse it without a login;
  # an unknown size is treated as too large, never as small enough.
  rows_url_of <- function(shard, version, name, bytes) {
    if (is.na(bytes) || bytes > ANON_MAX_BYTES || !nzchar(version)) return("")
    paste0(ROWS_API, "datapages.", shard, ":", gsub(".", "_", version, fixed = TRUE),
           ".", name, "/rows?format=csv")
  }

  # Start from an empty directory, so a table that has lost its page (withdrawn
  # without a tombstone row, or its licence cleared) does not linger from a
  # previous local run. In CI _site/ is fresh anyway.
  unlink(OUT_DIR, recursive = TRUE)
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  rows <- list(); urls <- character(0); lagging <- character(0); too_big <- character(0)

  # nom <-> core twins: `x_nom` recodes core table `x`. Only pairs where both
  # tables have a page are linked.
  tk <- tolower(tables)
  twin_of <- setNames(rep("", length(tables)), tk)
  for (j in which(grepl("_nom$", tk))) {
    base <- sub("_nom$", "", tk[j])
    if (base %in% tk) {
      twin_of[tk[j]] <- tables[match(base, tk)]
      twin_of[base]  <- tables[j]
    }
  }

  # Families of tables from the same source, for the Related tables section.
  bi <- match(tk, bib$.k)
  fam_key <- vapply(bi, function(i) if (is.na(i)) NA_character_ else
                      source_key(bib[i, , drop = FALSE]), character(1))
  cn_col <- names(tg)[gsub("[^a-z]", "", tolower(names(tg))) == "constructname"][1]
  ti <- match(tk, tg$.k)
  fam_df <- data.frame(
    table = tables,
    construct = if (is.na(cn_col)) "" else
      vapply(ti, function(i) if (is.na(i)) "" else chr(tg[[cn_col]][i]), character(1)),
    items = vapply(match(tk, md$.k), function(i) if (is.na(i)) "" else
                     num_fmt(md$n_items[i]), character(1)),
    stringsAsFactors = FALSE)
  families <- split(seq_along(tables), fam_key)
  families <- families[lengths(families) > 1]
  family_of <- setNames(vector("list", length(tables)), tk)
  for (f in families) {
    df <- fam_df[f, , drop = FALSE]
    df <- df[order(tolower(df$table), method = "radix"), , drop = FALSE]
    rownames(df) <- NULL
    for (j in f) family_of[[tk[j]]] <- df
  }
  message("[landing] ", sum(lengths(families)), " tables in ", length(families),
          " multi-table sources get a Related tables section")

  for (tb in tables) {
    k <- tolower(tb)
    mrow <- md[md$.k == k, , drop = FALSE][1, ]
    shard <- chr(mrow$dataset)
    si <- shard_info[[shard]]
    lt <- listed[[shard]]
    lrow <- lt[lt$.k == k, , drop = FALSE]

    brow <- bib[bib$.k == k, , drop = FALSE]
    trow <- tg[tg$.k == k, , drop = FALSE]
    irow <- itm[itm$.k == k, , drop = FALSE]

    tags <- list()
    if (nrow(trow)) {
      for (cn in TAG_COLS) {
        col <- names(trow)[tolower(gsub("[^a-z]", "", tolower(names(trow)))) ==
                           gsub("[^a-z]", "", tolower(cn))]
        if (length(col) && !blank(trow[1, col[1]])) tags[[cn]] <- chr(trow[1, col[1]])
      }
    }

    src <- source_of(shard)
    vtxt <- if (src == "core") chr(mrow$variables) else
      if (nrow(lrow) && !is.null(lrow$vars)) chr(lrow$vars[1]) else ""
    vars <- character(0)
    if (nzchar(vtxt)) {
      vars <- trimws(unlist(strsplit(vtxt, "[,;|]")))
      vars <- sort(unique(vars[nzchar(vars)]))
    }

    slug <- slug_of(tb)
    page_url <- paste0(SITE_URL, "/tables/", slug, "/")
    doi <- if (nrow(brow)) clean_doi(brow[1, "DOI__for_paper_"]) else ""
    doi_url <- if (nzchar(doi)) {
      if (grepl("^https?://", doi)) doi else paste0("https://doi.org/", sub("^doi:\\s*", "", doi))
    } else ""

    size_sentence <- if (src == "comp") paste0(
      num_fmt(mrow$n_responses), " comparisons among ",
      num_fmt(mrow$n_actors), " agents.")
    else paste0(
      num_fmt(mrow$n_responses), if (src == "sim") " simulated" else "", " responses from ",
      num_fmt(mrow$n_participants), " respondents to ",
      num_fmt(mrow$n_items), " items.")

    manifest_pin <- pin_of[[shard]] %||% ""
    if (nzchar(manifest_pin) && nzchar(si$version) && manifest_pin != si$version)
      lagging <- unique(c(lagging, shard))

    x <- list(
      table = chr(mrow$table), slug = slug, m = mrow, it = if (nrow(irow)) irow[1, ] else NULL,
      tags = tags, variables = vars, shard = shard, shard_version = si$version,
      shard_doi = si$doi, meta_version = meta_ver, irw_version = irw_version,
      irw_released_date = irw_released_date,
      manifest_pin = manifest_pin, size_sentence = size_sentence,
      description = if (nrow(brow)) chr(brow[1, "Description"]) else "",
      reference   = if (nrow(brow)) chr(brow[1, "Reference_x"]) else "",
      bibtex      = if (nrow(brow) && "BibTex" %in% names(brow))
                      chr(brow[1, "BibTex"]) else "",
      license     = if (nrow(brow)) chr(brow[1, "Derived_License"]) else "",
      license_terms = if (nrow(brow) && "Custom_License_Terms" %in% names(brow))
                        chr(brow[1, "Custom_License_Terms"]) else "",
      source_url  = if (nrow(brow)) chr(brow[1, "URL__for_data_"]) else "",
      source_via  = if (nrow(brow) && "Source_via" %in% names(brow))
                      chr(brow[1, "Source_via"]) else "",
      doi = doi, doi_url = doi_url,
      keywords = c(unname(unlist(tags)),
                   switch(src, sim = "simulated data", comp = "paired comparisons",
                          nom = "nominal responses", NULL)),
      src = src, twin = unname(twin_of[k]), family = family_of[[k]],
      truth_cols = grep("^cov_true_", vars, value = TRUE),
      page_url = page_url,
      croissant_url = paste0(SITE_URL, "/tables/", slug, "/croissant.jsonld"),
      issue = if (is.na(issue_of[k])) "" else unname(issue_of[k]),
      notes = notes_of[[k]],
      coldocs = coldocs_of[[k]],
      covlabels = covlabels_of[[k]],
      cblinks = cblinks_of[[k]]
    )
    agg <- if (nzchar(x$source_via)) aggregators[[x$source_via]] else NULL
    x$via_note   <- if (is.null(agg)) "" else agg$note
    x$via_bibtex <- if (is.null(agg)) "" else agg$bibtex
    x$redivis_url <- if (nrow(lrow) && nzchar(lrow$url[1])) lrow$url[1] else si$url
    x$rows_url    <- rows_url_of(shard, si$version, x$table,
                                 if (nrow(lrow)) lrow$bytes[1] else NA)
    # Google Dataset Search wants a description of at least 50 characters, and the
    # dictionary Sheet's Description column is frequently a two-word label
    # ("Personality assessment"): 10 of the first 25 pages fell under the limit.
    # So the published description is the label, where there is one, followed by
    # the table's own measured facts. Every part of it is sourced, nothing invented.
    lead <- if (!blank(x$description)) {
      d0 <- chr(x$description)
      if (!grepl("[.!?]$", d0)) d0 <- paste0(d0, ".")
      d0
    } else ""
    x$long_description <- trimws(paste(
      lead,
      paste0(switch(src,
               sim  = "Simulated item response data",
               comp = "Paired-comparison data",
               nom  = "Item response data keeping the option each respondent chose,",
               "Item response data"),
             " in the Item Response Warehouse (IRW), a harmonised ",
             "collection of item-level response data for psychometric research."),
      size_sentence,
      if (length(tags)) paste0("Classified as: ",
        paste(unlist(tags), collapse = "; "), ".") else "",
      collapse = " "))
    x$meta_description <- x$long_description

    page_dir <- file.path(OUT_DIR, slug)
    dir.create(page_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(build_page(x), file.path(page_dir, "index.html"))
    # Only HTML pages go in the sitemap. Each page points at its own Croissant
    # file with <link rel="alternate">, which is how crawlers are meant to find it.
    # A flagged table gets neither, until its fix is released.
    if (!nzchar(x$issue)) {
      writeLines(toJSON(build_croissant(x), auto_unbox = TRUE, pretty = TRUE, null = "null"),
                 file.path(page_dir, "croissant.jsonld"))
      urls <- c(urls, page_url)
    }
    rows[[length(rows) + 1]] <- list(table = x$table, slug = slug, shard = shard,
                                     n_responses = mrow$n_responses,
                                     license = x$license, issue = x$issue)
    if (!nzchar(x$rows_url)) too_big <- c(too_big, x$table)
  }

  for (i in seq_len(nrow(withdrawn))) {
    page_dir <- file.path(OUT_DIR, slug_of(withdrawn$table[i]))
    dir.create(page_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(build_tombstone(withdrawn$table[i], withdrawn$date[i], withdrawn$renamed_to[i]),
               file.path(page_dir, "index.html"))
  }

  rows <- rows[order(vapply(rows, function(r) tolower(r$table), character(1)))]
  writeLines(build_index(rows, irw_version), file.path(OUT_DIR, "index.html"))
  urls <- c(urls, paste0(SITE_URL, "/tables/"))
  write_tables_sitemap(unique(urls))

  flagged_n <- sum(tolower(tables) %in% names(issue_of))
  message("[landing] emitted ", length(rows), " pages (", flagged_n,
          " with a known-issue banner, noindex) and ", length(rows) - flagged_n,
          " Croissant files; ", nrow(withdrawn), " tombstones")
  message("[landing] ", length(too_big), " tables exceed ", ANON_MAX_BYTES / 1e6,
          "MB and have no no-account CSV download")
  stale <- setdiff(tolower(issues$table), tolower(tables))
  if (length(stale))
    message("[landing] WARNING: known_issues.tsv names tables that get no page: ",
            paste(stale, collapse = ", "))
  if (length(held))
    message("[landing] held, no page (see ben-domingue/irw#2266): ",
            paste(held, collapse = ", "))
  if (length(lagging))
    message("[landing] WARNING: version_manifest.tsv lags Redivis for: ",
            paste(lagging, collapse = ", "),
            " -- pages report both the manifest pin and the live released version.")
  .warn_lost_pages(c(tables, withdrawn$table))
}

# A table that had a page on the live site and now has neither a page nor a
# tombstone is about to become a 404 on an indexed URL. Warn loudly; the fix is a
# row in the data repo's withdrawals.csv (or, failing that, landing/withdrawn.tsv). Reads the live sitemap, so it only ever affects
# the log, never the output (rule 1). Tables kept out of the sitemap -- flagged or
# tombstoned -- cannot be checked this way, which is why withdrawn.tsv is a file.
.warn_lost_pages <- function(emitted) {
  get <- function(u) tryCatch(readLines(url(u), warn = FALSE), error = function(e) character(0))
  live <- get(paste0(SITE_URL, "/sitemap.xml"))
  # Since the sitemap became an index, the table URLs are one file further down.
  if (any(grepl("<sitemapindex", live, fixed = TRUE))) {
    kids <- regmatches(live, regexpr("https?://[^<]+\\.xml", live))
    live <- unlist(lapply(kids, get))
  }
  slugs <- regmatches(live, regexpr("/tables/[^/<]+/", live))
  slugs <- sub("^/tables/", "", sub("/$", "", slugs))
  lost <- setdiff(slugs, slug_of(emitted))
  if (length(lost))
    message("[landing] WARNING: these tables have a page on the live site but will ",
            "not after this publish -- record them in ben-domingue/irw itemtext/withdrawals.csv: ",
            paste(sort(lost), collapse = ", "))
}

main()
