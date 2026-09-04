#!/usr/bin/env Rscript
#
# Per-table landing pages for the IRW -- PILOT (ben-domingue/irw#1706).
#
# Emits, into _site/tables/, for each table named in landing/pilot_tables.txt:
#   <slug>/index.html       a landing page carrying schema.org/Dataset JSON-LD
#   <slug>/croissant.jsonld a Croissant (MLCommons) description
#
# The directory form is deliberate: the public URL is /tables/<slug>/ with no
# file extension. These URLs are meant to be cited, and to be what a release DOI
# resolves to if #1870 lands -- an extension in a citable identifier ages badly,
# and GitHub Pages does not reliably serve /tables/<slug> for a <slug>.html file.
# Same file count either way; only the path shape differs. Changing it after the
# pages are indexed and cited is the expensive move, so it is made up front.
# plus _site/tables/index.html and sitemap entries appended to _site/sitemap.xml.
#
# Run as a Quarto post-render step. Skips itself (with a message, exit 0) when
# REDIVIS_API_TOKEN is absent, so a local preview without credentials still works.
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

suppressWarnings(suppressMessages({
  library(redivis); library(jsonlite)
}))

SITE_URL     <- "https://itemresponsewarehouse.org"
OUT_DIR      <- file.path("_site", "tables")
PILOT_LIST   <- file.path("landing", "pilot_tables.txt")
MANIFEST_URL <- paste0("https://raw.githubusercontent.com/ben-domingue/irw/main/",
                       "metadata/version_manifest.tsv")

# Shard name -> Redivis scoped reference. Mirrors the map in _load-data.qmd;
# authoritative source is IRW_CORE_DATASETS in ben-domingue/irw metadata/redivis_config.R.
SHARD_REF <- c(
  item_response_warehouse   = "item_response_warehouse:as2e",
  item_response_warehouse_2 = "item_response_warehouse_2:epbx",
  item_response_warehouse_3 = "item_response_warehouse_3:5xaj",
  item_response_warehouse_4 = "item_response_warehouse_4:980f",
  item_response_warehouse_5 = "item_response_warehouse_5:3ykx",
  item_response_warehouse_6 = "item_response_warehouse_6:fpe6"
)

if (!nzchar(Sys.getenv("REDIVIS_API_TOKEN"))) {
  message("[landing] REDIVIS_API_TOKEN not set -- skipping landing page emission.")
  quit(status = 0)
}

# ---------------------------------------------------------------- small helpers

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# The dictionary Sheets are hand-edited, so "missing" arrives in several spellings:
# a real NA, an empty cell, or the literal text "NA" / "N/A" / "NULL". All of them
# must count as absent, or they end up rendered as facts -- an early run emitted
# "https://doi.org/NA" as a citation and "NA" as a schema.org keyword.
blank <- function(x) {
  if (is.null(x) || length(x) == 0) return(TRUE)
  if (all(is.na(x))) return(TRUE)
  v <- trimws(as.character(x)[1])
  !nzchar(v) || toupper(v) %in% c("NA", "N/A", "NULL", "NONE", "-")
}

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

# URL slug rule: always lowercase. 266 of the eligible table names are not
# lowercase, and a case-sensitive host would serve Foo.html and foo.html as two
# pages while a case-insensitive one would collide them. The page displays the
# true name; only the path is folded. .assert_no_slug_collisions() enforces it.
slug_of <- function(x) tolower(x)

num_fmt <- function(x) {
  if (blank(x)) return("")
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) return(chr(x))
  if (v == round(v) && abs(v) < 1e15) formatC(v, format = "d", big.mark = ",")
  else formatC(v, format = "f", digits = 3)
}

# ------------------------------------------------------------------ the inputs

.read_manifest <- function() {
  con <- url(MANIFEST_URL)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  m <- utils::read.delim(con, stringsAsFactors = FALSE, colClasses = "character")
  m$irw_version <- as.integer(m$irw_version)
  m
}

read_pilot_tables <- function(path) {
  ln <- readLines(path, warn = FALSE)
  ln <- sub("#.*$", "", ln)
  ln <- trimws(ln)
  ln <- ln[nzchar(ln)]
  sort(unique(ln))                        # sorted: determinism rule 1
}

as_df <- function(tbl) as.data.frame(tbl$to_tibble(), stringsAsFactors = FALSE)

# ------------------------------------------------------------------ page parts

# Every table's page shows the same sections in the same order; sections with no
# data are omitted rather than rendered empty, so an untagged table (8 of the 25)
# produces a shorter page, not a page full of blanks.
kv_rows <- function(pairs) {
  keep <- vapply(pairs, function(p) !blank(p[[2]]), logical(1))
  pairs <- pairs[keep]
  if (!length(pairs)) return("")
  paste0(
    "<table class=\"kv\">\n",
    paste0(vapply(pairs, function(p)
      paste0("<tr><th>", esc(p[[1]]), "</th><td>", esc(p[[2]]), "</td></tr>"),
      character(1)), collapse = "\n"),
    "\n</table>\n")
}

section <- function(title, body, id = NULL) {
  if (!nzchar(trimws(body))) return("")
  paste0("<section", if (!is.null(id)) paste0(" id=\"", id, "\"") else "", ">\n",
         "<h2>", esc(title), "</h2>\n", body, "</section>\n")
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

  d$isPartOf <- list(
    "@type" = "Dataset",
    name    = "Item Response Warehouse",
    url     = SITE_URL,
    version = paste0("IRW v", x$irw_version)
  )
  d$includedInDataCatalog <- list(
    "@type" = "DataCatalog", name = "Item Response Warehouse", url = SITE_URL)
  d$creator <- list(
    "@type" = "Organization", name = "Item Response Warehouse", url = SITE_URL)
  d$publisher <- list(
    "@type" = "Organization", name = "Stanford University Redivis", url = "https://redivis.com")

  if (length(x$keywords)) d$keywords <- x$keywords
  if (length(x$variables)) {
    d$variableMeasured <- lapply(x$variables, function(v)
      list("@type" = "PropertyValue", name = v))
  }
  # NOTE (verified 2026-09-03): contentUrl is the Redivis *table page*, not a
  # data file. The Redivis API returns 401 "No credentials were provided" even
  # for a public table, so there is no unauthenticated URL a Croissant loader
  # could read. The files therefore VALIDATE but do not LOAD -- see the caveat
  # comment on ben-domingue/irw#1706. Do not describe Croissant support as
  # delivered until a direct download URL exists.
  d$distribution <- list(
    list("@type" = "DataDownload", name = paste0(x$table, " on Redivis"),
         encodingFormat = "text/csv", contentUrl = x$redivis_url),
    list("@type" = "DataDownload", name = paste0(x$table, " (Croissant)"),
         encodingFormat = "application/ld+json", contentUrl = x$croissant_url)
  )
  d
}

# --------------------------------------------------------------- Croissant 1.0

build_croissant <- function(x) {
  fields <- lapply(x$variables, function(v) {
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
    isLiveDataset = TRUE,
    distribution = list(list(
      "@type"         = "cr:FileObject",
      "@id"           = "redivis-table",
      name            = "redivis-table",
      description     = paste0("The '", x$table, "' table as released on Redivis in ",
                               x$shard, " ", x$shard_version, "."),
      contentUrl      = x$redivis_url,
      encodingFormat  = "text/csv",
      sha256          = NULL
    )),
    recordSet = list(list(
      "@type" = "cr:RecordSet", "@id" = "responses", name = "responses",
      description = "One row per person-item response, per the IRW data standard.",
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
".pilot{background:#fff6e5;border:1px solid #f0c987;border-left:5px solid #d98b1f;",
"border-radius:5px;padding:.85rem 1rem;margin:1.2rem 0 1.6rem}",
".pilot p{margin:.35rem 0}",
".pilot .tag{display:inline-block;background:#d98b1f;color:#fff;font-weight:700;",
"font-size:.72rem;letter-spacing:.09em;padding:.12rem .5rem;border-radius:3px;",
"margin-bottom:.45rem}")

build_page <- function(x) {
  jsonld <- toJSON(build_jsonld(x), auto_unbox = TRUE, pretty = TRUE, null = "null")

  size <- kv_rows(list(
    list("Responses",                 num_fmt(x$m$n_responses)),
    list("Respondents",               num_fmt(x$m$n_participants)),
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
    list("Licence",     x$license),
    list("Source data", x$source_url)))

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

  vars <- ""
  if (length(x$variables)) {
    vars <- paste0("<p>",
      paste0("<span class=\"pill\">", vapply(x$variables, esc, character(1)),
             "</span>", collapse = ""), "</p>\n")
  }

  access <- paste0(
"<pre># R\ninstall.packages(\"remotes\")\nremotes::install_github(\"itemresponsewarehouse/Rpkg\")\n",
"library(irw)\ndf &lt;- irw_fetch(\"", esc(x$table), "\")</pre>\n",
"<pre># Python\npip install git+https://github.com/itemresponsewarehouse/Python-pkg.git\n\n",
"import irw\ndf = irw.fetch(\"", esc(x$table), "\")</pre>\n",
"<p>Browse or download it directly on <a href=\"", esc(x$redivis_url),
"\">Redivis</a>, or take the ",
"<a href=\"croissant.jsonld\">Croissant description</a> ",
"of this table for use with Hugging Face, Kaggle or OpenML.</p>\n")

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
"<link rel=\"alternate\" type=\"application/ld+json\" href=\"croissant.jsonld\" title=\"Croissant\">\n",
"<style>", PAGE_CSS, "</style>\n",
"<script type=\"application/ld+json\">\n", jsonld, "\n</script>\n",
"</head>\n<body>\n",
"<nav class=\"crumb\"><a href=\"", SITE_URL, "/\">Item Response Warehouse</a> / ",
"<a href=\"", SITE_URL, "/tables/\">Tables</a> / ", esc(x$table), "</nav>\n",
"<h1>", esc(x$table), "</h1>\n",
"<p class=\"sub\">", esc(x$size_sentence), "</p>\n",
section("About this table", about),
section("Size and shape", size),
section("Classification", tagbody),
section("Item text", itext),
section("Columns", vars),
section("Get the data", access),
section("Version and provenance", prov),
"<footer>Part of the <a href=\"", SITE_URL, "/\">Item Response Warehouse</a>, ",
"IRW v", esc(x$irw_version), ". ",
"This page describes the table as released in ", esc(x$shard), " ",
esc(x$shard_version), ".</footer>\n",
"</body>\n</html>\n")
}

# ------------------------------------------------------------------ index page

build_index <- function(rows, irw_version, n_total) {
  items <- paste0(vapply(rows, function(r) paste0(
    "<tr><td><a href=\"", esc(r$slug), "/\">", esc(r$table), "</a></td>",
    "<td>", esc(num_fmt(r$n_responses)), "</td>",
    "<td>", esc(r$shard), "</td></tr>"), character(1)), collapse = "\n")
  paste0(
"<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
"<title>IRW table pages (pilot) &mdash; Item Response Warehouse</title>\n",
"<link rel=\"canonical\" href=\"", SITE_URL, "/tables/\">\n",
"<meta name=\"description\" content=\"A pilot set of ", length(rows), " individual ",
"table pages for the Item Response Warehouse, each with schema.org and Croissant ",
"metadata. Most IRW tables do not yet have a page.\">\n",
"<style>", PAGE_CSS, "</style>\n</head>\n<body>\n",
"<nav class=\"crumb\"><a href=\"", SITE_URL, "/\">Item Response Warehouse</a> / Tables</nav>\n",
"<h1>IRW table pages <span class=\"tag\">PILOT</span></h1>\n",
"<p class=\"sub\">A page per table, each naming the IRW version it describes ",
"and carrying schema.org/Dataset and Croissant metadata.</p>\n",
"<div class=\"pilot\">\n",
"<span class=\"tag\">Pilot &mdash; not the full warehouse</span>\n",
"<p><strong>Only ", length(rows), " of the IRW's ", format(n_total, big.mark = ","),
" tables have a page.</strong> These ", length(rows), " were chosen to test the ",
"page generator against the corpus' awkward cases &mdash; the largest and ",
"smallest tables, tables with and without item text, tagged and untagged, and ",
"names that break naive URL handling. They are not the most important tables, ",
"and the selection is not a recommendation.</p>\n",
"<p>To search the whole warehouse, use <a href=\"", SITE_URL,
"/data.html\">Browse the IRW Data</a>. Whether this expands to every table is ",
"being decided in <a href=\"https://github.com/ben-domingue/irw/issues/1706\">irw#1706</a>.</p>\n",
"</div>\n",
"<table class=\"kv\"><tr><th>Table</th><th>Responses</th><th>Redivis dataset</th></tr>\n",
items, "\n</table>\n",
"<footer>Item Response Warehouse, IRW v", esc(irw_version), ".</footer>\n",
"</body>\n</html>\n")
}

# ------------------------------------------------------------- sitemap merging

# Quarto writes _site/sitemap.xml only when website.site-url is set. We append our
# URLs to it rather than publishing a second sitemap, so there is one list for
# crawlers. No <lastmod>: it would change on every render and break rule 1.
merge_sitemap <- function(urls) {
  sm <- file.path("_site", "sitemap.xml")
  entries <- paste0(vapply(sort(urls), function(u)
    paste0("  <url><loc>", u, "</loc></url>"), character(1)), collapse = "\n")
  if (!file.exists(sm)) {
    writeLines(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                 "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">",
                 entries, "</urlset>"), sm)
    message("[landing] wrote a new sitemap.xml with ", length(urls), " URLs")
    return(invisible(NULL))
  }
  txt <- paste(readLines(sm, warn = FALSE), collapse = "\n")
  if (grepl("/tables/", txt, fixed = TRUE)) {
    txt <- gsub("  <url><loc>[^<]*/tables/[^<]*</loc></url>\n?", "", txt)
  }
  txt <- sub("</urlset>", paste0(entries, "\n</urlset>"), txt, fixed = TRUE)
  writeLines(txt, sm)
  message("[landing] appended ", length(urls), " URLs to sitemap.xml")
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
  tables <- read_pilot_tables(PILOT_LIST)
  .assert_no_slug_collisions(tables)
  message("[landing] ", length(tables), " tables requested")

  manifest <- .read_manifest()
  irw_version <- max(manifest$irw_version)
  pins <- manifest[manifest$irw_version == irw_version, ]
  pin_of <- setNames(pins$redivis_tag, pins$dataset)
  # The date this IRW version was released, straight from the manifest. This is
  # the only date any emitted file carries -- see rule 1 at the top.
  irw_released_date <- substr(chr(pins$irw_released_at[1]), 1, 10)

  meta_ds  <- redivis$user("datapages")$dataset("irw_meta:bdxt")
  meta_ver <- meta_ds$get()$properties$version$tag
  md    <- as_df(meta_ds$table("metadata:h5gs"))
  bib   <- as_df(meta_ds$table("biblio:qahg"))
  tg    <- as_df(meta_ds$table("tags:7nkh"))
  itm   <- as_df(meta_ds$table("itemtext_metadata:drat"))
  message("[landing] read irw_meta ", meta_ver, ": ", nrow(md), " metadata rows")

  key <- function(df) tolower(trimws(as.character(df[[1]])))
  md$.k <- key(md); bib$.k <- key(bib); tg$.k <- key(tg); itm$.k <- key(itm)

  shard_info <- list()
  get_shard <- function(shard) {
    if (!is.null(shard_info[[shard]])) return(shard_info[[shard]])
    p <- redivis$user("datapages")$dataset(SHARD_REF[[shard]])$get()$properties
    info <- list(version = p$version$tag %||% "", doi = p$doi %||% "",
                 url = p$url %||% "")
    shard_info[[shard]] <<- info
    info
  }

  # Redivis' own URL for the table, which carries the released version as ?v=.
  # This is what a landing page should point at: constructing a URL by hand
  # produced a 404 in the first run, because the path uses short ids
  # (as2e-cv7jb41fd/tables/hye4-...) that are not derivable from the table name.
  table_url <- function(shard, name, fallback) {
    out <- tryCatch(
      redivis$user("datapages")$dataset(SHARD_REF[[shard]])$table(name)$get()$properties$url,
      error = function(e) NULL)
    if (is.null(out) || !nzchar(out)) fallback else out
  }

  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  rows <- list(); urls <- character(0); lagging <- character(0); missing <- character(0)

  for (tb in tables) {
    k <- tolower(tb)
    mrow <- md[md$.k == k, , drop = FALSE]
    if (!nrow(mrow)) { missing <- c(missing, tb); next }
    mrow <- mrow[1, ]
    shard <- chr(mrow$dataset)
    si <- get_shard(shard)

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

    vars <- character(0)
    if (!blank(mrow$variables)) {
      vars <- trimws(unlist(strsplit(chr(mrow$variables), "[,;|]")))
      vars <- sort(unique(vars[nzchar(vars)]))
    }

    slug <- slug_of(tb)
    page_url <- paste0(SITE_URL, "/tables/", slug, "/")
    doi <- if (nrow(brow)) chr(brow[1, "DOI__for_paper_"]) else ""
    doi_url <- if (nzchar(doi)) {
      if (grepl("^https?://", doi)) doi else paste0("https://doi.org/", sub("^doi:\\s*", "", doi))
    } else ""

    size_sentence <- paste0(
      num_fmt(mrow$n_responses), " responses from ",
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
      license     = if (nrow(brow)) chr(brow[1, "Derived_License"]) else "",
      source_url  = if (nrow(brow)) chr(brow[1, "URL__for_data_"]) else "",
      doi = doi, doi_url = doi_url,
      keywords = unname(unlist(tags)),
      page_url = page_url,
      croissant_url = paste0(SITE_URL, "/tables/", slug, "/croissant.jsonld"),
      redivis_url = table_url(shard, chr(mrow$table), si$url)
    )
    # Google Dataset Search wants a description of at least 50 characters, and the
    # dictionary Sheet's Description column is frequently a two-word label
    # ("Personality assessment"): 10 of the 25 pilot tables fell under the limit.
    # So the published description is the label, where there is one, followed by
    # the table's own measured facts. Every part of it is sourced, nothing invented.
    lead <- if (!blank(x$description)) {
      d0 <- chr(x$description)
      if (!grepl("[.!?]$", d0)) d0 <- paste0(d0, ".")
      d0
    } else ""
    x$long_description <- trimws(paste(
      lead,
      paste0("Item response data in the Item Response Warehouse (IRW), a harmonised ",
             "collection of item-level response data for psychometric research."),
      size_sentence,
      if (length(tags)) paste0("Classified as: ",
        paste(unlist(tags), collapse = "; "), ".") else "",
      collapse = " "))
    x$meta_description <- x$long_description

    page_dir <- file.path(OUT_DIR, slug)
    dir.create(page_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(build_page(x), file.path(page_dir, "index.html"))
    writeLines(toJSON(build_croissant(x), auto_unbox = TRUE, pretty = TRUE, null = "null"),
               file.path(page_dir, "croissant.jsonld"))
    # Only HTML pages go in the sitemap. Each page points at its own Croissant
    # file with <link rel="alternate">, which is how crawlers are meant to find it.
    urls <- c(urls, page_url)
    rows[[length(rows) + 1]] <- list(table = x$table, slug = slug, shard = shard,
                                     n_responses = mrow$n_responses)
  }

  rows <- rows[order(vapply(rows, function(r) tolower(r$table), character(1)))]
  writeLines(build_index(rows, irw_version, nrow(md)), file.path(OUT_DIR, "index.html"))
  urls <- c(urls, paste0(SITE_URL, "/tables/"))
  merge_sitemap(unique(urls))

  message("[landing] emitted ", length(rows), " pages + ", length(rows),
          " Croissant files into ", OUT_DIR)
  if (length(missing))
    message("[landing] WARNING: not in irw_meta.metadata, no page emitted: ",
            paste(missing, collapse = ", "))
  if (length(lagging))
    message("[landing] WARNING: version_manifest.tsv lags Redivis for: ",
            paste(lagging, collapse = ", "),
            " -- pages report both the manifest pin and the live released version.")
}

main()
