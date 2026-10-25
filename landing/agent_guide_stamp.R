# landing/agent_guide_stamp.R -- pre-render step (see _quarto.yml)
#
# Keeps the AI-assistant guides' numbers true, and stops the build when their
# FAMILY LIST is not.
#
# Why this file exists. `llms.txt` is the document the site tells a researcher
# to point their assistant at, and an assistant reading it cannot look anything
# up: whatever the file says is the whole of what it knows. In the 2026-10-24
# review it was the worst-maintained document in the system -- "~4,200 tables"
# against a live 6,720, "five DATA FAMILIES" with rankings (452 tables, its own
# standard, its own `source="rank"`) missing entirely, and no date or version
# stamp anywhere, so a reader could not even tell it was old. Ordinary
# staleness on a page a human reads is a nuisance; on this file it is the
# assistant's entire model of the corpus.
#
# So the counts are generated here, on the pattern of landing/hero_stats.R: from
# the PUBLISHED catalog, before every render, from the same source every other
# page reads.
#
# WHAT IS GENERATED AND WHAT IS NOT
#
#   generated   every count, and the AS OF stamp. These go stale by growing,
#               which is harmless to get wrong by a week and useless to
#               maintain by hand.
#   checked     the family LIST. A family is not a number: it has a `source`
#               value, a standard, its own shape rules and its own caveats, and
#               an assistant that has never heard of it will confidently apply
#               the core schema to its data. Adding a family to the catalog
#               therefore has to be a prose edit here, and a live family that
#               this file does not name is a HARD ERROR rather than a silently
#               appended row.
#   left alone   everything else. This is a text patcher, not a generator; the
#               prose is written by hand and stays that way.
#
# Failure: in CI (env CI set) a failed read or an undeclared family stops the
# render, as the landing pages do (ben-domingue/irw#2129), so a broken token
# cannot publish stale numbers and a new family cannot ship undocumented.
# Locally it warns and leaves llms.txt exactly as it is.
#
#   Rscript landing/agent_guide_stamp.R            # patch in place
#   Rscript landing/agent_guide_stamp.R --check     # report, change nothing

suppressPackageStartupMessages({
  library(jsonlite)
  library(redivis)
})

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
targets <- c("llms.txt")

# The families, by the `source` value the irw packages use, mapped to the
# Redivis datasets that hold them. Adding a shard means adding it here too;
# that is the same one-edit-per-file rule as ben-domingue/irw's
# metadata/redivis_config.R, which is authoritative for the dataset names.
FAMILIES <- list(
  core = c("item_response_warehouse", "item_response_warehouse_2",
           "item_response_warehouse_3", "item_response_warehouse_4",
           "item_response_warehouse_5", "item_response_warehouse_6",
           "item_response_warehouse_7", "item_response_warehouse_8"),
  conj = c("irw_conjoint", "irw_conjoint_2"),
  rank = c("irw_rank"),
  nom  = c("irw_nominal"),
  comp = c("irw_competitions"),
  sim  = c("irw_simsyn")
)
# Datasets that hold no response data, so they are not a family and must not be
# reported as an undeclared one. ARCHITECTURE.md section 2: "Auxiliary means
# only these."
AUXILIARY <- c("irw_meta", "irw_text", "irw_text_2", "irw_text_3")

fail <- function(msg) {
  if (nzchar(Sys.getenv("CI"))) stop("landing/agent_guide_stamp.R: ", msg, call. = FALSE)
  warning("landing/agent_guide_stamp.R: ", msg, " -- leaving the guides as they are.",
          call. = FALSE)
  quit(save = "no", status = 0)
}

count_tables <- function(dataset) {
  # By NAME, never name:referenceId -- ids rotate on every release.
  length(redivis$user("datapages")$dataset(dataset)$list_tables())
}

# The `datapages` Redivis account hosts several unrelated projects -- Wordbank,
# metalab, peekbank, palmerpenguins and more -- so "a dataset in the account"
# is not "an IRW family". IRW datasets are the ones whose REFERENCE begins with
# one of these; every family since nominal has followed the convention, and a
# family that did not would need a prose edit here anyway.
IRW_PREFIXES <- c("irw_", "item_response_warehouse")

read_catalog <- function() {
  # Datasets in the account, by REFERENCE and not by name: `name` is a display
  # name and does not resolve -- `irw_rank` shows as "IRW_rank" and the seventh
  # core warehouse as "Item Response Warehouse 7". The same distinction
  # metadata/check_client_reach.py documents in ben-domingue/irw.
  refs <- vapply(redivis$organization("datapages")$list_datasets(),
                 function(d) {
                   scoped <- d$properties$scopedReference
                   if (is.null(scoped)) NA_character_ else sub(":.*$", "", scoped)
                 }, character(1))
  refs <- refs[!is.na(refs)]
  refs[Reduce(`|`, lapply(IRW_PREFIXES, function(p) startsWith(refs, p)))]
}

counts <- tryCatch({
  out <- vapply(FAMILIES, function(datasets) sum(vapply(datasets, count_tables, numeric(1))),
                numeric(1))
  list(per_family = out,
       item_family = NA_integer_,
       version = redivis$user("datapages")$dataset("irw_meta:bdxt")$get()$properties$version$tag)
}, error = function(e) e)
if (inherits(counts, "error")) fail(paste0("could not read the catalog (",
                                           conditionMessage(counts), ")"))

# A live family nobody has written prose for. Checked by DATASET name against
# the configured lists, so a new shard of a known family is fine and a new
# family is not.
known <- c(unlist(FAMILIES, use.names = FALSE), AUXILIARY)
live <- tryCatch(read_catalog(), error = function(e) character(0))
undeclared <- setdiff(live, known)
if (length(undeclared) > 0) {
  fail(paste0("the catalog holds ", length(undeclared),
              " IRW dataset(s) this file does not describe: ",
              paste(undeclared, collapse = ", "),
              ". Add the family to FAMILIES and to the prose of ",
              paste(targets, collapse = ", "),
              " -- or to AUXILIARY if it holds no response data. ",
              "An assistant that has never heard of a family will apply the ",
              "core schema to its data."))
}

fmt <- function(n) formatC(n, format = "d", big.mark = ",")

# ---------------------------------------------------------------------------
# The Python capability claims, against the RELEASED wheel
# ---------------------------------------------------------------------------
#
# llms.txt's Python section names the functions Python does not have. That list
# does not go stale by growing, the way a table count does -- it goes stale by
# INVERTING, and silently: a reader is told to use R for something that has
# worked in Python for months. By PyPI 0.7.7 eight of its nine claims were
# false. `metadata()`, `use_version()`, `covariates()`, `table_sets()`,
# `check_resp()`, `resp2long()`, `merge()`, `recode()`, nominal support and
# `long2resp(resp_col=)` all existed; only "no tags()" held.
#
# So the claims are checked against the published wheel's `__all__`, not against
# the repository and not against anyone's memory. A name this file says is
# missing and the release exports is a hard error in CI.
PY_ABSENT <- c("tags", "predict", "simu_diff")

pypi_exports <- function() {
  info <- fromJSON("https://pypi.org/pypi/irw/json", simplifyVector = FALSE)
  version <- info$info$version
  wheel <- NULL
  for (u in info$urls) if (identical(u$packagetype, "bdist_wheel")) wheel <- u$url
  if (is.null(wheel)) stop("PyPI lists no wheel for irw ", version)
  tmp <- tempfile(fileext = ".whl")
  utils::download.file(wheel, tmp, quiet = TRUE, mode = "wb")
  dir <- tempfile(); dir.create(dir)
  utils::unzip(tmp, files = "irw/__init__.py", exdir = dir)
  text <- paste(readLines(file.path(dir, "irw", "__init__.py"), warn = FALSE),
                collapse = "\n")
  block <- regmatches(text, regexpr("(?s)__all__\\s*=\\s*\\[.*?\\]", text, perl = TRUE))
  if (length(block) != 1) stop("could not read __all__ from irw ", version)
  names <- regmatches(block, gregexpr("[\"'][A-Za-z_0-9]+[\"']", block))[[1]]
  list(version = version, exports = gsub("[\"']", "", names))
}

py <- tryCatch(pypi_exports(), error = function(e) e)
if (inherits(py, "error")) {
  fail(paste0("could not read the released Python package (",
              conditionMessage(py), ")"))
}
wrongly_absent <- intersect(PY_ABSENT, py$exports)
if (length(wrongly_absent) > 0) {
  fail(paste0("llms.txt says Python has no ",
              paste0(wrongly_absent, "()", collapse = ", "),
              ", and PyPI irw ", py$version, " exports ",
              if (length(wrongly_absent) == 1) "it" else "them",
              ". Update the Python section and PY_ABSENT together: a reader is ",
              "being told to use R for something that works in Python."))
}

# `item_family` is a column of the core tables, counted from the published
# metadata rather than from a dataset listing.
item_family <- tryCatch({
  meta <- as.data.frame(
    redivis$user("datapages")$dataset("irw_meta:bdxt")$table("metadata")$to_tibble())
  sum(grepl("item_family", meta$variables, fixed = TRUE))
}, error = function(e) NA_integer_)

values <- list(
  core = fmt(counts$per_family[["core"]]),
  all = fmt(sum(counts$per_family)),
  item_family = if (is.na(item_family)) NULL else fmt(item_family)
)

family_block <- paste0(
  "<!-- families:start -->\n",
  paste0(sprintf("  %-6s %6s  %s", names(FAMILIES), fmt(counts$per_family),
                 ifelse(names(FAMILIES) == "core", "(default)   id/item/resp, everything above",
                        paste0("source=\"", names(FAMILIES), "\""))),
         collapse = "\n"),
  "\n<!-- families:end -->")

pypkg_block <- paste0(
  "<!-- pypkg:start -->\n",
  "This section describes PyPI `irw` ", py$version,
  ", read from the released wheel.\n",
  "<!-- pypkg:end -->")

asof_block <- paste0(
  "<!-- asof:start -->\n",
  "AS OF: ", format(Sys.Date()), " · irw_meta ", counts$version, "\n",
  "Every count in this file is generated from the published catalog at that date by\n",
  "landing/agent_guide_stamp.R. If you are reading a copy older than a few weeks,\n",
  "the numbers have grown; the structure has not.\n",
  "<!-- asof:end -->")

patch_one <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  before <- text
  for (key in names(values)) {
    if (is.null(values[[key]])) next
    text <- gsub(paste0("(<!-- counts:", key, " -->)[^<]*(<!-- /counts:", key, " -->)"),
                 paste0("\\1", values[[key]], "\\2"), text, perl = TRUE)
  }
  # (?s) so `.` crosses newlines: without it these two blocks matched nothing
  # and the script reported the file "already current" while changing neither.
  text <- sub("(?s)<!-- families:start -->.*?<!-- families:end -->",
              family_block, text, perl = TRUE)
  text <- sub("(?s)<!-- asof:start -->.*?<!-- asof:end -->",
              asof_block, text, perl = TRUE)
  text <- sub("(?s)<!-- pypkg:start -->.*?<!-- pypkg:end -->",
              pypkg_block, text, perl = TRUE)
  if (identical(text, before)) {
    message("landing/agent_guide_stamp.R: ", path, " already current")
    return(invisible(FALSE))
  }
  if (check_only) {
    message("landing/agent_guide_stamp.R: ", path, " would change")
    return(invisible(TRUE))
  }
  writeLines(text, path)
  message("landing/agent_guide_stamp.R: patched ", path)
  invisible(TRUE)
}

stale <- vapply(targets, patch_one, logical(1))
if (check_only && any(stale)) quit(save = "no", status = 1)
