#!/usr/bin/env Rscript
#
# vignette_versions_compute.R
#
# Resolves, for every vignette, which IRW version the corpus was at when that
# vignette's results were computed, and writes the answer to
# vignettes/vignette_versions.tsv.
#
# Why this is a script and not a call to irw_version() inside each .qmd:
#
#   1. The caches are frozen. A vignette's compute date never changes, so its
#      version never changes either; resolving it on every render would be a
#      network round trip per page for an answer that is already known.
#   2. renv.lock pins `irw` at a commit predating R/manifest.R, so irw_version()
#      is not callable during a site render at all. Bumping the lockfile to get
#      it would make local renders (which use a dev install) pass while CI
#      fails. Reading the manifest directly avoids the whole question -- the
#      same reason landing/emit_landing_pages.R has its own .read_manifest().
#
# The version is INFERRED FROM A DATE, never recorded at run time. See the
# "Accuracy" notes below and the caveat text in components/data_version.R.
#
# Usage:  Rscript vignettes/vignette_versions_compute.R
#         IRW_MANIFEST=/path/to/version_manifest.tsv Rscript ...   (offline)
#
# Deterministic: two runs over unchanged inputs write a byte-identical file.

MANIFEST_URL <- paste0(
  "https://raw.githubusercontent.com/ben-domingue/irw/main/",
  "metadata/version_manifest.tsv"
)

# ---------------------------------------------------------------- the manifest

read_manifest <- function() {
  local_copy <- Sys.getenv("IRW_MANIFEST", "")
  src <- if (nzchar(local_copy)) local_copy else MANIFEST_URL
  con <- if (nzchar(local_copy)) local_copy else url(src)
  if (!nzchar(local_copy)) on.exit(try(close(con), silent = TRUE), add = TRUE)

  m <- utils::read.delim(con, stringsAsFactors = FALSE, colClasses = "character")

  expected <- c("irw_version", "irw_released_at", "dataset", "redivis_tag",
                "redivis_released_at", "precision", "redivis_released_before")
  if (!identical(names(m)[seq_along(expected)], expected)) {
    stop("The version manifest does not have the expected columns; ",
         "this script needs updating to match red_up/manifest.py.", call. = FALSE)
  }

  m$irw_version <- as.integer(m$irw_version)
  m$released    <- as.POSIXct(m$irw_released_at, format = "%Y-%m-%dT%H:%M:%SZ",
                              tz = "UTC")
  m
}

# Which IRW version was live at the END of the given day.
#
# A compute date is a day, not an instant, and a run that finished on day D read
# whatever was live during D -- so the day's last version is the better answer
# than its first. Where several versions were released that day the choice is
# genuinely ambiguous, which is what n_versions_that_day records.
resolve_version <- function(manifest, date) {
  when <- as.POSIXct(paste0(date, " 23:59:59"), format = "%Y-%m-%d %H:%M:%S",
                     tz = "UTC")
  live <- manifest$irw_version[manifest$released <= when]
  if (!length(live)) {
    stop("No IRW version existed on or before ", date, call. = FALSE)
  }
  max(live)
}

# TRUE when the resolved version's own release TIMESTAMP is reconstructed
# rather than recorded.
#
# Redivis' releasedAt was overwritten by a platform migration for everything
# published before 21 July 2026; those rows carry createdAt instead and are
# marked `bracketed`. Substituting createdAt lands within an hour 58% of the
# time and is off by more than a week 9% of the time (worst case 23 days).
#
# The test is deliberately narrow. Most versions contain SOME bracketed row,
# because a shard that has not been republished keeps its old tag and that tag's
# reconstructed date forever -- v283 (10 August 2026) still carries
# item_response_warehouse v43.0 from April. That says nothing about when v283
# itself was cut. What dates an IRW version is the release that triggered it:
# the row whose redivis_released_at equals the version's irw_released_at. Only
# if THAT row is bracketed is the version's position in history uncertain, and
# only then does the pre-July-2026 caveat belong on the page.
is_bracketed <- function(manifest, version) {
  rows <- manifest[manifest$irw_version == version, , drop = FALSE]
  trigger <- rows[rows$redivis_released_at == rows$irw_released_at, , drop = FALSE]
  if (!nrow(trigger)) return(TRUE)   # no identifiable trigger: assume the worst
  any(trigger$precision == "bracketed")
}

# TRUE when the version pins any shard to a tag whose own release date is
# reconstructed. Distinct from is_bracketed(): the version's POSITION in history
# is still exact, but which tag of that shard was really live on the day is a
# weaker claim than the manifest's tidy table suggests. Nearly every version
# before the shards were last republished has this, so it is a mild note rather
# than a warning.
has_reconstructed_shards <- function(manifest, version) {
  any(manifest$precision[manifest$irw_version == version] == "bracketed")
}

n_released_on <- function(manifest, date) {
  same_day <- substr(manifest$irw_released_at, 1, 10) == date
  length(unique(manifest$irw_version[same_day]))
}

# ------------------------------------------------------------- the compute date
#
# Three sources, in descending order of trust:
#
#   date_run  -- stamped into the cache by the compute script at save time. The
#                only source that is actually a record of the run.
#   git       -- the commit that ADDED the cache files. An upper bound: the run
#                happened on or before this day. Used where no date_run exists.
#   override  -- hand-entered below, with a reason. Used where the other two
#                are absent or provably wrong.
#
# File mtime is deliberately NOT used. This tree lives in Dropbox, so mtimes
# track sync events rather than compute events; they disagree with date_run for
# 2pl, network_psych, lsirm and dimensionality, and are later in every case.

VIGNETTE_DIR <- "vignettes"

# vignette stem -> the cache file(s) whose date_run dates the page. Where a page
# has several caches the newest date_run wins, since the page as published
# reflects the last run that fed it.
CACHES <- list(
  "2pl_across_datasets"    = "2pldata/2pl_across_datasets_results.rds",
  "asymmetric_irt"         = c("asymmetric_irt_data/asymmetric_irt_results.rds",
                               "asymmetric_irt_data/convergence_sim.rds",
                               "asymmetric_irt_data/example_itemfit.rds",
                               "asymmetric_irt_data/validation_gate_log.rds"),
  "dimensionality"         = c("dimensionality_data/dimensionality_results.rds",
                               "dimensionality_data/dimensionality_merge_results.rds"),
  "dutch_identity"         = "dutchdata/dutch_identity_results.rds",
  "hf_reliability_paradox" = "hf_reliability/hf_reliability_results.rds",
  "il_hte"                 = "ilhtedata/il_hte_results.rds",
  "item_text_difficulty"   = "itemtextdata/item_text_difficulty_results.rds",
  "local_dependence"       = c("local_dependence_data/local_dependence_results.rds",
                               "local_dependence_data/q3_demo_results.rds"),
  "lsirm_interaction_maps" = c("lsirmdata/lsirm_interaction_maps_results_full.rds",
                               "lsirmdata/lsirm_interaction_maps_results_scout.rds"),
  "network_psych"          = c("network_psych_data/network_psych_results.rds",
                               "network_psych_data/network_psych_prior_sensitivity_results.rds",
                               "network_psych_data/network_psych_sbm_results.rds"),
  "rt_imv"                 = "rtimvdata/rt_imv_results.rds"
)

# Vignettes whose caches predate the date_run convention, dated from git instead.
#
# Their compute scripts now stamp date_run, but the existing caches do not carry
# one and are not being regenerated. After either script is next run, move its
# entry into CACHES above -- the switch is manual, not automatic. gender_dif
# writes one .rds per dataset rather than a single results file, so its CACHES
# entry will need the newest date_run across gender_dif_data/*.rds.
GIT_DATED <- list(
  "continuous_bounded" = "continuous_bounded_data",
  "gender_dif"         = "gender_dif_data"
)

# Vignettes that fetch from Redivis at render time and so have no fixed version;
# components/data_version.R reports the live version for these instead.
#
# irt_python is listed for completeness but does NOT use the include: it is a
# Python-only page on Quarto's jupyter engine, and an R chunk would flip it to
# knitr and pull in reticulate, which is not in renv.lock. Its version sentence
# is written out by hand in the .qmd, exactly as its source link already is.
LIVE <- c("cfa", "imv", "irt_python")

# Neither: diffsim is pure simulation and touches no IRW table.
NO_DATA <- c("diffsim", "index")

OVERRIDES <- list(
  # gender_dif.qmd has said "Results were computed on 2026-05-28" since the
  # vignette's first commit, but all 181 caches were added to git on 2026-07-16
  # and nothing in the repo supports the earlier date. The two resolve to
  # different versions (v248 vs v264), so this is left on the git date -- an
  # upper bound -- rather than trusting unsourced prose. UNRESOLVED: if the run
  # really was 2026-05-28, set this to that date and fix the version.
)

date_run_of <- function(rel_paths) {
  dates <- character(0)
  for (p in rel_paths) {
    full <- file.path(VIGNETTE_DIR, p)
    if (!file.exists(full)) next
    x <- try(readRDS(full), silent = TRUE)
    if (inherits(x, "try-error") || !is.list(x) || is.null(x$date_run)) next
    dates <- c(dates, format(as.Date(x$date_run), "%Y-%m-%d"))
  }
  if (!length(dates)) return(NA_character_)
  max(dates)
}

# The last commit that added or changed a cache FILE in the directory.
#
# Restricted to *.rds on purpose: commits touching the directory for a .bib or a
# helper script say nothing about when the data was computed, and would date the
# page too late. Taking the last such commit rather than the first matters for
# directories that accrete -- continuous_bounded_data was created on 2026-07-30
# but last recomputed on 2026-08-26, and only the later date describes what the
# page actually shows. This is an upper bound: the run happened on or before it.
git_cache_date <- function(dir_rel) {
  full <- file.path(VIGNETTE_DIR, dir_rel)
  if (!dir.exists(full)) return(NA_character_)
  out <- suppressWarnings(system2(
    "git",
    c("log", "--format=%ad", "--date=short", "--",
      file.path(full, "*.rds")),
    stdout = TRUE, stderr = FALSE
  ))
  out <- out[nzchar(out)]
  if (!length(out)) return(NA_character_)
  max(out)
}

# ---------------------------------------------------------------------- assemble

main <- function() {
  # Paths below are relative to the repository root (the compute scripts and the
  # site render both assume a fixed working directory, so this one does too).
  # Say so plainly rather than failing later inside file().
  if (!dir.exists(VIGNETTE_DIR)) {
    stop("Run this from the repository root: no ", VIGNETTE_DIR, "/ directory ",
         "found in ", getwd(), call. = FALSE)
  }
  manifest <- read_manifest()
  rows <- list()

  add <- function(vignette, date, basis) {
    version <- resolve_version(manifest, date)
    n_day   <- n_released_on(manifest, date)
    bracketed <- is_bracketed(manifest, version)
    rows[[length(rows) + 1L]] <<- data.frame(
      vignette             = vignette,
      date_run             = date,
      basis                = basis,
      irw_version          = version,
      # `bracketed` is specifically "this version's own release timestamps are
      # reconstructed"; `approximate` is the umbrella over every reason the
      # number might be wrong. They are kept apart because the component states
      # a different caveat for each, and conflating them attaches the
      # pre-July-2026 warning to August versions.
      bracketed            = bracketed,
      shards_reconstructed = has_reconstructed_shards(manifest, version),
      approximate          = bracketed || basis != "date_run" || n_day > 1L,
      n_versions_that_day  = n_day,
      stringsAsFactors     = FALSE
    )
  }

  for (v in names(CACHES)) {
    d <- if (!is.null(OVERRIDES[[v]])) OVERRIDES[[v]] else date_run_of(CACHES[[v]])
    basis <- if (!is.null(OVERRIDES[[v]])) "override" else "date_run"
    if (is.na(d)) {
      warning("No date_run found for ", v, "; skipping.", call. = FALSE)
      next
    }
    add(v, d, basis)
  }

  for (v in names(GIT_DATED)) {
    d <- if (!is.null(OVERRIDES[[v]])) OVERRIDES[[v]] else git_cache_date(GIT_DATED[[v]])
    basis <- if (!is.null(OVERRIDES[[v]])) "override" else "git"
    if (is.na(d)) {
      warning("No git date found for ", v, "; skipping.", call. = FALSE)
      next
    }
    add(v, d, basis)
  }

  for (v in LIVE) {
    rows[[length(rows) + 1L]] <- data.frame(
      vignette = v, date_run = "", basis = "live", irw_version = NA_integer_,
      bracketed = FALSE, shards_reconstructed = FALSE, approximate = FALSE,
      n_versions_that_day = NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$vignette), , drop = FALSE]   # determinism

  path <- file.path(VIGNETTE_DIR, "vignette_versions.tsv")
  utils::write.table(out, path, sep = "\t", quote = FALSE, row.names = FALSE,
                     na = "")
  message("Wrote ", path, " (", nrow(out), " vignettes; newest IRW version in ",
          "manifest: v", max(manifest$irw_version), ")")
  invisible(out)
}

if (sys.nframe() == 0L) main()
