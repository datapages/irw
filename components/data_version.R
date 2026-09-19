# components/data_version.R
#
# Emits the "IRW version" line in every vignette's Reproducibility section, so
# the caveat wording lives in one place rather than being hand-written into 16
# vignettes.
#
# Usage from a vignette (see components/_data-version.qmd for the include that
# wraps this):
#
#   ```{r}
#   #| echo: false
#   #| output: asis
#   source("../components/data_version.R")
#   irw_data_version()
#   ```
#
# irw_data_version() auto-detects the current .qmd via knitr::current_input()
# and looks the stem up in vignettes/vignette_versions.tsv, which is written by
# vignettes/vignette_versions_compute.R. A stem that is not listed emits
# nothing, so adding a vignette does not break the build before its version has
# been resolved.
#
# Two cases, deliberately worded differently:
#
#   cached -- the page reads a frozen .rds. The version is INFERRED from the
#             compute date, so it is stated as approximate.
#   live   -- the page fetches from Redivis at render time. The version is
#             whatever is newest when the site is built, which is a fact rather
#             than an inference, so no caveat is attached.

MANIFEST_URL <- paste0(
  "https://raw.githubusercontent.com/ben-domingue/irw/main/",
  "metadata/version_manifest.tsv"
)

VERSIONS_TSV <- "vignette_versions.tsv"   # relative to vignettes/, the render wd

.irw_versions_table <- function() {
  if (!file.exists(VERSIONS_TSV)) return(NULL)
  utils::read.delim(VERSIONS_TSV, stringsAsFactors = FALSE,
                    colClasses = "character")
}

# The newest IRW version, for pages that fetch at render time.
#
# Falls back to the largest version in the baked TSV if the fetch fails, so an
# offline or rate-limited build degrades to a slightly stale number rather than
# failing. Returns NULL only if both sources are unavailable, in which case the
# caller emits nothing.
.irw_live_version <- function(versions) {
  live <- tryCatch({
    con <- url(MANIFEST_URL)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    m <- utils::read.delim(con, stringsAsFactors = FALSE, colClasses = "character")
    max(as.integer(m$irw_version))
  }, error = function(e) NULL, warning = function(w) NULL)

  if (!is.null(live) && is.finite(live)) return(list(version = live, stale = FALSE))

  if (!is.null(versions)) {
    baked <- suppressWarnings(max(as.integer(versions$irw_version), na.rm = TRUE))
    if (is.finite(baked)) return(list(version = baked, stale = TRUE))
  }
  NULL
}

irw_data_version <- function(stem = NULL) {
  if (is.null(stem)) {
    qmd <- tryCatch(knitr::current_input(), error = function(e) NULL)
    if (is.null(qmd)) return(invisible(NULL))
    stem <- tools::file_path_sans_ext(basename(qmd))
  }

  versions <- .irw_versions_table()
  if (is.null(versions)) return(invisible(NULL))

  row <- versions[versions$vignette == stem, , drop = FALSE]
  if (!nrow(row)) return(invisible(NULL))
  row <- row[1L, ]

  if (identical(row$basis, "live")) {
    got <- .irw_live_version(versions)
    if (is.null(got)) return(invisible(NULL))
    cat("\nThis page fetches IRW data when the site is built, so it reflects ",
        "**IRW v", got$version, "**",
        if (got$stale) {
          # Not the newest IRW version -- the newest this site has resolved
          # anything against. Say which, rather than implying it is current.
          paste0(" or later (the live manifest could not be read at build ",
                 "time, so this is the newest version any page on this site ",
                 "has been resolved against, not necessarily the current one)")
        } else {
          ", the newest version at render time"
        },
        ". See `irw_version()`.\n\n", sep = "")
    return(invisible(row))
  }

  if (!nzchar(row$irw_version)) return(invisible(NULL))
  pretty_date <- format(as.Date(row$date_run), "%B %e, %Y")
  pretty_date <- gsub("  ", " ", pretty_date)

  cat("\nThese results were computed against approximately **IRW v",
      row$irw_version, "** (the corpus as of ", pretty_date, ").\n\n",
      sep = "")

  # The caveats, stated once each and only when they apply. Both are real and
  # neither is implied by the other: the first is that we are inferring a
  # version from a date at all, the second is that the date itself may be wrong.
  notes <- character(0)
  notes <- c(notes, paste0(
    "The version was inferred from when the results were computed, not ",
    "recorded during the run, so it may be off by a version or more."))

  if (identical(row$basis, "git")) {
    notes <- c(notes, paste0(
      "The compute date is itself an upper bound, taken from when the cached ",
      "results were committed."))
  }
  if (identical(toupper(row$bracketed), "TRUE")) {
    notes <- c(notes, paste0(
      "IRW release dates before 21 July 2026 are reconstructed rather than ",
      "recorded, which widens the uncertainty further."))
  }
  if (identical(toupper(row$shards_reconstructed), "TRUE")) {
    notes <- c(notes, paste0(
      "This version pins one or more IRW shards to a release whose own date ",
      "Redivis lost in a platform migration, so which release of those shards ",
      "was live that day is reconstructed rather than recorded."))
  }
  n_day <- suppressWarnings(as.integer(row$n_versions_that_day))
  if (!is.na(n_day) && n_day > 1L) {
    notes <- c(notes, paste0(
      n_day, " IRW versions were released that day, so the exact one is ",
      "ambiguous."))
  }

  cat("::: {.callout-note collapse=\"true\"}\n",
      "## How this version was determined\n\n",
      paste(notes, collapse = " "),
      " To pin data to an exact version, use `irw_use_version()`.\n",
      ":::\n\n", sep = "")

  invisible(row)
}
