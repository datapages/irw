# components/source_links.R
#
# Emits the "Source code for this page" line used in every vignette's
# Reproducibility section, so the GitHub URL pattern lives in exactly one
# place rather than being hand-written into 18 vignettes.
#
# Usage from a vignette (see components/_source-links.qmd for the include
# that wraps this):
#
#   ```{r}
#   #| echo: false
#   #| output: asis
#   source("../components/source_links.R")
#   irw_source_links()
#   ```
#
# irw_source_links() auto-detects the current .qmd via knitr::current_input()
# and picks up companion scripts by naming convention:
#   - vignettes/<stem>*.R          (e.g. <stem>_compute.R, <stem>_helpers.R)
#   - vignettes/<stem>_data/*.R    (scripts parked next to the cache files)
# Anything outside those conventions is passed explicitly via `extra`, and
# false positives are dropped via `exclude`.

IRW_REPO   <- "datapages/irw"
IRW_BRANCH <- "main"
IRW_VIGNETTE_DIR <- "vignettes"

irw_source_url <- function(path_in_repo) {
  sprintf(
    "https://github.com/%s/blob/%s/%s",
    IRW_REPO, IRW_BRANCH, path_in_repo
  )
}

irw_source_links <- function(extra = character(), exclude = character(),
                             stem = NULL, label = "Source code for this page") {
  qmd <- tryCatch(knitr::current_input(), error = function(e) NULL)
  if (is.null(stem)) {
    if (is.null(qmd)) return(invisible(NULL))
    stem <- tools::file_path_sans_ext(basename(qmd))
  }
  qmd_file <- paste0(stem, ".qmd")

  # Companion scripts by convention. Rendering happens with the working
  # directory set to vignettes/, so these globs are relative to that.
  by_stem  <- Sys.glob(paste0(stem, "*.R"))
  data_dir <- Sys.glob(file.path(paste0(stem, "*_data"), "*.R"))

  files <- unique(c(qmd_file, sort(by_stem), sort(data_dir), extra))
  files <- setdiff(files, exclude)
  # Keep only what actually exists, so a renamed script fails loudly in review
  # rather than shipping a 404 link to readers.
  files <- files[file.exists(files)]
  if (!length(files)) return(invisible(NULL))

  links <- vapply(files, function(f) {
    sprintf("[`%s`](%s)", basename(f), irw_source_url(file.path(IRW_VIGNETTE_DIR, f)))
  }, character(1))

  cat("\n**", label, ":** ", paste(links, collapse = " · "), "\n\n", sep = "")
  invisible(files)
}
