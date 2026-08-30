## Fetch + prepare IRW tables as respondent x item numeric matrices.
suppressPackageStartupMessages({ library(irw); library(dplyr); library(tidyr) })

CACHE <- "analysis/plenitude_data/tables"
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

## Wide numeric matrix for one IRW table, cached as .rds.
## Complete cases only and zero-variance respondents dropped, matching their
## grab_measurements() preamble (na.omit + drop rows with var <= 1e-9).
get_matrix <- function(tbl) {
  f <- file.path(CACHE, paste0(tbl, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  d <- irw::irw_fetch(tbl)
  w <- d %>%
    select(id, item, resp) %>%
    distinct(id, item, .keep_all = TRUE) %>%
    pivot_wider(names_from = item, values_from = resp)
  m <- as.matrix(w[, setdiff(names(w), "id"), drop = FALSE])
  storage.mode(m) <- "numeric"
  rownames(m) <- as.character(w$id)
  m <- m[stats::complete.cases(m), , drop = FALSE]
  m <- m[apply(m, 1, var) > 1e-9, , drop = FALSE]
  m <- m[, apply(m, 2, var) > 1e-9, drop = FALSE]
  saveRDS(m, f)
  m
}

## Seeded respondent subsample to at most `n`.
subsample <- function(m, n = 2000L, seed = 1L) {
  if (nrow(m) <= n) return(m)
  set.seed(seed)
  m[sort(sample.int(nrow(m), n)), , drop = FALSE]
}
