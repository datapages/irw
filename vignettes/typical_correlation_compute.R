# typical_correlation_compute.R
#
# Compute stage for the "What does a typical correlation look like?" vignette
# (typical_correlation.qmd; proposal datapages/irw#451). PILOT version.
#
# Two sides:
#   published  every irw_cov table: off-diagonal cells of every published
#              matrix (covariances converted to correlations).
#   core       a seeded random sample of core tables: item-by-item matrices
#              computed from raw responses (tetrachoric for binary items,
#              polychoric for 3-8 categories (psych's limit), Pearson otherwise; Pearson
#              is always computed as well, as a check).
#
# Run from the repository root:
#   REDIVIS_API_TOKEN=... Rscript vignettes/typical_correlation_compute.R [n_core]
# It needs an irw build with source = "cov" (the development version; CRAN
# 1.4.0 does not have it). Set IRW_LIB to a library holding that build.
#
# Per-table results are cached under typical_correlation_data/{pub,core}/ so a
# crash resumes. Output: typical_correlation_data/typical_correlation_results.rds

lib <- Sys.getenv("IRW_LIB")
if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressMessages({ library(irw); library(dplyr) })
options(irw.cache = TRUE)

args   <- commandArgs(trailingOnly = TRUE)
n_core <- if (length(args)) as.integer(args[1]) else 150
out_dir <- "vignettes/typical_correlation_data"
dir.create(file.path(out_dir, "pub"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "core"), recursive = TRUE, showWarnings = FALSE)

seed_of <- function(s) sum(utf8ToInt(s) * seq_along(utf8ToInt(s))) %% .Machine$integer.max

# ---------------------------------------------------------------- published --
cov_meta <- irw_metadata(source = "cov")
pub_tables <- sort(unique(cov_meta$table))

pub_one <- function(tb) {
  f <- file.path(out_dir, "pub", paste0(tb, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  d <- suppressMessages(irw_fetch(tb, source = "cov"))
  d <- as.data.frame(d)
  if (!"group" %in% names(d)) d$group <- NA_character_
  d <- d[d$stat %in% c("cor", "cov"), ]
  out <- lapply(split(d, ifelse(is.na(d$group), ".", d$group)), function(g) {
    stat <- g$stat[1]
    off  <- g[g$var1 != g$var2 & !is.na(g$value), ]
    if (!nrow(off)) return(NULL)
    r <- off$value
    if (stat == "cov") {
      dg <- g[g$var1 == g$var2, ]
      sdv <- setNames(sqrt(dg$value), dg$var1)
      r <- off$value / (sdv[off$var1] * sdv[off$var2])
    }
    data.frame(table = tb, group = if (is.na(g$group[1])) NA else g$group[1],
               stat = stat, var1 = off$var1, var2 = off$var2, r = unname(r),
               n = off$n)
  })
  res <- do.call(rbind, out)
  saveRDS(res, f)
  res
}

message("published: ", length(pub_tables), " tables")
pub_cells <- bind_rows(lapply(pub_tables, function(tb) {
  tryCatch({ x <- pub_one(tb); if (!is.null(x)) for (v in c("group", "var1", "var2")) x[[v]] <- as.character(x[[v]]); x }, error = function(e) { message("  pub fail ", tb, ": ", conditionMessage(e)); NULL })
}))

# overlap clusters: tables that share a sample. Keep one table per cluster
# (the one with the most variables), so a sample is counted once.
ov <- cov_meta[!is.na(cov_meta$overlaps) & cov_meta$overlaps != "", c("table", "overlaps")]
edges <- do.call(rbind, lapply(seq_len(nrow(ov)), function(i)
  data.frame(a = ov$table[i], b = trimws(strsplit(ov$overlaps[i], ";")[[1]]))))
parent <- setNames(pub_tables, pub_tables)
find <- function(x) { while (parent[[x]] != x) x <- parent[[x]]; x }
if (!is.null(edges)) for (i in seq_len(nrow(edges))) {
  a <- edges$a[i]; b <- edges$b[i]
  if (!b %in% pub_tables) next
  ra <- find(a); rb <- find(b); if (ra != rb) parent[[rb]] <- ra
}
clusters <- vapply(pub_tables, find, "")
nv <- tapply(cov_meta$n_vars, cov_meta$table, max)
keep_pub <- unname(tapply(pub_tables, clusters, function(tt) tt[which.max(nv[tt])]))
pub_cells$overlap_dropped <- !pub_cells$table %in% keep_pub

# ---------------------------------------------------------------------- core --
meta <- irw_metadata()
tags <- tryCatch(irw_tags(), error = function(e) NULL)
cand <- meta %>%
  filter(n_items >= 3, n_items <= 300, n_responses <= 2e6, n_participants >= 100,
         density >= 0.5, is.na(longitudinal) | !longitudinal)
set.seed(20261121)
core_tables <- sort(sample(cand$table, min(n_core, nrow(cand))))
message("core: ", length(core_tables), " of ", nrow(cand), " eligible tables")

MAX_PEOPLE <- 3000
MAX_ITEMS  <- 60

core_one <- function(tb) {
  f <- file.path(out_dir, "core", paste0(tb, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  res <- tryCatch({
    d <- suppressMessages(irw_fetch(tb))
    d <- as.data.frame(d)[, c("id", "item", "resp", intersect(c("wave", "date", "rater"), names(d)))]
    d <- d[!is.na(d$resp), ]
    set.seed(seed_of(tb))
    # one occasion only: repeated measures make id-item duplicates
    if (anyDuplicated(d[, c("id", "item")])) {
      if ("wave" %in% names(d)) d <- d[d$wave == min(d$wave, na.rm = TRUE), ]
      if (anyDuplicated(d[, c("id", "item")])) stop("repeated measures")
    }
    items <- unique(d$item)
    if (length(items) > MAX_ITEMS) d <- d[d$item %in% sample(items, MAX_ITEMS), ]
    ids <- unique(d$id)
    if (length(ids) > MAX_PEOPLE) d <- d[d$id %in% sample(ids, MAX_PEOPLE), ]
    ncat <- tapply(d$resp, d$item, function(x) length(unique(x)))
    integerish <- all(abs(d$resp - round(d$resp)) < 1e-8)
    method <- if (all(ncat <= 2)) "tetrachoric" else if (integerish && max(ncat) <= 8) "polychoric" else "pearson"
    # psych counts categories from the value range, so codes like 0/5/10/15 or
    # 0/100 trip its 8-category limit. Recode each item to 1..k (order kept, so
    # the polychoric estimate is unchanged); Pearson below uses the raw codes.
    d_ord <- d
    d_ord$resp <- ave(d_ord$resp, d_ord$item, FUN = function(x) match(x, sort(unique(x))))
    m_main <- if (method == "pearson") NULL else
      tryCatch(suppressWarnings(irw_cov_compute(d_ord, method = method)),
               error = function(e) { message("    ", method, " failed: ", conditionMessage(e)); NULL })
    # psych's continuity correction for empty cells errors on some sparse
    # tables ("Something is wrong in polycor -- Try setting correct=0"); retry
    # without it, and say so in the method label.
    if (is.null(m_main) && method != "pearson") {
      fn <- if (method == "tetrachoric") function(x) psych::tetrachoric(x, correct = 0)$rho
            else function(x) psych::polychoric(x, correct = 0)$rho
      m_main <- tryCatch(suppressWarnings(suppressMessages(irw_cov_compute(d_ord, method = fn))),
                         error = function(e) { message("    retry failed: ", conditionMessage(e)); NULL })
      if (!is.null(m_main)) method <- paste0(method, " (correct = 0)")
    }
    m_pear <- suppressWarnings(irw_cov_compute(d, method = "pearson"))
    if (is.null(m_main)) { m_main <- m_pear; if (method != "pearson") method <- "pearson (fallback)" }
    off <- function(m) m[m$var1 != m$var2, ]
    a <- off(m_main); b <- off(m_pear)
    b <- b[match(paste(a$var1, a$var2), paste(b$var1, b$var2)), ]
    list(table = tb, method = method, n_items = length(unique(d$item)), n_people = length(unique(d$id)),
         cells = data.frame(table = tb, var1 = as.character(a$var1), var2 = as.character(a$var2), r = a$value,
                            r_pearson = b$value, n = a$n))
  }, error = function(e) list(table = tb, error = conditionMessage(e)))
  saveRDS(res, f)
  res
}

core_res <- lapply(seq_along(core_tables), function(i) {
  tb <- core_tables[i]
  message(sprintf("  [%d/%d] %s", i, length(core_tables), tb))
  core_one(tb)
})
core_fail  <- bind_rows(lapply(core_res, function(x) if (!is.null(x$error)) data.frame(table = x$table, error = x$error)))
core_ok    <- Filter(function(x) is.null(x$error), core_res)
core_info  <- bind_rows(lapply(core_ok, function(x) data.frame(table = x$table, method = x$method,
                                                               n_items_used = x$n_items, n_people_used = x$n_people)))
core_cells <- bind_rows(lapply(core_ok, `[[`, "cells"))

saveRDS(list(
  pub_cells = pub_cells, cov_meta = cov_meta, keep_pub = keep_pub,
  core_cells = core_cells, core_info = core_info, core_fail = core_fail,
  core_meta = meta[meta$table %in% core_tables, ], core_tags = tags,
  n_eligible = nrow(cand), n_core_sampled = length(core_tables),
  max_people = MAX_PEOPLE, max_items = MAX_ITEMS,
  irw_version = tryCatch(irw_version(), error = function(e) NA),
  date_run = Sys.Date()
), file.path(out_dir, "typical_correlation_results.rds"))
message("done")
