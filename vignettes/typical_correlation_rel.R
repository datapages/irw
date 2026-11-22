# typical_correlation_rel.R
#
# Reliability-ceiling check for typical_correlation.qmd: for every published
# irw_cov matrix that reports reliabilities (rel rows), compare each observed
# |r| with sqrt(rel_i * rel_j), the largest correlation two scales with those
# reliabilities can show if their true scores correlate perfectly.
#
# Run from the repository root after typical_correlation_compute.R (it reuses
# the fetched tables in the irw cache). Output:
#   typical_correlation_data/rel_ceiling.rds

lib <- Sys.getenv("IRW_LIB")
if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressMessages({ library(irw); library(dplyr) })

out_dir <- "vignettes/typical_correlation_data"
cov_meta <- irw_metadata(source = "cov")
rel_tables <- sort(unique(cov_meta$table[cov_meta$has_rel %in% TRUE]))
message("tables with rel rows: ", length(rel_tables))

one <- function(tb) {
  d <- as.data.frame(suppressMessages(irw_fetch(tb, source = "cov")))
  if (!"group" %in% names(d)) d$group <- NA
  d$group <- as.character(d$group); d$var1 <- as.character(d$var1); d$var2 <- as.character(d$var2)
  g_of <- ifelse(is.na(d$group), ".", d$group)
  bind_rows(lapply(split(d, g_of), function(g) {
    rel <- g[g$stat == "rel", ]
    if (!nrow(rel)) return(NULL)
    relv <- setNames(rel$value, rel$var1)
    m <- g[g$stat %in% c("cor", "cov"), ]
    off <- m[m$var1 != m$var2 & !is.na(m$value), ]
    r <- off$value
    if (nrow(off) && m$stat[1] == "cov") {
      dg <- m[m$var1 == m$var2, ]; sdv <- setNames(sqrt(dg$value), dg$var1)
      r <- off$value / (sdv[off$var1] * sdv[off$var2])
    }
    ok <- off$var1 %in% names(relv) & off$var2 %in% names(relv)
    if (!any(ok)) return(NULL)
    data.frame(table = tb, group = g$group[1], var1 = off$var1[ok], var2 = off$var2[ok],
               r = r[ok], rel1 = unname(relv[off$var1[ok]]), rel2 = unname(relv[off$var2[ok]]))
  }))
}

res <- bind_rows(lapply(rel_tables, function(tb)
  tryCatch(one(tb), error = function(e) { message("  fail ", tb, ": ", conditionMessage(e)); NULL })))
res$ceiling <- sqrt(res$rel1 * res$rel2)
res$exceeds <- abs(res$r) > res$ceiling
saveRDS(list(cells = res, date_run = Sys.Date()), file.path(out_dir, "rel_ceiling.rds"))
message("done: ", nrow(res), " pairs, ", sum(res$exceeds, na.rm = TRUE), " exceed the ceiling")
