# typical_correlation_matched.R
#
# Matched instrument for typical_correlation.qmd: the Rosenberg Self-Esteem
# Scale appears on both sides. irw_cov holds gnambs_2018_rses10 (item-level
# RSES matrices from the Gnambs, Scharl & Schroeders 2018 meta-analysis; some
# of its matrices are reconstructed from loadings, see the table's notes) and
# core holds rosenberg_selfesteem (raw responses). Both are 10-item matrices of
# the same items, so the item-level r can be compared directly.
#
# Run from the repository root. Output: typical_correlation_data/matched_rses.rds

lib <- Sys.getenv("IRW_LIB")
if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressMessages({ library(irw); library(dplyr) })
out_dir <- "vignettes/typical_correlation_data"

pub <- as.data.frame(suppressMessages(irw_fetch("gnambs_2018_rses10", source = "cov")))
if (!"group" %in% names(pub)) pub$group <- NA
pub <- pub[pub$stat == "cor" & pub$var1 != pub$var2 & !is.na(pub$value), ]
pub$group <- as.character(pub$group)

core <- as.data.frame(suppressMessages(irw_fetch("rosenberg_selfesteem")))
set.seed(1)
ids <- unique(core$id)
core <- core[core$id %in% sample(ids, min(5000, length(ids))), c("id", "item", "resp")]
core <- core[!is.na(core$resp), ]
m <- suppressWarnings(irw_cov_compute(core, method = "polychoric"))
m <- m[m$var1 != m$var2, ]
mp <- suppressWarnings(irw_cov_compute(core, method = "pearson"))
m$r_pearson <- mp$value[match(paste(m$var1, m$var2), paste(mp$var1, mp$var2))]

meta <- irw_metadata(source = "cov")
meta <- meta[meta$table == "gnambs_2018_rses10", c("group", "n_obs", "overlaps", "country", "notes")]
saveRDS(list(pub = pub, pub_meta = meta, core = m, core_items = sort(unique(core$item)),
             pub_items = sort(unique(c(pub$var1, pub$var2))),
             n_core_people = length(unique(core$id)), date_run = Sys.Date()),
        file.path(out_dir, "matched_rses.rds"))
message("pub matrices: ", length(unique(pub$group)), "; core pairs: ", nrow(m))
