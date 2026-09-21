# Assembles esm_floor_results.rds from whatever per-fit caches exist right now,
# so the page always reflects completed work even mid-batch.
suppressPackageStartupMessages({library(dplyr)})
DATA_DIR <- "vignettes/esm_floor_data"; FIT_DIR <- file.path(DATA_DIR,"fits")
sl <- c("westhoff2023_pbat","westhoff2023_stopd","vollbracht_et_al_2026_ambulatory_assessment",
        "nas_rogoza_2024_study5_nas","nas_rogoza_2024_study5_ngs","nas_rogoza_2024_study5_nvs",
        "zhang_2020_trait_creativity_mood","opentsstvr_linnig_2025_vas")
prof <- lapply(list.files(FIT_DIR,"^floor_",full.names=TRUE), readRDS)
fsum <- bind_rows(lapply(prof, function(x) select(x,-items))) %>%
  mutate(format = ifelse(table %in% sl, "Slider / continuous", "Ordinal"))
idet <- bind_rows(lapply(prof, function(x) mutate(x$items[[1]], table=x$table, .before=1)))
bf <- setdiff(list.files(FIT_DIR,"\\.rds$",full.names=TRUE), list.files(FIT_DIR,"^floor_",full.names=TRUE))
sb <- if (length(bf)) bind_rows(lapply(bf, readRDS)) else NULL
if (!is.null(sb) && !"converged" %in% names(sb)) sb$converged <- NA
saveRDS(list(floor_summary=fsum, item_detail=idet, stage_b=sb,
  candidate_tables=fsum$table, n_all_candidates=26L, esm_slider=sl,
  esm_ordinal=setdiff(fsum$table,sl), pilot=TRUE, date_run=Sys.Date(),
  session=capture.output(sessionInfo())), file.path(DATA_DIR,"esm_floor_results.rds"))
cat("cache rebuilt:", nrow(fsum), "tables,", if(is.null(sb)) 0 else nrow(sb), "fits\n")
