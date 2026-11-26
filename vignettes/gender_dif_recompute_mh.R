# gender_dif_recompute_mh.R
#
# One-off repair for datapages/irw#430: recompute the MH columns of the cached
# polytomous results with Mantel's (1963) ordinal test (gender_dif_mantel.R),
# keeping the cached logistic results, which were computed correctly.
#
# Each table is fetched again and preprocessed as in gender_dif_compute.R.
# The repair applies only when the table still has the cached item set; a table
# rebuilt since the vignette ran (different items) is listed in
# gender_dif_data/rerun_needed.txt for a full rerun of gender_dif_compute.R
# (DIF_ONLY=<that file>), since its logistic results are stale too.
#
# Tables the vignette sampled down to MAX_PERSONS respondents are sampled again
# with a seed fixed per table; the cached logistic results came from an
# earlier, unrecorded sample, so for those tables (n_persons == MAX_PERSONS)
# MH and logistic rest on different random subsets of the same table. Each
# repaired result records `mh_repaired` and `mh_resampled`.
#
# Usage: Rscript vignettes/gender_dif_recompute_mh.R   (from project root)

suppressPackageStartupMessages({
  library(irw)
  library(dplyr)
})
source("vignettes/gender_dif_mantel.R")

out_dir     <- "vignettes/gender_dif_data"
MAX_PERSONS <- 5000

files <- list.files(out_dir, pattern = "\\.rds$", full.names = TRUE)
rerun <- character(0)

for (f in files) {
  r <- readRDS(f)
  if (!isTRUE(r$is_poly) || identical(r$mh_method, "mantel1963")) next
  tab <- r$tab
  message("=== ", tab)
  df <- NULL
  for (attempt in 1:3) {
    df <- tryCatch(irw_fetch(tab), error = function(e) NULL)
    if (!is.null(df)) break
    Sys.sleep(5 * attempt)
  }
  if (is.null(df)) { message("  fetch failed; full rerun needed"); rerun <- c(rerun, tab); next }

  gcol <- r$group_col
  if (!gcol %in% names(df)) { message("  group column gone"); rerun <- c(rerun, tab); next }
  if (!all(r$group_vals %in% df[[gcol]])) {
    message("  group values changed (cached: ", paste(r$group_vals, collapse = ", "), "); full rerun needed")
    rerun <- c(rerun, tab); next
  }
  df <- df[df[[gcol]] %in% r$group_vals, ]
  df$group_bin <- as.integer(df[[gcol]] == r$group_vals[2])
  wide <- irw_long2resp(df)
  items <- r$items$item
  if (!all(items %in% names(wide))) {
    message("  item set changed since ", format(r$date_run), "; full rerun needed")
    rerun <- c(rerun, tab); next
  }
  id_grp <- df %>% select(id, group_bin) %>% distinct()
  g <- id_grp$group_bin[match(wide$id, id_grp$id)]
  resampled <- FALSE
  if (nrow(wide) > MAX_PERSONS) {
    set.seed(sum(utf8ToInt(tab)))
    s <- sample(nrow(wide), MAX_PERSONS)
    wide <- wide[s, ]; g <- g[s]; resampled <- TRUE
  } else if (nrow(wide) != r$n_persons) {
    message("  respondent count changed (", r$n_persons, " -> ", nrow(wide), "); full rerun needed")
    rerun <- c(rerun, tab); next
  }
  X <- as.matrix(wide[, items, drop = FALSE])
  mh <- tryCatch(mantel_dif(X, g, purify = TRUE), error = function(e) NULL)
  if (is.null(mh)) { message("  Mantel failed"); rerun <- c(rerun, tab); next }
  r$items$p_mh     <- as.numeric(mh$p.value)
  r$items$alpha_mh <- NA_real_
  r$items$smd_mh   <- as.numeric(mh$smd)
  r$items          <- r$items[, c("item", "p_mh", "alpha_mh", "delta_mh", "smd_mh",
                                  setdiff(names(r$items), c("item", "p_mh", "alpha_mh", "delta_mh", "smd_mh")))]
  r$mh_method      <- "mantel1963"
  r$mh_repaired    <- Sys.Date()
  r$mh_resampled   <- resampled
  saveRDS(r, f)
  message("  repaired: ", sum(mh$p.value < 0.05, na.rm = TRUE), "/", length(items), " flagged",
          if (resampled) " (resampled)" else "")
}

writeLines(rerun, file.path(out_dir, "rerun_needed.txt"))
message("\nDone. ", length(rerun), " table(s) need a full rerun: ", file.path(out_dir, "rerun_needed.txt"))
