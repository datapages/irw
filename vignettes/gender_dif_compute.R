# gender_dif_compute.R
#
# Omnibus DIF analysis by gender/sex across IRW datasets.
# For each dataset with cov_gender or cov_sex:
#   1. Binarize group using the two most common values
#   2. Fit 1PL (Rasch) and 2PL (lnorm prior on a1) multigroup models
#   3. Run Wald DIF test, uniform (item difficulty d only)
#   4. Record per-item stats and dataset-level summaries
#
# Prototype: first N_PROTO dichotomous datasets.
# Usage: Rscript vignettes/gender_dif_compute.R   (from project root)

suppressPackageStartupMessages({
  library(irw)
  library(mirt)
  library(dplyr)
})

set.seed(20260527)

N_PROTO   <- 60
MIN_GROUP <- 50
out_dir   <- "vignettes/gender_dif_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── 1. Candidate datasets ────────────────────────────────────────────────────
candidates <- union(
  irw_filter(var = "cov_gender"),
  irw_filter(var = "cov_sex")
)
message("Total candidates with gender/sex: ", length(candidates))
tables_to_run <- head(candidates, N_PROTO)

# ── 2. Per-dataset DIF ───────────────────────────────────────────────────────
fit_one <- function(tab) {
  message("\n=== ", tab, " ===")
  cache <- file.path(out_dir, paste0(tab, ".rds"))
  if (file.exists(cache)) { message("  cached"); return(readRDS(cache)) }

  # fetch with retry
  df <- NULL
  for (attempt in 1:3) {
    df <- tryCatch(irw_fetch(tab), error = function(e) {
      message("  fetch attempt ", attempt, " failed: ", e$message); NULL
    })
    if (!is.null(df)) break
    Sys.sleep(5 * attempt)
  }
  if (is.null(df)) return(NULL)

  # prefer cov_gender over cov_sex when both present
  gcol <- intersect(c("cov_gender", "cov_sex"), names(df))[1]
  if (is.na(gcol)) { message("  no gender/sex column"); return(NULL) }

  # binarize: two most common values; reference = most common
  grp_tbl <- sort(table(df[[gcol]]), decreasing = TRUE)
  if (length(grp_tbl) < 2) { message("  only 1 group level"); return(NULL) }
  top2 <- names(grp_tbl)[1:2]
  df   <- df[df[[gcol]] %in% top2, ]
  df$group_bin <- as.integer(df[[gcol]] == top2[2])   # 0 = ref, 1 = focal

  gsizes <- table(df$group_bin)
  if (any(gsizes < MIN_GROUP)) {
    message("  group too small (", paste(gsizes, collapse = "/"), ")"); return(NULL)
  }

  # wide response matrix aligned with group vector
  resp_wide <- irw_long2resp(df)
  if (nrow(resp_wide) < 2 * MIN_GROUP) {
    message("  too few rows after density filter (", nrow(resp_wide), ")"); return(NULL)
  }
  id_grp    <- df %>% select(id, group_bin) %>% distinct()
  group_vec <- id_grp$group_bin[match(resp_wide$id, id_grp$id)]

  # sample down for large datasets
  MAX_PERSONS <- 5000
  if (nrow(resp_wide) > MAX_PERSONS) {
    message("  sampling ", MAX_PERSONS, " / ", nrow(resp_wide), " respondents")
    samp      <- sample(nrow(resp_wide), MAX_PERSONS)
    resp_wide <- resp_wide[samp, ]
    group_vec <- group_vec[samp]
  }

  item_cols <- setdiff(names(resp_wide), "id")

  # cap items: DIF scheme="drop" fits one model per item, so 60+ items is very slow
  MAX_ITEMS <- 51
  if (length(item_cols) > MAX_ITEMS) {
    message("  sampling ", MAX_ITEMS, " / ", length(item_cols), " items")
    item_cols <- sample(item_cols, MAX_ITEMS)
  }

  resp_mat  <- as.matrix(resp_wide[, item_cols])

  # drop items with zero variance in either group
  keep_items <- sapply(item_cols, function(it) {
    vals <- resp_mat[, it]
    all(tapply(vals, group_vec, function(x) length(unique(na.omit(x)))) >= 2)
  })
  if (any(!keep_items)) {
    message("  dropping ", sum(!keep_items), " zero-variance item(s): ",
            paste(item_cols[!keep_items], collapse = ", "))
    resp_mat  <- resp_mat[, keep_items, drop = FALSE]
    item_cols <- item_cols[keep_items]
  }
  if (ncol(resp_mat) < 2) { message("  too few items after QC"); return(NULL) }

  ni      <- ncol(resp_mat)
  n_cats  <- apply(resp_mat, 2, function(x) length(unique(na.omit(x))))
  is_poly <- n_cats > 2

  # item types: Rasch/2PL for dichotomous; gpcm/graded for polytomous
  types_1pl <- ifelse(is_poly, "gpcm",   "Rasch")
  types_2pl <- ifelse(is_poly, "graded", "2PL")

  # which.par: "d" for dichotomous items; "d1","d2",... for polytomous
  max_k  <- max(n_cats)
  d_pars <- c(
    if (any(n_cats <= 2)) "d",
    if (any(n_cats >  2)) paste0("d", seq_len(max_k - 1))
  )

  # for 1PL with polytomous items: fix a1 = 1 in parameter table to get PCM
  # multipleGroup(pars = "values") returns the param table without estimating
  pcm_pars <- NULL
  if (any(is_poly)) {
    pcm_pars <- multipleGroup(resp_mat, 1, group = factor(group_vec),
                               itemtype = types_1pl, pars = "values")
    pcm_pars$value[pcm_pars$name == "a1" & pcm_pars$class == "gpcm"] <- 1
    pcm_pars$est[pcm_pars$name   == "a1" & pcm_pars$class == "gpcm"] <- FALSE
  }

  # no prior for 2PL here: slopes are constrained equal across groups (identified),
  # so estimation is stable; prior would switch mirt to logPost instead of X2/p
  model_2pl <- 1

  # baseline: all item params constrained equal across groups (no DIF) — identified
  # "slopes" needed for 1PL too: Rasch a1 isn't auto-constrained across groups
  # scheme="drop" + LR tests: free each item's difficulty one at a time
  inv_1pl <- c("slopes", "intercepts", "free_means", "free_variances")
  inv_2pl <- c("slopes", "intercepts", "free_means", "free_variances")

  run_dif <- function(itemtypes, model_spec, inv, label, pars = NULL) {
    mg <- tryCatch(
      multipleGroup(
        data      = resp_mat,
        model     = model_spec,
        group     = factor(group_vec),
        itemtype  = itemtypes,
        pars      = pars,
        invariance = inv,
        verbose   = FALSE,
        technical = list(NCYCLES = 5000)
      ),
      error = function(e) { message("  multipleGroup (", label, ") failed: ", e$message); NULL }
    )
    if (is.null(mg)) return(NULL)
    tryCatch(
      DIF(mg, which.par = d_pars, scheme = "drop", verbose = FALSE),
      error = function(e) { message("  DIF (", label, ") failed: ", e$message); NULL }
    )
  }

  res_1pl <- run_dif(types_1pl, model_spec = 1,        inv = inv_1pl, label = "1pl", pars = pcm_pars)
  res_2pl <- run_dif(types_2pl, model_spec = model_2pl, inv = inv_2pl, label = "2pl")

  out <- list(
    tab        = tab,
    group_col  = gcol,
    group_vals = top2,
    group_ns   = as.integer(gsizes),
    n_items    = ni,
    n_persons  = nrow(resp_mat),
    dif_1pl    = res_1pl,
    dif_2pl    = res_2pl
  )
  saveRDS(out, cache)
  out
}

# ── 3. Run ───────────────────────────────────────────────────────────────────
combined_path <- file.path(out_dir, "combined_results.rds")
results <- if (file.exists(combined_path)) readRDS(combined_path) else list()

for (tab in tables_to_run) {
  if (!tab %in% names(results)) {
    results[[tab]] <- fit_one(tab)
    saveRDS(results, combined_path)
  } else {
    message("\n=== ", tab, " === (in combined cache)")
  }
}

message("\nDone. Results in ", out_dir)
