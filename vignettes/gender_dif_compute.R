# gender_dif_compute.R
#
# Omnibus DIF analysis by gender/sex across IRW datasets.
# For each dataset: fetch, preprocess, run MH and logistic DIF, write result.
# Parallelised over table names via mclapply; each worker is self-contained.
# Re-running skips already-cached results.
#
# Usage: Rscript vignettes/gender_dif_compute.R   (from project root)

suppressPackageStartupMessages({
  library(irw)
  library(difR)
  library(VGAM)
  library(parallel)
  library(dplyr)
})

set.seed(20260527)

N_DATASETS        <- 250
N_CORES           <- 5L
MIN_GROUP         <- 50
MIN_ITEMS         <- 6
MAX_PERSONS       <- 5000
MAX_ITEMS         <- 51
MAX_ITEMS_POLYLOG <- 20   # skip difPolyLogistic above this item count
out_dir           <- "vignettes/gender_dif_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

EXCLUDE <- c(
  "evpromisi_stone_2021_ddeddep",
  "twod_rotation_mather2023"
)

# ── 1. Candidate datasets (randomised) ───────────────────────────────────────
candidates <- setdiff(
  union(irw_filter(var = "cov_gender"), irw_filter(var = "cov_sex")),
  EXCLUDE
)
candidates <- sample(candidates)

already_done <- function() {
  sub("\\.rds$", "", list.files(out_dir, pattern = "\\.rds$"))
}

done_before <- already_done()
to_run      <- head(setdiff(candidates, done_before), (N_DATASETS - length(done_before)) * 3L)
message(length(done_before), " cached; running up to ", length(to_run),
        " candidates on ", N_CORES, " cores")

# ── 2. DIF helper ─────────────────────────────────────────────────────────────
run_dif <- function(resp_mat, group_vec, is_poly) {
  resp_df <- as.data.frame(resp_mat)
  ni      <- ncol(resp_mat)
  items   <- colnames(resp_mat)

  # MH family with purification
  mh <- tryCatch(
    if (is_poly) difGMH(resp_df, group = group_vec, focal.names = 1, purify = TRUE)
    else         difMH(resp_mat, group = group_vec, focal.name  = 1, purify = TRUE),
    error = function(e) { message("  MH failed: ", e$message); NULL }
  )

  # Logistic family — skip difPolyLogistic for large polytomous datasets
  use_poly_log <- is_poly && ni <= MAX_ITEMS_POLYLOG
  log_fn       <- if (use_poly_log) difPolyLogistic
                  else if (!is_poly) difLogistic
                  else NULL
  logistic_run <- !is.null(log_fn)

  if (!logistic_run)
    message("  logistic skipped (poly items > ", MAX_ITEMS_POLYLOG, ")")

  log_u <- if (logistic_run) tryCatch(
    log_fn(Data = resp_df, group = group_vec, focal.name = 1, type = "udif",  purify = TRUE),
    error = function(e) { message("  logistic (udif) failed: ", e$message); NULL }
  )

  log_nu <- if (logistic_run) tryCatch(
    log_fn(Data = resp_df, group = group_vec, focal.name = 1, type = "nudif", purify = TRUE),
    error = function(e) { message("  logistic (nudif) failed: ", e$message); NULL }
  )

  if (is.null(mh) && is.null(log_u) && is.null(log_nu)) return(NULL)

  safe_vec <- function(x) {
    if (is.null(x)) rep(NA_real_, ni) else as.numeric(x)
  }

  p_mh     <- safe_vec(mh$p.value)
  alpha_mh <- safe_vec(mh$alphaMH)
  delta_mh <- if (!is_poly && !is.null(mh)) -2.35 * log(alpha_mh)
              else rep(NA_real_, ni)

  data.frame(
    item          = items,
    p_mh          = p_mh,
    alpha_mh      = alpha_mh,
    delta_mh      = delta_mh,
    p_unif        = safe_vec(log_u$p.value),
    p_nonunif     = safe_vec(log_nu$p.value),
    delta_r2_unif = safe_vec(log_u$deltaR2),
    logistic_run  = logistic_run,
    stringsAsFactors = FALSE
  )
}

# ── 3. Process one dataset (fetch → preprocess → DIF → write) ────────────────
process_one <- function(tab) {
  result_cache <- file.path(out_dir, paste0(tab, ".rds"))
  if (file.exists(result_cache)) {
    message("=== ", tab, " === (cached)")
    return(TRUE)
  }

  message("\n=== ", tab, " ===")

  df <- NULL
  for (attempt in 1:3) {
    df <- tryCatch(irw_fetch(tab),
                   error = function(e) {
                     message("  fetch attempt ", attempt, " failed: ", e$message); NULL
                   })
    if (!is.null(df)) break
    Sys.sleep(5 * attempt)
  }
  if (is.null(df)) return(FALSE)

  gcol <- intersect(c("cov_gender", "cov_sex"), names(df))[1]
  if (is.na(gcol)) { message("  no gender/sex column"); return(FALSE) }

  grp_tbl <- sort(table(df[[gcol]]), decreasing = TRUE)
  if (length(grp_tbl) < 2) { message("  only 1 group level"); return(FALSE) }
  top2 <- names(grp_tbl)[1:2]
  df   <- df[df[[gcol]] %in% top2, ]
  df$group_bin <- as.integer(df[[gcol]] == top2[2])

  resp_wide <- tryCatch(
    irw_long2resp(df),
    error = function(e) { message("  long2resp failed: ", e$message); NULL }
  )
  if (is.null(resp_wide) || nrow(resp_wide) < 2 * MIN_GROUP) {
    message("  too few rows after pivot (",
            if (!is.null(resp_wide)) nrow(resp_wide) else "NULL", ")")
    return(FALSE)
  }

  id_grp    <- df %>% select(id, group_bin) %>% distinct()
  group_vec <- id_grp$group_bin[match(resp_wide$id, id_grp$id)]

  gsizes <- table(group_vec)
  if (any(gsizes < MIN_GROUP)) {
    message("  group too small (", paste(as.integer(gsizes), collapse = "/"), ")")
    return(FALSE)
  }

  if (nrow(resp_wide) > MAX_PERSONS) {
    message("  sampling ", MAX_PERSONS, " / ", nrow(resp_wide), " respondents")
    samp      <- sample(nrow(resp_wide), MAX_PERSONS)
    resp_wide <- resp_wide[samp, ]
    group_vec <- group_vec[samp]
    gsizes    <- table(group_vec)
  }

  item_cols <- setdiff(names(resp_wide), "id")
  if (length(item_cols) > MAX_ITEMS) {
    message("  sampling ", MAX_ITEMS, " / ", length(item_cols), " items")
    item_cols <- sample(item_cols, MAX_ITEMS)
  }
  resp_mat <- as.matrix(resp_wide[, item_cols, drop = FALSE])

  keep_items <- sapply(item_cols, function(it) {
    all(tapply(resp_mat[, it], group_vec,
               function(x) length(unique(na.omit(x)))) >= 2)
  })
  if (any(!keep_items)) {
    message("  dropping ", sum(!keep_items), " zero-variance item(s)")
    resp_mat  <- resp_mat[, keep_items, drop = FALSE]
    item_cols <- item_cols[keep_items]
  }
  if (ncol(resp_mat) < MIN_ITEMS) {
    message("  too few items (", ncol(resp_mat), ")")
    return(FALSE)
  }

  ni      <- ncol(resp_mat)
  N       <- nrow(resp_mat)
  n_cats  <- apply(resp_mat, 2, function(x) length(unique(na.omit(x))))
  is_poly <- any(n_cats > 2)
  message("  N=", N, "  items=", ni, "  poly=", is_poly,
          "  groups=", paste(as.integer(gsizes), collapse = "/"))

  item_res <- run_dif(resp_mat, group_vec, is_poly)
  if (is.null(item_res)) return(FALSE)

  saveRDS(list(
    tab        = tab,
    group_col  = gcol,
    group_vals = top2,
    group_ns   = as.integer(gsizes),
    n_items    = ni,
    n_persons  = N,
    is_poly    = is_poly,
    items      = item_res
  ), result_cache)

  TRUE
}

# ── 4. Parallel run ───────────────────────────────────────────────────────────
mclapply(to_run, process_one,
         mc.cores       = N_CORES,
         mc.preschedule = FALSE)

message("\nDone. ", length(already_done()), " datasets in ", out_dir)
