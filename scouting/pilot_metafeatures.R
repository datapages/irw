## Task 3 - meta-feature pilot on IRW tables
## Ports evaluate.metafeatures() from Sotoudeh & DiMaggio's metafeature_RCA.R
## Design choices vs. their code are marked ## CHOICE:

S <- "/tmp/claude-1000/-home-ben-Dropbox-projects-irw-irw-site-vignettes/bffe2e22-3ff9-49bd-8dab-f1d88318f991/scratchpad/rca"
.libPaths(c(file.path(S, "rlib"), .libPaths()))
suppressMessages({
  library(irw); library(dplyr); library(stringr); library(tidyr); library(readr)
  library(DescTools); library(mfe); library(intrinsicDimension); library(psych)
})
set.seed(20260827)   ## CHOICE: their code injects an unseeded random `class` column for mfe

## ---- wide matrix from an IRW long table -------------------------------------
to_wide <- function(tb) {
  d <- irw_fetch(tb)
  w <- d |> select(id, item, resp) |> distinct(id, item, .keep_all = TRUE) |>
    pivot_wider(names_from = item, values_from = resp)
  ids <- w$id; m <- as.matrix(w[, -1, drop = FALSE])
  storage.mode(m) <- "numeric"; rownames(m) <- as.character(ids)
  m
}

## merged multi-construct matrix: inner-join sibling tables on id
to_wide_merged <- function(tbs) {
  ws <- lapply(tbs, function(tb) { m <- to_wide(tb); colnames(m) <- paste0(tb, "::", colnames(m)); m })
  ids <- Reduce(intersect, lapply(ws, rownames))
  if (length(ids) < 100) return(NULL)
  do.call(cbind, lapply(ws, function(m) m[ids, , drop = FALSE]))
}

## ---- cleaning ----------------------------------------------------------------
## CHOICE: complete cases only (RCA needs a full rectangular matrix); drop
## zero-variance rows (person-correlations undefined) and zero-variance columns
## (prcomp fails). Report how many were dropped.
clean_mat <- function(m) {
  m <- m[stats::complete.cases(m), , drop = FALSE]
  n0 <- nrow(m); p0 <- ncol(m)
  m <- m[, apply(m, 2, var) > 1e-9, drop = FALSE]
  m <- m[apply(m, 1, var) > 1e-9, , drop = FALSE]
  attr(m, "dropped_rows") <- n0 - nrow(m); attr(m, "dropped_cols") <- p0 - ncol(m)
  m
}

## ---- meta-features (their 14, verbatim formulas) -----------------------------
eval_mf <- function(df, num_vars = ncol(df)) {   ## CHOICE: num_vars = ncol, not their default 10
  df <- as.matrix(df)
  row_cors <- cor(t(df))
  mean_row_cor <- mean(row_cors, na.rm = TRUE)
  row_cor_kurtosis <- Kurt(row_cors)
  right_tail_cor_kurt <- Kurt(row_cors[row_cors > mean(row_cors, na.rm = TRUE)])
  overallKurt <- Kurt(as.numeric(df), na.rm = TRUE)
  overall_right_kurt <- Kurt(df > mean(df, na.rm = TRUE))   # verbatim: kurtosis of a LOGICAL
  overallSD <- sd(df, na.rm = TRUE)
  pc <- prcomp(df)$x
  var_col_first_PC <- var(pc[, 1]); skew_col_first_PC <- Skew(pc[, 1])
  skew_col_second_PC <- if (ncol(pc) >= 2) Skew(pc[, 2]) else NA_real_
  skewness <- Skew(df)
  dfn <- as.data.frame(df)
  mean_column_kurt <- mean(sapply(dfn, Kurt), na.rm = TRUE)
  mean_sd_ratio <- mean(sapply(dfn, function(x) sd(x, na.rm = TRUE)) / sd(df, na.rm = TRUE), na.rm = TRUE)
  dfn$class <- sample(1:4, nrow(dfn), replace = TRUE)
  nrOut <- statistical(class ~ ., dfn, features = "nrOutliers", by.class = FALSE)$nrOutliers
  idim <- pcaLocalDimEst(df, ver = "fan")$dim.est
  data.frame(
    OverallSD = overallSD, OverallKurt = overallKurt, OverallRightKurt = overall_right_kurt,
    VarColPC1 = var_col_first_PC, SkewColPC1 = skew_col_first_PC, SkewColPC2 = skew_col_second_PC,
    Skewness = skewness, Mean_Row_Corr = mean_row_cor, Row_Corr_Kurtosis = row_cor_kurtosis,
    CorrRightKurt = right_tail_cor_kurt, Mean_Col_Kurtosis = mean_column_kurt,
    Mean_SD_Ratio = mean_sd_ratio, PercentOut = (nrOut / num_vars) * 100,
    intrinsicDemnsionalityProp = idim / num_vars)
}

## supplementary: the brief asked for inter-ITEM correlation; their Mean_Row_Corr
## is person-by-person. Compute both Pearson and polychoric item-level analogues.
supp_mf <- function(df) {
  ii <- cor(df); iiv <- ii[upper.tri(ii)]
  poly <- tryCatch({
    p <- psych::polychoric(df, progress = FALSE)$rho; mean(p[upper.tri(p)])
  }, error = function(e) NA_real_)
  data.frame(Mean_InterItem_Pearson = mean(iiv), Mean_InterItem_Polychoric = poly,
             Mean_Row_Var = mean(apply(df, 1, var)), Mean_Col_Var = mean(apply(df, 2, var)))
}

run_one <- function(label, m, meta = list()) {
  m <- clean_mat(m)
  if (nrow(m) < 100 || ncol(m) < 4) return(NULL)
  t0 <- Sys.time()
  mf <- tryCatch(eval_mf(m), error = function(e) { message("  eval_mf fail: ", conditionMessage(e)); NULL })
  if (is.null(mf)) return(NULL)
  sp <- tryCatch(supp_mf(m), error = function(e) data.frame(Mean_InterItem_Pearson = NA, Mean_InterItem_Polychoric = NA, Mean_Row_Var = NA, Mean_Col_Var = NA))
  cbind(data.frame(label = label, kind = meta$kind %||% "single",
                   n_persons = nrow(m), n_items = ncol(m),
                   n_categories = meta$n_categories %||% NA,
                   affect_cog = meta$affect_cog %||% NA,
                   dropped_rows = attr(m, "dropped_rows"), dropped_cols = attr(m, "dropped_cols"),
                   secs = as.numeric(difftime(Sys.time(), t0, units = "secs"))),
        mf, sp)
}
`%||%` <- function(a, b) if (is.null(a)) b else a
