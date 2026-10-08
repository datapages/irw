## plenitude_lib.R -----------------------------------------------------------
## Ported implementation of Sotoudeh & DiMaggio (2021), "Coping With Plenitude,"
## Sociological Methods & Research 52(4):1838-1882, doi:10.1177/00491241211031273.
##
## Source artifacts are vendored in analysis/vendor/ (retrieved 2026-08-27 from
## the authors' Google Drive folder; the GitHub repo is a README-only stub).
## SHA-256 pins are recorded in analysis/vendor/CHECKSUMS.
##
## Everything here follows their released code. Five defects in that code are
## handled explicitly and are documented at each site as CARRY-FORWARD n.

suppressPackageStartupMessages({
  library(DescTools); library(proxy); library(acepack); library(igraph)
  library(fpc); library(lavaan); library(intrinsicDimension); library(Rcpp)
  library(matrixStats)
})

VENDOR <- file.path(rprojroot::find_root(rprojroot::has_file("_quarto.yml")), "analysis", "vendor")

## --- Rcpp helpers (f_outer, relationalityC) --------------------------------
sourceCpp(file.path(VENDOR, "f_outer.cpp"))

## --- their helpers, verbatim -----------------------------------------------
f <- function(m) { m[lower.tri(m)] <- t(m)[lower.tri(m)]; m }

changeX <- function(string) { changemat <- outer(string, string, "-"); diag(changemat) <- NA; changemat }

gen_change_list <- function(df) lapply(seq_len(nrow(df)), function(i) changeX(df[i, ]))

recursive_gen_change_list <- function(df)
  lapply(seq_len(nrow(df)), function(i) f_outer(f_outer(as.numeric(df[i, ]))))

relationality <- function(Xi, Xj) {
  kx <- ncol(Xi)
  lambda <- ifelse(Xi * Xj >= 0, 1, -1)
  dist <- 1 - abs(abs(Xi) - abs(Xj))
  dist <- lambda * dist
  to_keep <- which(colSums(is.na(dist)) < kx)
  if (length(to_keep) > 0) dist <- dist[to_keep, to_keep]
  kx <- ncol(dist)
  sum(dist, na.rm = TRUE) / (kx * (kx - 1))
}

rca.dist <- function(data) {
  data <- as.matrix(data)
  change_x_list <- gen_change_list(data)
  n <- nrow(data)
  distmat <- matrix(nrow = n, ncol = n)
  for (i in 1:(n - 1)) for (j in (i + 1):n)
    distmat[i, j] <- relationality(change_x_list[[i]], change_x_list[[j]])
  f(distmat)
}

recursive.rca.dist <- function(data) {
  data <- as.matrix(data)
  change_x_list <- recursive_gen_change_list(data)
  change_x_list <- lapply(change_x_list, function(x) ifelse(x[] > 0, 1, x[]))
  change_x_list <- lapply(change_x_list, function(x) ifelse(x[] < 0, -1, x[]))
  n <- nrow(data)
  distmat <- matrix(nrow = n, ncol = n)
  for (i in 1:(n - 1)) for (j in (i + 1):n)
    distmat[i, j] <- relationalityC(change_x_list[[i]], change_x_list[[j]])
  f(distmat)
}

corr.dist <- function(dtf, zero.action = "ownclass") {
  zeros <- which(apply(dtf, 1, var) <= 1e-9)
  if (zero.action[1] == "drop" && length(zeros) > 0) dtf <- dtf[-zeros, ]
  rv <- abs(cor(t(dtf)))
  if ((zero.action[1] == "ownclass") && length(zeros) > 0) {
    rv[zeros, ] <- 0; rv[, zeros] <- 0; rv[zeros, zeros] <- 1
  }
  diag(rv) <- 0
  rv
}

ace.dist <- function(dtf, zero.action = "ownclass") {
  zeros <- which(apply(dtf, 1, var) <= 1e-9)
  if (zero.action[1] == "drop" && length(zeros) > 0) dtf <- dtf[-zeros, ]
  dtf <- as.matrix(dtf)
  n <- nrow(dtf)
  distmat <- matrix(nrow = n, ncol = n)
  for (i in 1:(n - 1)) for (j in (i + 1):n)
    distmat[i, j] <- ace(dtf[i, ], dtf[j, ])$rsq
  rv <- f(distmat)
  if ((zero.action[1] == "ownclass") && length(zeros) > 0) {
    rv[zeros, ] <- 0; rv[, zeros] <- 0; rv[zeros, zeros] <- 1
  }
  diag(rv) <- 0
  rv
}

filter.insignif <- function(corr, N.vars, pcutoff = 0.05) {
  corr <- abs(corr)
  if (any(diag(corr) != 0)) stop("Non-zero elements on the diagonal.")
  suppressWarnings(tvalues <- corr * sqrt((N.vars - 2) / (1 - corr^2)))
  tvalues[is.infinite(tvalues)] <- 9999
  cutoff <- abs(qt(pcutoff / 2, N.vars))
  corr[tvalues < cutoff] <- 0
  corr
}

rescale.for.RCA <- function(df) { if (any(df <= 0)) df <- df - min(df) + 1; df }

## --- CARRY-FORWARD: mfe::nrOutliers drop-in --------------------------------
## `mfe` is archived from CRAN. Its rule (mfe/R/statistical.R:353-360) uses
## stats::quantile type 7, not boxplot.stats hinges. This reproduces it exactly.
nrOutliers <- function(m) sum(apply(m, 2, function(x) {
  qs <- stats::quantile(x); iqr <- (qs[4] - qs[2]) * 1.5
  (qs[2] - iqr) > qs[1] | (qs[4] + iqr) < qs[5]
}))

## Cell-level analogue: proportion of individual observations that are Tukey
## outliers within their own column. This is the reading that reproduces the
## magnitudes of Table 9's PercentOut column (CARRY-FORWARD 2).
propOutlierCells <- function(m) mean(apply(m, 2, function(x) {
  qs <- stats::quantile(x); iqr <- (qs[4] - qs[2]) * 1.5
  x < (qs[2] - iqr) | x > (qs[4] + iqr)
}))

## --- meta-features ----------------------------------------------------------
## CARRY-FORWARD 1: num_vars is a REQUIRED argument here. In the released code
##   it defaults to 10 and select.method() never passes it, silently corrupting
##   PercentOut and intrinsicDemnsionalityProp on any matrix that is not exactly
##   10 items wide. We never route through select.method().
## CARRY-FORWARD 2: both PercentOut readings are returned.
##   PercentOut         = released formula, (count of outlier-bearing variables / p) * 100
##   PercentOut_cells   = proportion of outlying cells
## CARRY-FORWARD 3: OverallRightKurt is the released Kurt(df > mean(df)) -- the
##   kurtosis of a LOGICAL matrix. Their regressions were fit on that quantity,
##   so it is primary. OverallRightKurt_corrected is the evidently-intended
##   right_half_kurt (defined at their line 217, never called), for sensitivity.
evaluate_metafeatures <- function(df, num_vars, seed = 1L) {
  stopifnot(!missing(num_vars))
  df <- as.matrix(df)
  right_half_kurt <- function(x) Kurt(x[x > mean(x, na.rm = TRUE)])

  row_cors <- cor(t(df))
  mean_row_cor <- mean(row_cors, na.rm = TRUE)
  row_cor_kurtosis <- Kurt(row_cors)
  right_tail_cor_kurt <- Kurt(row_cors[row_cors > mean(row_cors, na.rm = TRUE)])

  overallKurt <- Kurt(as.numeric(df), na.rm = TRUE)
  overall_right_kurt <- Kurt(df > mean(df, na.rm = TRUE))                # as released
  overall_right_kurt_corr <- right_half_kurt(as.numeric(df))            # as intended
  overallSD <- sd(df, na.rm = TRUE)

  pc <- prcomp(df)
  var_col_first_PC <- var(pc$x[, 1])
  skew_col_first_PC <- Skew(pc$x[, 1])
  skew_col_second_PC <- Skew(pc$x[, 2])
  skewness <- Skew(df)

  df_new <- as.data.frame(df)
  mean_column_kurt <- mean(sapply(df_new, Kurt), na.rm = TRUE)
  mean_sd_ratio <- mean(sapply(df_new, function(x) sd(x, na.rm = TRUE)) / sd(df, na.rm = TRUE), na.rm = TRUE)

  set.seed(seed)                                       # their df_new$class is unseeded
  nrOut <- nrOutliers(df)
  intrinsicDim <- pcaLocalDimEst(df, ver = "fan")$dim.est

  data.frame(
    OverallSD = overallSD, OverallKurt = overallKurt,
    OverallRightKurt = overall_right_kurt,
    VarColPC1 = var_col_first_PC, SkewColPC1 = skew_col_first_PC,
    SkewColPC2 = skew_col_second_PC, Skewness = skewness,
    Mean_Row_Corr = mean_row_cor, Row_Corr_Kurtosis = row_cor_kurtosis,
    CorrRightKurt = right_tail_cor_kurt, Mean_Col_Kurtosis = mean_column_kurt,
    Mean_SD_Ratio = mean_sd_ratio,
    PercentOut = (nrOut / num_vars) * 100,
    intrinsicDemnsionalityProp = intrinsicDim / num_vars,
    ## extras, not fed to the models unless explicitly swapped in
    PercentOut_cells = propOutlierCells(df),
    OverallRightKurt_corrected = overall_right_kurt_corr,
    nrOut_raw = nrOut, intrinsicDim_raw = intrinsicDim
  )
}

## --- predicted accuracy -----------------------------------------------------
## CARRY-FORWARD 5 (new; not in the scouting audit): their make_prediction()
##   multiplies coefficients by meta-features POSITIONALLY. The column order of
##   evaluate.metafeatures() does not match the coefficient order of the fitted
##   lm objects -- 7 of 14 positions are swapped. We match by NAME (which is what
##   an lm's coefficients mean) for the primary analysis, and reproduce the
##   released positional behaviour as a sensitivity check.
MF_NAMES <- c("OverallSD","OverallKurt","OverallRightKurt","VarColPC1","SkewColPC1",
              "SkewColPC2","Skewness","Mean_Row_Corr","Row_Corr_Kurtosis",
              "CorrRightKurt","Mean_Col_Kurtosis","Mean_SD_Ratio","PercentOut",
              "intrinsicDemnsionalityProp")

predict_accuracy <- function(models, mf, align = c("name", "positional")) {
  align <- match.arg(align)
  x <- as.numeric(mf[MF_NAMES])
  names(x) <- MF_NAMES
  vapply(models, function(m) {
    cf <- coef(m)
    if (align == "name") sum(cf * c(1, x[names(cf)[-1]]))
    else sum(cf * c(1, x))          # their released ordering
  }, numeric(1))
}

## --- the ten methods --------------------------------------------------------
MEASURES <- c("eJaccard", "Podani", "Euclidean", "Cosine", "eDice",
              "Correlation", "ACE", "Original Relationality",
              "Recursive Relationality")
ENSEMBLE_MEMBERS <- c("Correlation", "Recursive Relationality", "eJaccard", "Podani", "ACE")

## CARRY-FORWARD 4: community detection is Louvain, hardcoded in their line 345.
##   Kept as-is, matching the paper. Recorded as fixed, not a free parameter.
metaRCA <- function(data, measure = "Recursive Relationality", seed = 1L) {
  data <- as.data.frame(data)
  data_scaled <- data.frame(sapply(data, as.numeric))
  data_scaled <- rescale.for.RCA(data_scaled)

  if (measure %in% c("eJaccard", "Podani", "Euclidean", "Cosine", "eDice")) {
    if (measure %in% c("eJaccard", "Cosine", "eDice"))                    # "oca rescale"
      data_scaled <- apply(data_scaled, 2, function(x) x - median(min(x):max(x)))
    results <- as.matrix(proxy::simil(data_scaled, method = measure))
    if (measure != "Cosine") results <- results - median(results, na.rm = TRUE)
  } else if (measure == "Correlation") {
    results <- corr.dist(data_scaled)
    results <- filter.insignif(results, ncol(data_scaled), pcutoff = 0.05)
  } else if (measure == "ACE") {
    results <- ace.dist(data_scaled)
    results[] <- ifelse(is.nan(results[]), 0, results[])
    results <- filter.insignif(results, ncol(data_scaled), pcutoff = 0.10)
  } else if (measure == "Original Relationality") {
    results <- rca.dist(data_scaled)
    results <- results - median(results, na.rm = TRUE)
  } else if (measure == "Recursive Relationality") {
    results <- recursive.rca.dist(data_scaled)
  } else stop("unknown measure: ", measure)

  results <- as.matrix(results)
  diag(results) <- 0

  distmat <- if (measure == "Recursive Relationality") results * results else abs(results)
  if (measure == "Original Relationality") distmat[distmat < 0.05] <- 0
  else if (measure == "Cosine")
    distmat <- ifelse(distmat > quantile(distmat, .75, na.rm = TRUE), distmat, 0)

  distmat[!is.finite(distmat)] <- 0
  net <- igraph::graph_from_adjacency_matrix(distmat, mode = "undirected", weighted = TRUE)
  set.seed(seed)
  cluster_out <- igraph::cluster_louvain(net, weights = E(net)$weight)

  list(measure = measure,
       membership = as.numeric(igraph::membership(cluster_out)),
       modularity = igraph::modularity(cluster_out))
}

## Their dyadic_vote_ensemble(), verbatim in substance.
## NOTE: their grab_measurements() default `measures` argument omits "ACE" while
## the default `ensemble` argument includes it, so outs[ensemble] yields a NULL
## member. We pass all nine measures, which is evidently what was intended.
dyadic_vote_ensemble <- function(vec_list, seed = 1L) {
  stopifnot(length(unique(lengths(vec_list))) == 1)
  mat_compare <- function(v1) { tb <- table(seq_along(v1), v1); tb %*% t(tb) }
  vote_comps <- Reduce("+", lapply(vec_list, mat_compare)) / length(vec_list)
  g <- igraph::graph_from_adjacency_matrix(vote_comps, mode = "undirected", weighted = TRUE)
  set.seed(seed)
  as.numeric(igraph::membership(igraph::cluster_louvain(g, weights = E(g)$weight)))
}
