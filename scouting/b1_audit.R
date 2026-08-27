## Task B1 — static code audit, minimal reproducible demonstrations
## Runs the AS-RELEASED functions from metafeature_RCA.R (sha256 1677519764...)
S <- "/tmp/claude-1000/-home-ben-Dropbox-projects-irw-irw-site-vignettes/bffe2e22-3ff9-49bd-8dab-f1d88318f991/scratchpad/rca"
.libPaths(c(file.path(S, "rlib"), .libPaths()))
suppressMessages({ library(DescTools); library(mfe); library(intrinsicDimension) })
set.seed(42)

## --- the released function, lifted verbatim from lines 213-270 ----------------
released.evaluate.metafeatures <- function(df, num_vars = 10){
  df <- as.matrix(df)
  right_half_kurt = function(x){ return(Kurt(x[x > mean(x, na.rm = T)])) }   # line 217: NEVER CALLED
  row_cors = cor(t(df)); mean_row_cor = mean(row_cors, na.rm = T)
  row_cor_kurtosis = Kurt(row_cors)
  right_tail_cor_kurt = Kurt(row_cors[row_cors > mean(row_cors, na.rm = T)])
  overallKurt = Kurt(as.numeric(df), na.rm = T)
  overall_right_kurt = Kurt(df > mean(df, na.rm = T))                        # line 233: kurtosis of a LOGICAL
  overallSD <- sd(df, na.rm = T)
  var_col_first_PC <- var(prcomp(df)$x[,1])
  skew_col_first_PC <- Skew(prcomp(df)$x[,1]); skew_col_second_PC <- Skew(prcomp(df)$x[,2])
  skewness <- Skew(df)
  df_new = as.data.frame(df)
  mean_column_kurt = mean(sapply(df_new, Kurt), na.rm = T)
  mean_sd_ratio = mean(sapply(df_new, FUN = function(x) sd(x, na.rm = T))/sd(df, na.rm = T), na.rm = T)
  df_new$class = sample(1:4, nrow(df_new), replace = T)
  nrOut = statistical(class ~ ., df_new, features="nrOutliers", by.class=F)$nrOutliers
  intrinsicDemnsionality <- pcaLocalDimEst(df, ver = 'fan')$dim.est
  intrinsicDemnsionalityProp <- intrinsicDemnsionality/num_vars              # line 262: num_vars
  data.frame(OverallSD = overallSD, OverallKurt = overallKurt, OverallRightKurt = overall_right_kurt,
    VarColPC1 = var_col_first_PC, SkewColPC1 = skew_col_first_PC, SkewColPC2 = skew_col_second_PC,
    Skewness = skewness, Mean_Row_Corr = mean_row_cor, Row_Corr_Kurtosis = row_cor_kurtosis,
    CorrRightKurt = right_tail_cor_kurt, Mean_Col_Kurtosis = mean_column_kurt,
    Mean_SD_Ratio = mean_sd_ratio, PercentOut = ((nrOut/num_vars) * 100),    # line 268: num_vars
    intrinsicDemnsionalityProp = intrinsicDemnsionalityProp, nrOut_raw = nrOut)
}

mk <- function(n, p, seed) { set.seed(seed); matrix(sample(1:5, n*p, TRUE), nrow = n) }

cat("################ FINDING 1: num_vars = 10 hardcode ################\n")
cat("Released default num_vars=10 vs correctly parameterised num_vars=ncol(df).\n\n")
for (p in c(10, 18)) {
  m <- mk(400, p, 100 + p)
  a <- released.evaluate.metafeatures(m)                 # default: num_vars = 10
  b <- released.evaluate.metafeatures(m, num_vars = p)   # correct
  cat(sprintf("--- %d-item matrix (400 respondents) ---\n", p))
  cat(sprintf("  raw nrOutliers (count of outlier-bearing variables) = %s\n", a$nrOut_raw))
  cat(sprintf("  %-28s default=%10.5f   correct=%10.5f   %s\n", "PercentOut",
              a$PercentOut, b$PercentOut, ifelse(isTRUE(all.equal(a$PercentOut,b$PercentOut)),"same","DIVERGES")))
  cat(sprintf("  %-28s default=%10.5f   correct=%10.5f   %s\n", "intrinsicDemnsionalityProp",
              a$intrinsicDemnsionalityProp, b$intrinsicDemnsionalityProp,
              ifelse(isTRUE(all.equal(a$intrinsicDemnsionalityProp,b$intrinsicDemnsionalityProp)),"same","DIVERGES")))
  other <- setdiff(names(a), c("PercentOut","intrinsicDemnsionalityProp","nrOut_raw"))
  same <- all(sapply(other, function(f) isTRUE(all.equal(a[[f]], b[[f]]))))
  cat(sprintf("  all %d other features identical: %s\n\n", length(other), same))
}

cat("################ FINDING 2: OverallRightKurt on a logical ################\n")
m <- mk(400, 12, 7)
released_val <- Kurt(m > mean(m, na.rm = TRUE))                # as shipped (line 233)
intended_val <- Kurt(as.numeric(m)[as.numeric(m) > mean(m, na.rm = TRUE)])  # right_half_kurt (line 217)
cat(sprintf("  as released  Kurt(df > mean(df))          = %.6f\n", released_val))
cat(sprintf("  as intended  right_half_kurt(as.numeric(df)) = %.6f\n", intended_val))
cat(sprintf("  class of the argument actually passed: %s\n", class(m > mean(m))[1]))
cat("  Bernoulli-indicator identity check: for p=mean(indicator), excess kurtosis = (1-6p(1-p))/(p(1-p))\n")
pr <- mean(m > mean(m)); cat(sprintf("  p = %.4f  ->  closed form = %.6f  (matches released value)\n\n",
                                     pr, (1-6*pr*(1-pr))/(pr*(1-pr))))

cat("################ FINDING 4: PercentOut scale ################\n")
cat("Released formula: (nrOut/num_vars)*100, nrOut = COUNT of outlier-bearing variables.\n")
cat("So PercentOut is a multiple of 100/num_vars and lies on [0,100].\n")
for (p in c(7, 10, 14, 16)) cat(sprintf("  p=%2d -> attainable values: 0, %.2f, %.2f, ... 100\n", p, 100/p, 200/p))
cat("\nTable 9's published PercentOut column: 0.000, 0.002, 0.002, 0.004, 0.006, 0.007, 0.008, 0.022, 0.048\n")
cat("Smallest non-zero attainable by the released formula (p=40) = ", 100/40, "\n")
cat("=> Table 9's magnitudes are UNREACHABLE by the released formula.\n\n")
cat("Table 9 intrinsic-dimensionality column, as fractions:\n")
t9id <- c(0.429,0.357,0.333,0.500,0.200,0.286,0.313,0.571,0.333)
for (v in t9id) { den <- which(abs(round(v*(1:40))/(1:40) - v) < 5e-4)[1]
  cat(sprintf("  %.3f ~ %d/%d\n", v, round(v*den), den)) }
cat("\nIf num_vars had been left at 10, every value would be a multiple of 0.1.\n")
cat("They are not => Table 9 WAS computed with num_vars = actual item count.\n")
