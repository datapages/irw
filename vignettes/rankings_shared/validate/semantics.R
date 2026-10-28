suppressMessages(library(PlackettLuce))
# 4 items, each ranking marks 2 of them, leaving 2 unmarked.
O <- matrix(c("a","b", "b","c", "c","d", "d","a"), ncol=2, byrow=TRUE)
R <- as.rankings(O, input="orderings", items=c("a","b","c","d"))
print(R)
fit <- PlackettLuce(R, npseudo=0)
cf <- coef(fit, log=TRUE)          # log-worths, first item aliased to 0
w  <- exp(cf[c("a","b","c","d")]); w[is.na(w)] <- 1
cat("\ncoef:", paste(names(cf), round(cf,6), collapse="  "), "\n")

ll_subset <- 0; ll_full <- 0
for (i in 1:nrow(O)) {
  b <- O[i,]
  # interp 1: choice sets are the marked items only
  ll_subset <- ll_subset + log(w[b[1]]/(w[b[1]]+w[b[2]])) + log(w[b[2]]/w[b[2]])
  # interp 2: choice sets are ALL items not yet marked
  pool <- c("a","b","c","d")
  ll_full <- ll_full + log(w[b[1]]/sum(w[pool]))
  pool <- setdiff(pool, b[1])
  ll_full <- ll_full + log(w[b[2]]/sum(w[pool]))
}
cat("logLik(fit)      =", as.numeric(logLik(fit)), "\n")
cat("interp1 subset   =", ll_subset, "\n")
cat("interp2 full-set =", ll_full, "\n")
