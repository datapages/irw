suppressMessages(library(PlackettLuce))
items <- c("a","b","c","d")
# same 4 ballots, but the 2 unmarked items tied for last
Rk <- rbind(c(1,2,3,3), c(3,1,2,3), c(3,3,1,2), c(2,3,3,1))
colnames(Rk) <- items
R <- as.rankings(Rk, input="rankings")
print(R)
fit <- PlackettLuce(R, npseudo=0)
cat("\ncoef:", paste(names(coef(fit)), round(coef(fit),4), collapse="  "), "\n")
cat("logLik(ties fit) =", as.numeric(logLik(fit)), "\n")
w <- rep(1,4); names(w) <- items
O <- matrix(c("a","b","b","c","c","d","d","a"), ncol=2, byrow=TRUE)
ll_full <- 0
for (i in 1:nrow(O)) { pool <- items
  for (m in O[i,]) { ll_full <- ll_full + log(w[m]/sum(w[pool])); pool <- setdiff(pool,m) } }
cat("interp2 full-set at equal worths =", ll_full, "\n")
