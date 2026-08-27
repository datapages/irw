## Task 4 - non-degeneracy + timing smoke test
S <- "/tmp/claude-1000/-home-ben-Dropbox-projects-irw-irw-site-vignettes/bffe2e22-3ff9-49bd-8dab-f1d88318f991/scratchpad/rca"
.libPaths(c(file.path(S, "rlib"), .libPaths()))
suppressMessages({ library(dplyr); library(readr); library(igraph); library(Rcpp)
                   library(RCA); library(corclass); library(proxy) })
set.seed(20260827)
sourceCpp(file.path(S, "drive", "f_outer.cpp"))

## --- their recursive-relationality RCA (metaRCA core), lifted verbatim -------
f <- function(m) { m[lower.tri(m)] <- t(m)[lower.tri(m)]; m }
recursive_gen_change_list <- function(df) lapply(seq_len(nrow(df)), function(i) f_outer(f_outer(as.numeric(df[i, ]))))
recursive.rca.dist <- function(data) {
  data <- as.matrix(data)
  cl <- recursive_gen_change_list(data)
  cl <- lapply(cl, function(x) ifelse(x > 0, 1, ifelse(x < 0, -1, x)))
  d <- matrix(NA_real_, nrow(data), nrow(data))
  for (i in 1:(nrow(d) - 1)) for (j in (i + 1):nrow(d)) d[i, j] <- relationalityC(cl[[i]], cl[[j]])
  f(d)
}
metaRCA_recursive <- function(data) {
  d <- recursive.rca.dist(data); diag(d) <- 0; d <- d * d
  net <- graph_from_adjacency_matrix(d, mode = "undirected", weighted = TRUE)
  cl <- cluster_louvain(net, weights = E(net)$weight)
  list(membership = as.numeric(membership(cl)), modularity = modularity(cl))
}

## --- reverse-keying diagnostic ----------------------------------------------
## Person score on the PC1-polarity-consistent direction: s_i = sum_j sign(load_j) * z_ij.
## Then eta^2 of class membership on s. High eta^2 => classes largely track
## "agrees with PC1 polarity" vs not, i.e. careless / polarity-ignoring responders.
revkey_eta2 <- function(m, memb) {
  z <- scale(m); ld <- prcomp(m, scale. = TRUE)$rotation[, 1]
  s <- as.numeric(z %*% sign(ld))
  keep <- memb %in% names(which(table(memb) >= 2))
  if (length(unique(memb[keep])) < 2) return(NA_real_)
  summary(lm(s[keep] ~ factor(memb[keep])))$r.squared
}

summ <- function(memb) {
  tb <- sort(table(memb), decreasing = TRUE)
  list(k = length(tb), k_sub = sum(tb >= 0.05 * length(memb)),
       largest_prop = as.numeric(tb[1]) / length(memb))
}

run_table <- function(name, m, N) {
  m <- m[stats::complete.cases(m), , drop = FALSE]
  m <- m[, apply(m, 2, var) > 1e-9, drop = FALSE]
  m <- m[apply(m, 1, var) > 1e-9, , drop = FALSE]
  if (nrow(m) > N) m <- m[sample(nrow(m), N), , drop = FALSE]
  out <- list()

  t0 <- Sys.time()
  r1 <- try(RCA::RCA(as.data.frame(m), num = 1000, alpha = 0.05), silent = TRUE)
  t_rca <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (!inherits(r1, "try-error")) {
    memb <- r1$membership
    s <- summ(memb)
    out$rca_boot <- data.frame(method = "RCA (Goldberg, bootstrap num=1000)", secs = t_rca,
                               k = s$k, k_substantive = s$k_sub, largest_prop = s$largest_prop,
                               revkey_eta2 = revkey_eta2(m, memb))
  } else out$rca_boot <- data.frame(method = "RCA (Goldberg, bootstrap num=1000)", secs = t_rca,
                                    k = NA, k_substantive = NA, largest_prop = NA, revkey_eta2 = NA)

  t0 <- Sys.time()
  r2 <- try(corclass::cca(as.data.frame(m), filter.significance = TRUE, filter.value = 0.01,
                          zero.action = "drop", verbose = FALSE), silent = TRUE)
  t_cca <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (!inherits(r2, "try-error")) {
    memb <- r2$membership; s <- summ(memb)
    keep <- seq_len(nrow(m)) %in% seq_along(memb)
    out$cca <- data.frame(method = "CCA (corclass)", secs = t_cca, k = s$k, k_substantive = s$k_sub,
                          largest_prop = s$largest_prop,
                          revkey_eta2 = tryCatch(revkey_eta2(m[seq_along(memb), , drop = FALSE], memb), error = function(e) NA_real_))
  } else out$cca <- data.frame(method = "CCA (corclass)", secs = t_cca, k = NA, k_substantive = NA,
                               largest_prop = NA, revkey_eta2 = NA)

  t0 <- Sys.time()
  r3 <- try(metaRCA_recursive(m), silent = TRUE)
  t_rr <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (!inherits(r3, "try-error")) {
    s <- summ(r3$membership)
    out$recursive <- data.frame(method = "Recursive Relationality (paper's pick)", secs = t_rr,
                                k = s$k, k_substantive = s$k_sub, largest_prop = s$largest_prop,
                                revkey_eta2 = revkey_eta2(m, r3$membership))
  } else out$recursive <- data.frame(method = "Recursive Relationality (paper's pick)", secs = t_rr,
                                     k = NA, k_substantive = NA, largest_prop = NA, revkey_eta2 = NA)

  cbind(data.frame(table = name, N = nrow(m), n_items = ncol(m)), bind_rows(out))
}
