## run ONE table's Task 4 smoke test; args: <label> <path> <N> <do_recursive>
S <- "/tmp/claude-1000/-home-ben-Dropbox-projects-irw-irw-site-vignettes/bffe2e22-3ff9-49bd-8dab-f1d88318f991/scratchpad/rca"
a <- commandArgs(TRUE); label <- a[1]; path <- a[2]; N <- as.integer(a[3]); do_rec <- as.logical(a[4])
.libPaths(c(file.path(S, "rlib"), .libPaths()))
suppressMessages({ library(dplyr); library(readr); library(igraph); library(Rcpp)
                   library(RCA); library(corclass) })
set.seed(20260827)
sourceCpp(file.path(S, "drive", "f_outer.cpp"))

f <- function(m) { m[lower.tri(m)] <- t(m)[lower.tri(m)]; m }
recursive.rca.dist <- function(data) {
  cl <- lapply(seq_len(nrow(data)), function(i) {
    x <- f_outer(f_outer(as.numeric(data[i, ]))); sign(x)   # sign() == their two ifelse passes, one alloc
  })
  d <- matrix(NA_real_, nrow(data), nrow(data))
  for (i in 1:(nrow(d) - 1)) for (j in (i + 1):nrow(d)) d[i, j] <- relationalityC(cl[[i]], cl[[j]])
  f(d)
}
revkey_eta2 <- function(m, memb) {
  z <- scale(m); ld <- prcomp(m, scale. = TRUE)$rotation[, 1]
  s <- as.numeric(z %*% sign(ld))
  keep <- memb %in% names(which(table(memb) >= 2))
  if (length(unique(memb[keep])) < 2) return(NA_real_)
  summary(lm(s[keep] ~ factor(memb[keep])))$r.squared
}
summ <- function(memb) { tb <- sort(table(memb), decreasing = TRUE)
  list(k = length(tb), k_sub = sum(tb >= 0.05 * length(memb)), largest = as.numeric(tb[1]) / length(memb)) }

m <- readRDS(path)
m <- m[stats::complete.cases(m), , drop = FALSE]
m <- m[, apply(m, 2, var) > 1e-9, drop = FALSE]
m <- m[apply(m, 1, var) > 1e-9, , drop = FALSE]
if (nrow(m) > N) m <- m[sample(nrow(m), N), , drop = FALSE]
rows <- list()
add <- function(meth, secs, memb, mm = m) {
  if (is.null(memb)) return(data.frame(table=label, N=nrow(m), n_items=ncol(m), method=meth,
    secs=secs, k=NA, k_substantive=NA, largest_prop=NA, revkey_eta2=NA))
  s <- summ(memb)
  data.frame(table=label, N=nrow(m), n_items=ncol(m), method=meth, secs=secs, k=s$k,
    k_substantive=s$k_sub, largest_prop=s$largest, revkey_eta2=tryCatch(revkey_eta2(mm,memb), error=function(e) NA_real_))
}

t0 <- Sys.time(); r <- try(RCA::RCA(as.data.frame(m), num=1000, alpha=0.05), silent=TRUE)
rows[[1]] <- add("RCA (Goldberg, bootstrap num=1000)", as.numeric(difftime(Sys.time(),t0,units="secs")),
                 if (inherits(r,"try-error")) NULL else r$membership)

t0 <- Sys.time(); r <- try(corclass::cca(as.data.frame(m), filter.significance=TRUE, filter.value=0.01,
                                         zero.action="drop", verbose=FALSE), silent=TRUE)
t_cca <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
rows[[2]] <- if (inherits(r, "try-error")) {
  add("CCA (corclass)", t_cca, NULL)
} else {
  add("CCA (corclass)", t_cca, r$membership, m[seq_along(r$membership), , drop = FALSE])
}

if (do_rec) {
  t0 <- Sys.time(); r <- try({ d <- recursive.rca.dist(m); diag(d) <- 0; d <- d*d
    net <- graph_from_adjacency_matrix(d, mode="undirected", weighted=TRUE)
    as.numeric(membership(cluster_louvain(net, weights=E(net)$weight))) }, silent=TRUE)
  rows[[3]] <- add("Recursive Relationality (paper's pick)", as.numeric(difftime(Sys.time(),t0,units="secs")),
                   if (inherits(r,"try-error")) NULL else r)
} else {
  rows[[3]] <- data.frame(table=label, N=nrow(m), n_items=ncol(m),
    method="Recursive Relationality (paper's pick)", secs=NA, k=NA, k_substantive=NA,
    largest_prop=NA, revkey_eta2=NA)
}
out <- bind_rows(rows)
write_csv(out, file.path(S, "t4_parts", paste0(label, "_N", N, ".csv")))
print(out)
