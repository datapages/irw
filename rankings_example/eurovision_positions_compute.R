## Companion compute script for rankings_standard.qmd ("Where the information
## sits in a ranking"). Writes rankings_example/eurovision_positions.rds, which
## the page reads; rerun this by hand when the table or the method changes.
##
## Kept off the render because PlackettLuce is not in renv.lock (it pulls in
## psychotools, igraph and more) and the page should not take that dependency
## for one figure. Everything else here is base R.
##
## Spec: oneoff/rankings-vignette-pilot-2026-10-15/STANDARD_PAGE_EXAMPLE.md in
## ben-domingue/irw. One table, eurovision_2016_final: 210 jurors, 26 acts, each
## juror ranks every act except their own country's. Two curves over position
## 1..26: (1) agreement by slot, the entropy of which act occupies the slot
## across jurors over log(26); (2) held-out per-position gain of a forward
## Plackett-Luce fit against the uniform choice among the acts still available,
## and of the same fit on reversed lists (worst first), mapped back to the
## original positions. Gain = exp(mean log-likelihood difference per choice) - 1,
## a likelihood-ratio gain against chance (not IMV: the equivalent-coin IMV is
## undefined below log 0.5, where a uniform 26-way choice sits).
suppressPackageStartupMessages({ library(redivis); library(PlackettLuce) })
set.seed(20261015)
TABLE <- "eurovision_2016_final"
ds <- redivis$user("datapages")$dataset("irw_rank:93f5")
version <- ds$get()$properties$version$tag
d <- as.data.frame(ds$table(TABLE)$to_tibble())
d <- d[!d$item %in% c("(write-in)", "(overvote)"), ]
d <- d[order(d$id, d$resp), ]
## irw_rank_compress() renumbers 1..k within id x task; Eurovision positions
## are already consecutive, so it changes nothing here, and the site's pinned
## irw predates it. Same thing in base R, so the step is on record.
d$resp <- as.integer(stats::ave(seq_len(nrow(d)), d$id, FUN = seq_along))
acts <- sort(unique(d$item))
n <- length(acts)
own <- tapply(d$cov_jury_country, d$id, function(x) x[1])
orderings <- split(d$item, d$id)
stopifnot(all(lengths(orderings) %in% c(n - 1L, n)))
ids <- names(orderings)
N <- length(ids)
message(TABLE, " ", version, ": ", N, " jurors, ", n, " acts (", format(Sys.time()), ")")

## (1) Agreement by slot. Two readings of the same thing: the share of jurors
## who put the slot's most common act in that slot (plain), and the entropy of
## the occupant of the slot over log(n) (0 = everyone agrees, 1 = any act is as
## likely as any other).
occupant <- function(p) {
  occ <- vapply(orderings, function(o) if (length(o) >= p) o[p] else NA_character_, "")
  occ[!is.na(occ)]
}
modal_share <- sapply(seq_len(n), function(p) {
  occ <- occupant(p); if (length(occ) < 30) return(NA_real_)
  max(table(occ)) / length(occ)
})
modal_act <- sapply(seq_len(n), function(p) {
  occ <- occupant(p); if (length(occ) < 30) return(NA_character_)
  names(which.max(table(occ)))
})
entropy <- sapply(seq_len(n), function(p) {
  occ <- occupant(p); if (length(occ) < 30) return(NA_real_)
  pr <- table(occ) / length(occ)
  -sum(pr * log(pr)) / log(n)
})

## (2) Held-out per-position gain. A juror's choice set at position p is the
## acts not yet marked, less the juror's own country when it is in the show
## (they never rank it, so it is not a choice they could have made).
fit_worths <- function(ords) {
  ## one row per juror, acts in rank order, NA-padded (a juror who skipped
  ## their own country has 25 marks).
  M <- t(vapply(ords, function(o) c(o, rep(NA_character_, n - length(o))), character(n)))
  R <- as.rankings(M, input = "orderings")
  lw <- coef(PlackettLuce(R), log = TRUE)
  lw <- lw[acts]; lw[is.na(lw)] <- 0
  lw
}
score <- function(lw, ords, owns) {
  ## matrix: rows = jurors, cols = position in the list as scored; the gain of
  ## the mark at each position against uniform over the acts still available.
  g <- matrix(NA_real_, length(ords), n)
  for (j in seq_along(ords)) {
    o <- ords[[j]]
    avail <- setdiff(acts, owns[j])
    for (p in seq_along(o)) {
      lm <- lw[[o[p]]] - log(sum(exp(lw[avail])))
      g[j, p] <- lm + log(length(avail))        # minus log(1/|avail|)
      avail <- setdiff(avail, o[p])
    }
  }
  g
}
folds <- sample(rep(seq_len(5), length.out = N))
g_fwd <- matrix(NA_real_, N, n); g_rev <- matrix(NA_real_, N, n)
for (f in seq_len(5)) {
  message("fold ", f, " ", format(Sys.time()))
  tr <- folds != f; te <- !tr
  lw_f <- fit_worths(orderings[tr])
  lw_r <- fit_worths(lapply(orderings[tr], rev))
  g_fwd[te, ] <- score(lw_f, orderings[te], own[te])
  gr <- score(lw_r, lapply(orderings[te], rev), own[te])
  ## back to the original positions: a list of length k scored in reverse has
  ## its p-th reversed mark at original position k - p + 1.
  for (j in which(te)) {
    k <- length(orderings[[j]]); jj <- match(j, which(te))
    g_rev[j, k:1] <- gr[jj, 1:k]
  }
}
gain <- function(g) apply(g, 2, function(x) { x <- x[!is.na(x)]; if (length(x) < 30) NA_real_ else exp(mean(x)) - 1 })
gain_fwd <- gain(g_fwd); gain_rev <- gain(g_rev)
lw_all_f <- fit_worths(orderings); lw_all_r <- fit_worths(lapply(orderings, rev))
rho <- cor(lw_all_f, -lw_all_r, method = "spearman")

out <- list(table = TABLE, dataset = "irw_rank:93f5", version = version, computed = Sys.Date(),
            n_jurors = N, n_acts = n, modal_share = modal_share, modal_act = modal_act, entropy = entropy, gain_fwd = gain_fwd, gain_rev = gain_rev,
            rho_fwd_vs_rev = rho)
saveRDS(out, file.path("rankings_example", "eurovision_positions.rds"))
cat(sprintf("modal:   %s\n", paste(round(modal_share, 2), collapse = " ")))
cat(sprintf("entropy: %s\n", paste(round(entropy, 2), collapse = " ")))
cat(sprintf("fwd:     %s\n", paste(round(gain_fwd, 2), collapse = " ")))
cat(sprintf("rev:     %s\n", paste(round(gain_rev, 2), collapse = " ")))
cat(sprintf("spearman fwd vs rev worths: %.2f\n", rho))
