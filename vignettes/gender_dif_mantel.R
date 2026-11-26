# gender_dif_mantel.R
#
# Mantel (1963) test for DIF on ordinal (polytomous) items, the polytomous
# extension of the Mantel-Haenszel procedure (Zwick, Donoghue & Grima, 1993).
# difR::difGMH is the generalized MH test for several focal groups on
# dichotomous items, so it cannot be used on items scored 1-5 (datapages/irw#430).
#
# For item j, people are stratified by a matching score (the sum of the
# matching items). In stratum k, with n_Rk reference and n_Fk focal members,
# N_k = n_Rk + n_Fk, and item scores y:
#   F_k   = sum of y over the focal group
#   E_k   = n_Fk / N_k * sum(y)
#   V_k   = n_Rk * n_Fk / (N_k^2 * (N_k - 1)) * (N_k * sum(y^2) - sum(y)^2)
#   chi^2 = (sum F_k - sum E_k)^2 / sum V_k,  1 df.
# Strata with N_k < 2 or no member of one group carry no information and are
# skipped. A person missing the studied item is left out of that item's test;
# the matching score is the sum over the matching items a person answered,
# rescaled to the number of matching items so missing answers do not shift the
# stratum (rounded to the nearest integer).
#
# The effect size is Dorans' standardized mean difference (SMD): the focal
# group's mean item score minus the reference group's, within strata, weighted
# by the focal group's share of each stratum. Positive means the focal group
# scores higher than matched reference-group members.
#
# Purification, as difR does for MH: items flagged at alpha are dropped from
# the matching score (the studied item always stays in its own score), and the
# tests are rerun until the flagged set stops changing (at most 10 rounds).

mantel_one <- function(y, g, s) {
  ok <- !is.na(y) & !is.na(s)
  y <- y[ok]; g <- g[ok]; s <- s[ok]
  Fs <- Es <- Vs <- 0
  smd_num <- 0; nf_tot <- 0
  for (k in unique(s)) {
    i <- s == k
    yk <- y[i]; gk <- g[i]
    nF <- sum(gk == 1); nR <- sum(gk == 0); N <- nF + nR
    if (N < 2 || nF == 0 || nR == 0) next
    sy <- sum(yk); syy <- sum(yk^2)
    Fs <- Fs + sum(yk[gk == 1])
    Es <- Es + nF / N * sy
    Vs <- Vs + nR * nF / (N^2 * (N - 1)) * (N * syy - sy^2)
    smd_num <- smd_num + nF * (mean(yk[gk == 1]) - mean(yk[gk == 0]))
    nf_tot <- nf_tot + nF
  }
  if (Vs <= 0) return(c(chi = NA_real_, p = NA_real_, smd = NA_real_))
  chi <- (Fs - Es)^2 / Vs
  c(chi = chi, p = stats::pchisq(chi, 1, lower.tail = FALSE),
    smd = if (nf_tot > 0) smd_num / nf_tot else NA_real_)
}

mantel_dif <- function(resp, group, alpha = 0.05, purify = TRUE, max_iter = 10) {
  resp <- as.matrix(resp)
  ni <- ncol(resp)
  match_score <- function(cols, j) {
    cols <- union(cols, j)
    sub <- resp[, cols, drop = FALSE]
    answered <- rowSums(!is.na(sub))
    sc <- rowSums(sub, na.rm = TRUE) / pmax(answered, 1) * length(cols)
    sc[answered == 0] <- NA
    round(sc)
  }
  run <- function(anchor) {
    t(vapply(seq_len(ni), function(j)
      mantel_one(resp[, j], group, match_score(anchor, j)), numeric(3)))
  }
  anchor <- seq_len(ni)
  res <- run(anchor)
  iter <- 0L
  if (purify) {
    flagged <- which(res[, "p"] < alpha)
    repeat {
      iter <- iter + 1L
      if (iter > max_iter || length(flagged) == ni) break
      new_anchor <- setdiff(seq_len(ni), flagged)
      res <- run(new_anchor)
      new_flagged <- which(res[, "p"] < alpha)
      if (identical(new_flagged, flagged)) break
      flagged <- new_flagged
    }
  }
  list(p.value = res[, "p"], statistic = res[, "chi"], smd = res[, "smd"],
       iterations = iter)
}
