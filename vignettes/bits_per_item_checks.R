# bits_per_item_checks.R
#
# Validation for bits_per_item_helpers.R. Run before the full batch:
#   Rscript vignettes/bits_per_item_checks.R   # from project root
# If bits_per_item_results.rds exists, check 4 is also run over every table in it.
# Output: bits_per_item_data/checks_results.csv. Tolerances are fixed here,
# before looking at the results.

suppressMessages({
  library(irw)
  library(mirt)
  library(dplyr)
})
source("vignettes/bits_per_item_helpers.R")
set.seed(20261005)

out <- list()
record <- function(check, pass, detail) {
  out[[length(out) + 1]] <<- data.frame(check = check, pass = pass, detail = detail)
  message(if (pass) "PASS " else "FAIL ", check, ": ", detail)
}

prep_fit <- function(t) {
  r <- prepare_resp(irw_fetch(t))$resp
  list(resp = r, fit2 = fit_2pl(r),
       fitR = mirt(r, 1, itemtype = "Rasch", verbose = FALSE))
}
pars <- function(fit) { c <- coef(fit, simplify = TRUE); list(a = c$items[, "a1"], d = c$items[, "d"], sd = sqrt(c$cov[1, 1])) }

check_tables <- c("fitz_2024_numeracy", "machivallianism_test_vcl", "frac20", "buzgova_2023_gai")
fits <- setNames(lapply(check_tables, prep_fit), check_tables)

# 1. Binary entropy
v <- h2(c(0.5, 0, 1))
record("1 h(0.5)=1, h(0)=h(1)=0", isTRUE(all.equal(v, c(1, 0, 0))) && !anyNA(v),
       paste(v, collapse = ", "))

# 2. A single item gives at most 1 bit
g <- make_grid(1)
grid_ab <- expand.grid(a = c(0.2, 1, 3, 10, 50), d = c(-3, 0, 3))
ib <- item_info_bits(p_matrix(grid_ab$a, grid_ab$d, g$nodes), g$w)
ib_show <- unlist(lapply(fits, function(f) { p <- pars(f$fit2); item_info_bits(p_matrix(p$a, p$d, g$nodes), g$w) }))
record("2 single item <= 1 bit", all(c(ib, ib_show) <= 1),
       sprintf("max over synthetic items %.4f (a=50, d=0); max over %d fitted items %.4f",
               max(ib), length(ib_show), max(ib_show)))

# 3. Rasch: I(theta; S) equals enumeration I(theta; X) (fitz, 11 items)
p <- pars(fits$fitz_2024_numeracy$fitR); gr <- make_grid(p$sd)
Pr <- p_matrix(p$a, p$d, gr$nodes)
d3 <- abs(info_sum(Pr, gr$w) - info_pattern_exact(Pr, gr$w)$I)
record("3 Rasch I_S = I_X (enumeration, n=11)", d3 < 1e-10, sprintf("|diff| = %.2e", d3))

# 4. 2PL: I(theta; S) <= I(theta; X). Exact tables: strict. MC tables: I_S <= I_X + 3 SE.
res4 <- sapply(fits, function(f) {
  p <- pars(f$fit2); P <- p_matrix(p$a, p$d, g$nodes)
  ip <- info_pattern(P, g$w)
  c(I_S = info_sum(P, g$w), I_X = ip$I, se = ip$se)
})
ok4 <- res4["I_S", ] <= res4["I_X", ] + 3 * res4["se", ]
record("4 2PL I_S <= I_X (check tables)", all(ok4),
       paste(sprintf("%s %.3f<=%.3f", colnames(res4), res4["I_S", ], res4["I_X", ]), collapse = "; "))
rf <- "vignettes/bits_per_item_data/bits_per_item_results.rds"
if (file.exists(rf)) {
  s <- filter(readRDS(rf)$summary, model == "2PL")
  bad <- s$table[s$I_S > s$I_X + 3 * s$I_X_se]
  record("4b 2PL I_S <= I_X (all batch tables)", length(bad) == 0,
         sprintf("%d tables; violations: %s", nrow(s), if (length(bad)) paste(bad, collapse = ",") else "none"))
}

# 5. Monte Carlo agrees with enumeration (2PL, n <= 16), within 3 SE
for (t in c("fitz_2024_numeracy", "machivallianism_test_vcl")) {
  p <- pars(fits[[t]]$fit2); P <- p_matrix(p$a, p$d, g$nodes)
  ex <- info_pattern_exact(P, g$w)$I; mc <- info_pattern_mc(P, g$w, target_se = 0.005)
  record(paste("5 MC vs enumeration,", t), abs(ex - mc$I) < 3 * mc$se,
         sprintf("exact %.4f, MC %.4f (SE %.4f, %d draws)", ex, mc$I, mc$se, mc$draws))
}

# 6. Binned information <= continuous and increasing in K
for (t in c("fitz_2024_numeracy", "frac20")) {
  p <- pars(fits[[t]]$fit2)
  bc <- bin_convergence(p$a, p$d, Kmax = 8)
  b <- bc$bits[is.finite(bc$K)]; cont <- bc$bits[!is.finite(bc$K)]
  record(paste("6 binned <= continuous, increasing in K,", t),
         all(b <= cont + 3 * max(bc$se)) && all(diff(b) > 0),
         sprintf("%s: K=1..8 %s; continuous %.4f", bc$method[1],
                 paste(sprintf("%.3f", b), collapse = " "), cont))
}

# 7. Replays: expected bits <= 1 and starting entropy exactly K = 6
p <- pars(fits$fitz_2024_numeracy$fit2)
PG <- group_probs(p$a, p$d, 6); colnames(PG) <- names(p$a)
r <- fits$fitz_2024_numeracy$resp
idx <- sample(which(complete.cases(r)), 20)
reps <- unlist(lapply(idx, function(i) {
  x <- unlist(r[i, ])
  lapply(c("fixed", "adaptive"), function(o) replay(x, PG, o)$steps)
}), recursive = FALSE)
eb_max <- max(sapply(reps, function(s) max(s$expected_bits, na.rm = TRUE)))
h0 <- unique(sapply(reps, function(s) s$entropy[1]))
record("7 replay expected bits <= 1, start entropy = 6", eb_max <= 1 && all(h0 == 6),
       sprintf("max expected bits %.4f; starting entropy %s (40 replays)", eb_max, paste(h0, collapse = ",")))

# 8. Simulation: refitting recovers I close to the true-parameter value (|diff| < 0.05 bits)
n <- 15; a_true <- rlnorm(n, 0, 0.4); d_true <- rnorm(n)
Ptrue <- p_matrix(a_true, d_true, g$nodes)
I_true <- info_pattern_exact(Ptrue, g$w)$I
th <- rnorm(5000)
X <- as.data.frame((matrix(runif(5000 * n), 5000, n) < plogis(outer(th, a_true) + matrix(d_true, 5000, n, byrow = TRUE))) * 1)
names(X) <- paste0("i", 1:n)
p <- pars(fit_2pl(X))
I_fit <- info_pattern_exact(p_matrix(p$a, p$d, g$nodes), g$w)$I
record("8 simulated recovery (n=15, N=5000)", abs(I_fit - I_true) < 0.05,
       sprintf("true %.4f, refit %.4f", I_true, I_fit))

# 9. Grid stability: 81 vs 161 nodes, < 0.001 bits
g161 <- make_grid(1, n_nodes = 161)
res9 <- sapply(fits, function(f) {
  p <- pars(f$fit2)
  P81 <- p_matrix(p$a, p$d, g$nodes); P161 <- p_matrix(p$a, p$d, g161$nodes)
  c(I_S = abs(info_sum(P81, g$w) - info_sum(P161, g161$w)),
    H_XT = abs(h_x_given_theta(P81, g$w) - h_x_given_theta(P161, g161$w)),
    I_X = if (length(p$a) <= 16) abs(info_pattern_exact(P81, g$w)$I - info_pattern_exact(P161, g161$w)$I) else NA)
})
record("9 grid 81 vs 161 nodes < 0.001 bits", all(res9 < 0.001, na.rm = TRUE),
       sprintf("max |diff| %.2e over I_S, H(X|theta), and exact I_X where n <= 16 (%s)",
               max(res9, na.rm = TRUE), paste(colnames(res9), collapse = ", ")))

# ------------------------------------------------------------------------------
# Sufficiency checks (sufficiency_results.rds from bits_per_item_sufficiency.R)
# ------------------------------------------------------------------------------
sf <- "vignettes/bits_per_item_data/sufficiency_results.rds"
if (file.exists(sf)) {
  suff <- readRDS(sf)$showcases
  sw <- do.call(rbind, lapply(suff, `[[`, "sweep"))
  r <- sw[sw$model == "Rasch", ]; t2 <- sw[sw$model == "2PL", ]
  record("S1 Rasch I(theta;X|S) = 0 under every prior", all(abs(r$I_X_given_S) < 1e-10),
         sprintf("%d model-prior cases, max |I(X|S)| %.1e bits", nrow(r), max(abs(r$I_X_given_S))))
  record("S2 2PL I(theta;X|S) > 0 and I_S <= I_X under every prior",
         all(t2$I_X_given_S > 0) && all(sw$I_S <= sw$I_X + 1e-12),
         sprintf("2PL I(X|S) from %.4f to %.4f bits over %d cases", min(t2$I_X_given_S), max(t2$I_X_given_S), nrow(t2)))
  wt <- do.call(rbind, lapply(suff, `[[`, "weighted_score"))
  record("S3 2PL weighted score sum a_j x_j: I(theta;T) = I(theta;X)", all(abs(wt$I_T - wt$I_X) < 1e-10),
         paste(sprintf("%s |diff| %.1e (%d T levels / %d patterns)", wt$table, abs(wt$I_T - wt$I_X),
                       wt$n_levels, wt$n_patterns), collapse = "; "))
  tot <- do.call(rbind, lapply(suff, function(x) {
    lad <- x$ladder
    do.call(rbind, lapply(c("Rasch", "2PL"), function(m) {
      b <- x$by_s[x$by_s$model == m, ]
      data.frame(table = x$table, model = m, from_s = sum(b$p_s * b$I_X_given_s),
                 overall = lad$I_X_given_S[lad$model == m])
    }))
  }))
  record("S4 within-score totals reproduce I(theta;X|S)", all(abs(tot$from_s - tot$overall) < 1e-10),
         sprintf("max |sum_s P(s) I(X|S=s) - I(X|S)| %.1e over %d table-models", max(abs(tot$from_s - tot$overall)), nrow(tot)))
  # theta-freeness: P(x | S = s) from the full model at theta = -1 and theta = 1 (fitz Rasch, 11 items)
  fz <- suff$fitz_2024_numeracy; a_r <- rep(fz$rasch_sigma, length(fz$rasch_d))
  Xe <- as.matrix(expand.grid(rep(list(0:1), length(a_r))))
  cond_at <- function(th) {
    P <- p_matrix(a_r, fz$rasch_d, th)
    px <- exp(Xe %*% t(log(P)) + (1 - Xe) %*% t(log(1 - P)))[, 1]
    px / ave(px, rowSums(Xe), FUN = sum)
  }
  dtf <- max(abs(cond_at(-1) - cond_at(1)))
  esf <- max(abs(cond_at(0.3) - 2^rasch_log2_p_given_s(Xe, fz$rasch_d)))
  record("S5 Rasch P(x | S=s) identical at theta = -1 and 1", dtf < 1e-12,
         sprintf("max |diff| %.1e over %d patterns; vs elementary-symmetric formula %.1e", dtf, nrow(Xe), esf))
  fi <- do.call(rbind, lapply(suff, `[[`, "fisher"))
  fr <- fi[fi$model == "Rasch", ]; f2 <- fi[fi$model == "2PL", ]
  record("S6 Fisher: Rasch sum-score info = test info; 2PL <= at every node",
         all(abs(fr$sum_score_info - fr$test_info) < 1e-8 * pmax(1, fr$test_info)) &&
           all(f2$sum_score_info <= f2$test_info + 1e-10),
         sprintf("Rasch max rel |diff| %.1e; 2PL max (sum - test) %.1e", max(abs(fr$sum_score_info - fr$test_info) / fr$test_info),
                 max(f2$sum_score_info - f2$test_info)))
  # Fisher derivative cross-check: analytic vs central difference (step halving < 0.1%)
  p2 <- suff$fitz_2024_numeracy; a2 <- showcase_a <- readRDS("vignettes/bits_per_item_data/bits_per_item_results.rds")$showcases$fitz_2024_numeracy$items
  fd <- function(h, th) {
    up <- lord_wingersky(p_matrix(a2$a, a2$d, th + h)); dn <- lord_wingersky(p_matrix(a2$a, a2$d, th - h))
    mid <- lord_wingersky(p_matrix(a2$a, a2$d, th))
    sum(((up - dn) / (2 * h))^2 / mid)
  }
  an <- fisher_compare(a2$a, a2$d, c(-1, 0, 1.5))$sum_score_info
  cd <- sapply(c(-1, 0, 1.5), function(th) fd(1e-4, th))
  record("S6b sum-score Fisher: analytic LW derivative vs central difference", max(abs(an - cd) / an) < 1e-3,
         sprintf("max rel |diff| %.1e at theta = -1, 0, 1.5 (h = 1e-4)", max(abs(an - cd) / an)))
  lad <- do.call(rbind, lapply(suff, `[[`, "ladder"))
  record("S7 compression ladder ordered: H(X) >= H(S) >= I(theta;S), I(theta;S) <= I(theta;X)",
         all(lad$H_X >= lad$H_S & lad$H_S >= lad$I_S & lad$I_S <= lad$I_X + 1e-12),
         paste(sprintf("%s %s %.2f>=%.2f>=%.2f", lad$table, lad$model, lad$H_X, lad$H_S, lad$I_S), collapse = "; "))
}

# ------------------------------------------------------------------------------
# Polytomous checks (poly_results.rds from bits_per_item_poly_compute.R)
# ------------------------------------------------------------------------------
pf <- "vignettes/bits_per_item_data/poly_results.rds"
if (file.exists(pf)) {
  source("vignettes/bits_per_item_poly_helpers.R")
  options(irw.itemtext_disclaimer = FALSE)
  poly <- readRDS(pf)
  gb <- make_grid(1)

  it <- poly$items
  bound <- pmin(it$log2K, it$H_model)
  record("P1 per-item I(theta;X_j) <= min(log2 K_j, model-implied H(X_j))", all(it$I <= bound + 1e-12),
         sprintf("%d item-models; largest I / bound %.3f; largest I %.3f bits (log2 K up to %.2f)",
                 nrow(it), max(it$I / bound), max(it$I), max(it$log2K)))

  cl <- poly$collapses
  nested_ok <- cl |> group_by(table, model, item) |> group_modify(function(d, ...) {
    cs <- strsplit(d$cuts, ","); bad <- 0L
    for (i in seq_along(cs)) for (j in seq_along(cs))
      if (i != j && all(cs[[j]] %in% cs[[i]]) && d$I[j] > d$I[i] + 1e-12) bad <- bad + 1L
    data.frame(bad = bad, over_full = sum(d$I > d$I_full + 1e-12))
  }) |> ungroup()
  record("P2 every collapse <= full item; coarser never beats finer along nested merges",
         all(nested_ok$bad == 0 & nested_ok$over_full == 0),
         sprintf("%d item-models, %d collapses; nested violations %d; over-full %d",
                 nrow(nested_ok), nrow(cl), sum(nested_ok$bad), sum(nested_ok$over_full)))

  # Fresh GPCM fits on the two enumerable tables, for P3, P7, P8.
  pfit <- function(t, rev) {
    set.seed(sum(utf8ToInt(t)))
    r <- prepare_resp_poly(irw_fetch(t), reverse = rev, seed = sum(utf8ToInt(t)))$resp
    list(gpcm = mirt(r, 1, itemtype = "gpcm", verbose = FALSE),
         nrm = mirt(r, 1, itemtype = "nominal", verbose = FALSE))
  }
  pfits <- list(su_2024_gad7 = pfit("su_2024_gad7", character(0)),
                rosenberg_selfesteem = pfit("rosenberg_selfesteem", poly$settings$reverse$rosenberg_selfesteem))
  for (t in names(pfits)) {
    Ps <- item_probs(pfits[[t]]$gpcm, gb$nodes)
    ex <- poly_info_exact(Ps, gb$w)$I; mc <- poly_info_mc(Ps, gb$w, target_se = 0.005)
    record(paste("P3 MC vs enumeration (GPCM),", t), abs(ex - mc$I) < 3 * mc$se,
           sprintf("exact %.4f over %d patterns, MC %.4f (SE %.4f, %d draws)", ex, prod(sapply(Ps, ncol)),
                   mc$I, mc$se, mc$draws))
  }

  sw <- poly$showcase$sweep
  s_pcm <- filter(poly$summary, model == "PCM", I_X_method == "enumeration")
  record("P4 PCM I(theta;X|S) = 0: every prior (showcase) and every enumerated table",
         all(abs(sw$I_X_given_S[sw$model == "PCM"]) < 1e-10) && all(abs(s_pcm$gap) < 1e-10),
         sprintf("%d priors, max |I(X|S)| %.1e; tables %s, max %.1e", sum(sw$model == "PCM"),
                 max(abs(sw$I_X_given_S[sw$model == "PCM"])), paste(s_pcm$table, collapse = ","), max(abs(s_pcm$gap))))

  ws <- poly$showcase$weighted_score
  record("P5 GPCM weighted score sum a_j x_j: I(theta;T) = I(theta;X) (showcase)", abs(ws$I_T - ws$I_X) < 1e-10,
         sprintf("|diff| %.1e (%d T levels / %d patterns); sum score I(theta;S) %.4f vs I(theta;X) %.4f",
                 abs(ws$I_T - ws$I_X), ws$n_levels, ws$n_patterns, ws$I_S, ws$I_X))

  ge <- poly$showcase$grm_equal_slopes
  record("P6 GRM I(theta;X|S) > 0: every prior, and with one common slope (showcase)",
         all(sw$I_X_given_S[sw$model == "GRM"] > 1e-8) && ge$gap > 1e-8,
         sprintf("free slopes: %.4f to %.4f bits over %d priors; common slope %.3f: %.5f bits (converged %s)",
                 min(sw$I_X_given_S[sw$model == "GRM"]), max(sw$I_X_given_S[sw$model == "GRM"]),
                 sum(sw$model == "GRM"), ge$slope, ge$gap, ge$converged))

  th <- c(-2, -0.5, 0, 1, 2.5)
  fis <- do.call(rbind, lapply(seq_len(extract.mirt(pfits$rosenberg_selfesteem$gpcm, "nitems")), function(j) {
    item <- extract.item(pfits$rosenberg_selfesteem$gpcm, j)
    a <- coef(pfits$rosenberg_selfesteem$gpcm, IRTpars = FALSE, simplify = TRUE)$items[j, "a1"]
    an <- gpcm_fisher(probtrace(item, matrix(th)), a)
    data.frame(an = an, num = numeric_fisher(item, th), mirt = as.vector(iteminfo(item, matrix(th))))
  }))
  record("P7 GPCM Fisher a^2 Var(X_j|theta) = numerical second derivative (and mirt::iteminfo)",
         max(abs(fis$an - fis$num) / fis$an) < 1e-4 && max(abs(fis$an - fis$mirt) / fis$an) < 1e-8,
         sprintf("Rosenberg, 10 items x 5 nodes: max rel |diff| vs numeric %.1e, vs iteminfo %.1e",
                 max(abs(fis$an - fis$num) / fis$an), max(abs(fis$an - fis$mirt) / fis$an)))

  Pn <- item_probs(pfits$rosenberg_selfesteem$nrm, gb$nodes)
  perm_diff <- max(sapply(Pn, function(P) {
    I0 <- poly_item_info(list(P), gb$w)[, "I"]
    max(sapply(1:5, function(r) abs(poly_item_info(list(P[, sample(ncol(P))]), gb$w)[, "I"] - I0)))
  }))
  record("P8 relabelling categories (nominal model) leaves I(theta;X_j) unchanged", perm_diff < 1e-12,
         sprintf("Rosenberg, 10 items x 5 random permutations: max |diff| %.1e", perm_diff))

  dres <- readRDS("vignettes/bits_per_item_data/bits_per_item_results.rds")
  sc <- dres$showcases$fitz_2024_numeracy$items
  P2 <- p_matrix(sc$a, sc$d, gb$nodes)
  Ps2 <- lapply(seq_len(ncol(P2)), function(j) cbind(1 - P2[, j], P2[, j]))
  stored <- filter(dres$summary, table == "fitz_2024_numeracy", model == "2PL")
  dif <- c(item_bits = max(abs(poly_item_info(Ps2, gb$w)[, "I"] - sc$bits)),
           I_X = abs(poly_info_exact(Ps2, gb$w)$I - stored$I_X),
           I_S = abs(poly_info_sum(Ps2, gb$w) - stored$I_S),
           H_X_theta = abs(poly_h_x_given_theta(Ps2, gb$w) - stored$H_X_given_theta))
  record("P9 K = 2: polytomous code reproduces the stored dichotomous results (fitz_2024_numeracy, 2PL)",
         all(dif < 1e-8), paste(sprintf("%s |diff| %.1e", names(dif), dif), collapse = "; "))

  nc <- filter(poly$summary, !converged)
  record("P10 all polytomous fits converged", nrow(nc) == 0,
         if (nrow(nc)) paste(nc$table, nc$model, collapse = "; ") else
           sprintf("%d table-models", nrow(poly$summary)))
}

checks <- do.call(rbind, out)
write.csv(checks, "vignettes/bits_per_item_data/checks_results.csv", row.names = FALSE)
message(sum(checks$pass), " of ", nrow(checks), " checks passed")
