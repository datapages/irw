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

checks <- do.call(rbind, out)
write.csv(checks, "vignettes/bits_per_item_data/checks_results.csv", row.names = FALSE)
message(sum(checks$pass), " of ", nrow(checks), " checks passed")
