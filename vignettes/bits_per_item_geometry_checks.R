# bits_per_item_geometry_checks.R
#
# Validation for bits_per_item_geometry_helpers.R (the "Two rulers" block).
# Sourced at the end of bits_per_item_checks.R, which supplies record() and the
# fitted check tables; it can also be run on its own:
#   Rscript vignettes/bits_per_item_geometry_checks.R   # from project root
# Tolerances are fixed here, before looking at the results. A failure is
# reported, never tolerated by widening it.

if (!exists("record")) {
  suppressMessages({ library(irw); library(mirt); library(dplyr) })
  source("vignettes/bits_per_item_helpers.R")
  out <- list()
  record <- function(check, pass, detail) {
    out[[length(out) + 1]] <<- data.frame(check = check, pass = pass, detail = detail)
    message(if (pass) "PASS " else "FAIL ", check, ": ", detail)
  }
  standalone <- TRUE
}
source("vignettes/bits_per_item_geometry_helpers.R")
set.seed(20261022)

# Showcase item parameters, from the stored results: no refit needed.
gres <- readRDS("vignettes/bits_per_item_data/bits_per_item_results.rds")
sc <- gres$items[gres$items$table == "fitz_2024_numeracy", ]
a_sc <- sc$a; d_sc <- sc$d

# G1. Invariance: A is unchanged by a smooth reparametrisation of theta.
# psi = exp(theta / 2) and omega = theta^3. On the new scale the metric is
# g*(u) = g(theta) (dtheta / du)^2, so sqrt(g*) du = sqrt(g) dtheta and the
# integral must be the same.
#
# The quadrature matters here. A is invariant; the trapezoid rule is not. Under
# omega = theta^3 the derivative vanishes at 0, so sqrt(g*) ~ |omega|^(-2/3)
# there: integrable, but with an infinite derivative, and a trapezoid rule on
# the transformed nodes is off by 1.7e-2 at the production grid and converges
# only slowly (7.0e-4 at 10x density, 6.8e-5 at 100x). That is a statement
# about the trapezoid rule, not about A, so the invariance itself is checked
# with adaptive quadrature on the new coordinate, split at the singular point.
geo <- geodesic_ability(a_sc, d_sc)
# Invariance is a statement about one interval and its image, so the check
# compares the geodesic distance across a fixed theta interval with the same
# distance computed on the transformed coordinate. The production range maps to
# psi ~ 5e8 and omega ~ 6e4, which no adaptive rule spans in one call.
TH_LO <- -6; TH_HI <- 6
quad <- function(f, lo, hi) integrate(f, lo, hi, subdivisions = 10000L,
                                      rel.tol = 1e-10)$value
A_theta <- quad(function(x) sqrt(test_info(a_sc, d_sc, x)), TH_LO, TH_HI)
reparam <- function(f, inv, fprime) {
  integrand <- function(u) {
    th <- inv(u)
    sqrt(test_info(a_sc, d_sc, th)) / abs(fprime(th))
  }
  u_lo <- f(TH_LO); u_hi <- f(TH_HI)
  parts <- if (u_lo < 0 && u_hi > 0) list(c(u_lo, 0), c(0, u_hi)) else list(c(u_lo, u_hi))
  sum(vapply(parts, function(p) quad(integrand, p[1], p[2]), numeric(1)))
}
A_psi   <- reparam(function(x) exp(x / 2), function(u) 2 * log(u), function(x) exp(x / 2) / 2)
A_omega <- reparam(function(x) x^3, function(u) sign(u) * abs(u)^(1 / 3), function(x) 3 * x^2)
rel <- c(psi = abs(A_psi - A_theta) / A_theta, omega = abs(A_omega - A_theta) / A_theta)
record("G1 geodesic ability is reparametrisation invariant (psi = e^(theta/2), omega = theta^3)",
       all(rel < 1e-6),
       sprintf(paste("fitz_2024_numeracy, geodesic distance across theta in",
                     "[%g, %g] = %.8f; relative diff psi %.1e, omega %.1e"),
               TH_LO, TH_HI, A_theta, rel["psi"], rel["omega"]))

# G2. Closed form: m Rasch items with all difficulties 0 give
# A(theta) = 2 sqrt(m) arctan(e^(theta/2)) and A(+inf) = pi sqrt(m).
g2 <- do.call(rbind, lapply(c(3, 5, 10), function(m) {
  ga <- geodesic_ability(rep(1, m), rep(0, m))
  exact <- 2 * sqrt(m) * atan(exp(ga$nodes / 2))
  data.frame(m = m,
             curve = max(abs(ga$A - (exact - exact[1]))),
             total = abs(ga$A_total - pi * sqrt(m)) / (pi * sqrt(m)))
}))
record("G2 Rasch closed form: A(theta) = 2 sqrt(m) arctan(e^(theta/2)), A(+inf) = pi sqrt(m)",
       all(g2$curve < 1e-3) && all(g2$total < 1e-3),
       paste(sprintf("m=%d max|curve diff| %.1e, A_total rel %.1e", g2$m, g2$curve, g2$total),
             collapse = "; "))

# G3. Paper check (Tuerlinckx & Robledo, 2026, Figure 4): Rasch geodesic
# ability for the paper's difficulty sets. Qualitative agreement means A is
# monotone increasing, bounded, A(+inf) grows like sqrt(m), and the
# equal-difficulty curve A_0 approximates A well for the symmetric sets and
# less well for the asymmetric ones. Check only; these curves are not plotted
# in the vignette.
paper_sets <- list(
  sym_3   = seq(-1, 1, by = 1),      sym_5  = seq(-1, 1, by = 0.5),
  sym_10  = seq(-1, 1, by = 2 / 9),
  asym_3  = seq(-1, 3, by = 2),      asym_5 = seq(-1, 3, by = 1),
  asym_10 = seq(-1, 3, by = 4 / 9))
g3 <- do.call(rbind, lapply(names(paper_sets), function(nm) {
  b <- paper_sets[[nm]]; m <- length(b)
  ga <- geodesic_ability(rep(1, m), -b)
  data.frame(set = nm, m = m, monotone = all(diff(ga$A) >= 0),
             A_total = ga$A_total, ratio_A0 = ga$A_total / (pi * sqrt(m)))
}))
record("G3 paper check: Rasch geodesic curves for Tuerlinckx & Robledo's difficulty sets",
       all(g3$monotone) && all(g3$ratio_A0 > 1) &&
         all(diff(g3$A_total[g3$set %in% c("sym_3", "sym_5", "sym_10")]) > 0),
       paste(sprintf("%s m=%d A_total %.3f (%.3f x pi sqrt(m))", g3$set, g3$m,
                     g3$A_total, g3$ratio_A0), collapse = "; "))

# G4. Grid convergence: doubling the density and widening the range by 5
# changes A(+inf) by under 0.1%.
base <- geodesic_ability(a_sc, d_sc, geo_grid(a_sc, d_sc))
fine <- geodesic_ability(a_sc, d_sc,
                        geo_grid(a_sc, d_sc, n_nodes = 2 * base$grid$n_nodes - 1,
                                 pad_u = base$grid$pad_u + 5))
g4 <- abs(fine$A_total - base$A_total) / base$A_total
record("G4 geodesic grid convergence (density x2, range + 5 changes A(+inf) < 0.1%)",
       g4 < 1e-3,
       sprintf(paste("fitz_2024_numeracy: A_total %.6f -> %.6f, relative change %.2e",
                     "(tail bound %.1e, grid vs adaptive %.1e)"),
               base$A_total, fine$A_total, g4, base$tail_share, base$grid_rel_err))

# G5. Blahut-Arimoto: the bounds bracket the reported capacity, a single item
# cannot carry more than 1 bit, and capacity is at least the delivered bits.
nodes1 <- capacity_grid(1, 0)
ba1 <- ba_capacity(log_channel_pattern(1, 0, nodes1))
ba5 <- ba_capacity(log_channel_pattern(a_sc[1:5], d_sc[1:5], capacity_grid(a_sc[1:5], d_sc[1:5])))
I_X5 <- info_pattern_exact(p_matrix(a_sc[1:5], d_sc[1:5], make_grid(1)$nodes), make_grid(1)$w)$I
record("G5 Blahut-Arimoto brackets capacity, one item <= 1 bit, capacity >= delivered bits",
       ba1$C_lower <= ba1$C_upper && ba1$C_upper <= 1 + 1e-6 &&
         ba5$C_lower <= ba5$C_upper && ba5$C_lower >= I_X5 - 1e-6,
       sprintf("single item C in [%.5f, %.5f]; 5 items C in [%.5f, %.5f] vs delivered %.5f",
               ba1$C_lower, ba1$C_upper, ba5$C_lower, ba5$C_upper, I_X5))

# G6. Sufficiency as geometry: under Rasch the sum score's geodesic length
# equals the full-data length; under 2PL it can only be shorter.
bR <- seq(-2, 2, length.out = 12)
fullR <- geodesic_ability(rep(1, 12), -bR)$A_total
sumR  <- sum_score_geodesic(rep(1, 12), -bR)$A_total
sc2 <- split(gres$items, gres$items$table)
g6 <- do.call(rbind, lapply(sc2[intersect(names(sc2), c("fitz_2024_numeracy",
                                                        "machivallianism_test_vcl", "frac20"))],
  function(it) data.frame(table = it$table[1],
                          full = geodesic_ability(it$a, it$d)$A_total,
                          sums = sum_score_geodesic(it$a, it$d)$A_total)))
record("G6 Rasch sum-score geodesic length = full-data length; 2PL sum score <= full data",
       abs(fullR - sumR) / fullR < 1e-8 && all(g6$sums <= g6$full * (1 + 1e-10)),
       sprintf("Rasch 12 items %.8f vs %.8f (rel %.1e); 2PL: %s", fullR, sumR,
               abs(fullR - sumR) / fullR,
               paste(sprintf("%s %.3f <= %.3f", g6$table, g6$sums, g6$full), collapse = ", ")))

# G7. Clarke-Barron: on a long simulated Rasch test the approximation
# h(A) - 0.5 log2(2 pi e) is within 0.05 bits of the exact I(theta; S).
b100 <- seq(-3, 3, length.out = 100)
a100 <- rep(1, 100); d100 <- -b100
pop <- make_grid(1)
I_exact <- info_sum(p_matrix(a100, d100, pop$nodes), pop$w)
I_cb <- geodesic_population(a100, d100, 1)$I_CB
record("G7 Clarke-Barron within 0.05 bits of exact I(theta; S), 100-item Rasch test",
       abs(I_cb - I_exact) < 0.05,
       sprintf("exact I(theta; S) %.4f bits, Clarke-Barron %.4f, diff %.4f",
               I_exact, I_cb, I_cb - I_exact))

# G8. Batch-wide invariants, if the geometry results exist.
gf <- "vignettes/bits_per_item_data/geometry_results.rds"
if (file.exists(gf)) {
  gs <- readRDS(gf)$summary
  bad_cap <- gs$table[gs$C_kind != "sum_score_lower_bound" & gs$I_X > gs$C_lower + 1e-6]
  bad_ss  <- gs$table[gs$A_sumscore_shortfall < -1e-8]
  bad_gap <- gs$table[!gs$C_converged]
  record("G8a capacity >= delivered bits for every table (exact-capacity tables)",
         length(bad_cap) == 0,
         sprintf("%d table-models; violations: %s", nrow(gs),
                 if (length(bad_cap)) paste(bad_cap, collapse = ",") else "none"))
  record("G8b sum-score geodesic length never exceeds the full-data length",
         length(bad_ss) == 0,
         if (length(bad_ss)) paste(bad_ss, collapse = ",") else "none")
  record("G8c Blahut-Arimoto reached the 1e-4 bit bound gap on every table",
         length(bad_gap) == 0,
         sprintf("max gap %.1e; not converged: %s", max(gs$C_gap),
                 if (length(bad_gap)) paste(bad_gap, collapse = ",") else "none"))
}

if (exists("standalone")) {
  checks <- do.call(rbind, out)
  message(sum(checks$pass), " of ", nrow(checks), " geometry checks passed")
}
