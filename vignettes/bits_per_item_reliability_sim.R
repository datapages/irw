# bits_per_item_reliability_sim.R
#
# Reliability vs bits, in the abstract, for the "Reliability in bits" section
# of bits_per_item.qmd. No IRW data: synthetic 2PL tests with known item
# parameters, theta ~ N(0, 1). For each test we compute the exact information
# I(theta; X) (enumeration for n <= 16, Monte Carlo above) and the marginal
# reliability, then compare with the Gaussian value -1/2 log2(1 - rho), which is
# exact only when theta and the score are jointly normal.
#
# Three kinds of test, each at several lengths with 10 random draws of items:
#   moderate     a ~ lognormal(log 1.2, 0.3), b ~ N(0, 1)
#   sharp items  as moderate, but 3 items get a = 6 (near-Guttman items)
#   one tail     as moderate, but b ~ N(1.5, 0.5): items bunched at the high end
#
# Sourced at the end of bits_per_item_compute.R, or run on its own:
#   Rscript vignettes/bits_per_item_reliability_sim.R   # from project root
# Output: bits_per_item_data/reliability_sim_results.rds (skipped if present)

if (!exists("lord_wingersky")) source("vignettes/bits_per_item_helpers.R")

sim_file <- "vignettes/bits_per_item_data/reliability_sim_results.rds"

if (file.exists(sim_file)) {
  message("Reliability simulation exists, skipping: ", sim_file)
} else {
  set.seed(20261005)
  g <- make_grid(1)
  lengths <- c(5, 10, 20, 40, 60)
  kinds <- c("moderate", "sharp items", "one tail")
  draw_items <- function(n, kind) {
    a <- rlnorm(n, log(1.2), 0.3)
    b <- if (kind == "one tail") rnorm(n, 1.5, 0.5) else rnorm(n)
    if (kind == "sharp items") a[seq_len(min(3, n))] <- 6
    list(a = a, d = -a * b)
  }
  rows <- list()
  for (kind in kinds) for (n in lengths) for (r in 1:10) {
    it <- draw_items(n, kind)
    P <- p_matrix(it$a, it$d, g$nodes)
    ip <- info_pattern(P, g$w, target_se = 0.005)
    rho <- marginal_rel(it$a, P, g$nodes, g$w)
    rows[[length(rows) + 1]] <- data.frame(kind = kind, n_items = n, rep = r, I_X = ip$I, I_X_se = ip$se,
                                           rho = rho, gauss = gauss_bits(rho))
  }
  sim <- do.call(rbind, rows)
  saveRDS(list(sim = sim, date_run = Sys.Date(),
               design = "2PL, theta ~ N(0,1); a ~ lognormal(log 1.2, 0.3), b ~ N(0,1); sharp: 3 items a = 6; one tail: b ~ N(1.5, 0.5); 10 draws per length"),
          sim_file)
  message("Saved ", sim_file)
}
