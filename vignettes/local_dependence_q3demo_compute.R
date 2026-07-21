# local_dependence_q3demo_compute.R
#
# Small, self-contained simulation for the "how Q3 detects local dependence"
# demo embedded in local_dependence.qmd. Unlike local_dependence_compute.R,
# this does not touch real IRW data -- it simulates a tiny 8-item 2PL test
# where items 7 and 8 are constructed to share an extra "linked" factor at
# a strength rho, on top of the usual latent trait every item loads on.
# At rho = 0 the linked pair behaves like any other pair (no local
# dependence by construction); as rho increases, more of their variance
# comes from the shared extra factor instead of the modeled trait, so a
# standard unidimensional fit should increasingly flag exactly that pair
# via Q3 -- and only that pair, since every other item is unaffected.
#
# Fits one 2PL model per rho grid point (10 points, J=8, N=2000 -- each fit
# takes a couple of seconds, so the whole grid runs in well under a minute).
#
# Output: local_dependence_data/q3_demo_results.rds
#
# Usage:
#   Rscript vignettes/local_dependence_q3demo_compute.R   # from project root

library(mirt)
library(dplyr)
library(purrr)
library(tibble)

BASE_SEED <- 20260722

out_dir <- "vignettes/local_dependence_data"

N          <- 10000       # matches MAX_N in local_dependence_compute.R
J          <- 8
LINKED     <- c(7, 8)     # the pair constructed to share the extra factor
A          <- 2.0         # fixed discrimination, all items
B          <- seq(-1.5, 1.5, length.out = J)   # spread of difficulties
RHO_GRID   <- seq(0, 1, by = 0.1)
Q3_THRESHOLD <- 0.2       # matches the threshold used for the real batch

item_labels <- paste0("item_", seq_len(J))

simulate_and_fit <- function(rho) {
  # Fixed per-rho seed (not one global seed for the whole grid) so each
  # point is reproducible on its own and the curve is stable across reruns.
  set.seed(BASE_SEED + round(rho * 10))
  theta <- rnorm(N)
  nu    <- rnorm(N)   # extra factor, shared only by the linked pair

  eta <- matrix(theta, nrow = N, ncol = J)
  eta[, LINKED] <- sqrt(1 - rho) * theta + sqrt(rho) * nu

  p <- plogis(sweep(A * eta, 2, A * B, "-"))
  resp <- matrix(rbinom(N * J, 1, p), nrow = N, ncol = J)
  colnames(resp) <- item_labels
  resp <- as.data.frame(resp)

  fit <- mirt(resp, 1, itemtype = "2PL", verbose = FALSE,
              technical = list(NCYCLES = 2000))

  q3 <- residuals(fit, type = "Q3", verbose = FALSE)
  dimnames(q3) <- list(item_labels, item_labels)

  q3_long <- as.data.frame(as.table(q3)) %>%
    rename(item1 = Var1, item2 = Var2, q3 = Freq) %>%
    mutate(rho = rho)

  q3_upper <- q3[upper.tri(q3)]
  n_pairs   <- length(q3_upper)
  n_flagged <- sum(abs(q3_upper) > Q3_THRESHOLD)

  list(
    long = q3_long,
    summary = tibble(
      rho             = rho,
      n_pairs         = n_pairs,
      n_flagged       = n_flagged,
      prop_flagged    = n_flagged / n_pairs,
      max_abs_q3      = max(abs(q3_upper)),
      mean_abs_q3     = mean(abs(q3_upper)),
      linked_pair_q3  = q3[item_labels[LINKED[1]], item_labels[LINKED[2]]]
    )
  )
}

message("Fitting ", length(RHO_GRID), " demo models (J = ", J, ", N = ", N, ")...")
results <- map(RHO_GRID, function(r) {
  message("  rho = ", r)
  simulate_and_fit(r)
})

q3_demo_long    <- bind_rows(map(results, "long"))
q3_demo_summary <- bind_rows(map(results, "summary"))

saveRDS(
  list(
    long        = q3_demo_long,
    summary     = q3_demo_summary,
    item_labels = item_labels,
    linked_pair = item_labels[LINKED],
    rho_grid    = RHO_GRID,
    threshold   = Q3_THRESHOLD,
    date_run    = Sys.Date()
  ),
  file = file.path(out_dir, "q3_demo_results.rds")
)

message("Saved to ", out_dir, "/q3_demo_results.rds")
