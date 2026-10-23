# bits_per_item_geometry.R
#
# "Two rulers: bits and distances" block of the bits-per-item vignette. For
# each table already in bits_per_item_results.rds it computes the Fisher-Rao
# geometry of the fitted model: geodesic ability A(theta), the total geodesic
# length A(+inf), the population on the geodesic scale, the Clarke-Barron
# approximation to I(theta; X), channel capacity by Blahut-Arimoto, and the
# sum score's geodesic length. For the showcases it also maps the 64 equally
# likely theta groups onto the geodesic scale and simulates the error of a
# person estimate on both scales.
#
# It reuses the stored 2PL item parameters, so the 2PL arm needs no Redivis
# fetch. The Rasch arm does: Rasch difficulties were never stored (only the
# latent SD), so a Rasch geometry means refetching and refitting each table.
# That arm is therefore opt-in:
#
#   Rscript vignettes/bits_per_item_geometry.R              # 2PL only
#   GEOM_RASCH=1 Rscript vignettes/bits_per_item_geometry.R # + Rasch (refits)
#   PILOT=1 GEOM_RASCH=1 Rscript vignettes/bits_per_item_geometry.R  # 5 tables
#
# Output: bits_per_item_data/geometry_results.rds (skip-if-exists), with
# per-table caches under bits_per_item_data/fits/geometry/ (gitignored).
# Run the checks first: Rscript vignettes/bits_per_item_checks.R

suppressMessages({
  library(irw)
  library(mirt)
  library(dplyr)
  library(purrr)
  library(furrr)
  library(tibble)
})
source("vignettes/bits_per_item_helpers.R")
source("vignettes/bits_per_item_geometry_helpers.R")
options(irw.itemtext_disclaimer = FALSE)

set.seed(20261022)

out_dir      <- "vignettes/bits_per_item_data"
main_file    <- file.path(out_dir, "bits_per_item_results.rds")
geo_file     <- file.path(out_dir, "geometry_results.rds")
cache_dir    <- file.path(out_dir, "fits", "geometry")
log_file     <- file.path(cache_dir, "failures.log")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

PILOT      <- Sys.getenv("PILOT") == "1"
WITH_RASCH <- Sys.getenv("GEOM_RASCH") == "1"
MAX_ENUM   <- 16      # full-pattern capacity up to this many items
BA_TOL     <- 1e-4    # bits, on the Blahut-Arimoto bound gap
N_CAP      <- 201     # capacity input grid points
N_SIM      <- 2000    # simulated respondents per true theta (showcases)
N_TRUE     <- 41      # true theta values (showcases)
SHOWCASES  <- c("fitz_2024_numeracy", "machivallianism_test_vcl", "frac20")
WORKERS    <- 3

if (file.exists(geo_file)) {
  message("Geometry results exist, skipping: ", geo_file)
} else {

main   <- readRDS(main_file)
items  <- main$items
summ2  <- filter(main$summary, model == "2PL")
summR  <- filter(main$summary, model == "Rasch")

tables <- summ2$table
if (PILOT) {
  # one showcase, one long test, and three spread over the item-count range
  by_n <- summ2[order(summ2$n_items), ]
  tables <- unique(c("fitz_2024_numeracy",
                     by_n$table[nrow(by_n)],
                     by_n$table[round(c(0.1, 0.45, 0.8) * nrow(by_n))]))
  message("PILOT: ", paste(tables, collapse = ", "))
}

# ------------------------------------------------------------------------------
# Rasch parameters (refit; cached per table)
# ------------------------------------------------------------------------------

rasch_pars <- function(table_name) {
  f <- file.path(cache_dir, paste0(table_name, "_rasch.rds"))
  if (file.exists(f)) return(readRDS(f))
  t0 <- Sys.time()
  prep <- prepare_resp(irw_fetch(table_name), max_n = main$settings$MAX_N,
                       min_coverage = main$settings$MIN_COVERAGE)
  t_fetch <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  t1 <- Sys.time()
  fit <- mirt::mirt(prep$resp, 1, itemtype = "Rasch", verbose = FALSE)
  cf <- mirt::coef(fit, simplify = TRUE)
  res <- list(a = cf$items[, "a1"], d = cf$items[, "d"], sd = sqrt(cf$cov[1, 1]),
              converged = mirt::extract.mirt(fit, "converged"),
              secs_fetch = t_fetch,
              secs_fit = as.numeric(difftime(Sys.time(), t1, units = "secs")))
  saveRDS(res, f)
  res
}

# ------------------------------------------------------------------------------
# One model's geometry
# ------------------------------------------------------------------------------

model_geometry <- function(a, d, latent_sd, model, I_X_stored, n_items) {
  grid <- geo_grid(a, d)
  geo  <- geodesic_ability(a, d, grid)
  ram  <- ramsay_length(a, d, grid)
  pop  <- geodesic_population(a, d, latent_sd, geo = geo,
                              pop = make_grid(latent_sd))
  ss   <- sum_score_geodesic(a, d, grid)

  # Capacity. Rasch: the sum score is sufficient, so its channel is exact.
  # 2PL: full patterns up to MAX_ENUM items, otherwise the sum-score channel,
  # which is a lower bound C_S <= C.
  cap_nodes <- capacity_grid(a, d, n_nodes = N_CAP)
  cap_kind <- if (model == "Rasch") "sum_score_sufficient" else
    if (n_items <= MAX_ENUM) "pattern_exact" else "sum_score_lower_bound"
  logW <- if (cap_kind == "pattern_exact") log_channel_pattern(a, d, cap_nodes) else
    log_channel_sum(a, d, cap_nodes)
  t0 <- Sys.time()
  ba <- ba_capacity(logW, tol_bits = BA_TOL)
  ba_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  row <- tibble(
    model = model, n_items = n_items, latent_sd = latent_sd,
    A_total = geo$A_total, A_total_grid = geo$A_total_grid,
    A_tail_share = geo$tail_share, A_grid_rel_err = geo$grid_rel_err,
    L_ramsay = ram$L_total,
    A_pop_span = pop$A_pop_span,
    h_theta = pop$h_theta, E_log_sqrt_g = pop$E_log_sqrt_g, h_A = pop$h_A,
    sd_A = pop$sd_A, I_CB = pop$I_CB, I_X = I_X_stored,
    CB_diff = pop$I_CB - I_X_stored,
    CB_ratio = pop$I_CB / I_X_stored,
    C_lower = ba$C_lower, C_upper = ba$C_upper, C_gap = ba$gap_bits,
    C_converged = ba$converged, C_iterations = ba$iterations,
    C_support = ba$n_support, C_kind = cap_kind, C_secs = ba_secs,
    C_asymptotic = pop$C_asymptotic,
    match_ratio = I_X_stored / ba$C_lower,
    A_total_sumscore = ss$A_total,
    A_sumscore_shortfall = geo$A_total - ss$A_total,
    A_sumscore_prop = 1 - ss$A_total / geo$A_total
  )
  # The curves are large (one fine local grid per item) and no corpus figure
  # reads them, so they stay out of the saved object; the showcases get their
  # own curves below.
  list(row = row, capacity_input = tibble(theta = cap_nodes, q = ba$q))
}

one_table <- function(table_name) {
  f <- file.path(cache_dir, paste0(table_name, ".rds"))
  if (file.exists(f)) return(invisible(NULL))
  suppressMessages(library(irw))
  source("vignettes/bits_per_item_helpers.R", local = TRUE)
  source("vignettes/bits_per_item_geometry_helpers.R", local = TRUE)
  set.seed(sum(utf8ToInt(table_name)))
  res <- tryCatch({
    it <- items[items$table == table_name, ]
    s2 <- summ2[summ2$table == table_name, ]
    g2 <- model_geometry(it$a, it$d, 1, "2PL", s2$I_X, nrow(it))
    gR <- NULL
    if (WITH_RASCH) {
      rp <- rasch_pars(table_name)
      sR <- summR[summR$table == table_name, ]
      gR <- model_geometry(rp$a, rp$d, rp$sd, "Rasch", sR$I_X, length(rp$a))
      gR$row$rasch_secs_fetch <- rp$secs_fetch
      gR$row$rasch_secs_fit   <- rp$secs_fit
    }
    list(table = table_name,
         summary = bind_rows(g2$row, if (!is.null(gR)) gR$row),
         capacity_input = bind_rows(mutate(g2$capacity_input, model = "2PL"),
                                    if (!is.null(gR)) mutate(gR$capacity_input, model = "Rasch")))
  }, error = function(e) {
    cat(format(Sys.time()), table_name, conditionMessage(e), "\n",
        file = log_file, append = TRUE)
    NULL
  })
  if (!is.null(res)) saveRDS(res, f)
  invisible(NULL)
}

t_batch <- Sys.time()
if (WORKERS > 1 && length(tables) > 1) {
  plan(multisession, workers = WORKERS)
  future_map(tables, one_table, .options = furrr_options(seed = TRUE))
  plan(sequential)
} else {
  walk(tables, one_table)
}
batch_secs <- as.numeric(difftime(Sys.time(), t_batch, units = "secs"))

cached <- compact(map(tables, function(t) {
  f <- file.path(cache_dir, paste0(t, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}))
names(cached) <- map_chr(cached, "table")

# ------------------------------------------------------------------------------
# Showcase quantities (2PL): groups on the geodesic scale, simulated error on
# both scales, and the capacity-achieving input against the population.
# ------------------------------------------------------------------------------

showcase_geometry <- function(table_name) {
  it <- items[items$table == table_name, ]
  a <- it$a; d <- it$d; n <- nrow(it)
  geo <- geodesic_ability(a, d)
  A_at <- function(th) approx(geo$nodes, geo$A, xout = th, rule = 2)$y

  # 9. the vignette's 64 equally likely theta groups on the geodesic scale
  bnd <- group_bounds(main$settings$K_GROUPS)
  bnd_fin <- pmax(pmin(bnd, max(geo$nodes)), min(geo$nodes))
  groups <- tibble(k = seq_along(bnd), theta = bnd, A = A_at(bnd_fin)) |>
    mutate(width_A = c(diff(A), NA))

  # 10. empirical error on both scales: EAP and WLE at N_TRUE true values
  sg <- seq(-10, 10, length.out = 801)                  # scoring grid
  Pg <- p_matrix(a, d, sg)
  lP <- log(Pg); lQ <- log(1 - Pg)
  prior <- dnorm(sg); prior <- prior / sum(prior)
  half_log_g <- 0.5 * log(pmax(test_info(a, d, sg), 1e-300))   # Warm's correction
  A_sg <- A_at(sg)
  true_theta <- seq(-3, 3, length.out = N_TRUE)
  Pt <- p_matrix(a, d, true_theta)
  err <- map_dfr(seq_along(true_theta), function(i) {
    X <- (matrix(runif(N_SIM * n), N_SIM, n) < matrix(Pt[i, ], N_SIM, n, byrow = TRUE)) * 1
    ll <- X %*% t(lP) + (1 - X) %*% t(lQ)               # respondents x scoring grid
    mx <- apply(ll, 1, max)
    post <- exp(ll - mx) * matrix(prior, N_SIM, length(sg), byrow = TRUE)
    post <- post / rowSums(post)
    eap <- as.vector(post %*% sg)
    wle <- sg[apply(ll + matrix(half_log_g, N_SIM, length(sg), byrow = TRUE), 1, which.max)]
    tibble(true_theta = true_theta[i], true_A = A_at(true_theta[i]),
           sd_theta_eap = sd(eap), sd_A_eap = sd(A_at(eap)),
           sd_theta_wle = sd(wle), sd_A_wle = sd(A_at(wle)),
           bias_theta_eap = mean(eap) - true_theta[i],
           bias_A_eap = mean(A_at(eap)) - A_at(true_theta[i]))
  })

  # 12. capacity-achieving input vs the fitted population, on both scales
  cap <- cached[[table_name]]$capacity_input |> filter(model == "2PL")
  pop <- make_grid(1)
  dist <- bind_rows(
    tibble(which = "capacity", theta = cap$theta, p = cap$q, A = A_at(cap$theta)),
    tibble(which = "population", theta = pop$nodes, p = pop$w, A = A_at(pop$nodes))
  )

  list(groups = groups, error = err, distributions = dist,
       curves = tibble(theta = geo$nodes, A = geo$A, g = geo$g,
                       L_ramsay = ramsay_length(a, d, geo$grid)$L))
}

sc_tables <- intersect(SHOWCASES, names(cached))
showcases <- setNames(lapply(sc_tables, showcase_geometry), sc_tables)

# ------------------------------------------------------------------------------
# Polytomous arm: only if the polytomous results exist (items 1-4, 6, 7).
# ------------------------------------------------------------------------------

poly_file <- file.path(out_dir, "poly_results.rds")
poly_geometry <- NULL
if (file.exists(poly_file)) {
  message("Polytomous results found; the polytomous geometry arm is a separate",
          " step and is not computed here (see the pilot report).")
}

results <- list(
  summary = bind_rows(map(cached, function(x)
    mutate(x$summary, table = x$table, .before = 1))),
  capacity_inputs = bind_rows(map(cached, function(x)
    mutate(x$capacity_input, table = x$table, .before = 1))),
  showcases = showcases,
  failures = if (file.exists(log_file)) readLines(log_file) else character(0),
  tables_attempted = tables,
  settings = list(MAX_ENUM = MAX_ENUM, BA_TOL = BA_TOL, N_CAP = N_CAP,
                  N_SIM = N_SIM, N_TRUE = N_TRUE, WITH_RASCH = WITH_RASCH,
                  geo_grid = paste("range [min(b - 20/a), max(b + 20/a)]; curves on the",
                                   "union of 801 nodes per item in its own units;",
                                   "totals by adaptive quadrature split at every item"),
                  scoring_grid = "801 nodes on [-10, 10]",
                  workers = WORKERS),
  pilot = PILOT,
  batch_secs = batch_secs,
  date_run = Sys.Date(),
  session = sessionInfo()
)
saveRDS(results, geo_file)
message("Saved ", geo_file, ": ", length(cached), " of ", length(tables),
        " tables in ", round(batch_secs), "s")

}
