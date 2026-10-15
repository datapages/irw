# bits_per_item_poly_compute.R
#
# The polytomous appendix of the bits-per-item vignette ("More than right and
# wrong"). Three hand-picked tables, not a corpus (scout_report_poly.md):
# Rosenberg self-esteem (the showcase), BFI extraversion and the GAD-7. For
# each it fits PCM, GPCM, GRM and the nominal response model and computes, in
# bits on a standardised latent scale z ~ N(0, 1): per-item I(theta; X_j)
# against log2 K_j and the observed category entropy H(X_j); H(X | theta);
# the sum-score information I(theta; S) (polytomous Lord-Wingersky); the
# full-pattern I(theta; X) (exact for up to 2^20 patterns, Monte Carlo above);
# the sufficiency gap I(theta; X | S); every merge of adjacent categories for
# every item under the three ordered models, without refitting; the test with
# every item dichotomised (GPCM, exact over 2^n patterns); ordered vs nominal
# (bits, fit, and whether the nominal scoring order matches the labels);
# efficiency and reliability in bits.
#
# Decisions (2026-10-07): negatively keyed items are reversed before fitting
# (Rosenberg 3, 5, 8, 9, 10; BFI E2, E4, E6, E8, E10; IRW stores them
# unreversed); every category is kept, GAD-7's sparse top category included;
# no slope prior; the enumeration cutoff is 2^20 so the showcase is exact.
#
# Showcase extras: item text, P(X_j = k | group) for 64 equally likely groups
# (GPCM), the GPCM weighted-score check, a one-off equal-slope GRM refit, and
# the prior sweep of the sufficiency gap for PCM, GPCM and GRM (the 16 priors
# of bits_per_item_sufficiency.R, by enumeration over all 4^10 patterns).
#
# Output: bits_per_item_data/poly_results.rds
#
# Usage:
#   Rscript vignettes/bits_per_item_poly_compute.R   # from project root
# Checks: the "P" block of bits_per_item_checks.R.

suppressMessages({
  library(irw)
  library(mirt)
  library(dplyr)
  library(purrr)
  library(furrr)
})
source("vignettes/bits_per_item_helpers.R")
source("vignettes/bits_per_item_poly_helpers.R")
options(irw.itemtext_disclaimer = FALSE)

out_dir      <- "vignettes/bits_per_item_data"
fits_dir     <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
results_file <- file.path(out_dir, "poly_results.rds")
log_file     <- file.path(fits_dir, "poly_failures.log")

MAX_N    <- 10000
MC_SE    <- 0.005
K_GROUPS <- 6
WORKERS  <- 2
TABLES   <- c("rosenberg_selfesteem", "bfi_goldberg_1992_extraversion", "su_2024_gad7")
SHOWCASE <- "rosenberg_selfesteem"
REVERSE  <- list(rosenberg_selfesteem = c("3", "5", "8", "9", "10"),
                 bfi_goldberg_1992_extraversion = c("E2", "E4", "E6", "E8", "E10"),
                 su_2024_gad7 = character(0))
MODELS   <- c(PCM = "Rasch", GPCM = "gpcm", GRM = "graded", NRM = "nominal")
ORDERED  <- c("PCM", "GPCM", "GRM")

# Slope and threshold spread on the standardised scale (theta = sd * z; the
# PCM's slope is fixed at 1 on theta, so sd on z). Thresholds: step
# difficulties for PCM/GPCM, category boundaries for the GRM.
item_pars <- function(fit, model, sd) {
  cf <- coef(fit, IRTpars = FALSE, simplify = TRUE)$items
  a <- cf[, "a1"] * sd
  d <- cf[, grepl("^d[0-9]", colnames(cf)), drop = FALSE]
  b <- switch(model,
    PCM = , GPCM = { if (!"d0" %in% colnames(d)) d <- cbind(d0 = 0, d); -t(apply(d, 1, diff)) / a },
    GRM = -d / a,
    NRM = NULL)
  data.frame(slope = unname(a),
             threshold_spread = if (is.null(b)) NA_real_ else unname(apply(b, 1, function(r) diff(range(r)))))
}

nominal_order <- function(fit) {
  cf <- coef(fit, IRTpars = FALSE, simplify = TRUE)$items
  ak <- cf[, grepl("^ak", colnames(cf)), drop = FALSE] * cf[, "a1"]
  data.frame(nominal_scoring = apply(round(ak, 3), 1, paste, collapse = ","),
             order_matches = apply(ak, 1, function(r) all(diff(r) > 0)))
}

process_poly_table <- function(table_name) {
  t0 <- Sys.time()
  set.seed(sum(utf8ToInt(table_name)))
  prep <- prepare_resp_poly(irw_fetch(table_name), reverse = REVERSE[[table_name]],
                            max_n = MAX_N, seed = sum(utf8ToInt(table_name)))
  resp <- prep$resp; K <- prep$K; n <- ncol(resp)
  fits <- lapply(MODELS, function(it) mirt(resp, 1, itemtype = it, verbose = FALSE))
  g <- make_grid(1)
  H_obs <- vapply(resp, function(x) entropy(tabulate(x + 1, max(K)) / sum(!is.na(x))), numeric(1))

  per_model <- lapply(names(MODELS), function(m) {
    fit <- fits[[m]]; sd <- latent_sd(fit)
    Ps <- item_probs(fit, g$nodes, sd)
    ii <- poly_item_info(Ps, g$w)
    pat <- poly_info_pattern(Ps, g$w, target_se = MC_SE)
    I_S <- pat$I_S
    H_XT <- poly_h_x_given_theta(Ps, g$w)
    rho <- poly_marginal_rel(fit, g$nodes, g$w, sd)
    summ <- data.frame(table = table_name, model = m, converged = extract.mirt(fit, "converged"),
                       latent_sd = sd, n_items = n, sum_log2K = sum(log2(K)),
                       H_X_given_theta = H_XT, I_S = I_S, I_X = pat$I, I_X_se = pat$se,
                       I_X_method = pat$method, I_X_draws = pat$draws, gap = pat$gap, gap_se = pat$gap_se,
                       efficiency = pat$I / sum(log2(K)), bits_per_item = pat$I / n,
                       rho_marginal = rho, bits_rho_marginal = gauss_bits(rho),
                       logLik = extract.mirt(fit, "logLik"), AIC = extract.mirt(fit, "AIC"),
                       BIC = extract.mirt(fit, "BIC"), n_par = extract.mirt(fit, "nest"))
    items <- data.frame(table = table_name, model = m, item = colnames(resp), K = unname(K),
                        log2K = log2(unname(K)), H_obs = unname(H_obs),
                        H_model = ii[, "H_model"], I = ii[, "I"], item_pars(fit, m, sd))
    if (m == "NRM") items <- cbind(items, nominal_order(fit))
    collapses <- if (m %in% ORDERED) bind_rows(lapply(seq_len(n), function(j) {
      cl <- item_collapses(Ps[[j]], g$w)
      mc <- median_cut(resp[[j]], K[j])
      cbind(table = table_name, model = m, item = colnames(resp)[j], K = K[j],
            I_full = ii[j, "I"], cl, is_median_split = cl$cuts == as.character(mc))
    })) else NULL
    list(summary = summ, items = items, collapses = collapses, Ps = Ps)
  })
  names(per_model) <- names(MODELS)

  # Test level: every item dichotomised at its best single split, and at its
  # median split, under the fitted GPCM; exact over 2^n patterns.
  cl <- per_model$GPCM$collapses
  dich <- lapply(c(best = "best", median = "median"), function(rule) {
    cuts <- vapply(colnames(resp), function(it) {
      x <- cl[cl$item == it & cl$k == 2, ]
      as.integer(if (rule == "best") x$cuts[which.max(x$I)] else x$cuts[x$is_median_split])
    }, integer(1))
    Pd <- Map(collapse_probs, per_model$GPCM$Ps, cuts)
    data.frame(table = table_name, rule = rule, cuts = paste(cuts, collapse = ","),
               I_dich = poly_info_exact(Pd, g$w)$I, I_X = per_model$GPCM$summary$I_X)
  })

  list(table = table_name, info = prep$info, K = K, n_items = n,
       summary = bind_rows(map(per_model, "summary")),
       items = bind_rows(map(per_model, "items")),
       collapses = bind_rows(map(per_model, "collapses")),
       dichotomised = bind_rows(dich),
       secs = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

fit_to_disk <- function(table_name) {
  f <- file.path(fits_dir, paste0("poly_", table_name, ".rds"))
  if (file.exists(f)) return(invisible(NULL))
  suppressMessages({ library(irw); library(mirt); library(dplyr); library(purrr) })
  # multisession workers start clean; process_poly_table() looks its helpers up globally
  source("vignettes/bits_per_item_helpers.R")
  source("vignettes/bits_per_item_poly_helpers.R")
  res <- tryCatch(process_poly_table(table_name), error = function(e) {
    cat(format(Sys.time()), table_name, conditionMessage(e), "\n", file = log_file, append = TRUE)
    NULL
  })
  if (!is.null(res)) saveRDS(res, f)
  invisible(NULL)
}

plan(multisession, workers = WORKERS)
future_map(TABLES, fit_to_disk, .options = furrr_options(seed = TRUE,
  globals = c("process_poly_table", "item_pars", "nominal_order", "MODELS", "ORDERED",
              "REVERSE", "MAX_N", "MC_SE", "fits_dir", "log_file")))
plan(sequential)

# ==============================================================================
# Showcase (Rosenberg): item text, 64-group probabilities, sufficiency checks,
# prior sweep. Cached on its own.
# ==============================================================================

showcase_file <- file.path(fits_dir, "poly_showcase.rds")
if (!file.exists(showcase_file)) {
  nm <- SHOWCASE
  set.seed(sum(utf8ToInt(nm)))
  prep <- prepare_resp_poly(irw_fetch(nm), reverse = REVERSE[[nm]], max_n = MAX_N,
                            seed = sum(utf8ToInt(nm)))
  resp <- prep$resp
  fits <- lapply(MODELS[ORDERED], function(it) mirt(resp, 1, itemtype = it, verbose = FALSE))
  sds <- vapply(fits, latent_sd, numeric(1))

  it <- tryCatch(as.data.frame(irw_itemtext(nm)), error = function(e) NULL)
  key <- sub("^item_", "", colnames(resp))
  text <- rep(NA_character_, length(key))
  if (!is.null(it)) { one <- it[!duplicated(it$item), ]; text <- one$item_text[match(key, one$item)] }

  # P(X_j = k | group), 64 equally likely groups, GPCM: groups x items x K.
  fg <- fine_grid(2048)
  Pf <- item_probs(fits$GPCM, fg$nodes, 1)
  group_probs <- simplify2array(lapply(Pf, function(P) t(bin_cols(t(P), K_GROUPS))))  # groups x K x items
  group_probs <- aperm(group_probs, c(1, 3, 2))                                       # groups x items x K

  # GPCM weighted score T = sum a_j x_j (exact, all 4^10 patterns, base grid).
  g <- make_grid(1)
  a_gpcm <- coef(fits$GPCM, IRTpars = FALSE, simplify = TRUE)$items[, "a1"]
  wt <- poly_info_exact(item_probs(fits$GPCM, g$nodes), g$w, T_fun = function(X) as.vector(X %*% a_gpcm))
  wt_I_S <- poly_info_sum(item_probs(fits$GPCM, g$nodes), g$w)

  # GRM with one common slope (one-off check; not part of the table runs).
  ni <- ncol(resp)
  fit_grm_eq <- mirt(resp, mirt.model(paste0("F = 1-", ni, "\nCONSTRAIN = (1-", ni, ", a1)")),
                     itemtype = "graded", verbose = FALSE)
  Pe <- item_probs(fit_grm_eq, g$nodes)
  grm_eq <- data.frame(I_X = poly_info_exact(Pe, g$w)$I, I_S = poly_info_sum(Pe, g$w),
                       slope = coef(fit_grm_eq, simplify = TRUE)$items[1, "a1"],
                       converged = extract.mirt(fit_grm_eq, "converged"))
  grm_eq$gap <- grm_eq$I_X - grm_eq$I_S

  # Prior sweep: the 16 priors of the sufficiency section, item parameters
  # held fixed, latent scale standardised.
  priors <- c(
    unlist(lapply(c(-1.5, -0.5, 0, 0.5, 1.5), function(m) lapply(c(0.5, 1, 2), function(sd) {
      list(label = sprintf("N(%g, %g²)", m, sd), mean = m, sd = sd, kind = "normal",
           dens = local({ m <- m; sd <- sd; function(x) dnorm(x, m, sd) }))
    })), recursive = FALSE),
    list(list(label = "bimodal: ½ N(−1.5, 0.5²) + ½ N(1.5, 0.5²)", mean = 0, sd = NA,
              kind = "mixture", dens = function(x) 0.5 * dnorm(x, -1.5, 0.5) + 0.5 * dnorm(x, 1.5, 0.5)))
  )
  sweep_one <- function(pr) {
    gp <- prior_grid(pr$dens)
    bind_rows(lapply(ORDERED, function(m) {
      Ps <- item_probs(fits[[m]], gp$nodes, sds[[m]])
      I_X <- poly_info_exact(Ps, gp$w)$I; I_S <- poly_info_sum(Ps, gp$w)
      data.frame(table = nm, model = m, prior = pr$label, prior_mean = pr$mean, prior_sd = pr$sd,
                 prior_kind = pr$kind, I_X = I_X, I_S = I_S, I_X_given_S = I_X - I_S)
    }))
  }
  plan(multisession, workers = WORKERS)
  sweep <- bind_rows(future_map(priors, sweep_one, .options = furrr_options(seed = TRUE,
    packages = c("mirt", "dplyr"), globals = c("sweep_one", "fits", "sds", "ORDERED", "nm", "prior_grid",
                                    "item_probs", "poly_info_exact", "poly_info_sum", "pattern_grid",
                                    "pattern_logp", "poly_h_x_given_theta", "row_entropy", "entropy",
                                    "log2s", "poly_lord_wingersky", "info_from_L"))))
  plan(sequential)

  saveRDS(list(
    table = nm, items = data.frame(item = colnames(resp), label = key, text = text,
                                   reversed = key %in% REVERSE[[nm]]),
    category_labels = c("strongly disagree", "disagree", "agree", "strongly agree"),
    group_bounds = group_bounds(K_GROUPS), group_probs = group_probs,
    weighted_score = data.frame(I_T = wt$I_T, I_X = wt$I, I_S = wt_I_S, n_levels = wt$n_T_levels,
                                n_patterns = wt$n_patterns),
    grm_equal_slopes = grm_eq,
    sweep = sweep,
    priors = lapply(priors, function(p) p[c("label", "mean", "sd", "kind")])
  ), showcase_file)
}

# ==============================================================================
# Combine
# ==============================================================================

ok <- compact(setNames(lapply(TABLES, function(t) {
  f <- file.path(fits_dir, paste0("poly_", t, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}), TABLES))

results <- list(
  summary      = bind_rows(map(ok, "summary")),
  items        = bind_rows(map(ok, "items")),
  collapses    = bind_rows(map(ok, "collapses")),
  dichotomised = bind_rows(map(ok, "dichotomised")),
  info         = map(ok, "info"),
  showcase     = readRDS(showcase_file),
  failures     = if (file.exists(log_file)) readLines(log_file) else character(0),
  tables_attempted = TABLES,
  settings     = list(MAX_N = MAX_N, MC_SE = MC_SE, K_GROUPS = K_GROUPS, MAX_ENUM_PATTERNS = MAX_ENUM_PATTERNS,
                      reverse = REVERSE, sparse_rule = "keep every category; flag items with a category under 1%",
                      slope_prior = "none", grid = "81 nodes, +/- 6 SD of z ~ N(0, 1)",
                      sweep_grid = "401 nodes on [-10, 10], weights from the prior density"),
  date_run     = Sys.Date(),
  session      = sessionInfo()
)
saveRDS(results, results_file)
message("Saved ", results_file, ": ", length(ok), " of ", length(TABLES), " tables")

bib <- file.path(out_dir, "references.bib")
add_bib <- function(key, entry) if (file.exists(bib) && !any(grepl(key, readLines(bib)))) cat(entry, file = bib, append = TRUE)
add_bib("thissen1995item", paste0(
  "\n@article{thissen1995item,\n  author = {Thissen, David and Pommerich, Mary and Billeaud, Kathleen and Williams, Valerie S. L.},\n",
  "  title = {Item Response Theory for Scores on Tests Including Polytomous Items with Ordered Responses},\n",
  "  journal = {Applied Psychological Measurement},\n  volume = {19},\n  number = {1},\n  pages = {39--49},\n",
  "  year = {1995},\n  doi = {10.1177/014662169501900105}\n}\n"))
add_bib("rosenberg1965society", paste0(
  "\n@book{rosenberg1965society,\n  author = {Rosenberg, Morris},\n  title = {Society and the Adolescent Self-Image},\n",
  "  publisher = {Princeton University Press},\n  address = {Princeton, NJ},\n  year = {1965}\n}\n"))
add_bib("andersen1977sufficient", paste0(
  "\n@article{andersen1977sufficient,\n  author = {Andersen, Erling B.},\n  title = {Sufficient Statistics and Latent Trait Models},\n",
  "  journal = {Psychometrika},\n  volume = {42},\n  number = {1},\n  pages = {69--81},\n  year = {1977},\n",
  "  doi = {10.1007/BF02293746}\n}\n"))
# Dataset citations for the three tables, appended to the vignette's bib.
tmp_bib <- tempfile(fileext = ".bib")
irw_save_bibtex(TABLES, output_file = tmp_bib)
have <- sub("^@[a-zA-Z]+\\{([^,]+),.*", "\\1", grep("^@", readLines(bib), value = TRUE))
new <- paste(readLines(tmp_bib), collapse = "\n")
entries <- strsplit(new, "\n(?=@)", perl = TRUE)[[1]]
keys <- sub("^@[a-zA-Z]+\\{([^,]+),.*", "\\1", entries)
for (i in which(!keys %in% have & grepl("^@", entries))) cat("\n", entries[i], "\n", file = bib, append = TRUE, sep = "")
