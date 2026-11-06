# bits_per_item_poly_extra.R
#
# Extra tables for the "How many categories do you need?" figure of the
# polytomous appendix. A random draw (seed 7) of dense, single-administration
# polytomous tables (4-7 categories, 5-15 items, density >= .99, 1k-50k
# respondents, not longitudinal), fitted with the GPCM only. For each table:
# every merge of adjacent categories for every item (without refitting), the
# best merge per k, and the whole test with every item cut to two at its best
# split. Reverse keying is not applied: mutual information is invariant to it.
#
# Output: bits_per_item_data/poly_extra_results.rds
# Usage:  RENV_ACTIVATE_PROJECT=FALSE Rscript vignettes/bits_per_item_poly_extra.R

suppressMessages({ library(irw); library(mirt); library(dplyr); library(purrr); library(furrr) })
source("vignettes/bits_per_item_helpers.R")
source("vignettes/bits_per_item_poly_helpers.R")

out_dir  <- "vignettes/bits_per_item_data"
fits_dir <- file.path(out_dir, "fits")
log_file <- file.path(fits_dir, "poly_extra_failures.log")
MAX_N <- 10000; MC_SE <- 0.01; WORKERS <- 4; N_DRAW <- 30; SEED <- 7
EXCLUDE <- c("rosenberg_selfesteem", "bfi_goldberg_1992_extraversion", "su_2024_gad7")

meta <- irw_metadata()
pool <- subset(meta, n_categories >= 4 & n_categories <= 7 & n_items >= 5 & n_items <= 15 &
                 density >= 0.99 & n_participants >= 1000 & n_participants <= 50000 &
                 !longitudinal & !table %in% EXCLUDE)
set.seed(SEED)
TABLES <- sample(pool$table, N_DRAW)

one_table <- function(nm) {
  t0 <- Sys.time()
  prep <- prepare_resp_poly(irw_fetch(nm), max_n = MAX_N, seed = sum(utf8ToInt(nm)))
  resp <- prep$resp; K <- prep$K; n <- ncol(resp)
  fit <- mirt(resp, 1, itemtype = "gpcm", verbose = FALSE)
  g <- make_grid(1)
  Ps <- item_probs(fit, g$nodes, 1)
  ii <- poly_item_info(Ps, g$w)
  cl <- bind_rows(lapply(seq_len(n), function(j)
    cbind(table = nm, item = colnames(resp)[j], K = K[j], I_full = ii[j, "I"], item_collapses(Ps[[j]], g$w))))
  best <- cl |> group_by(item, k) |> slice_max(I, n = 1, with_ties = FALSE) |> ungroup()
  cuts <- vapply(colnames(resp), function(it) as.integer(best$cuts[best$item == it & best$k == 2]), integer(1))
  I_X <- poly_info_pattern(Ps, g$w, target_se = MC_SE)$I
  I_dich <- poly_info_exact(Map(collapse_probs, Ps, cuts), g$w)$I
  list(table = nm, n_items = n, K = K, n_used = nrow(resp), converged = extract.mirt(fit, "converged"),
       collapses = cl,
       dichotomised = data.frame(table = nm, I_X = I_X, I_dich = I_dich),
       secs = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

fit_to_disk <- function(nm) {
  f <- file.path(fits_dir, paste0("poly_extra_", nm, ".rds"))
  if (file.exists(f)) return(invisible(NULL))
  suppressMessages({ library(irw); library(mirt); library(dplyr); library(purrr) })
  source("vignettes/bits_per_item_helpers.R"); source("vignettes/bits_per_item_poly_helpers.R")
  res <- tryCatch(one_table(nm), error = function(e) {
    cat(format(Sys.time()), nm, conditionMessage(e), "\n", file = log_file, append = TRUE); NULL })
  if (!is.null(res)) saveRDS(res, f)
  invisible(NULL)
}

plan(multisession, workers = WORKERS)
future_map(TABLES, fit_to_disk, .options = furrr_options(seed = TRUE,
  globals = c("one_table", "MAX_N", "MC_SE", "fits_dir", "log_file")))
plan(sequential)

ok <- compact(lapply(TABLES, function(t) {
  f <- file.path(fits_dir, paste0("poly_extra_", t, ".rds")); if (file.exists(f)) readRDS(f) else NULL }))
saveRDS(list(
  tables = bind_rows(lapply(ok, function(r) data.frame(table = r$table, n_items = r$n_items, K = max(r$K),
                                                       n_used = r$n_used, converged = r$converged, secs = r$secs))),
  collapses = bind_rows(map(ok, "collapses")),
  dichotomised = bind_rows(map(ok, "dichotomised")),
  tables_attempted = TABLES,
  failures = if (file.exists(log_file)) readLines(log_file) else character(0),
  settings = list(seed = SEED, n_draw = N_DRAW, pool = nrow(pool), MAX_N = MAX_N, MC_SE = MC_SE, model = "GPCM"),
  date_run = Sys.Date()), file.path(out_dir, "poly_extra_results.rds"))
message("Saved: ", length(ok), " of ", length(TABLES), " tables")
