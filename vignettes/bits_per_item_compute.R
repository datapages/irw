# bits_per_item_compute.R
#
# How many bits does a test deliver about a respondent's latent trait? For
# each eligible dichotomous IRW table this fits Rasch and 2PL models and
# computes, in bits: H(X | theta), the sum-score information I(theta; S)
# (Lord-Wingersky), the full-pattern information I(theta; X) (exact for
# n <= 16 items, Monte Carlo above), H(X), efficiency, reliabilities and their
# Gaussian bit equivalents, per-item bits, length curves (random and greedy
# orders) and a sum-score model check. For three showcase tables it also
# writes the payloads for the page's interactive pieces: theta cut into 64
# equally likely groups, real-respondent replays (fixed and adaptive order),
# simulated remaining-uncertainty curves, the 32-pattern chart for the five
# most informative items, and the binned-vs-continuous convergence check.
#
# Table selection comes from the Phase 0 scan (bits_per_item_scout.R):
# dichotomous, 5-60 items, >= 500 respondents after the single-administration
# rule (earliest wave, then the control arm), no repeated id-item rows,
# response density >= 0.8, and no ENEM tables (left out for now).
#
# Note on the summary columns: for the 2PL, I_X - I_S (the "sum-score gap") is
# the conditional mutual information I(theta; X | S), by the chain rule
# I(theta; X) = I(theta; S) + I(theta; X | S): information about theta in which
# items were answered correctly beyond how many. Under Rasch it is zero.
#
# Output: bits_per_item_data/bits_per_item_results.rds
#         bits_per_item_data/references.bib
#
# Usage:
#   Rscript vignettes/bits_per_item_compute.R   # from project root
# Run checks first: Rscript vignettes/bits_per_item_checks.R

suppressMessages({
  library(irw)
  library(mirt)
  library(dplyr)
  library(purrr)
  library(furrr)
  library(tibble)
})
source("vignettes/bits_per_item_helpers.R")
options(irw.itemtext_disclaimer = FALSE)

set.seed(20261005)

out_dir      <- "vignettes/bits_per_item_data"
fits_dir     <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
results_file <- file.path(out_dir, "bits_per_item_results.rds")
bib_file     <- file.path(out_dir, "references.bib")
log_file     <- file.path(out_dir, "fits", "failures.log")

MIN_DENSITY    <- 0.8
MAX_N          <- 10000   # respondents; larger tables are subsampled with a fixed seed
MIN_COVERAGE   <- 0.5     # drop items answered by fewer than half the respondents
MC_SE          <- 0.005   # target Monte Carlo SE for I(theta; X), in bits
N_RANDOM       <- 5       # random item orders per length curve
K_GROUPS       <- 6       # 2^6 = 64 equally likely theta groups for the showcases
SHOWCASES      <- c("fitz_2024_numeracy", "machivallianism_test_vcl", "frac20")
PILOT          <- Sys.getenv("PILOT") == "1"   # PILOT=1 Rscript ... for a 15-table draft run
PILOT_N_TABLES <- 15
WORKERS        <- 4

# ==============================================================================
# 1. Select tables
# ==============================================================================

scan <- read.csv(file.path(out_dir, "scout_tables.csv"), stringsAsFactors = FALSE)
all_candidates <- scan |>
  filter(is.na(excl), density >= MIN_DENSITY, !grepl("^enem_", table)) |>
  pull(table)
message("Candidate tables: ", length(all_candidates))

tables <- if (PILOT) {
  c(intersect(SHOWCASES, all_candidates),
    sample(setdiff(all_candidates, SHOWCASES), PILOT_N_TABLES - length(SHOWCASES)))
} else all_candidates

tags_meta <- tryCatch(irw_tags(tables = tables), error = function(e) NULL)

# ==============================================================================
# 2. Per-table computation (cached per table)
# ==============================================================================

fit_to_disk <- function(table_name) {
  f <- file.path(fits_dir, paste0(table_name, ".rds"))
  if (file.exists(f)) return(invisible(NULL))
  # multisession workers start clean: load the helpers here
  suppressMessages(library(irw))
  source("vignettes/bits_per_item_helpers.R", local = TRUE)
  set.seed(sum(utf8ToInt(table_name)))
  res <- tryCatch(
    process_table(table_name, max_n = MAX_N, mc_se = MC_SE, n_random = N_RANDOM),
    error = function(e) {
      cat(format(Sys.time()), table_name, conditionMessage(e), "\n", file = log_file, append = TRUE)
      NULL
    })
  if (!is.null(res)) saveRDS(res, f)
  invisible(NULL)
}

plan(multisession, workers = WORKERS)
future_map(tables, fit_to_disk, .options = furrr_options(seed = TRUE))
plan(sequential)

# ==============================================================================
# 3. Showcase payloads (2PL)
# ==============================================================================

showcase_payload <- function(table_name) {
  set.seed(sum(utf8ToInt(table_name)))
  prep <- prepare_resp(irw_fetch(table_name), max_n = MAX_N, min_coverage = MIN_COVERAGE)
  resp <- prep$resp
  fit <- fit_2pl(resp)
  cf <- coef(fit, simplify = TRUE)$items
  a <- setNames(cf[, "a1"], colnames(resp)); d <- setNames(cf[, "d"], colnames(resp))
  g <- make_grid(1)
  P <- p_matrix(a, d, g$nodes)
  bits <- item_info_bits(P, g$w)

  # Item text and response labels (option text when the item text has it).
  it <- NULL
  for (attempt in 1:3) {
    it <- tryCatch(as.data.frame(irw_itemtext(table_name)), error = function(e) NULL)
    if (!is.null(it)) break
    Sys.sleep(5 * attempt)
  }
  # irw_long2resp() prefixes "item_"; IRW item ids may or may not carry it.
  key <- sub("^item_", "", colnames(resp))
  if (!is.null(it) && sum(colnames(resp) %in% it$item) > sum(key %in% it$item)) key <- colnames(resp)
  text <- lab0 <- lab1 <- rep(NA_character_, length(key))
  if (!is.null(it)) {
    one <- it[!duplicated(it$item), ]
    text <- one$item_text[match(key, one$item)]
    if ("option_text" %in% names(it)) {
      lab <- function(r) { o <- it[it$resp == r, ]; o$option_text[match(key, o$item)] }
      lab0 <- lab(0); lab1 <- lab(1)
    }
  }
  items <- data.frame(item = colnames(resp), label = sub("^item_", "", colnames(resp)), text = text,
                      label0 = ifelse(is.na(lab0) | lab0 %in% c("", "NA"), "incorrect", lab0),
                      label1 = ifelse(is.na(lab1) | lab1 %in% c("", "NA"), "correct", lab1),
                      a = unname(a), d = unname(d), b = unname(-d / a), bits = unname(bits))

  PG <- group_probs(a, d, K_GROUPS); colnames(PG) <- colnames(resp)

  # 32-pattern chart for the 5 most informative items.
  top5 <- order(-bits)[1:5]
  X5 <- as.matrix(expand.grid(rep(list(0:1), 5)))
  P5 <- P[, top5]
  implied <- as.vector(exp(X5 %*% t(log(P5)) + (1 - X5) %*% t(log(1 - P5))) %*% g$w)
  obs_rows <- resp[stats::complete.cases(resp[, top5]), top5]
  obs_key <- apply(as.matrix(obs_rows), 1, paste, collapse = "")
  pat_key <- apply(X5, 1, paste, collapse = "")
  observed <- as.vector(table(factor(obs_key, levels = pat_key))) / length(obs_key)
  patterns <- data.frame(pattern = pat_key, implied = implied, observed = observed,
                         n_obs = length(obs_key))[order(-implied), ]

  # Replays: complete respondents at sum-score quantiles .1/.3/.5/.7/.9,
  # plus the complete respondent with the lowest person-fit Zh.
  complete <- which(stats::complete.cases(resp))
  ss <- rowSums(resp[complete, ])
  pick <- vapply(c(.1, .3, .5, .7, .9), function(q) {
    target <- quantile(ss, q, type = 1)
    cand <- complete[ss == target]
    cand[sample.int(length(cand), 1)]
  }, integer(1))
  pf <- mirt::personfit(fit)$Zh
  zh_c <- pf[complete]; zh_c[complete %in% pick] <- Inf
  aberrant <- complete[which.min(zh_c)]
  who <- c(pick, aberrant)
  replays <- lapply(seq_along(who), function(i) {
    x <- unlist(resp[who[i], ])
    list(respondent = LETTERS[i], kind = c(rep("quantile", 5), "aberrant")[i],
         sum_score = sum(x), zh = pf[who[i]], responses = x,
         fixed = replay(x, PG, "fixed"), adaptive = replay(x, PG, "adaptive"),
         # easiest item first: makes "right on hard items, wrong on easy ones" readable
         by_difficulty = { ord <- order(items$b); replay(x[ord], PG[, ord, drop = FALSE], "fixed") })
  })

  list(
    table = table_name, info = prep$info, n = nrow(resp),
    converged = extract.mirt(fit, "converged"),
    instrument = if (!is.null(it)) it$instrument[1] else NA_character_,
    items = items, grid = data.frame(theta = g$nodes, w = g$w),
    group_bounds = group_bounds(K_GROUPS), group_probs = PG,
    patterns = patterns, replays = replays,
    uncertainty = uncertainty_curves(a, d, K_GROUPS, n_sim = 500),
    bin_convergence = bin_convergence(a, d, Kmax = 8)
  )
}

showcases <- setNames(lapply(intersect(SHOWCASES, tables), showcase_payload),
                      intersect(SHOWCASES, tables))

# ==============================================================================
# 4. Combine
# ==============================================================================

fits <- map(tables, function(t) {
  f <- file.path(fits_dir, paste0(t, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
})
names(fits) <- tables
ok <- compact(fits)

summary_tbl <- bind_rows(map(ok, "summary")) |>
  left_join(bind_rows(map(ok, function(r) tibble(
    table = r$table, n_items = r$n_items,
    n_ids_raw = r$info$n_ids_raw, n_ids_rule = r$info$n_ids_rule, n_ids_used = r$info$n_ids_used,
    subsampled = r$info$subsampled, wave_kept = as.character(r$info$wave_kept),
    treat_kept = as.character(r$info$treat_kept), n_items_raw = r$info$n_items_raw,
    n_low_coverage = r$info$n_low_coverage, n_zero_var = r$info$n_zero_var,
    n_ids_dropped_long2resp = r$info$n_ids_dropped_long2resp))), by = "table")
if (!is.null(tags_meta)) summary_tbl <- left_join(summary_tbl, tags_meta, by = "table")

failures <- if (file.exists(log_file)) readLines(log_file) else character(0)

results <- list(
  summary          = summary_tbl,
  items            = bind_rows(map(ok, "items")),
  curves           = bind_rows(map(ok, "curves")),
  showcases        = showcases,
  failures         = failures,
  candidate_tables = all_candidates,
  n_all_candidates = length(all_candidates),
  tables_attempted = tables,
  settings         = list(MIN_DENSITY = MIN_DENSITY, MAX_N = MAX_N, MIN_COVERAGE = MIN_COVERAGE,
                          MC_SE = MC_SE, N_RANDOM = N_RANDOM, K_GROUPS = K_GROUPS,
                          slope_prior = "lognormal(0, 1)", grid = "81 nodes, +/- 6 SD"),
  pilot            = PILOT,
  date_run         = Sys.Date(),
  session          = sessionInfo()
)
saveRDS(results, results_file)
message("Saved ", results_file, ": ", length(ok), " of ", length(tables), " tables")

irw_save_bibtex(names(ok), output_file = bib_file)

# ==============================================================================
# 5. Reliability vs bits in the abstract (synthetic tests, no IRW data).
#    Separate block, own output file; see that script.
# ==============================================================================

source("vignettes/bits_per_item_reliability_sim.R")

# ==============================================================================
# 6. Sufficiency in information terms ("Is the sum score enough?")
#    Separate block, own output file (sufficiency_results.rds); see that script.
# ==============================================================================

source("vignettes/bits_per_item_sufficiency.R")
