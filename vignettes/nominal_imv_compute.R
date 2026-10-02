# nominal_imv_compute.R
#
# How much is a wrong answer worth? For every multiple-choice table in the
# nominal source, compares out-of-sample predictions of right/wrong from a 2PL
# fitted to the 0/1 scores against two models fitted to the option chosen:
# Bock's nominal response model and the nested logit (Suh & Bolt, 2010). The
# Rasch -> 2PL gain on the same cells is the yardstick.
#
# Design: 5-fold cross-validation over person-item cells. Each fold hides 20%
# of the observed cells from every model; each model's EAP theta then predicts
# P(key) on the hidden cells, and the IMV compares those predictions. The
# outcome is always binary (keyed or not), so the option-level models are
# judged on the same thing the 2PL predicts: they only get to use the extra
# information, which wrong answer was chosen, on the training cells.
#
# A second, multinomial IMV judges the models on the option chosen. A 2PL has no
# opinion about the wrong options, so it enters as "2PL + shares": P(key) from
# the 2PL, with the rest split among the wrong options in the proportions they
# were chosen on that item's training cells. The binary IMV's weighted coin
# becomes a weighted K-sided die (see imv_cat() below). A third IMV looks only at
# the held-out wrong answers and asks which one was chosen; there the baseline
# is the shares alone, so it measures what theta says about the choice of
# distractor.
#
# Output: vignettes/nominal_imv_data/nominal_imv_results.rds (all tables) and
# vignettes/nominal_imv_data/references.bib. Per-table results go to
# vignettes/nominal_imv_data/fits/<table>.rds (gitignored resume cache; a table
# with a file there is skipped). Subsampled inputs and held-out predictions are
# cached in ~/.cache/irw_nominal_imv (not committed).
#
# Usage: Rscript vignettes/nominal_imv_compute.R   # from project root

library(irw)
library(mirt)
library(imv)
library(furrr)
library(dplyr)

set.seed(20260930)

out_dir   <- "vignettes/nominal_imv_data"
fits_dir  <- file.path(out_dir, "fits")
bib_file  <- file.path(out_dir, "references.bib")
cache_dir <- file.path(Sys.getenv("HOME"), ".cache", "irw_nominal_imv")
cells_dir <- file.path(cache_dir, "cells")   # held-out predictions, for re-scoring
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cells_dir, recursive = TRUE, showWarnings = FALSE)

SUBSAMPLE_N <- 3000   # respondents per table
MIN_N       <- 1000   # respondents left after prep(); below this the IMV is noise
N_FOLDS     <- 5
RARE        <- 0.01   # a wrong option chosen by fewer than 1% is pooled
MIN_ITEMS   <- 10
DENSE_ITEMS <- 40     # sparse tables: the most-answered items kept (see fetch_one)
WORKERS     <- min(4, parallel::detectCores() %/% 2)
PILOT       <- FALSE  # TRUE: only PILOT_TABLES, to check the pipeline and page
# Known answers: RMET is easy and its gain sits at the bottom of theta; ENEM
# math is hard and its gain sits at the top (near-random guessing at the bottom).
PILOT_TABLES <- c("wilmer-rmet-normative-data-set-2022_nom", "enem_2013_1mil_mt_nom")

# ------------------------------------------------------------------------------
# 1. Candidate tables: four ENEM tests (2013, one per area) and every other
#    table in irw_nominal v3.3 with at least MIN_ITEMS items. Listed by hand
#    because nominal_metadata.csv lags releases. Essays and other free text fall
#    out in prep(), as do tables without one keyed option per item. Not listed:
#    cos101_2026, goldberg_2018_spa_computer_use, himmelstein-berlin_numeracy-2025
#    and mthimkhulu_2023_pirls_reading, all under MIN_ITEMS items.
# ------------------------------------------------------------------------------

cands <- c(
  paste0("enem_2013_1mil_", c("ch", "cn", "lc", "mt"), "_nom"),
  "wilmer-rmet-normative-data-set-2022_nom", "wilmer-mrmet-normative-data-set-2022_nom",
  "borges_brazil_residency_2024_pbt_nom", "borges_brazil_residency_2024_cbt_nom",
  "choi_2020_ednet_listening_nom", "choi_2020_ednet_reading_nom",
  "vocabulary_iq_nom", "zorowitz_2023_marsib_nom", "fitz_2024_numeracy_nom",
  "psychtools_ability_nom", "experimental_iq_nom", "myszkowski_2018_spmls_nom",
  "sirota_2018_crt_nom", "suarez_2026_statistics_nom", "voropaeva_2026_health_media_nom",
  "nomt_hooper_2024_study2_nom", "papousek_2017_anatomy_nom", "geography_nom",
  "cifar10h_nom", "semeval2013_scientsbank_nom", "preference_inventory_nom",
  "hachenberger_2025_stroop_main_nom", "hachenberger_2025_stroop_pilot_nom",
  "much_tte_2025_matrixreasoning_nom", "much_tte_2025_concentrationtask_nom",
  "blum_2018_imak_nom", "asap20train_nom", "persuade_learningagency_nom"
)
all_cands <- cands
if (PILOT) cands <- PILOT_TABLES
if (nzchar(Sys.getenv("TABLES"))) cands <- strsplit(Sys.getenv("TABLES"), ",")[[1]]

# Not multiple choice in the nominal model's sense: both come from adaptive
# practice systems that draw a new set of options each time a place or term is
# asked, so an item has no fixed options for the model's categories to stand for.
EXCLUDE <- c(
  geography_nom             = "option set changes from one presentation of an item to the next",
  papousek_2017_anatomy_nom = "option set changes from one presentation of an item to the next"
)
message("Candidates: ", length(cands))

# ------------------------------------------------------------------------------
# 2. Fetch + subsample, one table at a time (the enem_*_1mil_* tables need ~3 GB
#    each while in memory, so this pass is sequential).
# ------------------------------------------------------------------------------

fetch_one <- function(tn) {
  f <- file.path(cache_dir, paste0(tn, ".rds"))
  if (file.exists(f)) return(invisible(f))
  message("fetch ", tn)
  n <- irw_fetch(tn, source = "nom")
  if (!all(c("resp", "text") %in% names(n))) {
    saveRDS(list(table = tn, skipped = "no resp/text columns (unscored free text)",
                 date_run = as.character(Sys.Date())), file.path(fits_dir, paste0(tn, ".rds")))
    return(invisible(NULL))
  }
  n <- data.table::as.data.table(n)[!is.na(text) & !is.na(resp), .(id, item, resp, text)]
  # Repeated attempts (EdNet, the adaptive practice systems): keep one per cell.
  n <- unique(n, by = c("id", "item"))
  n_full <- data.table::uniqueN(n$id)
  # Sparse designs, where the typical respondent sees under half the items
  # (EdNet, Anatom, CIFAR-10H), would lose everyone to prep()'s half-the-items
  # rule. Keep a dense block instead: the DENSE_ITEMS most-answered items, and
  # the respondents who answered at least half of them. These items are not a
  # random draw from the bank, so a dense-block result describes that block.
  n_bank <- data.table::uniqueN(n$item)
  dense <- n[, .N, by = id][, median(N)] < 0.5 * n_bank
  if (dense) {
    top <- n[, .N, by = item][order(-N)][seq_len(min(DENSE_ITEMS, .N)), item]
    n <- n[item %in% top]
    keep <- n[, .N, by = id][N >= 0.5 * length(top), id]
    n <- n[id %in% keep]
    if (!length(keep)) {
      saveRDS(list(table = tn, skipped = paste0("sparse design: no respondent answered half of the ",
                                               length(top), " most-answered of ", n_bank, " items"),
                   date_run = as.character(Sys.Date())), file.path(fits_dir, paste0(tn, ".rds")))
      return(invisible(NULL))
    }
  }
  ids <- unique(n$id)
  if (length(ids) > SUBSAMPLE_N) n <- n[id %in% sample(ids, SUBSAMPLE_N)]
  n <- as.data.frame(n)
  attr(n, "n_full") <- n_full
  attr(n, "dense_block") <- if (dense) c(bank = n_bank, block = length(top), persons = length(ids)) else NULL
  saveRDS(n, f)
  rm(n); gc()
  invisible(f)
}
for (tn in cands) tryCatch(fetch_one(tn), error = function(e) message("  fetch failed: ", tn, ": ", conditionMessage(e)))

# ------------------------------------------------------------------------------
# 3. Long -> two wide matrices: the option chosen (key = highest code) and 0/1.
# ------------------------------------------------------------------------------

prep <- function(n) {
  n$text <- as.character(n$text)
  keys <- tapply(n$text[n$resp == 1], n$item[n$resp == 1], function(x) unique(x))
  if (any(lengths(keys) != 1)) return("not one keyed option per item")
  if (!all(n$resp %in% 0:1)) return("resp is not 0/1")
  # Free text: an item with more than 12 answers that each draw at least RARE of
  # its responses, or whose rarer answers together make up over 10%. Counting
  # all distinct answers instead would throw out VIQT, where respondents pick
  # two of five words and most of the possible pairs are rare.
  freeish <- tapply(n$text, n$item, function(x) {
    sh <- table(x) / length(x)
    sum(sh >= RARE) > 12 || sum(sh[sh < RARE]) > 0.10
  })
  if (any(freeish)) return("free text (more than 12 common answers to an item)")
  ids <- unique(n$id); items <- sort(names(keys))
  opt <- x01 <- matrix(NA_integer_, length(ids), length(items), dimnames = list(as.character(ids), items))
  n_pooled <- 0L; n_cells_dropped <- 0L
  for (it in items) {
    d <- n[n$item == it, ]
    k <- keys[[it]]
    share <- table(d$text) / nrow(d)
    rare <- setdiff(names(share)[share < RARE], k)
    if (length(rare) > 1) n_pooled <- n_pooled + 1L
    d$opt <- ifelse(d$text %in% rare, "_rare", d$text)
    # A pooled category that is itself under RARE (e.g. ENEM's blank and
    # double-mark codes, ~0.5% between them) is too thin to estimate: drop it.
    if (any(d$opt == "_rare") && mean(d$opt == "_rare") < RARE) {
      n_cells_dropped <- n_cells_dropped + sum(d$opt == "_rare")
      d <- d[d$opt != "_rare", ]
    }
    lv <- c(sort(setdiff(unique(d$opt), k)), k)   # key last: sets theta's direction
    r <- match(as.character(d$id), rownames(opt))
    opt[r, it] <- match(d$opt, lv)
    x01[r, it] <- as.integer(d$opt == k)
  }
  p <- colMeans(x01, na.rm = TRUE)
  ncat <- apply(opt, 2, function(z) length(unique(na.omit(z))))
  ok <- ncat >= 3 & p > 0 & p < 1
  opt <- opt[, ok, drop = FALSE]; x01 <- x01[, ok, drop = FALSE]
  # Respondents left with under half their items (ENEM candidates who left
  # nearly everything blank) carry too little to estimate theta.
  keep <- rowMeans(!is.na(opt)) >= 0.5
  if (ncol(opt) < MIN_ITEMS) return("fewer than MIN_ITEMS usable items")
  list(opt = opt[keep, , drop = FALSE], x01 = x01[keep, , drop = FALSE],
       n_items_dropped = sum(!ok), n_persons_dropped = sum(!keep),
       n_cells_dropped = n_cells_dropped)
}

# ------------------------------------------------------------------------------
# 4. The multinomial IMV. The binary IMV turns a model's mean log-likelihood on
#    the held-out cells into a weighted coin: the w whose entropy,
#    -(w log w + (1 - w) log(1 - w)), matches it. Here the coin becomes a
#    K-sided die that lands on the observed option with probability w and on
#    each other side with (1 - w) / (K - 1). K varies by item, so w solves
#        mean_i[ w log w + (1 - w) log((1 - w) / (K_i - 1)) ] = mean_i log p_i,
#    where p_i is the probability the model gave the option chosen in cell i.
#    The IMV is then (w1 - w0) / w0, as before. With K = 2 everywhere this is
#    imv.binary(). p0 and p1 are the two models' probabilities of the observed
#    option; sigma clips them as imv.binary() does. The left side is convex in w
#    and smallest somewhere between 1/max(K) and 1/min(K) (the fair die when K is
#    constant); w is the root above that minimum, and a model no better than the
#    minimum gets the minimum. Kanopka (2023, ch. 3) extends the IMV to
#    polytomous outcomes differently, through threshold (omega_t) and pairwise
#    (omega_c) binary IMVs that stay on the dichotomous scale; see the .qmd for
#    why this page uses the die instead.
# ------------------------------------------------------------------------------

imv_cat <- function(p0, p1, K, sigma = 1e-4) {
  die <- function(p) {
    ll <- mean(log(pmin(pmax(p, sigma), 1 - sigma)))
    f <- function(w) mean(w * log(w) + (1 - w) * log((1 - w) / (K - 1))) - ll
    lo <- if (min(K) == max(K)) 1 / K[1] else
      optimize(f, c(1 / max(K), 1 / min(K)), tol = 1e-12)$minimum
    if (f(lo) >= 0) return(lo)
    uniroot(f, c(lo, 1 - 1e-12), tol = 1e-12)$root
  }
  w0 <- die(p0); w1 <- die(p1)
  (w1 - w0) / w0
}

# ------------------------------------------------------------------------------
# 5. Cross-validated comparison for one table.
# ------------------------------------------------------------------------------

fit_one <- function(tn) {
  out_file <- file.path(fits_dir, paste0(tn, ".rds"))
  if (file.exists(out_file)) return(readRDS(out_file))
  if (tn %in% names(EXCLUDE)) {
    res <- list(table = tn, skipped = EXCLUDE[[tn]], date_run = as.character(Sys.Date()))
    saveRDS(res, out_file); return(res)
  }
  f <- file.path(cache_dir, paste0(tn, ".rds"))
  if (!file.exists(f)) return(NULL)
  n <- readRDS(f)
  P <- prep(n)
  if (is.character(P)) {
    res <- list(table = tn, skipped = P, date_run = as.character(Sys.Date()))
    saveRDS(res, out_file); return(res)
  }
  if (nrow(P$opt) < MIN_N) {
    res <- list(table = tn, skipped = paste0("N = ", nrow(P$opt), ", below ", MIN_N),
                date_run = as.character(Sys.Date()))
    saveRDS(res, out_file); return(res)
  }
  opt <- P$opt; x01 <- P$x01
  ncat <- apply(opt, 2, max, na.rm = TRUE)
  obs <- which(!is.na(opt))
  fold <- sample(rep_len(seq_len(N_FOLDS), length(obs)))
  t0 <- Sys.time()

  per_fold <- lapply(seq_len(N_FOLDS), function(k) {
    ho <- obs[fold == k]
    opt_tr <- opt; opt_tr[ho] <- NA
    x_tr <- x01; x_tr[ho] <- NA
    mods <- list(
      rasch   = mirt(x_tr, 1, "Rasch", verbose = FALSE),
      twopl   = mirt(x_tr, 1, "2PL", verbose = FALSE),
      # The option models need more than mirt's default 500 EM cycles on the
      # long tests (the residency exams, ENEM math).
      nominal = mirt(opt_tr, 1, "nominal", verbose = FALSE, technical = list(NCYCLES = 2000)),
      nested  = mirt(opt_tr, 1, "2PLNRM", key = ncat, verbose = FALSE, technical = list(NCYCLES = 2000))
    )
    conv <- sapply(mods, function(m) extract.mirt(m, "converged"))
    rc <- arrayInd(ho, dim(opt))
    y_opt <- opt[ho]
    # Wrong-option shares on this fold's training cells: how the 2PL and Rasch
    # spread their P(wrong) when they are asked about options.
    shares <- lapply(seq_len(ncol(opt)), function(j) {
      z <- opt_tr[, j]; z <- z[!is.na(z) & z != ncat[j]]
      tabulate(z, ncat[j]) / length(z)
    })
    preds <- lapply(names(mods), function(m) {
      th <- fscores(mods[[m]], method = "EAP")[, 1]
      binary <- m %in% c("rasch", "twopl")
      p_key <- p_obs <- numeric(length(ho))
      for (j in unique(rc[, 2])) {
        w <- which(rc[, 2] == j)
        pr <- probtrace(extract.item(mods[[m]], j), matrix(th[rc[w, 1]]))
        if (binary) {
          P <- outer(pr[, 2], numeric(ncat[j])) + outer(pr[, 1], shares[[j]])
          P[, ncat[j]] <- pr[, 2]
        } else P <- pr
        # The keyed option is always the last category of the option models.
        p_key[w] <- P[, ncat[j]]
        p_obs[w] <- P[cbind(seq_along(w), y_opt[w])]
      }
      setNames(data.frame(p_key, p_obs), paste0(c("", "obs_"), m))
    })
    th2 <- fscores(mods$twopl, method = "EAP")[, 1]
    list(cells = data.frame(y = x01[ho], K = ncat[rc[, 2]], do.call(cbind, preds),
                            theta_2pl = th2[rc[, 1]], fold = k), conv = conv)
  })
  converged <- t(sapply(per_fold, `[[`, "conv"))
  cells <- do.call(rbind, lapply(per_fold, `[[`, "cells"))
  cuts <- quantile(cells$theta_2pl, c(0, 1/3, 2/3, 1))
  cells$tercile <- cut(cells$theta_2pl, cuts, include.lowest = TRUE, labels = c("low", "mid", "high"))
  saveRDS(cells, file.path(cells_dir, paste0(tn, ".rds")))

  imvs <- function(d) {
    wr <- d[d$y == 0, ]
    given_wrong <- function(m) wr[[paste0("obs_", m)]] / (1 - wr[[m]])
    c(
      # Binary: right/wrong on the held-out cells.
      rasch_to_2pl   = imv.binary(d$y, d$rasch, d$twopl),
      twopl_to_nom   = imv.binary(d$y, d$twopl, d$nominal),
      twopl_to_nest  = imv.binary(d$y, d$twopl, d$nested),
      nest_to_nom    = imv.binary(d$y, d$nested, d$nominal),
      # Multinomial: the option chosen, K options per item.
      cat_rasch_to_2pl  = imv_cat(d$obs_rasch, d$obs_twopl, d$K),
      cat_twopl_to_nom  = imv_cat(d$obs_twopl, d$obs_nominal, d$K),
      cat_twopl_to_nest = imv_cat(d$obs_twopl, d$obs_nested, d$K),
      cat_nest_to_nom   = imv_cat(d$obs_nested, d$obs_nominal, d$K),
      # Which wrong answer, among the held-out wrong answers (K - 1 options).
      # The 2PL's given-wrong prediction is the shares alone.
      wrong_shares_to_nom  = imv_cat(given_wrong("twopl"), given_wrong("nominal"), wr$K - 1),
      wrong_shares_to_nest = imv_cat(given_wrong("twopl"), given_wrong("nested"), wr$K - 1)
    )
  }
  by_fold    <- t(sapply(split(cells, cells$fold), imvs))
  by_tercile <- t(sapply(split(cells, cells$tercile), imvs))

  res <- list(
    table = tn, skipped = NA_character_,
    n_full = attr(n, "n_full"), n_persons = nrow(opt), n_items = ncol(opt),
    p_correct = mean(x01, na.rm = TRUE),
    n_options = table(ncat),
    n_items_dropped = P$n_items_dropped, n_persons_dropped = P$n_persons_dropped,
    n_cells_dropped = P$n_cells_dropped, dense_block = attr(n, "dense_block"),
    converged = converged,
    imv_all = imvs(cells), imv_by_fold = by_fold, imv_by_tercile = by_tercile,
    seconds = as.numeric(Sys.time() - t0, units = "secs"),
    date_run = as.character(Sys.Date()),
    irw_pkg = as.character(packageVersion("irw")),
    mirt_pkg = as.character(packageVersion("mirt"))
  )
  saveRDS(res, out_file)
  res
}

plan(multisession, workers = WORKERS)
results <- future_map(cands, function(tn) tryCatch(fit_one(tn), error = function(e)
  list(table = tn, skipped = paste("error:", conditionMessage(e)))),
  .options = furrr_options(seed = TRUE))
plan(sequential)
results <- Filter(Negate(is.null), results)
date_run <- as.Date(max(sapply(results, function(r) r$date_run %||% NA_character_), na.rm = TRUE))
fitted <- Filter(function(r) is.na(r$skipped), results)
summary_df <- bind_rows(lapply(fitted, function(r) tibble::tibble(
  table = r$table, n_full = r$n_full, n_persons = r$n_persons, n_items = r$n_items,
  p_correct = r$p_correct, dense_block = !is.null(r$dense_block),
  !!!as.list(r$imv_all)
)))
saveRDS(list(
  summary          = summary_df,
  results          = results,   # per-table detail: folds, terciles, convergence, skips
  candidate_tables = cands,
  n_all_candidates = length(all_cands),
  pilot            = PILOT,
  date_run         = date_run,
  session          = sessionInfo()
), file.path(out_dir, "nominal_imv_results.rds"))
message("Done: ", length(fitted), " tables fitted, ", length(results) - length(fitted), " skipped")

# ------------------------------------------------------------------------------
# 6. Citations: the tables analysed, then the methods papers the page cites.
# ------------------------------------------------------------------------------

tryCatch(irw_save_bibtex(summary_df$table, output_file = bib_file, source = "nom"),
         error = function(e) message("bibtex generation failed: ", conditionMessage(e)))

manual_entries <- c(
"@article{bock1972,
  author  = {Bock, R. Darrell},
  title   = {Estimating Item Parameters and Latent Ability when Responses Are Scored in Two or More Nominal Categories},
  journal = {Psychometrika},
  year    = {1972},
  volume  = {37},
  number  = {1},
  pages   = {29--51},
  doi     = {10.1007/BF02291411}
}",
"@article{suh2010,
  author  = {Suh, Youngsuk and Bolt, Daniel M.},
  title   = {Nested Logit Models for Multiple-Choice Item Response Data},
  journal = {Psychometrika},
  year    = {2010},
  volume  = {75},
  number  = {3},
  pages   = {454--473},
  doi     = {10.1007/s11336-010-9163-7}
}",
"@phdthesis{kanopka2023,
  author = {Kanopka, Klint},
  title  = {Computational Validity},
  school = {Stanford University},
  year   = {2023},
  note   = {Chapter 3, with Benjamin W. Domingue: Bookmaking for Categorical Responses: Extending the InterModel Vigorish to Quantify the Performance of Polytomous Item Response Models},
  url    = {https://purl.stanford.edu/tr545td7650}
}"
)
entry_key <- function(entry) sub("^@\\w+\\{([^,]+),.*$", "\\1", trimws(entry))
existing_keys <- if (file.exists(bib_file)) {
  key_lines <- grep("^@\\w+\\{", readLines(bib_file), value = TRUE)
  vapply(key_lines, entry_key, character(1), USE.NAMES = FALSE)
} else character(0)
new_entries <- manual_entries[!vapply(manual_entries, entry_key, character(1)) %in% existing_keys]
if (length(new_entries)) cat(paste0(new_entries, "\n"), file = bib_file, append = TRUE, sep = "\n")
