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
# A second IMV judges the models on the option chosen: omega_c, the polytomous
# IMV of Kanopka (2023, ch. 3), built from binary IMVs over pairs of options (see
# section 4). A 2PL has no opinion about the wrong options, so it enters as
# "2PL + shares": P(key) from the 2PL, with the rest split among the wrong
# options in the proportions they were chosen on that item's training cells. A
# third IMV looks only at the held-out wrong answers and asks which one was
# chosen: omega_c over the pairs of wrong options. There the baseline is the
# shares alone, so it measures what theta says about the choice of distractor.
#
# The nominal model is fitted with a1 fixed at 1 and every category slope free
# except the reference category's (see nominal_pars() below). This is the same
# model as mirt's default parameterization, which on these data stops below the
# maximum while reporting convergence; the first draw also fits the default, to
# record by how much.
#
# Uncertainty: every table is fitted on N_DRAWS independent draws of up to
# SUBSAMPLE_N respondents, each with its own split into folds. The IMVs are
# averaged over draws and their standard errors come from the draw-to-draw
# spread, which covers both which respondents were sampled and how their cells
# were split.
#
# Output: vignettes/nominal_imv_data/nominal_imv_results.rds (all tables) and
# vignettes/nominal_imv_data/references.bib. Per-table results go to
# vignettes/nominal_imv_data/fits/<table>_draws<N_DRAWS>.rds (gitignored resume
# cache; a table with a file there is skipped, so delete fits/ after changing
# this script). Subsampled inputs and held-out predictions are cached in
# ~/.cache/irw_nominal_imv (not committed; delete it to fetch newer IRW data),
# and irw keeps its own copy of each downloaded table (see irw_cache_info()).
#
# Usage, from the project root:
#   export REDIVIS_API_TOKEN=...   # read scope, for irw_fetch() and
#                                  # irw_save_bibtex(); without it, redivis opens
#                                  # a browser to log in and waits up to 10 min
#   N_DRAWS=10 WORKERS=4 Rscript vignettes/nominal_imv_compute.R
# Packages: irw, mirt, imv, furrr, data.table, dplyr, tibble.
# Environment variables, all optional:
#   N_DRAWS  draws per table (default 10)
#   WORKERS  tables fitted in parallel (default min(4, cores / 2)); does not
#            change the results
#   TABLES   comma-separated subset of cands, to check the pipeline. Each
#            table's draws and folds depend on its position in the list and on
#            the tables fetched before it, so its numbers differ from a full
#            run's, and it overwrites the committed .rds and .bib.
# Time: about 40 minutes for the full run with WORKERS = 7 on an 18-core Mac,
# and about two minutes for TABLES=psychtools_ability_nom,vocabulary_iq_nom
# N_DRAWS=2 WORKERS=2. Fetching holds one ENEM table (about 3 GB) in memory.

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

SUBSAMPLE_N <- 3000   # respondents per draw
N_DRAWS     <- as.integer(Sys.getenv("N_DRAWS", "10"))  # independent draws per table
MIN_N       <- 1000   # respondents left after prep(); below this the IMV is noise
# Codes that mark a non-response, not an option (ENEM's blank and double mark).
# They are dropped before rare options are pooled: left to the RARE rule, they
# enter the option models as a distractor, alone or pooled with rare options,
# on any item where they reach 1% of responses (14 of the 44 items in
# enem_2023_1mil_mt_nom).
NONRESPONSE <- c(".", "*")
N_FOLDS     <- 5
RARE        <- 0.01   # a wrong option chosen by fewer than 1% is pooled
MIN_ITEMS   <- 10
DENSE_ITEMS <- 40     # sparse tables: the most-answered items kept (see fetch_one)
WORKERS     <- as.integer(Sys.getenv("WORKERS", min(4, parallel::detectCores() %/% 2)))
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

# Cache and result files carry N_DRAWS in their names, so a resumed run never
# mixes draws from runs with a different number of them.
draws_file <- function(dir, tn, ext = ".rds") file.path(dir, sprintf("%s_draws%d%s", tn, N_DRAWS, ext))

fetch_one <- function(tn) {
  f <- draws_file(cache_dir, tn)
  if (file.exists(f)) return(invisible(f))
  message("fetch ", tn)
  n <- irw_fetch(tn, source = "nom")
  if (!all(c("resp", "text") %in% names(n))) {
    saveRDS(list(table = tn, skipped = "no resp/text columns (unscored free text)",
                 date_run = as.character(Sys.Date())), draws_file(fits_dir, tn))
    return(invisible(NULL))
  }
  n <- data.table::as.data.table(n)[!is.na(text) & !is.na(resp), .(id, item, resp, text)]
  # Repeated attempts (EdNet, the adaptive practice systems): keep one per cell.
  n <- unique(n, by = c("id", "item"))
  # irw_fetch() does not return rows in a fixed order, and the draws and folds
  # below follow row order: without sorting, two downloads of the same table give
  # different draws from the same seed.
  data.table::setorder(n, id, item)
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
                   date_run = as.character(Sys.Date())), draws_file(fits_dir, tn))
      return(invisible(NULL))
    }
  }
  # N_DRAWS independent draws of up to SUBSAMPLE_N respondents. A table with no
  # more than SUBSAMPLE_N respondents gives the same people in every draw, so its
  # draws differ only in the split into folds.
  ids <- unique(n$id)
  draws <- lapply(seq_len(N_DRAWS), function(d) {
    as.data.frame(if (length(ids) > SUBSAMPLE_N) n[id %in% sample(ids, SUBSAMPLE_N)] else n)
  })
  attr(draws, "n_full") <- n_full
  attr(draws, "dense_block") <- if (dense) c(bank = n_bank, block = length(top), persons = length(ids)) else NULL
  saveRDS(draws, f)
  rm(n, draws); gc()
  invisible(f)
}
for (tn in cands) tryCatch(fetch_one(tn), error = function(e) message("  fetch failed: ", tn, ": ", conditionMessage(e)))

# ------------------------------------------------------------------------------
# 3. Long -> two wide matrices: the option chosen (key = highest code) and 0/1.
# ------------------------------------------------------------------------------

prep <- function(n) {
  n$text <- as.character(n$text)
  # Respondents in their order of first appearance, taken before any cell is
  # dropped: the folds are drawn over the matrix cells in this order.
  ids <- unique(n$id)
  # Non-response codes are not options: drop those cells before anything is
  # counted, so they reach neither the option models nor the 0/1 matrix.
  nonresp <- n$text %in% NONRESPONSE
  n_cells_nonresponse <- sum(nonresp)
  n <- n[!nonresp, ]
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
  items <- sort(names(keys))
  opt <- x01 <- matrix(NA_integer_, length(ids), length(items), dimnames = list(as.character(ids), items))
  n_pooled <- 0L; n_cells_dropped <- 0L
  for (it in items) {
    d <- n[n$item == it, ]
    k <- keys[[it]]
    share <- table(d$text) / nrow(d)
    rare <- setdiff(names(share)[share < RARE], k)
    if (length(rare) > 1) n_pooled <- n_pooled + 1L
    d$opt <- ifelse(d$text %in% rare, "_rare", d$text)
    # A pooled category that is itself under RARE is too thin to estimate:
    # drop it.
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
       n_cells_dropped = n_cells_dropped, n_cells_nonresponse = n_cells_nonresponse)
}

# ------------------------------------------------------------------------------
# 4. The polytomous IMV, omega_c (Kanopka, 2023, ch. 3). For each pair of options
#    on an item, it takes the held-out cells where one of the two was chosen and
#    computes a binary IMV for which of the two it was, from each model's
#    probabilities of the two options rescaled to sum to one. Within an option,
#    its pairwise IMVs are averaged weighting by how many cells each pair has;
#    the options are then averaged weighting by how often each was chosen on the
#    item's training cells. Every piece is a binary IMV, so omega_c stays on the
#    binary IMV's scale whatever the number of options. It is computed per item
#    on all the held-out cells of a draw (the five folds together, as for the
#    binary IMV above) and averaged over items. ben-domingue/imvpolly computes it
#    fold by fold and averages the folds; that version is kept as a check
#    (omega_foldwise), and by_fold holds the per-fold values.
#
#    irtimv_imv() and irtimv_imv_c() are imv() and imv_c() from
#    ben-domingue/irtimv (irtimv-helper/R/00_funs.R), unchanged except for their
#    names (the imv package already exports an imv()) and one line in
#    irtimv_imv_c() that skips a pair of options with no cells, which cannot be
#    scored: on some items and thirds of theta two rare options are both absent.
# ------------------------------------------------------------------------------

irtimv_imv<-function(pr,p1,p2,eps=1e-6) {
    pr$pv1<-pr[[p1]]
    pr$pv2<-pr[[p2]]
    pr$pv1<-ifelse(pr$pv1 < eps,eps,pr$pv1)
    pr$pv2<-ifelse(pr$pv2 < eps,eps,pr$pv2)
    pr$pv1<-ifelse(pr$pv1 > 1-eps,1-eps,pr$pv1)
    pr$pv2<-ifelse(pr$pv2 > 1-eps,1-eps,pr$pv2)
    ##
    ll<-function(x,p='pv') {
        z<-log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
        z<-sum(z)/nrow(x)
        exp(z)
    }
    loglik1<-ll(pr,'pv1')
    loglik2<-ll(pr,'pv2')
    getcoins<-function(a) {
        f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
        nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
    }
    c1<-getcoins(loglik1)
    c2<-getcoins(loglik2)
    ew<-function(p1,p0) (p1-p0)/p0
    imv<-ew(c2,c1)
    imv
}

irtimv_imv_c<-function(y,pctt.tab,p1,p2) {
    nn<-length(pctt.tab)
    om<-numeric()
    iis<-0:(nn-1)
    for (ii in iis) {
        ns<-om.tmp<-numeric()
        jjs<-iis[-match(ii,iis)]
        for (jj in jjs) {
            y2<-y[y$resp %in% c(ii,jj),]
            if (nrow(y2) == 0) next   # added: a pair with no cells cannot be scored
            resp<-ifelse(y2$resp==ii,1,0)
            ##irt p-values for being
            p1.ii<-y2[[paste(p1,ii,sep='')]]
            p1.jj<-y2[[paste(p1,jj,sep='')]]
            p2.ii<-y2[[paste(p2,ii,sep='')]]
            p2.jj<-y2[[paste(p2,jj,sep='')]]
            ##
            z<-data.frame(resp=resp,
                          p1=p1.ii/(p1.ii+p1.jj),
                          p2=p2.ii/(p2.ii+p2.jj)
                          )
            j0<-as.character(jj)
            om.tmp[j0]<-irtimv_imv(z,p1="p1",p2="p2")
            ns[as.character(jj)]<-nrow(z)
        }
        om[ii+1]<-sum(om.tmp*ns)/sum(ns)
    }
    sum(om*pctt.tab)/sum(pctt.tab)
}

# omega_c for one item. y: the option chosen (1..K) on a set of held-out cells;
# P0, P1: the two models' probabilities of the K options on those cells; w: how
# often each option was chosen on the item's training cells.
omega_item <- function(y, P0, P1, w) {
  d <- data.frame(resp = y - 1)
  for (k in seq_along(w)) {
    d[[paste0("a", k - 1)]] <- P0[, k]
    d[[paste0("b", k - 1)]] <- P1[, k]
  }
  irtimv_imv_c(d, w, "a", "b")
}

# The six omega_c comparisons on a set of held-out cells: the option chosen
# (cat_*), over all cells and options, and which wrong answer (wrong_*), over
# the cells answered wrongly and the wrong options only. P holds each model's
# probabilities of every option, one row per cell (NA beyond an item's options);
# w and sh hold, per item, the training proportions of every option and of the
# wrong options. In a pair of wrong options the 2PL + shares baseline gives
# share_i / (share_i + share_j), which knows nothing about theta. Items are
# averaged with equal weight for the option chosen, as in imvpolly, and weighted
# by their wrong answers for which wrong answer, so that an easy item with a few
# dozen wrong answers counts less than a hard one with hundreds. An item with no
# held-out wrong answer drops out of the second.
CAT_PAIRS <- list(cat_rasch_to_2pl = c("rasch", "twopl"), cat_twopl_to_nom = c("twopl", "nominal"),
                  cat_twopl_to_nest = c("twopl", "nested"), cat_nest_to_nom = c("nested", "nominal"))
WRONG_PAIRS <- list(wrong_shares_to_nom = c("twopl", "nominal"), wrong_shares_to_nest = c("twopl", "nested"))
omegas <- function(cells, P, w, sh) {
  by_item <- split(seq_len(nrow(cells)), cells$item)
  one <- function(pairs, wrong_only) sapply(pairs, function(m) {
    v <- n <- numeric(0)
    for (j in names(by_item)) {
      r <- by_item[[j]]; K <- cells$K[r[1]]
      if (wrong_only) r <- r[cells$opt[r] != K]
      if (!length(r)) next
      k <- seq_len(if (wrong_only) K - 1 else K)
      v <- c(v, omega_item(cells$opt[r], P[[m[1]]][r, k, drop = FALSE], P[[m[2]]][r, k, drop = FALSE],
                           (if (wrong_only) sh else w)[[j]][k]))
      n <- c(n, length(r))
    }
    if (wrong_only) sum(v * n) / sum(n) else mean(v)
  })
  c(one(CAT_PAIRS, FALSE), one(WRONG_PAIRS, TRUE))
}

# ------------------------------------------------------------------------------
# 4b. The nominal model with a1 fixed at 1. mirt's default writes the slope of
#     option k as a1 * ak, with ak fixed for the reference option (0) and for the
#     last option (K - 1, the key here). Fixing a1 at 1 and freeing the last ak
#     gives the same model without the product. On these data the default stops
#     below the maximum while reporting convergence: the EM drifts into a valley
#     with a1 near 0 and large ak and moves through it more slowly than mirt's
#     tolerance. The parameterization here reaches the maximum.
# ------------------------------------------------------------------------------

nominal_pars <- function(opt_tr) {
  pv <- mirt(opt_tr, 1, "nominal", pars = "values")
  pv$value[pv$name == "a1"] <- 1
  pv$est[pv$name == "a1"] <- FALSE
  for (it in unique(pv$item[pv$item != "GROUP"])) {
    ak <- which(pv$item == it & grepl("^ak\\d+$", pv$name))
    # The last option mirt numbers on this item (the key). Looked up in mirt's
    # own table, since an option missing from a fold's training cells shifts
    # the numbering.
    pv$est[ak[which.max(as.integer(sub("^ak", "", pv$name[ak])))]] <- TRUE
  }
  pv
}

# ------------------------------------------------------------------------------
# 5. Cross-validated comparison for one table.
# ------------------------------------------------------------------------------

# The four binary IMVs for a set of held-out cells: right/wrong.
imvs_binary <- function(d) c(
  rasch_to_2pl   = imv.binary(d$y, d$rasch, d$twopl),
  twopl_to_nom   = imv.binary(d$y, d$twopl, d$nominal),
  twopl_to_nest  = imv.binary(d$y, d$twopl, d$nested),
  nest_to_nom    = imv.binary(d$y, d$nested, d$nominal)
)

# Proportion of each option among an item's cells (all options, or the wrong
# options only), from a person x item matrix of options.
option_props <- function(m, ncat, wrong_only = FALSE) {
  setNames(lapply(seq_len(ncol(m)), function(j) {
    z <- m[, j]; z <- z[!is.na(z) & (!wrong_only | z != ncat[j])]
    tabulate(z, ncat[j]) / length(z)
  }), seq_len(ncol(m)))
}

# One draw: prep, five-fold cross-validation and the IMVs. It returns a reason
# string when the draw cannot be fitted.
fit_draw <- function(n, d) {
  P <- prep(n)
  if (is.character(P)) return(P)
  if (nrow(P$opt) < MIN_N) return(paste0("N = ", nrow(P$opt), ", below ", MIN_N))
  opt <- P$opt; x01 <- P$x01
  ncat <- apply(opt, 2, max, na.rm = TRUE)
  obs <- which(!is.na(opt))
  fold <- sample(rep_len(seq_len(N_FOLDS), length(obs)))

  per_fold <- lapply(seq_len(N_FOLDS), function(k) {
    ho <- obs[fold == k]
    opt_tr <- opt; opt_tr[ho] <- NA
    x_tr <- x01; x_tr[ho] <- NA
    mods <- list(
      rasch   = mirt(x_tr, 1, "Rasch", verbose = FALSE),
      twopl   = mirt(x_tr, 1, "2PL", verbose = FALSE),
      # The option models need more than mirt's default 500 EM cycles on the
      # long tests (the residency exams, ENEM math).
      nominal = mirt(opt_tr, 1, "nominal", pars = nominal_pars(opt_tr), verbose = FALSE,
                     technical = list(NCYCLES = 2000)),
      nested  = mirt(opt_tr, 1, "2PLNRM", key = ncat, verbose = FALSE, technical = list(NCYCLES = 2000))
    )
    conv <- sapply(mods, function(m) extract.mirt(m, "converged"))
    # First draw only: mirt's default nominal parameterization on the same
    # cells, to record how far below the maximum it stops. Not used for any IMV.
    nominal_check <- if (d == 1) {
      def <- mirt(opt_tr, 1, "nominal", verbose = FALSE, technical = list(NCYCLES = 2000))
      c(loglik_a1 = extract.mirt(mods$nominal, "logLik"), loglik_default = extract.mirt(def, "logLik"),
        iter_a1 = extract.mirt(mods$nominal, "iterations"), iter_default = extract.mirt(def, "iterations"),
        converged_default = extract.mirt(def, "converged"))
    }
    rc <- arrayInd(ho, dim(opt))
    y_opt <- opt[ho]
    # Wrong-option shares on this fold's training cells: how the 2PL and Rasch
    # spread their P(wrong) when they are asked about options.
    shares <- option_props(opt_tr, ncat, wrong_only = TRUE)
    probs <- list()
    preds <- lapply(names(mods), function(m) {
      th <- fscores(mods[[m]], method = "EAP")[, 1]
      binary <- m %in% c("rasch", "twopl")
      p_key <- p_obs <- numeric(length(ho))
      Pm <- matrix(NA_real_, length(ho), max(ncat))   # every option's probability, for omega_c
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
        Pm[w, seq_len(ncat[j])] <- P
      }
      probs[[m]] <<- Pm
      setNames(data.frame(p_key, p_obs), paste0(c("", "obs_"), m))
    })
    th2 <- fscores(mods$twopl, method = "EAP")[, 1]
    cells <- data.frame(y = x01[ho], K = ncat[rc[, 2]], item = rc[, 2], opt = y_opt, do.call(cbind, preds),
                        theta_2pl = th2[rc[, 1]], fold = k)
    list(cells = cells, conv = conv, probs = probs, nominal_check = nominal_check,
         # omega_c on this fold, weighted by this fold's training proportions.
         omega = omegas(cells, probs, option_props(opt_tr, ncat), shares))
  })
  converged <- t(sapply(per_fold, `[[`, "conv"))
  cells <- do.call(rbind, lapply(per_fold, `[[`, "cells"))
  probs <- setNames(lapply(names(per_fold[[1]]$probs), function(m)
    do.call(rbind, lapply(per_fold, function(f) f$probs[[m]]))), names(per_fold[[1]]$probs))
  cuts <- quantile(cells$theta_2pl, c(0, 1/3, 2/3, 1))
  cells$tercile <- cut(cells$theta_2pl, cuts, include.lowest = TRUE, labels = c("low", "mid", "high"))
  cells$draw <- d
  omega_folds <- do.call(rbind, lapply(per_fold, `[[`, "omega"))   # folds x comparisons
  # omega_c on all of the draw's held-out cells, overall and within thirds of
  # theta, weighted by the options' proportions over all of the draw's cells
  # (each cell trains four of the five folds' models).
  w_all <- option_props(opt, ncat); sh_all <- option_props(opt, ncat, wrong_only = TRUE)
  by_tercile <- t(sapply(levels(cells$tercile), function(tc) {
    r <- which(cells$tercile == tc)
    c(imvs_binary(cells[r, ]), omegas(cells[r, ], lapply(probs, function(M) M[r, , drop = FALSE]), w_all, sh_all))
  }))
  list(
    cells = cells, converged = converged,
    probs = if (d == 1) probs,   # kept for the first draw only, to check omega_c
    nominal_check = if (d == 1) do.call(rbind, lapply(per_fold, `[[`, "nominal_check")),
    n_persons = nrow(opt), n_items = ncol(opt), p_correct = mean(x01, na.rm = TRUE),
    n_options = table(ncat),
    n_items_dropped = P$n_items_dropped, n_persons_dropped = P$n_persons_dropped,
    n_cells_dropped = P$n_cells_dropped, n_cells_nonresponse = P$n_cells_nonresponse,
    imv_all = c(imvs_binary(cells), omegas(cells, probs, w_all, sh_all)),
    omega_foldwise = colMeans(omega_folds),   # imvpolly's way, as a check
    by_fold = cbind(t(sapply(split(cells, cells$fold), imvs_binary)), omega_folds),
    by_tercile = by_tercile
  )
}

fit_one <- function(tn) {
  out_file <- draws_file(fits_dir, tn)
  if (file.exists(out_file)) return(readRDS(out_file))
  if (tn %in% names(EXCLUDE)) {
    res <- list(table = tn, skipped = EXCLUDE[[tn]], date_run = as.character(Sys.Date()))
    saveRDS(res, out_file); return(res)
  }
  f <- draws_file(cache_dir, tn)
  # No draws means the fetch failed: list the table as skipped rather than drop it.
  if (!file.exists(f)) return(list(table = tn, skipped = "fetch failed (see the run's messages)",
                                   date_run = as.character(Sys.Date())))
  draws <- readRDS(f)
  t0 <- Sys.time()

  # The first draw decides whether the table is fitted at all, with the same
  # skip reasons as before. A later draw that cannot be fitted (rare: prep()
  # leaves too few items or respondents) is left out, and n_draws says so.
  per_draw <- list()
  for (d in seq_along(draws)) {
    fd <- fit_draw(draws[[d]], d)
    if (is.character(fd)) {
      if (d > 1) next
      res <- list(table = tn, skipped = fd, date_run = as.character(Sys.Date()))
      saveRDS(res, out_file); return(res)
    }
    per_draw[[length(per_draw) + 1]] <- fd
  }
  D <- length(per_draw)
  cells <- do.call(rbind, lapply(per_draw, `[[`, "cells"))
  saveRDS(list(cells = cells, probs_draw1 = per_draw[[1]]$probs), draws_file(cells_dir, tn))

  # Means over draws, and standard errors from the draw-to-draw spread.
  se <- function(x) if (D > 1) sd(x) / sqrt(D) else NA_real_
  per <- function(field) sapply(per_draw, `[[`, field)
  imv_draws  <- do.call(rbind, lapply(per_draw, `[[`, "imv_all"))         # draws x comparisons
  terc_draws <- simplify2array(lapply(per_draw, `[[`, "by_tercile"))      # thirds x comparisons x draws

  res <- list(
    table = tn, skipped = NA_character_, n_draws = D,
    n_full = attr(draws, "n_full"), n_persons = round(mean(per("n_persons"))),
    n_items = per_draw[[1]]$n_items, p_correct = mean(per("p_correct")),
    n_options = per_draw[[1]]$n_options,
    # Per-draw counts, one value per draw.
    n_persons_by_draw = per("n_persons"), n_items_by_draw = per("n_items"),
    n_items_dropped = per("n_items_dropped"), n_persons_dropped = per("n_persons_dropped"),
    n_cells_dropped = per("n_cells_dropped"), n_cells_nonresponse = per("n_cells_nonresponse"),
    dense_block = attr(draws, "dense_block"),
    converged = do.call(rbind, lapply(per_draw, `[[`, "converged")),      # (draws x folds) x models
    nominal_check = per_draw[[1]]$nominal_check,                         # first draw: folds x (log-likelihoods, iterations)
    omega_foldwise = do.call(rbind, lapply(per_draw, `[[`, "omega_foldwise")),  # draws x comparisons, imvpolly's way
    imv_all = colMeans(imv_draws), imv_se = apply(imv_draws, 2, se), imv_draws = imv_draws,
    imv_by_fold = do.call(rbind, lapply(per_draw, `[[`, "by_fold")),      # (draws x folds) x comparisons
    imv_by_tercile = apply(terc_draws, c(1, 2), mean),
    imv_by_tercile_se = apply(terc_draws, c(1, 2), se),
    seconds = as.numeric(Sys.time() - t0, units = "secs"),
    date_run = as.character(Sys.Date()),
    irw_pkg = as.character(packageVersion("irw")),
    mirt_pkg = as.character(packageVersion("mirt"))
  )
  saveRDS(res, out_file)
  res
}

# furrr derives each table's seed, and so its folds, from the session's RNG
# state. fetch_one() moves that state only when it takes new draws, so without
# this a run on cached draws would split the folds differently from a fresh run.
set.seed(20260930)
plan(multisession, workers = WORKERS)
# scheduling = Inf hands out one table at a time, so the slow tables (the four
# ENEM tests and the residency exams sit next to each other in cands) do not
# queue on one worker. Each table keeps its own seed, so results are unchanged.
results <- future_map(cands, function(tn) tryCatch(fit_one(tn), error = function(e)
  list(table = tn, skipped = paste("error:", conditionMessage(e)))),
  .options = furrr_options(seed = TRUE, scheduling = Inf))
plan(sequential)
results <- Filter(Negate(is.null), results)
date_run <- as.Date(max(sapply(results, function(r) r$date_run %||% NA_character_), na.rm = TRUE))
fitted <- Filter(function(r) is.na(r$skipped), results)
summary_df <- bind_rows(lapply(fitted, function(r) tibble::tibble(
  table = r$table, n_full = r$n_full, n_persons = r$n_persons, n_items = r$n_items,
  p_correct = r$p_correct, dense_block = !is.null(r$dense_block), n_draws = r$n_draws,
  !!!as.list(r$imv_all),
  !!!setNames(as.list(r$imv_se), paste0(names(r$imv_se), "_se"))
)))
saveRDS(list(
  summary          = summary_df,
  results          = results,   # per-table detail: draws, folds, terciles, convergence, skips
  candidate_tables = cands,
  n_all_candidates = length(all_cands),
  n_draws          = N_DRAWS,
  pilot            = PILOT || nzchar(Sys.getenv("TABLES")),   # a TABLES run is partial too
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
