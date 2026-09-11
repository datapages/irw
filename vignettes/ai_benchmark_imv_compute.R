# ai_benchmark_imv_compute.R
#
# Does the extra structure of a 2PL, or of a second factor, pay for itself in
# prediction the same way when the respondents are AI models as when they are
# people? This script fits the same InterModel Vigorish (IMV) comparison --
# Rasch -> 2PL and 2PL -> 2-factor 2PL, scored on held-out responses -- to
# item-level AI benchmark results from OpenEval and to human IRW tables
# subsampled to the same number of respondents and items.
#
# AI data is read from Hugging Face (Open-Eval-Commons/OpenEval), pinned to one
# dataset revision. It is not in IRW and is licensed CC BY-NC 4.0, so raw
# responses stay in a local cache outside this repository; only fit summaries
# are written here. See datapages/irw#186.
#
# Human comparison: only a handful of binary IRW tables are naturally the size
# of these benchmarks (~120-150 respondents x 400+ items), so instead each
# human table is repeatedly subsampled to the benchmark's respondent count and
# a common item count. Human sources are right/wrong-scored tests, so every
# item is keyed in the same direction -- the positive slope prior below would
# misfit reverse-keyed personality items and inflate the 2PL gain.
#
# Output: ai_benchmark_imv_data/fits/<matrix>.rds        (one per matrix)
#         ai_benchmark_imv_data/ai_benchmark_imv_results.rds
#         ai_benchmark_imv_data/references.bib
#
# Usage (from project root):
#   Rscript vignettes/ai_benchmark_imv_compute.R          # full pipeline
#   Rscript vignettes/ai_benchmark_imv_compute.R smoke    # one small fit, no output
#   Rscript vignettes/ai_benchmark_imv_compute.R combine  # no fitting; combine fits on disk
#   Rscript vignettes/ai_benchmark_imv_compute.R only=choi  # fit matching matrices only

library(irw)
library(mirt)
library(arrow)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

args  <- commandArgs(trailingOnly = TRUE)
SMOKE <- "smoke" %in% args

HF_REPO     <- "Open-Eval-Commons/OpenEval"
HF_REVISION <- "23a1ded985b3c2cdaaddb27581a35bfabe0ad7e7"  # head as of 2026-09-05
HF_SUMMARY  <- "data_summary_090526.json"
CACHE_DIR   <- path.expand(Sys.getenv("OPENEVAL_CACHE",
                                      file.path("~/.cache/irw/openeval", HF_REVISION)))

out_dir  <- "vignettes/ai_benchmark_imv_data"
fits_dir <- file.path(out_dir, "fits")
bib_file <- file.path(out_dir, "references.bib")

PILOT <- TRUE  # TRUE: two benchmarks, three human sources, two draws -- a draft page

# `file` is the shard prefix on HF (underscores); names are OpenEval's (hyphens).
# `score` turns the per-response score into 0/1.
BENCHMARKS <- list(
  "ifeval" = list(
    file   = "ifeval",
    metric = "ifeval_strict_accuracy",
    # The stored value is the share of a prompt's instructions followed
    # (0, 1/3, 1/2, 2/3, 1). IFEval's prompt-level strict accuracy counts a
    # prompt as passed only when every instruction is followed.
    score  = function(v) as.integer(v == 1),
    coding = "prompt-level strict accuracy: 1 only if every instruction in the prompt is followed"
  ),
  "sorry-bench" = list(
    file   = "sorry_bench",
    metric = "ft-mistral-7b-sorrybench",
    score  = function(v) { stopifnot(all(v %in% c(0, 1))); as.integer(v) },
    coding = "fine-tuned Mistral-7B judge: 1 = the model complied with an unsafe request"
  )
)
# vermeiren_2022_vocab (236 x 668) was dropped after the first run: its 668
# item names are ~483 words with whitespace variants, and 57k (id, word) cells
# are duplicated, 14.6k of them with conflicting responses. The check in
# human_file() now stops on that. No other dense right/wrong IRW table has
# 300+ items apart from further forms of the CMSCE exam.
HUMAN_TABLES <- c("choi_2026_cmsce_2019_1", "previc_bohn2023")

if (!PILOT) stop("Full run not specified yet -- see datapages/irw#186.")

MIN_COVERAGE <- 0.90  # models must have answered this share of a benchmark's items
MIN_OUTCOME  <- 5     # an item needs at least this many 0s and 1s to be kept
J_COMMON     <- 300   # item count shared by every subsampled matrix
N_DRAWS      <- 2     # draws per (benchmark, source)
NFOLD        <- 5

# ==============================================================================
# 1. Fitting helpers (shared by AI and human matrices)
# ==============================================================================

# A stable integer seed per matrix, so a single matrix can be rerun on its own.
seed_for <- function(key) {
  x <- utf8ToInt(key)
  as.integer(sum(x * seq_along(x)) %% .Machine$integer.max)
}

# Items with at least MIN_OUTCOME of each response.
varying_items <- function(df) {
  df |>
    group_by(item) |>
    summarise(n1 = sum(resp == 1), n0 = sum(resp == 0), .groups = "drop") |>
    filter(n1 >= MIN_OUTCOME, n0 >= MIN_OUTCOME) |>
    pull(item)
}

# Model specifications. The 2PL and the second factor get the same lognormal
# slope prior used in imv.qmd; the second-factor slopes get a normal prior so
# they can be negative. Item 1 loads only on F1, mirt's usual exploratory
# identification. Predictions do not depend on rotation.
fit_models <- function(items) {
  J <- ncol(items)
  ctl <- list(NCYCLES = 10000)
  m_rasch <- mirt(items, 1, itemtype = "Rasch", verbose = FALSE)
  m_2pl <- mirt(items,
                mirt.model(paste0("F = 1-", J, "\n",
                                  "PRIOR = (1-", J, ", a1, lnorm, 0.0, 1.0)")),
                itemtype = "2PL", method = "EM", technical = ctl, verbose = FALSE)
  m_2f <- mirt(items,
               mirt.model(paste0("F1 = 1-", J, "\n",
                                 "F2 = 2-", J, "\n",
                                 "PRIOR = (1-", J, ", a1, lnorm, 0.0, 1.0),",
                                 " (2-", J, ", a2, norm, 0.0, 1.0)")),
               itemtype = "2PL", method = "EM", technical = ctl, verbose = FALSE)
  list(rasch = m_rasch, `2pl` = m_2pl, `2f` = m_2f)
}

# P(resp = 1) for held-out (id, item) rows from a model fit to `wide`.
# irw_predict() only handles unidimensional models, so all three models go
# through this one path: EAP scores, then each item's trace line.
predict_test <- function(model, wide, test) {
  theta <- fscores(model, method = "EAP", verbose = FALSE)
  row   <- match(test$id, wide$id)
  cols  <- colnames(model@Data$data)
  p <- numeric(nrow(test))
  for (j in unique(test$item)) {
    k   <- match(paste0("item_", j), cols)
    idx <- which(test$item == j)
    tr  <- probtrace(extract.item(model, k), theta[row[idx], , drop = FALSE])
    p[idx] <- tr[, 2]
  }
  p
}

# Cross-validated IMV for one long 0/1 matrix. One response-level fold
# assignment is shared by all three models, so both comparisons are scored on
# identical held-out responses.
cv_imv <- function(df, nfold = NFOLD) {
  fold <- sample(rep_len(seq_len(nfold), nrow(df)))
  per_fold <- vector("list", nfold)
  for (k in seq_len(nfold)) {
    train <- df[fold != k, ]
    test  <- df[fold == k, ]

    # An item that happens to lose all its 0s or 1s to the test fold can't be
    # fit; drop it from this fold for every model alike.
    keep  <- train |> group_by(item) |> summarise(v = n_distinct(resp) == 2, .groups = "drop") |>
      filter(v) |> pull(item)
    n_item_dropped <- n_distinct(train$item) - length(keep)
    train <- train[train$item %in% keep, ]
    test  <- test[test$item %in% keep & test$id %in% train$id, ]

    wide  <- suppressMessages(irw_long2resp(train, id_density_threshold = NULL, agg_method = "first"))
    items <- wide[, setdiff(names(wide), "id"), drop = FALSE]

    t0   <- Sys.time()
    fits <- fit_models(items)
    p    <- map(fits, predict_test, wide = wide, test = test)

    per_fold[[k]] <- tibble(
      fold            = k,
      n_test          = nrow(test),
      n_item_dropped  = n_item_dropped,
      imv_rasch_2pl   = irw_imv(test$resp, p$rasch, p$`2pl`),
      imv_2pl_2f      = irw_imv(test$resp, p$`2pl`, p$`2f`),
      converged_rasch = extract.mirt(fits$rasch, "converged"),
      converged_2pl   = extract.mirt(fits$`2pl`, "converged"),
      converged_2f    = extract.mirt(fits$`2f`, "converged"),
      secs            = as.numeric(difftime(Sys.time(), t0, units = "secs"))
    )
  }
  bind_rows(per_fold)
}

# Build one matrix from its long source data, fit it, return a summary.
# `n` = respondents to sample (NULL keeps all); `j` = items to sample after
# the variation filter (NULL keeps all that pass).
fit_matrix <- function(spec) {
  set.seed(seed_for(spec$key))
  df <- readRDS(spec$data_file)

  if (!is.null(spec$n)) {
    ids <- sample(unique(df$id), spec$n)
    df  <- df[df$id %in% ids, ]
  }
  passing <- varying_items(df)
  n_item_filtered <- n_distinct(df$item) - length(passing)
  # Two human sources have barely more than J_COMMON items, and a subsample of
  # ~120-150 people leaves some of them near-constant. Those draws keep every
  # item that passes (J is recorded) rather than failing.
  if (!is.null(spec$j) && length(passing) > spec$j) {
    passing <- sample(passing, spec$j)
  }
  df <- df[df$item %in% passing, c("id", "item", "resp")]

  folds <- cv_imv(df)
  list(
    summary = tibble(
      key             = spec$key,
      source          = spec$source,
      benchmark       = spec$benchmark,
      table           = spec$table,
      draw            = spec$draw,
      N               = n_distinct(df$id),
      J               = n_distinct(df$item),
      J_target        = if (is.null(spec$j)) NA_integer_ else as.integer(spec$j),
      n_obs           = nrow(df),
      density         = nrow(df) / (n_distinct(df$id) * n_distinct(df$item)),
      p_correct       = mean(df$resp),
      n_item_filtered = n_item_filtered,
      imv_rasch_2pl   = mean(folds$imv_rasch_2pl),
      imv_2pl_2f      = mean(folds$imv_2pl_2f),
      all_converged   = all(folds$converged_rasch, folds$converged_2pl, folds$converged_2f),
      minutes         = sum(folds$secs) / 60
    ),
    folds    = mutate(folds, key = spec$key, .before = 1),
    date_run = Sys.Date()
  )
}

if (SMOKE) {
  # One human draw at reduced size with two folds: exercises the 2-factor
  # prediction path and the priors in about a minute. Writes nothing.
  set.seed(1)
  df  <- irw_fetch("previc_bohn2023")[, c("id", "item", "resp")]
  ids <- sample(unique(df$id), 120)
  df  <- df[df$id %in% ids, ]
  df  <- df[df$item %in% sample(varying_items(df), 100), ]
  print(system.time(res <- cv_imv(df, nfold = 2)))
  print(res)
  quit(save = "no")
}

# ==============================================================================
# 2. OpenEval: download the pinned revision, reduce to long 0/1
#    R arrow can't read parquet over HTTP, so shards are fetched once into
#    CACHE_DIR. Almost all of their bytes are model output text, which is
#    never read.
# ==============================================================================

hf_url <- function(path) {
  sprintf("https://huggingface.co/datasets/%s/resolve/%s/%s", HF_REPO, HF_REVISION, path)
}

hf_files <- function(dir) {
  api <- sprintf("https://huggingface.co/api/datasets/%s/tree/%s/%s", HF_REPO, HF_REVISION, dir)
  jsonlite::fromJSON(api)[, c("path", "size")]
}

hf_download <- function(path, size = NULL) {
  dest <- file.path(CACHE_DIR, path)
  if (file.exists(dest) && (is.null(size) || file.size(dest) == size)) return(dest)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  message("  Downloading ", path)
  tmp <- paste0(dest, ".part")
  curl::curl_download(hf_url(path), tmp, quiet = TRUE)
  if (!is.null(size) && file.size(tmp) != size) stop("Size mismatch for ", path)
  file.rename(tmp, dest)
  dest
}

# Collapse obvious aliases: case, provider suffixes like ":0", and trailing
# dates or "-preview". Used only to report how many distinct models remain.
alias_key <- function(x) {
  x <- tolower(x)
  x <- sub(":[0-9]+$", "", x)
  x <- sub("-preview(-[0-9]{2}-[0-9]{4})?$", "", x)
  x <- sub("-(20[0-9]{2}-?[0-9]{2}-?[0-9]{2}|[0-9]{4})$", "", x)
  x
}

summary_json <- jsonlite::fromJSON(hf_download(HF_SUMMARY), simplifyVector = FALSE)
listing      <- bind_rows(hf_files("response"), hf_files("item"))

prep_benchmark <- function(bench) {
  cfg <- BENCHMARKS[[bench]]
  out <- file.path(CACHE_DIR, "long", paste0(cfg$file, ".rds"))

  shards <- listing |> filter(startsWith(path, paste0("response/", cfg$file, "-")))
  item_f <- listing |> filter(startsWith(path, paste0("item/", cfg$file, "-")))
  files  <- map2_chr(shards$path, shards$size, hf_download)
  items  <- read_parquet(hf_download(item_f$path, item_f$size), col_select = "item_id")$item_id

  if (file.exists(out)) return(readRDS(out)$counts)
  message("Preparing ", bench)

  # col_select mangles nested columns in arrow 25; select through a dataset.
  raw <- open_dataset(files) |> select(response_id, model, scores) |> collect()

  model <- raw$model$name
  stem  <- sub("_[0-9]+$", "", raw$response_id)   # replicate suffix _N
  run   <- as.integer(sub(".*_", "", raw$response_id))
  stopifnot(all(endsWith(stem, paste0("_", model))))
  item  <- substr(stem, 1, nchar(stem) - nchar(model) - 1)
  stopifnot(all(item %in% items))

  metric_ok <- map_lgl(raw$scores$metric, \(m) all(m$name == cfg$metric))
  stopifnot(all(metric_ok))
  # A few responses carry the same metric twice; keep them only if they agree.
  vals  <- raw$scores$value
  agree <- map_lgl(vals, \(v) length(unique(v)) == 1)
  value <- map_dbl(vals, 1)

  gen <- raw$model$model_adaptation$generation_parameters
  temperature <- map_dbl(gen, \(g) {
    t <- tryCatch(jsonlite::fromJSON(g)$temperature, error = function(e) NULL)
    if (is.null(t)) NA_real_ else as.numeric(t)
  })

  all_rows <- tibble(id = model, item, run, value, agree, temperature)
  long <- all_rows |>
    filter(run == 0, agree) |>
    distinct(id, item, .keep_all = TRUE) |>
    mutate(resp = cfg$score(value))

  coverage <- long |> count(id) |> mutate(coverage = n / length(items))
  keep_ids <- coverage$id[coverage$coverage >= MIN_COVERAGE]
  long     <- long |> filter(id %in% keep_ids) |> select(id, item, resp, temperature)

  summary_cov <- map_dbl(summary_json[[bench]], "coverage")
  counts <- tibble(
    benchmark           = bench,
    metric              = cfg$metric,
    coding              = cfg$coding,
    n_rows              = nrow(all_rows),
    n_replicate_rows    = sum(all_rows$run != 0),
    n_disagreeing       = sum(!all_rows$agree),
    n_fractional        = sum(!all_rows$value %in% c(0, 1)),
    n_models_all        = n_distinct(all_rows$id),
    n_models            = length(keep_ids),
    n_models_summary    = sum(summary_cov >= 100 * MIN_COVERAGE),
    n_models_dealiased  = n_distinct(alias_key(keep_ids)),
    n_items             = length(items),
    n_items_varying     = length(varying_items(long)),
    p_correct           = mean(long$resp),
    temperatures        = paste(sort(unique(long$temperature)), collapse = ", "),
    n_models_nongreedy  = n_distinct(long$id[!is.na(long$temperature) & long$temperature > 0])
  )
  if (counts$n_models != counts$n_models_summary) {
    warning(bench, ": ", counts$n_models, " models at coverage vs ",
            counts$n_models_summary, " in ", HF_SUMMARY)
  }

  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(long = long, counts = counts), out)
  counts
}

prep_counts <- map(names(BENCHMARKS), prep_benchmark) |> bind_rows()
print(prep_counts)

# AI matrices are read by workers from a slim copy holding only id/item/resp.
ai_file <- function(bench) {
  f <- file.path(CACHE_DIR, "long", paste0(BENCHMARKS[[bench]]$file, "_resp.rds"))
  if (!file.exists(f)) {
    saveRDS(readRDS(file.path(CACHE_DIR, "long", paste0(BENCHMARKS[[bench]]$file, ".rds")))$long |>
              select(id, item, resp), f)
  }
  f
}

# ==============================================================================
# 3. Human IRW tables: fetch once into the cache
# ==============================================================================

human_dir <- file.path(CACHE_DIR, "irw")
dir.create(human_dir, recursive = TRUE, showWarnings = FALSE)

human_file <- function(tbl) {
  f <- file.path(human_dir, paste0(tbl, ".rds"))
  if (!file.exists(f)) {
    message("Fetching ", tbl)
    df <- irw_fetch(tbl)[, c("id", "item", "resp")]
    stopifnot(all(df$resp %in% c(0, 1)))
    # irw_long2resp() trims item names, so whitespace variants would merge
    # into one column after the per-fold checks; duplicated cells would put
    # copies of a response on both sides of a fold.
    if (any(duplicated(data.frame(df$id, trimws(df$item))))) {
      stop(tbl, " has duplicate (id, item) cells once item names are trimmed")
    }
    saveRDS(df, f)
  }
  f
}
human_files <- set_names(map_chr(HUMAN_TABLES, human_file), HUMAN_TABLES)

# ==============================================================================
# 4. The matrices
#    Per benchmark: the full AI matrix; N_DRAWS AI draws of J_COMMON items; and
#    N_DRAWS draws of (benchmark N, J_COMMON) from each human table.
# ==============================================================================

specs <- list()
for (bench in names(BENCHMARKS)) {
  n_bench <- prep_counts$n_models[prep_counts$benchmark == bench]
  specs[[length(specs) + 1]] <- list(
    key = paste0("ai__", bench, "__full"), source = "ai", benchmark = bench,
    table = bench, draw = 0L, data_file = ai_file(bench), n = NULL, j = NULL)
  for (d in seq_len(N_DRAWS)) {
    specs[[length(specs) + 1]] <- list(
      key = paste0("ai__", bench, "__", d), source = "ai", benchmark = bench,
      table = bench, draw = d, data_file = ai_file(bench), n = NULL, j = J_COMMON)
    for (tbl in HUMAN_TABLES) {
      specs[[length(specs) + 1]] <- list(
        key = paste0("human__", bench, "__", tbl, "__", d), source = "human", benchmark = bench,
        table = tbl, draw = d, data_file = human_files[[tbl]], n = n_bench, j = J_COMMON)
    }
  }
}
message(length(specs), " matrices")

# ==============================================================================
# 5. Run, writing each result to disk as it completes
#    If the script crashes, re-running skips already-completed matrices
# ==============================================================================

dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

fit_to_disk <- function(spec) {
  out_file <- file.path(fits_dir, paste0(spec$key, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", spec$key)
    return(invisible(NULL))
  }
  result <- tryCatch(fit_matrix(spec), error = function(e) {
    message("    unexpected error for ", spec$key, ": ", conditionMessage(e))
    NULL
  })
  if (!is.null(result)) saveRDS(result, out_file)
}

# `only=<regex>` restricts the run to matching matrix keys.
only <- sub("^only=", "", grep("^only=", args, value = TRUE))
todo <- if (length(only)) keep(specs, \(s) grepl(only, s$key)) else specs
if ("combine" %in% args) todo <- list()  # rebuild results from fits already on disk

# AI matrices take 4-5x longer than human draws, so give each matrix its own
# future (scheduling = Inf) rather than chunking, which leaves workers idle.
# Each worker holds ~1-1.5 GB; twelve at once exhausted a 30 GB machine.
plan(multisession, workers = min(4, parallel::detectCores() %/% 2))
future_map(todo, fit_to_disk,
           .options = furrr_options(seed = TRUE, scheduling = Inf,
                                    packages = c("irw", "mirt", "dplyr", "purrr", "tibble")))
plan(sequential)

# ==============================================================================
# 6. Combine and save
# ==============================================================================

all_raw <- map(specs, function(s) {
  f <- file.path(fits_dir, paste0(s$key, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact()

results <- map(all_raw, "summary") |> bind_rows()
folds   <- map(all_raw, "folds") |> bind_rows()

message("\nDone. ", nrow(results), " of ", length(specs), " matrices fit.")

saveRDS(
  list(
    results       = results,
    folds         = folds,
    prep_counts   = prep_counts,
    human_tables  = HUMAN_TABLES,
    settings      = list(min_coverage = MIN_COVERAGE, min_outcome = MIN_OUTCOME,
                         j_common = J_COMMON, n_draws = N_DRAWS, nfold = NFOLD),
    hf_repo       = HF_REPO,
    hf_revision   = HF_REVISION,
    irw_version   = tryCatch(irw_version(), error = function(e) NA),
    pilot         = PILOT,
    date_run      = max(do.call(c, map(all_raw, "date_run"))),
    session       = sessionInfo()
  ),
  file = file.path(out_dir, "ai_benchmark_imv_results.rds")
)

message("Saved to ", out_dir, "/ai_benchmark_imv_results.rds")

# ==============================================================================
# 7. Citations: human tables from IRW, OpenEval by hand
# ==============================================================================

tryCatch(
  irw_save_bibtex(HUMAN_TABLES, output_file = bib_file),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)

manual_entries <- c(
  "@misc{jiang2026aievaluationrequire,
  title         = {AI Evaluation Should Require Standardized Item-Level Data Releases},
  author        = {Jiang, Han and Zhang, Susu and Zhu, Dongyao and Bai, Yuzhuo and Truong, Sang T. and Yi, Xiaoyuan and Koyejo, Sanmi and Xie, Xing and Xiao, Ziang},
  year          = {2026},
  eprint        = {2604.03244},
  archivePrefix = {arXiv},
  primaryClass  = {cs.AI},
  doi           = {10.48550/arXiv.2604.03244},
  url           = {https://arxiv.org/abs/2604.03244}
}",
  "@article{domingue2024imvlens,
  title   = {The InterModel Vigorish as a Lens for Understanding (and Quantifying) the Value of Item Response Models for Dichotomously Coded Items},
  author  = {Domingue, Benjamin W. and Kanopka, Klint and Kapoor, Radhika and Pohl, Steffi and Chalmers, R. Philip and Rahal, Charles and Rhemtulla, Mijke},
  journal = {Psychometrika},
  volume  = {89},
  number  = {3},
  pages   = {1034--1054},
  year    = {2024},
  doi     = {10.1007/s11336-024-09977-2}
}"
)
cat("\n", paste(manual_entries, collapse = "\n\n"), "\n", file = bib_file, append = TRUE)
