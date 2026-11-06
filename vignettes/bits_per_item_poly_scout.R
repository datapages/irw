# bits_per_item_poly_scout.R
#
# Phase 0 scouting for the polytomous arm of the "Bits per item" vignette
# ("More than right and wrong"). Applies the metadata filters (3-11
# categories, 3-60 items, >= 500 respondents), then fetches every survivor
# once to check its structure and to record, per item, which response values
# were used and how often. Nothing is fitted here.
#
# Single-administration rule: the same as bits_per_item_scout.R (earliest
# wave, then the lowest treat code; a table with repeated id-item rows after
# that is excluded). After the rule, items answered by fewer than half the
# respondents are dropped (as in prepare_resp()), and the filters on n_items
# and n_respondents are re-applied.
#
# Per item it records the observed values and their counts, so that K, sparse
# and empty categories can be summarised without refetching. Two notions of an
# empty category are kept apart: an interior gap (a value between the item's
# own lowest and highest observed values that nobody chose) and an edge gap (a
# value in the table's range that the item never reaches at either end).
#
# Output: bits_per_item_data/poly_scout_tables.csv  (one row per metadata survivor)
#         bits_per_item_data/poly_scout_items.csv   (one row per item x value)
#         bits_per_item_data/poly_scout/             (per-table cache, gitignored)
#
# Usage (from project root):
#   Rscript vignettes/bits_per_item_poly_scout.R
#   PART=1/3 Rscript vignettes/bits_per_item_poly_scout.R   # scan a third (parallel shells)
#   COMBINE=1 Rscript vignettes/bits_per_item_poly_scout.R  # only combine the cache
#   TABLES=a,b,c Rscript vignettes/bits_per_item_poly_scout.R  # scan named tables only
#
# The polytomous arm was scoped down to three hand-picked tables
# (scout_report_poly.md), so the committed CSVs come from the TABLES= form:
#   TABLES=rosenberg_selfesteem,bfi_goldberg_1992_extraversion,su_2024_gad7

suppressMessages({
  library(irw)
  library(dplyr)
})

out_dir   <- "vignettes/bits_per_item_data"
cache_dir <- file.path(out_dir, "poly_scout")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

MIN_CATS         <- 3
MAX_CATS         <- 11
MIN_ITEMS        <- 3
MAX_ITEMS        <- 60
MIN_PARTICIPANTS <- 500
MIN_COVERAGE     <- 0.5
BIG_RESPONSES    <- 2e6    # tables above this are scanned by PART=big only, one at a time

meta <- as_tibble(suppressWarnings(irw_metadata()))
f_cat  <- meta$n_categories >= MIN_CATS & meta$n_categories <= MAX_CATS
f_item <- f_cat & meta$n_items >= MIN_ITEMS & meta$n_items <= MAX_ITEMS
f_n    <- f_item & meta$n_participants >= MIN_PARTICIPANTS
steps <- tibble(
  step = c("core tables", "3-11 categories (metadata n_categories)", "3-60 items", ">= 500 respondents"),
  n = c(nrow(meta), sum(f_cat), sum(f_item), sum(f_n))
)
write.csv(steps, file.path(out_dir, "poly_scout_metadata_steps.csv"), row.names = FALSE)
cands <- meta[f_n, ] |> arrange(n_responses)

lowest <- function(x) {
  u <- unique(x[!is.na(x)])
  num <- suppressWarnings(as.numeric(as.character(u)))
  if (all(!is.na(num))) u[which.min(num)] else sort(as.character(u))[1]
}

scan_table <- function(tab) {
  row_file <- file.path(cache_dir, paste0(tab, ".rds"))
  if (file.exists(row_file)) return(invisible(NULL))
  t0 <- Sys.time()
  df <- tryCatch(irw_fetch(tab), error = function(e) conditionMessage(e))
  if (is.character(df)) {
    saveRDS(list(table = tibble(table = tab, fetch_error = df), items = NULL), row_file)
    return(invisible(NULL))
  }
  secs_fetch <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  df <- as.data.frame(df)
  n_ids_raw <- length(unique(df$id))
  has_rater <- any(grepl("rater", names(df)))
  other_struct <- paste(intersect(c("date", "time", "session", "occasion", "trial", "trial_num",
                                    "trial_number", "booklet", "test", "group", "cohort", "position"),
                                  names(df)), collapse = ",")
  n_waves <- if ("wave" %in% names(df)) length(unique(na.omit(df$wave))) else NA_integer_
  wave_kept <- NA_character_
  if (!is.na(n_waves) && n_waves > 1) {
    wave_kept <- as.character(lowest(df$wave))
    df <- df[!is.na(df$wave) & as.character(df$wave) == wave_kept, ]
  }
  n_treat <- if ("treat" %in% names(df)) length(unique(na.omit(df$treat))) else NA_integer_
  treat_kept <- NA_character_
  if (!is.na(n_treat) && n_treat > 1) {
    treat_kept <- as.character(lowest(df$treat))
    df <- df[!is.na(df$treat) & as.character(df$treat) == treat_kept, ]
  }
  df <- df[!is.na(df$resp), c("id", "item", "resp")]
  dup_rows <- sum(duplicated(paste(df$id, df$item, sep = "\r")))
  n_ids <- length(unique(df$id))
  coverage <- tapply(df$id, df$item, function(x) length(unique(x))) / n_ids
  n_items_raw <- length(coverage)
  keep_items <- names(coverage)[coverage >= MIN_COVERAGE]
  df <- df[df$item %in% keep_items, ]
  vals <- sort(unique(df$resp))
  non_integer <- any(abs(vals - round(vals)) > 1e-8)
  steps_v <- diff(vals)
  # Per item x value counts
  it <- as.data.frame(table(item = df$item, resp = df$resp), stringsAsFactors = FALSE)
  it <- it[it$Freq > 0, ]
  names(it)[3] <- "n"
  it$resp <- as.numeric(it$resp)
  it$table <- tab
  per_item <- it |> group_by(item) |>
    summarise(K_used = n(), lo = min(resp), hi = max(resp), n_item = sum(n), .groups = "drop")
  res <- tibble(
    table = tab, fetch_error = NA_character_, n_ids_raw = n_ids_raw,
    n_waves = n_waves, wave_kept = wave_kept, n_treat = n_treat, treat_kept = treat_kept,
    has_rater = has_rater, other_struct = other_struct, dup_rows = dup_rows,
    n_ids = n_ids, n_items_raw = n_items_raw, n_items = length(keep_items),
    n_low_coverage = n_items_raw - length(keep_items),
    n_rows = nrow(df), density = nrow(df) / (n_ids * max(1, length(keep_items))),
    n_values = length(vals), values = paste(head(vals, 25), collapse = ","),
    non_integer = non_integer,
    step_min = if (length(steps_v)) min(steps_v) else NA, step_max = if (length(steps_v)) max(steps_v) else NA,
    K_min = if (nrow(per_item)) min(per_item$K_used) else NA,
    K_max = if (nrow(per_item)) max(per_item$K_used) else NA,
    n_items_K2 = sum(per_item$K_used == 2), n_items_K1 = sum(per_item$K_used == 1),
    mixed_range = nrow(distinct(per_item, lo, hi)) > 1,
    secs_fetch = secs_fetch, secs = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
  saveRDS(list(table = res, items = it[, c("table", "item", "resp", "n")]), row_file)
  invisible(NULL)
}

part <- Sys.getenv("PART")
if (Sys.getenv("COMBINE") != "1") {
  big <- cands$n_responses > BIG_RESPONSES
  todo <- if (part == "big") cands$table[big] else cands$table[!big]
  if (nzchar(Sys.getenv("TABLES"))) todo <- strsplit(Sys.getenv("TABLES"), ",")[[1]]
  if (grepl("^[0-9]+/[0-9]+$", part)) {
    ij <- as.integer(strsplit(part, "/")[[1]])
    todo <- todo[seq_along(todo) %% ij[2] == (ij[1] - 1)]
  }
  for (i in seq_along(todo)) {
    message(format(Sys.time(), "%H:%M:%S "), i, "/", length(todo), " ", todo[i])
    tryCatch(scan_table(todo[i]),
             error = function(e) message("  scan error: ", conditionMessage(e)))
    gc(verbose = FALSE)
  }
}

# Combine whatever is cached.
files <- file.path(cache_dir, paste0(cands$table, ".rds"))
files <- files[file.exists(files)]
parts <- lapply(files, readRDS)
scan <- bind_rows(lapply(parts, `[[`, "table")) |>
  inner_join(select(cands, table, meta_n_items = n_items, meta_n_participants = n_participants,
                    meta_n_categories = n_categories, meta_density = density, meta_n_responses = n_responses,
                    longitudinal, variables), by = "table")
write.csv(scan, file.path(out_dir, "poly_scout_tables.csv"), row.names = FALSE)
write.csv(bind_rows(lapply(parts, `[[`, "items")), file.path(out_dir, "poly_scout_items.csv"), row.names = FALSE)
message("Combined ", length(files), " of ", nrow(cands), " tables")
