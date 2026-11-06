# bits_per_item_scout.R
#
# Phase 0 scouting for the "Bits per item" vignette. Applies the metadata
# filters (dichotomous, 5-60 items, >= 500 respondents), then fetches every
# survivor once to check its structure: wave / treat / repeated id-item rows,
# the actual response values, and how many respondents remain after the
# single-administration rule below.
#
# Single-administration rule (applied to the fetched data, not the metadata):
#   - if `wave` takes more than one value, keep the earliest wave;
#   - then, if `treat` takes more than one value, keep the lowest code
#     (the control arm in every IRW table that carries treat);
#   - a table that still has repeated id-item rows after that is trial-level
#     or otherwise repeated-measures data and is excluded.
# The filters on n_items and n_respondents are re-applied after the rule.
#
# Output: bits_per_item_data/scout_tables.csv (one row per metadata survivor)
#
# Usage:
#   Rscript vignettes/bits_per_item_scout.R   # from project root

suppressMessages({
  library(irw)
  library(dplyr)
})

out_dir <- "vignettes/bits_per_item_data"
dir.create(file.path(out_dir, "scout"), recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "scout_tables.csv")

MIN_ITEMS        <- 5
MAX_ITEMS        <- 60
MIN_PARTICIPANTS <- 500

meta <- as_tibble(irw_metadata())
steps <- tibble(
  step = c("core tables", "dichotomous (n_categories == 2)",
           "5-60 items", ">= 500 respondents"),
  n = c(nrow(meta),
        sum(meta$n_categories == 2),
        sum(meta$n_categories == 2 & meta$n_items >= MIN_ITEMS & meta$n_items <= MAX_ITEMS),
        sum(meta$n_categories == 2 & meta$n_items >= MIN_ITEMS & meta$n_items <= MAX_ITEMS &
              meta$n_participants >= MIN_PARTICIPANTS))
)
write.csv(steps, file.path(out_dir, "scout_metadata_steps.csv"), row.names = FALSE)

cands <- meta |>
  filter(n_categories == 2, n_items >= MIN_ITEMS, n_items <= MAX_ITEMS,
         n_participants >= MIN_PARTICIPANTS) |>
  arrange(n_responses)
if (Sys.getenv("REVERSE") == "1") cands <- cands[rev(seq_len(nrow(cands))), ]

lowest <- function(x) {
  u <- unique(x[!is.na(x)])
  num <- suppressWarnings(as.numeric(as.character(u)))
  if (all(!is.na(num))) u[which.min(num)] else sort(as.character(u))[1]
}

scan_table <- function(tab) {
  row_file <- file.path(out_dir, "scout", paste0(tab, ".rds"))
  if (file.exists(row_file)) return(readRDS(row_file))
  t0 <- Sys.time()
  df <- tryCatch(irw_fetch(tab), error = function(e) conditionMessage(e))
  if (is.character(df)) {
    res <- tibble(table = tab, fetch_error = df)
    saveRDS(res, row_file)
    return(res)
  }
  df <- as.data.frame(df)
  n_rows_raw <- nrow(df)
  n_ids_raw  <- length(unique(df$id))
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
  df <- df[!is.na(df$resp), ]
  key <- paste(df$id, df$item, sep = "\r")
  dup_rows <- sum(duplicated(key))
  resp_vals <- sort(unique(df$resp))
  n_ids <- length(unique(df$id))
  item_n <- table(df$item)
  per_id <- table(df$id)
  n_items <- length(item_n)
  p_item <- tapply(df$resp == max(resp_vals), df$item, mean)
  res <- tibble(
    table = tab, fetch_error = NA_character_,
    n_rows_raw = n_rows_raw, n_ids_raw = n_ids_raw,
    n_waves = n_waves, wave_kept = wave_kept,
    n_treat = n_treat, treat_kept = treat_kept,
    n_rows = nrow(df), n_ids = n_ids, n_items = n_items,
    dup_rows = dup_rows,
    resp_values = paste(resp_vals, collapse = ","),
    density = nrow(df) / (n_ids * n_items),
    n_complete = sum(per_id == n_items),
    p_min = min(p_item), p_max = max(p_item),
    has_rater = any(grepl("rater", names(df))),
    other_struct = paste(intersect(c("date", "time", "session", "occasion", "trial",
                                     "trial_num", "trial_number", "booklet", "test",
                                     "group", "cohort", "position"), names(df)),
                         collapse = ","),
    secs = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
  saveRDS(res, row_file)
  res
}

rows <- list()
for (i in seq_len(nrow(cands))) {
  tab <- cands$table[i]
  message(i, "/", nrow(cands), " ", tab)
  rows[[i]] <- scan_table(tab)
  gc(verbose = FALSE)
}
scan <- bind_rows(rows) |>
  left_join(select(cands, table, meta_n_items = n_items, meta_n_participants = n_participants,
                   meta_density = density, longitudinal, variables), by = "table")
write.csv(scan, out_csv, row.names = FALSE)
message("Wrote ", out_csv)
