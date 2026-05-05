# item_text_difficulty_compute.R
#
# Run this script once locally to produce the precomputed results object
# loaded by item_text_difficulty.qmd at render time.
#
# Output: itemtextdata/item_text_difficulty_results.rds
#
# Usage:
#   Rscript vignettes/item_text_difficulty_compute.R   # from project root

library(irw)
library(dplyr)
library(purrr)

# ------------------------------------------------------------------------------
# 1. Find eligible tables
# ------------------------------------------------------------------------------

text_tables <- irw_list_itemtext_tables()

meta <- irw_filter(
  construct_type   = "Cognitive/educational",
  n_categories     = 2,
  n_participants   = c(200, Inf),
  primary_language = "eng"
)

eligible_tables <- intersect(text_tables, meta)
message("Eligible tables: ", length(eligible_tables))

# ------------------------------------------------------------------------------
# 2. Process each table: prop_correct per item + item text
# ------------------------------------------------------------------------------

process_table <- function(table_name) {
  message("  Processing: ", table_name)

  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e)); NULL
  })
  if (is.null(df)) return(NULL)

  item_text <- tryCatch(irw_itemtext(table_name), error = function(e) {
    message("    item text fetch failed: ", conditionMessage(e)); NULL
  })
  if (is.null(item_text) || nrow(item_text) == 0) return(NULL)

  if (!"item_text" %in% names(item_text)) {
    message("    no item_text column, skipping"); return(NULL)
  }

  item_text <- item_text |>
    select(item, item_text) |>
    mutate(item = as.character(item)) |>
    filter(!is.na(item_text), nchar(trimws(item_text)) > 0) |>
    distinct(item, .keep_all = TRUE)

  if (nrow(item_text) == 0) return(NULL)

  prop_correct <- df |>
    mutate(resp = suppressWarnings(as.numeric(resp)),
           item = as.character(item)) |>
    group_by(item) |>
    summarise(
      prop_correct = mean(resp == 1, na.rm = TRUE),
      n_resp       = sum(!is.na(resp)),
      .groups      = "drop"
    )

  combined <- inner_join(prop_correct, item_text, by = "item")
  if (nrow(combined) == 0) return(NULL)

  combined |> mutate(table = table_name) |>
    select(table, item, prop_correct, n_resp, item_text)
}

# ------------------------------------------------------------------------------
# 3. Run, caching each table to disk
# ------------------------------------------------------------------------------

out_dir <- "vignettes/itemtextdata/tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

walk(eligible_tables, function(table_name) {
  out_file <- file.path(out_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (cached): ", table_name); return(invisible(NULL))
  }
  result <- process_table(table_name)
  if (!is.null(result)) saveRDS(result, out_file)
})

# ------------------------------------------------------------------------------
# 4. Combine
# ------------------------------------------------------------------------------

raw_data <- map(eligible_tables, function(tbl) {
  f <- file.path(out_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |> compact() |> bind_rows()

message("\nItems: ", nrow(raw_data), " across ", n_distinct(raw_data$table), " tables")

# ------------------------------------------------------------------------------
# 5. Extract text features (base R only, no quanteda)
# ------------------------------------------------------------------------------

all_data <- raw_data |>
  mutate(
    word_count   = lengths(strsplit(trimws(item_text), "\\s+")),
    avg_word_len = nchar(gsub("[[:punct:]]", "", item_text)) /
                   pmax(word_count, 1)
  )

message("Features extracted.")

# ------------------------------------------------------------------------------
# 6. Save
# ------------------------------------------------------------------------------

saveRDS(
  list(
    all_data        = all_data,
    eligible_tables = eligible_tables,
    date_run        = Sys.Date()
  ),
  file = "vignettes/itemtextdata/item_text_difficulty_results.rds"
)

message("Saved to vignettes/itemtextdata/item_text_difficulty_results.rds")
