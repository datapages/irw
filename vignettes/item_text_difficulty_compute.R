# item_text_difficulty_compute.R
#
# Run this script once locally to produce the precomputed results object
# loaded by item_text_difficulty.qmd at render time.
#
# Output: itemtextdata/item_text_difficulty_results.rds
#
# Usage:
#   Rscript item_text_difficulty_compute.R   # from project root

library(irw)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

# quanteda for text features — install if needed:
# install.packages("quanteda")
# install.packages("quanteda.textstats")
library(quanteda)
library(quanteda.textstats)

# ------------------------------------------------------------------------------
# 1. Find tables that have item text
# ------------------------------------------------------------------------------

text_tables <- irw_list_itemtext_tables()

# Further restrict to cognitive/educational dichotomous English tables with
# enough respondents for stable proportion-correct estimates
meta <- irw_filter(
  construct_type   = "Cognitive/educational",
  n_categories     = 2,
  n_participants   = c(200, Inf),
  primary_language = "eng"
)

# Keep only tables present in both
eligible_tables <- intersect(text_tables, meta)

message("Tables with item text + cognitive/dichotomous filter: ", length(eligible_tables))

# ------------------------------------------------------------------------------
# 2. For each table, compute proportion correct per item and extract item text
# ------------------------------------------------------------------------------

process_table <- function(table_name) {
  message("  Processing: ", table_name)

  # Fetch response data
  df <- tryCatch(irw_fetch(table_name), error = function(e) {
    message("    fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(df)) return(NULL)

  # Fetch item text
  item_text <- tryCatch(irw_itemtext(table_name), error = function(e) {
    message("    item text fetch failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(item_text) || nrow(item_text) == 0) return(NULL)

  # Coerce to consistent types across tables
  df <- df |> mutate(
    resp = suppressWarnings(as.numeric(resp)),
    item = as.character(item)
  )

  # Process item text
  text_col  <- setdiff(names(item_text), "item")[1]
  item_text <- item_text |>
    rename(item_text_raw = all_of(text_col)) |>
    mutate(item = as.character(item)) |>
    filter(!is.na(item_text_raw), nchar(trimws(item_text_raw)) > 0)

  # Compute proportion correct per item
  prop_correct <- df |>
    group_by(item) |>
    summarise(
      prop_correct = mean(resp == 1, na.rm = TRUE),
      n_resp       = sum(!is.na(resp)),
      .groups      = "drop"
    )

  inner_join(prop_correct, item_text, by = "item") |>
    mutate(table = table_name) |>
    select(table, item, prop_correct, n_resp, item_text_raw)
}

out_dir <- "itemtextdata/tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

process_to_disk <- function(table_name) {
  out_file <- file.path(out_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(invisible(NULL))
  }
  result <- process_table(table_name)
  if (!is.null(result)) saveRDS(result, out_file)
}

# Sequential is fine here — no heavy computation, just API calls
walk(eligible_tables, process_to_disk)

# ------------------------------------------------------------------------------
# 3. Combine
# ------------------------------------------------------------------------------

raw_data <- map(eligible_tables, function(tbl) {
  f <- file.path(out_dir, paste0(tbl, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}) |>
  compact() |>
  bind_rows()

message("\nItems with text + proportion correct: ", nrow(raw_data))
message("Tables represented: ", n_distinct(raw_data$table))

# ------------------------------------------------------------------------------
# 4. Extract text features
# ------------------------------------------------------------------------------

extract_features <- function(text) {
  # Guard against empty/NA
  text <- ifelse(is.na(text) | nchar(trimws(text)) == 0, "missing", text)

  corp  <- corpus(text)
  toks  <- tokens(corp, remove_punct = FALSE)
  toks_words <- tokens(corp, remove_punct = TRUE)

  # Readability (Flesch-Kincaid grade level + Flesch reading ease)
  read  <- textstat_readability(corp, measure = c("Flesch.Kincaid", "Flesch"))

  # Lexical diversity (type-token ratio)
  ttr   <- textstat_lexdiv(toks_words, measure = "TTR")

  # Basic counts
  word_count <- ntoken(toks_words)
  sent_count <- nsentence(corp)
  char_count <- nchar(text)

  # Average word length (characters)
  avg_word_len <- char_count / pmax(word_count, 1)

  # Negation: presence of common negation terms
  negation_terms <- c("not", "never", "no", "neither", "nor", "without",
                      "n't", "cannot", "cant", "doesnt", "isnt", "arent",
                      "wasnt", "werent", "hasnt", "havent", "wont", "wouldnt")
  toks_lower <- tokens_tolower(toks_words)
  has_negation <- as.integer(
    sapply(as.list(toks_lower), function(x) any(x %in% negation_terms))
  )

  # Question format: does the item end with "?" or start with a question word
  question_words <- c("what", "which", "who", "where", "when", "why", "how")
  first_word     <- tolower(sapply(strsplit(trimws(text), "\\s+"), `[`, 1))
  has_question   <- as.integer(
    grepl("\\?", text) | first_word %in% question_words
  )

  # Numbers/quantifiers present
  has_number <- as.integer(grepl("[0-9]|\\b(one|two|three|four|five|six|seven|eight|nine|ten|all|none|some|most|few|many)\\b",
                                  text, ignore.case = TRUE))

  # Capitalisation ratio (proper nouns / named entities proxy)
  words_split  <- strsplit(text, "\\s+")
  cap_ratio    <- sapply(words_split, function(w) {
    w <- w[nchar(w) > 0]
    if (length(w) == 0) return(0)
    # Exclude first word (always capitalised)
    if (length(w) == 1) return(0)
    mean(grepl("^[A-Z]", w[-1]))
  })

  tibble(
    word_count    = word_count,
    sent_count    = sent_count,
    avg_word_len  = avg_word_len,
    fk_grade      = read$Flesch.Kincaid,
    flesch_ease   = read$Flesch,
    ttr           = ttr$TTR,
    has_negation  = has_negation,
    has_question  = has_question,
    has_number    = has_number,
    cap_ratio     = cap_ratio
  )
}

message("\nExtracting text features...")
features <- extract_features(raw_data$item_text_raw)

all_data <- bind_cols(raw_data, features)

message("Done. Feature extraction complete.")
message("Columns: ", paste(names(all_data), collapse = ", "))

# ------------------------------------------------------------------------------
# 5. Save
# ------------------------------------------------------------------------------

saveRDS(
  list(
    all_data        = all_data,
    eligible_tables = eligible_tables,
    date_run        = Sys.Date(),
    session         = sessionInfo()
  ),
  file = "itemtextdata/item_text_difficulty_results.rds"
)

message("Saved to itemtextdata/item_text_difficulty_results.rds")

# Citations
for (tbl in eligible_tables) {
  tryCatch(
    irw_save_bibtex(tbl, output_file = "itemtextdata/irw_references.bib",
                    append = TRUE),
    error = function(e) message("  bibtex failed for: ", tbl)
  )
}

message("Citations saved to itemtextdata/irw_references.bib")
