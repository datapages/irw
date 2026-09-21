# guessing_m_check.R -- is m = 5 right for the ENEM tables?
#
# Every fixed-floor model on the page needs the chance level 1/m. For the
# gilbert_meta_* tables m is counted from IRW item text. The ENEM tables on the
# page (2013, 2014, 2019, 2024) carry no item text, so m = 5 used to rest on
# ENEM's documented format alone. Two checks now back it with data:
#
#   1. The analysed tables themselves. Since the v52.0 rebuild the ENEM tables
#      ship `resp_raw`, the letter each candidate marked (`.` = left blank,
#      `*` = more than one mark). If an item had fewer than five options, one
#      of A-E would be all but unused; if it had more, a sixth letter would
#      appear. So: on every item, are all five letters chosen, by how many at
#      the least, and does anything other than A-E, `.` or `*` occur?
#
#   2. The adjacent year with item text. The four enem_2023_1mil_* tables carry
#      INEP's booklet text: options per item, and the booklet instructions,
#      which state the option count in so many words.
#
# The first is the direct evidence; the second corroborates it from the
# printed exam. Neither involves fitting a model.
#
# Output: vignettes/guessingdata/guessing_m_check.rds
#
# Usage (needs REDIVIS_API_TOKEN):
#   Rscript vignettes/guessing_m_check.R   # from project root

suppressMessages({
  library(redivis); library(dplyr); library(purrr); library(tibble)
})

out_dir <- "vignettes/guessingdata"

ENEM_TABLES <- c("enem_2013_1mil_mt", "enem_2013_1mil_lc", "enem_2013_1mil_ch",
                 "enem_2013_1mil_cn", "enem_2014_1mil_ch", "enem_2019_1mil_ch",
                 "enem_2019_1mil_lc", "enem_2024_1mil_ch")
ITEMTEXT_TABLES <- c("enem_2023_1mil_ch", "enem_2023_1mil_cn",
                     "enem_2023_1mil_lc", "enem_2023_1mil_mt")

# --- 1. letters marked, per item, on the analysed tables ----------------------
# Aggregated server-side: one GROUP BY per table rather than ~45M rows pulled.
# Addressed by bare table name (reference ids rotate).
ds <- redivis$organization("datapages")$dataset("item_response_warehouse")
counts <- map_dfr(ENEM_TABLES, function(t) {
  message("  counting marked letters: ", t)
  ds$query(sprintf(
    "SELECT item, resp_raw, COUNT(*) AS n FROM %s GROUP BY item, resp_raw", t
  ))$to_tibble() |>
    mutate(table = t, item = as.character(item), n = as.numeric(n))
})

letters_by_item <- counts |>
  filter(resp_raw %in% LETTERS[1:5]) |>
  group_by(table, item) |>
  mutate(share = n / sum(n)) |>
  summarise(n_letters = n_distinct(resp_raw), min_share = min(share),
            n_marked = sum(n), .groups = "drop")

by_table <- letters_by_item |>
  group_by(table) |>
  # median before min: summarise() evaluates in order, so naming the minimum
  # `min_share` first would hand the median a single value
  summarise(n_items = n(), n_all_five = sum(n_letters == 5),
            median_min_share = median(min_share), min_share = min(min_share),
            .groups = "drop")
# the least-chosen letter on the least-chosen item, as a head count
least <- counts |>
  filter(resp_raw %in% LETTERS[1:5]) |>
  group_by(table) |>
  summarise(min_n_least = min(n), .groups = "drop")
by_table <- by_table |> left_join(least, by = "table")

other_codes <- counts |>
  filter(!resp_raw %in% LETTERS[1:5]) |>
  group_by(resp_raw) |>
  summarise(n = sum(n), .groups = "drop")

# --- 2. option counts in the ENEM 2023 item text ------------------------------
# Read straight from the item-text dataset, as part 1 reads the response data:
# the irw package pinned in renv.lock predates the irw_text_2 shard these
# tables live in, so irw_itemtext() reports them as unavailable.
ds_text <- redivis$organization("datapages")$dataset("irw_text_2")
itemtext <- map_dfr(ITEMTEXT_TABLES, function(t) {
  message("  reading item text: ", t)
  d <- ds_text$query(sprintf(
    "SELECT item, resp_raw, instructions FROM %s__items", t
  ))$to_tibble()
  opts <- d |>
    group_by(item) |>
    summarise(n_options = n(),
              letters = paste(sort(unique(resp_raw)), collapse = ""),
              .groups = "drop")
  tibble(
    table = t, n_items = nrow(opts),
    n_five_options = sum(opts$n_options == 5 & opts$letters == "ABCDE"),
    # the booklet's own instruction, e.g. "são apresentadas 5 opções"
    instructions_state_five = any(grepl("5 op[cç][oõ]es|cinco op[cç][oõ]es",
                                        d$instructions))
  )
})

print(as.data.frame(by_table), digits = 3)
print(as.data.frame(other_codes))
print(as.data.frame(itemtext))

saveRDS(
  list(by_table = by_table, other_codes = other_codes, itemtext = itemtext,
       letters_by_item = letters_by_item,
       date_run = Sys.Date(), session = sessionInfo()),
  file.path(out_dir, "guessing_m_check.rds")
)
message("\nWrote ", file.path(out_dir, "guessing_m_check.rds"))
