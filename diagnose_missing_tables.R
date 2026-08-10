## Diagnostic: which of the 447 tables missing from the data browser are
## missing from the *tags* sheet vs the *quantitative metadata* sheet
## (or filtered out by n_categories == 0), given that all 447 already have
## a biblio entry.
##
## Requires REDIVIS_API_TOKEN to be set in the environment.
## Run from repo root: Rscript diagnose_missing_tables.R

library(dplyr)
library(stringr)
library(redivis)

irw_meta <- redivis$user("bdomingu")$dataset("irw_meta:bdxt")

metadata_table <- irw_meta$table("metadata:h5gs")$to_tibble() |>
  mutate(table = str_to_lower(table))

tag_table <- irw_meta$table("tags:7nkh")$to_tibble() |>
  mutate(table = str_to_lower(table))

biblio <- irw_meta$table("biblio:qahg")$to_tibble() |>
  mutate(table = str_to_lower(table))

ds <- c("item_response_warehouse", "item_response_warehouse_2", "item_response_warehouse_3", "item_response_warehouse_4")
physical_tables <- ds |>
  lapply(function(d) {
    tabs <- redivis$user("datapages")$dataset(d)$list_tables()
    sapply(tabs, function(t) t$name)
  }) |>
  unlist() |>
  str_to_lower()

missing <- readLines("/tmp/claude-1000/-home-ben-Dropbox-projects-irw-irw-site/b6fee65e-47f3-4a4c-bc82-bf4d24c34bd3/scratchpad/missing_has_biblio.txt")

diagnosis <- tibble(table = missing) |>
  mutate(
    in_physical_list = table %in% physical_tables,
    in_biblio         = table %in% biblio$table,
    in_metadata       = table %in% metadata_table$table,
    n_categories      = metadata_table$n_categories[match(table, metadata_table$table)],
    dropped_by_ncat_filter = in_metadata & (n_categories == 0 | is.na(n_categories)),
    in_tags           = table %in% tag_table$table,
    reason = case_when(
      !in_metadata               ~ "missing from quantitative metadata (metadata:h5gs)",
      dropped_by_ncat_filter     ~ "in metadata but n_categories == 0 (filtered out)",
      !in_tags                   ~ "missing from tags sheet (tags:7nkh)",
      TRUE                       ~ "present in all sources -- unexplained, re-check join keys"
    )
  )

cat("\nBreakdown of reasons (n =", nrow(diagnosis), "):\n")
print(diagnosis |> count(reason, sort = TRUE))

write.csv(diagnosis, "missing_tables_diagnosis.csv", row.names = FALSE)
cat("\nFull per-table breakdown written to missing_tables_diagnosis.csv\n")
