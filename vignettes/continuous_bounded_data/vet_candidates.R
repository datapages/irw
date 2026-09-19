# Ad hoc vetting pass over the remaining scout-flagged "bounded" tables,
# checking for the same structural traps that ruled out
# eammi_grahe_2018_marriage_identity_allocation (ipsative/compositional) and
# much_tte_2025_currentmotivation / test_taking_much_2025_cm (duplicate
# id x item rows from repeated measurement occasions) before deciding which
# tables are safe to fit the 4 core specs to.

suppressMessages({
  library(irw); library(dplyr); library(tidyr)
})

tables <- c(
  "tears", "klippel_irw",
  "nas_rogoza_2024_study5_nas", "nas_rogoza_2024_study5_ngs", "nas_rogoza_2024_study5_nvs",
  "emoji_scheffler_2024", "opentsstvr_linnig_2025_vas",
  "ai_fear_dong_2026_ai", "ai_fear_dong_2026_requirement", "ai_fear_dong_2026_own_fear", "ai_fear_dong_2026_other_fear",
  "westhoff2023_stopd", "westhoff2023_pbat",
  "ehealth_rioux_2025_triplep", "ehealth_rioux_2025_beam", "ehealth_rioux_2025_abiliti",
  "gilbert_meta_95", "thomeczek2025_les", "double_marking_steele_2022",
  "mclaughlin_samuel_2025_auditory_session_1"
)

vet_one <- function(tb) {
  df <- tryCatch(irw_fetch(tb), error = function(e) NULL)
  if (is.null(df)) return(data.frame(table = tb, status = "fetch_failed"))
  n_items <- length(unique(df$item))
  n_persons <- length(unique(df$id))
  dup <- df |> count(id, item) |> filter(n > 1) |> nrow()
  wide <- tryCatch(
    df |> select(id, item, resp) |> distinct(id, item, .keep_all = TRUE) |>
      pivot_wider(names_from = item, values_from = resp),
    error = function(e) NULL
  )
  rowsum_cv <- NA_real_
  if (!is.null(wide) && n_items > 1) {
    mat <- as.matrix(wide[, setdiff(colnames(wide), "id")])
    storage.mode(mat) <- "double"
    rs <- rowSums(mat, na.rm = TRUE)
    rowsum_cv <- sd(rs, na.rm = TRUE) / mean(rs, na.rm = TRUE)
  }
  n_complete <- if (!is.null(wide)) sum(complete.cases(wide)) else NA_integer_
  data.frame(table = tb, status = "ok", n_items = n_items, n_persons = n_persons,
             n_rows = nrow(df), dup_id_item_rows = dup, n_complete_persons = n_complete,
             rowsum_cv = round(rowsum_cv, 3))
}

out <- do.call(rbind, lapply(tables, function(tb) {
  message("vetting: ", tb)
  vet_one(tb)
}))
print(out, row.names = FALSE)
write.csv(out, "vignettes/continuous_bounded_data/vet_results.csv", row.names = FALSE)
