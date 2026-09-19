# Re-vetting pass: the first vet_candidates.R pass wrongly flagged several
# tables as "repeated-measures duplication" without checking for the wave/
# rater/stimulus columns IRW's own data standard (standard.qmd) documents
# for exactly this scenario. This script filters each affected table to a
# single occasion (one wave / one rater / one stimulus-phase combo) and
# re-checks whether the resulting single-administration cross-section is
# usable: unique id x item, no ipsative row-sum constraint, a real N and J.

suppressMessages({library(irw); library(dplyr); library(tidyr)})

vet_single_occasion <- function(tb, filter_expr = NULL, label = "") {
  df <- irw_fetch(tb)
  if (!is.null(filter_expr)) df <- filter_expr(df)
  n_items <- length(unique(df$item))
  n_persons <- length(unique(df$id))
  dup <- df |> count(id, item) |> filter(n > 1) |> nrow()
  wide <- tryCatch(
    df |> select(id, item, resp) |> distinct(id, item, .keep_all = TRUE) |>
      pivot_wider(names_from = item, values_from = resp),
    error = function(e) NULL
  )
  rowsum_cv <- NA_real_; n_complete <- NA_integer_; range_ok <- NA
  if (!is.null(wide) && n_items > 1) {
    mat <- as.matrix(wide[, setdiff(colnames(wide), "id")])
    storage.mode(mat) <- "double"
    rs <- rowSums(mat, na.rm = TRUE)
    rowsum_cv <- round(sd(rs, na.rm = TRUE) / mean(rs, na.rm = TRUE), 3)
    n_complete <- sum(complete.cases(mat))
    item_ranges <- apply(mat, 2, function(x) range(x, na.rm = TRUE))
    range_ok <- all(item_ranges[1, ] <= min(item_ranges[1, ]) + 0.2 * diff(range(mat, na.rm = TRUE))) # loose check
  }
  data.frame(table = paste0(tb, label), n_items = n_items, n_persons = n_persons,
             dup_id_item = dup, n_complete = n_complete, rowsum_cv = rowsum_cv)
}

results <- list()

# tears: rater x stimulus x phase design -- pick one rater, one phase, all stimuli
# collapsed doesn't make sense (stimulus IS the repeated unit); instead pick
# ONE stimulus + ONE phase so each of the 11 emotion "items" gets exactly one
# rating per person.
df <- irw_fetch("tears")
cat("tears: raters=", paste(unique(df$rater)[1:3], collapse=","), "... phases=", paste(unique(df$phase), collapse=","), " n stimuli=", length(unique(df$stimulus)), "\n")
one_stim <- sort(unique(df$stimulus))[1]
one_phase <- sort(unique(df$phase))[1]
results$tears <- vet_single_occasion("tears", function(d) filter(d, stimulus == one_stim, phase == one_phase), paste0("_stim", one_stim, "_phase", one_phase))

# nas_rogoza: wave -- pick wave==min
for (tb in c("nas_rogoza_2024_study5_nas","nas_rogoza_2024_study5_ngs","nas_rogoza_2024_study5_nvs")) {
  df <- irw_fetch(tb)
  waves <- sort(unique(df$wave))
  cat(tb, ": waves =", paste(head(waves,5), collapse=","), "... (", length(waves), "total)\n")
  w1 <- waves[1]
  results[[tb]] <- vet_single_occasion(tb, function(d) filter(d, wave == w1), paste0("_wave", w1))
}

# emoji_scheffler_2024: rater -- actually "rater" here is likely the PERSON id
# doing the rating and "id" might be the stimulus; check which is finer-grained
df <- irw_fetch("emoji_scheffler_2024")
cat("emoji_scheffler_2024: n unique id=", length(unique(df$id)), " n unique rater=", length(unique(df$rater)), "\n")
one_rater <- sort(unique(df$rater))[1]
results$emoji <- vet_single_occasion("emoji_scheffler_2024", function(d) filter(d, rater == one_rater), paste0("_rater", one_rater))

# opentsstvr: wave
df <- irw_fetch("opentsstvr_linnig_2025_vas")
waves <- sort(unique(df$wave))
cat("opentsstvr_linnig_2025_vas: waves =", paste(waves, collapse=","), "\n")
w1 <- waves[1]
results$opentsstvr <- vet_single_occasion("opentsstvr_linnig_2025_vas", function(d) filter(d, wave == w1), paste0("_wave", w1))

# westhoff: wave
for (tb in c("westhoff2023_stopd","westhoff2023_pbat")) {
  df <- irw_fetch(tb)
  waves <- sort(unique(df$wave))
  cat(tb, ": n waves =", length(waves), " range:", min(waves), "-", max(waves), "\n")
  w1 <- waves[1]
  results[[tb]] <- vet_single_occasion(tb, function(d) filter(d, wave == w1), paste0("_wave", w1))
}

# gilbert_meta_95: wave
df <- irw_fetch("gilbert_meta_95")
waves <- sort(unique(df$wave))
cat("gilbert_meta_95: waves =", paste(waves, collapse=","), "\n")
w1 <- waves[1]
results$gilbert <- vet_single_occasion("gilbert_meta_95", function(d) filter(d, wave == w1), paste0("_wave", w1))

# much_tte_2025_currentmotivation: wave
df <- irw_fetch("much_tte_2025_currentmotivation")
waves <- sort(unique(df$wave))
cat("much_tte_2025_currentmotivation: waves =", paste(waves, collapse=","), "\n")
w1 <- waves[1]
results$much_tte <- vet_single_occasion("much_tte_2025_currentmotivation", function(d) filter(d, wave == w1), paste0("_wave", w1))

# double_marking_steele_2022: rater
df <- irw_fetch("double_marking_steele_2022")
raters <- sort(unique(df$rater))
cat("double_marking_steele_2022: raters =", paste(raters, collapse=","), "\n")
r1 <- raters[1]
results$double_marking <- vet_single_occasion("double_marking_steele_2022", function(d) filter(d, rater == r1), paste0("_rater", r1))

# thomeczek2025_les: rater
df <- irw_fetch("thomeczek2025_les")
raters <- sort(unique(df$rater))
cat("thomeczek2025_les: raters =", paste(raters, collapse=","), "\n")
r1 <- raters[1]
results$thomeczek <- vet_single_occasion("thomeczek2025_les", function(d) filter(d, rater == r1), paste0("_rater", r1))

# test_taking_much_2025_cm: no wave/rater column -- check directly for real duplicates
df <- irw_fetch("test_taking_much_2025_cm")
dup <- df |> count(id, item) |> filter(n > 1) |> nrow()
cat("test_taking_much_2025_cm: dup id-item rows (no wave/rater column) =", dup, "of", nrow(df), "\n")

out <- do.call(rbind, results)
cat("\n=== Re-vetted (single-occasion) results ===\n")
print(out, row.names = FALSE)
write.csv(out, "vignettes/continuous_bounded_data/vet_results2.csv", row.names = FALSE)
