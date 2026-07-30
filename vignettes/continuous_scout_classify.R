# continuous_scout_classify.R
#
# Classify stage of the continuous-response scoping scout (see
# continuous_scout_fetch.R for context and the two-stage rationale). Loads
# the raw per-table/per-item resp summaries cached by the fetch stage and
# makes the bounded/unbounded/unclear call. Cheap and deterministic given
# the cache, so thresholds here are meant to be tinkered with and rerun
# without touching Redivis again.
#
# Two independent signals feed the call:
#   1. resp-based: does the table's observed min/max sit at round,
#      interpretable endpoints, and do individual items consistently reach
#      those endpoints (vs. one item using a visibly different scale)?
#   2. metadata-based: irw_tags()'s item_format field has a clean, curated
#      "Slider/continuous" category (bounded evidence) and "Constructed
#      Response" category (open-ended, unbounded evidence). This is
#      independent of anything computed from resp, so agreement between the
#      two is a stronger signal than either alone, and disagreement is
#      exactly the kind of thing a human should look at rather than have
#      silently resolved one way.
#
# This is a SCOUTING script only: no model fitting, no LLM/API calls, no
# writing to Redivis/GitHub/any public artifact. Output is non-authoritative
# and meant for human review.
#
# Output: continuous_scout_candidates.csv (repo root)
#         continuous_scout_references.bib (repo root)
#
# Usage:
#   Rscript vignettes/continuous_scout_fetch.R      # once, or after changing N_CATEGORIES_CAP
#   Rscript vignettes/continuous_scout_classify.R    # repeat freely while tuning thresholds below

library(irw)
library(dplyr)

# ==============================================================================
# Tunable constants (eyeball results and adjust as needed)
# ==============================================================================

ROUND_ENDPOINTS <- c(0, 1, -1, 100, -100, 10, -10, 5, -5)  # interpretable
                         # fixed response-format endpoints to check min/max against
ENDPOINT_TOL <- 1e-6    # how close a min/max has to be to a round endpoint

# Item-to-item range consistency: NOT "how close are the raw observed
# per-item extremes" (an SD-based version of this systematically
# misclassified clean fixed-format tables -- e.g. an 11-item 0-100 VAS table
# where 9 items hit exactly [0,100] but 2 items topped out at 92/98 got
# flagged "inconsistent" purely from sample size -- exactly the
# small-sample-extremes trap the original scouting prompt warns about).
# Instead: does each item's observed range come within a tolerance of the
# TABLE's observed min/max? That tolerates individual items not happening to
# touch the boundary, while still catching items on a genuinely different
# scale (e.g. a 1-9 item mixed in with 0-100 items).
BOUNDARY_APPROACH_TOL_FRAC <- 0.15  # an item counts as "reaching" a bound if
                         # it comes within this fraction of the table's
                         # observed range
ITEM_CONSISTENCY_FRAC <- 0.7  # fraction of items that must reach both the
                         # observed min and max (within tolerance) for the
                         # table's range to count as consistent across items

in_rds  <- "vignettes/continuous_scout_data/continuous_scout_raw.rds"
out_csv <- "continuous_scout_candidates.csv"
out_bib <- "continuous_scout_references.bib"

if (!file.exists(in_rds)) {
  stop("Raw cache not found at ", in_rds,
       " -- run continuous_scout_fetch.R first.")
}
raw <- readRDS(in_rds)
candidates    <- raw$candidates
table_summary <- raw$table_summary
item_summary  <- raw$item_summary

message("Loaded cache: ", nrow(table_summary), " tables, ",
        nrow(item_summary), " table x item rows")

# ==============================================================================
# resp-based signal
# ==============================================================================

is_round_endpoint <- function(x) any(abs(x - ROUND_ENDPOINTS) < ENDPOINT_TOL)

item_consistency <- item_summary |>
  left_join(table_summary |> select(table, min_resp, max_resp, range_resp), by = "table") |>
  mutate(
    scale = pmax(range_resp, 1e-9),
    tol = BOUNDARY_APPROACH_TOL_FRAC * scale,
    near_min = item_min <= min_resp + tol,
    near_max = item_max >= max_resp - tol
  ) |>
  group_by(table) |>
  summarise(
    near_min_frac = mean(near_min),
    near_max_frac = mean(near_max),
    .groups = "drop"
  ) |>
  mutate(
    range_consistent_across_items = (near_min_frac >= ITEM_CONSISTENCY_FRAC) &
      (near_max_frac >= ITEM_CONSISTENCY_FRAC)
  )

resp_signal <- table_summary |>
  left_join(item_consistency, by = "table") |>
  rowwise() |>
  mutate(endpoints_round = is_round_endpoint(min_resp) && is_round_endpoint(max_resp)) |>
  ungroup() |>
  mutate(
    # Truth table -- see header comment for rationale:
    #   round & consistent      -> bounded   (strong: fixed format, items agree)
    #   round & NOT consistent  -> unclear   (looks bounded, e.g. 0-100 VAS,
    #                                         but items don't uniformly reach
    #                                         the edges -- floor/ceiling
    #                                         effects or a mixed scale;
    #                                         table-level range still doesn't
    #                                         extend past an interpretable
    #                                         ceiling/floor, so NOT the
    #                                         "derived score" pattern)
    #   NOT round & consistent  -> unclear   (fixed range across items, but
    #                                         endpoints aren't on the
    #                                         interpretable list)
    #   NOT round & NOT consist.-> unbounded (clearest "derived/open-ended
    #                                         score" signal)
    resp_class = case_when(
      endpoints_round & range_consistent_across_items ~ "bounded",
      !endpoints_round & !range_consistent_across_items ~ "unbounded",
      TRUE ~ "unclear"
    )
  )

# ==============================================================================
# metadata-based signal (irw_tags()$item_format)
# ==============================================================================

tags_meta <- tryCatch(
  irw_tags(tables = candidates$table),
  error = function(e) {
    message("irw_tags() failed: ", conditionMessage(e))
    NULL
  }
)

if (is.null(tags_meta)) {
  message("Proceeding without item_format metadata signal -- tags unavailable")
  tags_meta <- tibble(table = character(), item_format = character(),
                       construct_type = character(), sample = character(),
                       measurement_tool = character(), construct_name = character())
}

metadata_signal <- tags_meta |>
  select(table, item_format) |>
  mutate(
    metadata_hint = case_when(
      item_format == "Slider/continuous" ~ "bounded",
      item_format == "Constructed Response" ~ "unbounded",
      TRUE ~ NA_character_   # "Likert Scale/selected response" and "Mixed"
                              # don't map cleanly to either for a >=12-category
                              # item -- no hint, not a vote either way
    )
  )

# ==============================================================================
# Combine the two signals
# ==============================================================================

combined <- resp_signal |>
  left_join(metadata_signal, by = "table") |>
  mutate(
    bound_class = case_when(
      resp_class != "unclear" & (is.na(metadata_hint) | metadata_hint == resp_class) ~ resp_class,
      resp_class != "unclear" & !is.na(metadata_hint) & metadata_hint != resp_class ~ "unclear",
      resp_class == "unclear" & !is.na(metadata_hint) ~ metadata_hint,
      TRUE ~ "unclear"
    ),
    signals_agree = !is.na(metadata_hint) & (metadata_hint == resp_class),
    signals_conflict = !is.na(metadata_hint) & (metadata_hint != resp_class) & (resp_class != "unclear")
  )

n_conflict <- sum(combined$signals_conflict)
if (n_conflict > 0) {
  message(n_conflict, " table(s) where resp-based and metadata signals conflict ",
          "(downgraded to unclear): ", paste(combined$table[combined$signals_conflict], collapse = ", "))
}

# ==============================================================================
# Consolidate and output
# ==============================================================================

final <- candidates |>
  select(table, n_categories) |>
  inner_join(
    combined |> select(table, n_unique, min_resp, max_resp, range_resp,
                        range_consistent_across_items, pct_noninteger,
                        resp_class, item_format, metadata_hint, bound_class,
                        signals_agree, signals_conflict),
    by = "table"
  )

if (nrow(tags_meta) > 0 && all(c("construct_type", "sample", "measurement_tool", "construct_name") %in% colnames(tags_meta))) {
  final <- final |>
    left_join(
      tags_meta |> select(table, construct_type, sample, measurement_tool, construct_name),
      by = "table"
    )
}

final <- final |> arrange(bound_class, desc(n_categories))

write.csv(final, out_csv, row.names = FALSE)
message("Wrote ", nrow(final), " rows to ", out_csv)

tryCatch(
  irw_save_bibtex(unique(final$table), output_file = out_bib),
  error = function(e) message("  bibtex generation failed: ", conditionMessage(e))
)

# ==============================================================================
# Console summary
# ==============================================================================

cat("\n--- Bound classification cross-tab (final, metadata-combined) ---\n")
print(table(final$bound_class, useNA = "ifany"))

cat("\n--- resp-only vs. final (shows what metadata changed) ---\n")
print(table(resp_only = final$resp_class, final = final$bound_class))

n_bounded <- sum(final$bound_class == "bounded")
n_unbounded <- sum(final$bound_class == "unbounded")
n_unclear <- sum(final$bound_class == "unclear")

cat("\n--- Judgment ---\n")
cat(sprintf(
  "Of %d tables with n_categories >= 12, %d classify as bounded, %d as unbounded, and %d as unclear.\n",
  nrow(final), n_bounded, n_unbounded, n_unclear
))
top_bounded <- final |> filter(bound_class == "bounded") |> arrange(desc(n_categories)) |> pull(table) |> head(5)
top_unbounded <- final |> filter(bound_class == "unbounded") |> arrange(desc(n_categories)) |> pull(table) |> head(5)
cat("Recommended for manual inspection first (bounded): ", paste(top_bounded, collapse = ", "), "\n")
cat("Recommended for manual inspection first (unbounded): ", paste(top_unbounded, collapse = ", "), "\n")
