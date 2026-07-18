# hf_reliability_compute.R
#
# Replicates the "reliability paradox" (Hedge, Powell & Sumner, 2018,
# Behavior Research Methods, 50:1166-1186) using IRW cognitive-control task
# data: individual conditions in a congruency-style contrast are far more
# reliable than the within-subject difference score computed from them,
# because the two conditions are highly correlated across people.
#
# Produces the precomputed results loaded by hf_reliability_paradox.qmd.
#
# Output: hf_reliability/hf_reliability_results.rds
#         hf_reliability/references.bib (dataset citations appended)
#
# Usage:
#   Rscript vignettes/hf_reliability_compute.R   # from project root

library(irw)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(data.table)

set.seed(20260716)

out_dir  <- "vignettes/hf_reliability"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
bib_file <- file.path(out_dir, "references.bib")

N_TRIAL_FLOOR      <- 5
N_TRIAL_FLOOR_SENS <- 10
N_SPLITS           <- 500
MIN_N_RESPONDENTS  <- 50

spearman_brown <- function(r) 2 * r / (1 + r)

# irw_fetch() occasionally fails transiently against the Redivis API
# (observed for several tables during this vignette's dataset search, not
# specific to any one table). Retry with a short backoff before treating a
# fetch as a genuine failure (e.g. a table that really doesn't exist).
irw_fetch_retry <- function(table, tries = 3, wait_s = 5) {
  for (i in seq_len(tries)) {
    df <- tryCatch(irw_fetch(table), error = function(e) NULL)
    if (!is.null(df)) return(df)
    if (i < tries) {
      message("  irw_fetch('", table, "') failed (attempt ", i, "/", tries, "), retrying...")
      Sys.sleep(wait_s)
    }
  }
  message("  irw_fetch('", table, "') failed after ", tries, " attempts.")
  NULL
}

# ==============================================================================
# Step 0: locate candidate datasets
#
# Narrow IRW's metadata to tables that are plausibly trial-level
# congruent/incongruent (or go/no-go, compatible/incompatible) designs, using
# a keyword screen over table names, dataset descriptions, and variable
# lists, then fetch each survivor and report basic structural facts (does it
# have a repeated-trial structure? an rt column? a plausible 2-level
# condition-like variable?) for manual confirmation.
# ==============================================================================

message("=== Step 0: candidate dataset search ===")

md <- irw_metadata()

kw_pattern <- paste(
  "heart", "flower", "imps2025", "inhibit", "executive", "flanker", "stroop",
  "simon", "nogo", "no.?go", "congruent", "incongruent", "compatib",
  "conflict", "stop.?signal", "eriksen", "attention network",
  sep = "|"
)

candidate_tables <- md %>%
  filter(
    grepl(kw_pattern, table,   ignore.case = TRUE) |
    grepl(kw_pattern, dataset, ignore.case = TRUE) |
    grepl(kw_pattern, variables, ignore.case = TRUE)
  ) %>%
  pull(table)

message("Keyword screen matched ", length(candidate_tables), " tables: ",
        paste(candidate_tables, collapse = ", "))

# Small-cardinality columns (excluding id/item/resp/rt/date/wave/cov_*) are
# candidate condition variables; the item column is also checked for a
# common two-level prefix (e.g. "go_left_easy" / "stop_left_easy").
detect_condition_candidates <- function(df) {
  skip <- c("id", "item", "resp", "rt")
  cols <- setdiff(names(df), skip)
  out <- list()
  for (cl in cols) {
    v <- df[[cl]]
    if (!is.character(v) && !is.factor(v)) next
    nu <- length(unique(na.omit(v)))
    if (nu >= 2 && nu <= 6) out[[cl]] <- sort(unique(na.omit(v)))
  }
  if ("item" %in% names(df)) {
    prefixes <- unique(sub("_.*$", "", df$item))
    if (length(prefixes) == 2) out[["item_prefix"]] <- prefixes
  }
  out
}

raw_data <- list()
step0_rows <- list()

for (tbl in candidate_tables) {
  df <- irw_fetch_retry(tbl)
  if (is.null(df)) {
    step0_rows[[tbl]] <- tibble(table = tbl, n_responses = NA, n_participants = NA,
                                 has_rt = NA, condition_candidates = "fetch failed",
                                 has_wave = NA, decision = "excluded: fetch failed")
    next
  }
  raw_data[[tbl]] <- df

  has_rt   <- "rt" %in% names(df)
  has_wave <- any(c("time", "wave", "date") %in% names(df))
  cond_cand <- detect_condition_candidates(df)
  cond_str <- if (length(cond_cand) == 0) "none found" else
    paste(sprintf("%s={%s}", names(cond_cand), map_chr(cond_cand, paste, collapse = ",")),
          collapse = "; ")

  n_participants <- length(unique(df$id))

  step0_rows[[tbl]] <- tibble(
    table = tbl,
    n_responses = nrow(df),
    n_participants = n_participants,
    has_rt = has_rt,
    condition_candidates = cond_str,
    has_wave = has_wave
  )
}

step0_summary <- bind_rows(step0_rows)
print(step0_summary, width = Inf)

# ------------------------------------------------------------------------------
# Manual confirmation of the final candidate list, informed by the Step 0 scan
# above plus direct inspection of each table's condition-coding scheme
# (documented in the prep_* functions below):
#
#   imps2025_hf (Hearts & Flowers, imps2025 datathon data) -- KEPT, as two
#     variants: "blocked" (hearts test vs. flowers test blocks) and "mixed"
#     (item-level heart/flower trials within the mixed test block). `block`
#     has 3 levels (hearts/flowers/mixed test), not 2 -- the mixed block is
#     used as a second, trial-intermixed contrast rather than discarded.
#     Has a genuine `time` wave variable -> supports test-retest.
#
#   alcoholstroop_jones2024 (addiction/attentional-bias Stroop) -- KEPT.
#     `stimulus` (0 = neutral word, 1 = alcohol-related word) is the
#     congruent/incongruent analogue: an attentional-bias RT/accuracy
#     contrast rather than a classic color-word Stroop conflict, but
#     structurally identical (two conditions, per-trial rt + resp). No wave
#     variable -> split-half only.
#
#   matzke-etal-2019 (stop-signal task) -- KEPT, N=53 respondents (just above
#     the 50-respondent floor). `item` prefix go_/stop_ gives the condition.
#     Successful stop trials have no RT by construction, so unlike Hedge et
#     al.'s SSRT (race-model estimate), we use failed-stop-trial RT as a
#     naive structural analogue -- flagged prominently as a deviation in the
#     vignette. No wave variable -> split-half only.
#
#   matzke-experiment1-2022, matzke-experiment3-standard,
#   matzke-experiment3-gamified-2022 -- EXCLUDED: N=7, 9, 9 respondents,
#   below the 50-respondent floor.
#
#   wang2022_iat -- EXCLUDED: no rt column and a fixed 20-item bank (each
#     item answered ~once per person), i.e. a survey-style dichotomous IRT
#     dataset despite the "iat" name, not a trial-level RT task.
#
#   mexico_*_safety_conflicts (8 tables) -- EXCLUDED: keyword false positive
#     on "conflicts" (a public-safety perceptions survey); no rt column, no
#     congruent/incongruent structure.
#
#   enkavi_2019_stroop, enkavi_2019_simon, enkavi_2019_ant_flanker -- KEPT.
#     Located by direct inspection of item_response_warehouse_3, not via the
#     keyword scan above: at the time of writing these tables aren't yet
#     indexed in irw_metadata(), so the keyword screen can't find them, but
#     irw_fetch() resolves them directly by table name regardless. All three
#     are part of the Enkavi, Eisenberg et al. (2019) Self-Regulation
#     Ontology test-retest battery: N=523, a genuine 2-wave retest design
#     (same structure as imps2025_hf's wave variable, but a much shorter,
#     general-population adult retest interval rather than a ~6-month
#     child-development gap -- no per-wave date is available in the table
#     itself, so see Enkavi et al. (2019) directly for the interval design).
#     stroop and simon are already a clean binary congruent/incongruent
#     contrast. ant_flanker's itemcov_condition has a third "neutral" level;
#     those trials are excluded here to match the two-condition contrast
#     used throughout this vignette.
#
#   enkavi_2019_navon, enkavi_2019_stopsignal, enkavi_2019_gonogo,
#   enkavi_2019_taskswitch, enkavi_2019_dpx_axcpt -- EXCLUDED for now (same
#   battery, same N=523/2-wave design). navon has a 2 (level) x 3 (conflict)
#   structure needing a bespoke contrast; stopsignal/gonogo/taskswitch/
#   dpx_axcpt aren't binary congruent/incongruent contrasts (go/no-go,
#   switch cost, AX-CPT probe types) and would need either engine changes or
#   a separate analysis. Good candidates for a follow-up extension.
# ------------------------------------------------------------------------------

final_tables <- c("imps2025_hf", "alcoholstroop_jones2024", "matzke-etal-2019",
                   "enkavi_2019_stroop", "enkavi_2019_simon", "enkavi_2019_ant_flanker")
message("\nFinal candidate tables: ", paste(final_tables, collapse = ", "))

# ------------------------------------------------------------------------------
# The enkavi_2019_* tables aren't yet indexed in irw_metadata() (so the
# keyword screen above can't find them), but irw_fetch() resolves them
# directly by table name like any other IRW table -- fetched here as a
# manual addition to the candidate set.
# ------------------------------------------------------------------------------

enkavi_step0_rows <- list()
for (tbl in c("enkavi_2019_stroop", "enkavi_2019_simon", "enkavi_2019_ant_flanker")) {
  message("Fetching: ", tbl)
  df <- irw_fetch_retry(tbl)
  raw_data[[tbl]] <- df

  if (is.null(df)) {
    enkavi_step0_rows[[tbl]] <- tibble(table = tbl, n_responses = NA, n_participants = NA,
                                        has_rt = NA, condition_candidates = "fetch failed",
                                        has_wave = NA)
    next
  }

  cond_cand <- detect_condition_candidates(df)
  cond_str <- if (length(cond_cand) == 0) "none found" else
    paste(sprintf("%s={%s}", names(cond_cand), map_chr(cond_cand, paste, collapse = ",")),
          collapse = "; ")

  enkavi_step0_rows[[tbl]] <- tibble(
    table = tbl,
    n_responses = nrow(df),
    n_participants = length(unique(df$id)),
    has_rt = "rt" %in% names(df),
    condition_candidates = cond_str,
    has_wave = "wave" %in% names(df)
  )
}
step0_summary <- bind_rows(step0_summary, bind_rows(enkavi_step0_rows))

# ==============================================================================
# Generic split-half / test-retest reliability engine
# ==============================================================================

# One random half-split per (id, condition) group; ties broken randomly so
# groups of odd size alternate which half gets the extra trial across splits.
assign_random_half <- function(dt) {
  dt[, half := (data.table::frank(stats::runif(.N), ties.method = "random") <= ceiling(.N / 2)) + 1L,
     by = .(id, condition)]
  dt
}

# Given long-format half-condition means (id, condition, half, value), return
# split-half correlations (raw, pre Spearman-Brown) for congruent alone,
# incongruent alone, and the difference score.
split_correlations <- function(summ) {
  wide <- data.table::dcast(summ, id ~ condition + half, value.var = "value")
  needed <- c("congruent_1", "congruent_2", "incongruent_1", "incongruent_2")
  if (!all(needed %in% names(wide))) return(c(congruent = NA_real_, incongruent = NA_real_, difference = NA_real_))
  cc <- suppressWarnings(cor(wide$congruent_1,   wide$congruent_2,   use = "pairwise.complete.obs"))
  ic <- suppressWarnings(cor(wide$incongruent_1, wide$incongruent_2, use = "pairwise.complete.obs"))
  d1 <- wide$incongruent_1 - wide$congruent_1
  d2 <- wide$incongruent_2 - wide$congruent_2
  dd <- suppressWarnings(cor(d1, d2, use = "pairwise.complete.obs"))
  c(congruent = cc, incongruent = ic, difference = dd)
}

# Runs N_SPLITS random split-halves on a prepared trial-level data.table
# (columns: id, condition, rt, correct, rt_eligible) and returns mean +/- SD
# of the Spearman-Brown-corrected reliability for RT and accuracy, each for
# congruent, incongruent, and the difference score.
run_split_half <- function(dt, n_splits = N_SPLITS) {
  dt <- data.table::copy(dt)
  dt[, rt_ok := rt_eligible == 1L & !is.na(rt)]

  rt_mat  <- matrix(NA_real_, nrow = n_splits, ncol = 3, dimnames = list(NULL, c("congruent", "incongruent", "difference")))
  acc_mat <- rt_mat

  for (s in seq_len(n_splits)) {
    dt <- assign_random_half(dt)

    rt_summ <- dt[rt_ok == TRUE, .(value = mean(rt)), by = .(id, condition, half)]
    rt_mat[s, ] <- spearman_brown(split_correlations(rt_summ))

    acc_summ <- dt[, .(value = mean(correct)), by = .(id, condition, half)]
    acc_mat[s, ] <- spearman_brown(split_correlations(acc_summ))
  }

  list(
    rt  = tibble(measure = "rt",  reliability_type = colnames(rt_mat),
                 reliability_est = colMeans(rt_mat, na.rm = TRUE),
                 reliability_sd  = apply(rt_mat, 2, sd, na.rm = TRUE)),
    acc = tibble(measure = "acc", reliability_type = colnames(acc_mat),
                 reliability_est = colMeans(acc_mat, na.rm = TRUE),
                 reliability_sd  = apply(acc_mat, 2, sd, na.rm = TRUE))
  )
}

# Full-data (no splitting) per-person condition means, used for variance
# components and test-retest correlations.
person_condition_means <- function(dt) {
  dt <- as.data.table(dt)
  dt[, rt_ok := rt_eligible == 1L & !is.na(rt)]
  rt_m  <- dt[rt_ok == TRUE, .(rt = mean(rt)), by = .(id, condition)]
  acc_m <- dt[, .(acc = mean(correct)), by = .(id, condition)]

  rt_w  <- data.table::dcast(rt_m,  id ~ condition, value.var = "rt")
  acc_w <- data.table::dcast(acc_m, id ~ condition, value.var = "acc")
  names(rt_w)[-1]  <- paste0("rt_",  names(rt_w)[-1])
  names(acc_w)[-1] <- paste0("acc_", names(acc_w)[-1])

  out <- merge(rt_w, acc_w, by = "id", all = TRUE) %>% as_tibble()
  out %>% mutate(
    D_rt  = rt_incongruent - rt_congruent,
    D_acc = acc_congruent  - acc_incongruent
  )
}

variance_components <- function(means_wide, measure = c("rt", "acc")) {
  measure <- match.arg(measure)
  cong  <- means_wide[[paste0(measure, "_congruent")]]
  incng <- means_wide[[paste0(measure, "_incongruent")]]
  D     <- if (measure == "rt") means_wide$D_rt else means_wide$D_acc
  tibble(
    measure = measure,
    var_congruent   = var(cong,  na.rm = TRUE),
    var_incongruent = var(incng, na.rm = TRUE),
    cov_cong_incong = cov(cong, incng, use = "pairwise.complete.obs"),
    var_difference  = var(D, na.rm = TRUE)
  )
}

# ==============================================================================
# Trial-floor filtering + reporting helpers
# ==============================================================================

apply_trial_floor <- function(dt, floor_n) {
  counts <- dt[, .N, by = .(id, condition)]
  wide_n <- data.table::dcast(counts, id ~ condition, value.var = "N", fill = 0)
  ok_ids <- wide_n$id[wide_n$congruent >= floor_n & wide_n$incongruent >= floor_n]
  dt[id %in% ok_ids]
}

trial_summary <- function(dt) {
  dt[, .N, by = .(id, condition)][, .(mean_trials = mean(N)), by = condition]
}

# ==============================================================================
# Per-dataset prep functions
#
# Each returns a standardized trial-level tibble:
#   id, condition (factor: congruent/incongruent), rt, correct (0/1),
#   rt_eligible (0/1, trials counted toward the RT mean), wave
# ==============================================================================

prep_hf_blocked <- function(df) {
  df %>%
    filter(block %in% c("hearts test", "flowers test")) %>%
    transmute(
      id = as.character(id),
      condition = factor(ifelse(block == "hearts test", "congruent", "incongruent"),
                          levels = c("congruent", "incongruent")),
      rt = rt, correct = as.integer(resp), rt_eligible = as.integer(resp),
      wave = time
    )
}

prep_hf_mixed <- function(df) {
  df %>%
    filter(block == "mixed test") %>%
    transmute(
      id = as.character(id),
      condition = factor(ifelse(stim_shape == "heart", "congruent", "incongruent"),
                          levels = c("congruent", "incongruent")),
      rt = rt, correct = as.integer(resp), rt_eligible = as.integer(resp),
      wave = time
    )
}

prep_alcoholstroop <- function(df) {
  df %>%
    mutate(rt = as.numeric(rt)) %>%
    transmute(
      id = as.character(id),
      condition = factor(ifelse(stimulus == 0, "congruent", "incongruent"),
                          levels = c("congruent", "incongruent")),
      rt = rt, correct = as.integer(resp), rt_eligible = as.integer(resp),
      wave = NA_character_
    )
}

prep_matzke <- function(df) {
  df %>%
    mutate(
      cond_raw    = ifelse(grepl("^go", item), "go", "stop"),
      dir_correct = (resp == -1 & cov_s == "left") | (resp == 1 & cov_s == "right"),
      correct     = ifelse(cond_raw == "go", as.integer(dir_correct), as.integer(is.na(rt))),
      # RT eligibility deviates from "correct trials only" for the stop
      # condition: successful inhibitions have no RT by construction, so we
      # use failed-stop RTs (a response occurred) as a naive structural
      # analogue to SSRT. See the caveat in the vignette methods section.
      rt_eligible = ifelse(cond_raw == "go", as.integer(dir_correct), as.integer(!is.na(rt)))
    ) %>%
    transmute(
      id = as.character(id),
      condition = factor(ifelse(cond_raw == "go", "congruent", "incongruent"),
                          levels = c("congruent", "incongruent")),
      rt = rt, correct = correct, rt_eligible = rt_eligible,
      wave = NA_character_
    )
}

# Enkavi et al. (2019) tasks already carry a two-level itemcov_condition
# column coded exactly as congruent/incongruent, and rt/resp in the standard
# IRW shape -- no condition recoding needed, unlike the datasets above.
prep_enkavi_binary <- function(df) {
  df %>%
    transmute(
      id = as.character(id),
      condition = factor(itemcov_condition, levels = c("congruent", "incongruent")),
      rt = as.numeric(rt), correct = as.integer(resp), rt_eligible = as.integer(resp),
      wave = as.character(wave)
    )
}

# ANT Flanker adds a third "neutral" itemcov_condition level (no flankers);
# those trials are dropped here to keep the same binary contrast used
# elsewhere in this vignette.
prep_enkavi_flanker <- function(df) {
  df %>%
    filter(itemcov_condition %in% c("congruent", "incongruent")) %>%
    prep_enkavi_binary()
}

# ==============================================================================
# Dataset specs
# ==============================================================================

hf_wave_order <- c(
  "2021-2022 Fall" = 1, "2021-2022 Spring" = 2,
  "2022-2023 Fall" = 3, "2022-2023 Spring" = 4,
  "PLUS Fall" = 10, "PLUS Spring" = 11
)

enkavi_wave_order <- c("1" = 1, "2" = 2)
enkavi_wave_pairs  <- list(c("1", "2"))

specs <- list(
  list(key = "imps2025_hf_blocked", table = "imps2025_hf", task_type = "congruency",
       prep_fn = prep_hf_blocked, wave_order = hf_wave_order,
       wave_pairs = list(c("PLUS Fall", "PLUS Spring"), c("2021-2022 Fall", "2021-2022 Spring")),
       notes = "Hearts & Flowers (imps2025 datathon): hearts test block = congruent (respond same side), flowers test block = incongruent (respond opposite side)."),
  list(key = "imps2025_hf_mixed", table = "imps2025_hf", task_type = "congruency",
       prep_fn = prep_hf_mixed, wave_order = hf_wave_order,
       wave_pairs = list(c("PLUS Fall", "PLUS Spring"), c("2021-2022 Fall", "2021-2022 Spring")),
       notes = "Hearts & Flowers mixed test block: trial-intermixed congruent/incongruent contrast (heart vs. flower stimulus within the same block), unlike the blocked design above."),
  list(key = "alcoholstroop_jones2024", table = "alcoholstroop_jones2024", task_type = "attentional-bias",
       prep_fn = prep_alcoholstroop, wave_order = NULL, wave_pairs = NULL,
       notes = "Addiction/attentional-bias Stroop: neutral word (congruent analogue) vs. alcohol-related word (incongruent analogue). Not a classic color-word conflict task, but structurally identical: per-trial rt + accuracy in two conditions. No wave variable -- split-half only."),
  list(key = "matzke_stopsignal_2019", table = "matzke-etal-2019", task_type = "response-inhibition (structural analogue only)",
       prep_fn = prep_matzke, wave_order = NULL, wave_pairs = NULL,
       notes = "Stop-signal task: go trials (congruent analogue) vs. stop-signal trials (incongruent analogue). RT for the stop condition uses failed-stop-trial RT (a response occurred), NOT Hedge et al.'s SSRT race-model estimate -- interpret D_rt and D_acc here as a naive structural analogue only, not a validated inhibition measure. No wave variable -- split-half only."),
  list(key = "enkavi_stroop", table = "enkavi_2019_stroop", task_type = "congruency",
       prep_fn = prep_enkavi_binary, wave_order = enkavi_wave_order, wave_pairs = enkavi_wave_pairs,
       notes = "Enkavi, Eisenberg et al. (2019) Self-Regulation Ontology battery: classic color-word Stroop, congruent vs. incongruent, N=523. Genuine 2-wave retest design (general-population adults; a much shorter interval than the imps2025_hf child/development gap, though the exact interval isn't recoverable from the table itself)."),
  list(key = "enkavi_simon", table = "enkavi_2019_simon", task_type = "congruency",
       prep_fn = prep_enkavi_binary, wave_order = enkavi_wave_order, wave_pairs = enkavi_wave_pairs,
       notes = "Enkavi, Eisenberg et al. (2019) Self-Regulation Ontology battery: Simon task (spatial stimulus-response compatibility), congruent vs. incongruent, N=523, 2-wave retest."),
  list(key = "enkavi_ant_flanker", table = "enkavi_2019_ant_flanker", task_type = "congruency",
       prep_fn = prep_enkavi_flanker, wave_order = enkavi_wave_order, wave_pairs = enkavi_wave_pairs,
       notes = "Enkavi, Eisenberg et al. (2019) Self-Regulation Ontology battery: Attention Network Test flanker component, congruent vs. incongruent (neutral-flanker trials excluded to match the binary contrast used elsewhere in this vignette), N=523, 2-wave retest.")
)

# ==============================================================================
# Main loop
# ==============================================================================

message("\n=== Running reliability analyses ===")

result_rows <- list()

for (spec in specs) {
  message("\n--- ", spec$key, " ---")
  df <- raw_data[[spec$table]]
  if (is.null(df)) df <- irw_fetch_retry(spec$table)
  if (is.null(df)) {
    message("  SKIPPED: could not fetch ", spec$table)
    next
  }

  trial_dt <- as.data.table(spec$prep_fn(df))

  for (floor_n in c(N_TRIAL_FLOOR, N_TRIAL_FLOOR_SENS)) {

    dt_floor <- apply_trial_floor(trial_dt, floor_n)
    n_resp <- length(unique(dt_floor$id))
    message("  floor=", floor_n, ": N respondents = ", n_resp)

    if (n_resp < MIN_N_RESPONDENTS) {
      message("  SKIPPED: fewer than ", MIN_N_RESPONDENTS, " respondents at floor=", floor_n)
      next
    }

    # Restrict to a single session per person for split-half (earliest
    # available wave, or all rows if there is no wave variable) so
    # split-half reliability isn't inflated by pooling repeated sessions.
    if (!is.null(spec$wave_order)) {
      dt_single <- copy(dt_floor)
      dt_single[, wave_ord := spec$wave_order[wave]]
      dt_single <- dt_single[dt_single[, .I[wave_ord == min(wave_ord)], by = id]$V1]
      dt_single <- apply_trial_floor(dt_single, floor_n)
    } else {
      dt_single <- dt_floor
    }

    ts <- trial_summary(dt_single)
    n_resp_single <- length(unique(dt_single$id))

    sh <- run_split_half(dt_single, N_SPLITS)
    means_wide <- person_condition_means(dt_single)
    vc_rt  <- variance_components(means_wide, "rt")
    vc_acc <- variance_components(means_wide, "acc")

    for (m in c("rt", "acc")) {
      sh_m <- sh[[m]]
      vc_m <- if (m == "rt") vc_rt else vc_acc
      for (i in seq_len(nrow(sh_m))) {
        result_rows[[length(result_rows) + 1]] <- tibble(
          dataset = spec$key, task_type = spec$task_type, measure = m,
          reliability_type = sh_m$reliability_type[i],
          reliability_est = sh_m$reliability_est[i],
          reliability_sd = sh_m$reliability_sd[i],
          trial_floor = floor_n,
          n_respondents = n_resp_single,
          mean_trials_congruent = ts$mean_trials[ts$condition == "congruent"],
          mean_trials_incongruent = ts$mean_trials[ts$condition == "incongruent"],
          var_congruent = vc_m$var_congruent, var_incongruent = vc_m$var_incongruent,
          cov_cong_incong = vc_m$cov_cong_incong, var_difference = vc_m$var_difference,
          notes = spec$notes
        )
      }
    }

    # Test-retest (floor=5 only, to keep runtime and scope manageable)
    if (floor_n == N_TRIAL_FLOOR && !is.null(spec$wave_pairs)) {
      for (pair in spec$wave_pairs) {
        dt_w1 <- apply_trial_floor(dt_floor[wave == pair[1]], floor_n)
        dt_w2 <- apply_trial_floor(dt_floor[wave == pair[2]], floor_n)
        ids_both <- intersect(unique(dt_w1$id), unique(dt_w2$id))
        if (length(ids_both) < MIN_N_RESPONDENTS) {
          message("  test-retest ", paste(pair, collapse = " -> "), ": only ", length(ids_both),
                  " ids in both waves, skipping")
          next
        }
        mw1 <- person_condition_means(dt_w1[id %in% ids_both]) %>% rename_with(~paste0(., "_w1"), -id)
        mw2 <- person_condition_means(dt_w2[id %in% ids_both]) %>% rename_with(~paste0(., "_w2"), -id)
        mw  <- inner_join(mw1, mw2, by = "id")

        retest_label <- paste0("test-retest: ", pair[1], " -> ", pair[2])
        for (m in c("rt", "acc")) {
          for (cond in c("congruent", "incongruent", "difference")) {
            v1name <- if (cond == "difference") paste0("D_", m, "_w1") else paste0(m, "_", cond, "_w1")
            v2name <- if (cond == "difference") paste0("D_", m, "_w2") else paste0(m, "_", cond, "_w2")
            r <- suppressWarnings(cor(mw[[v1name]], mw[[v2name]], use = "pairwise.complete.obs"))
            result_rows[[length(result_rows) + 1]] <- tibble(
              dataset = spec$key, task_type = spec$task_type, measure = m,
              reliability_type = paste0(cond, " (", retest_label, ")"),
              reliability_est = r, reliability_sd = NA_real_,
              trial_floor = floor_n, n_respondents = length(ids_both),
              mean_trials_congruent = NA_real_, mean_trials_incongruent = NA_real_,
              var_congruent = NA_real_, var_incongruent = NA_real_,
              cov_cong_incong = NA_real_, var_difference = NA_real_,
              notes = spec$notes
            )
          }
        }
      }
    }
  }
}

results <- bind_rows(result_rows)
message("\nDone. ", nrow(results), " result rows across ", n_distinct(results$dataset), " dataset variants.")

# ==============================================================================
# Hedge, Powell & Sumner (2018) reference values
#
# Hardcoded from their Table 1 (Studies 1 and 2; test-retest ICCs, two-way
# random-effects, absolute agreement, ~3 weeks apart). We report the
# average of Study 1 and Study 2 as a single representative estimate.
# ==============================================================================

hedge2018_reference <- tribble(
  ~task,        ~measure, ~condition,     ~icc_study1, ~icc_study2,
  "Flanker",    "rt",     "congruent",     0.74,        0.69,
  "Flanker",    "rt",     "incongruent",   0.66,        0.62,
  "Flanker",    "rt",     "difference",    0.40,        0.57,
  "Flanker",    "acc",    "congruent",     0.46,        0.37,
  "Flanker",    "acc",    "incongruent",   0.71,        0.58,
  "Flanker",    "acc",    "difference",    0.58,        0.72,
  "Stroop",     "rt",     "congruent",     0.77,        0.72,
  "Stroop",     "rt",     "incongruent",   0.67,        0.70,
  "Stroop",     "rt",     "difference",    0.60,        0.66,
  "Stroop",     "acc",    "congruent",     0.36,        0.42,
  "Stroop",     "acc",    "incongruent",   0.62,        0.39,
  "Stroop",     "acc",    "difference",    0.48,        0.44,
  "Go/No-go",   "rt",     "go",            0.74,        0.63,
  "Go/No-go",   "acc",    "commission",    0.76,        0.76,
  "Stop-signal","rt",     "go",            0.35,        0.57,
  "Stop-signal","rt",     "ssrt",          0.36,        0.49
) %>%
  mutate(icc_mean = (icc_study1 + icc_study2) / 2)

comparison_table <- results %>%
  filter(trial_floor == N_TRIAL_FLOOR, !grepl("test-retest", reliability_type)) %>%
  select(dataset, measure, reliability_type, reliability_est, reliability_sd, n_respondents)

message("\nComparison table (floor=5, split-half):")
print(comparison_table, n = 100)

# ==============================================================================
# Save results
# ==============================================================================

saveRDS(
  list(
    step0_summary = step0_summary,
    results = results,
    comparison_table = comparison_table,
    hedge2018_reference = hedge2018_reference,
    n_splits = N_SPLITS,
    trial_floors = c(N_TRIAL_FLOOR, N_TRIAL_FLOOR_SENS),
    date_run = Sys.Date(),
    session = sessionInfo()
  ),
  file = file.path(out_dir, "hf_reliability_results.rds")
)
message("\nSaved to ", out_dir, "/hf_reliability_results.rds")

# ==============================================================================
# Bibtex citations
#
# enkavi_2019_* tables aren't yet indexed in irw_metadata(), so
# irw_save_bibtex() can't look them up -- excluded here to avoid a single
# unknown-table lookup failing bibtex generation for every table. Their
# citation (Enkavi et al., 2019) is added by hand to references.bib instead.
# ==============================================================================

bibtex_tables <- setdiff(unique(map_chr(specs, "table")),
                          c("enkavi_2019_stroop", "enkavi_2019_simon", "enkavi_2019_ant_flanker"))

generated <- tryCatch(
  irw_save_bibtex(bibtex_tables, output_file = file.path(out_dir, "irw_references.bib")),
  error = function(e) { message("irw_save_bibtex failed: ", conditionMessage(e)); character(0) }
)

# Only append entries whose citation key isn't already in bib_file, so
# re-running this script doesn't duplicate citations on every run.
if (length(generated) > 0) {
  entry_key <- function(entry) sub("^@\\w+\\{([^,]+),.*$", "\\1", trimws(entry))
  existing_keys <- if (file.exists(bib_file)) {
    bib_lines <- readLines(bib_file)
    key_lines <- grep("^@\\w+\\{", bib_lines, value = TRUE)
    vapply(key_lines, entry_key, character(1), USE.NAMES = FALSE)
  } else character(0)
  new_entries <- generated[!vapply(generated, entry_key, character(1)) %in% existing_keys]
  if (length(new_entries) > 0) {
    cat(paste0(new_entries, "\n"), file = bib_file, append = TRUE, sep = "\n")
    message(length(new_entries), " new citation(s) appended to ", bib_file)
  } else {
    message("No new citations to append to ", bib_file, " (all already present).")
  }
}
