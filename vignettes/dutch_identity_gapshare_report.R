# dutch_identity_gapshare_report.R
#
# Turns the output of dutch_identity_gapshare.R into the summary and figures.
# Reads vignettes/dutchdata/gapshare_tables.csv (+ gapshare_bigJ.csv if present)
# and writes gapshare_summary.md and three PNGs.
#
# The one-sentence diagnoses of the lowest-share tables are hand-written; this
# script emits the facts they rest on and a placeholder line for each, which is
# then filled in by hand. Rerunning overwrites them.
#
# Usage:
#   Rscript vignettes/dutch_identity_gapshare_report.R   # from project root

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

out_dir <- "vignettes/dutchdata"
irw_blue <- "#2c6fa6"; irw_red <- "#b4454b"; irw_grey <- "#8a8a8a"

main <- read.csv(file.path(out_dir, "gapshare_tables.csv"), stringsAsFactors = FALSE)
bigj_path <- file.path(out_dir, "gapshare_bigJ.csv")
bigj <- if (file.exists(bigj_path)) read.csv(bigj_path, stringsAsFactors = FALSE) else NULL
run  <- readRDS(file.path(out_dir, "gapshare_run.rds"))
excl <- read.csv(file.path(out_dir, "gapshare_excluded.csv"), stringsAsFactors = FALSE)

# Hand-written diagnoses of the lowest-share tables. Grounded in the item names
# in gapshare_tables.csv and, where noted, the table's processing notes.
DIAGNOSES <- c(
  onah_2021_covid_info_sources =
    "the ten 'items' are COVID information **sources** (church/mosque, radio, NCDC/WHO/UNICEF websites, newspapers, social media, television, health worker, friends and family), not indicators of a trait; official-website use and interpersonal sources are separate clusters, so multi-cluster pairwise structure is built in",
  spain_2017_politics_agreement =
    "the weakest association in the set -- the full SOE beats independence by only 0.028 nats per held-out respondent, so the share is a ratio of two small numbers and is correspondingly unstable; a low share here means 'little to explain', not 'rank-1 fails'",
  gilbert_meta_6 =
    "Bruhn (2016) financial literacy, using the authors' dummy-coded items (`data/gilbert_hte/il_hte_06.R`); the ten items span separate sections of the instrument and the A/B suffixes mark sub-parts of shared parent questions, both routes to local dependence",
  teicher_2015_mace_phys_neglect =
    "the MACE physical-neglect subscale mixes material deprivation (no food, dirty clothes) with supervision and protection (parent in ER, parent protects, looked out for each other) -- distinct facets inside one short subscale, i.e. testlet structure; N = 1,050 is also the smallest here, so the SOE's interaction parameters are poorly determined",
  wirs =
    "the WIRS data from the `ltm` package: six binary items on which channels management uses to consult employees; distinct consultation channels are not indicators of a single latent propensity"
)

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
iqr_str <- function(x) paste0("[", fmt(quantile(x, .25, na.rm = TRUE)), ", ",
                              fmt(quantile(x, .75, na.rm = TRUE)), "]")

unflagged <- filter(main, !flag_any)
usable    <- filter(main, share_out_usable)
usable_uf <- filter(usable, !flag_any)

# ------------------------------------------------------------------ figures --
p_hist <- main |>
  select(table, `In-sample` = share_in, `Held-out` = share_out_floor) |>
  pivot_longer(-table, names_to = "which", values_to = "share") |>
  filter(!is.na(share), share > -1, share < 2) |>
  ggplot(aes(x = share)) +
  geom_histogram(bins = 30, fill = irw_blue, alpha = 0.8, colour = "white") +
  geom_vline(xintercept = 1, linetype = "dashed", colour = irw_grey) +
  facet_wrap(~which, ncol = 1, scales = "free_y") +
  labs(x = "Gap share  (ℓ_rq − ℓ_ind) / (ℓ_soe − ℓ_ind)", y = "Tables",
       caption = "Held-out panel: tables with a usable denominator only. Dashed line at 1.")
ggsave(file.path(out_dir, "gapshare_hist.png"), p_hist,
       width = 7, height = 5.5, dpi = 150)

p_J <- main |>
  select(table, J, `In-sample` = share_in, `Held-out` = share_out_floor,
         share_out_usable) |>
  pivot_longer(c(`In-sample`, `Held-out`), names_to = "which", values_to = "share") |>
  filter(!is.na(share), which == "In-sample" | share_out_usable) |>
  ggplot(aes(x = factor(J), y = share)) +
  geom_boxplot(outlier.shape = NA, colour = irw_grey, fill = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, size = 1.8, colour = irw_blue) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = irw_grey) +
  facet_wrap(~which, ncol = 1, scales = "free_y") +
  labs(x = "Number of items (J)", y = "Gap share",
       caption = "The in-sample share is mechanically higher at small J: the SOE has fewer extra parameters to spend.")
ggsave(file.path(out_dir, "gapshare_by_J.png"), p_J, width = 7, height = 6, dpi = 150)

if ("construct_type" %in% names(main)) {
  ct <- main |>
    filter(!is.na(construct_type), construct_type != "") |>
    add_count(construct_type, name = "n_ct") |>
    filter(n_ct >= 3)
  if (nrow(ct)) {
    p_ct <- ct |>
      select(table, construct_type, `In-sample` = share_in,
             `Held-out` = share_out_floor, share_out_usable) |>
      pivot_longer(c(`In-sample`, `Held-out`), names_to = "which", values_to = "share") |>
      filter(!is.na(share), which == "In-sample" | share_out_usable) |>
      ggplot(aes(x = share, y = construct_type)) +
      geom_boxplot(outlier.shape = NA, colour = irw_grey, fill = NA) +
      geom_jitter(height = 0.15, width = 0, alpha = 0.6, size = 1.8, colour = irw_blue) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = irw_grey) +
      facet_wrap(~which, ncol = 1, scales = "free_x") +
      labs(x = "Gap share", y = NULL,
           caption = "Construct types with at least three tables.")
    ggsave(file.path(out_dir, "gapshare_by_construct.png"), p_ct,
           width = 7.5, height = 6, dpi = 150)
  }
}

# ------------------------------------------------------------------ summary --
low <- usable_uf |> arrange(share_out_floor) |> head(5)

lines <- c(
  "# How much of pairwise association does one latent dimension explain?",
  "",
  sprintf("Run %s. IRW versions pinned: `%s`.", run$date_run, run$irw_version),
  if (!is.na(run$pin_warning %||% NA))
    sprintf("Pin caveat: %s", gsub("\n", " ", run$pin_warning)) else NULL,
  sprintf("Seed %d, %d-fold CV over respondents.", run$seed, 5L),
  "",
  "## Quantity",
  "",
  "For each table, three nested loglinear models are fitted to the 2^J pattern",
  "counts (zero cells included): independence (J+1 parameters), the restricted",
  "log-quadratic with rank-1 Γ (2J+1), and the full SOE (1+J+J(J−1)/2). The gap",
  "share is (ℓ_rq − ℓ_ind) / (ℓ_soe − ℓ_ind).",
  "",
  "In sample the share is bounded in [0,1] and is inflated at small J. The",
  "held-out version uses 5-fold CV over respondents, summing the multinomial",
  "log-likelihood of each held-out fold under models fitted on the other four.",
  "",
  "**The held-out ratio is reported only where its denominator is usable.** Out",
  "of sample the SOE can score worse than independence; the ratio then flips",
  "sign and means nothing. The primary numbers are the per-held-out-respondent",
  "log-likelihood gaps in nats, which stay interpretable either way.",
  "",
  "Fitted cell probabilities are floored at 1/(10·N_train) before held-out",
  "evaluation, so a single respondent landing in a numerically empty cell cannot",
  "dominate a fold. Unfloored values are in the CSV.",
  "",
  "## Coverage",
  "",
  sprintf("- tables considered: %d", nrow(excl)),
  sprintf("- usable: %d", nrow(main)),
  sprintf("- flagged (glm warning, non-convergence, or |coef| > 15): %d", sum(main$flag_any)),
  sprintf("- held-out denominator usable: %d of %d", nrow(usable), nrow(main)),
  "",
  "Exclusion reasons (`gapshare_excluded.csv`):",
  "",
  paste0("- ", excl |> filter(!ok) |> count(reason_class = sub("[0-9].*$", "…", reason)) |>
           arrange(desc(n)) |> with(paste0("`", reason_class, "`: ", n))),
  "",
  "## Headline",
  "",
  sprintf("| | n | median | IQR |"),
  "|---|---|---|---|",
  sprintf("| in-sample, all | %d | %s | %s |", nrow(main),
          fmt(median(main$share_in, na.rm = TRUE)), iqr_str(main$share_in)),
  sprintf("| in-sample, unflagged | %d | %s | %s |", nrow(unflagged),
          fmt(median(unflagged$share_in, na.rm = TRUE)), iqr_str(unflagged$share_in)),
  sprintf("| **held-out, all** | %d | **%s** | %s |", nrow(usable),
          fmt(median(usable$share_out_floor, na.rm = TRUE)), iqr_str(usable$share_out_floor)),
  sprintf("| held-out, unflagged | %d | %s | %s |", nrow(usable_uf),
          fmt(median(usable_uf$share_out_floor, na.rm = TRUE)), iqr_str(usable_uf$share_out_floor)),
  "",
  "Per held-out respondent (nats, floored), median over usable tables:",
  "",
  sprintf("- rank-1 over independence: %s", fmt(median(usable$cv_gap_rq_nats, na.rm = TRUE), 4)),
  sprintf("- full SOE over independence: %s", fmt(median(usable$cv_gap_soe_nats, na.rm = TRUE), 4)),
  sprintf("- what the SOE adds beyond rank-1: %s",
          fmt(median(usable$cv_gap_soe_nats - usable$cv_gap_rq_nats, na.rm = TRUE), 4)),
  "",
  "![](gapshare_hist.png)",
  "",
  "## Against J",
  "",
  paste(capture.output(print(as.data.frame(
    main |> group_by(J) |>
      summarise(n = n(),
                med_in = round(median(share_in, na.rm = TRUE), 3),
                n_out = sum(share_out_usable, na.rm = TRUE),
                med_out = round(median(share_out_floor[share_out_usable], na.rm = TRUE), 3),
                .groups = "drop")))), collapse = "\n"),
  "",
  "![](gapshare_by_J.png)",
  ""
)

if ("construct_type" %in% names(main)) {
  ct_tab <- main |>
    mutate(construct_type = ifelse(is.na(construct_type) | construct_type == "",
                                   "(untagged)", construct_type)) |>
    group_by(construct_type) |>
    summarise(n = n(),
              med_in = round(median(share_in, na.rm = TRUE), 3),
              n_out = sum(share_out_usable, na.rm = TRUE),
              med_out = round(median(share_out_floor[share_out_usable], na.rm = TRUE), 3),
              .groups = "drop") |> arrange(desc(n))
  lines <- c(lines, "## By construct type", "",
             paste(capture.output(print(as.data.frame(ct_tab))), collapse = "\n"),
             "", "![](gapshare_by_construct.png)", "")
}

lines <- c(lines, "## Lowest held-out share (unflagged)", "",
  paste(capture.output(print(as.data.frame(
    low |> select(table, J, N, share_in, share_out_floor,
                  cv_gap_rq_nats, cv_gap_soe_nats,
                  any_of(c("construct_type", "measurement_tool"))) |>
      mutate(across(where(is.numeric), \(x) round(x, 3)))))), collapse = "\n"),
  "",
  paste0("- **", low$table, "** — ",
         ifelse(low$table %in% names(DIAGNOSES),
                DIAGNOSES[low$table], "_why:_ not yet diagnosed")),
  "",
  sprintf("Note that %d of the %d usable tables have a held-out SOE-over-independence gap below 0.05 nats per respondent. The share is a ratio there, so it is noisy even though the denominator is positive; read those rows alongside `cv_gap_soe_nats`.",
          sum(usable$cv_gap_soe_nats < 0.05, na.rm = TRUE), nrow(usable)),
  "")

if (!is.null(bigj)) {
  bj_run  <- tryCatch(readRDS(file.path(out_dir, "gapshare_run_bigJ.rds")),
                      error = function(e) NULL)
  bj_excl <- tryCatch(read.csv(file.path(out_dir, "gapshare_excluded_bigJ.csv"),
                               stringsAsFactors = FALSE),
                      error = function(e) NULL)
  bj_attempted <- if (is.null(bj_excl)) NA_integer_ else length(unique(bj_excl$table))
  bj_u    <- bigj$share_out_floor[bigj$share_out_usable]
  bj_rng  <- tapply(bigj$share_out_floor[bigj$share_out_usable],
                    bigj$table[bigj$share_out_usable],
                    function(x) diff(range(x)))
  lines <- c(lines, "", "## J > 10 stratum (10 items drawn at random, 5 draws per table)", "",
    sprintf("%d draws across %d tables. Held-out share median %s, IQR %s. Spread WITHIN a table across draws: median range %s, max %s.",
            nrow(bigj), length(unique(bigj$table)),
            fmt(median(bj_u, na.rm = TRUE)), iqr_str(bj_u),
            fmt(median(bj_rng, na.rm = TRUE)), fmt(max(bj_rng, na.rm = TRUE))),
    "",
    "Reported separately and not pooled with the main result: which ten items are drawn moves the answer.",
    "")
  # Coverage is only partial, and not a random subsample -- say so where the numbers are.
  if (!is.null(bj_run) && isTRUE(bj_run$cached_only) &&
      !is.null(bj_run$n_candidates) &&
      length(unique(bigj$table)) < bj_run$n_candidates) {
    lines <- c(lines, sprintf(
      paste("**Partial coverage.** The stratum had %d candidate tables; %d were attempted and %d",
            "yielded usable draws. Candidates are processed in ascending order of total responses,",
            "so what is here is the smaller-volume end of the stratum and what is missing is the",
            "large end (the `enem_*_1mil_*` family at ~1M respondents, `choi_2020_ednet_*`). This",
            "is a truncated sample, not a random one: read it as a demonstration that the answer",
            "moves with the item draw, not as an estimate for J > 10 tables as a class."),
      bj_run$n_candidates, bj_attempted, length(unique(bigj$table))), "")
  }
}

writeLines(lines, file.path(out_dir, "gapshare_summary.md"))
message("Wrote ", file.path(out_dir, "gapshare_summary.md"))
