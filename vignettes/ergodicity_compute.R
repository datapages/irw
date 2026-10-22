# ergodicity_compute.R
#
# Compute stage for "Does the group result describe the person?"
# (ergodicity.qmd).
#
# Question. Fisher, Medaglia & Jeronimus (2018, PNAS) argue that a group-level
# result transfers to an individual only under ergodicity -- homogeneity across
# people and stationarity within them -- and show, in six intensive-longitudinal
# datasets, that individual variability can run several times the group estimate.
# Adolf & Fried (2019, PNAS) reply that the inference needs LESS than ergodicity,
# so the honest deliverable is not a verdict ("these data are non-ergodic") but
# the quantity itself: how far apart are the within-person and between-person
# descriptions of the same items?
#
# This script measures that quantity across the curated IRW ESM pool -- 26
# tables against Fisher et al.'s 6 -- in two forms:
#
#   (1) ITEM level, the direct Fisher analogue. Per item: the between-person SD
#       (of person means) against the distribution of within-person SDs (each
#       person over their own occasions). Their headline is a ratio.
#
#   (2) PAIR level, which is the one that reads. Per item pair: the group
#       correlation computed on person means, against the distribution of
#       within-person correlations computed per person over occasions -- its
#       spread, and how often a person's correlation has the OPPOSITE SIGN to
#       the group's. The group says these two things go together; for some
#       fraction of people they move apart.
#
# Everything here is correlations and SDs -- no model fitting, no Stan. A
# multilevel/DSEM decomposition is deliberately out of scope (Stage B, not
# started; it needs Ben's sign-off first).
#
# THE CONFOUND THAT COULD MANUFACTURE THE WHOLE RESULT, and how it is handled:
#
#   * Precision. A within-person correlation on 12 occasions is mostly noise,
#     and noise alone produces spread and sign flips. So every pair carries the
#     spread EXPECTED under perfect homogeneity, computed analytically in
#     Fisher-z space from each person's own occasion count (se_i = 1/sqrt(T_i-3)),
#     plus the sign-flip rate expected from the same model. The reported
#     heterogeneity is the excess over that, never the raw spread.
#   * Unequal occasions within a table. Person-level summaries are therefore a
#     mix of precisions; each is reported unweighted AND weighted by (T_i - 3),
#     and the per-table occasion distribution is carried through to the page.
#   * Stationarity. Fisher et al. need it; ESM series trend. Every quantity is
#     computed twice, raw and after a per-person-per-item linear detrend on the
#     occasion index.
#   * Floor mass. A floor attenuates within-person correlations harder than
#     between-person ones, which would fake the headline. The per-item floor
#     percentages already measured by esm_floor.qmd are joined onto every item
#     and pair here as a covariate, so the page can show the ratio against the
#     floor instead of hoping it does not matter.
#   * Response direction is not harmonised in IRW. Reverse keying flips a
#     correlation's sign -- but it flips the group's and the person's together,
#     so the sign-flip rate (defined relative to the group) is invariant and the
#     SD ratios are sign-free. Each pair is nonetheless oriented so that
#     r_between >= 0 before anything is pooled across pairs, and the flip is
#     recorded.
#
# Outputs (committed; the cache is data, not a build artifact):
#   ergodicity_data/ergodicity_results.rds
#   ergodicity_data/fits/<table>.rds       (per-table, so a crash resumes; gitignored)
#   ergodicity_data/references.bib
#
# Usage:
#   Rscript vignettes/ergodicity_compute.R      # from the repo root
#
# Requires REDIVIS_API_TOKEN (read scope; nothing here writes to Redivis).

suppressPackageStartupMessages({
  library(irw); library(dplyr); library(tidyr); library(purrr)
  library(furrr); library(tibble)
})

set.seed(20261022)

PILOT          <- FALSE  # TRUE: the 6 anchor tables below, both response formats
MIN_OCC        <- 10L    # occasions a person needs before their own SD / correlation is used
MIN_PERSONS    <- 15L    # persons a pair needs before it is reported
MAX_PAIRS      <- 200L   # per comparable item group; seeded sample beyond this
# Stage A is correlations and SDs, no model fitting: the full pool is minutes,
# not hours, so this runs on ONE core by default and stays out of the way of
# whatever else the machine is doing. Raise it only if that changes.
WORKERS        <- 1L

DATA_DIR <- "vignettes/ergodicity_data"
FIT_DIR  <- file.path(DATA_DIR, "fits")
FLOOR_RDS <- "vignettes/esm_floor_data/esm_floor_results.rds"
dir.create(FIT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# The curated ESM pool -- taken verbatim from esm_floor_compute.R, not re-derived.
#
# No rule in IRW finds this set: metadata$longitudinal is a grep over column
# names that matches cov_birthdate, the intensive_longitudinal collection is
# 72/81 one COVID panel at ~2.6 waves, and there is no temporal tag. The set
# came from an occasions-per-person screen (responses per participant / item
# count) followed by reading every survivor's processing script.
#
# Four tables pass that screen and fail the script -- narcissism_schneider_2025
# (id defect), moralvignettes_rakhmankulova_2025 (id is a video, rater is the
# person), political_psychology (the script drops wave), musifeast17_vanderwalle
# (date is a clip trial inside one session) -- plus ~21 cognitive trial-level
# tables where "occasions" are trials. And two more were removed during the
# esm_floor build after inspection: emoji_scheffler_2024 (id = the emoji) and
# tears (id = Video_ID) are stimulus-rating designs where the person is `rater`,
# the same defect as moralvignettes. They are still listed as ESM in the scoping
# memory; they are not ESM. Curation, not a column rule, is what catches this.
# ---------------------------------------------------------------------------
ESM_SLIDER <- c(
  "westhoff2023_pbat", "westhoff2023_stopd",
  "vollbracht_et_al_2026_ambulatory_assessment",
  "nas_rogoza_2024_study5_nas", "nas_rogoza_2024_study5_ngs",
  "nas_rogoza_2024_study5_nvs",
  "zhang_2020_trait_creativity_mood", "opentsstvr_linnig_2025_vas"
)
ESM_ORDINAL <- c(
  "smoking_perseverance_mcneish_2025", "mhscdc_fried_2020_ema",
  "kuehner_2017_mw_rumination", "steinberg_2023_mentalizing_momentary",
  "evpromisi_stone_2021_ddeddep", "evpromisi_stone_2021_ddedanx",
  "evpromisi_stone_2021_ddpainin",
  "strohacker_2024_arms_readiness", "strohacker_2024_bmzi_motive",
  "gilbert_meta_101", "soderberg_2024_esm_affect", "soderberg_2024_esm_lecture",
  "soderberg_2024_esm_morning", "sondell_2018_dementia_motivation",
  "schafer_2016_music_goals", "schafer_2016_music_effects",
  "debacker_2018_justice_appraisal", "debacker_2018_decisionjustification"
)
ESM_POOL <- c(ESM_SLIDER, ESM_ORDINAL)

# Pilot anchors: three sliders and three ordinal tables, chosen as sanity checks
# rather than as a random sample.
#   smoking_perseverance_mcneish_2025 -- the dataset behind McNeish (2025), A
#     Primer on Intensive Longitudinal Psychometrics, so one table has a
#     published methods treatment to check against.
#   mhscdc_fried_2020_ema -- Fried's own EMA study, and the most severely floored
#     table in the pool (per-item floor mass to 77.7%), so if the floor confound
#     can break this analysis it breaks here.
#   vollbracht_et_al_2026_ambulatory_assessment -- the randomised format contrast.
#   westhoff2023_pbat, nas_rogoza_2024_study5_nas, soderberg_2024_esm_affect --
#     well-powered, many occasions, unremarkable floors: the baseline cases.
PILOT_TABLES <- c(
  "westhoff2023_pbat", "vollbracht_et_al_2026_ambulatory_assessment",
  "nas_rogoza_2024_study5_nas",
  "smoking_perseverance_mcneish_2025", "mhscdc_fried_2020_ema",
  "soderberg_2024_esm_affect"
)

# vollbracht randomised 406 participants to a Likert (1) or slider (2) version of
# the same ambulatory protocol. The assignment is BETWEEN persons -- every one of
# the 406 ids carries exactly one cov_group -- so it is a randomised format
# contrast, NOT the same people answering both formats.
FORMAT_ARM <- c(vollbracht_et_al_2026_ambulatory_assessment = "cov_group")
ARM_LABELS <- list(vollbracht_et_al_2026_ambulatory_assessment = c(`1` = "Likert", `2` = "Slider"))

# Where one table carries several instruments, pairs are kept within instrument:
# the item-name stem is the instrument.
INSTRUMENT_BY_STEM <- c(
  "vollbracht_et_al_2026_ambulatory_assessment",  # att, cla, ct, pum, wt, stress
  "debacker_2018_justice_appraisal"               # distr/proc x group/pers
)
instrument_of <- function(tab, item)
  if (tab %in% INSTRUMENT_BY_STEM) sub("[0-9]+$", "", item) else rep(tab, length(item))

# IRW does not harmonise the name of the time column, so each table exposes its
# own. Prefer an explicit within-person counter over `date`, which is sometimes a
# single survey timestamp rather than a series.
occasion_col <- function(df) {
  for (nm in c("wave", "trial_occasion", "occasion", "date")) if (nm %in% names(df)) return(nm)
  NA_character_
}

# Items with more than 10 observed categories count as slider/continuous -- the
# same cut esm_floor used. Format is an item property, not a table one:
# vollbracht's Likert arm is ordinal although the table is a slider study.
format_of <- function(n_cat) ifelse(n_cat > 10, "Slider / continuous", "Ordinal")

z_of <- function(r) atanh(pmax(pmin(r, 0.9999), -0.9999))

# Lag-1 autocorrelation of one person's series on one item, used to deflate that
# person's occasion count to an EFFECTIVE one. Clamped to [0, 0.9]: a negative
# estimate would legitimately raise the effective sample size, but allowing that
# would let noise inflate the baseline's precision and so inflate the reported
# excess, so this only ever deflates. 0.9 keeps a near-unit-root estimate from
# collapsing t_eff to nothing.
ar1_of <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4L) return(0)
  d <- x - mean(x)
  den <- sum(d^2)
  if (den <= 0) return(0)
  min(max(sum(d[-1] * d[-n]) / den, 0), 0.9)
}

# ---------------------------------------------------------------------------
# Per-person correlations for one comparable group of items
# ---------------------------------------------------------------------------
# w: one row per (id, occasion), one column per item.
# Returns one row per (person, pair) with the person's correlation, the number of
# occasions it rests on, and an EFFECTIVE occasion count that accounts for serial
# dependence in the two series. A person whose response never varies (a floored
# item) yields NA and is counted separately rather than dropped silently -- that
# exclusion is itself a floor effect.
#
# t_eff uses the standard Bartlett-type adjustment for the correlation between
# two autocorrelated series, t_eff = T (1 - rho_i rho_j) / (1 + rho_i rho_j).
# This matters because every reported quantity on the page is an excess over a
# baseline built from T: ESM occasions are not independent draws, so the true
# baseline is HIGHER than an independence calculation gives and the excess
# correspondingly smaller. Both versions are computed and carried through.
person_pair_cors <- function(w, items, min_occ) {
  M <- as.matrix(w[, items, drop = FALSE])
  storage.mode(M) <- "double"
  cmb <- t(utils::combn(length(items), 2))
  idx <- split(seq_len(nrow(M)), w$id)
  rows <- vector("list", length(idx))
  for (k in seq_along(idx)) {
    mi <- M[idx[[k]], , drop = FALSE]
    if (nrow(mi) < min_occ) next
    ok <- !is.na(mi)
    tp <- crossprod(ok)                       # occasions with BOTH items answered
    cm <- suppressWarnings(stats::cor(mi, use = "pairwise.complete.obs"))
    keep <- tp[cmb] >= min_occ
    if (!any(keep)) next
    rho <- apply(mi, 2L, ar1_of)
    ri <- rho[cmb[keep, 1]]; rj <- rho[cmb[keep, 2]]
    t_raw <- as.integer(tp[cmb][keep])
    rp <- ri * rj
    rows[[k]] <- tibble(
      id    = names(idx)[k],
      item_i = items[cmb[keep, 1]], item_j = items[cmb[keep, 2]],
      t_occ = t_raw,
      rho_i = ri, rho_j = rj,
      t_eff = t_raw * (1 - rp) / (1 + rp),
      r     = cm[cmb][keep]
    )
  }
  bind_rows(rows)
}

# ---------------------------------------------------------------------------
# One table
# ---------------------------------------------------------------------------
profile_table <- function(tab) {
  d <- irw_fetch(tab)
  if (!all(c("id", "item", "resp") %in% names(d))) stop("missing core columns")
  d <- d %>% filter(!is.na(resp))
  occ <- occasion_col(d)
  if (is.na(occ)) stop("no usable occasion column")

  arm_col <- unname(FORMAT_ARM[tab])
  d$.arm <- if (!is.na(arm_col)) unname(ARM_LABELS[[tab]][as.character(d[[arm_col]])]) else NA_character_
  d$.occ <- d[[occ]]
  d$id   <- as.character(d$id)

  # Duplicate (id, occasion, item) rows are collapsed by mean. They are rare here
  # but real in IRW, and pivot_wider would otherwise emit list columns.
  d <- d %>% group_by(id, .arm, .occ, item) %>%
    summarise(resp = mean(resp), .groups = "drop")

  # Occasions per person, COUNTED, not inferred from responses/items: the generic
  # screen divides by all items and so understates any branched instrument
  # (vollbracht reads ~37 against a true median of 64).
  occ_person <- d %>% group_by(.arm, id) %>% summarise(n_occ = n_distinct(.occ), .groups = "drop")

  # Occasion RANK within person drives the detrend, so an irregular or
  # date-valued index still gives a monotone predictor.
  d <- d %>% group_by(id) %>% mutate(.t = dense_rank(.occ)) %>% ungroup()

  per_item <- d %>%
    group_by(arm = .arm, item) %>%
    summarise(n = n(), lo = min(resp), hi = max(resp), n_cat = n_distinct(resp),
              mean_pooled = mean(resp),
              floor_pct = 100 * mean(resp == min(resp)), .groups = "drop") %>%
    mutate(instrument = instrument_of(tab, item),
           format = format_of(n_cat),
           scale  = ifelse(format == "Ordinal", sprintf("%g to %g", lo, hi),
                           sprintf("slider from %g to %g", lo, hi)))

  # Per-person-per-item linear detrend on the occasion rank. Computed as a
  # closed-form slope rather than lm() -- there are up to 220k rows per table and
  # one lm() per person-item is minutes of nothing.
  d <- d %>% group_by(id, item) %>%
    mutate(.resid = {
      n <- sum(!is.na(resp))
      if (n < 3 || stats::var(.t) == 0) resp - mean(resp)
      else {
        b <- stats::cov(resp, .t) / stats::var(.t)
        resp - (mean(resp) + b * (.t - mean(.t)))
      }
    }) %>% ungroup()

  # A comparable group = one arm x one instrument x one scale. Pairs are formed
  # only inside such a group: a correlation is scale-free, but the ESTIMATOR is
  # not -- a within-person correlation on a 5-point item with a floor is not the
  # same object as one on a 0-100 slider, which is exactly the format cut this
  # vignette is for.
  groups <- per_item %>% group_by(arm, instrument, scale, format) %>%
    filter(n() >= 2) %>% summarise(items = list(item), .groups = "drop")

  out <- pmap(groups, function(arm, instrument, scale, format, items) {
    dd <- if (is.na(arm)) d else d[!is.na(d$.arm) & d$.arm == arm, ]
    map_dfr(c("raw", "detrended"), function(spec) {
      val <- if (spec == "raw") "resp" else ".resid"
      w <- dd %>% filter(item %in% items) %>%
        select(id, .occ, item, value = all_of(val)) %>%
        pivot_wider(names_from = item, values_from = value) %>%
        arrange(id, .occ)
      it <- intersect(items, names(w))
      if (length(it) < 2) return(tibble())

      # Between-person: the group correlation, on person means. A person enters an
      # item's mean only with MIN_OCC occasions on that item, so the between and
      # within estimates rest on the same people.
      #
      # Person means come from `resp`, NEVER from the detrended values, in BOTH
      # specs. A per-person linear detrend subtracts each person's own fitted
      # line, which removes their mean exactly -- so a between-person correlation
      # on residual means is zero by construction, not a finding. Detrending is a
      # statement about within-person covariation and is applied only there. (The
      # first pilot run did this wrong and every detrended group correlation came
      # back near zero, which is how it was caught.)
      pm <- dd %>% filter(item %in% it) %>%
        group_by(id, item) %>% filter(n() >= MIN_OCC) %>%
        summarise(m = mean(resp), .groups = "drop") %>%
        pivot_wider(names_from = item, values_from = m)
      pmm <- as.matrix(pm[intersect(it, names(pm))])
      if (ncol(pmm) < 2) return(tibble())
      cb <- suppressWarnings(stats::cor(pmm, use = "pairwise.complete.obs"))
      nb <- crossprod(!is.na(pmm))

      pc_all <- person_pair_cors(w, it, MIN_OCC)
      if (!nrow(pc_all)) return(tibble())

      # Persons who cleared MIN_OCC on both items but whose responses never
      # varied: cor() is undefined. Counted, because that is a floor effect and
      # not a missing-data nuisance.
      novar <- pc_all %>% filter(is.na(r)) %>% count(item_i, item_j, name = "n_novar")
      pc <- pc_all %>% filter(!is.na(r))
      if (!nrow(pc)) return(tibble())

      res <- pc %>% group_by(item_i, item_j) %>%
        summarise(
          n_persons   = n(),
          occ_median  = median(t_occ),
          occ_min     = min(t_occ), occ_max = max(t_occ),
          r_within_median = median(r), r_within_q25 = quantile(r, .25),
          r_within_q75 = quantile(r, .75), r_within_min = min(r), r_within_max = max(r),
          z_mean      = mean(z_of(r)),
          z_sd_obs    = stats::sd(z_of(r)),
          # Spread EXPECTED if every person shared the group correlation, from
          # each person's own occasion count. se_i = 1/sqrt(T_i - 3) in z space.
          z_sd_exp    = sqrt(mean(1 / pmax(t_occ - 3, 1))),
          # The same baseline on effective occasions: the defensible version,
          # since ESM occasions are serially dependent.
          z_sd_exp_eff = sqrt(mean(1 / pmax(t_eff - 3, 1))),
          rho_median  = median(pmax(rho_i, rho_j)),
          occ_eff_median = median(t_eff),
          z_mean_wt   = sum(z_of(r) * pmax(t_occ - 3, 1)) / sum(pmax(t_occ - 3, 1)),
          .groups = "drop"
        ) %>%
        rowwise() %>%
        mutate(
          r_between = cb[item_i, item_j],
          n_between = nb[item_i, item_j]
        ) %>% ungroup() %>%
        filter(!is.na(r_between), n_persons >= MIN_PERSONS)
      if (!nrow(res)) return(tibble())

      # Orientation: flip the pair so the GROUP correlation is non-negative
      # before anything is pooled. Response direction is not harmonised in IRW
      # and item names do not recover keying; a flip moves the group and every
      # person together, so no reported quantity changes except its readability.
      res <- res %>% mutate(flipped = r_between < 0)
      pc2 <- pc %>% left_join(res %>% select(item_i, item_j, r_between, n_between, flipped),
                              by = c("item_i", "item_j")) %>%
        filter(!is.na(r_between)) %>%
        mutate(r = ifelse(flipped, -r, r), r_between = ifelse(flipped, -r_between, r_between))

      # The quantity that reads: how often does a person's correlation point the
      # other way from the group's -- and how often would it, on this many
      # occasions, if everyone were identical?
      flips <- pc2 %>% group_by(item_i, item_j) %>%
        summarise(
          frac_opposite = mean(sign(r) != sign(r_between)),
          frac_opposite_wt = sum((sign(r) != sign(r_between)) * pmax(t_occ - 3, 1)) /
                             sum(pmax(t_occ - 3, 1)),
          # P(a person's estimate lands the other side of zero | group value,
          # their own precision), averaged over persons.
          frac_opposite_exp = mean(stats::pnorm(-abs(z_of(first(r_between))) *
                                                 sqrt(pmax(t_occ - 3, 1)))),
          frac_opposite_exp_eff = mean(stats::pnorm(-abs(z_of(first(r_between))) *
                                                 sqrt(pmax(t_eff - 3, 1)))),
          # Fisher et al.'s coverage statistic: they report that only 68% of
          # individual correlations fall inside a 99.7% (+-3 SE) interval built
          # around the GROUP estimate. Reproduced here pair by pair -- and with
          # the share that would fall inside it under perfect homogeneity, which
          # is NOT 99.7%: the interval is scaled by the group's precision
          # (se = 1/sqrt(n_between - 3), hundreds of persons) while each person's
          # own estimate carries its own much wider se, so a homogeneous
          # population already misses the interval often. The excess is the
          # reportable quantity, exactly as for the sign flips.
          frac_in_ci = {
            se_g <- 1 / sqrt(pmax(first(n_between) - 3, 1))
            zg <- z_of(first(r_between))
            mean(abs(z_of(r) - zg) <= 3 * se_g)
          },
          frac_in_ci_exp = {
            se_g <- 1 / sqrt(pmax(first(n_between) - 3, 1))
            mean(2 * stats::pnorm(3 * se_g * sqrt(pmax(t_occ - 3, 1))) - 1)
          },
          # The group's own interval needs no deflation: it is built over
          # PERSONS, who are independent of each other, not over occasions.
          frac_in_ci_exp_eff = {
            se_g <- 1 / sqrt(pmax(first(n_between) - 3, 1))
            mean(2 * stats::pnorm(3 * se_g * sqrt(pmax(t_eff - 3, 1))) - 1)
          },
          .groups = "drop")

      res %>%
        mutate(r_within_median = ifelse(flipped, -r_within_median, r_within_median),
               z_mean = ifelse(flipped, -z_mean, z_mean),
               z_mean_wt = ifelse(flipped, -z_mean_wt, z_mean_wt),
               r_between = abs(r_between)) %>%
        left_join(flips, by = c("item_i", "item_j")) %>%
        left_join(novar, by = c("item_i", "item_j")) %>%
        mutate(
          n_novar = coalesce(n_novar, 0L),
          # Persons excluded because one item never moved for them, as a share of
          # those who had the occasions to be included: a floor effect, reported.
          frac_novar = n_novar / (n_persons + n_novar),
          # Heterogeneity is the EXCESS spread over what this many occasions
          # produce under homogeneity, never the raw spread.
          z_sd_excess = sqrt(pmax(z_sd_obs^2 - z_sd_exp^2, 0)),
          het_ratio   = z_sd_obs / z_sd_exp,
          z_sd_excess_eff = sqrt(pmax(z_sd_obs^2 - z_sd_exp_eff^2, 0)),
          het_ratio_eff   = z_sd_obs / z_sd_exp_eff,
          # The reportable excess over the noise baseline, for both quantities.
          frac_opposite_excess = frac_opposite - frac_opposite_exp,
          frac_opposite_excess_eff = frac_opposite - frac_opposite_exp_eff,
          table = tab, arm = arm, instrument = instrument,
          scale = scale, format = format, spec = spec, .before = 1)
    })
  }) %>% bind_rows()

  # Item-level dispersion: the direct Fisher analogue. Between-person SD of
  # person means against the distribution of within-person SDs.
  item_disp <- map_dfr(c("raw", "detrended"), function(spec) {
    val <- if (spec == "raw") "resp" else ".resid"
    # Same rule as the pairs: the person MEAN is always the raw mean (the detrend
    # zeroes it), the within-person SD is the spec's.
    ps <- d %>% group_by(arm = .arm, item, id) %>% filter(n() >= MIN_OCC) %>%
      summarise(m = mean(resp), s = stats::sd(.data[[val]]),
                t_occ = n(), .groups = "drop")
    ps %>% group_by(arm, item) %>%
      filter(n() >= MIN_PERSONS) %>%
      summarise(
        n_persons = n(),
        # Fisher et al. also report that group and individual MEANS agree (14 of
        # 15 variables, 3% median discrepancy) -- location generalises even where
        # dispersion does not, and leaving it out would show only the half of
        # their argument that cuts against generalisability. Two quantities,
        # because they say different things: the group mean against the average
        # person's mean (near-identical by construction, the literal analogue of
        # their test), and the distance from the group mean to a typical
        # INDIVIDUAL's own mean, which is the substantive version.
        mean_of_person_means = mean(m),
        mean_dev_median = median(abs(m - mean(m))),
        mean_dev_q90 = quantile(abs(m - mean(m)), .90),
        sd_between = stats::sd(m),
        sd_within_median = median(s, na.rm = TRUE),
        sd_within_q25 = quantile(s, .25, na.rm = TRUE),
        sd_within_q75 = quantile(s, .75, na.rm = TRUE),
        sd_within_q90 = quantile(s, .90, na.rm = TRUE),
        sd_within_max = max(s, na.rm = TRUE),
        n_flat = sum(s == 0, na.rm = TRUE),
        occ_median = median(t_occ),
        .groups = "drop") %>%
      mutate(
        ratio_median = sd_within_median / sd_between,
        # The 90th-percentile person, not the maximum. A person's own SD rests on
        # as few as MIN_OCC occasions, so the MAXIMUM over a few hundred persons
        # is an order statistic that inflates wherever series are short -- the
        # pool's largest value (20x) comes from its shortest table. The q90 is
        # the defensible version of "a highly variable individual"; the max is
        # kept beside it as context, not as the claim.
        ratio_q90    = sd_within_q90 / sd_between,
        ratio_max    = sd_within_max / sd_between,
        var_ratio_median = (sd_within_median / sd_between)^2,
        var_ratio_q90    = (sd_within_q90 / sd_between)^2,
        var_ratio_max    = (sd_within_max / sd_between)^2,
        table = tab, spec = spec)
  }) %>% left_join(per_item %>% select(arm, item, instrument, format, scale,
                                       n_cat, lo, hi, mean_pooled, floor_pct),
                   by = c("arm", "item")) %>%
    mutate(
      range = hi - lo,
      # Percentages of the item's observed range, so items on different scales
      # pool. `disc_group_pct` is Fisher's comparison; `dev_person_*_pct` is how
      # far the group mean sits from a person's own level.
      disc_group_pct      = 100 * abs(mean_pooled - mean_of_person_means) / range,
      dev_person_med_pct  = 100 * mean_dev_median / range,
      dev_person_q90_pct  = 100 * mean_dev_q90 / range)

  # Why a table can yield no pairs at all, recorded instead of crashing: a pool
  # member is not guaranteed to clear MIN_OCC / MIN_PERSONS, and six of the 26 do
  # not. An empty `out` has no columns, so n_pairs cannot be filtered out of it.
  n_elig <- occ_person %>% filter(n_occ >= MIN_OCC) %>% nrow()
  n_groups <- nrow(groups)
  n_pairs_raw <- if ("spec" %in% names(out)) sum(out$spec == "raw") else 0L
  note <- if (n_pairs_raw > 0) NA_character_
    else if (n_groups == 0) sprintf("no comparable item group with 2+ items on one scale (%d items)", n_distinct(per_item$item))
    else if (n_elig < MIN_PERSONS) sprintf("only %d persons reach %d occasions (need %d)", n_elig, MIN_OCC, MIN_PERSONS)
    else sprintf("%d comparable groups, %d persons at %d+ occasions, but no pair cleared both gates", n_groups, n_elig, MIN_OCC)

  list(
    summary = tibble(
      table = tab,
      pool_format = if (tab %in% ESM_SLIDER) "Slider / continuous" else "Ordinal",
      n_items = n_distinct(per_item$item),
      n_persons = n_distinct(d$id),
      n_responses = nrow(d),
      occasion_col = occ,
      occ_median = median(occ_person$n_occ),
      occ_q25 = quantile(occ_person$n_occ, .25),
      occ_q75 = quantile(occ_person$n_occ, .75),
      occ_min = min(occ_person$n_occ), occ_max = max(occ_person$n_occ),
      n_pairs = n_pairs_raw,
      n_groups = n_groups,
      n_persons_eligible = n_elig,
      n_arms = n_distinct(per_item$arm),
      note = note
    ),
    items = item_disp,
    pairs = out,
    occ_person = occ_person %>% mutate(table = tab, .before = 1)
  )
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
tables <- if (PILOT) PILOT_TABLES else ESM_POOL
message("=== ergodicity: ", length(tables), " of ", length(ESM_POOL),
        " ESM tables (", if (PILOT) "PILOT" else "FULL", ") ===")

run_one <- function(tab) {
  f <- file.path(FIT_DIR, paste0(tab, ".rds"))
  if (file.exists(f)) { message("  cached : ", tab); return(invisible(NULL)) }
  message("  compute: ", tab)
  out <- tryCatch(profile_table(tab),
                  error = function(e) { message("    FAILED: ", conditionMessage(e)); NULL })
  if (!is.null(out)) saveRDS(out, f)
  invisible(NULL)
}

if (WORKERS > 1L) {
  plan(multisession, workers = WORKERS)
  future_walk(tables, run_one, .options = furrr_options(seed = TRUE))
  plan(sequential)
} else {
  walk(tables, run_one)
}

res <- map(tables, function(tab) {
  f <- file.path(FIT_DIR, paste0(tab, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
})
names(res) <- tables
res <- compact(res)
message("Computed: ", length(res), " of ", length(tables), " tables")

table_summary <- bind_rows(map(res, "summary"))
item_summary  <- bind_rows(map(res, "items"))
pair_summary  <- bind_rows(map(res, "pairs"))
occ_person    <- bind_rows(map(res, "occ_person"))

# Floor mass as a covariate, from the esm_floor cache rather than recomputed --
# same pool, same definition, already committed in this repo.
floor_items <- tryCatch({
  fl <- readRDS(FLOOR_RDS)$item_detail
  fl %>% transmute(table, arm, item, floor_pct_esm = floor_pct)
}, error = function(e) { message("esm_floor cache unreadable: ", conditionMessage(e)); NULL })
if (!is.null(floor_items)) {
  pair_summary <- pair_summary %>%
    left_join(floor_items %>% rename(item_i = item, floor_i = floor_pct_esm),
              by = c("table", "arm", "item_i")) %>%
    left_join(floor_items %>% rename(item_j = item, floor_j = floor_pct_esm),
              by = c("table", "arm", "item_j")) %>%
    mutate(floor_max = pmax(floor_i, floor_j, na.rm = TRUE))
}

message(sprintf("  %d pairs (raw) across %d tables; median het_ratio %.2f, median frac_opposite %.3f",
                sum(pair_summary$spec == "raw"), n_distinct(pair_summary$table),
                median(pair_summary$het_ratio[pair_summary$spec == "raw"], na.rm = TRUE),
                median(pair_summary$frac_opposite[pair_summary$spec == "raw"], na.rm = TRUE)))

saveRDS(
  list(
    table_summary    = table_summary,
    item_summary     = item_summary,
    pair_summary     = pair_summary,
    occ_person       = occ_person,
    esm_slider       = ESM_SLIDER,
    esm_ordinal      = ESM_ORDINAL,
    candidate_tables = tables,
    n_all_candidates = length(ESM_POOL),
    min_occ          = MIN_OCC,
    min_persons      = MIN_PERSONS,
    pilot            = PILOT,
    date_run         = Sys.Date(),
    session          = sessionInfo()
  ),
  file = file.path(DATA_DIR, "ergodicity_results.rds")
)
message("Wrote ", file.path(DATA_DIR, "ergodicity_results.rds"))

bib_path <- file.path(DATA_DIR, "references.bib")
tryCatch(irw_save_bibtex(unique(table_summary$table), output_file = bib_path),
         error = function(e) message("bibtex generation failed: ", conditionMessage(e)))

# irw_save_bibtex() OVERWRITES the file, so method citations are appended after
# it on every run rather than maintained by hand in the .bib.
cat('
@article{fisher2018lack,
  title={Lack of group-to-individual generalizability is a threat to human subjects research},
  author={Fisher, Aaron J. and Medaglia, John D. and Jeronimus, Bertus F.},
  journal={Proceedings of the National Academy of Sciences},
  volume={115}, number={27}, pages={E6106--E6115}, year={2018},
  doi={10.1073/pnas.1711978115}
}

@article{adolf2019ergodicity,
  title={Ergodicity is sufficient but not necessary for group-to-individual generalizability},
  author={Adolf, Janne K. and Fried, Eiko I.},
  journal={Proceedings of the National Academy of Sciences},
  volume={116}, number={14}, pages={6540--6541}, year={2019},
  doi={10.1073/pnas.1818675116}
}

@article{mcneish2025primer,
  title={A Primer on Intensive Longitudinal Psychometrics},
  author={McNeish, Daniel},
  journal={Educational and Psychological Measurement}, year={2025},
  note={Dataset published in IRW as smoking_perseverance_mcneish_2025}
}

@article{vollbracht2026,
  title={Slider versus Likert scales: Psychometric properties in ambulatory assessment},
  author={Vollbracht, D. and Ottenstein, C. and Ecker, S.},
  journal={Behavior Research Methods},
  volume={58}, pages={97}, year={2026},
  doi={10.3758/s13428-026-02992-4}
}
', file = bib_path, append = TRUE)
