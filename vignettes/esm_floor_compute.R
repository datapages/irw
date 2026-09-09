# esm_floor_compute.R
#
# Compute stage for the "What is a floor, actually?" vignette (esm_floor.qmd).
#
# Question. Intensive longitudinal (ESM) responses pile up at the bottom of the
# scale. A recent preprint ("Beneath the Floor: Censored DSEM Models for
# Analyzing ESM Data with Floor Effects", PsyArXiv 2026-07-15) argues that
# ignoring that pile biases DSEM random-effect correlations and recommends a
# censored model. Reviewers of the scoping brief (E. Ulitzsch, L. Zhang) pointed
# out that censoring is only one theory of what a floor is -- an appropriate
# link (beta, or zero-one-inflated beta) models the boundedness directly. So the
# vignette treats this as a test between three readings of the same pile:
#
#   censoring  -- a latent continuum extends below the boundary, unobserved.
#   ZOIB       -- being AT the floor is a different event from being just above.
#   graded     -- the response was categorical all along; the floor is category 1.
#
# Two stages, deliberately separable:
#
#   Stage A (cheap, no Stan). Floor mass per ITEM vs floor mass of the COMPOSITE,
#     across the curated ESM pool. This is the descriptive result that motivates
#     everything else and it stands alone if Stage B is unfinished.
#
#   Stage B (Stan). Model comparison on vollbracht_et_al_2026, which randomised
#     406 participants to a Likert or a slider version of the same ambulatory
#     protocol. NOTE: the format is BETWEEN persons (verified: all 406 ids have
#     exactly one cov_group), not within, so this is a randomised format
#     contrast, not a within-person crossing.
#
# Outputs (all committed to git -- the cache is data, not a build artifact):
#   esm_floor_data/esm_floor_results.rds
#   esm_floor_data/fits/<label>.rds        (per-fit, so a crash resumes)
#   esm_floor_data/references.bib
#
# Usage:
#   Rscript vignettes/esm_floor_compute.R
#
# Requires REDIVIS_API_TOKEN (read scope is sufficient; nothing here writes).

suppressPackageStartupMessages({
  library(irw); library(dplyr); library(tidyr); library(purrr); library(tibble)
})

set.seed(20260908)

PILOT          <- TRUE   # TRUE: Stage B on the two headline items only
STAGE_B        <- TRUE   # FALSE: skip all Stan fitting, emit Stage A only
CHAINS         <- 2
ITER           <- 1500   # 750 warmup; a draft setting, raise for the final run
MAX_PERSONS    <- 250    # per arm; vollbracht has ~200/206 so this is not binding

DATA_DIR <- "vignettes/esm_floor_data"
FIT_DIR  <- file.path(DATA_DIR, "fits")
dir.create(FIT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# The curated ESM pool.
#
# These 24 tables were identified by occasions-per-person (responses per
# participant / item count), then CONFIRMED one at a time against each table's
# processing script. Neither metadata$longitudinal nor the intensive_longitudinal
# collection finds this set: the former is a grep over column names that matches
# cov_birthdate, and the latter requires date+wave together, which most real ESM
# tables do not carry. See esm_floor.qmd for why four plausible-looking
# candidates were rejected.
# ---------------------------------------------------------------------------
ESM_SLIDER <- c(
  "westhoff2023_pbat", "westhoff2023_stopd",
  "vollbracht_et_al_2026_ambulatory_assessment",
  "emoji_scheffler_2024", "tears",
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

# Occasion index: IRW does not harmonise the name of the time column, so each
# table exposes its own. Prefer an explicit within-person counter over `date`,
# which is sometimes a single survey timestamp rather than a series.
occasion_col <- function(df) {
  for (nm in c("wave", "trial_occasion", "occasion", "date")) if (nm %in% names(df)) return(nm)
  NA_character_
}

# ---------------------------------------------------------------------------
# Stage A -- floor mass per item vs per composite
# ---------------------------------------------------------------------------
# The field reports floor rates for a composite score. Because items in one
# instrument sit at very different points on the scale, averaging them can drive
# the composite's floor mass to ~0 while individual items remain heavily piled.
# That is the gap this measures. `resp` direction is NOT harmonised in IRW, so
# "floor" here means the observed minimum of that item, which for a reverse-keyed
# item is the substantive ceiling -- reported, not silently corrected.
floor_profile <- function(tab) {
  d <- irw_fetch(tab)
  if (!all(c("id", "item", "resp") %in% names(d))) stop("missing core columns")
  d <- d %>% filter(!is.na(resp))
  occ <- occasion_col(d)

  per_item <- d %>%
    group_by(item) %>%
    summarise(n = n(), lo = min(resp), hi = max(resp),
              n_cat = n_distinct(resp),
              floor_pct = 100 * mean(resp == min(resp)),
              ceil_pct  = 100 * mean(resp == max(resp)),
              .groups = "drop")

  # Composite = person-occasion mean across items, which is what an ESM paper
  # almost always models. Needs an occasion index to define "an occasion".
  comp_floor <- NA_real_
  if (!is.na(occ)) {
    comp <- d %>%
      group_by(id, .occ = .data[[occ]]) %>%
      summarise(m = mean(resp), k = n(), .groups = "drop") %>%
      filter(k >= 2)
    if (nrow(comp) > 0) comp_floor <- 100 * mean(comp$m == min(comp$m))
  }

  tibble(
    table          = tab,
    n_items        = nrow(per_item),
    n_persons      = n_distinct(d$id),
    n_responses    = nrow(d),
    occasion_col   = occ %||% NA_character_,
    floor_min      = min(per_item$floor_pct),
    floor_max      = max(per_item$floor_pct),
    floor_median   = median(per_item$floor_pct),
    composite_floor = comp_floor,
    items          = list(per_item)
  )
}
`%||%` <- function(a, b) if (is.null(a)) b else a

message("=== Stage A: floor profiles across the ESM pool ===")
stage_a <- map(ESM_POOL, function(tab) {
  f <- file.path(FIT_DIR, paste0("floor_", tab, ".rds"))
  if (file.exists(f)) { message("  cached : ", tab); return(readRDS(f)) }
  message("  fetch  : ", tab)
  out <- tryCatch(floor_profile(tab),
                  error = function(e) { message("    FAILED: ", conditionMessage(e)); NULL })
  if (!is.null(out)) saveRDS(out, f)
  out
})
names(stage_a) <- ESM_POOL
stage_a <- compact(stage_a)

floor_summary <- bind_rows(lapply(stage_a, function(x) select(x, -items))) %>%
  mutate(format = ifelse(table %in% ESM_SLIDER, "Slider / continuous", "Ordinal"))
item_detail <- bind_rows(lapply(names(stage_a), function(tab)
  stage_a[[tab]]$items[[1]] %>% mutate(table = tab, .before = 1)))

message(sprintf("Stage A done: %d of %d tables profiled", nrow(floor_summary), length(ESM_POOL)))

# ---------------------------------------------------------------------------
# Stage B -- four theories of the floor, on the randomised-format table
# ---------------------------------------------------------------------------
# vollbracht_et_al_2026 randomised participants to a response FORMAT
# (cov_group 1 = Likert, 2 = slider) and ran the same ambulatory protocol in
# both arms. Two items carry the argument:
#
#   att1   -- 1-5 in the Likert arm, 0-100 in the slider arm. Its format
#             actually changed, and its floor mass falls 20.9% -> 7.3%.
#   stress -- 1-5 in BOTH arms; it was never sliderised. It therefore acts as an
#             internal control: whatever changes for att1 but not for stress is
#             attributable to format rather than to the arm's participants.
#
# Every model uses the same mean structure -- person random intercept, AR(1)
# within person -- and a person-level dispersion random effect correlated with
# the intercept, because that correlation (individual mean level vs innovation
# variance) is the parameter the preprint reports as biased.
#
# IMPORTANT and reported as a limitation, not hidden: "dispersion" is not the
# same object across families. Gaussian has sigma; the beta families have a
# precision phi; cumulative has a discrimination disc. Their person-level
# correlations with the intercept are analogous in role but not numerically
# identical quantities. The AR(1) coefficient IS comparable across all four, so
# it is the primary outcome here and the correlation is secondary.
# att1  -- format changed (1-5 -> 0-100), highest floor mass, but thin: the att
#          items were administered far less often than the mood block.
# stress -- never sliderised, so it is the internal control; well powered.
# wt1    -- format changed (-2..2 -> -50..50) AND well powered, so it carries the
#          format contrast that att1 is too thin to support.
PILOT_ITEMS <- c("att1", "stress", "wt1")

fit_one <- function(dat, family_label) {
  suppressPackageStartupMessages(library(brms))
  disp <- switch(family_label,
    gaussian = "sigma", censored = "sigma", zoib = "phi", cumulative = "disc")

  form <- switch(family_label,
    gaussian  = brms::bf(resp ~ 1 + ar(time = occ, gr = id) + (1 | p | id),
                         sigma ~ 1 + (1 | p | id)),
    censored  = brms::bf(resp | cens(cens_ind) ~ 1 + ar(time = occ, gr = id) + (1 | p | id),
                         sigma ~ 1 + (1 | p | id)),
    zoib      = brms::bf(resp01 ~ 1 + ar(time = occ, gr = id) + (1 | p | id),
                         phi ~ 1 + (1 | p | id)),
    cumulative = brms::bf(resp_ord ~ 1 + ar(time = occ, gr = id) + (1 | p | id),
                          disc ~ 1 + (1 | p | id))
  )
  fam <- switch(family_label,
    gaussian = brms::brmsfamily("gaussian"),
    censored = brms::brmsfamily("gaussian"),
    zoib     = brms::brmsfamily("zero_one_inflated_beta"),
    cumulative = brms::brmsfamily("cumulative"))

  brms::brm(form, data = dat, family = fam,
            chains = CHAINS, iter = ITER, refresh = 0, backend = "rstan",
            silent = 2, control = list(adapt_delta = 0.95))
}

# Which families are even admissible depends on the format -- itself part of the
# finding. You cannot fit ZOIB to a 5-category item, or a graded model to a
# 101-point slider, so "apply the same fix to both formats" is not available
# even in principle.
# NOTE: keyed off the observed response support, NOT the arm label. `stress` was
# never sliderised, so it is a 5-category item in BOTH arms -- asking for a
# zero-one-inflated beta there would be fitting a continuous density to five
# points. That admissibility follows the data rather than the condition is the
# point of the section, so the code has to honour it.
families_for <- function(n_cat) {
  if (n_cat <= 10) c("gaussian", "censored", "cumulative")
  else             c("gaussian", "censored", "zoib")
}

extract_pars <- function(fit, family_label) {
  vc <- tryCatch(brms::VarCorr(fit)$id$cor, error = function(e) NULL)
  cor_mean_disp <- if (!is.null(vc) && dim(vc)[1] >= 2) vc[1, "Estimate", 2] else NA_real_
  fx <- brms::fixef(fit)
  ar <- tryCatch(as.data.frame(brms::as_draws_df(fit))[["ar[1]"]], error = function(e) NULL)
  tibble(
    phi_ar1      = if (!is.null(ar)) mean(ar) else NA_real_,
    phi_ar1_lo   = if (!is.null(ar)) quantile(ar, .025) else NA_real_,
    phi_ar1_hi   = if (!is.null(ar)) quantile(ar, .975) else NA_real_,
    cor_mean_disp = cor_mean_disp,
    loo_elpd     = tryCatch(brms::loo(fit)$estimates["elpd_loo", "Estimate"],
                            error = function(e) NA_real_)
  )
}

stage_b <- NULL
if (STAGE_B) {
  message("=== Stage B: model comparison on vollbracht_et_al_2026 ===")
  # Raw fetch is cached locally but deliberately NOT committed (1.2 MB, and
  # re-derivable); re-fetch when it is absent, e.g. on a fresh clone.
  raw_path <- file.path(DATA_DIR, "vollbracht_raw.rds")
  v <- if (file.exists(raw_path)) readRDS(raw_path) else {
    x <- irw_fetch("vollbracht_et_al_2026_ambulatory_assessment"); saveRDS(x, raw_path); x
  }

  runs <- tidyr::expand_grid(item = PILOT_ITEMS, arm = c("Likert", "Slider")) %>%
    mutate(group = ifelse(arm == "Likert", 1, 2))

  stage_b <- pmap_dfr(runs, function(item, arm, group) {
    dat <- v %>%
      filter(.data$item == !!item, cov_group == group, !is.na(resp)) %>%
      transmute(id = as.factor(id), occ = as.integer(trial_occasion), resp = as.numeric(resp))
    keep <- dat %>% count(id) %>% filter(n >= 10) %>% slice_head(n = MAX_PERSONS) %>% pull(id)
    dat <- dat %>% filter(id %in% keep) %>% arrange(id, occ)
    if (nrow(dat) < 200) return(tibble())

    lo <- min(dat$resp); hi <- max(dat$resp)
    dat <- dat %>% mutate(
      cens_ind = ifelse(resp == lo, "left", "none"),          # floor = left-censored
      resp01   = (resp - lo) / (hi - lo),                     # ZOIB needs [0,1]
      resp_ord = factor(resp, levels = sort(unique(resp)), ordered = TRUE)
    )

    map_dfr(families_for(n_distinct(dat$resp)), function(fam) {
      label <- sprintf("%s_%s_%s", item, tolower(arm), fam)
      f <- file.path(FIT_DIR, paste0(label, ".rds"))
      if (file.exists(f)) { message("  cached : ", label); return(readRDS(f)) }
      message("  fitting: ", label, "  (n=", nrow(dat), ", persons=", n_distinct(dat$id), ")")
      t0 <- Sys.time()
      out <- tryCatch({
        fit <- fit_one(dat, fam)
        extract_pars(fit, fam) %>%
          mutate(item = item, arm = arm, family = fam,
                 n_obs = nrow(dat), n_persons = n_distinct(dat$id),
                 floor_pct = 100 * mean(dat$resp == lo),
                 scale_lo = lo, scale_hi = hi, n_cat = n_distinct(dat$resp),
                 mins = as.numeric(difftime(Sys.time(), t0, units = "mins")),
                 .before = 1)
      }, error = function(e) {
        message("    FAILED: ", conditionMessage(e))
        tibble(item = item, arm = arm, family = fam, error = conditionMessage(e))
      })
      saveRDS(out, f)
      out
    })
  })
  message("Stage B done: ", nrow(stage_b), " fits")
}

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
bib_path <- file.path(DATA_DIR, "references.bib")
tryCatch(
  irw_save_bibtex(intersect(ESM_POOL, floor_summary$table), output_file = bib_path),
  error = function(e) message("bibtex generation failed: ", conditionMessage(e)))

# irw_save_bibtex() OVERWRITES the file, so the method citations have to be
# appended after it on every run, not maintained by hand in the .bib.
#
# molenaar2022zero is copied verbatim from continuous_bounded_data/references.bib
# -- the same paper that vignette cites as the missing zero-one-inflated model.
#
# The preprint's AUTHOR FIELD IS DELIBERATELY EMPTY. The OSF record did not
# expose a contributor list and the names were not recoverable from search;
# inventing them would be worse than an incomplete entry. Fill this in before
# the page is published.
cat('
@article{molenaar2022zero,
  title={Zero and One Inflated Item Response Theory Models for Bounded Continuous Data},
  author={Molenaar, Dylan and C{\'u}ri, Mariana and Baz{\'a}n, Jorge L.},
  journal={Journal of Educational and Behavioral Statistics},
  volume={47}, number={6}, pages={693--735}, year={2022},
  doi={10.3102/10769986221108455}
}

@misc{beneaththefloor2026,
  title={Beneath the Floor: Censored {DSEM} Models for Analyzing {ESM} Data with Floor Effects},
  year={2026},
  howpublished={PsyArXiv preprint},
  url={https://osf.io/preprints/psyarxiv/3adgx},
  note={Author list not yet filled in -- see esm_floor_compute.R}
}

@article{vollbracht2026,
  title={Slider versus Likert scales: Psychometric properties in ambulatory assessment},
  author={Vollbracht, D. and Ottenstein, C. and Ecker, S.},
  journal={Behavior Research Methods},
  volume={58}, pages={97}, year={2026},
  doi={10.3758/s13428-026-02992-4}
}
', file = bib_path, append = TRUE)

saveRDS(list(
  floor_summary    = floor_summary,
  item_detail      = item_detail,
  stage_b          = stage_b,
  candidate_tables = ESM_POOL,
  n_all_candidates = length(ESM_POOL),
  esm_slider       = ESM_SLIDER,
  esm_ordinal      = ESM_ORDINAL,
  pilot            = PILOT,
  date_run         = Sys.Date(),
  session          = utils::capture.output(utils::sessionInfo())
), file.path(DATA_DIR, "esm_floor_results.rds"))

message("Wrote ", file.path(DATA_DIR, "esm_floor_results.rds"))
