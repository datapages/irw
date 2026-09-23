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
  library(R.utils); library(irw); library(dplyr); library(tidyr); library(purrr); library(tibble)
})

set.seed(20260908)

PILOT          <- TRUE   # TRUE: Stage B on the two headline items only
STAGE_B        <- TRUE   # FALSE: skip all Stan fitting, emit Stage A only
CHAINS         <- 2
ITER           <- 2000   # 1000 warmup
ADAPT_DELTA    <- 0.99
# max_treedepth 12 allows up to 2^12 = 4096 leapfrog steps per iteration. On
# stress/Slider -- a 5-point item with 37% of responses at the floor, the most
# extreme geometry in the set -- that turned a fit whose Likert twin took 6
# minutes into one still running after 8 hours. 10 caps the cost at 1024 steps;
# saturation then surfaces as a treedepth warning rather than an overnight
# stall, which is the failure mode you want.
MAX_TREEDEPTH  <- 10
FIT_BUDGET_MIN <- 45     # abandon a fit past this and record it as timed out
# Cells are capped to a COMMON size rather than each arm's natural size. The
# mood-block items carry ~11k rows against att1's ~1.8k, so uncapped the fits
# differ in n by 6x and take wildly different wall-clock. Equal cells also mean
# a difference between items cannot be an artefact of differing precision.
MAX_PERSONS    <- 90
MAX_OCCASIONS  <- 45     # earliest N occasions per person

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
  # REMOVED after inspection: emoji_scheffler_2024 (`id = name`, the emoji;
  # `rater = submission_id`, the person) and tears (`id <- x$Video_ID`,
  # `rater <- x$ID`). Both are stimulus-rating designs where the person is the
  # rater, not the id -- the same error that excluded moralvignettes. They
  # reached this list from the occasions-per-person screen, not from the
  # script-confirmed set, which is exactly the failure mode curation prevents.
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
# When items are averaged into a composite (a common input to DSEM), items that
# sit at different points on the scale can wash out one another's floors. This
# measures how much, but only for composites someone would actually form
# (datapages/irw#226). The first version averaged EVERY item in a table at each
# occasion -- in vollbracht that mixed 1-5, -50..50 and 0-100 items, so its
# ~7,000x "hiding" ratio was an artefact of the averaging, not a finding.
#
# A composite here is formed within:
#   - one ARM, where a table randomised response format (FORMAT_ARM);
#   - one INSTRUMENT: the table, or the item-name stem where a table carries
#     several instruments (INSTRUMENT_BY_STEM);
#   - one SCALE: items sharing their floor value and format (ordinal items must
#     also share their top category), so every item's floor is the same number.
# `resp` direction is NOT harmonised in IRW, and keying cannot be recovered
# reliably from item names. So within each such group, items whose item-rest
# correlation is negative are dropped one at a time (most negative first) until
# none remains -- excluded, not reverse-keyed, and reported per composite.
#
# Per-item floor is likewise computed within arm: pooled across vollbracht's arms,
# att1's minimum is the slider's 0, none of the Likert arm's 1-5 responses counts
# as floor, and 20.9% (Likert) and 7.3% (slider) read as 3.9%.
FORMAT_ARM  <- c(vollbracht_et_al_2026_ambulatory_assessment = "cov_group")
ARM_LABELS  <- list(vollbracht_et_al_2026_ambulatory_assessment = c(`1` = "Likert", `2` = "Slider"))
# Checked against every table in the pool: vollbracht is the only one where an
# item's response format differs by a grouping column. (opentsstvr's `treat`
# changes where observed VAS responses stop, not the scale.)
INSTRUMENT_BY_STEM <- c(
  "vollbracht_et_al_2026_ambulatory_assessment",  # att, cla, ct, pum, wt, stress
  "debacker_2018_justice_appraisal"               # distr/proc x group/pers
)
# Tables with no composite anyone would model, so none is formed:
NO_COMPOSITE <- c(
  # 10 mood adjectives plus originality/usefulness ratings of a creative task --
  # two instruments, not separable by item name.
  "zhang_2020_trait_creativity_mood",
  # a profile of 11 distinct exercise motives; the inventory is not summed.
  "strohacker_2024_bmzi_motive"
)
instrument_of <- function(tab, item)
  if (tab %in% INSTRUMENT_BY_STEM) sub("[0-9]+$", "", item) else rep(tab, length(item))

# Items with more than 10 observed categories are treated as slider/continuous,
# the same cut families_for() uses. Format is an item property, not a table one:
# vollbracht's Likert arm is ordinal although the table is a slider study.
format_of <- function(n_cat) ifelse(n_cat > 10, "Slider / continuous", "Ordinal")

composite_one <- function(d, items, lo) {
  w <- d %>% filter(item %in% items) %>%
    group_by(id, .occ, item) %>% summarise(resp = mean(resp), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = item, values_from = resp)
  keep <- items; dropped <- character()
  repeat {
    if (length(keep) < 2) break
    m <- as.matrix(w[keep])
    r <- vapply(keep, function(i) {
      rest <- rowMeans(m[, setdiff(keep, i), drop = FALSE], na.rm = TRUE)
      ok <- !is.na(m[, i]) & is.finite(rest)
      if (sum(ok) < 10) NA_real_ else suppressWarnings(cor(m[ok, i], rest[ok]))
    }, numeric(1))
    if (all(is.na(r) | r >= 0)) break
    worst <- keep[which.min(r)]
    keep <- setdiff(keep, worst); dropped <- c(dropped, worst)
  }
  if (length(keep) < 2) return(NULL)
  m <- as.matrix(w[keep])
  k <- rowSums(!is.na(m))
  m <- m[k >= 2, , drop = FALSE]
  if (nrow(m) == 0) return(NULL)
  # At the composite's floor = every item answered at that occasion sits at the
  # common floor value.
  at_floor <- apply(m, 1, function(x) all(x[!is.na(x)] == lo))
  tibble(items_kept = list(keep), items_dropped = list(dropped),
         n_occasions = nrow(m), composite_floor = 100 * mean(at_floor))
}

floor_profile <- function(tab) {
  d <- irw_fetch(tab)
  if (!all(c("id", "item", "resp") %in% names(d))) stop("missing core columns")
  d <- d %>% filter(!is.na(resp))
  occ <- occasion_col(d)
  arm_col <- unname(FORMAT_ARM[tab])
  d$.arm <- if (!is.na(arm_col)) unname(ARM_LABELS[[tab]][as.character(d[[arm_col]])]) else NA_character_

  per_item <- d %>%
    group_by(arm = .arm, item) %>%
    summarise(n = n(), lo = min(resp), hi = max(resp),
              n_cat = n_distinct(resp),
              floor_pct = 100 * mean(resp == min(resp)),
              ceil_pct  = 100 * mean(resp == max(resp)),
              .groups = "drop") %>%
    mutate(instrument = instrument_of(tab, item),
           format = format_of(n_cat),
           scale  = ifelse(format == "Ordinal", sprintf("%g to %g", lo, hi),
                           sprintf("slider from %g", lo)))

  composites <- tibble()
  if (!is.na(occ) && !tab %in% NO_COMPOSITE) {
    d$.occ <- d[[occ]]
    groups <- per_item %>% group_by(arm, instrument, scale, format) %>%
      filter(n() >= 2) %>% summarise(items = list(item), lo = first(lo), .groups = "drop")
    composites <- purrr::pmap_dfr(groups, function(arm, instrument, scale, format, items, lo) {
      dd <- if (is.na(arm)) d else d[!is.na(d$.arm) & d$.arm == arm, ]
      out <- composite_one(dd, items, lo)
      if (is.null(out)) return(tibble())
      fl <- per_item$floor_pct[per_item$item %in% out$items_kept[[1]] &
                               (is.na(arm) | per_item$arm %in% arm)]
      out %>% mutate(table = tab, arm = arm, instrument = instrument, scale = scale,
                     format = format, n_items = length(items_kept[[1]]),
                     n_dropped = length(items_dropped[[1]]),
                     floor_max = max(fl), floor_median = median(fl), floor_min = min(fl),
                     .before = 1)
    })
  }

  list(
    summary = tibble(
      table        = tab,
      n_items      = n_distinct(per_item$item),
      n_persons    = n_distinct(d$id),
      n_responses  = nrow(d),
      occasion_col = occ %||% NA_character_,
      floor_min    = min(per_item$floor_pct),
      floor_max    = max(per_item$floor_pct),
      floor_median = median(per_item$floor_pct)
    ),
    items      = per_item,
    composites = composites
  )
}
`%||%` <- function(a, b) if (is.null(a)) b else a

message("=== Stage A: floor profiles across the ESM pool ===")
stage_a <- map(ESM_POOL, function(tab) {
  # profile_ (not the old floor_) caches: the structure changed with #226, and a
  # stale floor_ cache would silently resurrect the pooled-arm, all-item version.
  f <- file.path(FIT_DIR, paste0("profile_", tab, ".rds"))
  if (file.exists(f)) { message("  cached : ", tab); return(readRDS(f)) }
  message("  fetch  : ", tab)
  out <- tryCatch(floor_profile(tab),
                  error = function(e) { message("    FAILED: ", conditionMessage(e)); NULL })
  if (!is.null(out)) saveRDS(out, f)
  out
})
names(stage_a) <- ESM_POOL
stage_a <- compact(stage_a)

floor_summary <- bind_rows(lapply(stage_a, `[[`, "summary")) %>%
  mutate(format = ifelse(table %in% ESM_SLIDER, "Slider / continuous", "Ordinal"))
item_detail <- bind_rows(lapply(names(stage_a), function(tab)
  stage_a[[tab]]$items %>% mutate(table = tab, .before = 1)))
composite_summary <- bind_rows(lapply(stage_a, `[[`, "composites"))
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
# Every model uses the same mean structure -- person random intercept, plus a
# person-mean-centred lagged response carrying within-person carryover -- and a person-level dispersion random effect correlated with
# the intercept, because that correlation (individual mean level vs innovation
# variance) is the parameter the preprint reports as biased.
#
# IMPORTANT and reported as a limitation, not hidden: "dispersion" is not the
# same object across families. Gaussian has sigma; the beta families have a
# precision phi; cumulative has a discrimination disc. phi and disc run INVERSE
# to variance, so their correlations are sign-flipped onto a common scale before
# any comparison (common_sign(), below); even then they are analogous in role,
# not numerically identical quantities. The AR(1) coefficient IS comparable across all four, so
# it is the primary outcome here and the correlation is secondary.
# att1  -- format changed (1-5 -> 0-100), highest floor mass, but thin: the att
#          items were administered far less often than the mood block.
# stress -- never sliderised, so it is the internal control; well powered.
# wt1    -- format changed (-2..2 -> -50..50) AND well powered, so it carries the
#          format contrast that att1 is too thin to support.
# wt1 dropped from the draft run: the doubled iterations needed for convergence
# have to be paid for in wall-clock somewhere, and att1 (format changed) plus
# stress (format unchanged, the control) is the minimum pair that carries the
# argument. Add wt1 back for the full run.
PILOT_ITEMS <- c("att1", "stress")

fit_one <- function(dat, family_label) {
  suppressPackageStartupMessages(library(brms))
  disp <- switch(family_label,
    gaussian = "sigma", censored = "sigma", zoib = "phi", cumulative = "disc")

  form <- switch(family_label,
    gaussian  = brms::bf(resp_z ~ 1 + lag_c + (1 | p | id),
                         sigma ~ 1 + (1 | p | id)),
    censored  = brms::bf(resp_z | cens(cens_ind) ~ 1 + lag_c + (1 | p | id),
                         sigma ~ 1 + (1 | p | id)),
    zoib      = brms::bf(resp01 ~ 1 + lag_c + (1 | p | id),
                         phi ~ 1 + (1 | p | id)),
    cumulative = brms::bf(resp_ord ~ 1 + lag_c + (1 | p | id),
                          disc ~ 1 + (1 | p | id))
  )
  fam <- switch(family_label,
    gaussian = brms::brmsfamily("gaussian"),
    censored = brms::brmsfamily("gaussian"),
    zoib     = brms::brmsfamily("zero_one_inflated_beta"),
    cumulative = brms::brmsfamily("cumulative"))

  # Weakly informative priors. Without them the first draft run produced Rhat
  # 1.8, bulk ESS 3.1 and 416 divergences: the location-scale random effects
  # (a person's mean correlated with their log residual SD) are only weakly
  # identified at ESM sample sizes, and flat priors leave the sampler nowhere
  # to stand. lkj(2) in particular keeps the correlation off +/-1.
  # No class="b" prior: the formula carries only an Intercept and no other
  # population-level fixed effect, so class "b" matches no parameter and brms
  # rejects the whole model. (It did, on all 12 fits, instantly.)
  prs <- c(
    brms::prior(normal(0, 1),    class = "b"),
    brms::prior(exponential(2),  class = "sd"),
    brms::prior(lkj(2),          class = "cor")
  )
  brms::brm(form, data = dat, family = fam, prior = prs,
            chains = CHAINS, iter = ITER, refresh = 0, backend = "rstan",
            silent = 2, control = list(adapt_delta = ADAPT_DELTA, max_treedepth = MAX_TREEDEPTH))
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

# Convergence diagnostics are recorded per fit, not assumed. Without them a
# stuck sampler is indistinguishable from a precise estimate: the first draft run
# returned ar = 0.092 [0.084, 0.099], an interval ~6x narrower than 1/sqrt(n)
# allows, which is what prompted adding this. Any fit whose ar Rhat exceeds 1.01
# or whose bulk ESS falls under 400 is reported as unreliable on the page rather
# than silently plotted.
extract_pars <- function(fit, family_label) {
  vc <- tryCatch(brms::VarCorr(fit)$id$cor, error = function(e) NULL)
  cor_mean_disp <- if (!is.null(vc) && dim(vc)[1] >= 2) vc[1, "Estimate", 2] else NA_real_
  ar <- tryCatch(as.data.frame(brms::as_draws_df(fit))[["b_lag_c"]], error = function(e) NULL)

  smry <- tryCatch(as.data.frame(posterior::summarise_draws(
            posterior::subset_draws(brms::as_draws(fit), variable = "b_lag_c"))),
          error = function(e) NULL)
  rhat_ar <- if (!is.null(smry) && nrow(smry)) smry$rhat[1]      else NA_real_
  ess_ar  <- if (!is.null(smry) && nrow(smry)) smry$ess_bulk[1]  else NA_real_
  ndiv <- tryCatch(sum(brms::nuts_params(fit, pars = "divergent__")$Value),
                   error = function(e) NA_real_)

  tibble(
    rhat_ar = rhat_ar, ess_ar = ess_ar, n_divergent = ndiv,
    converged = !is.na(rhat_ar) && rhat_ar <= 1.01 && !is.na(ess_ar) && ess_ar >= 400,
    phi_ar1      = if (!is.null(ar)) mean(ar) else NA_real_,
    phi_ar1_lo   = if (!is.null(ar)) quantile(ar, .025) else NA_real_,
    phi_ar1_hi   = if (!is.null(ar)) quantile(ar, .975) else NA_real_,
    cor_mean_disp = cor_mean_disp,
    loo_elpd     = tryCatch(brms::loo(fit)$estimates["elpd_loo", "Estimate"],
                            error = function(e) NA_real_)
  )
}

# The person-level dispersion parameter does not point the same way in every
# family (datapages/irw#226): gaussian and censored `sigma` grows with variance,
# but cumulative `disc` is the inverse of the latent SD and ZOIB `phi` is a
# precision. So cor(mean, disp) from those two carries the OPPOSITE sign
# convention, and comparing raw signs manufactured a flip in att1/Likert that is
# not there. Keep the raw estimate and add `cor_mean_var`, the same correlation
# on a common "higher = more variable" scale. For the ZOIB the beta variance is
# mu(1 - mu) / (1 + phi), so the conversion is right in direction, not magnitude.
DISP_PARAM   <- c(gaussian = "sigma", censored = "sigma", cumulative = "disc", zoib = "phi")
DISP_INVERSE <- c(sigma = FALSE, disc = TRUE, phi = TRUE)
common_sign <- function(sb) {
  if (is.null(sb) || !nrow(sb)) return(sb)
  sb %>% mutate(disp_param   = unname(DISP_PARAM[family]),
                cor_mean_var = ifelse(DISP_INVERSE[disp_param], -1, 1) * cor_mean_disp)
}

save_results <- function(stage_b_now) saveRDS(list(
  floor_summary    = floor_summary,
  item_detail      = item_detail,
  composite_summary = composite_summary,
  stage_b          = stage_b_now,
  candidate_tables = ESM_POOL,
  n_all_candidates = length(ESM_POOL),
  esm_slider       = ESM_SLIDER,
  esm_ordinal      = ESM_ORDINAL,
  pilot            = PILOT,
  date_run         = Sys.Date(),
  session          = utils::capture.output(utils::sessionInfo())
), file.path(DATA_DIR, "esm_floor_results.rds"))

stage_b <- NULL

# Write a Stage-A-only cache BEFORE Stage B starts. Stage B can run for hours and
# may be interrupted; without this an incomplete run leaves no cache at all and
# the page renders as "cache not found" despite Stage A being finished.
save_results(NULL)
message("Wrote Stage-A-only cache; the page is renderable from here on.")

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
    dat <- dat %>% filter(id %in% keep) %>% arrange(id, occ) %>%
      group_by(id) %>% slice_head(n = MAX_OCCASIONS) %>% ungroup()
    if (nrow(dat) < 200) return(tibble())

    lo <- min(dat$resp); hi <- max(dat$resp)
    dat <- dat %>% mutate(
      cens_ind = ifelse(resp == lo, "left", "none"),          # floor = left-censored
      resp01   = (resp - lo) / (hi - lo),                     # ZOIB needs [0,1]
      resp_ord = factor(resp, levels = sort(unique(resp)), ordered = TRUE),
      resp_z   = as.numeric(scale(resp))
    ) %>%
      # Carryover is modelled as DSEM actually formulates it -- a regression on
      # the previous occasion, y_it = mu_i + phi*(y_i,t-1 - mu_i) + e_it -- not
      # as a residual ARMA covariance. brms's ar() builds a per-group correlation
      # matrix, and combined with a person-varying sigma (which the preprint's
      # target parameter requires) it was both badly identified and ruinously
      # slow: Rhat 1.41, bulk ESS 5, and 147 minutes for a single fit.
      #
      # The lagged predictor is cheaper, better identified, closer to DSEM, and
      # gives a phi that is an ordinary regression coefficient -- so it is
      # genuinely comparable across all four response families, which an ar[]
      # parameter was not.
      group_by(id) %>%
      arrange(occ, .by_group = TRUE) %>%
      mutate(lag_c = dplyr::lag(resp_z) - mean(resp_z, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(!is.na(lag_c))   # drops each person's first occasion

    map_dfr(families_for(n_distinct(dat$resp)), function(fam) {
      label <- sprintf("%s_%s_%s", item, tolower(arm), fam)
      f <- file.path(FIT_DIR, paste0(label, ".rds"))
      if (file.exists(f)) { message("  cached : ", label); return(readRDS(f)) }
      message("  fitting: ", label, "  (n=", nrow(dat), ", persons=", n_distinct(dat$id), ")")
      t0 <- Sys.time()
      out <- tryCatch({
        fit <- R.utils::withTimeout(fit_one(dat, fam),
                                    timeout = FIT_BUDGET_MIN * 60,
                                    onTimeout = "error")
        extract_pars(fit, fam) %>%
          mutate(item = item, arm = arm, family = fam,
                 n_obs = nrow(dat), n_persons = n_distinct(dat$id),
                 floor_pct = 100 * mean(dat$resp == lo),
                 scale_lo = lo, scale_hi = hi, n_cat = n_distinct(dat$resp),
                 mins = as.numeric(difftime(Sys.time(), t0, units = "mins")),
                 .before = 1)
      }, error = function(e) {
        msg <- conditionMessage(e)
        timed_out <- grepl("reached elapsed time limit|timed out", msg, ignore.case = TRUE)
        message("    ", if (timed_out) "TIMED OUT" else "FAILED", ": ", msg)
        tibble(item = item, arm = arm, family = fam,
               n_cat = n_distinct(dat$resp),
               floor_pct = 100 * mean(dat$resp == lo),
               mins = FIT_BUDGET_MIN,
               error = if (timed_out) "exceeded the wall-clock budget" else msg)
      })
      saveRDS(out, f)
      out
    })
  })
  stage_b <- common_sign(stage_b)
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
save_results(stage_b)
message("Wrote ", file.path(DATA_DIR, "esm_floor_results.rds"))
