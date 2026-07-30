# Continuous-response vignette: scouting handoff

Produced by `vignettes/continuous_scout_fetch.R` + `vignettes/continuous_scout_classify.R`
(scouting only, non-authoritative, no model fitting done). Full data:
`continuous_scout_candidates.csv` (repo root), raw per-item cache in
`continuous_scout_data/continuous_scout_raw.rds`.

55 IRW tables have `n_categories >= 12` (the standard site summaries' cutoff
for "cleanly ordinal"). Each was fetched and classified bounded / unbounded /
unclear from (a) whether the table's observed min/max sit at round,
interpretable endpoints and are consistently reached across items, and
(b) `irw_tags()`'s `item_format` field (`Slider/continuous` = bounded
evidence, `Constructed Response` = unbounded evidence). Where the two signals
disagreed, the table was downgraded to `unclear` rather than resolved
automatically — those are marked "CONFLICT" below.

## Bounded (24) — beta-IRT (Noel & Dauvier) candidates

- `tears` — 11 items, clean 0–100 VAS; 2 items top out at 92/98 (small-n, not a format issue).
- `much_tte_2025_currentmotivation` — clean 0–100, "Current Motivation" survey.
- `klippel_irw` — 0–100 but only n=32/item and per-item ranges vary a lot; `Slider/continuous` tag is what resolved this to bounded — verify per-item sample size before trusting.
- `nas_rogoza_2024_study5_nas` — 0–100, Narcissistic Antagonism Scale, daily-diary study.
- `nas_rogoza_2024_study5_ngs` — companion subscale (Grandiosity), same diary study.
- `nas_rogoza_2024_study5_nvs` — companion subscale (Vulnerability), same diary study. These 3 could triangulate as a multi-subscale demo.
- `emoji_scheffler_2024` — 0–100, tagged `Slider/continuous`; valence/arousal/familiarity/clarity/complexity ratings.
- `opentsstvr_linnig_2025_vas` — 0–100, name literally says VAS; VR-environment perception.
- `test_taking_much_2025_cm` — 0–100, "Current Motivation" during test-taking.
- `ai_fear_dong_2026_ai` — 0–100, tagged `Slider/continuous`; 1 of 4 related sub-measures.
- `ai_fear_dong_2026_requirement` — companion sub-measure, same study.
- `ai_fear_dong_2026_own_fear` — companion sub-measure, same study.
- `ai_fear_dong_2026_other_fear` — companion sub-measure, same study. Good 4-subscale set from one study for a multi-subscale demo.
- `westhoff2023_stopd` — 0–100 clean; no `irw_tags()` entry (table not found by tags lookup).
- `westhoff2023_pbat` — same, no tags available.
- `ehealth_rioux_2025_triplep` — 0–100 but some items miss the top (93 unique values); `Slider/continuous` tag resolved it; 1 of a 3-table eHealth family.
- `ehealth_rioux_2025_beam` — companion, same family.
- `ehealth_rioux_2025_abiliti` — companion, same family.
- `lsbq_maleki_2025_non_persian_proficiency` — 0–10, 41% non-integer values (genuinely fractional), tagged `Slider/continuous` — strong candidate.
- `eammi_grahe_2018_marriage_identity_allocation` — 0–100 clean.
- `gilbert_meta_95` — 0–10, Eysenck Personality Inventory Impulsivity subscale — check whether this is item-level or an already-aggregated subscale score.
- `thomeczek2025_les` — 1–20, per-item ranges inconsistent (small sample?); `Slider/continuous` tag resolved the call — check before trusting.
- `double_marking_steele_2022` — 0–100 clean, observational rating (dissertation grading).
- `mclaughlin_samuel_2025_auditory_session_1` — 1.5–10, 39% non-integer, item ranges inconsistent, `Slider/continuous` tag resolved it — small table, check before trusting.

## Unbounded (18) — Samejima-CRM candidates

- `DEMOS` — huge range (0–1121.9), 98% non-integer, no metadata tags — likely a derived/aggregate score; confirm what "item" means here before use.
- `det_naismith_2023` — `Constructed Response` (writing test); range −3.4 to 6, negative values unusual — check codebook.
- `figure_skating` — ISU judges' scores, 2002 Olympics pairs figure skating, tagged `Constructed Response`, 95% non-integer — good genuine continuous-score candidate.
- `vollbracht_et_al_2026_ambulatory_assessment` — −50 to 100, unusual bipolar-looking range, ambulatory/EMA study — check instrument.
- `chile_2023_social-welfare-survey_yy` — construct_name "CASEN Income" — an income variable, not an item-response scale; **likely not a real IRT candidate**.
- `chile_2023_social-welfare-survey_u` — "CASEN Use of Time", 0–1440 = minutes/day — same caveat, a covariate not a scale.
- `cognitive_load_klimova_2023_know` — 1–1000, "Task Knowledge" — unusual scale, check what's measured.
- `simsalRbim_Mice_LargeValence` — animal (mice) behavioral data, no metadata tags — check relevance to a human-response vignette.
- `simsalRbim_Monkey_LargeValence` — animal (monkey) data, same caveat.
- `mclaughlin_samuel_2025_auditory_session_2` — 0–100, resp signal alone was unclear; `Constructed Response` tag (auditory speech transcription accuracy) is what pushed it to unbounded.
- `chile_2023_children-adolescents-survey_cp_c` — "Cuidador Principal" (primary caregiver) — part of the CASEN covariate family, same caveat as the two above.
- `uti_newlands_2023_wpai` — Work Productivity & Activity Impairment (WPAI), a known health-economics instrument — plausibly a real candidate.
- `florida_twins_par` — "My parents Scale", tagged `Likert Scale/selected response` despite `n_categories`=39 — possible item_format tag/data mismatch, worth a look.
- `chile_2023_social-welfare-survey_rr` — "CASEN Social Relationships" — CASEN covariate caveat.
- `contreras_valdez_2022_edeq` — EDEQ items are normally a 0–6 Likert; this table's range suggests a subscale/summed score rather than raw items — check codebook.
- `opladen2025_edeq` — same EDEQ caveat (range 0–112 looks like a summed score).
- `chile_2023_social-welfare-survey_ss` — "CASEN Health" — CASEN covariate caveat.
- `piterova-slovak-science-related-populism` — "Party Positions", 0–98, feels VAS/thermometer-like even though untagged — worth a second look, might actually belong in bounded.

## Unclear (13) — needs a closer look before use

- `realpic_souza2021` — "RealPic" picture-norming ratings, 96% non-integer, item ranges inconsistent — real continuous data, worth checking despite the ambiguous label.
- `yu2025` — "Perceptual response variability in size estimation and reproduction" — a genuinely continuous perceptual-estimation task (0.1–200, 38% non-integer); **strong Samejima-CRM candidate despite the "unclear" label**.
- `estcrm_selfeff` — CONFLICT: resp signal says unbounded, `item_format` says `Slider/continuous`. Table name references the `EstCRM` R package (Samejima CRM estimation) — likely a canonical CRM example dataset; prioritize.
- `estcrm_epia` — same `EstCRM`-package naming pattern, consistent per-item range (1–111) — check together with `estcrm_selfeff`.
- `climatechange_geiger_2025` — 0–100 but item-inconsistent, tagged `Mixed`.
- `wine_luckett2021` — confirmed genuinely mixed format on manual check: 2 items on a 1–9 scale, 1 item on 0–100 (wine aroma intensity vs. familiarity/pleasantness) — good example of "unclear" being the right call; not usable as a single continuous outcome without splitting by item.
- `deception_professors` — 0–100, tagged `Mixed`.
- `deception_game` — 0–100, tagged `Likert Scale/selected response` despite the wide range — possible tag mismatch.
- `lsbq_maleki_2025_persian_comprehension` — CONFLICT: resp signal (unbounded) vs. `item_format` (`Slider/continuous`). Companion to `lsbq_maleki_2025_non_persian_proficiency`, which landed in bounded — check both together.
- `simsalRbim_Mice_LowValence` — 0–35, no metadata tags, part of the simsalRbim animal-data family.
- `immer12_immer` — 29–58 range, non-zero floor, tagged `Likert` despite `n_categories`=29 — odd, check codebook.
- `identity_fusion_gomez_2025` — 0–6 but 27 unique values (genuinely fractional within a narrow range). The round-endpoints heuristic doesn't recognize 6 as a "nice" bound — likely a **false negative**; probably belongs in bounded on manual inspection.
- `eammi_grahe_2018_marriage_timing` — 10–26 range, non-zero floor, tagged `Likert` — check codebook (possibly age-related).

## Recommendations

- **Bounded half is vignette-ready.** The `ai_fear_dong_2026_*` (4 subscales) and `nas_rogoza_2024_study5_*` (3 subscales) families are good for demonstrating the model across related subscales from one dataset.
- **Unbounded half is thinner and messier.** Prioritize `estcrm_selfeff`, `estcrm_epia`, and `yu2025` first — most conceptually on-target for Samejima's CRM (one pair literally named after the R package implementing it, the other a genuinely continuous perceptual-estimation task). `figure_skating` is a solid secondary real-data example.
- **Treat all `chile_2023_social-welfare-survey_*` and `_cp_c` tables as likely non-candidates** despite their unbounded classification — construct names (CASEN Income, Use of Time, Health, Social Relationships) indicate these are demographic/covariate columns bundled as "items," not a coherent single-construct instrument.
- **Check the EDEQ tables' codebook** (`contreras_valdez_2022_edeq`, `opladen2025_edeq`) — EDEQ items are normally bounded 0–6 Likert, so these continuous-looking ranges suggest subscale/summed scores rather than raw items; may need re-fetching at the true item level if so.
- **Don't auto-resolve the 2 signal-conflict tables** (`estcrm_selfeff`, `lsbq_maleki_2025_persian_comprehension`) — a human should look at the actual resp distributions before deciding.
- **Overall recommendation for the vignette**: build the bounded/beta-IRT half directly from real IRW data (strong candidate set above). For the unbounded/Samejima-CRM half, lead with `rt` (response time) as the primary, explicitly-labeled continuous outcome, and use `estcrm_*` / `yu2025` / `figure_skating` as secondary, more caveated real-data validation rather than the main demonstration — the honest read is that IRW doesn't have a deep bench of clean, single-construct unbounded continuous item responses.
