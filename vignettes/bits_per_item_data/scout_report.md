# Bits per item: Phase 0 scouting report

2026-10-05. Branch `vignette/bits-per-item`. Scripts: `vignettes/bits_per_item_scout.R`
(filters and structural scan) and `vignettes/bits_per_item_helpers.R` (information
functions, used for the timing runs). Per-table scan results: `scout_tables.csv`.

**Status: the scan is incomplete.** 322 of the 336 metadata survivors were fetched and checked.
The 14 missing tables are all ENEM `_1mil` tables. Claude Code stopped the scan because the
machine ran low on memory: each ENEM fetch is 45M rows, and the redivis download forks about 7
children of ~5.5 GB each. Their metadata (density 0.82–1.0, 45–55 items) says they would all
pass, so the counts below would rise by 14, but I have not checked their structure.

## 1. Filters, step by step

| Step | Tables |
|---|---|
| Core IRW tables (`irw_metadata()`) | 4,627 |
| Dichotomous (`n_categories == 2`) | 767 |
| 5–60 items | 506 |
| ≥ 500 respondents | 336 |
| Fetched and scanned | 322 (14 ENEM not yet) |
| Pass structural checks after the single-administration rule | 282 |
| **And density ≥ 0.8 (proposed, see §3)** | **245** (+14 ENEM expected = 259) |

### Single-administration rule: restrict, not exclude

I restricted these tables rather than excluding them. The rule is applied to the fetched data:

1. If `wave` takes more than one value, keep the earliest wave (numeric minimum, or the
   alphabetical minimum for string waves).
2. Then, if `treat` takes more than one value, keep the lowest code. That is `0` (control) in
   every table where it applied.
3. If repeated id-item rows remain after that, the table is trial-level or repeated-measures
   data and is excluded.
4. Re-apply ≥ 500 respondents and 5–60 items.

74 of the 282 survivors were restricted: 57 by wave and 56 by treat, mostly the 49
`gilbert_meta_*` RCT tables. The control-arm rule roughly halves N for those even when the
earliest wave is a pre-treatment baseline. I chose that on purpose, because it is one uniform
rule. No table has a `rater` column. `booklet`, `position`, `cluster_id` and `block_id` do not
mark repeated administration, so I left those tables alone.

## 2. Exclusions for structural reasons (40 tables)

| Reason | n | Tables |
|---|---|---|
| Repeated id-item rows after the rule (trial-level tasks) | 9 | `enkavi_2019_{ant_flanker,dpx_axcpt,navon,simon,stopsignal,stroop}`, `imps2025_hf`, `nflverse_passes`, `crspolish_wiesyk_2024_lie` (221 ids, 2,037 duplicate rows) |
| < 500 respondents after wave/treat restriction | 21 | `gilbert_meta_{9,23,24,26,37,38,43,109,110,111}`, `project_kids_{told_grade,topel,wj_ap,wj_pc_grade,wj_pv_grade,wj_spell_grade,wj_spell_wave,wj_wa_grade}`, `dscore_mds_weber_2019`, `knight_2026_crt` (499), `quopl2_forster_2021_dmat` |
| Item count outside 5–60 after the rule | 6 | `project_kids_wj_{pc_wave,pv_wave,wa_wave,wf}` (66–112 items), `ravens_deboeck2012` (105), `much_tte_2025_concentrationtask` (3) |
| Responses not 0/1 despite `n_categories == 2` in metadata | 4 | `preschool_sel_{akt,dn,emt,htks}` (values 0, 1, 2) |

The metadata's `n_items` and `n_participants` disagree with the fetched data for several of
these tables, so the fetch-level check matters.

## 3. Survivors (245 at density ≥ 0.8; 282 without the density filter)

- **n_items:** min 5, Q1 8, median 12, mean 18.2, Q3 25, max 60. By band: 5–10: 98;
  11–16: 50; 17–20: 23; 21–30: 29; 31–45: 26; 46–60: 19. 97 tables have more than 16 items,
  so they need the Monte Carlo I(θ; X).
- **n_respondents (after the rule):** min 504, 10th percentile 610, Q1 918, median 1,361,
  Q3 4,389, 90th percentile 44,906, max 1,000,000. 40 tables will be subsampled to 10,000.
  This becomes ~54 once the ENEM tables are added.
- **Construct type (first tag):** cognitive/educational 67, opinion/attitude 54,
  affective/mental health 27, behavioral 14, personality 6, developmental 3,
  physical health 2, other 1, untagged 71.
- 162 have item text in IRW.

## 4. Decisions I need from you before Phase 1

1. **Density filter (my proposal: ≥ 0.8; it drops 37).** The handoff sets no density
   threshold. I(θ; X) treats every respondent as answering every item, so for planned-missing
   designs (e.g. `icar_sapa`, density ~0.2) and skip-logic surveys (the
   `colombia_2023_politics_*` set, `chile_2024_safety_*`) it describes a form nobody took.
   The earlier `2pl_across_datasets` vignette used 0.8.
2. **ENEM weight.** There are 18 ENEM `_1mil` tables, one exam programme, about 7% of the
   corpus. Keep all 18, or cap at one per subject area (4)?
3. **Rarely answered items.** ENEM `cn` 2014/2018 (52/55 items, density 0.82–0.87) and
   `lc` (50 items: the English/Spanish choice) contain items that only some booklets or
   language groups saw. I propose a general rule: drop items answered by < 50% of the
   table's respondents before fitting, and record how many were dropped.
4. **No slope prior.** I fit plain ML Rasch/2PL, as the handoff specifies. Some tables
   get negative or very large slopes (`wooly_hartman2022` a = −0.7, `rmet_higgins_2022_tom`
   −1.4, NPI tables). Bits are symmetric in the sign of a, but a runaway slope inflates them.
   The alternatives are a lognormal(0, 1) prior on a, as in `2pl_across_datasets`, or plain
   ML with high-slope tables flagged.
5. **String waves.** For two tables the earliest wave is chosen alphabetically
   (`KTEEM_Schoen_2019-2022` → `Spring19`). I have not confirmed that it is the first
   chronologically.

## 5. Proposed showcase tables

| Table | Items | N | 2PL fit (M2) | a range | b range | Why |
|---|---|---|---|---|---|---|
| `fitz_2024_numeracy` | 11 | 2,806 | RMSEA .073, CFI .945 | 0.75–3.89 | −2.09 to 1.19 | Risk numeracy ("Out of 1,000 rolls, how many…"). Everyday construct, English item text, good spread. I(θ; X) = 1.19 bits. Enumeration is exact. |
| `buzgova_2023_gai` | 20 | 980 | RMSEA .068, CFI .971 | 1.13–3.42 | 0.53 to 1.48 | Geriatric Anxiety Inventory ("I often feel nervous"). Gives a non-cognitive example with the cleanest fit, but every item targets the anxious tail, so the bits sit in the upper tail. That is a teaching point, but it fails the "spread of difficulties" criterion. I(θ; X) = 1.57 bits. |
| `frac20` | 20 | 536 | RMSEA .099, CFI .957 | 0.87–4.30 | −1.10 to 0.66 | Tatsuoka fraction subtraction. The item text *is* the item ("5/3 − 3/4"), which makes the walk-through vivid. Classic dataset. But N is barely over 500, RMSEA is weak, and b is narrow. I_S = 1.85 bits. |

Runners-up: `machivallianism_test_vcl` (16-word vocabulary checklist, N 73k, fit OK, but the
three fake words load *positively*: over-claiming); `argentina_2017_victimization_precaution`
(13 items, CFI .973, but the item text is Spanish); `lec5_xiong_2025` (a checklist of life
events, not a latent trait).

**A question on the GAI:** the instrument is copyrighted (Pachana et al.). IRW item text carries
no reuse licence, and an interactive page that shows all 20 items is reproduction. If that is a
problem, the replacement is the vocabulary checklist or a further cognitive table.

## 6. Runtime estimate

The full per-table pipeline (fetch → rule → subsample → Rasch + 2PL → H(X|θ), Lord–Wingersky
I(θ; S), exact or MC I(θ; X) with SE < 0.005, reliabilities, per-item bits, 5 random + greedy
length curves, sum-score KL) was timed end to end on 6 tables:

| Table | Items | N used | I(θ;X) method | Total | of which fetch |
|---|---|---|---|---|---|
| `fukuda_2021_withholding_behavior` | 6 | 1,000 | exact | 4.9 s | 4.4 s |
| `fitz_2024_numeracy` | 11 | 2,806 | exact | 1.0 s | 0.5 s |
| `machivallianism_test_vcl` | 16 | 10,000 (of 73k) | exact | 6.9 s | 3.9 s |
| `buzgova_2023_gai` | 20 | 980 | MC | 3.4 s | 2.2 s |
| `icar_sapa` | 60 | 9,844 | MC | 31.8 s | 26.4 s |
| `enem_2024_1mil_ch` (from cache) | 45 | 10,000 (of 1M) | MC | 35.1 s | 10.1 s (+21.8 s prepare), 5.5 GB peak |

Model fitting and all the information quantities take ≤ 5 s per table. Data movement
dominates. The scan has already cached every non-ENEM table locally, so:

- **241 non-ENEM tables:** ~5–10 s each → about 10 min with 4 workers.
- **18 ENEM tables:** 4 are cached (~35 s each). The 14 others need a fresh fetch of
  ~5.6 min each, and they must run **one at a time, with nothing else heavy** (this run
  showed that two at once runs out of memory) → ~80 min.
- **Showcases (3 tables):** replays, the 500-respondent adaptive simulation and the
  K = 1…8 bin check are all on ≤ 20 items. That is minutes at most (not yet timed).

**Total: ~1.5 h**, almost all of it ENEM downloads. It drops to ~15 min if ENEM is capped
at the 4 cached tables.

## 7. Things I noticed or decided on my own

- **`mirt::marginal_rxx()` (mirt 1.46.1) is wrong for Rasch fits.** It computes
  TI/(TI + var(θ)) where it should compute TI/(TI + 1/var(θ)), and it integrates over N(0, 1)
  whatever variance was fitted. On `fukuda_2021_withholding_behavior` it gives .12 for the Rasch fit, against
  .67 when computed correctly. I wrote `marginal_rel()`, which reproduces `marginal_rxx()`
  exactly for the 2PL (.698323 on that table), and I use it for both models.
- MC target SE is 0.005 bits, stricter than the handoff's 0.01. Every timed MC run stopped
  at 60k draws, with SE ≈ 0.0044.
- The MC draws θ from the same discrete quadrature g as every other quantity, so exact,
  Lord–Wingersky and MC numbers are directly comparable.
- `irw_long2resp()` silently drops respondents with response density < 0.1 (e.g. 16 of 10,000
  in `ecuador_2011_safety_avoidance`). I'll record that count per table.
- The sum-score KL model check needs complete cases. Tables with none (e.g. `icar_sapa`) get NA.
- Responses stored as something other than 0/1 are recoded to 0/1 (higher value = 1).
- First numbers, for orientation only: 2PL I(θ; X) ran 0.99–1.86 bits on the 6 timed tables.
  The Gaussian −½ log₂(1 − ρ_marginal) was within 0.03 bits on `icar_sapa` and ENEM, and
  0.09–0.12 bits low on the three short tables. On `buzgova_2023_gai` it was 0.46 bits low
  (1.11 vs 1.57), which fits the GAI's items all sitting in one tail.
