# Bits per item, polytomous appendix: Phase 0 scouting report

2026-10-07. Branch `vignette/bits-polytomous` (off `vignette/bits-sufficiency`). Script:
`vignettes/bits_per_item_poly_scout.R`. Per-table and per-item-by-category results:
`poly_scout_tables.csv`, `poly_scout_items.csv`.

## Scope (decided 2026-10-07)

The appendix uses **three hand-picked tables, not a corpus**. The metadata filters
(3–11 categories, 3–60 items, ≥ 500 respondents) leave 1,950 tables, against 336 for the
dichotomous arm, so I stopped the full scan after 3 tables. The appendix keeps four of the
handoff's figures: the log₂ K ceiling against real bits, collapsing categories, sufficiency
by model family, and ordered vs. nominal. It drops the Fisher-view figure, the hook-widget
extension, and the "whole warehouse, revisited" distribution.

| Step | Tables |
|---|---|
| Core IRW tables (`irw_metadata()`) | 4,912 |
| 3–11 categories (metadata `n_categories`) | 4,044 |
| 3–60 items | 3,767 |
| ≥ 500 respondents | 1,950 |
| + 4–7 categories, 5–15 items, density ≥ .99, not longitudinal, N 1k–50k, item text | 142 (shortlist) |
| **Chosen** | **3** |

## The three tables

| Table | Role | Items | K | N (after rule) | Used | Patterns Πⱼ Kⱼ |
|---|---|---|---|---|---|---|
| `rosenberg_selfesteem` | showcase | 10 | 4 | 47,929 | 10,000 subsample | 4¹⁰ = 1,048,576 |
| `bfi_goldberg_1992_extraversion` | | 10 | 5 | 19,718 | 10,000 subsample | 5¹⁰ ≈ 9.8M |
| `su_2024_gad7` | | 7 | 4 | 24,292 | 10,000 subsample | 4⁷ = 16,384 |

- **Single administration:** none of the three has `wave` or `treat`. None has repeated
  id-item rows or a rater column. All are fully crossed (density ≥ .997). No restriction is
  needed.
- **Responses** are consecutive integers in every table (Rosenberg 1–4, BFI 1–5,
  GAD-7 0–3). Every item uses every category, and no table mixes scales across items.
- **Fit check** (10,000-respondent subsample, mirt 1.46.1): PCM, GPCM, GRM and nominal all
  converge on all three tables. Each fit takes 0.5–5 s.

## Decision 1: reverse-keyed items (new; not in the handoff)

**IRW stores Rosenberg and BFI extraversion unreversed.** On the raw data, item-rest
correlations are −0.24 to +0.14. The fitted GPCM/GRM slopes split by sign exactly along the
published keying:

- Rosenberg: items 3, 5, 8, 9, 10 have negative slopes. These are the negatively worded items.
- BFI extraversion: E2, E4, E6, E8, E10 have negative slopes. These are the reverse-keyed items.
- GAD-7 is all one direction (item-rest r .66–.77).

Mutual information is invariant to relabeling categories, so under GPCM, GRM and nominal,
reversal leaves I(θ; Xⱼ) and I(θ; X) unchanged. It does not leave the PCM or the sum score
unchanged. The PCM has no slope, so with half the items reversed it is badly misspecified,
and S then adds agreement with positive and negative statements. That would wreck the
sufficiency figure.

**Proposal:** reverse the negatively keyed items (x → K + 1 − x, or 3 − x for GAD-7's
0–3 scale, which needs no reversal anyway) before fitting. Use the published keys, which
match the slope signs. Record the reversed items in the outputs and say so in the text. The
alternative is to leave the items raw and drop the PCM and sum-score parts for these two
tables, which I don't recommend.

## Decision 2: sparse categories

| Table | Categories < 1% (item × category) | Empty |
|---|---|---|
| Rosenberg | 0 of 40 (smallest: 4.3%) | 0 |
| BFI extraversion | 0 of 50 (smallest: 7.4%) | 0 |
| GAD-7 | **7 of 28**: the top category ("nearly every day") on every item, 0.4–1.3%. The next category is 0.8–2.1%. | 0 |

At N = 10,000, GAD-7's top category has about 40–130 responses per item. That is enough for
the PCM, GPCM and GRM thresholds, but the nominal model will be the shakiest there. In this
general-population sample, 65–86% answer 0 and 13–33% answer 1, so each item works almost
like a dichotomous one.

Options. Any recode must happen before ceilings are computed, because it changes K.

1. **Keep every category; flag GAD-7's sparse items** (my recommendation). The log₂ 4 = 2-bit
   ceiling stays, and the observed category entropy H(Xⱼ) (about 0.7–1.1 bits here) shows
   the gap. This is the "categories offered are not categories used" point the appendix
   wants to make, with the instrument's own scale intact. Cost: noisier nominal estimates
   for the top categories.
2. **Merge any category under 1% into its neighbour.** GAD-7 becomes K = 3 on every item,
   with a 1.58-bit ceiling. The estimates are more stable, but the collapse is then done by
   us, before the analysis that asks what collapsing costs, and the ceiling no longer matches
   the instrument.
3. **Drop empty categories only** (none here, so it is the same as option 1 for these three
   tables). It is worth writing down as the rule for anyone who adds tables later.

## Showcase: `rosenberg_selfesteem`

The Rosenberg scale (Rosenberg, 1965) is 10 items on four categories, strongly disagree to
strongly agree. A general reader knows the construct. All four categories are well used
(4–50% each), there is item text, and N is large. Because half its items are negatively
worded, it also makes the reverse-keying point concrete.

The hook widget is out of scope. For the record, it would generalise without restructuring:
`pg` becomes groups × items × K, there is one answer button per category, the two bars
become K, and h₂ becomes the category entropy. That is about 40 changed lines.

## Settings I'd use (please confirm)

- **Subsample:** 10,000 respondents per table, seeded by table name as in the dichotomous arm.
- **Grid:** the same as the dichotomous arm (81 nodes, ±6 SD of the fitted latent
  distribution). The PCM estimates the latent variance; GPCM, GRM and nominal fix N(0, 1).
- **Slopes:** no lognormal prior. It would force slopes positive, which only works after
  reversal, and the four models should be compared as fitted. If you want the dichotomous
  arm's prior after reversal, say so.
- **Enumeration cutoff:** the handoff says Πⱼ Kⱼ ≤ 10⁶. Rosenberg is 1,048,576, just over.
  I propose raising the cutoff to 2²⁰ so the showcase is exact. The GPCM weighted-score and
  GRM sufficiency checks need enumeration on the showcase. In chunks it takes about a
  minute and well under 1 GB. BFI uses Monte Carlo with SE < 0.005 bits, matching the
  dichotomous arm's stricter target rather than the handoff's 0.01. GAD-7 is enumerated.
- **Prior sweep** for the sufficiency inset: the same 16 priors as `bits_per_item_sufficiency.R`.

## Runtime estimate

Fetch: 3–6 s per table, already cached. Four fits: under 10 s per table. Rosenberg
enumeration: about a minute per model × 3 models, plus the prior sweep, which is 16 priors
× 3 models on the showcase, so a few minutes. Item collapses: 7 (K = 4) or 15 (K = 5) per
item, all cheap. The test-level "dichotomize everything" number is one Monte Carlo per
table. **Total: under 15 minutes, serial.**

## Things I noticed or decided on my own

- I stopped the corpus scan at your request. Only the three chosen tables are kept in the
  scout CSVs.
- Metadata `n_categories` agrees with the fetched data for all three tables.
- `irw_long2resp()` drops a handful of near-empty respondents (5 in the Rosenberg
  subsample, 1 in BFI). This will be recorded per table, as in the dichotomous arm.
- The IRW response-styles vignette was withdrawn from the site (2026-08-30), so there is
  nothing to cross-reference. The text will mention response styles as part of H(X | θ)
  without a link.
