# Scouting report: RCA/CCA algorithm-selection vignette on IRW data

**Date:** 2026-08-27
**Scope:** feasibility scouting only. No vignette code was written and no models were fitted beyond the Task 4 pilots.
**Reference:** Sotoudeh & DiMaggio (2021), "Coping With Plenitude," *Sociological Methods & Research* 52(4):1838–1882, doi:10.1177/00491241211031273.

---

> [!IMPORTANT]
> **Verdict superseded — read the amendment first.** The NO-GO below rested
> entirely on threshold 2, which failed only because it compared an IRW
> *interquartile range* against Table 9's *full range* — not a like-for-like
> test. Retested IQR against IQR, 5 of 13 comparable features pass, and the
> result is robust to dropping the high-category tail and to the N = 2,000 cap.
> **All four thresholds are met.** See "Amendment (2026-08-27, follow-on)" below.
>
> Everything above that amendment is preserved exactly as first written, per the
> amendment's own convention of appending rather than rewriting. The
> recommendation in the next section is therefore the *original* one and no
> longer the operative verdict.
>
> The corpus-scale vignette was subsequently built: see
> `analysis/plenitude_replication.qmd` and PR #99, which draws its sampling
> frame from `scouting/scout_eligible_tables.csv` and carries forward the code
> defects documented in Task B1.

## Recommendation: NO-GO on the corpus-scale vignette (SUPERSEDED — see above)

Three of the four thresholds pass. The one that fails is the one that decides whether the vignette has a subject.

| # | Threshold | Verdict | Evidence |
|---|---|---|---|
| 1 | Corpus size — ≥60 single tables **or** ≥25 verified merge groups | **MET** | 584 eligible tables (9.7× the bar); 114 merge groups (4.6× the bar) |
| 2 | Feature spread — ≥5 meta-features whose IRW IQR exceeds the Table 9 range | **MISSED** | **0 of 13** comparable features. The lone nominal pass (`PercentOut`) is a scale artifact |
| 3 | Non-degeneracy — >1 substantive class on a majority of tables, not explained by reverse-keying | **MET** | 0 runs with k=1, 0 with a class >75%, 19/19 with ≥2 substantive classes; reverse-key η² median 0.154 |
| 4 | Compute — full run tractable | **MET** | ~9.3 h single-core, ~1.2 h on 8 cores |

**Unresolved:** `id`-comparability verified on 4 of 114 merge groups; ANES licensing terms; Online Appendix E.

### Why threshold 2 is decisive

The vignette's premise was that Sotoudeh & DiMaggio's regressions extrapolate because nine GSS/ANES modules occupy a narrow band of the simulated feature space, and that hundreds of real datasets could characterize the empirical manifold directly instead. That premise requires IRW to *spread* across the feature space where their nine datasets do not.

It does not. Across 29 pilot matrices, **not one comparable meta-feature has an interquartile range exceeding the full range of their nine datasets.** IRW reaches further at the extremes — its full range beats theirs on 11 of 14 features — but its middle is tightly packed. IRW is overwhelmingly Likert self-report (2,003 of 2,435 tagged tables), and it shows: the corpus populates the *same* narrow region their nine datasets occupy, just more densely, with a thin tail of atypical tables (the 101-category rating scales) doing all the range-stretching.

Characterizing the empirical manifold with 584 IRW tables would therefore mostly re-confirm where the manifold already is. There is no prediction problem to solve, because there is no meaningful variation in the predictors. More datasets in the same place does not fix extrapolation.

The merge path does not rescue this. Merging *raises* mean person-correlation (0.18 → 0.36) and *lowers* proportional intrinsic dimensionality — it manufactures block structure rather than the heterogeneous covariance RCA is designed to find.

### What the scouting run does support

Two smaller, defensible things, offered as observations rather than a redesign:

1. **A methods-critique note.** The artifact chain is a personal Google Drive folder with no DOI. `evaluate.metafeatures()` has a hardcoded `num_vars = 10` that silently corrupts two of fourteen features on any matrix that is not ten items wide; `OverallRightKurt` takes the kurtosis of a logical vector while the evidently-intended helper sits defined and uncalled; Louvain is hardcoded despite the paper conceding the choice was consequential; and Table 9's `PercentOut` column is on a different scale than the function that supposedly produced it. These are checkable claims about a published method, and they are the most solid findings here.

2. **GSS ingestion, independent of the vignette.** None of the nine modules is in IRW. Eight are blocked by one NORC licensing question, and IRW already carries 106 tables under `Permission via Email` — so the path exists and has been walked before. One email covers all eight.

Per the brief, I am not proposing a reshaped vignette. Threshold 2 failed on the merits and the corpus-scale version should be reported as not viable.

---

## Task 1 — Repo reconnaissance

### The GitHub repo is a stub

`https://github.com/raminasotoudeh/coping_with_plenitude` contains **one file**: a 1.2 KB `README.md`. Single branch (`main`, `a617939`). No code, no data.

The README redirects to a public Google Drive folder (`1TxvPMWHjX-ZK1Ih0IRX-FvRNFNmxMmcc`), stating the files were moved there because of GitHub file-size limits. That folder is **not listed by the Drive API** for an unauthenticated caller and the `claude.ai` Drive connector returns nothing for it, but the folder's HTML listing embeds the file IDs, and each file downloads over `drive.usercontent.google.com`.

The folder holds **six** files, not the four the README describes:

| File | Size | Status |
|---|---|---|
| `metafeature_RCA.R` | 15.6 KB | retrieved — the main file |
| `f_outer.cpp` | 1.0 KB | retrieved — Rcpp helpers |
| `recursive_relationality.R` | 2.0 KB | retrieved — standalone duplicate of the recursive path |
| `squared_RCA.R` | 2.6 KB | retrieved |
| `metafeature_models_cleaned.RDS` | ~40 KB unpacked | retrieved (fitted meta-feature regressions) |
| `models_w_noise.RDS` | large | **NOT retrieved** — download timed out; see Open items |

**Implication for reproducibility:** the paper's artifact chain runs through a personal Google Drive folder with no DOI, no versioning, and no guarantee of persistence. Anything built on it should vendor its own copy of these files.

### The ten distance measures

All measures are implemented in **`metafeature_RCA.R`**, in **plain R**, dispatched from a single function `metaRCA(data, measure = ...)`. They are **not entangled with the simulation harness** — `metaRCA()` takes a bare data matrix and returns a membership vector. Portability is good.

| Measure in brief | Implemented as | Backing code | Portable? |
|---|---|---|---|
| `eJaccard` | `proxy::simil(method="eJaccard")` | CRAN `proxy` | yes |
| `Podani` | `proxy::simil(method="Podani")` | CRAN `proxy` | yes |
| `eDice` | `proxy::simil(method="eDice")` | CRAN `proxy` | yes |
| `cosine` | `proxy::simil(method="Cosine")` | CRAN `proxy` | yes |
| `Euclidean` | `proxy::simil(method="Euclidean")` | CRAN `proxy` | yes |
| `correlation` | `corr.dist()` + `filter.insignif()` | local, self-contained | yes |
| `ACE` | `ace.dist()` → `acepack::ace()$rsq` | CRAN `acepack` | yes |
| `original relationality` | `rca.dist()` → `relationality()` | local, pure R, O(n²) double loop | yes, but slow |
| `new relationality` (a.k.a. Recursive Relationality — the paper's headline measure) | `recursive.rca.dist()` → `relationalityC()` | **Rcpp**, `f_outer.cpp` | yes |
| pairwise-voting ensemble | `dyadic_vote_ensemble()` | local, self-contained | yes |

Notes that matter for porting:

- `f_outer.cpp` defines **both** `f_outer` (pairwise differences) **and** `relationalityC`. The recursive path is complete — no missing symbol. It compiles under Rcpp 1.1.2 without modification.
- Each measure has **bespoke post-processing** inside `metaRCA()` that is easy to miss and not documented in the paper: the `proxy`-family measures get median-centred (except Cosine); `eJaccard`/`Cosine`/`eDice` get an extra "oca rescale" (`x - median(min(x):max(x))`) applied first; Cosine is hard-thresholded at its 75th percentile; Original Relationality is thresholded at 0.05; Recursive Relationality is **squared** rather than absolute-valued. Reimplementing "just the distance measure" without this block will not reproduce their results.
- `filter.insignif()` is applied only to Correlation (p<0.05) and ACE (p<0.10). Two different cutoffs, unexplained.

### Community detection — a real and undocumented degree of freedom

`metaRCA()` **hardcodes `igraph::cluster_louvain()`**. There is no argument to change it. This matches the paper's own admission that swapping leading-eigenvector for Louvain accounted for a meaningful share of RCA's measured improvement — but the shipped code does not expose the choice, so a user of `metaRCA()` silently gets Louvain regardless of what the rest of the RCA literature (and the CRAN `RCA` package) does.

Note also that `metaRCA()` has **no bootstrap edge-pruning at all**. The CRAN `RCA` package prunes edges by bootstrap (`RCA(matrix, num=1000, alpha=0.05)`); the paper's pipeline substitutes an analytic t-test (`filter.insignif`) for two measures and no pruning for the rest. These are not the same algorithm, and timings from one do not transfer to the other.

### Meta-feature code: present, reusable, and none require simulation parameters

`evaluate.metafeatures(df, num_vars = 10)` is a **pure function of the data matrix**. It returns exactly **14** features, and they line up one-to-one with the 14 columns of Table 9. **None of them require the simulation's generative parameters** — all are computable on real data. This is the single most reusable piece of the artifact.

| Returned name | Table 9 column |
|---|---|
| `OverallSD` | Overall SD |
| `OverallKurt` | Overall Kurtosis |
| `OverallRightKurt` | Overall Kurtosis of Right-tail |
| `VarColPC1` | Variance of PC 1 |
| `SkewColPC1` | Skewness of PC 1 |
| `SkewColPC2` | Skewness of PC 2 |
| `Skewness` | Overall Skewness |
| `Mean_Row_Corr` | Mean correlation of responses |
| `Row_Corr_Kurtosis` | Kurtosis of RCs |
| `CorrRightKurt` | Kurtosis of right-tail of RCs |
| `Mean_Col_Kurtosis` | Average kurtosis of variables |
| `Mean_SD_Ratio` | Mean SD Ratio |
| `intrinsicDemnsionalityProp` | Intrinsic Dimensionality |
| `PercentOut` | Percent of Variables with Outlier Observations |

Three defects in this function that anyone reusing it must handle:

1. **`num_vars` defaults to 10 and is never inferred from the data.** It is the denominator of both `PercentOut` and `intrinsicDemnsionalityProp`. Calling `evaluate.metafeatures(x)` on a 30-item matrix silently produces two wrong features. We set `num_vars = ncol(df)`.
2. **An unseeded random column is injected**: `df_new$class <- sample(1:4, nrow(df_new), replace=TRUE)`, purely to satisfy `mfe::statistical()`'s formula interface. With `by.class=FALSE` the `nrOutliers` result should not depend on it, but the function is non-deterministic by construction. We set a seed.
3. **`overall_right_kurt` computes the kurtosis of a logical vector**: `Kurt(df > mean(df))`. This is almost certainly a bug — the analogous `right_half_kurt` helper defined immediately above it (`Kurt(x[x > mean(x)])`) is the evidently intended form, and it is **defined but never called**. We reproduced their line verbatim rather than silently repairing it, but Table 9's `OverallRightKurt` column is therefore measuring the kurtosis of a Bernoulli indicator, not of the right tail of the response distribution. That column's very low reported simulation overlap (3%) is consistent with it being a near-degenerate quantity.

**Terminology correction for the brief:** the brief lists "mean inter-item correlation" as a meta-feature. `Mean_Row_Corr` is `mean(cor(t(df)))` — the mean **person-by-person** correlation, which is the CCA similarity matrix itself, and Table 9 labels it "Mean correlation of responses." There is no inter-*item* correlation feature in their set. We computed inter-item Pearson and polychoric analogues separately as supplementary columns; they are not substitutes for `Mean_Row_Corr`.

### The nine empirical matrices are NOT distributed

Neither the GitHub repo nor the Drive folder contains the constructed GSS/ANES modules — only code and fitted models. `metafeature_RCA.R` loads `readstata13`, implying the authors worked from local `.dta` files that were never shipped. **Reproducing their Table 9 exactly requires rebuilding the nine matrices from source data**, which requires Online Appendix E (see Task 5).

---

## Task 2 — Corpus eligibility

Source: `irw_metadata()` (2,949 tables) joined to the `irw_meta` Redivis `tags` (2,435 rows) and `biblio` (3,688 rows) tables.

### Marginal counts

| Criterion | Tables |
|---|---|
| `density == 1` | 1,476 |
| `n_categories >= 3` | 2,385 |
| `n_categories >= 4` | 2,263 |
| `n_categories >= 5` | 1,894 |
| `n_items` in 6–40 | 1,923 |
| `n_participants >= 300` | 2,093 |

### Sequential attrition

| Step | Remaining |
|---|---|
| all IRW tables | 2,949 |
| `density == 1` | 1,476 |
| + `n_items` 6–40 | 1,017 |
| + `n_participants >= 300` | 694 |
| + `n_categories >= 3` | 607 |
| + `n_categories >= 4` | **584** |
| + `n_categories >= 5` | 491 |

The `n_categories >= 4` cutoff is cheap: it costs 23 tables relative to `>= 3` and buys the argument against tied similarity matrices. Going to `>= 5` costs a further 93. **584** is the headline single-table corpus.

### Construct split (584 eligible, not filtered on)

| Group | Tables |
|---|---|
| affective/opinion (Opinion/attitude, Affective/mental health, Personality) | 260 |
| cognitive (Cognitive/educational) | 66 |
| other (Behavioral, Physical health, Developmental, …) | 39 |
| **untagged** (no row in the tags sheet) | **219** |

219 of 584 eligible tables have no construct tag at all — 37%. Any claim in the vignette that splits results by construct type is really running on 365 tables, not 584.

### Merge-eligible groups

Name prefixes are unreliable (`ffm_ext` and `bfi_goldberg_1992_extraversion` are the same study). **DOI is populated for all 2,949 tables** and is the better key.

Among the 584 eligible tables, 522 carry a usable DOI, spanning 249 distinct DOIs:

| Group size | Count |
|---|---|
| ≥2 eligible tables | **114 groups** (387 tables) |
| ≥3 eligible tables | 67 groups |
| ≥2 *distinct* construct types | 29 groups |

Largest groups run to 12 tables (`anunciacao_2025_personality_*`) and 10 (`bfi_goldberg_1992_*`). Substantively many are exactly what RCA wants — e.g. `alsuhibani_2022_*` spans paranoia, conspiracy beliefs, attachment, and locus of control.

### `id` comparability — VERIFIED, merge path is open

Tested on 4 groups (12 tables fetched):

| Group | Result |
|---|---|
| `lorenz_2016` (hope / resilience / optimism) | 321/321 shared — **100%** |
| `jablonska_2020` (HADS / RSES / Instagram addiction) | 974/974 — **100%** |
| `silva_2018` (BSQ / TFEQ / WHOQOL) | **100%** (smaller table nests inside larger) |
| `alsuhibani_2022` (PADS / GCBS / LOC) | 94–100% *within* sub-sample; **0% across sub-samples** |

`id` **is** stable across sibling tables from the same study. The `alsuhibani` case is the failure mode to guard against: that paper ran three separate sub-samples and ids are namespaced by sample (`1xxxxx` vs `2xxxxx`), so a naive DOI-keyed merge yields a matrix that is mostly missing. This is cheaply auto-detectable with exactly the pairwise-overlap test used here, so it is a filter to apply, not a blocker.

**Caveat on generality:** verified on 4 of 114 groups. Verifying all 114 requires fetching ~387 tables, which was outside this run's download budget. The check is cheap per group and should be run as a gate inside any real pipeline rather than assumed.

**Deliverables:** `scouting/scout_eligible_tables.csv` (584 rows), `scouting/scout_merge_groups.csv` (114 rows).

---

## Task 3 — Meta-feature pilot

### What was run

29 matrices: **25 single tables** (stratified on `n_categories` band × affective/cognitive tag — 18 affective/opinion, 7 cognitive) plus **4 merged multi-construct matrices**. Item counts 6–63, respondent counts 295–4,992.

`evaluate.metafeatures()` was ported verbatim (`scouting/pilot_metafeatures.R`), with the three fixes noted in Task 1 (`num_vars = ncol`, seeded, `OverallRightKurt` left as-is).

**Design choices made without asking, per the brief:**

- `num_vars = ncol(df)` rather than their hardcoded 10.
- Respondents capped at **N = 5,000** per matrix (meta-features are stable well below this; `Mean_Row_Corr` is O(N²) to compute).
- Tables with **> 50,000 respondents excluded** from the sampling frame. This is a *download-cost* decision, not a statistical one — RCA subsamples to N=500 regardless. It removes 26 of 584 eligible tables (4.5%). The first draw pulled `ffm_csn` at 1,015,341 respondents, which alone stalled the fetch for 8 minutes.
- Complete cases only; zero-variance rows dropped (819 rows across all 29 matrices; **0 columns** dropped).
- Outliers: left as `mfe`'s Tukey-boxplot `nrOutliers`.
- Polychoric via `psych::polychoric` default (two-step ML, continuity-corrected).

### Feature spread vs. the paper's nine datasets — THRESHOLD 2 FAILS

The threshold asks for **≥5 meta-features whose IRW IQR exceeds the Table 9 range**. Result: **1 of 14**, and that one is an artifact.

| Feature | IRW IQR | IRW range | Table 9 range | IQR > T9 range? |
|---|---|---|---|---|
| OverallKurt | 0.821 | 8.472 | 2.300 | no |
| OverallRightKurt | 0.120 | 0.902 | 1.061 | no |
| OverallSD | 0.756 | 26.51 | 1.597 | no |
| Skewness | 0.396 | 3.741 | 1.312 | no |
| VarColPC1 | 17.35 | 2141 | 46.65 | no |
| SkewColPC1 | 0.466 | 2.678 | 1.629 | no |
| SkewColPC2 | 0.340 | 1.266 | 2.063 | no |
| Mean_Row_Corr | 0.168 | 0.654 | 0.564 | no |
| Row_Corr_Kurtosis | 0.551 | 2.404 | 3.939 | no |
| CorrRightKurt | 0.704 | 11.45 | 2.672 | no |
| Mean_Col_Kurtosis | 0.850 | 8.664 | 3.607 | no |
| Mean_SD_Ratio | 0.071 | 0.456 | 0.436 | no |
| intrinsicDemnsionalityProp | 0.096 | 0.389 | 0.371 | no |
| **PercentOut** | **43.33** | 100.0 | 0.048 | **yes** |

**The single pass is spurious.** IRW `PercentOut` spans 0–100; Table 9's spans 0.000–0.048. Their own code computes `(nrOut/num_vars)*100`, which for a *count* of outlier-bearing variables can never land at 0.004. Table 9's column is on a different scale than the function that supposedly produced it. The two numbers are not comparable, so this feature cannot count toward the threshold. **Comparable features passing: 0 of 13.**

### But the picture is more interesting than a flat fail

The threshold compares an **IQR** (middle 50% of 29 matrices) against a **full range** (all 9 of theirs) — an inherently demanding test. On the like-for-like comparison, IRW is clearly wider:

**IRW full range exceeds the Table 9 range on 11 of 14 features** (all but `OverallRightKurt`, `SkewColPC2`, `Row_Corr_Kurtosis`).

So: **the IRW slice reaches further than nine GSS modules on most features, but its middle is tightly clustered.** Most IRW tables look like each other — unsurprising, since IRW is overwhelmingly Likert self-report (2,003 of 2,435 tagged tables are "Likert Scale/selected response"). The range is stretched by a handful of atypical tables, notably the wide-category ones: `test_taking_much_2025_cm` (101 categories, OverallSD 18.9, VarColPC1 2,144) and `climatechange_geiger_2025` (OverallSD 27.3), which are IRW's analogues to their Occupational Prestige outlier.

That is a real finding for the vignette's premise: **IRW does not densely populate a wider manifold; it populates the same narrow region more thickly, with a thin tail of unusual tables.** Characterizing the empirical manifold "directly instead of assuming it" would mostly re-confirm where the manifold already is.

### Degenerate / misbehaving features

None were undefined or constant — all 14 returned finite, distinct values on all 29 matrices. Specific concerns:

- **`PercentOut`** — not degenerate but not comparable to theirs (above). Also lumpy: it is a count over `ncol`, so on a 9-item matrix it can only take 10 values, and 2 of 29 matrices sat at exactly 0 and 4 at exactly 100.
- **`intrinsicDemnsionalityProp`** — only **21 distinct values across 29 matrices**. It is a small integer over `ncol`, so it is coarse by construction. Fine as a covariate, poor as a continuous predictor.
- **`OverallRightKurt`** — the narrowest feature (IRW range 0.902, IQR 0.120) and the one whose definition is a probable bug (kurtosis of a logical). Its low variance in both the simulations and IRW is consistent with it carrying little information.
- **`Mean_InterItem_Polychoric`** (supplementary) — **failed on 5 of 29 matrices**, with `psych::polychoric` warning about unequal category counts and cells adjusted for zero counts. On merged multi-construct matrices, where sub-scales have different response formats, the polychoric analogue is unreliable. Pearson inter-item was computable everywhere (range −0.026 to 0.613). **Recommendation: use Pearson.**

### Merged vs. single

| | Mean_Row_Corr | intrinsicDimProp | VarColPC1 | OverallSD | PercentOut |
|---|---|---|---|---|---|
| merged (n=4) | 0.356 | 0.24 | 13.24 | 1.472 | 60.8 |
| single (n=25) | 0.178 | 0.30 | 10.12 | 1.392 | 35.9 |

Merging **doubles** mean person-correlation (0.18 → 0.36) and *lowers* proportional intrinsic dimensionality. Both point the same way: stacking sub-scales that share a respondent sample creates strong block structure, which raises overall person-similarity rather than creating the heterogeneous covariance patterns RCA is meant to detect. Merging is mechanically easy and substantively **not obviously** the fix the vignette premise assumes.

**Deliverables:** `scouting/scout_metafeatures.csv` (29 rows × 14 features + supplements), `scouting/scout_feature_spread.csv`, `scouting/table9.csv` (their Table 9, transcribed).

---

## Task 4 — Non-degeneracy and timing

Five pilot tables at N = 500, spanning 12–40 items, each run through three algorithms: **RCA** (CRAN `RCA`, with the bootstrap edge-pruning step, `num=1000, alpha=0.05`), **CCA** (`corclass::cca`, significance-filtered), and **Recursive Relationality** (the paper's own headline measure, lifted from `metafeature_RCA.R`).

### Non-degeneracy — THRESHOLD 3 MET

| | |
|---|---|
| runs returning results | 19 |
| runs with **k = 1** | **0** |
| runs with largest class **> 90%** | **0** |
| runs with largest class > 75% | 0 |
| runs with ≥2 substantive classes (≥5% of N) | **19 of 19** |

No degeneracy. Largest class across all runs peaked at 66.6%, and every single run recovered at least two substantive classes. RCA and CCA both routinely return many tiny classes on top of a few real ones (RCA median k = 10, median substantive k = 3), which is why the "substantive" count matters more than raw k.

| Method | median secs | median k | median substantive k | median largest | median reverse-key η² |
|---|---|---|---|---|---|
| CCA (corclass) | 0.1 | 4 | 3 | 0.436 | 0.154 |
| RCA (bootstrap num=1000) | 81.8 | 10 | 3 | 0.426 | 0.301 |
| Recursive Relationality | 90.5 | 4 | 4 | 0.328 | 0.242 |

### Reverse-keying diagnostic — passes, but not comfortably

Median η² = **0.154**: about a sixth of recovered class variance is explained by nothing more than whether a respondent agrees with the sign of the first principal component's loadings. **3 of 19** runs exceeded 0.5 (`short_dark_triad` under CCA at 0.624; `hexaco_ashton_2014_h` under Recursive Relationality at 0.611; `MERGED_jablonska_2020` under RCA at 0.510).

So the diagnostic does **not** explain *most* of the structure, and the threshold is met as written. But a quarter is not a rounding error, and the worst cases are bad. Reported plainly: **the classes are not merely careless-responder artifacts, but a meaningful minority of the signal is polarity agreement**, and any real vignette would need this diagnostic as a standing control, not a one-off check.

**The merged matrix is worse, not better.** On `MERGED_jablonska_2020` (46 items, 5 sub-scales sharing a respondent sample), RCA returned η² = **0.510** — the highest RCA value observed, and above the 0.5 line. CCA on the same matrix went the other way (η² = 0.014) but collapsed to just 2 substantive classes with 52% of respondents in the largest. This is the same warning Task 3's meta-features gave from the other direction: stacking sub-scales creates strong block structure, and the algorithms respond to it by tracking response polarity rather than finding heterogeneous covariance patterns. **The merge path — the thing that would have made the corpus-scale version viable — is the configuration where the substantive premise looks weakest.**

### Timing and scaling — THRESHOLD 4 MET

Wall-clock at N = 500, showing the dominant cost:

| Table | items | RCA (w/ bootstrap) | CCA | Recursive Relationality |
|---|---|---|---|---|
| heard_roch_2022_idss | 12 | 23.1 s | 0.11 s | 7.0 s |
| pppasbpnssedaisw_pedro_2022_panas | 20 | 54.9 s | 0.22 s | 50.9 s |
| disgust_berger2014 | 25 | 81.8 s | 0.10 s | 130.1 s |
| short_dark_triad | 27 | 95.6 s | 0.13 s | not attempted |
| hexaco_ashton_2014_h | 40 | 200.8 s | 0.19 s | **835.0 s** |

**Recursive Relationality is O(N²·p⁴) in item count** — confirmed empirically, with `secs / p⁴` constant at 3.3e-4, 2.6e-4, 3.3e-4, 3.3e-4 across the four item counts. It builds a doubly-recursive difference vector of length `C(C(p,2),2)` per respondent, so item count, not sample size, is the binding constraint. RCA's bootstrap scales roughly as p² (`secs/p²` ≈ 0.13 throughout). CCA is effectively free.

**The N-scaling check is weak and should not be leaned on.** The brief asked for a rerun at N = 1000. `heard_roch_2022_idss` has only **574** usable respondents after complete-case and zero-variance filtering, so what actually ran was 500 → 574, a 15% increase rather than a doubling. Over that narrow range: RCA 23.1 → 28.4 s, Recursive Relationality 7.0 → 9.5 s. The recursive ratio (1.35× time for 1.15× N) is consistent with O(N²), but the range is far too short to confirm it. Class recovery was stable across the two sizes (k = 5 → 4, largest 0.286 → 0.324, η² 0.058 → 0.066), which is mildly reassuring about sampling stability. **A real N-scaling curve was not established.**

**Full-run estimate — 100 tables × 10 measures × 3 replicates at N = 500**, costed against the actual item-count distribution of the 584 eligible tables (median 11 items, IQR 8–20) rather than a single representative table:

- Recursive Relationality: ~1.85 h per replicate
- RCA with bootstrap: ~1.04 h per replicate
- 8 remaining measures at ~1 s each: ~0.6 h total
- **Total ≈ 9.3 hours single-core, ≈ 1.2 hours on 8 cores.** Tractable.

The caveat that matters more than the total: **cost is entirely tail-driven.** A 40-item table costs 42 minutes for its 3 recursive replicates; an 11-item table costs 15 seconds — a 175× spread. Only 8.4% of eligible tables have ≥30 items, but they would dominate the bill. Cap item count or budget the tail explicitly.

**Deliverable:** `scouting/scout_task4.csv`.

---

## Task 5 — Source-data scout for IRW ingestion

### Is any of it already in IRW?

Searched `table`, `doi`, `reference`, `construct_name` and `measurement_tool` across all 2,949 tables.

- **GSS: zero hits.** No match on "General Social Survey", "GSS", or "NORC". None of the eight GSS modules is in IRW.
- **ANES: one hit,** `polca_election` — but its description points to the **2000** National Election Studies, not the 2012 ANES the paper uses. Not the same data.
- No hits for "Boutyline", "DiMaggio", "music taste", or "occupational prestige".

So **none of the nine modules is currently in IRW**, and all nine are candidates on their own merits.

### Licensing — this is the blocker

**GSS (8 of the 9 modules).** NORC's terms state: *"No part of the contents of NORC websites may be reproduced, stored, or transmitted in any form or by any means, electronic or mechanical, in whole or in any part, without the express written consent of NORC."* There is **no** Creative Commons or public-domain designation. Free-to-download is not the same as free-to-redistribute, and IRW ingestion is redistribution. This does not block *analysis*; it blocks *reposting* to Redivis without written consent from NORC.

**ANES (1 module).** Public-use data are free but gated behind registration. The precise reuse and redistribution terms could **not be confirmed** — `electionstudies.org` returns HTTP 403 to automated fetches (see Open items). Recorded as unresolved rather than guessed.

### Item-level availability and scoring

Item-level responses are available for both GSS and ANES public-use files (they are respondent-level microdata, not scale scores), so the IRW structural requirement is met. One wrinkle the brief anticipated: **Occupational Prestige is a 1–100 rating**, which is a continuous/near-continuous response rather than a Likert item. Note that IRW already contains comparable wide-category tables (the pilot drew `test_taking_much_2025_cm` at `n_categories = 101`), so this is a precedent that exists rather than a novel problem.

### Online Appendix E

Appendices A–F are cited throughout the paper as available at `http://smr.sagepub.com/supplemental/`. That is a **stale generic URL** — it does not resolve to this article's supplement. The supplement is reachable from the article landing page at `journals.sagepub.com/doi/10.1177/00491241211031273` (SAGE moved supplemental material there), but retrieving it requires institutional access and was not attempted in this run. **Without Appendix E we cannot reproduce their exact module boundaries**, only guess at them from the Table 8 descriptions.

### Recommendation per module

| Module | Source | Recommendation |
|---|---|---|
| Music tastes | 1993 GSS | **Skip** — blocked on NORC redistribution consent |
| Science, religion, spiritualism | 1988 GSS | **Skip** — same |
| Occupational prestige | 1989 GSS | **Skip** — same, plus 1–100 response handling |
| Economic attitudes | 1996 GSS | **Skip** — same |
| Mental health causes | 2006 GSS | **Skip** — same |
| Job ideals | 2016 GSS | **Skip** — same |
| Government spending | 2016 GSS | **Skip** — same |
| Trust in institutions | 2016 GSS | **Skip** — same |
| Political attitudes | 2012 ANES | **Ingest-with-caveats** — pending confirmation of ANES redistribution terms |

The blocking reason for all eight GSS modules is a single licensing question, not eight separate ones. If someone is willing to email `GSS@norc.org` for written consent, all eight unblock together, and GSS is a genuinely valuable addition to IRW — large, long-running, publicly documented, and heavily cited. That email is the highest-leverage action in this whole section.

---

## Open items

1. `models_w_noise.RDS` — download from Google Drive timed out (curl exit 28). Needed only to reproduce their noise-robustness predictions, not for meta-feature computation.
2. ANES reuse/redistribution terms — `electionstudies.org` returns HTTP 403 to automated fetch. Needs a human to read the DUA.
3. Online Appendix E (per-module variable construction) — behind SAGE institutional access; not retrieved.
4. `id`-comparability verified on 4 of 114 merge groups.
5. `mfe` is **archived from CRAN** (installed here from the source archive, with `clusterCrit`, `ECoL`, `infotheo`, `rrcov` as deps). Any pipeline depending on `evaluate.metafeatures()` inherits an archived dependency for the `PercentOut` feature alone.

---
---

# Amendment (2026-08-27, follow-on)

Appended, not rewritten. Everything above stands as originally written. This
section revisits threshold 2 on a defensible comparison and establishes the code
findings rigorously.

## Task A — Threshold 2 on a fairer footing

### A1. Like-for-like dispersion — THE THRESHOLD FLIPS

The original test compared an IRW **IQR** (n=29) against a Table 9 **range** (n=9). That
is not like-for-like. All three comparisons, on the 13 comparable features
(`PercentOut` excluded as a scale artifact — see Task B1):

| Feature | IRW IQR | T9 IQR | IQR vs IQR | IRW range | T9 range | range vs range | IQR vs range |
|---|---|---|---|---|---|---|---|
| OverallKurt | 0.821 | 0.695 | **PASS** | 8.47 | 2.30 | PASS | fail |
| OverallRightKurt | 0.120 | 0.301 | fail | 0.902 | 1.06 | fail | fail |
| OverallSD | 0.756 | 0.529 | **PASS** | 26.5 | 1.60 | PASS | fail |
| Skewness | 0.395 | 0.547 | fail | 3.74 | 1.31 | PASS | fail |
| VarColPC1 | 17.4 | 5.68 | **PASS** | 2140 | 46.7 | PASS | fail |
| SkewColPC1 | 0.466 | 0.603 | fail | 2.68 | 1.63 | PASS | fail |
| SkewColPC2 | 0.340 | 0.522 | fail | 1.27 | 2.06 | fail | fail |
| Mean_Row_Corr | 0.168 | 0.223 | fail | 0.654 | 0.564 | PASS | fail |
| Row_Corr_Kurtosis | 0.551 | 0.735 | fail | 2.40 | 3.94 | fail | fail |
| CorrRightKurt | 0.704 | 0.662 | **PASS** | 11.5 | 2.67 | PASS | fail |
| Mean_Col_Kurtosis | 0.850 | 0.412 | **PASS** | 8.66 | 3.61 | PASS | fail |
| Mean_SD_Ratio | 0.0706 | 0.082 | fail | 0.456 | 0.436 | PASS | fail |
| intrinsicDimProp | 0.0965 | 0.116 | fail | 0.389 | 0.371 | PASS | fail |
| **Totals (of 13)** | | | **5** | | | **10** | **0** |

**On the like-for-like IQR-vs-IQR test, 5 of 13 features pass — meeting the "at least five" bar exactly.**

### A2. Excluding the high-category tail

Two of 29 matrices have `n_categories > 20` (`test_taking_much_2025_cm` and
`climatechange_geiger_2025`, both 101-category rating scales). Dropping them (n = 27):

| Comparison | all 29 | excl. n_cat>20 (n=27) |
|---|---|---|
| IQR vs IQR | 5 | **5** (same five features) |
| range vs range | 10 | **6** |
| IQR vs range | 0 | 0 |

Two things follow, and they point in opposite directions:

- **Your prediction about the range comparison was right.** It drops 10 → 6 when the two
  wide-category tables are removed, confirming the range advantage was substantially
  tail-driven and that range at n=29 is outlier-sensitive.
- **My "tightly packed middle" claim was wrong.** The IQR-vs-IQR pass count is *unchanged*
  at 5, and it is the *same five features* (OverallKurt, OverallSD, VarColPC1,
  CorrRightKurt, Mean_Col_Kurtosis). The middle of the IRW distribution is genuinely wider
  than the middle of theirs on those five, and that is not an artifact of the tail.

### A3. The N confound

Meta-features recomputed with N capped at 2,000 (matching their ~1,500–2,500 datasets)
instead of 5,000. Only **5 of 29** matrices were actually affected.

| | N cap 5,000 | N cap 2,000 |
|---|---|---|
| IQR-vs-IQR passes (of 13) | 5 | **6** |
| median absolute change in IRW IQR | — | **1.4%** |
| features flipping pass/fail | — | 1 (`intrinsicDimProp`) |

The cap is not driving the result. Median IQR shift is 1.4%; the only feature to flip does
so *toward* passing. `intrinsicDimProp` (+43% IQR) and `SkewColPC1`/`CorrRightKurt` (+19%)
are the N-sensitive ones, consistent with them being the coarsest/most tail-dependent
features. **The objection is closed: correcting the N confound does not weaken the
comparison, it marginally strengthens it.**

### Reverse-keying as a distribution

| Group | n | min | Q1 | median | Q3 | max | n > 0.5 |
|---|---|---|---|---|---|---|---|
| single | 17 | 0.058 | 0.115 | **0.154** | 0.368 | 0.624 | 2 |
| merged | 2 | 0.014 | 0.138 | 0.262 | 0.386 | 0.510 | 1 |

By method (singles): CCA median 0.149 (max 0.624), RCA 0.306 (max 0.391),
Recursive Relationality 0.115 (max 0.611).

The distribution is **right-skewed, not centered** — most runs sit at 0.06–0.15, with a
tail reaching 0.62. A median of 0.154 understates the risk in the same way it overstates
the typical case: RCA is the most *consistently* contaminated (median 0.306, but a tight
range), while CCA and Recursive Relationality are usually clean with occasional bad
failures. Worth watching, not disqualifying.

### Threshold 2: status changed

**Threshold 2 passes under the like-for-like test, and the pass is robust.** 5 of 13 on
IQR-vs-IQR with all 29 matrices; the same 5 excluding the high-category tail; 6 of 13 at
N = 2,000. The original MISSED verdict was an artifact of comparing an IQR against a range.

Per the brief I am stopping here rather than revisiting the vignette design. Reporting the
status change and leaving the decision open. Note the pass is *marginal* (5 against a bar
of 5) and rests on five features of which two — `OverallSD` and `VarColPC1` — are
scale-dependent and therefore sensitive to the response-category mix of whatever corpus
slice is used.

**Data:** `scouting/A1_dispersion_all29.csv`, `A2_dispersion_cat20.csv`,
`A3_Nsensitivity.csv`, `scout_metafeatures_N2000.csv`.

## Task B1 — Static code audit

The Drive artifacts are not in version control and the GitHub repo (`a617939`) contains
only a README, so there is no upstream commit SHA to pin. **Findings are pinned by SHA-256
of the files as retrieved 2026-08-27:**

| File | SHA-256 |
|---|---|
| `metafeature_RCA.R` (494 lines) | `167751976405b6870dcd8e0166d4ef1c9afd948b4c3ddb8a665bb238a4c231cf` |
| `f_outer.cpp` (46 lines) | `a6a854fa96704521e39abb7dba44645d674834832f933a99cd55c4c670618498` |
| `metafeature_models_cleaned.RDS` | `38d7b0529123adfd4a61d244d60617b09eb12dfa22575554b4828eab9da21033` |

Reproduction script: `scouting/b1_audit.R`.

### Finding 1 — `num_vars = 10` hardcode

**Location:** `metafeature_RCA.R:213` — `evaluate.metafeatures <- function(df, num_vars = 10)`.
Used at **line 262** (`intrinsicDemnsionalityProp <- intrinsicDemnsionality/num_vars`) and
**line 268** (`PercentOut = ((nrOut/num_vars) * 100)`).

**Mechanism:** `num_vars` is never inferred from `df`. It is the denominator for exactly two
of the fourteen features. The other twelve are computed directly from the matrix and are
unaffected.

**Demonstration** (400 respondents, synthetic):

| Matrix | Feature | default (`num_vars=10`) | correct (`num_vars=ncol`) | |
|---|---|---|---|---|
| 10 items | intrinsicDimProp | 0.600 | 0.600 | same |
| 10 items | all others | — | — | same |
| **18 items** | **intrinsicDimProp** | **1.000** | **0.556** | **diverges** |
| **18 items** | **PercentOut** | **170.00** | **94.44** | **diverges** |
| 18 items | other 12 features | — | — | identical |

At 18 items the released default returns **PercentOut = 170%** — a percentage exceeding
100, which is impossible by construction and is a self-evident signal of the defect. The
`intrinsicDimProp` failure is quieter and worse: it silently saturates at 1.000.

**The documented workflow always hits this path.** `select.method()` at
`metafeature_RCA.R:282-287` calls `evaluate.metafeatures(data)` with **no** `num_vars`
argument, and the README instructs users to call `select.method()`. Any user running the
released tool on a matrix that is not exactly 10 items wide gets two corrupted features
feeding the method-selection regressions.

### Scoping question — narrow erratum, not broader

**Answer: the defect does not touch the simulation results (Tables 4–7). It is confined to
the empirical application.**

Evidence: **Table 3** (paper p.1852) lists the grid-search parameters — number of schemas,
schema variance, maximum error variance, shift, scaling, probability of inversion.
**Item count is not among them.** The text at p.1846 confirms items were fixed: *"the
original—unshifted and unscaled—schema (i.e., a vector of values for each of the 10 items)
is drawn from a normal distribution for each item."* All 198,000 simulated datasets have
exactly 10 items, so `num_vars = 10` is *correct* on the simulation side, and the fitted
regressions in `metafeature_models_cleaned.RDS` are unaffected.

**Further — Table 9 itself appears to have been computed with the correct denominator.**
Its intrinsic-dimensionality column is 0.429, 0.357, 0.333, 0.500, 0.200, 0.286, 0.313,
0.571, 0.333. Had `num_vars` been left at 10, every value would be a multiple of 0.1. They
are not: they resolve to 3/7, 5/14, 1/3, 1/2, 1/5, 2/7, 5/16, 4/7, 1/3 — denominators
consistent with actual module item counts. So the authors passed `num_vars` explicitly for
the empirical table.

**Net:** this is a defect in the *released tool* as handed to other researchers, not
(on the available evidence) an error in the paper's own published numbers. That is the
narrower of the two possibilities, and it should be stated that way.

### Finding 2 — `OverallRightKurt` on a logical vector

**Location:** `metafeature_RCA.R:233` — `overall_right_kurt = Kurt(df > mean(df, na.rm = T))`.
The intended helper `right_half_kurt` is defined at **line 217** and, by exhaustive grep of
the file, **never called** (single occurrence).

**Mechanism:** `df > mean(df)` is a logical matrix. `DescTools::Kurt` coerces it to 0/1, so
the feature is the excess kurtosis of a Bernoulli indicator — a deterministic function of
the proportion above the mean, carrying no information about the shape of the right tail.

**Demonstration** (400 × 12 synthetic):

| | Value |
|---|---|
| as released, `Kurt(df > mean(df))` | **−1.847932** |
| as intended, `right_half_kurt(as.numeric(df))` | **−1.999943** |
| Bernoulli closed form `(1−6p(1−p))/(p(1−p))` at p = 0.4042 | **−1.847452** |

The released value matches the Bernoulli closed form to three decimals, confirming it is a
pure function of `p` and nothing else. This explains the feature's anomalous behaviour
elsewhere: it has the narrowest spread of any feature in the IRW pilot (IQR 0.120) and the
lowest reported simulation overlap in Table 9 (3%) — both consistent with a near-degenerate
quantity.

### Finding 3 — Louvain hardcoded

**Location:** `metafeature_RCA.R:289` — `metaRCA <- function(data, measure = "Recursive Relationality")`.
The signature exposes **only** `data` and `measure`. Community detection is fixed at
**line 345** (`cluster_out <- cluster_louvain(net, weights = E(net)$weight)`) and again in
the ensemble at **line 446**. Grep for `leading.eigen|walktrap|fastgreedy|infomap|spinglass`
over the file returns nothing — no alternative is implemented or reachable.

This matters because the paper itself reports that swapping leading-eigenvector for Louvain
accounted for a meaningful share of RCA's measured improvement over Boutyline's baseline.
A degree of freedom the authors identify as consequential is not exposed to users of the
released tool.

### Finding 4 — `PercentOut` scale

**Location:** `metafeature_RCA.R:268`, `PercentOut = ((nrOut/num_vars) * 100)`, with `nrOut`
from `mfe::statistical(..., "nrOutliers")` at line 259. `mfe`'s documentation defines
`nrOutliers` as *"Number of attributes with outliers values"* — an integer count of columns.

**Mechanism:** `PercentOut` is therefore an integer count divided by an item count, times
100. It can only take values on the lattice {0, 100/p, 200/p, …} and lies in [0, 100].

**Demonstration:** the smallest attainable non-zero value across any plausible item count
(p = 40) is **2.5**. Table 9's published `PercentOut` column is
0.000, 0.002, 0.002, 0.004, 0.006, 0.007, 0.008, 0.022, 0.048 — **every non-zero entry is
at least 50× smaller than the smallest value the released formula can produce.**

**What transformation would produce those magnitudes?** A *cell* proportion rather than a
*variable* proportion. Computing the fraction of individual cells that are Tukey outliers
across 12 IRW tables gives a range of [0.000, 0.065] with median 0.018 — the same order of
magnitude as Table 9's [0.000, 0.048]. That is consistent with Table 9's column being the
proportion of outlying *observations*, despite the column being headed "Percent of
Variables with Outlier Observations."

**Unresolved hypothesis — stated as such.** Table 9's intrinsic-dimensionality column is
consistent with the released code correctly parameterised (Finding 1), while its
`PercentOut` column is *inconsistent with the released formula on any parameterisation*.
Two features in the same table appear to come from different computations. One reading is
that the published empirical meta-features were not produced by the released
`evaluate.metafeatures()`. **That claim is not established here and should not be asserted
on this evidence alone.** What would settle it: reconstructing the nine matrices from
Appendix E's item selections and running the released function against published Table 9
(Task B2, currently blocked). A cheaper partial test: if the authors confirm which
`PercentOut` definition was used, the discrepancy either resolves to a labelling/scaling
slip in the table or does not.

## Task B2 — Blocked, not attempted

Gate not satisfied. Online Appendix E (per-module variable construction) is required and
remains unreachable: `journals.sagepub.com/doi/suppl/10.1177/00491241211031273` returns
**HTTP 403**, as does the paper's own cited `smr.sagepub.com/supplemental/`. GSS item-level
data is separately blocked pending Task C.

Per instruction, **no reconstruction was attempted from guessed module boundaries.** A
failed reproduction from the wrong item selections would be worse than none, and would
contaminate the Finding 4 hypothesis above rather than testing it.

## Closed open items

- **`models_w_noise.RDS`** — **resolved.** The file had in fact downloaded (96.7 MB, valid
  gzip); the earlier "timeout" killed the shell after `curl` completed. However it **fails
  to load** via `readRDS` (the companion `metafeature_models_cleaned.RDS` loads fine and
  contains 9 fitted `lm` models, one per distance measure, each with an intercept plus the
  14 meta-features). Whether the file is corrupt in transit or in a non-RDS format is
  undetermined; it is needed only for the noise-robustness predictions.
- **`mfe` archived from CRAN** — **resolved with a verified drop-in.** `mfe`'s rule
  (`mfe/R/statistical.R:353-360`) uses `stats::quantile` (type 7), not `boxplot.stats`
  hinges. This four-line replacement reproduces it exactly on 15 random cases:

  ```r
  nrOutliers <- function(m) sum(apply(m, 2, function(x) {
    qs <- stats::quantile(x); iqr <- (qs[4] - qs[2]) * 1.5
    (qs[2] - iqr) > qs[1] | (qs[4] + iqr) < qs[5]
  }))
  ```
  No archived dependency is required. (A naive `boxplot.stats` implementation does **not**
  match — it disagreed on 1 of 8 test cases.)
- **`Permission via Email` count** — corrected: **111** tables, not 106. The exact-string
  count is 106; a further 5 carry it in combination (e.g. `Permission via Email, CC BY 4.0`).
- **`id`-comparability coverage** — extended from 4 to **24 of 114 merge groups (21%)**.
  A further 20 groups were sampled across group sizes (2–7 member tables, capped at 4
  tables tested per group, 68 tables fetched) and **all 20 verified at 100% minimum
  pairwise id overlap**. Combined tally: 23 VERIFIED, 1 PARTIAL (`alsuhibani_2022`, whose
  ids are namespaced by sub-sample). No new failure modes appeared.

  This raises confidence that id-stability is the rule and disjoint sub-samples the
  exception, but note the sample was restricted to groups with `min_n_participants <=
  20000`, so very large multi-wave studies remain untested. The pairwise-overlap gate
  should still run inside any real pipeline rather than being assumed —
  `silva_2018` is the standing warning: 100% overlap at 3 tables, 0% at 5.

  Data: `scouting/id_verification_extended.csv`.

## Revised threshold table

| # | Threshold | Original verdict | Revised verdict |
|---|---|---|---|
| 1 | Corpus size | MET | **MET** (24/114 groups now id-verified) |
| 2 | Feature spread | MISSED | **MET (marginal)** — 5/13 like-for-like, robust to tail removal and N cap |
| 3 | Non-degeneracy | MET | **MET** (η² right-skewed, median 0.154 singles) |
| 4 | Compute | MET | **MET** |

**All four thresholds are now met.** The original NO-GO rested on threshold 2, and
threshold 2 failed only under a comparison that was not like-for-like. Stating that
plainly, per instruction, and leaving the decision on the vignette open rather than
reopening the design.
