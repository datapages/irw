# Rankings-family vignette, draft run (2026-10-22)

Question A from the 10-15 pilot — *how much of a ranking is shared* — run over the whole live
family. Draft artifact for Ben: <https://claude.ai/artifact/K8GGhagA2QZkUoNYXDiAR6>

The vignette itself is still held on the "≥20 jurisdictions" clause of datapages/irw#357. This
is a read-and-react draft, not the site page; no branch, no PR, nothing in `src/` touched.

## What was run

Data: `irw_rank:93f5` **v1.5** (437 tables, 36,837,920 rows), the release of 2026-10-22.

1. `stage_rank.py` — staged all 437 tables into `~/.cache/irw_rank_vignette/tables/`. Every one
   was **salvaged** from CSVs already built on this machine (most of them in other sessions'
   `/tmp/claude-1000/.../scratchpad/` directories, which are ephemeral); nothing had to be
   exported from Redivis. Each table's row count was checked against the Redivis listing —
   437/437 match, 36,837,920 rows. See `staging_report.csv`.
2. `compute_a.py` — per table: held-out Plackett–Luce gain over uniform (consensus) and the
   gain of a 2-class PL mixture over it (camps), 5-fold CV. Method and code imported unchanged
   from `../rankings-vignette-pilot-2026-10-15/pilot.py`. **417 fitted, 20 skipped** (fewer
   than 3 candidates: 19 Minneapolis contests + Takoma Park ward 4), 0 failed. Tables over
   20,000 ballots are fitted on a seeded 20,000-ballot sample (293 of them); `--cap 0` reruns
   uncapped. Results in `results_a.json`; log in `run_full.log`. ~38 CPU-minutes, one core.
3. `make_widget.py` → `build/widget.html`, the Plackett–Luce explainer at the top of the
   page: sliders over the Burlington worths, a clickable ballot, and the model written out
   (choice probability, ballot likelihood, the gain statistic, the mixture, and the
   estimation facts — ridge, folds, EM, what Luce's axiom assumes away).
4. `make_figs.py` → `build/` (SVG fragments + `stats.json`), `build_page.py` → the artifact
   HTML, saved here as `rankings_draft.html` (the published copy; the artifact is republished
   from the scratchpad copy of the same file). Rerun both after any recompute.

Run everything single-threaded and niced — numpy's BLAS otherwise fans out over ~13 cores:

```
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 NUMEXPR_NUM_THREADS=1 \
  nice -n 19 ionice -c3 python3 compute_a.py
```

## Verification

- **Staging:** 437/437 tables, every row count equal to Redivis. No fetch needed.
- **Against the pilot:** the 20 tables of the 10-15 pilot refit uncapped
  (`results_pilotcheck.json`) agree on the consensus gain to better than 0.01 — Burlington
  0.4798 (pilot 0.48), NYC D49 0.1361, Alaska HD10 0.0418, Eurovision 0.046–0.10.
- **Not verified:** the ballot cap's effect (planned capped-vs-uncapped check on three large
  tables was not run), and the EM restart noise — the one measurement we have is that two runs
  of the same code gave Alaska HD10 camps 0.129 and 0.145. `wobble.py` does the reseed check
  properly and has not been run to completion.

## Findings

- Elections median consensus 0.155 (357 contests); Debian 0.169; Eurovision juries 0.068 and
  the least consensual family in the corpus; surveys highest at 0.50, which is an artifact of
  long candidate lists, not agreement.
- **Camps:** a second order beats one in 140 of 405 tables, and they are concentrated —
  124 of 279 New South Wales districts, against 7/69 Minneapolis, 6/33 Debian, 0/6 Eurovision
  finals. NSW is optional preferential, ~6 candidates, 2.4 marks per ballot, party blocs.
- **Open methodological choice (Ben's):** the consensus gain against a *uniform* baseline
  mixes field concentration with agreement about order. Minneapolis 2013 (35 candidates) scores
  2.75 for that reason; within elections candidate count correlates +0.17 with the gain.
  Options are a marginal-popularity baseline, or splitting the gain by position (the question-B
  machinery from the pilot). Nothing was changed without his call.

---

# Second run (2026-11-02): both shards, 1,018 tables

Rerun after the FairVote backlog landed and the family got its second shard. Same statistic,
same code path; nothing in the method was changed. Artifact republished to the same URL.

## What was run

Data: `irw_rank:93f5` **v2.1** (950 tables) + `irw_rank_2:byyj` **v2.1** (68 tables) =
1,018 tables, 99,224,348 rows, both released 2026-11-02.

1. `stage_rank.py` now reads both shards (`SHARDS`). All 581 new tables were **salvaged** from
   CSVs still on disk (the 11-01 upload session's staging directories); 1,018/1,018 row counts
   match Redivis, nothing fetched. `staging_report.csv` rewritten.
2. `design_meta.json` regenerated from `src/data/rankings/design_tables.csv` (1,019 rows).
3. `compute_a.py`, resumable: the 437 October results were kept as computed then (backup in
   `results_a_v1_5_backup.json`), the 581 new tables fitted. Log `run_full_v2_1.log`.
   **793 fitted, 225 skipped (<3 candidates), 0 failed**, 441 capped at 20,000 ballots.
4. `make_figs.py`: axes widened (consensus to 25, camps to 2.0, candidates to 90) for the new
   extremes; `stats.json` now carries per-group counts, non-NSW election stats, consensus
   medians by candidate-count band and the Spearman correlations, so the prose reads numbers
   instead of typing them. `build_page.py` prose rewritten around those stats.

## Findings that changed

- **The candidate-count confound is now the main result.** Within elections the rank
  correlation between candidates and consensus gain went from +0.17 (437 tables) to **+0.52**
  (718 contests); median gain by field size: 3 candidates 0.065, 4–5 0.13, 6–9 0.16, 10+ 0.51.
  The top of the corpus is `rcv_easthampton_20211102_mayor` at **20.5**: 63 written-in names
  coded as candidates. Cambridge STV (23–45 candidates) scores 0.9–4.0. The baseline change
  (marginal-popularity, or split by position) should happen before the page is written.
- **Camps live in two places:** NSW 124/279 as before, and **Alaska 25/41** (top-four generals,
  two Republicans splitting a bloc; camps median 0.086 > consensus 0.053). The US cities show
  almost none: NYC 3/86, SF 1/61, Oakland 3/43, Minneapolis 7/69; outside NSW 56/438 in all.
  Corpus total 190/777.
- **Ballon d'Or juries** (3 tables, top-5/top-10 of 25–30) score 1.6–2.7, Eurovision stays at
  0.068: the same concentration mechanism.
- **225 two-candidate contests** had nothing to rank (Alaska 74, NYC 40, Oakland 30, SF 21 ...).
- #357's "≥20 jurisdictions": elections now span NSW + 38 US groups (136 distinct
  `jurisdiction` values), so the clause is met on any reading.

## Not verified (unchanged)

Ballot-cap effect, EM restart noise (`wobble.py` still not run), and the October results were
not refitted under a fresh seed.
