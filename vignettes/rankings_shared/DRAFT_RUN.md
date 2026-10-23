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
