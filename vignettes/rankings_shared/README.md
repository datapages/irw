# How much of a ranking is shared — draft vignette (A)

Working branch for the rank-family vignette. **Nothing here renders yet**: there is no `.qmd`,
the page is not in `_quarto.yml`, and the vignette itself is still held on the "≥100 election
tables over ≥20 jurisdictions" clause of datapages/irw#357. What this directory holds is the
compute, the results and a draft of the page as a standalone artifact, under version control
instead of in `oneoff/`.

Draft artifact (private): <https://claude.ai/artifact/K8GGhagA2QZkUoNYXDiAR6>

## The question

Two numbers per ranking table, from `irw_rank` v1.5 (437 tables, 36,837,920 rows):

- **consensus gain** — fit one Plackett–Luce order on four-fifths of the ballots, predict each
  mark of the held-out fifth, and compare with a uniform pick among the candidates not yet
  marked. The statistic is `exp(mean log-likelihood difference per mark) − 1`.
- **camps gain** — the same quantity for a two-class Plackett–Luce mixture, scored against the
  single-order fit, where there are at least 150 ballots.

417 tables fitted, 20 skipped (fewer than 3 candidates), 0 failed.

## Layout

| | |
|---|---|
| `pilot/` | the 2026-10-15 pilot: `pilot.py` is the method (PL fit, mixture, gain stat) and everything else imports it unchanged. Also the question-B work that became the Eurovision example on `rankings_standard.qmd`. |
| `stage_rank.py` | stages the family into `~/.cache/irw_rank_vignette/tables/` (not in the repo); reconciles every row count against Redivis. `staging_report.csv` is its output. |
| `compute_a.py` | the driver. Resumable, `--cap N` (default 20,000 seeded ballots), `--only`. Writes `results_a.json`; `run_full.log` is the 437-table run. |
| `make_widget.py` | the Plackett–Luce explainer at the top of the page: sliders, a clickable ballot, and the model written out. |
| `make_figs.py` | SVG fragments and `stats.json` into `build/` (untracked). |
| `build_page.py` | assembles `rankings_draft.html`, the artifact. The HTML is **not** tracked — the repo ignores `*.html` — but it rebuilds byte-identically from what is here. |
| `wobble.py` | reseed check on the EM noise. **Not yet run.** |

Rerun order: `compute_a.py` → `make_widget.py` → `make_figs.py` → `build_page.py`. Run single-
threaded and niced; numpy's BLAS otherwise fans out over ~13 cores:

```
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 NUMEXPR_NUM_THREADS=1 \
  nice -n 19 ionice -c3 python3 compute_a.py
```

`DRAFT_RUN.md` is the run report for 2026-10-22: what was staged, what was verified, findings.

## Open before this becomes a page

1. **The baseline.** The uniform baseline mixes field concentration with agreement about order —
   Minneapolis 2013, with 35 candidates, scores 2.75 for that reason; within elections the
   candidate count correlates +0.17 with the gain. The alternatives are a marginal-popularity
   baseline or splitting the gain by position. Ben's call; nothing was changed without it.
2. **Python or R.** The compute here is Python, importing the pilot's own vectorised PL. The site
   convention is `<name>_compute.R`, and `rankings_standard.qmd` already uses the `PlackettLuce`
   package for the Eurovision example — but that package is not in `renv.lock` (it needs CVXR),
   so it is run by hand and cached, and neither it nor PLMIX scores held-out ballots. A port
   would mean rewriting the held-out scorer and the mixture in R.
3. **#357.** Whether "≥20 jurisdictions" means jurisdictions or contests. `data/rankings/
   design_tables.csv` records 114 distinct `jurisdiction` values, each NSW district its own.
4. **Limitations prose** — Ben's, per the vignette skill.
5. **Not verified:** the ballot cap's effect (capped vs uncapped on three large tables), and the
   EM restart noise (two runs gave Alaska HD10 camps 0.129 and 0.145 — read nothing into camps
   differences below ~0.02).

When it does become a page it should split into `vignettes/rankings_shared.qmd` plus a compute
script and a `rankings_shared_data/` directory, which is the convention everything else in
`vignettes/` follows.

The original working copy is `oneoff/rankings-vignette-draft-2026-10-22/` (outside git); this
branch is now the canonical copy, and `compute_a.py` here imports `pilot/` from inside the
directory rather than from its sibling in `oneoff/`.
