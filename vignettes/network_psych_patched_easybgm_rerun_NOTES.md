# Prep notes: re-running Option A against the patched `easybgm`

Status as of 2026-08-10: **DONE.** Pinned, run, diffed, written up in
`network_psych.qmd` (Data and methods + Limitations). Confirmed: the
redirect bug had zero effect on any result reported in this vignette,
patched or not. This file is kept as a record of the reasoning and the
verification approach, not an open TODO.

## What the bug actually is, and why it doesn't affect current results

Currently pinned: `easybgm` 0.4.0 from CRAN (`renv.lock`). Karoline Huth
fixed a bug upstream (GitHub commit
[`d5a3da0a50`](https://github.com/KarolineHuth/easybgm/commit/d5a3da0a50),
2026-08-03, "update bug always specifying bgms for binary fit"). The diff
is a one-line removal in `R/easybgm.R`:

```diff
     if(package == "BGGM") package <- "package_bggm"
     if(package == "bgms") package <- "package_bgms"
-    if(type == "binary") package <- "package_bgms"
```

**This vignette's Option A never passes `type = "binary"`** — it always
passes `type = "continuous"`, deliberately, as the uniform-GGM comparison
that matches Huth et al.'s own simplifying choice (see `fit_bayesian_edge_evidence()`
in `network_psych_compute.R`). The buggy line only executes when
`type == "binary"`, so it never fires for any call this vignette makes.
**Re-running against the patched package should not change any Option A
result.** The value of doing it anyway is (a) removing a documented but
now-unnecessary workaround/limitation from the vignette, and (b) confirming
nothing *else* changed between CRAN 0.4.0 and the patched commit that could
have side effects (dependency handling, numerical details, etc.) — a
verification pass, not a correctness fix.

## Compatibility check (done, not yet acted on)

At commit `d5a3da0a50`, `easybgm`'s `DESCRIPTION` still declares
`Version: 0.4.0` and `Imports: bgms (>= 0.1.4)` — identical to what's
already pinned (`bgms` 0.1.6.3 in `renv.lock`). **No `bgms` version bump
needed.** This matters because `easybgm`'s later commits (through
2026-08-10 HEAD) migrate to `bgms >= 0.2.0.0`'s S7 fit objects, which would
be a much bigger, riskier change (`fit_ordinal_mrf_edge_evidence()` in
`network_psych_compute.R` calls `bgms` directly and is written against the
current S3-era API) — pin the specific fix commit, not HEAD.

## What's already prepared in `network_psych_compute.R`

- `RERUN_AGAINST_PATCHED_EASYBGM` env-var toggle (default `FALSE`) routes
  all output to `vignettes/network_psych_data_patched_rerun/` instead of
  `vignettes/network_psych_data/`, so a verification re-run can never
  overwrite the currently published, correct 610-table results.
- No change to *what* gets fit (`type` stays `"continuous"` always) — see
  the comment on `fit_bayesian_edge_evidence()`.
- `fit_to_disk()`'s existing per-table caching means an interrupted re-run
  resumes cleanly, same as the original batch did.

## What happened when it ran (2026-08-10)

1. **Pinned:** `renv::install("KarolineHuth/easybgm@d5a3da0a50")` into the
   shared main-repo renv library, then `renv::snapshot(project = ".")`
   targeted at this worktree's `renv.lock`. **Caveat:** a plain
   `renv::snapshot()` rewrote the CRAN mirror URL for every package in the
   lockfile (cosmetic `cran.rstudio.com` -> `packagemanager.posit.co`
   noise, ~40 unrelated lines) — reverted that and hand-patched just the
   `easybgm` entry's `Source`/`Remote*` fields, copied from the installed
   package's own `DESCRIPTION`. Diff the lockfile before committing a
   snapshot; don't trust it blindly on a shared file.
2. **Ran:** `RERUN_AGAINST_PATCHED_EASYBGM=TRUE Rscript network_psych_compute.R`.
   First attempt failed almost immediately (`future` "MultisessionFuture
   interrupted", before any table completed) — likely resource contention
   from running concurrently with the SBM check (item #5) at the same
   time, not a code bug (no OOM evidence, 16GB free). Retried alone and it
   completed cleanly: 902/959 candidates usable (contrast with the
   original 610/660 — IRW gained ~300 candidate tables in the roughly
   2.5 weeks since the original batch ran on 2026-07-23).
3. **Diffed:** the larger/differently-ordered candidate pool shifts
   `furrr`'s per-table seed assignment, which changes which respondents
   get downsampled for any table over the 10,000-respondent cap —
   unrelated to `easybgm`, but a real confound for a naive full-sample
   diff. Restricted to the 514 tables with an identical sample+item count
   in both runs: `strength_a_cor` (computed upstream of `easybgm` entirely
   — from `bootnet`/`mirt`, never touches it) is bit-identical for 501/507;
   the Bayesian evidence-category proportions (the actual target of this
   check) differ by 0.001-0.005 on average — consistent with ordinary
   Monte Carlo noise in `BGGM`'s own sampler, not a systematic shift from
   the patch.
4. **Written up:** Data and methods (Option A description) and Limitations
   in `network_psych.qmd` both updated with the verification result.
   `vignettes/network_psych_data_patched_rerun/` (the full 902-table
   duplicate cache, ~8.3MB) was deleted after the comparison was run and
   documented — it served its one purpose and isn't referenced by the
   `.qmd`; re-run this script again if the comparison ever needs
   reproducing.
