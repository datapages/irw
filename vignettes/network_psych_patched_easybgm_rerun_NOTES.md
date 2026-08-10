# Prep notes: re-running Option A against the patched `easybgm`

Status as of 2026-08-10: **prepared, not run.** This file is the single
place documenting what "run #7" actually means and what's left to do.

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

## What's still needed before this can actually run

1. **Pin the package** (not done — modifies the tracked `renv.lock`, held
   for an explicit go-ahead):
   ```r
   renv::install("KarolineHuth/easybgm@d5a3da0a50")
   renv::snapshot()
   ```
2. **Run the batch:**
   ```bash
   RERUN_AGAINST_PATCHED_EASYBGM=TRUE REDIVIS_API_TOKEN=$(cat ~/.redivis_api_token) \
     Rscript vignettes/network_psych_compute.R
   ```
   Runtime: the vignette's own text puts a full Option A batch at "well
   under two [hours]" (contrast with Option B/`bgms`'s 7-27 hours) — this
   is the smaller of the two "massive rerun" categories discussed for this
   vignette, not a multi-hour ordeal.
3. **Diff the two result sets**
   (`vignettes/network_psych_data/network_psych_results.rds` vs.
   `vignettes/network_psych_data_patched_rerun/network_psych_results.rds`):
   confirm `summary` tables match (mod MCMC-seed noise) and no table's
   evidence category classification flips.
4. **If confirmed identical:** simplify the Limitations bullet about the
   redirect bug (it's fully resolved, not just worked around), and decide
   whether to keep `network_psych_data_patched_rerun/` as a small audit
   artifact or delete it once the comparison is documented.
