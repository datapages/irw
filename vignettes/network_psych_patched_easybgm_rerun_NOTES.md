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

---

# Addendum 2026-09-10: the *second* easybgm bug (the clustering Bayes factor)

Karoline Huth reports that a colleague found a bug in easybgm's SBM Bayes
factor code and suggests re-running the models against the GitHub version.
This is a **different bug** from the one above (that one was the
`type == "binary"` package-dispatch line, and it never fired for this
vignette). Findings from reading the upstream fix, before deciding whether
to re-run:

**What was fixed upstream.** Commits
[`26e6fa5f`](https://github.com/KarolineHuth/easybgm/commit/26e6fa5f) and
[`03d992b5`](https://github.com/KarolineHuth/easybgm/commit/03d992b5)
(2026-09-07, Nikola Sekulovski, PR #137) rewrite `clusterBayesfactor()`
wholesale. The rewrite is explicitly scoped to **bgms 0.2.0.0**: it assumes
the *shifted*-Poisson prior on the cluster count (`B - 1 ~ Poisson(lambda)`)
and corrects for the fact that 0.2.0.0's SBM summary normalizes `P(B | T)`
over `1:p` separately for each occupied count `T`. Its own documentation
warns: "Use it for fits with this prior and summary convention, not
indiscriminately for historical fits."

**Why the fix does not apply to this vignette's numbers as they stand.**
`network_psych_sbm_check.R` never calls `easybgm` — deliberately (see its
header). It calls `bgms::bgm()` and `bgms::extract_sbm()` directly and forms
the Bayes factor by hand as posterior odds / prior odds, where the prior odds
come from bgms 0.1.6.3's documented **zero-truncated** Poisson prior
(`bgm.Rd`: "Rate of the zero-truncated Poisson prior on the number of
clusters"), giving `PRIOR_P_K1 = 0.582`. The buggy easybgm code path was
therefore never executed here, and the patched replacement cannot be run
against these cached results: it is written for a prior and a summary
convention that bgms 0.1.6.3 does not use.

**Two things in the rewrite that do bear on our numbers.**

1. *What the count means.* The new docs state that `B` counts the clusters
   the model has **available**, including unoccupied ones — "it is not the
   occupied item-cluster count or latent dimensionality." If bgms 0.1.6.3's
   `posterior_num_blocks` has the same meaning, then our `post_p_k1` is
   `P(available blocks = 1)`, which is a slightly different claim from "the
   network is one cluster." This is checkable from
   `posterior_mode_allocations` / the raw allocation draws — but **the SBM
   fits were not cached** (`network_psych_data/fits/` holds the 610 Option A
   fits only, no SBM objects), so checking it means refitting.
2. *What clustering evidence is evidence of.* The new docs add: "Evidence
   for clustering concerns the network's edge structure and is not by itself
   evidence of multidimensionality." That is a direct upstream statement
   about the SBM-vs-eigenvalue mismatch Huth's colleague raised, and is
   arguably a better explanation of it than the BF > 10 / BF > 3 threshold
   argument.

**What a re-run would cost.** bgms 0.2.0.0 moves to S7 fit objects.
`fit_ordinal_mrf_edge_evidence()` in `network_psych_compute.R` calls `bgms`
directly and is written against the S3-era API, so upgrading bgms to re-run
the SBM check also breaks (and would require re-running) the Option B
ordinal-MRF pass. The SBM fits themselves are ~2-3 min/table x 20 tables at
4 chains. Not started — pending a decision.

**Done in the meantime** (2026-09-10, branch `worktree-huth-network-notes`):
terminology corrected to "clustering Bayes factor" throughout, the BF > 3
threshold and the slow-accumulation-of-null-evidence point written into the
SBM section and Limitations, and the weighted-density figure removed.
