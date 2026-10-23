# Handoff: worked example for the rankings standard page (`rankings_standard.qmd`)

Ben's decision, 2026-10-15 (final; supersedes an earlier draft of this file): the
rankings-family vignette will be *how much of a ranking is shared*, run across the
whole family once the bulk of tables is in. The standard page gets a **one-table
mini analysis** of a different question: *where in a ranked list the information
sits*. This note is for whoever builds step 6 (site) of
`oneoff/rankings-eurovision-tables-2026-10-15/BUILDOUT_PROMPT.md`.

## What the mini analysis shows

One table, `eurovision_2016_final` (live on `irw_rank:93f5` v1.0; 210 jurors, 26
acts, each juror ranks every act except their own country's). One figure, one
short paragraph. Two curves over ballot position 1..26:

1. **Agreement by slot.** For each position p, the entropy of which act occupies
   it across jurors, divided by log(26). It is U-shaped: jurors agree about the
   top and the bottom and disagree in the middle (pilot: 0.84 at slot 1, ~0.98 in
   the middle, 0.86 at slot 26).
2. **What a top-down model can use.** Fit Plackett–Luce (PL) on training jurors;
   on held-out jurors score the mark at each position against the uniform choice
   among the acts not yet marked, and report exp(mean log-lik difference) − 1 per
   position. Forward PL: positive at the top (+0.25 at slot 1), falling to below
   chance over the last few slots (−0.04 to −0.11). Fit PL on the *reversed*
   lists (worst first) and score the same way: the mirror image (bottom +0.08 to
   +0.28, top −0.23). The two fits agree on the ordering of the acts (Spearman
   .93).

The sentence the reader takes away: positions are not equally informative, a
ranking is built from both ends, and a top-down model reads only one of them.
That is why the table stores positions exactly as recorded and leaves
compression to `irw_rank_compress()`. Say it is a one-table illustration, not a
claim about the family; the vignette (and a later follow-up on complete
rankings) carries the claims.

Pilot numbers for all 15 Eurovision shows are in `README.md`,
`position_results.json` and `reverse_results.json` here; code `position_gain.py`
and `reverse_pl.py` (Python, 5-fold CV, ridge 0.01 on log-worths).

## How to build it

- R, in the site's credential-skip pattern, pinned to the shard version.
  `PlackettLuce::PlackettLuce()` for the fit (`as.rankings(..., input =
  "orderings")`); reverse = the same call on `rev()`-ed orderings. Held-out
  position-wise scoring is a few lines on the fitted worths (neither package
  scores new ballots). PlackettLuce and PLMIX are installed on Ben's machine
  (CVXR 1.0-15 from the CRAN archive; current CVXR needs Rust).
- Ballot prep to show: drop `(overvote)`/`(write-in)` rows (none in Eurovision),
  `irw_rank_compress()`, then ordered item lists. Keep a juror's own country out
  of their choice set if the helper makes that easy; the pilot left it in, which
  penalises both directions alike.
- Scoring stat: call it a likelihood-ratio gain against chance, not IMV; the
  binary equivalent-coin IMV does not apply to a choice among 26 (a uniform
  26-way choice sits below log 0.5). One sentence suffices.

## Out of scope for the standard page

Consensus vs camps (PL vs 2-class mixture, Burlington vs Alaska), truncation
depth on election ballots, Mallows / two-ended models, cross-family IMV
comparisons: all vignette material. Do not put it on the standard page.
