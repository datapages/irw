# Rankings-family vignette pilot (2026-10-15)

Two candidate central questions for the `rank` family vignette, piloted on the 21
local tables (6 FairVote reference contests in `src/data/rankings/examples/`, 15
Eurovision juror shows in `oneoff/rankings-eurovision-tables-2026-10-15/`).
Python (numpy/scipy); PlackettLuce for R was not installable without `libmpfr-dev`.

- `pilot.py` — question A: held-out Plackett–Luce (PL) gain vs uniform on the next
  mark, 5-fold CV; 2-component PL mixture gain over single PL; plus truncation
  shares. Output `results.json`.
- `position_gain.py` — question B: entropy of the occupant of each position
  (normalised by log n), held-out PL gain for the p-th mark given earlier marks,
  Spearman of worths from top-k training vs full. Output `position_results.json`.
- Gain stat = exp(mean log-lik difference per choice) − 1, a stand-in for IMV on
  K-way choices (the binary equivalent-coin IMV does not apply below log 0.5).
- Ballot prep: (overvote)/(write-in) rows dropped, first mark of a duplicated item
  kept, gaps ignored. Ridge 0.01 on log-worths.

## Findings (pilot, not verified)

A. Consensus vs heterogeneity: Burlington 2006 consensus 0.48, mixture adds 0.04;
NYC D49 0.14 / 0.008; Alaska HD10 0.04 / 0.13 (heterogeneity dominates, 3 candidates,
polarised); Eurovision shows 0.05–0.10 per choice, mixture (bloc structure) adds
only 0.005–0.025.

B. Position: Eurovision entropy is U-shaped (jurors agree on the top AND the
bottom, disagree in the middle), yet PL held-out gain falls monotonically and goes
negative at the last 3–5 positions (−0.1 to −0.27). A top-down sequential model
cannot use bottom-of-ranking agreement. Candidate check: reverse PL (rank the worst
first) or Mallows. Elections: Burlington gain high at positions 1–2 and 5, low at
3–4; Alaska position-2 gain is negative (first choices mispredict second choices).
Worths from top-k: k≈8 of 26 recovers Spearman .9 with the full fit.

## Reverse PL check (`reverse_pl.py`, Eurovision jurors with >= n-1 marks)

Forward PL: top-5 positions gain +0.16 to +0.42, bottom-5 (excluding the forced
last) −0.04 to −0.11, in all 15 shows. Reverse PL (worst ranked first): top-5
−0.23 to −0.33, bottom-5 +0.08 to +0.28. Worth orderings from the two fits
correlate .93–.98. Jurors build a ranking from both ends; one worth vector read in
one direction cannot serve both. Caveat: a juror's own country is unranked, and
both fits keep it in the choice set, which penalises both directions alike.

## Decision (Ben, 2026-10-15, final)

Vignette = question A (how much is shared), run across the whole family and held
until the bulk of tables is in: it yields two numbers per table from any ranking
however short. Question B (information by position) needs complete rankings, so
it becomes the one-table mini analysis on the rankings standard page:
`STANDARD_PAGE_EXAMPLE.md`, pointer in
`oneoff/rankings-eurovision-tables-2026-10-15/BUILDOUT_PROMPT.md`.
