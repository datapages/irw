# Validating the vignette's Plackett–Luce against the PlackettLuce package

Run 2026-10-28, on 20 tables of `irw_rank:93f5` v1.5 sampled across the family (4 Debian
resolutions, 2 Eurovision juries, 6 single-contest elections, 3 Minneapolis contests including
the 35-candidate 2013 mayoral race, 3 NSW districts, 3 survey rankings). R 4.6.1,
PlackettLuce 0.4.5.

The vignette fits Plackett–Luce in Python rather than with Heather Turner's `PlackettLuce`
package. This directory checks that the implementation is right, and finds the one place where
the two cannot be made to agree because they are not fitting the same model.

## The finding that shapes everything else: partial ballots

`PlackettLuce` drops unranked items from a ranking. A ballot marking 3 of 6 candidates is
treated as a *complete ranking of those 3*: the choice sets are of size 3, 2, 1. That is right
for its motivating examples (a NASCAR race ranks the drivers who entered, not all 83), and
wrong for a top-k ballot, where the unmarked candidates were on the ballot paper and the voter
declined them. The vignette's model keeps them in the denominator: choice sets of size 6, 5, 4.

Verified, not assumed (`semantics.R`): four ballots marking 2 of 4 items, equal worths.
`logLik` from the package is −2.8, the subset likelihood; the full-set likelihood is −9.9.
Encoding the unmarked items as tied for last does not recover it either (`ties.R`) — the
package fits a Davidson–Luce tie parameter and returns −20. There is no way to express
"available but unchosen" in the package's rankings format.

This matters a great deal, because most ballots in the family are partial: NSW districts
average 2.4 marks of ~6, Minneapolis caps at 3.

## What was compared

Both sides unregularised — ridge 0 in Python, `npseudo = 0` in R, which turns off the package's
default pseudo-rankings. Log-worths centred on zero on both sides.

| | what it tests | tables | worst disagreement |
|---|---|---|---|
| **A** production fit vs package, **complete ballots only** — the one case where the two likelihoods are the same function | the shipped code path | 13 | max \|Δ log worth\| **4.7e-05**, worth 0.005%, logLik 4.5e-07 |
| **B** our optimiser under the package's semantics vs package, **all ballots** | the optimiser on real ragged shapes | 18 | max \|Δ log worth\| **8.3e-04**, worth 0.083%, logLik 8.3e-05 |
| **C** internal: our two semantics on complete ballots | that A is a fair test | 13 | **0** exactly |

The single worst case in B is the 35-candidate Minneapolis 2013 mayoral race, where the package
reaches a log-likelihood 8.3e-05 higher than ours — our L-BFGS-B stops marginally early on a
flat direction. Spread over ~48,000 scored marks that is 2e-09 per mark.

The package **failed outright on two tables**, `rcv_mpls_20211102_mayor` and
`atsusaka_2025_ranking_questions`: *"Network is not fully connected — cannot estimate all item
parameters with npseudo = 0."* Those are exactly the tables where the vignette's ridge is doing
real work: it moves the log-worths by at most 0.0002 on seventeen of the twenty tables, 0.0021
on Minneapolis 2013, and **0.118** on Minneapolis 2021 — the disconnected one. The ridge and
`npseudo` are solving the same problem.

## Does the headline statistic move? No — the software is irrelevant, the semantics are not

`gain_folds_*` refits the same 5-fold split three ways and scores every fold with the *same*
full-set held-out scorer, so only the fit differs:

| table | n | production | our fit, pkg semantics | **package** | sub − pkg |
|---|---|---|---|---|---|
| atsusaka_2024_oakland_rcv_survey | 10 | 0.0378 | 0.0360 | 0.0360 | −0.000004 |
| debian_2019_gr_init_systems | 8 | 0.0633 | 0.0605 | 0.0605 | −0.000002 |
| eurovision_2016_final | 26 | 0.0424 | 0.0414 | 0.0414 | −0.000001 |
| rcv_burlington_20060307_mayor | 6 | 0.4796 | 0.3124 | 0.3124 | +0.000001 |
| rcv_mpls_20131105_mayor | 35 | 2.7539 | 1.0085 | 1.0085 | −0.000013 |
| rcv_nsw_20150328_la_clarence | 8 | 0.0601 | −0.0456 | −0.0456 | +0.000002 |

Swapping our optimiser for the package changes the reported gain in the **fifth decimal place**,
on every table, including the one where the fitted worths differ by 2.2e-03.

Changing the *semantics* changes it enormously, and the damage tracks ballot completeness:
Eurovision and Debian (near-complete) move by 0.001–0.003, Burlington falls 0.48 → 0.31,
Minneapolis 2013 falls 2.75 → 1.01, and NSW Clarence crosses zero, 0.060 → **−0.046** — fitted
worths that predict held-out marks *worse than guessing*. Fitting a top-k ballot as if the
unmarked candidates had never been on the paper does not merely lose precision; it gets the
worths wrong in a direction that is penalised when the ballot is scored as what it is.

The production column also reproduces the published draft (Burlington 0.480, Minneapolis 2013
2.753, NSW Clarence 0.060) under a different fold seed.

## Files

| | |
|---|---|
| `semantics.R`, `ties.R` | the two probes establishing what the package does with unranked items |
| `pl_subset.py` | our optimiser under the package's semantics; `python3 pl_subset.py` gradient-checks it (4.7e-07) |
| `export_and_fit.py` → `plackettluce_fit.R` → `compare.py` | tracks A, B, C → `build/comparison.json` |
| `gain_folds_export.py` → `gain_folds_fit.R` → `gain_folds_score.py` | the gain table → `build/fold_python.json` |

Ballot exports under `build/orderings/` and `build/folds/` are regenerable and not tracked.
Run single-threaded and niced, as everything else here is:

```
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 NUMEXPR_NUM_THREADS=1 \
  nice -n 19 ionice -c3 python3 export_and_fit.py
```
