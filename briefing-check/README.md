# briefing-check

A repeatable check of the agents briefing (`llms.txt`, published at
`itemresponsewarehouse.org/llms.txt`) against the two client packages, written for one failure mode:
everything runs, nothing errors, and the numbers are wrong.

Every check asserts a property a silent no-op cannot satisfy. A filter must return well under the
whole catalogue. The section 3 quota guard must exclude every table at 1M+ rows. `info()` must print
the record. The section 0 numbers are fixed by the data shipped in the R package, so they are asserted
exactly. Pitfall 2 (`long2resp()` dropping `cov_*` and low-density ids) is checked offline on a toy
frame in both languages. Any FAIL exits with status 1. WARNs do not fail the run: they flag briefing
text that may have gone stale in either direction, for example a function section 9 says Python does
not have appearing in the package.

The two sides then get compared. Both scripts write their counts to a small JSON and `compare.py`
holds them against each other. That comparison needs no expected values, which is what makes it
survive corpus versions: it fails on divergence, whichever side is wrong. On 2026-09-05 it would have
printed `n_categories=2: python 4229, r 451`.

Nothing here downloads a response table, so it spends no Redivis quota.

## When to run

By hand, after editing `llms.txt`, or after a package API changes. Run the R script from outside this
repo, or with renv deactivated: the site's `renv.lock` pins an older `irw` and has no `mokken`, so
inside the project the check would be reading the site's library rather than the documented install.

## Files

| File | What it does |
|---|---|
| `check_briefing.py` | Python side. Writes `counts_python.json` (or `--json PATH`, empty to skip) |
| `check_briefing.R` | R side, including section 0 offline. Writes `counts_r.json` |
| `compare.py` | Holds the two JSONs against each other; exits 1 on any difference |
| `examples/` | The three local runs of 2026-09-05 and their JSONs, as reference output |

## Run

Python, with the documented install line:

    python3 -m venv venv && ./venv/bin/pip install "git+https://github.com/itemresponsewarehouse/Python-pkg.git"
    REDIVIS_API_TOKEN=... ./venv/bin/python check_briefing.py

R, with the documented install lines (it installs `irw`, `psych` and `mokken` itself if missing; the
warehouse part needs `pak::pak("redivis/redivis-r")` and the token):

    REDIVIS_API_TOKEN=... Rscript check_briefing.R

Then:

    python3 compare.py counts_python.json counts_r.json

## Measured on 2026-09-05, irw_meta at v21.0

- A fresh install pinned to the last commit before the fix (`a5eb750`, the 0.0.2 everyone had):
  exit 1, 9 of 21 checks failing. `include_metadata=True` carries one column, all six filters return
  4,229 of 4,230 tables, the six counts are identical, and `info()` raises `NotFoundError`.
  `compare.py` against the R counts: 0 agree, 3 differ, exit 1.
- The same script against 0.1.0: exit 0, 23 of 23. Filters give 451, 667, 290, 86, 2,713 and 71.
- The R script (irw 1.1.2): exit 0, 30 of 30, with 451 / 667 / 290 and 3,205 through the quota guard.
  `compare.py` against the 0.1.0 counts: 4 agree, 0 differ, exit 0.
- One thing the R run surfaces: in an `Rscript` run, redivis' *"No reference id was provided for the
  table"* warning still prints once. That is the client objecting to bare-name addressing, which is
  the behaviour wanted here.

One detail that matters for the bar: the no-op did not return the whole catalogue, it returned 4,229
of 4,230 (a duplicate name dropped), so "strictly fewer than the catalogue" would have passed. The
Python script requires at most 98% of the catalogue per filter, and every documented filter sits far
below that.

## Origin

Written from the scripts used to test the briefing on 2026-09-05 (Santiago Rivadeneira Quintero).
Scope recorded in datapages/irw#140.
