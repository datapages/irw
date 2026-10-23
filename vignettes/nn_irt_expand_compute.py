"""Fit the 2PL and a two-dimensional MIRT on every eligible IRW table.

Part I's 24 tables were chosen by hand, which cannot settle whether the gains a
two-dimensional model shows are a second substantive dimension or item keying.
This sweep applies a rule instead, following Domingue et al. (2024): dichotomous,
at least 500 respondents, and -- our additions -- at least 10 items and density
>= 0.9, since the cell-level split assumes dense tables.

For each table it records the IMV of MIRT k=2 over the 2PL on held-out cells and
one covariate for the keying question: the fraction of items whose rest
correlation is negative. The `mirt_key_alignment` column is retained for the
record but should not be used -- see nn_irt_expand_keying.py, which recomputes
the alignment properly from the saved loading matrix.

Resumable: one row per table appended to fits/expand_results.csv, tables already
in the file are skipped. Run from vignettes/.
"""
import os
import sys
import time
import traceback
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, ".")
import nn_irt_helpers as H
from imvpy import vanilla_imv

# The corpus metadata lives in the main checkout, not in a vignette worktree; point
# IRW_METADATA at it if this script is run from somewhere else.
METADATA = Path(
    os.environ.get("IRW_METADATA", "~/Dropbox/projects/irw/src/metadata/metadata.csv")
).expanduser()
OUT = H.FITS / "expand_results.csv"
MIN_PERSONS, MIN_ITEMS, MIN_DENSITY = 500, 10, 0.9
# Every table is subsampled to 5,000 respondents and 116 items before fitting, so
# rows past that are fetched and thrown away. Uncapped the pool is 2.7bn rows, nearly
# all of it in a few mass administrations (the 1m-respondent ENEM subject forms, a
# 107m-row syntax study). Capping the fetch keeps 262 of 349 tables for 63m rows; the
# cost is the largest administrations, which subsample to the same size as the rest.
MAX_RESPONSES = 2_000_000


def eligible():
    m = pd.read_csv(METADATA)
    q = m[(m.n_categories == 2) & (m.n_participants >= MIN_PERSONS)
          & (m.n_items >= MIN_ITEMS) & (m.density >= MIN_DENSITY)
          & (m.n_responses <= MAX_RESPONSES)]
    return sorted(q.table.astype(str))


def rest_correlation_signs(d):
    """Fraction of items whose correlation with the rest score is negative."""
    wide = d.pivot_table(index="pid", columns="iid", values="resp")
    total = wide.sum(axis=1)
    r = {}
    for c in wide.columns:
        rest = total - wide[c]
        ok = wide[c].notna() & rest.notna()
        if ok.sum() > 2 and wide.loc[ok, c].nunique() > 1 and rest[ok].nunique() > 1:
            r[c] = np.corrcoef(wide.loc[ok, c], rest[ok])[0, 1]
    s = pd.Series(r)
    return float((s < 0).mean()), s


def run_table(t, models):
    Rasch, TwoPL, Bilinear, MLP = models
    d = H.prepare(t)
    n_p, n_i = int(d.pid.max() + 1), int(d.iid.max() + 1)
    y = d[d.test].resp.values
    p2 = H.best_epoch_preds(lambda: TwoPL(n_p, n_i), d)
    pb = H.best_epoch_preds(lambda: Bilinear(n_p, n_i, k=2), d)
    imv = vanilla_imv(p2, pb, y)

    # the keying covariates: a 2PL for signed slopes, and the MIRT loadings
    m2, _ = H.fit(TwoPL(n_p, n_i), d)
    a = m2.a.weight.detach().numpy().ravel()
    mb, _ = H.fit(Bilinear(n_p, n_i, k=2), d)
    A = mb.a.weight.detach().numpy()               # (items, 2)
    neg_rest, rest_r = rest_correlation_signs(d)
    key = np.sign(a)
    # how much of the second dimension is keying direction: |corr| between each
    # MIRT loading column and the 2PL slope sign, taking the larger
    align = max(abs(np.corrcoef(A[:, k], key)[0, 1]) if np.std(A[:, k]) > 0 and np.std(key) > 0
                else np.nan for k in range(A.shape[1]))
    return {"table": t, "persons": n_p, "items": n_i, "test_cells": int(d.test.sum()),
            "mean_resp": round(float(d.resp.mean()), 4),
            "imv_mirt_over_2pl": round(float(imv), 6),
            "neg_slope_frac": round(float(np.mean(a < 0)), 4),
            "neg_rest_frac": round(neg_rest, 4),
            "mirt_key_alignment": round(float(align), 4)}


def main(limit=None):
    done = set()
    if OUT.exists():
        done = set(pd.read_csv(OUT).table.astype(str))
    todo = [t for t in eligible() if t not in done]
    if limit:
        todo = todo[:limit]
    print(f"{len(done)} done, {len(todo)} to go", flush=True)
    models = H.make_models()
    for i, t in enumerate(todo, 1):
        t0 = time.time()
        try:
            row = run_table(t, models)
            row["seconds"] = round(time.time() - t0, 1)
            row["status"] = "ok"
        except Exception as e:
            row = {"table": t, "status": f"{type(e).__name__}: {str(e)[:120]}",
                   "seconds": round(time.time() - t0, 1)}
            if "--trace" in sys.argv:
                traceback.print_exc()
        pd.DataFrame([row]).to_csv(OUT, mode="a", header=not OUT.exists(), index=False)
        print(f"[{i}/{len(todo)}] {t[:44]:44s} {row['status'][:48]:48s} {row['seconds']:6.1f}s", flush=True)


if __name__ == "__main__":
    n = next((int(x) for x in sys.argv[1:] if x.isdigit()), None)
    main(limit=n)
