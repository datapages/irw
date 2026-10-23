"""Where does the second dimension pay off?

Takes the expansion sweep's per-item 2PL slopes and MIRT loadings and asks whether
the gain is explained by structure orthogonal to the 2PL direction, or simply by
tables that still carry reverse-keyed items.

The naive statistic -- the larger |corr(loading, 2PL slope)| over the two MIRT
dimensions -- is an artifact: dimension 1 recovers the 2PL direction by
construction. So orient the solution first (rotate so column 1 points along the
2PL slope vector), then look at what is left in the orthogonal column.

Writes nn_irt_data/expand_summary.csv -- one row per table, the committed
deliverable of the sweep. The per-table fits it is built from stay in fits/,
which is not in the repository, so this file is what the vignette reads.
"""
from pathlib import Path

import numpy as np
import pandas as pd

DATA = Path(__file__).parent / "nn_irt_data"
FITS = DATA / "fits"
OUT = DATA / "expand_summary.csv"


def oriented(a_2pl, loadings):
    """Rotate a two-column loading matrix so column 1 lies along the 2PL slopes.

    Returns the aligned and orthogonal components as vectors over items.
    """
    u = loadings.T @ a_2pl
    norm = np.linalg.norm(u)
    if norm == 0:
        return None
    u = u / norm
    v = np.array([-u[1], u[0]])
    return loadings @ u, loadings @ v


def main():
    loadings = pd.read_csv(FITS / "expand_loadings.csv")
    results = pd.read_csv(FITS / "expand_results.csv").rename(
        columns={"imv_mirt_over_2pl": "imv"}
    )

    rows = []
    for table, group in loadings.groupby("table"):
        a = group["a_2pl"].to_numpy(float)
        A = group[["A1", "A2"]].to_numpy(float)
        if np.allclose(a, 0) or not np.isfinite(A).all():
            continue
        parts = oriented(a, A)
        if parts is None:
            continue
        aligned, orth = parts
        signs = np.sign(a)
        rows.append(
            dict(
                table=table,
                align=abs(np.corrcoef(aligned, a)[0, 1]) if aligned.std() > 0 else np.nan,
                orth_share=(orth @ orth) / ((aligned @ aligned) + (orth @ orth)),
                orth_sign_r=(
                    abs(np.corrcoef(orth, signs)[0, 1])
                    if np.unique(signs).size > 1 and orth.std() > 0
                    else np.nan
                ),
                frac_neg=float((a < 0).mean()),
            )
        )

    out = (
        pd.DataFrame(rows)
        .merge(results[["table", "imv", "items", "persons", "mean_resp"]], on="table", how="left")
        .dropna(subset=["imv"])
    )
    out["mixed_sign"] = out["frac_neg"] > 0
    out.to_csv(OUT, index=False)

    print(f"{len(out)} tables -> {OUT}")
    for col in ("align", "orth_share", "frac_neg"):
        print(f"  corr(imv, {col:11s}) = {out['imv'].corr(out[col]):+.3f}")
    print(
        out.groupby("mixed_sign")
        .agg(n=("imv", "size"), mean=("imv", "mean"), median=("imv", "median"))
        .to_string(float_format=lambda x: f"{x:9.4f}")
    )


if __name__ == "__main__":
    main()
