"""Save the item loadings behind the expansion sweep, so the keying question can
be asked properly offline.

The sweep's `mirt_key_alignment` column took the larger of the two |corr(loading,
sign of 2PL slope)| values, which is close to meaningless: the first MIRT
dimension recovers the 2PL direction almost by construction, signs included, so
that maximum mostly measures a tautology. Asking whether the SECOND dimension is
a keying factor needs the solution oriented first, and that needs the loadings
rather than a scalar summary of them.

Writes one long CSV: table, iid, a_2pl (signed slope), A1, A2 (MIRT k=2
loadings). Orientation and any alignment statistic are then computed in the page
from this file. Fetches are already cached by the sweep, so this is fits only.
Resumable. Run from vignettes/.
"""
import sys, time
import numpy as np
import pandas as pd

sys.path.insert(0, ".")
import nn_irt_helpers as H

OUT = H.FITS / "expand_loadings.csv"
SRC = H.FITS / "expand_results.csv"


def main():
    res = pd.read_csv(SRC)
    tables = res.loc[res.status == "ok", "table"].astype(str).tolist()
    done = set()
    if OUT.exists():
        done = set(pd.read_csv(OUT, usecols=["table"]).table.astype(str))
    todo = [t for t in tables if t not in done]
    print(f"{len(done)} done, {len(todo)} to go", flush=True)
    Rasch, TwoPL, Bilinear, MLP = H.make_models()
    for i, t in enumerate(todo, 1):
        t0 = time.time()
        try:
            d = H.prepare(t)
            n_p, n_i = int(d.pid.max() + 1), int(d.iid.max() + 1)
            m2, _ = H.fit(TwoPL(n_p, n_i), d)
            mb, _ = H.fit(Bilinear(n_p, n_i, k=2), d)
            a = m2.a.weight.detach().numpy().ravel()
            A = mb.a.weight.detach().numpy()
            pd.DataFrame({"table": t, "iid": np.arange(n_i), "a_2pl": a,
                          "A1": A[:, 0], "A2": A[:, 1]}).to_csv(
                OUT, mode="a", header=not OUT.exists(), index=False)
            msg = f"{n_i} items"
        except Exception as e:
            msg = f"FAILED {type(e).__name__}: {str(e)[:60]}"
        print(f"[{i}/{len(todo)}] {t[:44]:44s} {msg[:44]:44s} {time.time()-t0:5.1f}s", flush=True)


if __name__ == "__main__":
    main()
