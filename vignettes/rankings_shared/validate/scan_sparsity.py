"""Which tables have candidates the data barely identify?

Under the vignette's full-set semantics an unmarked candidate still sits in every
denominator, so the likelihood is monotone in their worth and only the ridge stops
theta running to -inf.  This scan finds where that bites: candidates with no marks at
all, and candidates with very few.
"""
import sys, json, pathlib
sys.path.insert(0, "pilot")
import pilot                                    # noqa: E402
import numpy as np

CACHE = pathlib.Path.home() / ".cache/irw_rank_vignette/tables"
CAP, SEED = 20000, 20261022
out = {}
for i, p in enumerate(sorted(CACHE.glob("*.csv")), 1):
    items, ballots = pilot.load(p)
    n = len(items)
    if n < 3:
        continue
    if len(ballots) > CAP:
        rng = np.random.default_rng(SEED)
        ballots = [ballots[k] for k in rng.choice(len(ballots), CAP, replace=False)]
    cnt = np.zeros(n, int)
    for b in ballots:
        cnt[b] += 1
    out[p.stem] = {"n_items": n, "n_ballots": len(ballots),
                   "zero_mark": int((cnt == 0).sum()),
                   "under10": int((cnt < 10).sum()),
                   "min_marks": int(cnt.min()),
                   "min_share": float(cnt.min() / len(ballots))}
    if i % 100 == 0:
        print(f"  {i} scanned", flush=True)
json.dump(out, open("validate/build/sparsity.json", "w"), indent=1, sort_keys=True)

z = [(k, v) for k, v in out.items() if v["zero_mark"]]
u = [(k, v) for k, v in out.items() if v["under10"] and not v["zero_mark"]]
print(f"\n{len(out)} tables scanned")
print(f"{len(z)} with a candidate nobody marked:")
for k, v in sorted(z, key=lambda t: -t[1]["zero_mark"]):
    print(f"  {k[:46]:46s} n={v['n_items']:3d} zero={v['zero_mark']:2d} under10={v['under10']:2d}")
print(f"{len(u)} more with a candidate under 10 marks:")
for k, v in sorted(u, key=lambda t: t[1]["min_marks"])[:25]:
    print(f"  {k[:46]:46s} n={v['n_items']:3d} min={v['min_marks']:3d} under10={v['under10']:2d}")
