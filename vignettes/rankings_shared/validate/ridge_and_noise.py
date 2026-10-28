"""Two sensitivities of the published consensus gain: the ridge, and the fold split.

Consensus gain only (no mixture), so one table is a handful of PL fits.  For each table:
  * gain at ridge 0.01 (production) under three different fold seeds -> split noise
  * gain at ridge 0 under the first of those seeds -> what the ridge is worth
The published value from results_a.json is printed alongside.

Run on the five tables with a candidate under 10 marks, plus a stratified control sample.
"""
import sys, json, pathlib, random
import numpy as np

sys.path.insert(0, "pilot")
import pilot                                    # noqa: E402

CACHE = pathlib.Path.home() / ".cache/irw_rank_vignette/tables"
CAP, SEED = 20000, 20261022
SEEDS = [20261028, 7, 99]

AT_RISK = ["rcv_mpls_20171107_mayor", "rcv_mpls_20211102_mayor",
           "voterautrement_2024_rep_top2", "voterautrement_2024_self_top2",
           "rcv_mpls_20171107_parksboardal"]


def gain(ballots, n, fold_seed, ridge):
    B = pilot.pad(ballots)
    N = len(ballots)
    perm = np.random.default_rng(fold_seed).permutation(N)
    saved, pilot.RIDGE = pilot.RIDGE, ridge
    lm, lb = [], []
    for k in range(5):
        test = np.zeros(N, bool); test[perm[k::5]] = True
        th = pilot.fit_pl(B[~test], n)
        a, b_ = pilot.choice_loglik(th, B[test], n)
        lm.append(a); lb.append(b_)
    pilot.RIDGE = saved
    return pilot.imv_from_ll(np.concatenate(lm), np.concatenate(lb))


def main():
    pub = json.load(open("results_a.json"))
    sparsity = json.load(open("validate/build/sparsity.json"))
    fitted = [k for k, v in pub.items() if "gain_pl_vs_uniform" in v]
    groups = sorted({pub[k]["group"] for k in fitted})
    rng = random.Random(11)
    controls = []
    for g in groups:
        pool = [k for k in fitted if pub[k]["group"] == g and k not in AT_RISK]
        controls += rng.sample(pool, min(3, len(pool)))
    names = AT_RISK + sorted(controls)

    rows, res = [], {}
    for i, name in enumerate(names, 1):
        items, ballots = pilot.load(CACHE / f"{name}.csv")
        n = len(items)
        if len(ballots) > CAP:
            r = np.random.default_rng(SEED)
            ballots = [ballots[k] for k in r.choice(len(ballots), CAP, replace=False)]
        gs = [gain(ballots, n, s, 0.01) for s in SEEDS]
        g0 = gain(ballots, n, SEEDS[0], 0.0)
        rec = {"n_items": n, "published": pub[name]["gain_pl_vs_uniform"],
               "ridge01_by_seed": [round(x, 5) for x in gs],
               "ridge0_seed1": round(g0, 5),
               "ridge_effect": round(gs[0] - g0, 5),
               "seed_spread": round(max(gs) - min(gs), 5),
               "min_marks": sparsity[name]["min_marks"],
               "at_risk": name in AT_RISK}
        res[name] = rec
        rows.append((name, rec))
        print(f"[{i}/{len(names)}] {name[:44]:44s} pub={rec['published']:.4f} "
              f"ridge.01={gs[0]:.4f} ridge0={g0:.4f} d={rec['ridge_effect']:+.5f} "
              f"spread={rec['seed_spread']:.5f}", flush=True)
        json.dump(res, open("validate/build/ridge_and_noise.json", "w"),
                  indent=1, sort_keys=True)

    print("\n--- ridge 0.01 vs 0, same split ---")
    for tag, sel in (("at-risk (min marks < 10)", True), ("controls", False)):
        s = [abs(r["ridge_effect"]) for _, r in rows if r["at_risk"] is sel]
        print(f"  {tag:26s} n={len(s):2d}  max |d gain| {max(s):.5f}  median {sorted(s)[len(s)//2]:.5f}")
    print("--- fold-split spread at ridge 0.01 (3 seeds) ---")
    s = sorted(r["seed_spread"] for _, r in rows)
    print(f"  max {s[-1]:.5f}  median {s[len(s)//2]:.5f}")
    print("--- published value vs this run's seed range ---")
    bad = [(k, r) for k, r in rows
           if not (min(r["ridge01_by_seed"]) - r["seed_spread"] <= r["published"]
                   <= max(r["ridge01_by_seed"]) + r["seed_spread"])]
    print(f"  {len(bad)} of {len(rows)} outside the seed range widened by one spread")
    for k, r in bad:
        print(f"    {k[:44]:44s} pub={r['published']:.4f} seeds={r['ridge01_by_seed']}")


if __name__ == "__main__":
    main()
