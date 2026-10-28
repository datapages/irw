"""Does the headline statistic change if the package does the fitting?

For each table, the same 5-fold split is fitted three ways and scored by the *same*
full-set held-out scorer (pilot.choice_loglik), so only the fit differs:

  production   full-set semantics, our optimiser, ridge 0.01  -- the vignette's number
  py_subset    package semantics, our optimiser, ridge 0
  package      package semantics, PlackettLuce, npseudo 0     -- fitted by gain_folds_score.py

py_subset vs package isolates the software.  production vs py_subset shows what the
semantics choice is worth, which is a modelling question, not a software one.

Step 1 of 2: write the fold training sets for R and the Python-side gains.
"""
import sys, json, pathlib
import numpy as np

HERE = pathlib.Path(__file__).parent
sys.path.insert(0, str(HERE.parent / "pilot"))
sys.path.insert(0, str(HERE))
import pilot                                    # noqa: E402
import pl_subset                                # noqa: E402

CACHE = pathlib.Path.home() / ".cache/irw_rank_vignette/tables"
CAP, SEED, FOLDSEED = 20000, 20261022, 20261028
TABLES = ["debian_2019_gr_init_systems", "eurovision_2016_final",
          "rcv_burlington_20060307_mayor", "rcv_nsw_20150328_la_clarence",
          "rcv_mpls_20131105_mayor", "atsusaka_2024_oakland_rcv_survey"]


def main():
    out = HERE / "build"
    (out / "folds").mkdir(parents=True, exist_ok=True)
    man, res = [], {}

    for name in TABLES:
        items, ballots = pilot.load(CACHE / f"{name}.csv")
        n = len(items)
        if len(ballots) > CAP:
            rng = np.random.default_rng(SEED)
            ballots = [ballots[k] for k in rng.choice(len(ballots), CAP, replace=False)]
        B = pilot.pad(ballots)
        N = len(ballots)
        perm = np.random.default_rng(FOLDSEED).permutation(N)
        folds = [perm[i::5] for i in range(5)]

        lm_prod, lm_sub, lb_all, thetas = [], [], [], {}
        for k, f in enumerate(folds):
            test = np.zeros(N, bool); test[f] = True
            tr, te = B[~test], B[test]
            pilot.RIDGE = 0.01
            th_p = pilot.fit_pl(tr, n)
            pl_subset.RIDGE = 0.0
            th_s = pl_subset.fit(tr, n)
            a, b_ = pilot.choice_loglik(th_p, te, n); lm_prod.append(a); lb_all.append(b_)
            a2, _ = pilot.choice_loglik(th_s, te, n); lm_sub.append(a2)
            thetas[k] = [round(x, 8) for x in th_s]
            key = f"{name}__f{k}"
            K = max(len(b) for b in [list(r[r >= 0]) for r in tr])
            with open(out / "folds" / f"{key}.csv", "w", newline="") as fh:
                fh.write(",".join(f"o{j+1}" for j in range(K)) + "\n")
                for row in tr:
                    b = [str(x + 1) for x in row if x >= 0]
                    fh.write(",".join(b + [""] * (K - len(b))) + "\n")
            man.append((key, name, n, k))

        lb = np.concatenate(lb_all)
        res[name] = {"n_items": n, "n_ballots": N,
                     "n_marks_scored": int(len(lb)),
                     "gain_production": pilot.imv_from_ll(np.concatenate(lm_prod), lb),
                     "gain_py_subset": pilot.imv_from_ll(np.concatenate(lm_sub), lb),
                     "py_subset_theta_by_fold": thetas}
        print(f"{name:42s} n={n:3d} prod={res[name]['gain_production']:.4f} "
              f"pysub={res[name]['gain_py_subset']:.4f}", flush=True)

    with open(out / "folds" / "manifest.csv", "w") as fh:
        fh.write("key,table,n_items,fold\n")
        for r in man:
            fh.write(",".join(str(x) for x in r) + "\n")
    json.dump(res, open(out / "fold_python.json", "w"), indent=1, sort_keys=True)


if __name__ == "__main__":
    main()
