"""Step 3: score the package's fold fits with the same held-out scorer, and compare gains."""
import sys, json, pathlib
import numpy as np

HERE = pathlib.Path(__file__).parent
sys.path.insert(0, str(HERE.parent / "pilot"))
sys.path.insert(0, str(HERE))
import pilot                                    # noqa: E402

CACHE = pathlib.Path.home() / ".cache/irw_rank_vignette/tables"
CAP, SEED, FOLDSEED = 20000, 20261022, 20261028


def main():
    out = HERE / "build"
    py = json.load(open(out / "fold_python.json"))
    rf = json.load(open(out / "r_fold_fits.json"))
    rows = []
    for name, rec in sorted(py.items()):
        items, ballots = pilot.load(CACHE / f"{name}.csv")
        n = len(items)
        if len(ballots) > CAP:
            rng = np.random.default_rng(SEED)
            ballots = [ballots[k] for k in rng.choice(len(ballots), CAP, replace=False)]
        B = pilot.pad(ballots); N = len(ballots)
        perm = np.random.default_rng(FOLDSEED).permutation(N)
        lm, lb, dtheta = [], [], 0.0
        for k in range(5):
            f = perm[k::5]
            test = np.zeros(N, bool); test[f] = True
            th = np.asarray(rf[f"{name}__f{k}"]["theta"], float)
            th = th - th.mean()
            ours = np.asarray(rec["py_subset_theta_by_fold"][str(k)], float)
            dtheta = max(dtheta, float(np.abs(th - (ours - ours.mean())).max()))
            a, b_ = pilot.choice_loglik(th, B[test], n)
            lm.append(a); lb.append(b_)
        g_pkg = pilot.imv_from_ll(np.concatenate(lm), np.concatenate(lb))
        rows.append((name, rec["n_items"], round(rec["mean_marks"], 2) if "mean_marks" in rec
                     else None, rec["gain_production"], rec["gain_py_subset"], g_pkg, dtheta))
        rec["gain_package"] = g_pkg
        rec["max_dtheta_vs_package"] = dtheta
    json.dump(py, open(out / "fold_python.json", "w"), indent=1, sort_keys=True)

    print(f"{'table':42s} {'n':>3s} {'production':>11s} {'py_subset':>10s} "
          f"{'package':>10s} {'sub-pkg':>9s} {'max|dtheta|':>11s}")
    for name, n, _, gp, gs, gk, dt in rows:
        print(f"{name[:42]:42s} {n:3d} {gp:11.4f} {gs:10.4f} {gk:10.4f} "
              f"{gs-gk:+9.5f} {dt:11.2e}")


if __name__ == "__main__":
    main()
