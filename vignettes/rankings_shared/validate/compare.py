"""Compare the Python fits with the PlackettLuce package fits, and report.

Three comparisons per table where the data allow:
  A  production fit (pilot.fit_pl, full-set semantics) vs the package, on complete
     ballots only -- the one case where the two likelihoods are the same function.
  B  our subset-semantics fit vs the package, on every ballot.
  C  internal: on complete ballots the two Python semantics must coincide exactly.

Also checks the log-likelihood itself, not just the optimum, by evaluating our objective
at our own solution and comparing with logLik() from the package.

Usage: python3 compare.py [--out-dir build]
"""
import sys, json, pathlib, argparse
import numpy as np

HERE = pathlib.Path(__file__).parent
sys.path.insert(0, str(HERE.parent / "pilot"))
sys.path.insert(0, str(HERE))
import pilot                                    # noqa: E402
import pl_subset                                # noqa: E402

CACHE = pathlib.Path.home() / ".cache/irw_rank_vignette/tables"
CAP, SEED = 20000, 20261022


def ballots_for(name, meta):
    items, ballots = pilot.load(CACHE / f"{name}.csv")
    if meta[name]["capped"]:
        rng = np.random.default_rng(SEED)
        ballots = [ballots[k] for k in rng.choice(len(ballots), CAP, replace=False)]
    return len(items), ballots


def cmp(a, b):
    a, b = np.asarray(a, float), np.asarray(b, float)
    a, b = a - a.mean(), b - b.mean()
    return {"max_abs": float(np.abs(a - b).max()),
            "rms": float(np.sqrt(((a - b) ** 2).mean())),
            "corr": float(np.corrcoef(a, b)[0, 1]) if len(a) > 1 else 1.0,
            "max_worth_pct": float((np.abs(np.exp(a) - np.exp(b)) / np.exp(b)).max() * 100)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", default="build")
    a = ap.parse_args()
    out = HERE / a.out_dir
    meta = json.load(open(out / "python_fits.json"))
    rf = json.load(open(out / "r_fits.json"))

    report = {}
    for name, m in sorted(meta.items()):
        n, ballots = ballots_for(name, meta)
        rec = {k: m[k] for k in ("n_items", "n_ballots", "n_ballots_total", "capped",
                                 "mean_marks", "n_complete", "ridge_shift")}
        pl_subset.RIDGE = 0.0

        rA = rf.get(f"{name}__complete", {})
        if "theta" in rA and "py_full_theta_complete" in m:
            rec["A_production_vs_package"] = cmp(m["py_full_theta_complete"], rA["theta"])
            rec["C_python_semantics_agree"] = cmp(m["py_full_theta_complete"],
                                                  m["py_subset_theta_complete"])
            Bc = pilot.pad([b for b in ballots if len(b) == n])
            ll_ours = -pl_subset.nll(np.asarray(m["py_full_theta_complete"]), Bc, n)
            rec["A_loglik"] = {"ours": round(ll_ours, 6),
                               "package": round(rA["logLik"], 6),
                               "diff": round(ll_ours - rA["logLik"], 8)}
        elif m["n_complete"] < 50:
            rec["A_production_vs_package"] = f"skipped: {m['n_complete']} complete ballots"

        rB = rf.get(f"{name}__all", {})
        if "theta" in rB:
            rec["B_subset_vs_package"] = cmp(m["py_subset_theta"], rB["theta"])
            B = pilot.pad(ballots)
            ll_ours = -pl_subset.nll(np.asarray(m["py_subset_theta"]), B, n)
            rec["B_loglik"] = {"ours": round(ll_ours, 6),
                               "package": round(rB["logLik"], 6),
                               "diff": round(ll_ours - rB["logLik"], 8)}
        else:
            rec["B_subset_vs_package"] = f"package failed: {rB.get('error', 'no fit')}"
        report[name] = rec

    json.dump(report, open(out / "comparison.json", "w"), indent=1, sort_keys=True)

    def line(name, rec, key, llkey):
        c = rec.get(key)
        if isinstance(c, str):
            return f"{name[:42]:42s} {'--':>10s} {'--':>10s}  {c[:40]}"
        if c is None:
            return None
        ll = rec.get(llkey, {})
        return (f"{name[:42]:42s} {c['max_abs']:10.2e} {c['max_worth_pct']:9.3f}% "
                f" logLik diff {ll.get('diff', float('nan')):+.2e}")

    for key, llkey, title in (
            ("A_production_vs_package", "A_loglik",
             "A. production fit vs PlackettLuce, complete ballots (same likelihood)"),
            ("B_subset_vs_package", "B_loglik",
             "B. our optimiser under package semantics vs PlackettLuce, all ballots"),
            ("C_python_semantics_agree", None,
             "C. internal: full-set vs subset semantics on complete ballots")):
        print(f"\n{title}")
        print(f"{'table':42s} {'max|dlog w|':>10s} {'max dworth':>10s}")
        worst = 0.0
        for name, rec in sorted(report.items()):
            s = line(name, rec, key, llkey or "A_loglik")
            if s:
                print("  " + s)
            c = rec.get(key)
            if isinstance(c, dict):
                worst = max(worst, c["max_abs"])
        print(f"  {'worst max|d log worth| across tables':42s} {worst:10.2e}")

    print("\nridge 0.01, production: how far it moves the log-worths")
    for name, rec in sorted(report.items()):
        r = rec["ridge_shift"]
        print(f"  {name[:42]:42s} max {r['max_abs']:.4f}  rms {r['rms']:.4f}")


if __name__ == "__main__":
    main()
