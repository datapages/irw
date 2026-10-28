"""Stage a validation sample: export ballots for R, and fit them in Python.

Two tracks, because PlackettLuce and the vignette do not share a likelihood on partial
ballots (see README.md in this directory):

  A  complete ballots only -- every voter ranked all n candidates, so the package's
     subset semantics and the vignette's full-set semantics coincide exactly.  This
     validates the production fit (pilot.fit_pl) against the package on the same objective.
  B  all ballots, our optimiser run under the package's subset semantics (pl_subset).
     This validates the optimiser on the real, ragged data shapes.

Both are fitted unregularised (ridge 0 here, npseudo 0 in R).  The ridge the vignette
actually uses is quantified separately, in `ridge_shift`.

Usage: python3 export_and_fit.py [--out-dir build]
"""
import sys, json, time, pathlib, argparse
import numpy as np

HERE = pathlib.Path(__file__).parent
sys.path.insert(0, str(HERE.parent / "pilot"))
sys.path.insert(0, str(HERE))
import pilot                                    # noqa: E402
import pl_subset                                # noqa: E402

CACHE = pathlib.Path.home() / ".cache/irw_rank_vignette/tables"
CAP, SEED = 20000, 20261022
MIN_COMPLETE = 50

SAMPLE = [
    # Debian general resolutions and leader elections: ~70% complete ballots
    "debian_2007_dpl", "debian_2014_gr_init_coupling",
    "debian_2019_gr_init_systems", "debian_2026_gr_llm_usage",
    # Eurovision juries: long rankings, jurors may not rank their own country
    "eurovision_2016_final", "eurovision_2018_sf1",
    # single-contest elections
    "rcv_burlington_20060307_mayor", "rcv_alaska_20221108_hd10",
    "rcv_eastpointe_20191105_council", "rcv_genola_20211102_council",
    "rcv_nyc_20210622_demcouncild49",
    # Minneapolis, including the 35-candidate stress case
    "rcv_mpls_20131105_mayor", "rcv_mpls_20211102_mayor",
    "rcv_mpls_20171107_councilward10",
    # NSW optional preferential: short ballots, where the semantics gap bites hardest
    "rcv_nsw_20150328_la_albury", "rcv_nsw_20150328_la_clarence",
    "rcv_nsw_20150328_la_balmain",
    # survey rankings
    "atsusaka_2024_oakland_rcv_survey", "voterautrement_2017_irv1",
    "atsusaka_2025_ranking_questions",
]


def write_orderings(path, ballots, n):
    """One row per ballot, item codes 1..n, blank-padded to the longest ballot."""
    K = max(len(b) for b in ballots)
    with open(path, "w", newline="") as fh:
        fh.write(",".join(f"o{j+1}" for j in range(K)) + "\n")
        for b in ballots:
            fh.write(",".join([str(i + 1) for i in b] + [""] * (K - len(b))) + "\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", default="build")
    a = ap.parse_args()
    out = HERE / a.out_dir
    (out / "orderings").mkdir(parents=True, exist_ok=True)

    meta = {}
    for i, name in enumerate(SAMPLE, 1):
        t0 = time.time()
        items, ballots = pilot.load(CACHE / f"{name}.csv")
        n = len(items)
        n_all = len(ballots)
        capped = n_all > CAP
        if capped:
            rng = np.random.default_rng(SEED)
            ballots = [ballots[k] for k in rng.choice(n_all, CAP, replace=False)]
        rec = {"n_items": n, "n_ballots": len(ballots), "n_ballots_total": n_all,
               "capped": capped,
               "mean_marks": round(float(np.mean([len(b) for b in ballots])), 3)}

        # --- track B: every ballot, subset semantics ---------------------------
        B = pilot.pad(ballots)
        pl_subset.RIDGE = 0.0
        rec["py_subset_theta"] = [round(x, 8) for x in pl_subset.fit(B, n)]
        write_orderings(out / "orderings" / f"{name}__all.csv", ballots, n)

        # --- track A: complete ballots only, production fit --------------------
        comp = [b for b in ballots if len(b) == n]
        rec["n_complete"] = len(comp)
        if len(comp) >= MIN_COMPLETE:
            Bc = pilot.pad(comp)
            saved, pilot.RIDGE = pilot.RIDGE, 0.0
            rec["py_full_theta_complete"] = [round(x, 8) for x in pilot.fit_pl(Bc, n)]
            # cross-check: on complete ballots the two semantics must agree
            rec["py_subset_theta_complete"] = [round(x, 8) for x in pl_subset.fit(Bc, n)]
            pilot.RIDGE = saved
            write_orderings(out / "orderings" / f"{name}__complete.csv", comp, n)

        # --- what the production ridge does to the worths ----------------------
        saved, pilot.RIDGE = pilot.RIDGE, 0.0
        th0 = pilot.fit_pl(B, n)
        pilot.RIDGE = 0.01
        th1 = pilot.fit_pl(B, n)
        pilot.RIDGE = saved
        rec["ridge_shift"] = {"max_abs": round(float(np.abs(th1 - th0).max()), 6),
                              "rms": round(float(np.sqrt(((th1 - th0) ** 2).mean())), 6)}

        rec["secs"] = round(time.time() - t0, 1)
        meta[name] = rec
        print(f"[{i}/{len(SAMPLE)}] {name} n={n} N={len(ballots)} "
              f"complete={rec['n_complete']} {rec['secs']}s", flush=True)
        json.dump(meta, open(out / "python_fits.json", "w"), indent=1, sort_keys=True)


if __name__ == "__main__":
    main()
