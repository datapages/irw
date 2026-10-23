"""Question A across the live rankings family: how much of a ranking is shared.

Two numbers per table, both from the 10-15 pilot (imported, not reimplemented):
  gain_pl_vs_uniform  -- 5-fold held-out Plackett-Luce gain over a uniform next mark
  gain_mix2_vs_pl     -- what a 2-class PL mixture ("camps") adds, N >= 150 only
Gain = exp(mean log-lik difference per choice) - 1.

Usage: python3 compute_a.py [--cap N] [--out results_a.json] [--only t1,t2,...]
       --cap 0 disables the ballot cap (the full-family rerun).
Reads ~/.cache/irw_rank_vignette/tables/ (see stage_rank.py).
"""
import sys, json, time, pathlib, argparse
import numpy as np

HERE = pathlib.Path(__file__).parent
PILOT = HERE / "pilot"
CACHE = pathlib.Path.home() / ".cache/irw_rank_vignette/tables"
sys.path.insert(0, str(PILOT))
import pilot                      # noqa: E402  -- load, question_a, PL fits, gain stat

SEED = 20261022


def group_of(name):
    return name.split("_")[1] if name.startswith("rcv_") else name.split("_")[0]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--cap", type=int, default=20000)
    ap.add_argument("--out", default="results_a.json")
    ap.add_argument("--only", default=None)
    a = ap.parse_args()

    names = sorted(p.stem for p in CACHE.glob("*.csv"))
    if a.only:
        want = set(a.only.split(","))
        names = [n for n in names if n in want]
    out = HERE / a.out
    results = json.load(open(out)) if out.exists() else {}

    for i, name in enumerate(names, 1):
        if name in results:
            continue
        t0 = time.time()
        try:
            items, ballots = pilot.load(CACHE / f"{name}.csv")
            n = len(items)
            if n < 3:
                print(f"[{i}/{len(names)}] {name} SKIP <3 items", flush=True)
                results[name] = {"group": group_of(name), "n_items": n, "skipped": "<3 items"}
                continue
            n_all = len(ballots)
            capped = bool(a.cap) and n_all > a.cap
            if capped:
                rng = np.random.default_rng(SEED)
                keep = rng.choice(n_all, a.cap, replace=False)
                ballots = [ballots[k] for k in keep]
            r = {"group": group_of(name), "n_items": n, "n_ballots": len(ballots),
                 "n_ballots_total": n_all, "capped": capped, "cap": a.cap, "seed": SEED,
                 "mean_marks": round(float(np.mean([len(b) for b in ballots])), 3)}
            r.update(pilot.question_a(ballots, n))
            r["secs"] = round(time.time() - t0, 1)
            results[name] = r
            print(f"[{i}/{len(names)}] {name} n={n} N={len(ballots)}"
                  f"{'(capped)' if capped else ''} cons={r['gain_pl_vs_uniform']:.4f}"
                  f" mix={r.get('gain_mix2_vs_pl', float('nan')):.4f} {r['secs']}s", flush=True)
        except Exception as e:                                   # noqa: BLE001
            results[name] = {"group": group_of(name),
                             "failed": f"{type(e).__name__}: {e}"[:300]}
            print(f"[{i}/{len(names)}] {name} FAILED {type(e).__name__}: {e}", flush=True)
        json.dump(results, open(out, "w"), indent=1, sort_keys=True)

    done = [r for r in results.values() if "gain_pl_vs_uniform" in r]
    print(f"\n{len(done)} tables fitted, "
          f"{sum(1 for r in results.values() if 'failed' in r)} failed, "
          f"{sum(1 for r in results.values() if 'skipped' in r)} skipped")


if __name__ == "__main__":
    main()
