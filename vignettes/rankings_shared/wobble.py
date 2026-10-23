"""How much do the two numbers move with the fold split and the mixture's EM init?

pilot.RNG drives both the 5-fold split and fit_mix's dirichlet start, so reseeding it
and refitting gives the run-to-run spread we have to treat as noise in the draft.
Usage: python3 wobble.py
"""
import sys, json, pathlib, statistics
import numpy as np
HERE = pathlib.Path(__file__).parent
sys.path.insert(0, str(HERE.parent / "rankings-vignette-pilot-2026-10-15"))
import pilot
CACHE = pathlib.Path.home() / ".cache/irw_rank_vignette/tables"

TABLES = ["rcv_alaska_20221108_hd10", "rcv_burlington_20060307_mayor",
          "rcv_eastpointe_20191105_council", "rcv_nyc_20210622_demcouncild49",
          "eurovision_2017_final", "rcv_mpls_20091106_ward7council",
          "debian_2021_gr_fsf_statement", "voterautrement_2017_borda4"]
SEEDS = [1, 2, 3, 4, 5]
out = {}
for name in TABLES:
    items, ballots = pilot.load(CACHE / f"{name}.csv")
    n = len(items)
    runs = []
    for s in SEEDS:
        pilot.RNG = np.random.default_rng(s)
        runs.append(pilot.question_a(ballots, n))
    rec = {"n_items": n, "n_ballots": len(ballots)}
    for key in ("gain_pl_vs_uniform", "gain_mix2_vs_pl"):
        vals = [r[key] for r in runs if key in r]
        if vals:
            rec[key] = {"mean": round(statistics.mean(vals), 4),
                        "min": round(min(vals), 4), "max": round(max(vals), 4),
                        "range": round(max(vals) - min(vals), 4)}
    out[name] = rec
    print(name, json.dumps(rec), flush=True)
json.dump(out, open(HERE / "wobble.json", "w"), indent=1, sort_keys=True)
