"""Question B, cleaner: (a) entropy of the item occupying each position, normalised
by log(n); (b) held-out gain vs uniform of the p-th mark given earlier marks, PL
trained on all positions; (c) Spearman of worths from top-k training vs full."""
import sys, math, collections, json
import numpy as np
from scipy.stats import spearmanr
sys.argv = [sys.argv[0]]; exec(open("pilot.py").read().split('if __name__')[0])

def per_position_gain(ballots, n):
    N = len(ballots); B = pad(ballots); fs = folds(N); P = B.shape[1]
    lm, lb, pos = [], [], []
    for f in fs:
        test = np.zeros(N, bool); test[f] = True
        th = fit_pl(B[~test], n)
        w, mask, wB, rem = _rem(th, B[test])
        tB = np.where(mask, th[np.where(mask, B[test], 0)], 0.0)
        lm.append((tB - np.log(rem))[mask])
        lb.append(-np.log(np.broadcast_to(n - np.arange(P), mask.shape))[mask])
        pos.append(np.broadcast_to(np.arange(P), mask.shape)[mask])
    lm, lb, pos = map(np.concatenate, (lm, lb, pos))
    out = []
    for p in range(P):
        m = pos == p
        out.append(round(float(np.exp((lm[m] - lb[m]).mean()) - 1), 3) if m.sum() >= 30 else None)
    return out

def entropy(ballots, n):
    K = max(len(b) for b in ballots); out = []
    for p in range(K):
        c = collections.Counter(b[p] for b in ballots if len(b) > p); tot = sum(c.values())
        if tot < 30: out.append(None); continue
        out.append(round(-sum(v/tot*math.log(v/tot) for v in c.values()) / math.log(n), 2))
    return out

def depth_recovery(ballots, n):
    B = pad(ballots); K = B.shape[1]; full = fit_pl(B, n); out = {}
    for k in [kk for kk in range(1, K+1) if kk <= 6 or kk == K or kk % 4 == 0]:
        out[k] = round(float(spearmanr(fit_pl(B[:, :k], n), full)[0]), 3)
    return out

res = {}
import glob
for path in sorted(glob.glob("../../src/data/rankings/examples/*.csv")) + sorted(glob.glob("../rankings-eurovision-tables-2026-10-15/eurovision_*.csv")):
    name = path.split("/")[-1][:-4]; items, ballots = load(path); n = len(items)
    if n < 3: continue
    res[name] = {"entropy": entropy(ballots, n), "gain_by_position": per_position_gain(ballots, n), "spearman_topk_vs_full": depth_recovery(ballots, n)}
    r = res[name]
    print(f"{name:32s} H:", " ".join("  . " if x is None else f"{x:.2f}" for x in r["entropy"]))
    print(f"{'':32s} G:", " ".join("   . " if x is None else f"{x:+.2f}" for x in r["gain_by_position"]))
    print(f"{'':32s} S:", " ".join(f"k{k}={v:.2f}" for k, v in r["spearman_topk_vs_full"].items()))
json.dump(res, open("position_results.json", "w"), indent=1)
