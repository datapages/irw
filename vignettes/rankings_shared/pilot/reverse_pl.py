"""Does a reversed Plackett-Luce (rank the worst first) recover the bottom-of-ranking
agreement that forward PL misses?  Complete rankings only (Eurovision)."""
import sys, glob, json
import numpy as np
sys.argv = [sys.argv[0]]; exec(open("pilot.py").read().split('if __name__')[0])

def pos_gain(theta_fit, B_train_dir, B_test_fwd, n, reverse):
    """per original position: held-out gain of the mark at that position."""
    P = B_test_fwd.shape[1]
    Bt = B_test_fwd[:, ::-1] if reverse else B_test_fwd
    w, mask, wB, rem = _rem(theta_fit, Bt)
    tB = np.where(mask, theta_fit[np.where(mask, Bt, 0)], 0.0)
    lm = tB - np.log(rem); lb = -np.log(np.broadcast_to(n - np.arange(P), mask.shape))
    g = lm - lb
    if reverse: g = g[:, ::-1]      # back to original position index
    return g

out = {}
for path in sorted(glob.glob("../rankings-eurovision-tables-2026-10-15/eurovision_*.csv")):
    name = path.split("/")[-1][:-4]; items, ballots = load(path); n = len(items)
    ballots = [b for b in ballots if len(b) >= n - 1]    # jurors skip their own country
    B = pad(ballots); N = len(ballots); P = B.shape[1]
    gf, gr = [], []
    for f in folds(N):
        test = np.zeros(N, bool); test[f] = True
        thf = fit_pl(B[~test], n); thr = fit_pl(B[~test][:, ::-1], n)
        gf.append(pos_gain(thf, None, B[test], n, False)); gr.append(pos_gain(thr, None, B[test], n, True))
    gf = np.exp(np.concatenate(gf).mean(0)) - 1; gr = np.exp(np.concatenate(gr).mean(0)) - 1
    thf = fit_pl(B, n); thr = fit_pl(B[:, ::-1], n)
    from scipy.stats import spearmanr
    rho = spearmanr(thf, -thr)[0]
    out[name] = {"n_complete": N, "fwd": gf.round(3).tolist(), "rev": gr.round(3).tolist(), "rho_fwd_vs_rev_worths": round(float(rho), 3),
                 "fwd_top5": round(float(gf[:5].mean()), 3), "rev_top5": round(float(gr[:5].mean()), 3),
                 "fwd_bottom5": round(float(gf[-6:-1].mean()), 3), "rev_bottom5": round(float(gr[-6:-1].mean()), 3)}
    o = out[name]
    print(f"{name:24s} N={N:3d} rho={o['rho_fwd_vs_rev_worths']:.2f}  top5 fwd {o['fwd_top5']:+.3f} rev {o['rev_top5']:+.3f}   bottom5 fwd {o['fwd_bottom5']:+.3f} rev {o['rev_bottom5']:+.3f}")
json.dump(out, open("reverse_results.json", "w"), indent=1)
