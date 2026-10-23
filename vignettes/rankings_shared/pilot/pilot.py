"""Pilot for the rankings-family vignette (2026-10-15).

Two candidate questions, run on the 21 local rank tables:
  A. How much of a ranking is shared?  Plackett-Luce (PL) worths fitted on training
     ballots, scored on held-out ballots as sequential choices (IMV vs uniform);
     then a 2-component PL mixture to see what heterogeneity adds.
  B. Where in the ballot is the information?  (i) normalised entropy of the item
     placed at each position; (ii) held-out fit of PL trained on ballots truncated
     to their top-k marks, k = 1..K; (iii) truncation depth distribution.

Ballot prep: drop (overvote)/(write-in) rows, keep the first mark of a duplicated
item, order items by resp (gaps ignored).  Usage: python3 -I pilot.py <csv...>
"""
import sys, csv, math, collections, json
import numpy as np
from scipy.optimize import minimize

RESERVED = {"(overvote)", "(write-in)"}
RNG = np.random.default_rng(20261015)
RIDGE = 0.01

def load(path):
    rows = list(csv.DictReader(open(path, newline="")))
    by = collections.defaultdict(list)
    for r in rows:
        if r["item"] in RESERVED: continue
        by[(r["id"], r["task"])].append((int(r["resp"]), r["item"]))
    items = sorted({it for v in by.values() for _, it in v})
    idx = {it: i for i, it in enumerate(items)}
    ballots = []
    for v in by.values():
        v.sort(); seen = set(); b = []
        for _, it in v:
            if it in seen: continue
            seen.add(it); b.append(idx[it])
        if b: ballots.append(b)
    return items, ballots

# ---- Plackett-Luce (vectorised) -------------------------------------------------
def pad(ballots):
    K = max(len(b) for b in ballots)
    B = np.full((len(ballots), K), -1, int)
    for i, b in enumerate(ballots): B[i, :len(b)] = b
    return B

def _rem(theta, B):
    """remaining-worth sums at each position; mask of real marks."""
    w = np.exp(theta); mask = B >= 0
    wB = np.where(mask, w[np.where(mask, B, 0)], 0.0)
    rem = w.sum() - (np.cumsum(wB, 1) - wB)
    return w, mask, wB, rem

def pl_nll(theta, B, n, weights=None):
    w, mask, wB, rem = _rem(theta, B)
    tB = np.where(mask, theta[np.where(mask, B, 0)], 0.0)
    per = np.where(mask, tB - np.log(rem), 0.0).sum(1)
    wt = 1.0 if weights is None else weights
    return -(wt * per).sum() + RIDGE * np.sum(theta**2)

def pl_grad(theta, B, n, weights=None):
    w, mask, wB, rem = _rem(theta, B)
    N = B.shape[0]
    wt = np.ones(N) if weights is None else weights
    inv = np.where(mask, 1.0 / rem, 0.0)
    S = np.cumsum(inv, 1)                      # S_j = sum_{i<=j} 1/rem_i
    S_last = S[:, -1]                          # all positions of the ballot
    g = np.zeros(n)
    Bm = B[mask]; wtm = np.repeat(wt, mask.sum(1))
    np.add.at(g, Bm, wtm)                      # count term
    g -= w * (wt * S_last).sum()               # every item pays all positions ...
    np.add.at(g, Bm, w[Bm] * wtm * (np.repeat(S_last, mask.sum(1)) - S[mask]))  # ... ranked ones less
    return -g + 2 * RIDGE * theta

def fit_pl(B, n, weights=None, theta0=None):
    th = np.zeros(n) if theta0 is None else theta0
    res = minimize(pl_nll, th, args=(B, n, weights), jac=pl_grad, method="L-BFGS-B")
    th = res.x; return th - th.mean()

def choice_loglik(theta, B, n, depth=None):
    w, mask, wB, rem = _rem(theta, B)
    if depth is not None:
        mask = mask.copy(); mask[:, depth:] = False
    tB = np.where(mask, theta[np.where(mask, B, 0)], 0.0)
    lm = (tB - np.log(rem))[mask]
    k = n - np.arange(B.shape[1])
    lb = -np.log(np.broadcast_to(k, B.shape))[mask]
    return lm, lb

def imv_from_ll(l_model, l_base):
    """Pilot stand-in for IMV on K-way choices: the geometric-mean likelihood ratio,
    exp(mean(l_model - l_base)) - 1, i.e. how much more likely the model makes the
    observed next mark than the baseline, on average.  (The binary equivalent-coin
    IMV does not apply: a uniform K-way choice sits below log 0.5.)"""
    return float(np.exp((l_model - l_base).mean()) - 1)

# ---- mixture of PL ------------------------------------------------------------
def ballot_ll(theta, B):
    w, mask, wB, rem = _rem(theta, B)
    tB = np.where(mask, theta[np.where(mask, B, 0)], 0.0)
    return np.where(mask, tB - np.log(rem), 0.0).sum(1)

def fit_mix(B, n, K=2, iters=20):
    N = B.shape[0]
    resp = RNG.dirichlet(np.ones(K), N)
    thetas = [fit_pl(B, n, weights=resp[:, k]) for k in range(K)]
    pi = resp.mean(0)
    for _ in range(iters):
        L = np.stack([ballot_ll(thetas[k], B) for k in range(K)], 1) + np.log(pi)
        L -= L.max(1, keepdims=True); resp = np.exp(L); resp /= resp.sum(1, keepdims=True)
        pi = resp.mean(0) + 1e-9
        thetas = [fit_pl(B, n, weights=resp[:, k], theta0=thetas[k]) for k in range(K)]
    return pi, thetas

def mix_choice_loglik(pi, thetas, B, n):
    """Sequential-choice log-lik under a mixture: posterior over components given
    the earlier marks, then the mixture probability of the next mark."""
    K = len(pi); N, P = B.shape
    per = []   # per component: N x P log p of each mark
    for k in range(K):
        w, mask, wB, rem = _rem(thetas[k], B)
        tB = np.where(mask, thetas[k][np.where(mask, B, 0)], 0.0)
        per.append(np.where(mask, tB - np.log(rem), 0.0))
    per = np.stack(per, 2)                       # N x P x K
    prior = np.cumsum(per, 1) - per + np.log(pi) # log posterior before each mark
    prior -= prior.max(2, keepdims=True)
    post = np.exp(prior); post /= post.sum(2, keepdims=True)
    lp = np.log((post * np.exp(per)).sum(2))
    return lp[mask]

# ---- analyses -------------------------------------------------------------------
def folds(N, k=5):
    perm = RNG.permutation(N); return [perm[i::k] for i in range(k)]

def question_a(ballots, n):
    N = len(ballots); fs = folds(N); B = pad(ballots)
    lm, lb, lmix = [], [], []
    for f in fs:
        test = np.zeros(N, bool); test[f] = True
        tr, te = B[~test], B[test]
        th = fit_pl(tr, n); a, b_ = choice_loglik(th, te, n); lm.append(a); lb.append(b_)
        if N >= 150:
            pi, ths = fit_mix(tr, n, K=2); lmix.append(mix_choice_loglik(pi, ths, te, n))
    lm, lb = np.concatenate(lm), np.concatenate(lb)
    out = {"gain_pl_vs_uniform": imv_from_ll(lm, lb),
           "ll_gain_per_choice": float((lm - lb).mean())}
    if lmix:
        lmix = np.concatenate(lmix)
        out["gain_mix2_vs_pl"] = imv_from_ll(lmix, lm)
        out["ll_gain_mix2_per_choice"] = float((lmix - lm).mean())
    # first-choice-only IMV for comparison
    th = fit_pl(B, n); a, b_ = choice_loglik(th, B, n, depth=1)
    out["gain_first_choice_insample"] = imv_from_ll(a, b_)
    return out

def question_b(ballots, n):
    N = len(ballots); K = max(len(b) for b in ballots)
    # (i) entropy by position, normalised by log(n - p + 1)
    ent = []
    for p in range(K):
        c = collections.Counter(b[p] for b in ballots if len(b) > p)
        tot = sum(c.values())
        if tot < 20: ent.append(None); continue
        h = -sum(v / tot * math.log(v / tot) for v in c.values())
        ent.append(round(h / math.log(min(n - p, len(c)) if n - p > 1 else 2), 3))
    # (ii) depth curve: train on top-k truncations, test full held-out ballots
    fs = folds(N); depth = {}; B = pad(ballots)
    ks = [k for k in range(1, K + 1) if k <= 6 or k == K or k % 4 == 0]
    for k in ks:
        lm, lb = [], []
        for f in fs:
            test = np.zeros(N, bool); test[f] = True
            tr, te = B[~test][:, :k], B[test]
            th = fit_pl(tr, n); a, b_ = choice_loglik(th, te, n); lm.append(a); lb.append(b_)
        depth[k] = round(imv_from_ll(np.concatenate(lm), np.concatenate(lb)), 4)
    # (iii) truncation: marks per ballot
    marks = collections.Counter(len(b) for b in ballots)
    return {"entropy_by_position": ent, "gain_by_training_depth": depth,
            "marks_per_ballot": dict(sorted(marks.items()))}

if __name__ == "__main__":
    results = {}
    for path in sys.argv[1:]:
        name = path.split("/")[-1].replace(".csv", "")
        items, ballots = load(path); n = len(items)
        if n < 3: print(name, "skipped: <3 candidates"); continue
        r = {"n_items": n, "n_ballots": len(ballots)}
        r.update(question_a(ballots, n)); r.update(question_b(ballots, n))
        results[name] = r
        print(name, json.dumps({k: v for k, v in r.items() if k not in ("entropy_by_position", "gain_by_training_depth", "marks_per_ballot")}), flush=True)
    json.dump(results, open("results.json", "w"), indent=1)
