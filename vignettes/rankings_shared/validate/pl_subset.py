"""Plackett-Luce under *subset* semantics, for comparison with the PlackettLuce R package.

The vignette's model (pilot.py) treats an unmarked candidate as available but unchosen:
the denominator at mark p is the worth of every candidate not yet marked, including the
ones the voter never marks.  The PlackettLuce package instead drops unranked items from
the ranking entirely, so a 3-mark ballot in a 6-candidate race is a complete ranking of
those 3.  The two likelihoods differ whenever a ballot is partial.

This module implements the package's semantics in our optimiser so the two can be compared
on the same objective.  Everything else -- log-worths, L-BFGS-B, ridge, mean-centring --
is shared with pilot.py.
"""
import numpy as np
from scipy.optimize import minimize

RIDGE = 0.0


def _rem(theta, B):
    """Per-ballot remaining-worth sums, summing only over that ballot's own marks."""
    w = np.exp(theta)
    mask = B >= 0
    wB = np.where(mask, w[np.where(mask, B, 0)], 0.0)
    rem = wB.sum(1, keepdims=True) - (np.cumsum(wB, 1) - wB)
    return w, mask, wB, rem


def nll(theta, B, n, weights=None):
    w, mask, wB, rem = _rem(theta, B)
    tB = np.where(mask, theta[np.where(mask, B, 0)], 0.0)
    per = np.where(mask, tB - np.log(np.where(mask, rem, 1.0)), 0.0).sum(1)
    wt = 1.0 if weights is None else weights
    return -(wt * per).sum() + RIDGE * np.sum(theta ** 2)


def grad(theta, B, n, weights=None):
    """d/dtheta_m = -count_m + w_m * sum_{ballots marking m} S_{i,pos(m)},
    where S_{i,j} = sum_{p<=j} 1/rem_{i,p}.  Unmarked items pay nothing -- that is the
    whole difference from the full-set gradient in pilot.py, where they pay every position.
    """
    w, mask, wB, rem = _rem(theta, B)
    wt = np.ones(B.shape[0]) if weights is None else weights
    inv = np.where(mask, 1.0 / np.where(mask, rem, 1.0), 0.0)
    S = np.cumsum(inv, 1)
    g = np.zeros(n)
    Bm = B[mask]
    wtm = np.repeat(wt, mask.sum(1))
    np.add.at(g, Bm, wtm)
    np.add.at(g, Bm, -w[Bm] * wtm * S[mask])
    return -g + 2 * RIDGE * theta


def fit(B, n, weights=None, theta0=None):
    th = np.zeros(n) if theta0 is None else theta0
    res = minimize(nll, th, args=(B, n, weights), jac=grad, method="L-BFGS-B")
    return res.x - res.x.mean()


def _gradcheck(seed=0):
    """Analytic gradient against finite differences, on ragged ballots."""
    from scipy.optimize import approx_fprime
    rng = np.random.default_rng(seed)
    n, N = 7, 40
    B = np.full((N, n), -1, int)
    for i in range(N):
        k = rng.integers(1, n + 1)
        B[i, :k] = rng.permutation(n)[:k]
    th = rng.normal(size=n)
    a = grad(th, B, n)
    f = approx_fprime(th, lambda t: nll(t, B, n), 1e-7)
    return float(np.abs(a - f).max())


if __name__ == "__main__":
    print("max |analytic - numeric| gradient:", _gradcheck())
