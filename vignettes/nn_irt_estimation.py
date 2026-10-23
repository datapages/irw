"""Two ways to estimate the 2PL in torch, for vignettes/nn_irt_estimation.qmd.

alt_jml_2pl:  alternating joint maximum likelihood. Hold the item parameters
              fixed and update every person's theta (one Newton step block per
              sweep), then hold the persons fixed and update every item's (a, d).
              Repeat until the parameters stop moving. The prior version adds the
              N(0,1) penalty to each person's log-likelihood (MAP); the
              constrained version has no penalty and instead standardises theta
              across persons after each person update, moving the scale into a
              and d so the fitted probabilities do not change.

em_mml_2pl:   EM for marginal maximum likelihood, the estimator mirt uses. E-step:
              each person's posterior over a quadrature grid given their training
              cells. M-step: each item's (a, d) by weighted logistic regression on
              the grid, with the posterior weights as the data. Held-out cells get
              the posterior-predictive probability.

Both work on the training cells of a prepared table (see nn_irt_helpers.prepare)
and return predictions for the held-out cells in the same order as d[d.test].
Everything is float64 torch; the Newton steps are written out because each block
is a small separable problem.
"""
import torch
import torch.nn.functional as F

DT = torch.float64
A_BOUND = 6.0   # |a| bound, alternating JML only; see _item_newton


def _cells(d):
    """Persons x items matrices of responses (Y) and training indicators (M)."""
    n_p, n_i = int(d.pid.max() + 1), int(d.iid.max() + 1)
    tr = d[~d.test]
    p = torch.tensor(tr.pid.values)
    i = torch.tensor(tr.iid.values)
    Y = torch.zeros(n_p, n_i, dtype=DT)
    M = torch.zeros(n_p, n_i, dtype=DT)
    Y[p, i] = torch.tensor(tr.resp.values, dtype=DT)
    M[p, i] = 1.0
    return Y, M


def _item_loglik(a, dd, xx, n, s):
    """Per-item log-likelihood, sum_k s_k eta_k - n_k log(1 + exp(eta_k))."""
    eta = xx * a + dd
    return (s * eta - n * F.softplus(eta)).sum(0)


def _item_newton(a, dd, x, n, s, steps=8, cap=1.0, ridge=1e-6, halvings=12,
                 bound=float("inf")):
    """Damped Newton ascent on each item's (a, d).

    x:   (K,) covariate values (persons' theta, or the quadrature grid)
    n,s: (K, I) trial and success counts for each covariate value and item
    P_k = sigmoid(a_i x_k + d_i).

    The raw Newton step diverges on an item the persons separate: the likelihood
    rises without bound as the slope grows, so capping the step only slows the
    march. Three safeguards, all per item and all vectorised:
    a small ridge on the Hessian, backtracking (halve the step until the item's
    own log-likelihood does not fall), and a bound on |a| when the caller asks
    for one.

    The bound is off by default. EM's M-step maximises a marginal likelihood
    whose optimum is finite but can legitimately be large on an item the
    quadrature barely constrains, and bounding it there would change the EM and
    MML fits. `alt_jml_2pl` passes A_BOUND, matching the bound the person step
    already puts on theta, because unpenalised JML has no finite optimum on an
    item the persons separate; it reports how many items reach it.
    """
    xx = x[:, None]
    for _ in range(steps):
        P = torch.sigmoid(xx * a + dd)
        r = s - n * P
        w = n * P * (1 - P)
        gA = (r * xx).sum(0)
        gD = r.sum(0)
        hAA = (w * xx * xx).sum(0) + ridge
        hAD = (w * xx).sum(0)
        hDD = w.sum(0) + ridge
        det = (hAA * hDD - hAD * hAD).clamp_min(1e-12)
        step_a = ((hDD * gA - hAD * gD) / det).clamp(-cap, cap)
        step_d = ((hAA * gD - hAD * gA) / det).clamp(-cap, cap)
        ll0 = _item_loglik(a, dd, xx, n, s)
        t = torch.ones_like(a)
        for _ in range(halvings):
            a_t = (a + t * step_a).clamp(-bound, bound)
            dd_t = dd + t * step_d
            worse = _item_loglik(a_t, dd_t, xx, n, s) < ll0 - 1e-10
            if not bool(worse.any()):
                break
            t = torch.where(worse, t * 0.5, t)
        a = (a + t * step_a).clamp(-bound, bound)
        dd = dd + t * step_d
    return a, dd


def _person_newton(theta, a, dd, Y, M, prior, steps=5, cap=2.0, bound=6.0):
    """Newton ascent on each person's theta given the items (prior = 1 gives the N(0,1) MAP)."""
    for _ in range(steps):
        P = torch.sigmoid(theta[:, None] * a + dd)
        g = ((Y - P) * M * a).sum(1) - prior * theta
        h = (P * (1 - P) * M * a * a).sum(1) + prior
        theta = (theta + (g / h).clamp(-cap, cap)).clamp(-bound, bound)
    return theta


def alt_jml_2pl(d, prior=True, max_sweeps=5000, tol=1e-5, patience=100):
    """Alternating JML (MAP if prior=True, constrained if prior=False).

    Stopping: the sweep with the lowest held-out log-loss is kept, as the torch
    fits keep their best epoch. The run stops early when the fitted logits stop
    moving (change < tol), or after `patience` sweeps with no held-out gain.
    Unpenalised JML has no finite optimum when an item separates the persons, so
    on some tables the item slopes diverge past the best sweep; the best sweep is
    what is reported. info records it.
    """
    Y, M = _cells(d)
    n_p, n_i = Y.shape
    te = d[d.test]
    tp = torch.tensor(te.pid.values)
    ti = torch.tensor(te.iid.values)
    ty = torch.tensor(te.resp.values, dtype=DT)
    pc = ((Y * M).sum(0) / M.sum(0)).clamp(0.02, 0.98)
    dd = torch.log(pc / (1 - pc))
    a = torch.ones(n_i, dtype=DT)
    theta = torch.zeros(n_p, dtype=DT)
    pr = 1.0 if prior else 0.0
    best = (float("inf"), None, None, None, 0)
    delta, sweep, stopped = float("inf"), 0, "cap"
    for sweep in range(1, max_sweeps + 1):
        eta_old = theta[:, None] * a + dd
        theta = _person_newton(theta, a, dd, Y, M, pr)
        if not prior:
            m, s = theta.mean(), theta.std(unbiased=False)
            theta = (theta - m) / s
            # theta_old = m + s * theta_new, so a*theta_old + d = (a*s)*theta_new + (d + a*m)
            a, dd = a * s, dd + a * m
        a, dd = _item_newton(a, dd, theta, M, Y * M, bound=A_BOUND)
        # convergence on the fitted logits, not the parameters: the prior version
        # can drift along the scale ridge (theta shrinking, a growing) while a*theta is fixed
        eta = theta[:, None] * a + dd
        delta = (eta - eta_old).abs().max().item()
        p = torch.sigmoid(eta[tp, ti]).clamp(1e-9, 1 - 1e-9)
        ll = -(ty * torch.log(p) + (1 - ty) * torch.log(1 - p)).mean().item()
        if ll < best[0]:
            best = (ll, theta.clone(), a.clone(), dd.clone(), sweep)
        if delta < tol:
            stopped = "tol"
            break
        if sweep - best[4] >= patience:
            stopped = "patience"
            break
    _, theta, a, dd, best_sweep = best
    info = {"sweeps": sweep, "best_sweep": best_sweep, "stopped": stopped,
            "heldout_logloss": best[0], "last_change": delta,
            "theta_sd": theta.std(unbiased=False).item(), "median_a": a.median().item(),
            "max_a": a.abs().max().item(),
            "items_at_bound": int((a.abs() >= A_BOUND - 1e-9).sum())}
    return theta, a, dd, info


def alt_jml_predict(theta, a, dd, d):
    """Plug-in predictions for the held-out cells: sigmoid(a_i theta_p + d_i) at the point estimates."""
    p = torch.tensor(d[d.test].pid.values)
    i = torch.tensor(d[d.test].iid.values)
    return torch.sigmoid(theta[p] * a[i] + dd[i]).numpy()


def em_mml_2pl(d, n_quad=61, max_iter=5000, tol=1e-10):
    """EM for marginal maximum likelihood with a N(0,1) prior on theta.

    Returns (preds, a, dd, info) with preds the posterior-predictive probabilities
    for the held-out cells, in the order of d[d.test].
    """
    Y, M = _cells(d)
    n_p, n_i = Y.shape
    grid = torch.linspace(-4, 4, n_quad, dtype=DT)
    logw = -0.5 * grid * grid
    logw = logw - torch.logsumexp(logw, 0)            # normalised N(0,1) weights on the grid
    MY = M * Y
    MN = M - MY
    a = torch.ones(n_i, dtype=DT)
    dd = torch.zeros(n_i, dtype=DT)

    def estep(a, dd):
        logP = F.logsigmoid(grid[:, None] * a + dd)     # (grid, items)
        log1mP = F.logsigmoid(-(grid[:, None] * a + dd))
        LL = MY @ logP.T + MN @ log1mP.T + logw         # (persons, grid)
        return LL, torch.logsumexp(LL, 1).sum()

    prev, it, converged = -float("inf"), 0, False
    for it in range(1, max_iter + 1):
        LL, ll = estep(a, dd)
        if abs(ll.item() - prev) < tol * abs(ll.item()):
            converged = True
            break
        prev = ll.item()
        R = torch.softmax(LL, 1)
        n = R.T @ M                                      # expected trials, (grid, items)
        s = R.T @ MY                                     # expected successes
        a, dd = _item_newton(a, dd, grid, n, s, steps=10)
    LL, ll = estep(a, dd)
    R = torch.softmax(LL, 1)
    Pgrid = torch.sigmoid(grid[:, None] * a + dd)
    post = (R @ Pgrid).clamp(0, 1)                       # posterior-predictive, (persons, items)
    p = torch.tensor(d[d.test].pid.values)
    i = torch.tensor(d[d.test].iid.values)
    info = {"iterations": it, "converged": converged, "marginal_loglik": ll.item()}
    return post[p, i].numpy(), a, dd, info


def _posterior_predict(d, a, dd, grid):
    """Posterior-predictive probabilities for the held-out cells, given item parameters."""
    Y, M = _cells(d)
    logw = -0.5 * grid * grid
    logw = logw - torch.logsumexp(logw, 0)
    logP = F.logsigmoid(grid[:, None] * a + dd)
    log1mP = F.logsigmoid(-(grid[:, None] * a + dd))
    LL = (M * Y) @ logP.T + (M - M * Y) @ log1mP.T + logw
    R = torch.softmax(LL, 1)
    post = (R @ torch.sigmoid(grid[:, None] * a + dd)).clamp(0, 1)
    p = torch.tensor(d[d.test].pid.values)
    i = torch.tensor(d[d.test].iid.values)
    return post[p, i].numpy()


def mml_lbfgs_2pl(d, n_quad=61, em_iter=200, rounds=4):
    """Marginal maximum likelihood, polished with L-BFGS on the exact marginal likelihood.

    Starts from `em_iter` EM iterations, then maximises the marginal log-likelihood
    (N(0,1) quadrature prior, the same grid as em_mml_2pl) directly with torch
    autograd. EM alone stalls on tables with items that nearly separate the persons,
    where the likelihood is very flat; L-BFGS gets further along those ridges.
    Returns (preds, a, dd, info) as em_mml_2pl does.
    """
    Y, M = _cells(d)
    grid = torch.linspace(-4, 4, n_quad, dtype=DT)
    logw = -0.5 * grid * grid
    logw = logw - torch.logsumexp(logw, 0)
    MY = M * Y
    MN = M - MY

    def marg_ll(a, dd):
        logP = F.logsigmoid(grid[:, None] * a + dd)
        log1mP = F.logsigmoid(-(grid[:, None] * a + dd))
        return torch.logsumexp(MY @ logP.T + MN @ log1mP.T + logw, 1).sum()

    _, a0, d0, em_info = em_mml_2pl(d, n_quad=n_quad, max_iter=em_iter)
    a = a0.clone().requires_grad_(True)
    dd = d0.clone().requires_grad_(True)
    opt = torch.optim.LBFGS([a, dd], lr=1, max_iter=500, tolerance_grad=1e-9,
                            tolerance_change=1e-12, history_size=50, line_search_fn="strong_wolfe")

    def closure():
        opt.zero_grad()
        loss = -marg_ll(a, dd)
        loss.backward()
        return loss

    for _ in range(rounds):
        opt.step(closure)
    a, dd = a.detach(), dd.detach()
    info = {"em_iterations": em_info["iterations"], "marginal_loglik": marg_ll(a, dd).item(),
            "max_abs_d": dd.abs().max().item()}
    return _posterior_predict(d, a, dd, grid), a, dd, info
