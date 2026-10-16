"""Scale drift in the prior version of alternating JML, for nn_irt_estimation.qmd.

Runs alternating MAP/JML on the residency exam for a fixed number of sweeps, with no
early stopping, and records the person SD, the median slope and the held-out
log-loss every 100 sweeps. Writes nn_irt_data/drift_borges_alt_jml.csv.

    .venv/bin/python vignettes/nn_irt_estimation_drift.py
"""
import torch
import pandas as pd
import nn_irt_estimation as E
from nn_irt_helpers import DATA, prepare

SWEEPS = 2000


def main():
    torch.set_num_threads(4)
    d = prepare("borges_brazil_residency_2024_cbt")
    Y, M = E._cells(d)
    te = d[d.test]
    tp = torch.tensor(te.pid.values)
    ti = torch.tensor(te.iid.values)
    ty = torch.tensor(te.resp.values, dtype=E.DT)
    pc = ((Y * M).sum(0) / M.sum(0)).clamp(0.02, 0.98)
    dd = torch.log(pc / (1 - pc))
    a = torch.ones(Y.shape[1], dtype=E.DT)
    theta = torch.zeros(Y.shape[0], dtype=E.DT)
    rows = []
    for sweep in range(1, SWEEPS + 1):
        theta = E._person_newton(theta, a, dd, Y, M, 1.0)
        a, dd = E._item_newton(a, dd, theta, M, Y * M)
        if sweep % 100 == 0 or sweep == 1:
            p = torch.sigmoid(theta[tp] * a[ti] + dd[ti]).clamp(1e-9, 1 - 1e-9)
            ll = -(ty * torch.log(p) + (1 - ty) * torch.log(1 - p)).mean().item()
            rows.append({"sweep": sweep, "theta_sd": theta.std(unbiased=False).item(),
                         "median_a": a.median().item(), "heldout_logloss": ll})
    pd.DataFrame(rows).to_csv(DATA / "drift_borges_alt_jml.csv", index=False)


if __name__ == "__main__":
    main()
