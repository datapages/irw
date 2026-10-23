"""Fit the 2PL three ways on every table, for nn_irt_estimation.qmd.

Estimators (all on the page's training cells, predictions for the held-out cells):
  alt JML (prior)   alternating MAP / joint maximum likelihood, N(0,1) penalty on theta
  alt JML (constr.) alternating joint maximum likelihood, theta standardised each sweep
  EM MML            EM for marginal maximum likelihood (the mirt estimator)
  MML L-BFGS        EM start, then L-BFGS on the marginal likelihood (torch autograd)

Writes nn_irt_data/<table>_estimation_preds.csv.gz (y and one column per estimator)
and nn_irt_data/<table>_estimation_info.csv (sweeps, convergence, drift diagnostics).
Tables run in parallel processes.

    .venv/bin/python vignettes/nn_irt_estimation_compute.py
"""
import os
from multiprocessing import Pool

import pandas as pd


def run(name):
    import torch
    from nn_irt_helpers import DATA, FITS, prepare
    from nn_irt_estimation import alt_jml_2pl, alt_jml_predict, em_mml_2pl, mml_lbfgs_2pl

    torch.set_num_threads(2)
    d = prepare(name)
    y = d[d.test].resp.values
    th, a, dd, info_p = alt_jml_2pl(d, prior=True)
    p_ajp = alt_jml_predict(th, a, dd, d)
    th, a, dd, info_c = alt_jml_2pl(d, prior=False)
    p_ajc = alt_jml_predict(th, a, dd, d)
    p_em, _, _, info_em = em_mml_2pl(d)
    p_ml, _, _, info_ml = mml_lbfgs_2pl(d)
    pd.DataFrame({"y": y, "alt JML (prior)": p_ajp, "alt JML (constr.)": p_ajc,
                  "EM MML": p_em, "MML L-BFGS": p_ml}).to_csv(FITS / f"{name}_estimation_preds.csv.gz", index=False)
    info = pd.DataFrame([{"estimator": k, **v} for k, v in
                         [("alt JML (prior)", info_p), ("alt JML (constr.)", info_c), ("EM MML", info_em),
                          ("MML L-BFGS", info_ml)]])
    info.to_csv(DATA / f"{name}_estimation_info.csv", index=False)
    return name


if __name__ == "__main__":
    import sys
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    from nn_irt_helpers import TABLES
    with Pool(6) as pool:
        for name in pool.imap_unordered(run, TABLES):
            print("done", name, flush=True)
