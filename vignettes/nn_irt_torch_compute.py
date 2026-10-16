"""Fit every PyTorch network, prior and constrained versions, on every table.

Writes nn_irt_data/<table>_torch_preds.csv.gz, which nn_irt.qmd reads (and
refits only when the cache is missing). Tables run in parallel processes.

    .venv/bin/python vignettes/nn_irt_torch_compute.py
"""
import os
from multiprocessing import Pool

os.environ.setdefault("OMP_NUM_THREADS", "2")


def run(name):
    import torch
    from nn_irt_helpers import prepare, torch_preds

    torch.set_num_threads(2)
    d = prepare(name)
    torch_preds(name, d)
    return name


if __name__ == "__main__":
    import sys
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    from nn_irt_helpers import TABLES
    with Pool(6) as pool:
        for name in pool.imap_unordered(run, TABLES):
            print("done", name, flush=True)
