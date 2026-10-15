"""Helpers for vignettes/nn_irt.qmd (item response models as neural networks).

Everything the page and the offline R script share lives here: the table list,
the cell-level holdout mask, the IMV, and the small PyTorch models.
"""
from __future__ import annotations

import os
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
DATA = HERE / "nn_irt_data"
SEED = 20261015
TEST_FRAC = 0.2
MAX_PERSONS = 5000   # larger tables are subsampled so every fit stays laptop-sized
MAX_ITEMS = 116

GROUPS = {
    "Achievement / ability test": [
        "borges_brazil_residency_2024_cbt", "encceja_2024_ef_lp",
        "wolbers_2017_listening", "anunciacao_2024_intelligence_gmi", "psychtools_ability",
        "fullscaleiq_vocab_options",
    ],
    "Knowledge test, heterogeneous content": [
        "onah_2021_covid_knowledge", "ding_2020_covid_knowledge",
        "bialowolski_2024_financial_literacy", "art",
    ],
    "Test with known multidimensional structure": [
        "cdm_ecpe", "fullscaleiq_mentalrotation_options", "choi_2026_cmsce_2021_2",
    ],
    "Personality / clinical inventory": [
        "psychoneurotic_inventory", "narcissistic_personality_inventory", "tma", "psychtools_epi",
        "mcmi_mokken", "condon_2025_caq", "domaradzka_2018_adq_anhedonic_depression",
        "pinheiro_2024_srq20", "elshafie_2020_edsc",
    ],
    "Developmental screener": ["dscore_asq_weber_2019", "dscore_denver_weber_2019"],
}
TABLES = [t for ts in GROUPS.values() for t in ts]
GROUP_OF = {t: g for g, ts in GROUPS.items() for t in ts}
SHORT = {
    "borges_brazil_residency_2024_cbt": "residency exam (BR)", "encceja_2024_ef_lp": "ENCCEJA EF Portuguese",
    "wolbers_2017_listening": "listening test (NL)",
    "anunciacao_2024_intelligence_gmi": "GMI intelligence", "psychtools_ability": "psychTools ability",
    "fullscaleiq_vocab_options": "FSIQ vocabulary",
    "onah_2021_covid_knowledge": "COVID knowledge (Onah)",
    "ding_2020_covid_knowledge": "COVID knowledge (Ding)", "bialowolski_2024_financial_literacy": "financial literacy",
    "art": "author recognition", "cdm_ecpe": "ECPE grammar", "fullscaleiq_mentalrotation_options": "FSIQ mental rotation",
    "choi_2026_cmsce_2021_2": "CMSCE medical exam", "psychoneurotic_inventory": "Woodworth inventory",
    "narcissistic_personality_inventory": "NPI", "tma": "Taylor anxiety", "psychtools_epi": "Eysenck EPI",
    "mcmi_mokken": "MCMI", "condon_2025_caq": "creative achievement", "domaradzka_2018_adq_anhedonic_depression": "ADQ anhedonia",
    "pinheiro_2024_srq20": "SRQ-20", "elshafie_2020_edsc": "EDSC", "dscore_asq_weber_2019": "ASQ (D-score)",
    "dscore_denver_weber_2019": "Denver (D-score)",
}


# ---------------------------------------------------------------- data ----
def load_table(name: str) -> pd.DataFrame:
    """Fetch an IRW table (id, item, resp), caching a parquet copy locally.

    The cache is the file the offline R script reads, so Python and R see the
    same rows. Fetched once; a fresh fetch at a later IRW version may differ.
    """
    DATA.mkdir(exist_ok=True)
    path = DATA / f"{name}.parquet"
    if path.exists():
        return pd.read_parquet(path)
    import irw

    df = irw.fetch(name, columns=["id", "item", "resp"]).dropna()
    df["id"] = df["id"].astype(str)
    df["item"] = df["item"].astype(str)
    df["resp"] = df["resp"].astype(int)
    df = df.drop_duplicates(["id", "item"]).reset_index(drop=True)
    vals = set(df["resp"].unique())
    if vals == {1, 2}:            # forced-choice binaries coded 1/2 (e.g. the NPI)
        df["resp"] = df["resp"] - 1
        vals = {0, 1}
    if not vals <= {0, 1}:
        raise ValueError(f"{name} is not dichotomous: resp values {sorted(vals)}")
    df.to_parquet(path, index=False)
    return df


def index_table(df: pd.DataFrame) -> pd.DataFrame:
    """Add integer person/item indices (0-based, sorted by label)."""
    df = df.copy()
    df["pid"] = pd.Categorical(df["id"], categories=sorted(df["id"].unique())).codes.astype(np.int64)
    df["iid"] = pd.Categorical(df["item"], categories=sorted(df["item"].unique())).codes.astype(np.int64)
    return df


def holdout_mask(df: pd.DataFrame, seed: int = SEED, frac: float = TEST_FRAC) -> np.ndarray:
    """Boolean mask over rows of df: True = held-out cell.

    Cells are sampled at random within each person so that every person keeps
    at least one training response; items are dense enough that every item
    keeps hundreds. The same mask is written to disk for R.
    """
    rng = np.random.default_rng(seed)
    test = np.zeros(len(df), dtype=bool)
    for _, idx in df.groupby("pid").indices.items():
        k = int(round(frac * len(idx)))
        k = min(k, len(idx) - 1)
        if k > 0:
            test[rng.choice(idx, size=k, replace=False)] = True
    return test


def subsample(df: pd.DataFrame, seed: int = SEED) -> pd.DataFrame:
    """Cap persons at MAX_PERSONS and items at MAX_ITEMS, sampled at random."""
    rng = np.random.default_rng(seed)
    ids = np.sort(df["id"].unique())
    items = np.sort(df["item"].unique())
    if len(ids) > MAX_PERSONS:
        df = df[df["id"].isin(rng.choice(ids, MAX_PERSONS, replace=False))]
    if len(items) > MAX_ITEMS:
        df = df[df["item"].isin(rng.choice(items, MAX_ITEMS, replace=False))]
    return df.reset_index(drop=True)


def prepare(name: str) -> pd.DataFrame:
    """Load, subsample, index and split one table; write the split for R."""
    df = subsample(load_table(name))
    # drop items that are constant (every respondent answered the same way):
    # they carry no information and break mirt
    var = df.groupby("item")["resp"].nunique()
    df = df[df["item"].isin(var.index[var > 1])]
    df = index_table(df)
    df["test"] = holdout_mask(df)
    # an item constant within the training cells is unfittable: move one
    # held-out cell of the minority response back into training
    tr = df[~df.test]
    nun = tr.groupby("iid")["resp"].nunique()
    for iid in nun.index[nun < 2]:
        only = tr.loc[tr.iid == iid, "resp"].iloc[0]
        cand = df.index[(df.iid == iid) & df.test & (df.resp != only)]
        if len(cand):
            df.loc[cand[0], "test"] = False
    out = DATA / f"{name}_split.csv.gz"
    if not out.exists():
        df[["id", "item", "resp", "test"]].to_csv(out, index=False)
    return df


# ----------------------------------------------------------------- imv ----
def log_loss(y: np.ndarray, p: np.ndarray, eps: float = 1e-6) -> float:
    p = np.clip(p, eps, 1 - eps)
    return float(-np.mean(y * np.log(p) + (1 - y) * np.log(1 - p)))


def equivalent_coin(ll: float) -> float:
    """w such that a coin with P(heads)=w has mean log-likelihood -ll."""
    from scipy.optimize import brentq

    ll = min(ll, np.log(2) - 1e-9)   # a model worse than a coin counts as a coin
    f = lambda w: w * np.log(w) + (1 - w) * np.log(1 - w) + ll
    return brentq(f, 0.5, 1 - 1e-9)


def imv(y: np.ndarray, p0: np.ndarray, p1: np.ndarray) -> float:
    """InterModel Vigorish of model 1 over model 0 on held-out cells."""
    w0 = equivalent_coin(log_loss(y, p0))
    w1 = equivalent_coin(log_loss(y, p1))
    return (w1 - w0) / w0


# -------------------------------------------------------------- models ----
def make_models():
    import torch
    import torch.nn as nn

    class Rasch(nn.Module):
        """logit P(correct) = theta_person + d_item  (d = -difficulty)."""

        def __init__(self, n_persons, n_items):
            super().__init__()
            self.theta = nn.Embedding(n_persons, 1)
            self.d = nn.Embedding(n_items, 1)
            nn.init.normal_(self.theta.weight, std=0.1)
            nn.init.zeros_(self.d.weight)

        def forward(self, p, i):
            return (self.theta(p) + self.d(i)).squeeze(-1)

    class TwoPL(nn.Module):
        """logit P(correct) = a_item * theta_person + d_item.

        Slope-intercept form, as mirt uses it. The slope is signed: items keyed
        against the trait get a negative a rather than being forced to zero.
        """

        def __init__(self, n_persons, n_items):
            super().__init__()
            self.theta = nn.Embedding(n_persons, 1)
            self.a = nn.Embedding(n_items, 1)
            self.d = nn.Embedding(n_items, 1)
            nn.init.normal_(self.theta.weight, std=0.1)
            nn.init.ones_(self.a.weight)
            nn.init.zeros_(self.d.weight)

        def forward(self, p, i):
            return (self.a(i) * self.theta(p) + self.d(i)).squeeze(-1)

    class Bilinear(nn.Module):
        """MIRT: k-dimensional 2PL, logit = a_item . theta_person + d_item."""

        def __init__(self, n_persons, n_items, k):
            super().__init__()
            self.theta = nn.Embedding(n_persons, k)
            self.a = nn.Embedding(n_items, k)
            self.d = nn.Embedding(n_items, 1)
            nn.init.normal_(self.theta.weight, std=0.1)
            nn.init.normal_(self.a.weight, mean=1.0 / k ** 0.5, std=0.1)
            nn.init.zeros_(self.d.weight)

        def forward(self, p, i):
            return (self.theta(p) * self.a(i)).sum(-1) + self.d(i).squeeze(-1)

    class MLP(nn.Module):
        """Concatenate k-dim person and item embeddings, decode with an MLP of the given hidden widths."""

        def __init__(self, n_persons, n_items, k, hidden=(32, 32)):
            super().__init__()
            self.theta = nn.Embedding(n_persons, k)
            self.v = nn.Embedding(n_items, k)
            nn.init.normal_(self.theta.weight, std=0.1)
            nn.init.normal_(self.v.weight, std=0.1)
            layers, d_in = [], 2 * k
            for h in hidden:
                layers += [nn.Linear(d_in, h), nn.ReLU()]
                d_in = h
            layers.append(nn.Linear(d_in, 1))
            self.net = nn.Sequential(*layers)

        def forward(self, p, i):
            return self.net(torch.cat([self.theta(p), self.v(i)], -1)).squeeze(-1)

    return Rasch, TwoPL, Bilinear, MLP


def fit(model, df, epochs=80, lr=0.02, batch=4096, person_decay=1.0, seed=SEED,
        verbose=False):
    """Fit by minibatch Adam on the training cells.

    person_decay is an L2 penalty on person parameters (a normal prior, so the
    estimate is MAP rather than joint ML). Returns (model, history) where
    history has train and test log-loss per epoch.
    """
    import torch
    import torch.nn as nn

    torch.manual_seed(seed)
    g = torch.Generator().manual_seed(seed)
    tr, te = df[~df.test], df[df.test]
    P = torch.tensor(tr.pid.values)
    I = torch.tensor(tr.iid.values)
    Y = torch.tensor(tr.resp.values, dtype=torch.float32)
    counts = tr.groupby("pid").size()
    NP = torch.tensor(counts.reindex(tr.pid.values).values, dtype=torch.float32)
    Pt = torch.tensor(te.pid.values)
    It = torch.tensor(te.iid.values)
    Yt = torch.tensor(te.resp.values, dtype=torch.float32)
    loss_fn = nn.BCEWithLogitsLoss()
    opt = torch.optim.Adam(model.parameters(), lr=lr)
    n = len(tr)
    hist = []
    for ep in range(epochs):
        model.train()
        perm = torch.randperm(n, generator=g)
        for s in range(0, n, batch):
            j = perm[s:s + batch]
            opt.zero_grad()
            loss = loss_fn(model(P[j], I[j]), Y[j])
            # normal prior on persons: (lambda/2) * sum_p theta_p^2, spread over
            # each person's training responses so the minibatch mean is unbiased
            pen = (model.theta(P[j]) ** 2).sum(-1) / NP[j]
            loss = loss + person_decay / 2 * pen.mean()
            loss.backward()
            opt.step()
        model.eval()
        with torch.no_grad():
            hist.append((ep + 1,
                         loss_fn(model(P, I), Y).item(),
                         loss_fn(model(Pt, It), Yt).item()))
        if verbose:
            print(f"epoch {ep+1:3d}  train {hist[-1][1]:.4f}  test {hist[-1][2]:.4f}")
    return model, pd.DataFrame(hist, columns=["epoch", "train", "test"])


def predict(model, df):
    import torch

    model.eval()
    with torch.no_grad():
        z = model(torch.tensor(df.pid.values), torch.tensor(df.iid.values))
        return torch.sigmoid(z).numpy()
