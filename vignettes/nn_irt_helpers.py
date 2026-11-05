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
# Heavy, regenerable caches (fetches, splits, predictions) live in a subdirectory
# that .gitignore excludes via vignettes/**/fits/. Small derived outputs that are
# costly to rebuild -- the mirt item parameters, the estimator diagnostics, the
# bibliography -- stay in DATA and are committed.
FITS = DATA / "fits"
SEED = 20261015
TEST_FRAC = 0.2   # share of each person's cells held out for scoring
VAL_FRAC = 0.1    # share held out for choosing the stopping epoch, never scored
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
    FITS.mkdir(parents=True, exist_ok=True)
    path = FITS / f"{name}.parquet"
    if path.exists():
        return pd.read_parquet(path)
    import irw

    df = irw.fetch(name, columns=["id", "item", "resp"]).dropna()
    df["id"] = df["id"].astype(str)
    df["item"] = df["item"].astype(str)
    resp = pd.to_numeric(df["resp"])
    if not (resp == resp.round()).all():   # check before casting, so 0.5 is not truncated to 0
        raise ValueError(f"{name} has non-integer resp values")
    df["resp"] = resp.astype(int)
    # a person-item pair answered twice keeps its first response; count the
    # pairs whose repeats disagree so the choice is visible rather than silent
    dup = df[df.duplicated(["id", "item"], keep=False)]
    n_conflict = int((dup.groupby(["id", "item"])["resp"].nunique() > 1).sum())
    if n_conflict:
        print(f"{name}: {n_conflict} person-item pairs with conflicting repeats; keeping the first")
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


def holdout_mask(df: pd.DataFrame, seed: int = SEED, frac: float = TEST_FRAC,
                 val_frac: float = VAL_FRAC) -> tuple[np.ndarray, np.ndarray]:
    """Two boolean masks over rows of df: (test, val).

    Within each person, TEST_FRAC of the cells are held out for scoring and a
    further VAL_FRAC for choosing when to stop training; the rest train. Every
    person keeps at least one training response; items are dense enough that
    every item keeps hundreds. Both masks are written to disk for R.
    """
    rng = np.random.default_rng(seed)
    test = np.zeros(len(df), dtype=bool)
    val = np.zeros(len(df), dtype=bool)
    for _, idx in df.groupby("pid").indices.items():
        idx = rng.permutation(idx)
        k = min(int(round(frac * len(idx))), len(idx) - 1)
        v = min(int(round(val_frac * len(idx))), len(idx) - 1 - k)
        test[idx[:k]] = True
        val[idx[k:k + v]] = True
    return test, val


def train_rows(df: pd.DataFrame) -> pd.DataFrame:
    """The training cells: neither held out for scoring nor for validation."""
    return df[~df.test & ~df.val]


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
    df["test"], df["val"] = holdout_mask(df)
    # An item constant within the training cells is unfittable, so one held-out
    # cell of the minority response is moved back into training (validation
    # cells first). This looks at held-out outcomes, so it is reported rather
    # than hidden: prepare() records how many cells it moved.
    tr = train_rows(df)
    nun = tr.groupby("iid")["resp"].nunique()
    moved = 0
    for iid in nun.index[nun < 2]:
        only = tr.loc[tr.iid == iid, "resp"].iloc[0]
        for col in ("val", "test"):
            cand = df.index[(df.iid == iid) & df[col] & (df.resp != only)]
            if len(cand):
                df.loc[cand[0], col] = False
                moved += 1
                break
    df.attrs["cells_moved_to_train"] = moved
    # always rewrite, so R can never read a split left over from older code
    df[["id", "item", "resp", "test", "val"]].to_csv(FITS / f"{name}_split.csv.gz", index=False)
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


def theta_rows(model, p):
    """Person parameters for the persons in p.

    prior version (constrained=False): the raw embedding rows; identification
    comes from the N(0,1) penalty applied in fit().

    constrained version: every column of the embedding is standardised over
    all persons (mean 0, SD 1) inside the forward pass, so theta is N(0,1) in
    the sample by construction and no penalty is needed. The standardisation
    uses the whole embedding, not each minibatch, so the constraint does not
    depend on which persons happen to be in a batch.
    """
    w = model.theta.weight
    if model.constrained:
        w = (w - w.mean(0, keepdim=True)) / w.std(0, unbiased=False, keepdim=True)
    return w[p]


def make_models():
    import torch
    import torch.nn as nn

    class Rasch(nn.Module):
        """logit P(correct) = c * theta_person + d_item  (d = -difficulty).

        c is a single slope shared by every item, so it is a Rasch model: the
        items still differ only in difficulty. It is free only in the
        constrained version, where theta is standardised to SD 1 and something
        has to carry the scale. In the prior version the spread of theta
        carries it, c would be redundant, and it is held at 1.
        """

        def __init__(self, n_persons, n_items, constrained=False):
            super().__init__()
            self.constrained = constrained
            self.theta = nn.Embedding(n_persons, 1)
            self.d = nn.Embedding(n_items, 1)
            self.c = nn.Parameter(torch.ones(1), requires_grad=constrained)
            nn.init.normal_(self.theta.weight, std=0.1)
            nn.init.zeros_(self.d.weight)

        def forward(self, p, i):
            return (self.c * theta_rows(self, p) + self.d(i)).squeeze(-1)

    class TwoPL(nn.Module):
        """logit P(correct) = a_item * theta_person + d_item.

        Slope-intercept form, as mirt uses it. The slope is signed: items keyed
        against the trait get a negative a rather than being forced to zero.
        """

        def __init__(self, n_persons, n_items, constrained=False):
            super().__init__()
            self.constrained = constrained
            self.theta = nn.Embedding(n_persons, 1)
            self.a = nn.Embedding(n_items, 1)
            self.d = nn.Embedding(n_items, 1)
            nn.init.normal_(self.theta.weight, std=0.1)
            nn.init.ones_(self.a.weight)
            nn.init.zeros_(self.d.weight)

        def forward(self, p, i):
            return (self.a(i) * theta_rows(self, p) + self.d(i)).squeeze(-1)

    class Bilinear(nn.Module):
        """MIRT: k-dimensional 2PL, logit = a_item . theta_person + d_item."""

        def __init__(self, n_persons, n_items, k, constrained=False):
            super().__init__()
            self.constrained = constrained
            self.theta = nn.Embedding(n_persons, k)
            self.a = nn.Embedding(n_items, k)
            self.d = nn.Embedding(n_items, 1)
            nn.init.normal_(self.theta.weight, std=0.1)
            nn.init.normal_(self.a.weight, mean=1.0 / k ** 0.5, std=0.1)
            nn.init.zeros_(self.d.weight)

        def forward(self, p, i):
            return (theta_rows(self, p) * self.a(i)).sum(-1) + self.d(i).squeeze(-1)

    class MLP(nn.Module):
        """Concatenate k-dim person and item embeddings, decode with an MLP of the given hidden widths."""

        def __init__(self, n_persons, n_items, k, hidden=(32, 32), constrained=False):
            super().__init__()
            self.constrained = constrained
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
            return self.net(torch.cat([theta_rows(self, p), self.v(i)], -1)).squeeze(-1)

    return Rasch, TwoPL, Bilinear, MLP


def fit(model, df, epochs=80, lr=0.02, batch=4096, person_decay=1.0, seed=SEED,
        keep_best=True, verbose=False):
    """Fit by minibatch Adam on the training cells, one optimiser for the whole run.

    person_decay is an N(0,1) penalty on the person parameters. With free item
    slopes it does not fix the scale of theta (theta/c with c*a fits as well and
    pays a smaller penalty), so what keeps the fit finite is early stopping:
    with keep_best the model is returned at the epoch with the lowest
    validation loss. The test cells are never used to choose anything; their
    loss is recorded only so the curves can show it.

    Seed the initialisation by calling torch.manual_seed before building the
    model; `seed` here controls only the order of the minibatches.

    Returns (model, history) with train, val and test log-loss per epoch.
    """
    import copy
    import torch
    import torch.nn as nn

    g = torch.Generator().manual_seed(seed)
    tr, va, te = train_rows(df), df[df.val], df[df.test]
    P = torch.tensor(tr.pid.values)
    I = torch.tensor(tr.iid.values)
    Y = torch.tensor(tr.resp.values, dtype=torch.float32)
    counts = tr.groupby("pid").size()
    NP = torch.tensor(counts.reindex(tr.pid.values).values, dtype=torch.float32)
    Pt = torch.tensor(te.pid.values)
    It = torch.tensor(te.iid.values)
    Yt = torch.tensor(te.resp.values, dtype=torch.float32)
    Pv = torch.tensor(va.pid.values)
    Iv = torch.tensor(va.iid.values)
    Yv = torch.tensor(va.resp.values, dtype=torch.float32)
    loss_fn = nn.BCEWithLogitsLoss()
    opt = torch.optim.Adam(model.parameters(), lr=lr)
    n = len(tr)
    hist = []
    best = (float("inf"), None)
    for ep in range(epochs):
        model.train()
        perm = torch.randperm(n, generator=g)
        for s in range(0, n, batch):
            j = perm[s:s + batch]
            opt.zero_grad()
            loss = loss_fn(model(P[j], I[j]), Y[j])
            if not model.constrained:
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
                         loss_fn(model(Pv, Iv), Yv).item(),
                         loss_fn(model(Pt, It), Yt).item()))
        if keep_best and hist[-1][2] < best[0]:
            best = (hist[-1][2], copy.deepcopy(model.state_dict()))
        if verbose:
            print(f"epoch {ep+1:3d}  train {hist[-1][1]:.4f}  val {hist[-1][2]:.4f}")
    if keep_best:
        model.load_state_dict(best[1])
    return model, pd.DataFrame(hist, columns=["epoch", "train", "val", "test"])


def predict(model, df):
    import torch

    model.eval()
    with torch.no_grad():
        z = model(torch.tensor(df.pid.values), torch.tensor(df.iid.values))
        return torch.sigmoid(z).numpy()


def best_epoch_preds(model_fn, d, epochs=30):
    """Train once, stop at the lowest validation loss, predict the test cells."""
    import torch

    torch.manual_seed(SEED)          # fixes the initialisation, whatever ran before
    model, _ = fit(model_fn(), d, epochs=epochs)
    return predict(model, d[d.test])


NN_NAMES = ["Rasch", "2PL", "MIRT k=2", "MLP small", "MLP large"]


def check_cells(cached: pd.DataFrame, te: pd.DataFrame, path) -> None:
    """Fail loudly if a prediction cache does not cover exactly the current test cells, in order."""
    ok = (len(cached) == len(te)
          and (cached["id"].astype(str).values == te["id"].values).all()
          and (cached["item"].astype(str).values == te["item"].values).all()
          and (cached["y"].values == te["resp"].values).all())
    if not ok:
        raise ValueError(f"{path} does not match the current split; delete it and refit")


def nn_specs(n_p, n_i, constrained):
    """Factories for the five networks, in the prior or the constrained version."""
    Rasch, TwoPL, Bilinear, MLP = make_models()
    return {
        "Rasch": lambda: Rasch(n_p, n_i, constrained),
        "2PL": lambda: TwoPL(n_p, n_i, constrained),
        "MIRT k=2": lambda: Bilinear(n_p, n_i, 2, constrained),
        "MLP small": lambda: MLP(n_p, n_i, 2, hidden=(16,), constrained=constrained),
        "MLP large": lambda: MLP(n_p, n_i, 2, hidden=(64, 64), constrained=constrained),
    }


def torch_preds(t, d, epochs=30):
    """Held-out predictions for every network, both versions, for one table.

    Keys: "id", "item", "y" (the test cells and their responses), then each
    model as NN_NAMES, with " (constr.)" appended for the constrained version.
    Cached as nn_irt_data/fits/<t>_torch_preds.csv.gz; delete the cache to
    refit. A cache whose cells do not match the current split is an error.
    """
    cache = FITS / f"{t}_torch_preds.csv.gz"
    te = d[d.test]
    if cache.exists():
        c = pd.read_csv(cache, dtype={"id": str, "item": str})
        check_cells(c, te, cache)
        return {k: c[k].values for k in c.columns}
    n_p, n_i = int(d.pid.max() + 1), int(d.iid.max() + 1)
    out = {"id": te.id.values, "item": te.item.values, "y": te.resp.values}
    for constrained in (False, True):
        suffix = " (constr.)" if constrained else ""
        for name, mk in nn_specs(n_p, n_i, constrained).items():
            out[name + suffix] = best_epoch_preds(mk, d, epochs)
    pd.DataFrame(out).to_csv(cache, index=False)
    return out
