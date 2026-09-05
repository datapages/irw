#!/usr/bin/env python3
"""Repeatable check of the agents briefing (itemresponsewarehouse.org/llms.txt) against the Python package.

The failure mode this is written for: everything runs, nothing errors, and the numbers are wrong.
So every check here asserts a property that a silent no-op cannot satisfy, and the script exits 1
if any FAIL is recorded. WARNs do not fail the run: they flag briefing text that may have gone stale.

Needs Python 3.9+ (the package's own floor), the package installed with the documented line, and
REDIVIS_API_TOKEN in the environment. Downloads no response tables: it only touches the catalogue and
the metadata tables, so it spends no Redivis quota.

    python3 -m venv venv && ./venv/bin/pip install "git+https://github.com/itemresponsewarehouse/Python-pkg.git"
    REDIVIS_API_TOKEN=... ./venv/bin/python check_briefing.py [--json counts_python.json]

The counts go to a small JSON so compare.py can hold them against the R side: the two packages are
meant to return the same tables for the same filter, and that comparison needs no expected values.
"""
from __future__ import annotations

import argparse
import inspect
import json
import sys
import warnings

RESULTS: list[tuple[str, str, str]] = []


def record(status: str, name: str, detail: str = "") -> None:
    RESULTS.append((status, name, detail))
    print(f"[{status}] {name}" + (f": {detail}" if detail else ""))


def check(cond: bool, name: str, detail: str = "") -> None:
    record("PASS" if cond else "FAIL", name, detail)


def warn_if(cond: bool, name: str, detail: str = "") -> None:
    record("WARN" if cond else "PASS", name, detail if cond else "")


def main(json_path: str | None = "counts_python.json") -> int:
    warnings.simplefilter("always")
    print("python", sys.version.split()[0])
    guard_n: int | None = None
    counts: dict[str, int] = {}
    version = None

    # 1. Import, and one version number the user can read back.
    try:
        import irw
    except Exception as e:  # noqa: BLE001
        record("FAIL", "import irw", f"{type(e).__name__}: {e}")
        return report()
    version = getattr(irw, "__version__", None)
    check(isinstance(version, str) and version not in ("", "0.0.0"), "irw.__version__ is set", str(version))

    # 2. The catalogue arrives, and the metadata merge arrives with it (sections 3 and 5 depend on it).
    names = irw.list_tables()
    total = len(names)
    check(total > 1000, "list_tables() returns the catalogue", f"{total} tables")
    try:
        meta = irw.list_tables(include_metadata=True)
        meta_cols = set(meta.columns)
        needed = {"n_responses", "n_categories", "density", "n_items"}
        check(needed <= meta_cols, "list_tables(include_metadata=True) carries the numeric metadata",
              f"{meta.shape[1]} columns, missing {sorted(needed - meta_cols) or 'none'}")
    except Exception as e:  # noqa: BLE001
        meta = None
        record("FAIL", "list_tables(include_metadata=True)", f"raised {type(e).__name__}: {str(e)[:160]}")

    # 3. Filters must filter. A no-op returns the whole catalogue for every filter; that is the bug
    #    this file exists for. "Strictly fewer" is not enough: on 0.0.2 the no-op returned 4,229 of
    #    4,230 (one duplicate name dropped), so the bar is at most 98% of the catalogue, for each
    #    filter, and not all the same count. Every filter below is expected far under that.
    NOOP_FRACTION = 0.98
    filters = {
        "n_categories=2": dict(n_categories=2),
        "n_categories=2, density=None": dict(n_categories=2, density=None),
        "n_responses=[0, 1000]": dict(n_responses=[0, 1000]),
        "n_items=[100, None]": dict(n_items=[100, None]),
        "density=[0.99, 1]": dict(density=[0.99, 1]),
        "var='rt'": dict(var="rt"),
    }
    for label, kw in filters.items():
        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                counts[label] = len(irw.filter(**kw))
            check(counts[label] <= NOOP_FRACTION * total, f"filter({label}) filters", f"{counts[label]} of {total}")
        except Exception as e:  # noqa: BLE001
            record("FAIL", f"filter({label})", f"raised {type(e).__name__}: {str(e)[:160]}")
    if len(counts) >= 3:
        check(len(set(counts.values())) > 1, "different filters give different counts", str(counts))
    if "n_categories=2" in counts and "n_categories=2, density=None" in counts:
        check(counts["n_categories=2, density=None"] >= counts["n_categories=2"],
              "dropping the default density filter never removes tables",
              f"{counts['n_categories=2']} -> {counts['n_categories=2, density=None']}")

    # 4. The quota guard of section 3 must actually exclude the big tables.
    if meta is not None and "n_responses" in meta.columns:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            guarded = set(irw.filter(n_responses=[0, 1e6]))
        big = set(meta.loc[meta["n_responses"] >= 1e6, "name"])
        guard_n = len(guarded)
        leaked = sorted(guarded & big)
        check(not leaked, "n_responses=[0, 1e6] excludes every table at 1M+ rows",
              f"{len(guarded)} tables pass, {len(big)} are 1M+, leaked: {leaked[:5]}")
        check("criticalperiod_syntax" not in guarded, "the largest table does not pass the quota guard")

    # 5. info() on a known table must return, not raise.
    try:
        import contextlib
        import io
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            irw.info("4thgrade_math_sirt")
        check("4thgrade_math_sirt" in buf.getvalue(), "info('4thgrade_math_sirt') prints the record")
    except Exception as e:  # noqa: BLE001
        record("FAIL", "info('4thgrade_math_sirt')", f"raised {type(e).__name__}: {str(e)[:160]}")

    # 6. version() reports the corpus version.
    try:
        v = irw.version()
        check(len(v) > 0, "version() returns the corpus version", f"{len(v)} rows")
    except Exception as e:  # noqa: BLE001
        record("FAIL", "version()", f"raised {type(e).__name__}: {str(e)[:160]}")

    # 7. Section 9 says what Python does not have. If any of these appears, the briefing is stale
    #    in the other direction and section 9 should be updated, so these are WARNs, not FAILs.
    for fn in ("covariates", "table_sets", "check_resp"):
        warn_if(hasattr(irw, fn), f"section 9 says Python has no {fn}()", "it now exists: update section 9")
    sig = inspect.signature(irw.long2resp)
    warn_if("resp_col" in sig.parameters, "section 9 says long2resp() has no resp_col", "it now does: update section 9")
    try:
        irw.list_tables(source="nom")
        record("WARN", "section 9 says source='nom' is unreachable", "it is reachable now: update section 9")
    except Exception:  # noqa: BLE001
        record("PASS", "section 9 says source='nom' is unreachable", "still unreachable")

    # 8. Pitfall 2, offline. The briefing says long2resp() drops every cov_* column and, by default,
    #    every id answering under 10% of the items, and that Python has no covariates() to reattach
    #    them aligned. A toy frame checks the documented behaviour without touching the warehouse.
    try:
        import pandas as pd
        items = [f"i{k}" for k in range(20)]
        rows = [{"id": 1, "item": it, "resp": 1, "cov_g": "a"} for it in items]
        rows += [{"id": 2, "item": it, "resp": 0, "cov_g": "b"} for it in items]
        rows += [{"id": 3, "item": "i0", "resp": 1, "cov_g": "c"}]  # 1 of 20 items: 5%
        toy = pd.DataFrame(rows)

        def ids_of(frame):
            return set(frame["id"]) if "id" in frame.columns else set(frame.index)

        wide = irw.long2resp(toy)
        warn_if("cov_g" in wide.columns, "pitfall 2: long2resp() drops cov_* columns",
                "it now keeps them: update sections 2 and 9")
        check(3 not in ids_of(wide), "pitfall 2: the default id_density_threshold drops an id answering 5% of items",
              f"ids kept: {sorted(ids_of(wide))}")
        wide_all = irw.long2resp(toy, id_density_threshold=None)
        check(3 in ids_of(wide_all), "pitfall 2: id_density_threshold=None keeps that id")
    except Exception as e:  # noqa: BLE001
        record("FAIL", "pitfall 2 toy check", f"raised {type(e).__name__}: {str(e)[:160]}")

    if json_path:
        with open(json_path, "w") as f:
            json.dump({"package": "python", "irw_version": version, "filters": counts,
                       "quota_guard_pass": guard_n}, f, indent=2)
        print(f"counts written to {json_path}")

    return report()


def report() -> int:
    fails = [r for r in RESULTS if r[0] == "FAIL"]
    warns = [r for r in RESULTS if r[0] == "WARN"]
    print(f"\n{len(RESULTS)} checks: {len(RESULTS) - len(fails) - len(warns)} pass, {len(warns)} warn, {len(fails)} fail")
    return 1 if fails else 0


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--json", default="counts_python.json", help="where to write the counts (empty string to skip)")
    args = ap.parse_args()
    sys.exit(main(args.json or None))
