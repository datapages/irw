#!/usr/bin/env python3
"""Hold the Python counts against the R counts.

The briefing's promise is that both packages return the same tables for the same filter, so the
two sides must agree on every count they both report. This needs no expected values, which is
what makes it survive corpus versions: it fails on divergence, whichever side is wrong.

    python3 compare.py counts_python.json counts_r.json

On 2026-09-05 this would have printed n_categories=2: python 4229, r 451, and exited 1.

Two counts files can only be held against each other if they saw the same catalogue, so a
difference in n_tables is refused rather than reported as disagreement: it means the corpus moved
between the two runs, or one of the files is left over from an earlier one.
"""
import json
import sys


def main(py_path: str, r_path: str) -> int:
    py = json.load(open(py_path))
    r = json.load(open(r_path))

    py_n, r_n = py.get("n_tables"), r.get("n_tables")
    if py_n is not None and r_n is not None and py_n != r_n:
        print(f"refusing to compare: the runs saw different catalogues ({py_n} tables on the python "
              f"side, {r_n} on the r side). The corpus moved between them, or one file is left over "
              "from an earlier run. Rerun both sides.")
        return 1
    if py_n is None or r_n is None:
        print("note: one side reports no catalogue size, so a leftover file cannot be detected here")
    for side, obj in (("python", py), ("r", r)):
        if obj.get("written_at"):
            print(f"{side} counts written {obj['written_at']}")

    rows = []
    for key in sorted(set(py["filters"]) & set(r["filters"])):
        rows.append((f"filter({key})", py["filters"][key], r["filters"][key]))
    if py.get("quota_guard_pass") is not None and r.get("quota_guard_pass") is not None:
        rows.append(("n_responses=[0, 1e6] passes", py["quota_guard_pass"], r["quota_guard_pass"]))
    if not rows:
        print("nothing to compare: no shared keys (a run with no REDIVIS_API_TOKEN writes no counts)")
        return 1
    bad = 0
    print(f"{'check':40} {'python':>8} {'r':>8}")
    for name, a, b in rows:
        flag = "" if a == b else "   <-- differ"
        bad += a != b
        print(f"{name:40} {a:>8} {b:>8}{flag}")
    print(f"\npython irw {py.get('irw_version')} vs R irw {r.get('irw_version')}: "
          f"{len(rows) - bad} agree, {bad} differ")
    return 1 if bad else 0


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(__doc__)
        sys.exit(2)
    sys.exit(main(sys.argv[1], sys.argv[2]))
