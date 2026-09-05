#!/usr/bin/env python3
"""Hold the Python counts against the R counts.

The briefing's promise is that both packages return the same tables for the same filter, so the
two sides must agree on every count they both report. This needs no expected values, which is
what makes it survive corpus versions: it fails on divergence, whichever side is wrong.

    python3 compare.py counts_python.json counts_r.json

On 2026-09-05 this would have printed n_categories=2: python 4229, r 451, and exited 1.
"""
import json
import sys


def main(py_path: str, r_path: str) -> int:
    py = json.load(open(py_path))
    r = json.load(open(r_path))
    rows = []
    for key in sorted(set(py["filters"]) & set(r["filters"])):
        rows.append((f"filter({key})", py["filters"][key], r["filters"][key]))
    if py.get("quota_guard_pass") is not None and r.get("quota_guard_pass") is not None:
        rows.append(("n_responses=[0, 1e6] passes", py["quota_guard_pass"], r["quota_guard_pass"]))
    if not rows:
        print("nothing to compare: no shared keys")
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
