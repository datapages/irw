"""Stage every table of the live rankings family into one persistent directory.

Most of the family is already built on this machine, but ~380 of the CSVs live in
other agent sessions' /tmp scratchpads and will vanish when those are cleaned.  So:
salvage by table name, reconcile every row count against the live Redivis listing,
and fetch from Redivis only what is missing or disagrees.

Usage: python3 -I stage_rank.py [--no-fetch]
Writes ~/.cache/irw_rank_vignette/tables/<table>.csv and staging_report.csv here.
"""
import os, sys, csv, glob, shutil, subprocess, pathlib

VERSION = "v1_5"                      # pinned release this draft reads
DATASET = "irw_rank:93f5"
CACHE = pathlib.Path.home() / ".cache/irw_rank_vignette/tables"
HERE = pathlib.Path(__file__).parent
IRW = pathlib.Path("/home/ben/Dropbox/projects/irw")
SCRATCH = "/tmp/claude-1000/-home-ben-Dropbox-projects-irw"

# Dropbox copies first (persistent), then the scratchpads.
SEARCH = [
    str(IRW / "src/data/rankings/examples/*.csv"),
    str(IRW / "oneoff/rankings-debian-tables-2026-10-15/*.csv"),
    str(IRW / "oneoff/rankings-eurovision-tables-2026-10-15/*.csv"),
    SCRATCH + "/*/scratchpad/*/*.csv",
    SCRATCH + "/*/scratchpad/*/*/*.csv",
]


def live_tables():
    import redivis
    os.environ.setdefault(
        "REDIVIS_API_TOKEN",
        open(os.path.expanduser("~/.redivis_api_token")).read().strip())
    d = redivis.organization("datapages").dataset(DATASET, version=VERSION.replace("_", "."))
    return {t.name: t.properties["numRows"] for t in d.list_tables()}


def group_of(name):
    if name.startswith("rcv_"):
        return name.split("_")[1]
    return name.split("_")[0]


def data_rows(path):
    """Row count excluding the header, counted without loading the file."""
    n = int(subprocess.run(["wc", "-l", str(path)], capture_output=True, text=True,
                           check=True).stdout.split()[0])
    return max(n - 1, 0)


def candidates(live):
    """table name -> list of local paths, largest first."""
    found = {}
    for pattern in SEARCH:
        for p in glob.glob(pattern):
            name = os.path.basename(p)[:-4]
            if name not in live:
                continue                      # not in the released family (e.g. aberdeen)
            found.setdefault(name, []).append(p)
    for name in found:
        found[name].sort(key=lambda p: os.path.getsize(p), reverse=True)
    return found


def fetch(name):
    import irw
    df = irw.fetch(name, source="rank", columns=["id", "task", "item", "resp"])
    out = CACHE / f"{name}.csv"
    df.to_csv(out, index=False)
    return out


def main():
    no_fetch = "--no-fetch" in sys.argv
    CACHE.mkdir(parents=True, exist_ok=True)
    live = live_tables()
    print(f"live {DATASET} {VERSION}: {len(live)} tables, "
          f"{sum(live.values()):,} rows", flush=True)
    local = candidates(live)
    print(f"local copies found for {len(local)} of {len(live)} tables", flush=True)

    rows = []
    for i, (name, want) in enumerate(sorted(live.items()), 1):
        dest = CACHE / f"{name}.csv"
        origin, got = None, None
        if dest.exists():
            got, origin = data_rows(dest), "cached"
        if got != want:
            for src in local.get(name, []):
                if data_rows(src) == want:
                    shutil.copyfile(src, dest)
                    got, origin = want, "salvaged"
                    break
        if got != want and not no_fetch:
            try:
                fetch(name)
                got, origin = data_rows(dest), "fetched"
            except Exception as e:                       # noqa: BLE001
                origin = f"FAILED: {type(e).__name__}: {e}"[:200]
        rows.append({"table": name, "group": group_of(name), "redivis_rows": want,
                     "local_rows": got, "origin": origin or "MISSING"})
        if i % 25 == 0 or got != want:
            print(f"  [{i}/{len(live)}] {name} {origin} {got}/{want}", flush=True)

    with open(HERE / "staging_report.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)

    ok = [r for r in rows if r["local_rows"] == r["redivis_rows"]]
    print(f"\nstaged {len(ok)}/{len(rows)} tables, {sum(r['local_rows'] for r in ok):,} rows")
    for r in rows:
        if r not in ok:
            print("  MISMATCH", r["table"], r["local_rows"], "vs", r["redivis_rows"], r["origin"])
    import collections
    print("origins:", collections.Counter(r["origin"].split(":")[0] for r in rows).most_common())


if __name__ == "__main__":
    main()
