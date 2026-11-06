#!/usr/bin/env python3
"""Check each welcome_<lang>.md against the English for structural drift.

The English is split across two files -- welcome_en.md carries the title,
tagline and the switcher include, and /welcome/_prose_en.md carries everything
from "## Why the IRW exists" down -- while the 24 translations are whole files.
So the English side is ASSEMBLED here, by expanding the includes, before
anything is compared. See README.md.

What it compares, and why each one matters:

  headings     the level sequence, e.g. H1 H2 H2 H2 H3 H2. A translation that
               has lost or gained a section is missing content, or has content
               the English no longer has.
  code         fenced blocks, compared character for character. The translator
               note says never to translate code, comments included, so any
               difference at all is a defect.
  links        the set of URLs and DOIs. A translated page that points
               somewhere else, or has dropped a link, is a broken page.
  columns      backticked `id`, `item`, `resp` and friends. These are literal
               field names and must survive translation unchanged.
  images       src= and markdown image targets, which must point at the same
               shared figures.
  switcher     the {{< include /welcome/_lang_switch.md >}} line. One line
               since the switcher was shared; before that it was 137 lines
               copied into every file.

It reports drift, not quality: it cannot tell you whether the French is good,
only whether the French page still has the same bones as the English one.

Usage:  python3 welcome/check_translations.py            # table of mismatches
        python3 welcome/check_translations.py --verbose  # list every item
        python3 welcome/check_translations.py --lang fr  # one language

Exit status is 1 if any language has a mismatch, so it can gate a commit.
Standard library only.
"""

import argparse
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)

INCLUDE = re.compile(r"\{\{<\s*include\s+([^\s>]+)\s*>\}\}")
FENCE = re.compile(r"^(```+|~~~+)(.*)$")
HEADING = re.compile(r"^(#{1,6})\s+(.*)$")
URL = re.compile(r"https?://[^\s)\"'<>\]]+")
BACKTICK = re.compile(r"`([^`\n]+)`")
IMG_HTML = re.compile(r"<img[^>]*\bsrc=[\"']([^\"']+)[\"']")
IMG_MD = re.compile(r"!\[[^\]]*\]\(([^)\s]+)")

# Backticked spans that are column names rather than prose or function calls.
# Anything with a bracket is a call; anything with a space is a phrase.
COLUMNISH = re.compile(r"^[a-z][a-z0-9_]*(<[a-z_]+>)?$")


def read(path):
    with open(path, encoding="utf-8") as fh:
        return fh.read()


def expand(path, seen=None):
    """Inline {{< include >}} so the English reads as one document.

    A path starting with / is project-relative, which is how the welcome files
    write theirs; anything else is relative to the including file.
    """
    seen = seen or set()
    real = os.path.realpath(path)
    if real in seen:                      # a cycle would otherwise hang
        return ""
    seen = seen | {real}
    out = []
    for line in read(path).splitlines(keepends=True):
        m = INCLUDE.search(line)
        if not m:
            out.append(line)
            continue
        target = m.group(1)
        target = (os.path.join(ROOT, target.lstrip("/")) if target.startswith("/")
                  else os.path.join(os.path.dirname(path), target))
        if os.path.exists(target):
            out.append(expand(target, seen))
        else:
            out.append(line)              # leave it; the comparison will show it
    return "".join(out)


def strip_comments(text):
    return re.sub(r"<!--.*?-->", "", text, flags=re.S)


def code_blocks(text):
    """Fenced blocks as (info string, body). Fences inside a block are body."""
    blocks, cur, fence = [], None, None
    for line in text.splitlines():
        m = FENCE.match(line)
        if cur is None:
            if m:
                fence, cur = m.group(1), {"info": m.group(2).strip(), "body": []}
            continue
        if m and m.group(1)[0] == fence[0] and len(m.group(1)) >= len(fence):
            blocks.append((cur["info"], "\n".join(cur["body"])))
            cur, fence = None, None
        else:
            cur["body"].append(line)
    if cur is not None:                   # unterminated fence: keep what we have
        blocks.append((cur["info"], "\n".join(cur["body"])))
    return blocks


def outside_code(text):
    """The document with fenced blocks removed, so code does not feed the
    link, column and image checks."""
    out, fence = [], None
    for line in text.splitlines():
        m = FENCE.match(line)
        if fence is None:
            if m:
                fence = m.group(1)
                continue
            out.append(line)
        elif m and m.group(1)[0] == fence[0] and len(m.group(1)) >= len(fence):
            fence = None
    return "\n".join(out)


def features(text):
    text = strip_comments(text)
    bare = outside_code(text)
    cols = {c for c in BACKTICK.findall(bare) if COLUMNISH.match(c)}
    return {
        "headings": [len(m.group(1)) for m in
                     (HEADING.match(l) for l in bare.splitlines()) if m],
        "code": code_blocks(text),
        "links": {u.rstrip(".,;:") for u in URL.findall(bare)},
        "columns": cols,
        "images": set(IMG_HTML.findall(bare)) | set(IMG_MD.findall(bare)),
        "switcher": text.count("{{< include /welcome/_lang_switch.md >}}"),
    }


def compare(en, tr):
    """Return {check: message} for every check that failed."""
    bad = {}

    if en["headings"] != tr["headings"]:
        bad["headings"] = "levels %s, English has %s" % (
            "".join("H%d" % h for h in tr["headings"]) or "none",
            "".join("H%d" % h for h in en["headings"]))

    if len(en["code"]) != len(tr["code"]):
        bad["code"] = "%d blocks, English has %d" % (len(tr["code"]), len(en["code"]))
    else:
        diff = [i for i, (a, b) in enumerate(zip(en["code"], tr["code"])) if a != b]
        if diff:
            bad["code"] = "block %s differs (code must never be translated)" % (
                ", ".join(str(i + 1) for i in diff))

    for key, label in (("links", "link"), ("columns", "column"), ("images", "image")):
        miss, extra = sorted(en[key] - tr[key]), sorted(tr[key] - en[key])
        if miss or extra:
            parts = []
            if miss:
                parts.append("missing " + ", ".join(miss[:4]) +
                             (" +%d" % (len(miss) - 4) if len(miss) > 4 else ""))
            if extra:
                parts.append("extra " + ", ".join(extra[:4]) +
                             (" +%d" % (len(extra) - 4) if len(extra) > 4 else ""))
            bad[key] = "%s: %s" % (label, "; ".join(parts))

    if en["switcher"] != tr["switcher"]:
        bad["switcher"] = "include appears %d times, English %d" % (
            tr["switcher"], en["switcher"])

    return bad


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--lang", help="check one language code, e.g. fr")
    ap.add_argument("--verbose", action="store_true",
                    help="list the headings, links and columns found")
    args = ap.parse_args()

    en_path = os.path.join(HERE, "welcome_en.md")
    en = features(expand(en_path))

    langs = sorted(
        f[len("welcome_"):-len(".md")]
        for f in os.listdir(HERE)
        if f.startswith("welcome_") and f.endswith(".md") and f != "welcome_en.md")
    if args.lang:
        if args.lang not in langs:
            sys.exit("no such language: %s (have %s)" % (args.lang, ", ".join(langs)))
        langs = [args.lang]

    if args.verbose:
        print("English, assembled from welcome_en.md + its includes:")
        print("  headings  %s" % "".join("H%d" % h for h in en["headings"]))
        print("  code      %d blocks" % len(en["code"]))
        print("  links     %d" % len(en["links"]))
        print("  columns   %s" % ", ".join(sorted(en["columns"])))
        print("  images    %s" % ", ".join(sorted(en["images"])) or "none")
        print()

    CHECKS = ["headings", "code", "links", "columns", "images", "switcher"]
    rows, failing = [], 0
    for lang in langs:
        bad = compare(en, features(expand(os.path.join(HERE, "welcome_%s.md" % lang))))
        failing += bool(bad)
        rows.append((lang, bad))

    width = max(len(c) for c in CHECKS)
    print("%-7s %s" % ("lang", "  ".join(c.ljust(width) for c in CHECKS)))
    print("-" * (8 + (width + 2) * len(CHECKS)))
    for lang, bad in rows:
        cells = ["FAIL".ljust(width) if c in bad else "ok".ljust(width) for c in CHECKS]
        print("%-7s %s" % (lang, "  ".join(cells)))

    if failing:
        print("\n%d of %d translations differ from the English:\n" % (failing, len(rows)))
        for lang, bad in rows:
            if bad:
                print("  %s" % lang)
                for check in CHECKS:
                    if check in bad:
                        print("    %-9s %s" % (check, bad[check]))
    else:
        print("\nall %d translations match the English structurally" % len(rows))

    return 1 if failing else 0


if __name__ == "__main__":
    sys.exit(main())
