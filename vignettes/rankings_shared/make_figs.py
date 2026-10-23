"""Build the draft artifact's figures and tables from results_a.json.

Emits one HTML fragment file per piece into build/, so assembling the page is a
concatenation and a rerun after more tables land is one command.
Usage: python3 make_figs.py
"""
import json, math, pathlib, statistics as st, collections, html

HERE = pathlib.Path(__file__).parent
BUILD = HERE / "build"
BUILD.mkdir(exist_ok=True)

FAMILY = {"irv": "election", "stv": "election", "sequential": "election",
          "condorcet": "debian", "jury": "jury", "survey": "survey"}
PANELS = [("election", "Ranked-choice elections", "IRV and STV contests: Minneapolis, New South Wales, six US cities"),
          ("debian", "Debian developer votes", "Condorcet ballots on project resolutions"),
          ("jury", "Eurovision juries", "five jurors per country rank every act"),
          ("survey", "Survey rankings", "ranking questions put to respondents")]


def load():
    res = json.load(open(HERE / "results_a.json"))
    meta = json.load(open(HERE / "design_meta.json"))
    rows = []
    for name, r in sorted(res.items()):
        if "gain_pl_vs_uniform" not in r:
            continue
        m = meta.get(name, {})
        rows.append({"table": name, "fam": FAMILY.get(m.get("method"), "other"),
                     "method": m.get("method", "?"), "juris": m.get("jurisdiction", ""),
                     "office": m.get("office", ""), "items": r["n_items"],
                     "ballots": r["n_ballots"], "ballots_all": r.get("n_ballots_total"),
                     "marks": r["mean_marks"], "capped": r.get("capped", False),
                     "cons": r["gain_pl_vs_uniform"], "camps": r.get("gain_mix2_vs_pl")})
    return rows


# ---- tiny SVG scatter helper ---------------------------------------------------
def logscale(lo, hi, px0, px1):
    a, b = math.log10(lo), math.log10(hi)
    return lambda v: px0 + (px1 - px0) * (math.log10(max(v, lo)) - a) / (b - a)


def ticks(lo, hi):
    out = []
    e = math.floor(math.log10(lo))
    while 10 ** e <= hi * 1.001:
        for mult in (1, 2, 5):
            v = mult * 10 ** e
            if lo * 0.999 <= v <= hi * 1.001:
                out.append(v)
        e += 1
    return out


def fmt(v):
    if v >= 1:
        return f"{v:g}"
    return f"{v:g}".lstrip("0") or "0"


def scatter(rows, xk, yk, xlab, ylab, xlim, ylim, w=330, h=250, diag=False,
            labels=(), title=""):
    pad_l, pad_b, pad_t, pad_r = 44, 36, 20 if title else 8, 14
    x = logscale(xlim[0], xlim[1], pad_l, w - pad_r)
    y = logscale(ylim[0], ylim[1], h - pad_b, pad_t)
    s = [f'<svg viewBox="0 0 {w} {h}" class="fig" role="img" aria-label="{html.escape(xlab)} against {html.escape(ylab)}">']
    if title:
        s.append(f'<text x="{pad_l}" y="13" class="pt">{html.escape(title)}</text>')
    # grid + ticks
    for v in ticks(*xlim):
        px = x(v)
        s.append(f'<line x1="{px:.1f}" y1="{pad_t}" x2="{px:.1f}" y2="{h-pad_b}" class="grid"/>')
        s.append(f'<text x="{px:.1f}" y="{h-pad_b+14}" class="tk" text-anchor="middle">{fmt(v)}</text>')
    for v in ticks(*ylim):
        py = y(v)
        s.append(f'<line x1="{pad_l}" y1="{py:.1f}" x2="{w-pad_r}" y2="{py:.1f}" class="grid"/>')
        s.append(f'<text x="{pad_l-6:.1f}" y="{py+3.5:.1f}" class="tk" text-anchor="end">{fmt(v)}</text>')
    if diag:
        s.append(f'<line x1="{x(max(xlim[0], ylim[0])):.1f}" y1="{y(max(xlim[0], ylim[0])):.1f}"'
                 f' x2="{x(min(xlim[1], ylim[1])):.1f}" y2="{y(min(xlim[1], ylim[1])):.1f}" class="diag"/>')
    s.append(f'<line x1="{pad_l}" y1="{h-pad_b}" x2="{w-pad_r}" y2="{h-pad_b}" class="axis"/>')
    s.append(f'<line x1="{pad_l}" y1="{pad_t}" x2="{pad_l}" y2="{h-pad_b}" class="axis"/>')
    for r in rows:
        xv, yv = r[xk], r[yk]
        if xv is None or yv is None:
            continue
        tip = (f"{r['table']}  {xlab} {xv:.3f}, {ylab} {yv:.3f}"
               f"  ({r['items']} candidates, {r['ballots']:,} ballots)")
        s.append(f'<circle cx="{x(xv):.1f}" cy="{y(yv):.1f}" r="3.6" class="pt-mark">'
                 f'<title>{html.escape(tip)}</title></circle>')
    for tbl, text, dx, dy in labels:
        r = next((q for q in rows if q["table"] == tbl), None)
        if not r or r[xk] is None or r[yk] is None:
            continue
        px, py = x(r[xk]), y(r[yk])
        s.append(f'<circle cx="{px:.1f}" cy="{py:.1f}" r="3.6" class="pt-mark hi"/>')
        anchor = "end" if dx < 0 else "start"
        s.append(f'<text x="{px+dx:.1f}" y="{py+dy:.1f}" class="lab" text-anchor="{anchor}">{html.escape(text)}</text>')
    s.append(f'<text x="{(pad_l+w-pad_r)/2:.0f}" y="{h-4}" class="ax" text-anchor="middle">{html.escape(xlab)}</text>')
    s.append(f'<text x="11" y="{(pad_t+h-pad_b)/2:.0f}" class="ax" text-anchor="middle"'
             f' transform="rotate(-90 11 {(pad_t+h-pad_b)/2:.0f})">{html.escape(ylab)}</text>')
    s.append("</svg>")
    return "".join(s)


def main():
    rows = load()
    with_camps = [r for r in rows if r["camps"] is not None]
    xlim = (0.003, 3.2)
    ylim = (0.001, 1.2)

    # Figure 1: small multiples, consensus against camps, one panel per family
    panels = []
    for fam, title, sub in PANELS:
        rs = [r for r in with_camps if r["fam"] == fam]
        n_fam = sum(1 for r in rows if r["fam"] == fam)
        if not rs:
            continue
        lab = []
        if fam == "election":
            lab = [("rcv_burlington_20060307_mayor", "Burlington 2006", 9, 4),
                   ("rcv_alaska_20221108_hd10", "Alaska HD10", -8, -6),
                   ("rcv_mpls_20131105_mayor", "Mpls mayor 2013", -8, -12)]
        if fam == "jury":
            lab = [("eurovision_2017_final", "2017 final", 7, -4)]
        panels.append(
            f'<figure class="panel"><div class="fig-wrap">'
            + scatter(rs, "cons", "camps", "consensus gain", "camps gain", xlim, ylim,
                      diag=True, labels=lab)
            + f'</div><figcaption><strong>{html.escape(title)}</strong> '
              f'<span class="n">{len(rs)} of {n_fam} tables</span><br>{html.escape(sub)}'
              + ('' if len(rs) == n_fam else
                 f'<br><span class="n">the rest have under 150 ballots, the mixture gate</span>')
              + '</figcaption></figure>')
    n_floor = sum(1 for r in with_camps if r["camps"] <= ylim[0])
    note = (f'<p class="fig-note">Both axes are logarithmic and clipped at the plotted range. '
            f'The dashed line is where the second order is worth as much as the first. '
            f'{n_floor} tables sit on the bottom rule, meaning a camps gain at or below '
            f'{ylim[0]:g} (five of them negative: the mixture lost on held-out ballots). '
            f'One survey table sits on the left rule with a consensus gain of −0.001.</p>')
    (BUILD / "fig_cloud.html").write_text(
        '<div class="grid-2">' + "".join(panels) + "</div>" + note)

    # Figure 2: the field-concentration confound
    f2 = scatter(rows, "items", "cons", "candidates on the ballot", "consensus gain",
                 (2.5, 70), xlim,
                 labels=[("rcv_mpls_20131105_mayor", "Minneapolis mayor 2013 (35)", -8, -5),
                         ("eurovision_2017_final", "Eurovision 2017 (26)", -8, 4),
                         ("rcv_burlington_20060307_mayor", "Burlington 2006 (6)", 8, 4)],
                 w=680, h=300)
    (BUILD / "fig_items.html").write_text(f'<div class="fig-wrap wide">{f2}</div>')

    # Extremes table
    def row_html(r):
        camps = "—" if r["camps"] is None else f"{r['camps']:.3f}"
        return ("<tr><td class='mono'>" + html.escape(r["table"]) + "</td>"
                f"<td>{r['items']}</td><td>{r['ballots']:,}{'*' if r['capped'] else ''}</td>"
                f"<td>{r['marks']:.2f}</td><td>{r['cons']:.3f}</td><td>{camps}</td></tr>")

    top = sorted(rows, key=lambda r: -r["cons"])[:6]
    bot = sorted(rows, key=lambda r: r["cons"])[:6]
    camps_led = sorted([r for r in with_camps if r["camps"] > r["cons"]],
                       key=lambda r: -(r["camps"] / max(r["cons"], 1e-9)))[:6]
    head = ("<thead><tr><th>table</th><th>cand.</th><th>ballots</th><th>marks/ballot</th>"
            "<th>consensus</th><th>camps</th></tr></thead>")
    parts = []
    for cap, rs in [("Most shared", top), ("Least shared", bot),
                    ("Camps beat consensus", camps_led)]:
        parts.append(f'<h4>{cap}</h4><div class="t-wrap"><table>{head}<tbody>'
                     + "".join(row_html(r) for r in rs) + "</tbody></table></div>")
    (BUILD / "tables.html").write_text("".join(parts))

    # Numbers the prose needs
    fams = collections.defaultdict(list)
    for r in rows:
        fams[r["fam"]].append(r)
    stats = {"n_fitted": len(rows), "n_with_camps": len(with_camps),
             "n_capped": sum(1 for r in rows if r["capped"]),
             "camps_beat": sum(1 for r in with_camps if r["camps"] > r["cons"]),
             "by_family": {f: {"n": len(rs),
                               "cons_med": round(st.median(r["cons"] for r in rs), 3),
                               "cons_min": round(min(r["cons"] for r in rs), 3),
                               "cons_max": round(max(r["cons"] for r in rs), 3),
                               "camps_med": round(st.median([r["camps"] for r in rs if r["camps"] is not None]), 3)
                               if any(r["camps"] is not None for r in rs) else None}
                           for f, rs in fams.items()},
             "ballots_total": sum(r["ballots_all"] or r["ballots"] for r in rows)}
    (BUILD / "stats.json").write_text(json.dumps(stats, indent=1))
    print(json.dumps(stats, indent=1))


if __name__ == "__main__":
    main()
