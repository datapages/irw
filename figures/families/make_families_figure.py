"""Generate the IRW data-families figure (core / nominal / comp / conj / rank).

Extends diagram-cross-classification.svg: each panel shows what a respondent
meets (left) and the long table it becomes (right). The panels differ in what
is crossed and what the response is.

Usage: python make_families_figure.py  -> writes families-*.svg
"""

FONT = 'font-family="sans-serif"'
INK, MUTED, RULE, ZEBRA, HEAD = "#343a40", "#6c757d", "#dee2e6", "#f8f9fa", "#495057"
SUITS = ["♠", "♥", "♦", "♣"]
RAMP = ["#a9c9ef", "#5c98e0", "#2469bc", "#153e6f"]           # ordinal 0..3
NOMCOL = {"A": "#8352ff", "B": "#ff4d8d", "C": "#00a3a3", "D": "#8a8478"}
ATTR1 = {"r": "#ee7c4f", "b": "#5c98e0", "g": "#1ec288"}       # attr_1 levels
ATTR2 = {"c": "●", "t": "▲", "s": "■"}                         # attr_2 levels
ACCENT = {"core": "#2469bc", "nom": "#8352ff", "comp": "#cc4814",
          "conj": "#127350", "rank": "#a86f00"}

W, H = 372, 212   # panel size


def t(x, y, s, size=10, fill=INK, anchor="middle", weight="normal", extra=""):
    fam = "" if "font-family" in extra else FONT
    return (f'<text x="{x:.1f}" y="{y:.1f}" text-anchor="{anchor}" {fam} '
            f'font-size="{size}" font-weight="{weight}" fill="{fill}" {extra}>{s}</text>')


def r(x, y, w, h, fill, stroke="none", sw=1, rx=0, extra=""):
    return (f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" rx="{rx}" '
            f'fill="{fill}" stroke="{stroke}" stroke-width="{sw}" {extra}/>')


def line(x1, y1, x2, y2, stroke=RULE, sw=1, extra=""):
    return (f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" '
            f'stroke="{stroke}" stroke-width="{sw}" {extra}/>')


def header(key, name, src, sub):
    a = ACCENT[key]
    return [r(0, 0, 4, 30, a),
            t(12, 13, name, 13, a, "start", "700"),
            t(12 + 8.2 * len(name) + 6, 13, f'source = "{src}"', 9.5, MUTED, "start",
              extra='font-family="monospace"'),
            t(12, 27, sub, 9.5, MUTED, "start")]


def arrow(x=176, y=112):
    return [line(x, y, x + 18, y, HEAD, 2, 'stroke-linecap="round"'),
            f'<polygon points="{x+18},{y-5} {x+25},{y} {x+18},{y+5}" fill="{HEAD}"/>']


def footer(text):
    return [t(W / 2, H - 6, text, 9.5, HEAD, extra='font-style="italic"')]


def table(x0, y0, cols, rows, groups=(), rh=9):
    """cols: [(name, width)]; rows: list of lists of cell-draw fns f(cx, cy, w)->[svg];
    groups: row indices before which to draw a darker rule."""
    out, tw = [], sum(w for _, w in cols)
    out.append(r(x0, y0, tw, 16, ZEBRA, RULE, 1.2))
    cx = x0
    for name, w in cols:
        out.append(t(cx + w / 2, y0 + 11.5, name, 8.5 if len(name) < 6 else 7, HEAD,
                     extra='font-family="monospace"'))
        cx += w
    y = y0 + 16
    for i, row in enumerate(rows):
        out.append(r(x0, y, tw, rh, "#ffffff" if i % 2 == 0 else ZEBRA))
        cx = x0
        for (name, w), cell in zip(cols, row):
            out += cell(cx, y + rh / 2, w)
            cx += w
        y += rh
    out.append(r(x0, y0, tw, y - y0, "none", RULE, 1.2))
    cx = x0
    for _, w in cols[:-1]:
        cx += w
        out.append(line(cx, y0, cx, y))
    for g in groups:
        gy = y0 + 16 + g * rh
        out.append(line(x0, gy, x0 + tw, gy, "#adb5bd", 1.3))
    return out, y


# cell renderers ---------------------------------------------------------------
def num(v, fill=INK):
    return lambda cx, cy, w: [t(cx + w / 2, cy + 2.6, v, 7.2, fill)]


GLYPH = 'font-family="DejaVu Sans, Segoe UI Symbol, Apple Symbols, sans-serif"'


def g(x, y, s, size, fill=INK):
    return t(x, y, s, size, fill, extra=GLYPH)


def suit(s):
    return lambda cx, cy, w: [g(cx + w / 2, cy + 3.2, s, 9)]


def mono(v, fill=INK, size=6.6):
    return lambda cx, cy, w: [t(cx + w / 2, cy + 2.4, v, size, fill,
                                extra='font-family="monospace"')]


def ord_swatch(v):
    return lambda cx, cy, w: [r(cx + 3, cy - 3.3, w - 6, 6.6, RAMP[v]),
                              t(cx + w / 2, cy + 2.5, str(v), 6.6,
                                "#ffffff" if v >= 1 else INK, weight="700")]


def nom_swatch(v):
    return lambda cx, cy, w: [r(cx + 3, cy - 3.3, w - 6, 6.6, NOMCOL[v]),
                              t(cx + w / 2, cy + 2.5, v, 6.6, "#ffffff", weight="700")]


def attr1(v):
    return lambda cx, cy, w: [r(cx + w / 2 - 5, cy - 3, 10, 6, ATTR1[v], rx=1)]


def attr2(v):
    return lambda cx, cy, w: [g(cx + w / 2, cy + 3, ATTR2[v], 7.5)]


# panels -----------------------------------------------------------------------
def grid_panel(key, name, src, sub, values, cell_fill, cell_text_fill, swatch, resp_col, foot):
    out = header(key, name, src, sub)
    gx, gy, c = 30, 62, 32
    out.append(t(gx + 2 * c, gy - 22, "item", 10, HEAD))
    out.append(t(12, gy - 4, "id", 10, HEAD))
    for j, s in enumerate(SUITS):
        out.append(g(gx + j * c + c / 2, gy - 6, s, 16))
    rows = []
    for i, vals in enumerate(values):
        out.append(t(12, gy + i * c + c / 2 + 4.5, str(i + 1), 12, INK, weight="600"))
        for j, v in enumerate(vals):
            out.append(r(gx + j * c, gy + i * c, c, c, cell_fill(v), "#ffffff", 2))
            out.append(t(gx + j * c + c / 2, gy + i * c + c / 2 + 4.5, str(v), 12,
                         cell_text_fill(v), weight="700"))
            rows.append([num(str(i + 1)), suit(SUITS[j]), swatch(v)])
    out += arrow(gx + 4 * c + 12, gy + 1.5 * c)
    tb, _ = table(222, 40, [("id", 26), ("item", 30), (resp_col, 42)], rows, groups=(4, 8))
    out += tb
    out += footer(foot)
    return out


def core_panel():
    vals = [[2, 0, 3, 1], [1, 1, 2, 0], [3, 2, 3, 2]]
    return grid_panel("core", "Core", "core",
                      "a person answers an item; the response is ordered",
                      vals, lambda v: RAMP[v], lambda v: "#ffffff" if v >= 1 else INK,
                      ord_swatch, "resp", "one row = id × item  ·  resp is a score")


def nom_panel():
    vals = [["B", "A", "D", "C"], ["A", "A", "C", "B"], ["D", "B", "A", "A"]]
    return grid_panel("nom", "Nominal", "nom",
                      "the same crossing, but the categories have no order",
                      vals, lambda v: NOMCOL[v], lambda v: "#ffffff",
                      nom_swatch, "text", "one row = id × item  ·  text is a category")


def comp_panel():
    out = header("comp", "Competitions", "comp",
                 "no items: the entities meet each other, two at a time")
    games = [("♠", "♥", "a"), ("♦", "♣", "b"), ("♥", "♦", "a"), ("♠", "♣", "d"),
             ("♣", "♥", "b"), ("♠", "♦", "a"), ("♥", "♠", "a"), ("♦", "♣", "a")]
    # matrix: rows and columns are the same agents
    gx, gy, c = 34, 62, 27
    out.append(t(gx + 2 * c, gy - 22, "agent", 10, HEAD))
    out.append(t(14, gy - 4, "agent", 9, HEAD))
    for j, s in enumerate(SUITS):
        out.append(g(gx + j * c + c / 2, gy - 6, s, 15))
        out.append(g(16, gy + j * c + c / 2 + 5, s, 15))
    for i in range(4):
        for j in range(4):
            x, y = gx + j * c, gy + i * c
            out.append(r(x, y, c, c, "#e9ecef" if i == j else "#fff4ee", "#ffffff", 2))
    # marks: for each game put a mark in the cell (agent_a row, agent_b col) and its mirror
    slots = {}
    for a, b, w in games:
        for rowa, cola, res in ((a, b, w), (b, a, {"a": "b", "b": "a", "d": "d"}[w])):
            i, j = SUITS.index(rowa), SUITS.index(cola)
            k = slots.get((i, j), 0)
            slots[(i, j)] = k + 1
            cx, cy = gx + j * c + 8 + k * 11, gy + i * c + c / 2
            col = ACCENT["comp"]
            if res == "a":
                out.append(f'<circle cx="{cx}" cy="{cy}" r="4" fill="{col}"/>')
            elif res == "b":
                out.append(f'<circle cx="{cx}" cy="{cy}" r="3.4" fill="#ffffff" stroke="{col}" stroke-width="1.4"/>')
            else:
                out.append(f'<circle cx="{cx}" cy="{cy}" r="3.4" fill="#ffffff" stroke="{col}" stroke-width="1.4"/>')
                out.append(f'<path d="M{cx},{cy-3.4} A3.4,3.4 0 0,0 {cx},{cy+3.4} Z" fill="{col}"/>')
    ly = gy + 4 * c + 13
    lx = gx - 4
    out.append(f'<circle cx="{lx+4}" cy="{ly}" r="3.4" fill="{ACCENT["comp"]}"/>')
    out.append(t(lx + 11, ly + 3, "row won", 7.5, MUTED, "start"))
    out.append(f'<circle cx="{lx+52}" cy="{ly}" r="3" fill="#fff" stroke="{ACCENT["comp"]}" stroke-width="1.3"/>')
    out.append(t(lx + 58, ly + 3, "lost", 7.5, MUTED, "start"))
    out.append(f'<circle cx="{lx+84}" cy="{ly}" r="3" fill="#fff" stroke="{ACCENT["comp"]}" stroke-width="1.3"/>')
    out.append(f'<path d="M{lx+84},{ly-3} A3,3 0 0,0 {lx+84},{ly+3} Z" fill="{ACCENT["comp"]}"/>')
    out.append(t(lx + 90, ly + 3, "draw", 7.5, MUTED, "start"))
    out += arrow(gx + 4 * c + 12, gy + 1.5 * c)
    lab = {"a": "agent_a", "b": "agent_b", "d": "draw"}
    rows = [[suit(a), suit(b), mono(lab[w])] for a, b, w in games]
    tb, _ = table(214, 40, [("agent_a", 46), ("agent_b", 46), ("winner", 52)], rows)
    out += tb
    out += footer("one row = agent_a × agent_b  ·  winner (plus scores)")
    return out


def conj_panel():
    out = header("conj", "Conjoint", "conj",
                 "profiles are built at random from attributes, then never seen again")
    # id 1: two tasks, two profiles each. (attr_1 colour, attr_2 shape, chosen)
    tasks = [[("r", "t", 1), ("b", "c", 0)], [("g", "c", 0), ("r", "s", 1)]]
    x0, y0, cw, ch = 18, 54, 28, 42
    out.append(t(x0 - 6, y0 + 2, "id 1", 9.5, HEAD, "start", "600"))
    for k, task in enumerate(tasks):
        tx = x0 + k * 72
        out.append(t(tx + 31, y0 + 18, f"task {k+1}", 8.5, MUTED))
        for p, (a1, a2, ch_) in enumerate(task):
            cx, cy = tx + p * 34, y0 + 24
            col = ACCENT["conj"]
            out.append(r(cx, cy, cw, ch, "#ffffff", col if ch_ else "#adb5bd",
                         2.6 if ch_ else 1, rx=4))
            out.append(r(cx + 6, cy + 7, cw - 12, 9, ATTR1[a1], rx=1.5))
            out.append(g(cx + cw / 2, cy + 31, ATTR2[a2], 12))
            out.append(t(cx + cw / 2, cy + ch + 11, str(p + 1), 8, MUTED))
            if ch_:
                out.append(f'<circle cx="{cx+cw}" cy="{cy}" r="6" fill="{col}"/>')
                out.append(f'<path d="M{cx+cw-3},{cy} l2,2.2 l4,-4.4" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/>')
    out.append(t(x0 + 65, y0 + 96, "profile", 8, MUTED))
    # attribute key
    ky = y0 + 116
    out.append(t(x0 - 6, ky, "attr_1:", 7.5, MUTED, "start", extra='font-family="monospace"'))
    for i, (k_, v) in enumerate(ATTR1.items()):
        out.append(r(x0 + 26 + i * 14, ky - 6, 10, 6, v, rx=1))
    out.append(t(x0 + 74, ky, "attr_2:", 7.5, MUTED, "start", extra='font-family="monospace"'))
    for i, v in enumerate(ATTR2.values()):
        out.append(g(x0 + 108 + i * 12, ky + 1, v, 8))
    out += arrow(158, y0 + 45)
    id2 = [[("b", "s", 0), ("g", "t", 1)], [("r", "c", 1), ("b", "t", 0)]]
    rows = []
    for idn, tt in ((1, tasks), (2, id2)):
        for k, task in enumerate(tt):
            for p, (a1, a2, ch_) in enumerate(task):
                rows.append([num(str(idn)), num(str(k + 1)), num(str(p + 1)),
                             attr1(a1), attr2(a2),
                             num(str(ch_), ACCENT["conj"] if ch_ else INK)])
    tb, _ = table(190, 40, [("id", 18), ("task", 24), ("profile", 34), ("attr_1", 32),
                            ("attr_2", 32), ("choice", 34)], rows, groups=(2, 4, 6))
    out += tb
    out += footer("one row = id × task × profile  ·  attributes, not items, are the target")
    return out


def rank_panel():
    out = header("rank", "Rankings", "rank",
                 "a person puts a fixed set of options in order")
    ballots = [["♦", "♠", "♣", "♥"], ["♥", "♠", None, "♣"], ["♦", "♥", None, None]]
    x0, y0 = 30, 52
    col = ACCENT["rank"]
    for b, ballot in enumerate(ballots):
        bx = x0 + b * 48
        out.append(t(bx + 17, y0 + 4, f"id {b+1}", 9.5, HEAD, weight="600"))
        out.append(r(bx, y0 + 10, 34, 112, "#fffaf1", "#d9c7a6", 1, rx=4))
        for pos in range(4):
            py = y0 + 18 + pos * 26
            out.append(t(bx + 8, py + 13, str(pos + 1), 8, MUTED))
            s = ballot[pos]
            if s:
                out.append(r(bx + 14, py, 16, 18, "#ffffff", col, 1.2, rx=3))
                out.append(g(bx + 22, py + 14, s, 12))
            else:
                out.append(r(bx + 14, py, 16, 18, "none", "#c8c8c8", 1, rx=3,
                             extra='stroke-dasharray="2 2"'))
    out += arrow(176, y0 + 60)
    rows, groups, n = [], [], 0
    for b, ballot in enumerate(ballots):
        if b:
            groups.append(n)
        for pos, s in enumerate(ballot):
            if s:
                rows.append([num(str(b + 1)), num("1"), suit(s), num(str(pos + 1), col)])
                n += 1
    tb, yend = table(210, 40, [("id", 24), ("task", 28), ("item", 30), ("resp", 30)],
                     rows, groups=groups)
    out += tb
    # annotate the gap in ballot 2 (rows 4..6: positions 1,2,4)
    gy = 40 + 16 + 6 * 9 + 4.5
    out.append(t(325, gy + 2.5, "← no 3: skips", 7, MUTED, "start"))
    out.append(t(325, gy + 11, "stay gaps", 7, MUTED, "start"))
    out += footer("one row = id × task × marked position  ·  resp is a rank")
    return out


def tree_panel():
    out = [t(12, 13, "Which family?", 13, INK, "start", "700"),
           t(12, 27, "sort a new dataset with two questions", 9.5, MUTED, "start")]

    def box(x, y, w, h, label, key=None, sub=None):
        o = [r(x, y, w, h, "#ffffff" if key else ZEBRA, ACCENT[key] if key else "#adb5bd",
               1.8 if key else 1, rx=6)]
        if sub:
            o.append(t(x + w / 2, y + h / 2 - 1, label, 9, ACCENT[key] if key else INK, weight="700"))
            o.append(t(x + w / 2, y + h / 2 + 9, sub, 7.5, MUTED))
        else:
            o.append(t(x + w / 2, y + h / 2 + 3.5, label, 9, ACCENT[key] if key else INK,
                       weight="700" if key else "600"))
        return o

    def elbow(x1, y1, x2, y2, label=None, lx=None):
        o = [f'<path d="M{x1},{y1} V{(y1+y2)/2} H{x2} V{y2}" fill="none" stroke="#adb5bd" stroke-width="1.3"/>']
        if label:
            o.append(t(lx if lx else x2, (y1 + y2) / 2 - 3, label, 7.5, MUTED))
        return o

    q1 = (86, 40, 200, 30)
    out += box(*q1, "Do the things responded to")
    out[-1] = t(186, 52, "Do the things responded to", 8.6, INK, weight="600")
    out.append(t(186, 63, "keep a stable identity?", 8.6, INK, weight="600"))
    out += elbow(140, 70, 52, 96, "no", 92)
    out += box(12, 96, 80, 30, "Conjoint", "conj", "built from attributes")
    out += elbow(232, 70, 270, 96, "yes", 254)
    out.append(r(196, 96, 148, 30, ZEBRA, "#adb5bd", 1, rx=6))
    out.append(t(270, 108, "How does a person", 8.6, INK, weight="600"))
    out.append(t(270, 119, "meet them?", 8.6, INK, weight="600"))
    # three branches; core and nominal share the "one at a time" branch
    out.append('<path d="M270,126 V134" fill="none" stroke="#adb5bd" stroke-width="1.3"/>')
    out.append('<path d="M154,134 H333" fill="none" stroke="#adb5bd" stroke-width="1.3"/>')
    out.append('<path d="M154,134 V158 M127,158 H180 M127,158 V166 M180,158 V166" fill="none" stroke="#adb5bd" stroke-width="1.3"/>')
    for x in (256, 333):
        out.append(f'<path d="M{x},134 V166" fill="none" stroke="#adb5bd" stroke-width="1.3"/>')
    for x, lab, w in ((154, "one at a time", 52), (256, "two at a time", 52), (333, "all, in order", 50)):
        out.append(r(x - w / 2, 141, w, 11, "#ffffff"))
        out.append(t(x, 149.5, lab, 7.5, MUTED))
    out += box(104, 166, 46, 30, "Core", "core", "ordered")
    out += box(154, 166, 52, 30, "Nominal", "nom", "unordered")
    out += box(214, 166, 84, 30, "Competitions", "comp", "pairs")
    out += box(302, 166, 62, 30, "Rankings", "rank", "positions")
    return out


PANELS = {"core": core_panel, "nom": nom_panel, "comp": comp_panel,
          "conj": conj_panel, "rank": rank_panel}


def wrap(parts, w=W, h=H, label=""):
    body = "\n  ".join(parts)
    return (f'<svg viewBox="0 0 {w} {h}" xmlns="http://www.w3.org/2000/svg" role="img" '
            f'aria-label="{label}">\n  {body}\n</svg>\n')


ALT = {
    "core": "Core: an id-by-item grid of ordered scores becomes a long table of id, item, resp.",
    "nom": "Nominal: an id-by-item grid of unordered categories becomes a long table of id, item, text.",
    "comp": "Competitions: an agent-by-agent matrix of contests becomes a table of agent_a, agent_b, winner.",
    "conj": "Conjoint: randomly built profiles chosen within tasks become a table of id, task, profile, attributes, choice.",
    "rank": "Rankings: ballots ordering a fixed set of options become a table of id, task, item, resp where resp is a position; skipped positions are gaps.",
}

if __name__ == "__main__":
    for k, f in PANELS.items():
        open(f"families-{k}.svg", "w").write(wrap(f(), label=ALT[k]))
    # combined: 2 columns x 3 rows
    layout = [("core", 0, 0), ("nom", 1, 0), ("comp", 0, 1), ("rank", 1, 1),
              ("conj", 0, 2), ("tree", 1, 2)]
    gap, pad = 20, 8
    parts = []
    for key, cx, cy in layout:
        x, y = pad + cx * (W + gap), pad + cy * (H + gap)
        inner = tree_panel() if key == "tree" else PANELS[key]()
        parts.append(f'<g transform="translate({x},{y})">\n    ' + "\n    ".join(inner) + "\n  </g>")
        if key != "tree":
            pass
    # light card behind each panel
    cards = [r(pad + cx * (W + gap) - 4, pad + cy * (H + gap) - 4, W + 8, H + 8, "#ffffff",
               "#e9ecef", 1, rx=8) for _, cx, cy in layout]
    tw, th = 2 * W + gap + 2 * pad, 3 * H + 2 * gap + 2 * pad
    open("families-all.svg", "w").write(
        wrap(cards + parts, tw, th,
             "The IRW data families compared: core, nominal, competitions, rankings and conjoint, "
             "each shown as what a respondent meets and the long table it becomes, plus a two-question guide to which family a dataset belongs in."))
    open("families-tree.svg", "w").write(wrap(tree_panel(), label="Two questions that sort a dataset into an IRW family."))
