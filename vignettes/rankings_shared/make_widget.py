"""The Plackett-Luce explainer widget at the top of the draft page.

Six candidates, the worths actually fitted to rcv_burlington_20060307_mayor, a ballot
the reader builds by clicking, the per-mark arithmetic the whole vignette rests on, and
the model written out properly underneath. The default state is rendered here in Python
so the page is complete before any script runs; render() in the page reproduces the same
numbers on every change.

Usage: python3 make_widget.py   ->  build/widget.html
"""
import json, math, pathlib

HERE = pathlib.Path(__file__).parent
BUILD = HERE / "build"
BUILD.mkdir(exist_ok=True)

# Fitted Plackett-Luce worths, rcv_burlington_20060307_mayor (9,784 ballots, 6 candidates),
# normalised to sum to 1. Candidate codes are the ones IRW stores; names are not kept.
WORTHS = [("c01", 0.3787), ("c02", 0.3067), ("c03", 0.1810),
          ("c04", 0.0643), ("c05", 0.0611), ("c06", 0.0082)]
DEFAULT = ["c03", "c01", "c05"]


def steps(worths, ballot):
    pool = [k for k, _ in worths]
    w = dict(worths)
    out = []
    for pick in ballot:
        tot = sum(w[k] for k in pool)
        out.append({"pick": pick, "num": w[pick], "den": tot,
                    "p": w[pick] / tot, "u": 1.0 / len(pool), "pool": len(pool)})
        pool = [k for k in pool if k != pick]
    return out


def summary(st):
    if not st:
        return None
    lr = sum(math.log(s["p"] / s["u"]) for s in st)
    return {"ratio": math.exp(lr), "gain": math.exp(lr / len(st)) - 1}


def pct(x):
    return f"{100 * x:.1f}%"


def render_rows(st):
    rows = []
    for i, s in enumerate(st, 1):
        rows.append(
            f'<tr><td class="step">{i}</td><td class="pick mono">{s["pick"]}</td>'
            f'<td>{pct(s["p"])}</td><td>{pct(s["u"])}</td>'
            f'<td class="rat">&times;{s["p"]/s["u"]:.2f}</td></tr>')
    return "".join(rows)


def render_live(st):
    """The ballot's own likelihood written out as the product in equation 2."""
    if not st:
        return "Click a candidate above and this line fills in."
    mod = " &times; ".join(f'<span class="ff">{s["num"]:.3f}&#8201;/&#8201;{s["den"]:.3f}</span>'
                           for s in st)
    base = " &times; ".join(f'<span class="ff">1&#8201;/&#8201;{s["pool"]}</span>' for s in st)
    pm = math.prod(s["p"] for s in st)
    pu = math.prod(s["u"] for s in st)
    return (f'Pr(this ballot) = {mod} = <span class="num">{pm:.5f}</span>, against '
            f'{base} = <span class="num">{pu:.5f}</span> under guessing.')


# ---- the model, written out ---------------------------------------------------
def frac(num, den):
    return (f'<span class="frac"><span class="fnum">{num}</span>'
            f'<span class="fden">{den}</span></span>')


W = '<i>w</i>'
EQ1 = ('<div class="eq">Pr(next mark is <i>j</i> &nbsp;|&nbsp; <i>S<sub>p</sub></i>) = '
       + frac(f'{W}<sub><i>j</i></sub>',
              f'&sum;<sub><i>k</i>&thinsp;&isin;&thinsp;<i>S<sub>p</sub></i></sub> {W}<sub><i>k</i></sub>')
       + '</div>')
EQ2 = ('<div class="eq">Pr(<i>b</i>) = &prod;<sub><i>p</i>=1</sub><sup><i>m</i></sup> '
       + frac(f'{W}<sub><i>b<sub>p</sub></i></sub>',
              f'&sum;<sub><i>k</i>&thinsp;&isin;&thinsp;<i>S<sub>p</sub></i></sub> {W}<sub><i>k</i></sub>')
       + '</div>')
EQ3 = ('<div class="eq">gain = exp&thinsp;(&thinsp;'
       + frac('1', '<i>M</i>') + ' &sum;<sub>marks</sub> log '
       + frac('Pr(mark&thinsp;|&thinsp;<i>S<sub>p</sub></i>)',
              '1&thinsp;/&thinsp;|<i>S<sub>p</sub></i>|')
       + '&thinsp;)&thinsp; &minus; 1</div>')
EQ4 = ('<div class="eq">Pr(<i>b<sub>p</sub></i> &nbsp;|&nbsp; earlier marks) = '
       '&sum;<sub><i>c</i></sub> &pi;<sub><i>c</i></sub>(<i>p</i>) &middot; '
       + frac(f'{W}<sub><i>c</i>,&thinsp;<i>b<sub>p</sub></i></sub>',
              f'&sum;<sub><i>k</i>&thinsp;&isin;&thinsp;<i>S<sub>p</sub></i></sub> {W}<sub><i>c</i>,&thinsp;<i>k</i></sub>')
       + '</div>')

FACTS = [
    ("Parameters",
     "One worth per candidate, fitted as log-worths &theta;<sub><i>j</i></sub> = log "
     "<i>w</i><sub><i>j</i></sub>, so the model is a softmax over whoever is left. Worths are "
     "identified only up to a common scale; the fit is recentred to &sum;&theta; = 0 and the "
     "numbers above are rescaled to sum to one."),
    ("Objective",
     "Penalised negative log-likelihood, &minus;&sum; log Pr(<i>b</i>) + 0.01&thinsp;&sum;"
     "&theta;<sup>2</sup>, minimised by L-BFGS-B with the analytic gradient. The ridge pulls the "
     "worths toward equality, which matters only for candidates with almost no marks."),
    ("Partial ballots",
     "A ballot that stops after three marks contributes exactly three factors. The model is never "
     "asked what the voter thought of the candidates left blank, and no order is imputed for them; "
     "the blanks enter only as denominators."),
    ("Held out",
     "Five folds of whole ballots. Worths are refitted on four-fifths and every mark of the "
     "remaining fifth is scored once, so the average in equation 3 is over <i>marks</i>, not "
     "ballots, and long ballots carry more weight."),
    ("Camps",
     "Two classes, EM for 20 iterations: each ballot gets a posterior over classes, and each "
     "class's worths are a PL fit weighted by those posteriors. Scoring is sequential (equation "
     "4) &mdash; a ballot is never assigned to one camp, the earlier marks just shift "
     "&pi;<sub><i>c</i></sub>(<i>p</i>) before the next one is predicted. Fitted only where "
     "there are at least 150 ballots, from a random Dirichlet start, which is why two runs of "
     "the same table differ by a couple of hundredths."),
    ("What is assumed away",
     "Plackett&ndash;Luce is Luce's choice axiom applied down the ballot: the relative odds of two "
     "candidates are the same at every position, and nothing about a mark matters except which "
     "candidates remain. No position effects, no ties, no candidate covariates, no dependence "
     "between marks. The two-class mixture is the first and cheapest relaxation of that."),
]


def main():
    st0 = steps(WORTHS, DEFAULT)
    sm = summary(st0)
    bars = "".join(
        f'<div class="w-row"><span class="mono w-name">{k}</span>'
        f'<input type="range" id="w-{k}" min="1" max="1000" value="{round(v*1000):d}"'
        f' aria-label="relative worth of {k}" class="w-slide">'
        f'<span class="w-val num" id="wv-{k}">{v:.3f}</span></div>'
        for k, v in WORTHS)
    chips = "".join(
        f'<button type="button" class="chip" id="chip-{k}" data-k="{k}"'
        f' aria-pressed="{"true" if k in DEFAULT else "false"}">{k}</button>'
        for k, _ in WORTHS)
    facts = "".join(f"<dt>{t}</dt><dd>{d}</dd>" for t, d in FACTS)

    html = f"""
<section class="widget" id="plw">
  <h3 class="w-head">The model, in one ballot</h3>
  <p class="w-intro">Plackett&ndash;Luce gives each candidate one number, a <em>worth</em>. A
  voter picks their first choice in proportion to worth, then picks again from whoever is left,
  in proportion to the same worths. Nothing else: no position effects, no second thoughts. The
  worths below are the ones fitted to Burlington 2006 &mdash; 9,784 ballots, six candidates,
  stored as codes because IRW keeps no candidate names.</p>
  <div class="w-grid">
    <div class="w-panel">
      <h4>Worths</h4>
      <div class="w-bars">{bars}</div>
      <button type="button" class="w-reset" id="w-reset">Reset to the fitted values</button>
    </div>
    <div class="w-panel">
      <h4>A ballot</h4>
      <p class="w-hint">Click candidates in the order a voter would mark them.</p>
      <div class="chips">{chips}</div>
      <div class="t-wrap"><table class="w-table">
        <thead><tr><th>mark</th><th>pick</th><th>model</th><th>guessing</th><th>ratio</th></tr></thead>
        <tbody id="w-rows">{render_rows(st0)}</tbody>
      </table></div>
      <p class="w-out" id="w-out">This ballot is <span class="num">{sm['ratio']:.1f}&times;</span>
      as likely under the fitted worths as under guessing &mdash; a gain of
      <span class="num">{sm['gain']:.3f}</span> per mark.</p>
    </div>
  </div>

  <div class="w-math">
    <h4>The arithmetic, exactly</h4>
    <p>Write <i>w<sub>j</sub></i> for candidate <i>j</i>'s worth and <i>S<sub>p</sub></i> for the
    set of candidates still unmarked when the voter makes their <i>p</i>th mark. The whole model
    is one line, and it is the line in the <em>model</em> column above:</p>
    {EQ1}
    <p>A ballot <i>b</i> with <i>m</i> marks is that line applied down the ballot &mdash; each
    mark a choice from a set one smaller than the last:</p>
    {EQ2}
    <p class="w-live" id="w-live">{render_live(st0)}</p>
    <p>The baseline is what a reader who knows nothing about the candidates would say: a uniform
    pick from <i>S<sub>p</sub></i>. Comparing the two mark by mark, over the <i>M</i> held-out
    marks of a table, gives the statistic this page reports &mdash; a geometric mean likelihood
    ratio, minus one, so that 0 means the fitted order is worth nothing and 0.20 means each
    observed mark is on average 20% more likely under it than under ignorance:</p>
    {EQ3}
    <p>A per-mark quantity is the point: it is comparable across a 35-candidate mayoral race and
    a three-name survey question, where a per-ballot likelihood is not. The <em>camps</em> gain is
    the same statistic with a two-class mixture in the numerator and the single-order fit in the
    denominator. Two sets of worths, a class probability &pi;<sub><i>c</i></sub> updated by the
    marks already seen, and the next mark predicted by the blend:</p>
    {EQ4}
    <dl class="w-facts">{facts}</dl>
  </div>

  <p class="w-foot">One difference between the widget and the page: here the worths are fitted on
  all the ballots and the arithmetic is run on a ballot of your choosing, which is how a model
  flatters itself. On the page the worths are fitted on four-fifths of the ballots and scored on
  the fifth they never saw.</p>
</section>
<script>
(function () {{
  var FIT = {json.dumps(dict(WORTHS))};
  var KEYS = {json.dumps([k for k, _ in WORTHS])};
  var ballot = {json.dumps(DEFAULT)};
  var $ = function (id) {{ return document.getElementById(id); }};
  function worths() {{
    var raw = {{}}, tot = 0;
    KEYS.forEach(function (k) {{ var v = +$("w-" + k).value; raw[k] = v; tot += v; }});
    KEYS.forEach(function (k) {{ raw[k] = raw[k] / tot; }});
    return raw;
  }}
  function fmt(x) {{ return (100 * x).toFixed(1) + "%"; }}
  function ff(a, b) {{ return '<span class="ff">' + a + "&#8201;/&#8201;" + b + "</span>"; }}
  function render() {{
    var w = worths(), rows = "", lr = 0, pool = KEYS.slice();
    var mod = [], base = [], pm = 1, pu = 1;
    KEYS.forEach(function (k) {{ $("wv-" + k).textContent = w[k].toFixed(3); }});
    ballot.forEach(function (pick, i) {{
      var tot = 0;
      pool.forEach(function (k) {{ tot += w[k]; }});
      var p = w[pick] / tot, u = 1 / pool.length;
      lr += Math.log(p / u);
      pm *= p; pu *= u;
      mod.push(ff(w[pick].toFixed(3), tot.toFixed(3)));
      base.push(ff("1", pool.length));
      rows += '<tr><td class="step">' + (i + 1) + '</td><td class="pick mono">' + pick +
        '</td><td>' + fmt(p) + '</td><td>' + fmt(u) + '</td><td class="rat">&times;' +
        (p / u).toFixed(2) + '</td></tr>';
      pool = pool.filter(function (k) {{ return k !== pick; }});
    }});
    $("w-rows").innerHTML = rows;
    KEYS.forEach(function (k) {{
      $("chip-" + k).setAttribute("aria-pressed", ballot.indexOf(k) >= 0 ? "true" : "false");
    }});
    if (!ballot.length) {{
      $("w-out").innerHTML = "No marks yet &mdash; click a candidate above.";
      $("w-live").innerHTML = "Click a candidate above and this line fills in.";
    }} else {{
      $("w-out").innerHTML = "This ballot is <span class='num'>" + Math.exp(lr).toFixed(1) +
        "&times;</span> as likely under these worths as under guessing &mdash; a gain of " +
        "<span class='num'>" + (Math.exp(lr / ballot.length) - 1).toFixed(3) + "</span> per mark.";
      $("w-live").innerHTML = "Pr(this ballot) = " + mod.join(" &times; ") +
        " = <span class='num'>" + pm.toFixed(5) + "</span>, against " + base.join(" &times; ") +
        " = <span class='num'>" + pu.toFixed(5) + "</span> under guessing.";
    }}
  }}
  KEYS.forEach(function (k) {{
    $("w-" + k).addEventListener("input", render);
    $("chip-" + k).addEventListener("click", function () {{
      var i = ballot.indexOf(k);
      if (i >= 0) {{ ballot.splice(i, 1); }} else {{ ballot.push(k); }}
      render();
    }});
  }});
  $("w-reset").addEventListener("click", function () {{
    KEYS.forEach(function (k) {{ $("w-" + k).value = Math.round(FIT[k] * 1000); }});
    ballot = {json.dumps(DEFAULT)}.slice();
    render();
  }});
  render();
}})();
</script>
"""
    (BUILD / "widget.html").write_text(html)
    print("wrote build/widget.html; default ballot", DEFAULT, "gain", round(sm["gain"], 3))


if __name__ == "__main__":
    main()
