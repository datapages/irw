"""Assemble the draft artifact page from build/ fragments.

Usage: python3 build_page.py <out.html>   (rerun after make_figs.py)
"""
import json, sys, pathlib

HERE = pathlib.Path(__file__).parent
B = HERE / "build"
S = json.load(open(B / "stats.json"))
fam = S["by_family"]
OUT = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else HERE / "rankings_draft.html")

HEAD = """<title>How much of a ranking is shared</title>
<meta name="claude:viewport-fit-sides" content="cover">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Libre+Franklin:wght@500;600;700&family=Source+Serif+4:ital,opsz,wght@0,8..60,400;0,8..60,600;1,8..60,400&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>
/* Layout: one reading column; figures break out to a wider measure. */
:root{
  --paper:#fbfaf7; --panel:#f3f1ec; --ink:#16181c; --ink-2:#4c5159; --ink-3:#7b818b;
  --rule:#ddd9d0; --accent:#2a78d6; --accent-2:#c75a24; --mark:#2a78d6; --mark-hi:#eb6834;
  --flag:#f6efe2; --flag-rule:#d8c69c;
  --display:"Libre Franklin",system-ui,sans-serif;
  --body:"Source Serif 4",Georgia,serif;
  --mono:"IBM Plex Mono",ui-monospace,monospace;
}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){
  --paper:#15171a; --panel:#1d2024; --ink:#eceae4; --ink-2:#b4b8bf; --ink-3:#848a93;
  --rule:#2e3237; --accent:#3987e5; --accent-2:#e0783f; --mark:#3987e5; --mark-hi:#d95926;
  --flag:#241f17; --flag-rule:#5a4a2c; color-scheme:dark;
}}
:root[data-theme="dark"]{
  --paper:#15171a; --panel:#1d2024; --ink:#eceae4; --ink-2:#b4b8bf; --ink-3:#848a93;
  --rule:#2e3237; --accent:#3987e5; --accent-2:#e0783f; --mark:#3987e5; --mark-hi:#d95926;
  --flag:#241f17; --flag-rule:#5a4a2c; color-scheme:dark;
}
body{background:var(--paper);color:var(--ink);font-family:var(--body);font-size:17px;line-height:1.62;
  padding-left:max(16px,env(safe-area-inset-left,0px));padding-right:max(16px,env(safe-area-inset-right,0px));
  padding-block:0 4rem;-webkit-text-size-adjust:100%}
.wrap{max-width:62rem;margin:0 auto}
.col{max-width:36rem;margin-inline:auto}
h1,h2,h3,h4,.eyebrow,.pt,.tk,.ax,.lab,th,.n,figcaption{font-family:var(--display)}
h1{font-size:clamp(1.9rem,1.2rem + 2.6vw,2.9rem);line-height:1.08;font-weight:700;letter-spacing:-.02em;
  text-wrap:balance;margin:.4em 0 .3em}
h2{font-size:1.32rem;font-weight:600;letter-spacing:-.01em;margin:3rem 0 .2rem;text-wrap:balance}
h3{font-size:1.02rem;font-weight:600;margin:2rem 0 .1rem}
h4{font-size:.8rem;font-weight:600;text-transform:uppercase;letter-spacing:.08em;color:var(--ink-2);margin:1.6rem 0 .4rem}
p{margin:.75em 0}
.eyebrow{font-size:.74rem;text-transform:uppercase;letter-spacing:.13em;color:var(--ink-3);margin-top:2.2rem}
.lede{font-size:1.14rem;color:var(--ink-2)}
a{color:var(--accent)}
code,.mono{font-family:var(--mono);font-size:.86em}
.flag{background:var(--flag);border:1px solid var(--flag-rule);border-left-width:3px;border-radius:2px;
  padding:.85rem 1rem;margin:1.5rem 0;font-size:.92rem;line-height:1.5}
.flag strong{font-family:var(--display);font-weight:600}
.num{font-family:var(--display);font-variant-numeric:tabular-nums;font-weight:600;color:var(--ink)}
hr{border:0;border-top:1px solid var(--rule);margin:2.5rem 0}
/* figures */
.fig-wrap{min-width:0;overflow-x:auto}
svg.fig{display:block;width:100%;height:auto;max-width:100%;min-width:300px}
.grid{stroke:var(--rule);stroke-width:1}
.axis{stroke:var(--ink-3);stroke-width:1}
.diag{stroke:var(--ink-3);stroke-width:1;stroke-dasharray:3 3}
.pt-mark{fill:var(--mark);fill-opacity:.5;stroke:var(--paper);stroke-width:1}
.pt-mark.hi{fill:var(--mark-hi);fill-opacity:1}
.tk{font-size:9.5px;fill:var(--ink-3);font-variant-numeric:tabular-nums}
.ax{font-size:10.5px;fill:var(--ink-2)}
.lab{font-size:10px;fill:var(--ink);font-weight:500}
.pt{font-size:11px;fill:var(--ink);font-weight:600}
.grid-2{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,21rem),1fr));gap:1.4rem 1.6rem;margin:1.4rem 0}
figure{margin:0}
figcaption{font-size:.84rem;line-height:1.45;color:var(--ink-2);margin-top:.3rem}
figcaption .n{color:var(--ink-3);font-weight:500}
.fig-note{max-width:36rem;margin:.2rem auto 0;font-family:var(--display);font-size:.79rem;line-height:1.45;color:var(--ink-3)}
.wide{margin:1.4rem 0}
/* tables */
.t-wrap{overflow-x:auto;min-width:0}
table{border-collapse:collapse;font-size:.86rem;width:100%}
th{text-align:left;font-weight:600;font-size:.72rem;text-transform:uppercase;letter-spacing:.06em;
  color:var(--ink-3);border-bottom:1px solid var(--rule);padding:.35rem .7rem .35rem 0;white-space:nowrap}
td{padding:.3rem .7rem .3rem 0;border-bottom:1px solid var(--rule);font-variant-numeric:tabular-nums;white-space:nowrap}
td.mono{font-size:.78rem;white-space:normal}
th:not(:first-child),td:not(:first-child){text-align:right}
/* Plackett-Luce explainer */
.widget{background:var(--panel);border:1px solid var(--rule);border-radius:3px;padding:1.1rem 1.2rem 1.3rem;margin:2rem 0 2.5rem}
.w-head{margin:0 0 .3rem;font-size:1.05rem}
.w-intro,.w-foot{font-size:.9rem;line-height:1.5;color:var(--ink-2);margin:.3rem 0 0;max-width:46rem}
.w-foot{margin-top:1rem;padding-top:.8rem;border-top:1px solid var(--rule)}
.w-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,17rem),1fr));gap:1.1rem 1.8rem;margin-top:1rem}
.w-panel{min-width:0}
.w-panel h4{margin:0 0 .5rem}
.w-bars{display:flex;flex-direction:column;gap:.3rem}
.w-row{display:grid;grid-template-columns:2.6rem 1fr 3rem;align-items:center;gap:.5rem}
.w-name{font-size:.8rem;color:var(--ink-2)}
.w-val{font-size:.82rem;text-align:right;font-variant-numeric:tabular-nums}
.w-slide{width:100%;accent-color:var(--accent);min-width:0}
.w-reset{margin-top:.7rem;font-family:var(--display);font-size:.78rem;background:none;color:var(--ink-2);
  border:1px solid var(--rule);border-radius:2px;padding:.3rem .6rem;cursor:pointer}
.w-reset:hover{color:var(--ink);border-color:var(--ink-3)}
.w-hint{font-size:.82rem;color:var(--ink-3);margin:0 0 .5rem}
.chips{display:flex;flex-wrap:wrap;gap:.35rem;margin-bottom:.7rem}
.chip{font-family:var(--mono);font-size:.8rem;padding:.25rem .55rem;border:1px solid var(--rule);
  border-radius:2px;background:var(--paper);color:var(--ink-2);cursor:pointer}
.chip[aria-pressed="true"]{background:var(--accent);border-color:var(--accent);color:#fff}
.chip:hover{border-color:var(--ink-3)}
.w-table{font-size:.84rem}
.w-table td.step{color:var(--ink-3)}
.w-table td.pick{color:var(--ink)}
.w-table td.rat{color:var(--accent-2);font-weight:500}
.w-table th:not(:first-child),.w-table td:not(:first-child){text-align:right}
.w-out{font-size:.9rem;margin:.7rem 0 0;color:var(--ink-2)}
/* the model written out */
.w-math{max-width:44rem;margin-top:1.5rem;padding-top:1rem;border-top:1px solid var(--rule)}
.w-math h4{margin-top:0}
.w-math p{font-size:.9rem;line-height:1.55;color:var(--ink-2);margin:.7em 0}
.eq{text-align:center;font-size:1rem;color:var(--ink);margin:1.1rem 0;padding:.15rem 0;
  overflow-x:auto;overflow-y:hidden;white-space:nowrap;min-width:0}
.eq i{font-style:italic}
.eq sub,.eq sup{font-size:.7em}
.frac{display:inline-flex;flex-direction:column;align-items:center;vertical-align:middle;
  line-height:1.3;margin:0 .15em}
.fnum{padding:0 .4em .12em;border-bottom:1px solid currentColor}
.fden{padding:.12em .4em 0}
.w-live{font-family:var(--mono);font-size:.76rem!important;line-height:1.75!important;
  background:var(--paper);border:1px solid var(--rule);border-radius:2px;padding:.55rem .7rem;
  overflow-x:auto;white-space:nowrap;min-width:0}
.ff{font-variant-numeric:tabular-nums;white-space:nowrap}
.w-facts{margin:1.4rem 0 0;font-size:.87rem;line-height:1.5;color:var(--ink-2)}
.w-facts dt{font-family:var(--display);font-size:.71rem;font-weight:600;text-transform:uppercase;
  letter-spacing:.07em;color:var(--ink-3);margin-top:.85rem}
.w-facts dd{margin:.15rem 0 0}
:focus-visible{outline:2px solid var(--accent);outline-offset:2px}
@media (prefers-reduced-motion:reduce){*{animation:none!important;transition:none!important}}
footer{margin-top:3rem;padding-top:1.2rem;border-top:1px solid var(--rule);font-size:.84rem;color:var(--ink-3)}
footer code{color:var(--ink-2)}
</style>
"""


def main():
    cloud = (B / "fig_cloud.html").read_text()
    items = (B / "fig_items.html").read_text()
    tables = (B / "tables.html").read_text()
    widget = (B / "widget.html").read_text()
    e, d, j, s = fam["election"], fam["debian"], fam["jury"], fam["survey"]

    body = f"""
<div class="wrap">
<div class="col">
<p class="eyebrow">IRW vignette · draft</p>
<h1>How much of a ranking is shared?</h1>
<p class="lede">Two numbers per table, computed the same way for a six-candidate mayoral
election and for five jurors ranking twenty-six songs: how much a single consensus order
buys you over knowing nothing, and how much more two orders buy you over one.</p>

<div class="flag"><strong>Draft.</strong> A working artifact for Ben to read, not the
posted vignette. It reads the whole of <code>irw_rank:93f5</code> v1.5 (437 tables,
36,837,920 rows, released 2026-10-22): <span class="num">{S['n_fitted']}</span> tables fitted,
20 skipped because only two candidates stood — nineteen Minneapolis contests and Takoma Park
ward 4 — and nothing failed. Tables over 20,000 ballots are fitted on a seeded 20,000-ballot
sample ({S['n_capped']} of them). The open choice is in <em>One problem with the statistic</em>
below; the limitations section is yours.</div>
</div>

{widget}

<div class="col">
<h2>The ballots, and what a count throws away</h2>
<p>The 2006 mayoral election in Burlington, Vermont was run by instant runoff among six
candidates, five positions on the ballot. The city certified a final round of 4,761 votes to
3,986; a count written against the IRW table reproduces it (4,761 to 3,985, the one vote a
documented discrepancy in the city's own file). That count is a good check on the table and a
poor summary of it. It uses the ballots to answer one question — who wins — and then discards
9,784 orderings. Burlington is also the standing example of what the discarded part can hold:
the Condorcet winner, the candidate who beat each rival head to head, lost the runoff.</p>
<p>So ask something the count cannot. Fix a single order of the candidates and use it to
predict each next mark on a held-out ballot. How much better is that than guessing among the
candidates not yet marked? Call it the <em>consensus gain</em>. Then allow two orders instead
of one, let the ballots sort themselves between them, and ask how much the second order adds.
Call that the <em>camps gain</em>. Every ranking in IRW, however short, yields both.</p>

<h3>The two numbers</h3>
<p>Both are held-out likelihood ratios, the arithmetic of the widget above. We fit Plackett–Luce — one worth per candidate, each
mark a choice among those still available — on four-fifths of the ballots and score the
remaining fifth, mark by mark, against a uniform choice among the unmarked candidates. The
statistic is <code>exp(mean log-likelihood difference per choice) − 1</code>: a consensus gain
of 0.20 means the fitted order makes each observed mark 20% more likely than ignorance does.
The camps gain is the same quantity for a two-class Plackett–Luce mixture, scored against the
single-order fit and reported only where there are at least 150 ballots to split.</p>

<h2>What 437 tables look like</h2>
<p>The families separate, and not in the direction we expected.</p>
</div>

{cloud}

<div class="col">
<p>Elections are the agreeable case: median consensus gain
<span class="num">{e['cons_med']:.2f}</span> across <span class="num">{e['n']}</span> contests,
with the Minneapolis mayoral races at the top of the whole corpus. Debian's developers, voting
on project resolutions, sit just above them at <span class="num">{d['cons_med']:.2f}</span> —
a Condorcet ballot on a technical question looks much like a municipal election. Eurovision
juries are the <em>least</em> consensual bodies in the family, median
<span class="num">{j['cons_med']:.2f}</span> and a range of only
{j['cons_min']:.2f}–{j['cons_max']:.2f} across eighteen shows. Five professional jurors per
country, ranking the same twenty-six performances they have all just watched, agree less about
what they saw than a city's voters agree about who should run it. The survey rankings post the
highest median of all ({s['cons_med']:.2f} across {s['n']} tables), and that number is an
artifact rather than a finding; the next section says why.</p>
<p>The camps gain is where the corpus pays for its breadth. A second order beats a single one
in <span class="num">{S['camps_beat']}</span> of {S['n_with_camps']} tables, and they are not
spread evenly: <span class="num">124</span> of the 279 New South Wales districts, against 7 of
69 Minneapolis contests, 6 of 33 Debian votes and none of the six Eurovision finals. New South
Wales runs optional preferential voting, six candidates to a district and 2.4 marks to a
ballot, and its voters arrive in party blocs; the median district gets a consensus gain of
0.149 and a further 0.133 from splitting the electorate in two. One order describes a
Minneapolis council race. It does not describe a state seat in New South Wales.</p>
<p>The small cases are legible in the same way. Alaska's House District 10 in 2022 is three
candidates, two of them Republicans splitting a bloc: consensus 0.04, camps 0.15. Debian's
2019 vote on init systems and its 2026 vote on LLM usage behave the same way — the project was
not of one mind, and the mixture says so in a number. The extreme is
<code>atsusaka_2025_ranking_questions</code>, where the second class is worth ten times the
first order.</p>

<h2>One problem with the statistic</h2>
<p>Minneapolis 2013 has a consensus gain of 2.75, the largest in the corpus, and it is not
because Minneapolis voters are unusually like-minded. That ballot carried 35 mayoral
candidates, most of whom nobody ranked. Against a uniform baseline, learning that 30 of 35
candidates are near-hopeless is enormously informative, and it has nothing to do with whether
voters agree on an <em>order</em>. The same mechanism puts the French survey rankings at the
top of their family: <code>voterautrement_2024_self_top2</code> asks respondents to rank a
37-name European Parliament list and scores 1.62. Within elections the rank correlation
between candidate count and consensus gain is +0.17; across the whole corpus it is +0.05,
because Eurovision's 26-act ballots run the other way.</p>
</div>

{items}

<div class="col">
<p>This does not sink the vignette, but it does mean the consensus gain as specified answers
a compound question — how concentrated is the field, and how much do people agree beyond that.
Two ways out, both cheap: baseline each next mark against the <em>marginal</em> popularity of
the unmarked candidates rather than against a uniform draw, which removes the concentration
term by construction; or split the gain by position, reporting the first mark (concentration)
separately from the continuations (shared order). The second is the question-B machinery from
the October pilot, already written. My preference is the first as the headline number and the
split as the figure beneath it, but this is the one real choice left in the analysis and it is
yours.</p>

<h2>The tables at the edges</h2>
{tables}
<p class="mono" style="color:var(--ink-3);font-size:.78rem">* fitted on a seeded 20,000-ballot sample.</p>

<h2>What this draft does not yet settle</h2>
<p>Four things, in rough order of how much they would change the page.</p>
<p><strong>The baseline question above.</strong> Everything else is bookkeeping by comparison.</p>
<p><strong>The camps number carries real noise.</strong> The mixture is fitted by EM from a
random start, so it finds a different local optimum run to run. Two independent runs of the
same code on Alaska HD10 gave 0.129 and 0.145 — a 0.016 spread, the same order as the whole
camps gain on the quieter tables, where the consensus gains agreed to better than 0.01. Either
fit the mixture from several starts and keep the best training likelihood, or report only its
sign and rough size. Read nothing into camps differences below roughly 0.02 until that is
settled.</p>
<p><strong>New South Wales dominates the cloud.</strong> 279 of 437 tables are NSW districts,
so any unweighted summary across the family is mostly a summary of one state's 2015–2023
assembly elections — and since NSW is also where the camps result lives, that is a claim about
one electoral system as much as about rankings in general. The page should either weight by
jurisdiction or say plainly that it is showing contests, not places. Worth testing against
the Australian Senate data when it lands.</p>
<p><strong>Limitations are yours to write.</strong> The mechanical ones are listed here: the
ballot cap, NSW's weight, the 20 two-candidate contests dropped for having nothing to rank,
and single-peakedness set aside entirely. The substantive ones — what
a Plackett–Luce worth does and does not mean when voters are strategic, and whether a juror's
ranking of acts is the same kind of object as a voter's ranking of candidates — are the ones a
reader will want, and they should be in your words.</p>

<footer>
<p>Computed from <code>irw_rank:93f5</code> v1.5 (437 tables, 36,837,920 rows), staged locally
and row-count verified table by table against Redivis. Method and code reused unchanged from
<code>oneoff/rankings-vignette-pilot-2026-10-15/pilot.py</code> (Plackett–Luce, 5-fold
cross-validation, ridge 0.01 on log-worths, reserved <code>(overvote)</code> and
<code>(write-in)</code> marks dropped, first mark of a repeated candidate kept, gaps ignored);
driver, figures and this page in
<code>oneoff/rankings-vignette-draft-2026-10-22/</code>. Reproduction check: the 20 tables of
the October pilot refit to within 0.01 on the consensus gain. Candidate names are not stored
anywhere in IRW; the items are alphabetical codes. Drafted by Claude for Ben, 2026-10-22.</p>
</footer>
</div>
</div>
"""
    OUT.write_text(HEAD + body)
    print("wrote", OUT, f"{OUT.stat().st_size/1024:.0f} KB")


if __name__ == "__main__":
    main()
