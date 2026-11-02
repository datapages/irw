"""Assemble the draft artifact page from build/ fragments.

Usage: python3 build_page.py <out.html>   (rerun after make_figs.py)
"""
import json, sys, pathlib

HERE = pathlib.Path(__file__).parent
B = HERE / "build"
S = json.load(open(B / "stats.json"))
fam = S["by_family"]
OUT = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else HERE / "rankings_draft.html")

HEAD = """<!doctype html>
<html lang="en">
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>How much of a ranking is shared</title>
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
/* the two numbers */
.w-tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,14rem),1fr));gap:.8rem;margin:1rem 0 .6rem}
.w-tile{background:var(--paper);border:1px solid var(--rule);border-radius:3px;padding:.75rem .9rem .7rem}
.w-tile-k{font-family:var(--display);font-size:.72rem;font-weight:600;text-transform:uppercase;letter-spacing:.08em;color:var(--ink-2)}
.w-tile-v{font-size:2rem;line-height:1.1;margin:.15rem 0 .25rem;color:var(--accent-2)}
.w-tile-s{font-family:var(--display);font-size:.76rem;line-height:1.4;color:var(--ink-3)}
.w-camps{margin-top:1.4rem;padding-top:1rem;border-top:1px solid var(--rule)}
.w-camps h4{margin:0 0 .3rem}
.w-camp h5{font-family:var(--display);font-size:.8rem;font-weight:600;margin:.6rem 0 .3rem;color:var(--ink-2)}
.w-camp h5 .n{font-weight:500;color:var(--ink-3)}
.w-row-ro{grid-template-columns:2.6rem 1fr 3rem}
.w-bar{height:.55rem;background:var(--rule);border-radius:2px;overflow:hidden;min-width:0}
.w-bar span{display:block;height:100%;background:var(--accent)}
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

    G = S["by_group"]
    eb = S["elec_by_items"]
    nn = S["elec_nonnsw"]
    nsw, ak, nyc, sf, oak, mpls, cam, ev, bd = (G["nsw"], G["alaska"], G["nyc"], G["sf"], G["oakland"],
                                                G["mpls"], G["cambridge"], G["eurovision"], G["ballondor"])
    skipped = S["skipped"]
    body = f"""
<div class="wrap">
<div class="col">
<p class="eyebrow">IRW vignette · draft, second run</p>
<h1>How much of a ranking is shared?</h1>
<p class="lede">Two numbers per table, computed the same way for a six-candidate mayoral
election and for five jurors ranking twenty-six songs: how much a single consensus order
buys you over knowing nothing, and how much more two orders buy you over one.</p>

<div class="flag"><strong>Draft, rerun 2026-11-02.</strong> A working artifact for Ben to read, not the
posted vignette. It reads both shards of the family, <code>irw_rank</code> v2.1 (950 tables) and
<code>irw_rank_2</code> v2.1 (68 tables): {S['n_results']:,} tables and 99,224,348 rows, released
2026-11-02. <span class="num">{S['n_fitted']}</span> tables fitted, {S['n_skipped']} skipped because
only two candidates stood (there is nothing to rank; {skipped['alaska']} of them are Alaska legislative
seats, {skipped['nyc']} New York council primaries, {skipped['oakland']} Oakland contests), and nothing
failed. Tables over 20,000 ballots are fitted on a seeded 20,000-ballot sample ({S['n_capped']} of them).
The October draft covered 437 tables; the 581 new ones are almost all FairVote ballot files from
US cities, plus three Ballon d'Or juries. The open choice is in <em>One problem with the statistic</em>
below, and the new data make it more pressing, not less; the limitations section is yours.</div>
</div>

{widget}

<div class="col">
<h2>The ballots, and what a count throws away</h2>
<p>The 2006 mayoral election in Burlington, Vermont was run by instant runoff among six
candidates, five positions on the ballot. The city certified a final round of 4,761 votes to
3,986; a count written against the IRW table reproduces it (4,761 to 3,985, the one vote a
documented discrepancy in the city's own file). That count is a good check on the table and a
poor summary of it. The city received 9,865 ballots; 9,784 of them ranked at least one
candidate (77 were blank and 4 marked only an overvote), and that is the table. The certified
final round accounts for 8,746 of those, the other 1,038 having ranked neither finalist and
been set aside as exhausted. So the count uses the ballots to answer one question, who wins,
and in doing so collapses 9,784 orderings over six candidates into two numbers. Burlington is
also the standing example of what the collapsed part can hold: the Condorcet winner, the
candidate who beat each rival head to head, lost the runoff.</p>
<p>So we ask something the count cannot. We fix a single order of the candidates and use it to
predict each next mark on a held-out ballot. How much better is that than guessing among the
candidates not yet marked? We call it the <em>consensus gain</em>. Then we allow two orders instead
of one, let the ballots sort themselves between them, and ask how much the second order adds.
We call that the <em>camps gain</em>. Every ranking in IRW, however short, yields both.</p>
<p>Both are per-mark likelihood ratios against a reference, so they read the same way: 0 means
the model adds nothing, 0.20 means each mark is on average 20% more likely under it than under
the reference. The reference differs, and that is the point. The consensus gain is scored
against ignorance, the camps gain against the consensus order, so the second number is what the
first one missed. One caution before reading them together: the camps gain is scored mark by
mark, and before a ballot's first mark the model cannot tell which camp it is from, so it predicts
that mark with the blend of the two camps, which is almost exactly the consensus ranking. The
first mark earns no camps credit. What the camps gain detects is disagreement that <em>persists
down the ballot</em>: knowing a voter's first choice changes what they will do next. A split that
lives only in the first choice is invisible to it. Read together, then: a high consensus gain
with a camps gain near zero is an electorate whose ranking is shared once the first mark is made,
whether because it agrees about the order or because the field is so lopsided that one order is
all there is to know (Burlington is the first, split on one candidate and agreed on the other
five; the 35-candidate Minneapolis race is the second, which is why the baseline question below
matters). A low consensus gain with a camps gain above it is an electorate of two minds all the
way down: the first mark tells you the camp and the camp tells you the rest. Alaska's three-way
general elections, where a vote for one Republican predicts a second mark for the other, are the
clean case. Both low is indifference or noise; both high is a split electorate whose camps still
rank most of the field alike. The ratio of the two, camps over consensus, is the page's
one-number summary of structure: below one, a consensus with dissent; above one, a contest
between blocs.</p>

<h3>The two numbers</h3>
<p>Both are held-out likelihood ratios, the arithmetic of the widget above. We fit Plackett–Luce, one worth per candidate, each
mark a choice among those still available, on four-fifths of the ballots and score the
remaining fifth, mark by mark, against a uniform choice among the unmarked candidates. The
statistic is <code>exp(mean log-likelihood difference per choice) − 1</code>: a consensus gain
of 0.20 means the fitted order makes each observed mark 20% more likely than ignorance does.
The camps gain is the same quantity for a two-class Plackett–Luce mixture, scored against the
single-order fit and reported only where there are at least 150 ballots to split.</p>
<p class="flag"><strong>On the statistic.</strong> This form, a per-mark geometric-mean likelihood ratio
against a uniform draw, is held for now and is ours, not a named statistic. The same two
ingredients appear under three standard labels, any of which the page could report instead or
alongside: the <em>logarithmic score</em> difference in bits per mark (log<sub>2</sub>(1 + gain), the
forecast-verification form, with a house precedent in the bits-per-item vignette); the
<em>perplexity ratio</em>, since gain + 1 is exactly uniform perplexity over model perplexity;
and McFadden's <em>&rho;&sup2;</em>, 1 &minus; LL(model)/LL(null), the discrete-choice convention,
whose standard null predicts each alternative at its share. That last null is the
marginal-popularity baseline discussed below, so adopting &rho;&sup2; would settle the baseline
question at the same time. A K-way IMV generalisation remains the other candidate.</p>

<h2>What {S['n_fitted']} tables look like</h2>
<p>The families still separate, but the widest spread is now <em>within</em> elections, and it
tracks the length of the ballot.</p>
</div>

{cloud}

<div class="col">
<p>Elections have a median consensus gain of <span class="num">{e['cons_med']:.2f}</span> across
<span class="num">{e['n']}</span> contests in New South Wales and {S['n_election_groups'] - 1} US
jurisdictions, and a range from 0.00 to {e['cons_max']:.1f}. The top of the corpus is no longer
Minneapolis. It is <code>rcv_easthampton_20211102_mayor</code>, a Massachusetts city whose 2021
mayoral file codes 63 written-in names as candidates, 67 in all, with 1.7 marks a ballot: once the
model learns that 65 of them are nobody's choice, every held-out mark looks twenty times more
likely than a uniform guess. Cambridge's STV council and school races (23 to 45 candidates) and
Portland, Oregon's first ranked council elections sit just below it for the same reason. At the
other end, the {eb['3']['n']} three-candidate contests have a median gain of {eb['3']['cons_med']:.3f};
four or five candidates give {eb['4-5']['cons_med']:.2f}, six to nine {eb['6-9']['cons_med']:.2f},
ten or more {eb['10+']['cons_med']:.2f}. Debian's developers, voting on project resolutions, sit at
<span class="num">{d['cons_med']:.2f}</span>, which is what a four-candidate municipal election
looks like. Eurovision juries remain the least consensual bodies in the corpus, median
<span class="num">{ev['cons_med']:.2f}</span> across eighteen shows: five professional jurors per
country, ranking the same twenty-six performances they have all just watched, agree less about what
they saw than a city's voters agree about who should run it. The three Ballon d'Or juries, new this
run, score {bd['cons_med']:.2f} and look like the other end of the jury world, but they are top-five
and top-ten ballots over 25 to 30 names, so the same concentration mechanism is doing most of the
work. The survey rankings post {s['cons_med']:.2f} across {s['n']} tables and the French
37-name European Parliament lists are, again, at the top.</p>
<p>The camps gain is where the corpus pays for its breadth, and the new tables sharpen the
finding. A second order beats a single one in <span class="num">{S['camps_beat']}</span> of
{S['n_with_camps']} tables, and they are in two places. <span class="num">{nsw['camps_beat']}</span>
of the {nsw['n']} New South Wales districts, as before: optional preferential voting, six candidates
to a district, {nsw['marks_med']} marks to a ballot, voters in party blocs, median consensus
{nsw['cons_med']:.3f} and a further {nsw['camps_med']:.3f} from splitting the electorate in two.
And <span class="num">{ak['camps_beat']}</span> of {ak['n']} Alaska legislative seats, which are
three- and four-candidate top-four general elections where two Republicans split a bloc against one
Democrat: consensus {ak['cons_med']:.3f}, camps {ak['camps_med']:.3f}, the one family in the corpus
where the second order is worth more than the first. Everywhere else in the United States the
mixture finds almost nothing: {nyc['camps_beat']} of {nyc['n_camps']} New York primaries,
{sf['camps_beat']} of {sf['n_camps']} San Francisco contests, {oak['camps_beat']} of {oak['n_camps']}
in Oakland, {mpls['camps_beat']} of {mpls['n_camps']} in Minneapolis; outside New South Wales,
{nn['camps_beat']} of {nn['n_camps']} election tables in all. Debian is {G['debian']['camps_beat']} of
{G['debian']['n']}, Eurovision finals none of six. One order describes a city council race in a
one-party city. It does not describe a state seat in New South Wales or a top-four general in
Alaska, and the difference is the party structure of the field, not the voting rule.</p>
<p>The small cases are legible in the same way. Alaska's House District 15 in 2022: consensus
0.006, camps 0.157. Debian's 2019 vote on init systems and its 2026 vote on LLM usage behave the
same way; the project was not of one mind, and the mixture says so in a number. The extremes are
survey tables: <code>atsusaka_2024_alaska_rcv_survey</code>, where a single order is worth nothing
and two orders are worth 1.35, and <code>corstange_2022_syria_factions_2015</code>, where the
second class is worth thirteen times the first.</p>

<h2>One problem with the statistic</h2>
<p>In October the within-elections rank correlation between candidate count and consensus gain
was +0.17 and this section was a caution. With {e['n']} contests it is
<span class="num">+{S['rho_items_cons_elec']:.2f}</span> (+{S['rho_items_cons_all']:.2f} across the
whole corpus), and the medians by ballot length above are a staircase. Against a uniform baseline,
learning that most of a long field is hopeless is enormously informative, and it has nothing to do
with whether voters agree on an <em>order</em>. Easthampton's 20.5 is the statistic measuring how
many write-in names a clerk transcribed. The Ballon d'Or's 2.7 is mostly that a hundred journalists
were handed thirty names and all put the same five at the top.</p>
</div>

{items}

<div class="col">
<p>This does not sink the vignette, but as the family stands the consensus gain answers a compound
question: how concentrated is the field, and how much do people agree beyond that. The first part
now dominates. Two ways out, both cheap: baseline each next mark against the <em>marginal</em>
popularity of the unmarked candidates rather than against a uniform draw, which removes the
concentration term by construction; or split the gain by position, reporting the first mark
(concentration) separately from the continuations (shared order). The second is the question-B
machinery from the October pilot, already written. My preference is the first as the headline
number and the split as the figure beneath it, and I would now make the change before writing the
page rather than after. It is still your call; nothing in the statistic was changed for this run.</p>

<h2>The tables at the edges</h2>
{tables}
<p class="mono" style="color:var(--ink-3);font-size:.78rem">* fitted on a seeded 20,000-ballot sample.</p>

<h2>What this draft does not yet settle</h2>
<p>Four things, in rough order of how much they would change the page.</p>
<p><strong>The baseline question above.</strong> Everything else is bookkeeping by comparison.</p>
<p><strong>The camps number carries real noise.</strong> The mixture is fitted by EM from a
random start, so it finds a different local optimum run to run. Two independent runs of the
same code on Alaska HD10 gave 0.129 and 0.145, a 0.016 spread, the same order as the whole
camps gain on the quieter tables, where the consensus gains agreed to better than 0.01. Either
fit the mixture from several starts and keep the best training likelihood, or report only its
sign and rough size. Camps differences below roughly 0.02 mean nothing until that is
settled. {S['camps_neg']} tables have a negative camps gain, where the mixture lost on held-out
ballots.</p>
<p><strong>What a table is.</strong> {nsw['n']} of {S['n_fitted']} fitted tables are NSW districts
and another {nyc['n'] + ak['n'] + mpls['n'] + sf['n'] + oak['n']} are New York, Alaska, Minneapolis,
San Francisco and Oakland, so an unweighted summary is a summary of six electoral systems. That is
better than one, and the camps contrast between New South Wales, Alaska and the US cities is only
visible because of it, but the page should still say plainly that it is showing contests, not
places. The {S['n_skipped']} two-candidate contests dropped here are also a fact about ranked-choice
elections in practice: more than a fifth of the family's tables had nothing to rank.</p>
<p><strong>Limitations are yours to write.</strong> The mechanical ones are listed here: the
ballot cap, the weight of a few jurisdictions, the {S['n_skipped']} two-candidate contests dropped,
write-in coding (Easthampton), and single-peakedness set aside entirely. The substantive ones, what
a Plackett–Luce worth does and does not mean when voters are strategic, and whether a juror's
ranking of acts is the same kind of object as a voter's ranking of candidates, are the ones a
reader will want, and they should be in your words.</p>

<footer>
<p>Computed from <code>irw_rank:93f5</code> v2.1 (950 tables) and <code>irw_rank_2:byyj</code> v2.1
(68 tables), {S['n_results']:,} tables and 99,224,348 rows, staged locally and row-count verified
table by table against Redivis. Method and code reused unchanged from the 2026-10-15 pilot
(Plackett–Luce, 5-fold cross-validation, ridge 0.01 on log-worths, reserved <code>(overvote)</code>
and <code>(write-in)</code> marks dropped, first mark of a repeated candidate kept, gaps ignored);
the 437 tables of the October run were not refitted, their results are reused as computed then.
Driver, figures and this page in <code>vignettes/rankings_shared/</code> on the
<code>rankings-shared-vignette</code> branch of datapages/irw. Candidate names are not stored
anywhere in IRW; the items are alphabetical codes. Drafted by Claude for Ben, 2026-10-22; rerun
2026-11-02.</p>
</footer>
</div>
</div>
"""
    OUT.write_text(HEAD + body)
    print("wrote", OUT, f"{OUT.stat().st_size/1024:.0f} KB")


if __name__ == "__main__":
    main()
