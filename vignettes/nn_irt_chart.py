"""Interactive strip chart of IMV results for nn_irt.qmd: one row per model, a
dot per table. Hovering (or tapping) a dot highlights the same table in every
row and shows its values. Plain d3 from cdnjs, so the chart needs no notebook
widget and survives being frozen or copied out of the site.
"""
import json

D3_URL = "https://cdnjs.cloudflare.com/ajax/libs/d3/7.9.0/d3.min.js"


def imv_stripchart_html(records, models, labels, group_colour, div_id="imv-strip",
                        xlabel="IMV relative to PyTorch 2PL", xlim=0.05):
    """records: list of {table, short, group, model, imv}; models: row order.

    xlim sets the half-width of the window; values outside it are drawn as
    triangles at the edge with the real number in the tooltip.
    """
    payload = json.dumps({
        "records": records,
        "models": models,
        "labels": labels,
        "colours": group_colour,
        "xlabel": xlabel,
        "xlim": xlim,
    })
    return f"""
<div id="{div_id}" class="imv-strip"></div>
<style>
.imv-strip {{ font: 12px system-ui, sans-serif; position: relative; }}
.imv-strip .tip {{ position: absolute; pointer-events: none; background: #fff; border: 1px solid #bbb;
  border-radius: 4px; padding: 6px 8px; font-size: 11px; line-height: 1.35; box-shadow: 0 1px 4px rgba(0,0,0,.15);
  white-space: nowrap; display: none; z-index: 10; }}
.imv-strip .tip b {{ display: block; margin-bottom: 2px; }}
.imv-strip .tip .val {{ display: inline-block; min-width: 52px; text-align: right; font-variant-numeric: tabular-nums; }}
.imv-strip .legend span {{ display: inline-block; margin-right: 14px; }}
.imv-strip .legend i {{ display: inline-block; width: 10px; height: 10px; border-radius: 50%; margin-right: 4px; vertical-align: -1px; }}
</style>
<!-- Quarto pages load require.js; hide its define() while d3 loads, or d3
     registers as an AMD module and the global d3 below is undefined. -->
<script>window.__amdDefine = window.define; window.define = undefined;</script>
<script src="{D3_URL}"></script>
<script>window.define = window.__amdDefine;</script>
<script>
(function() {{
  const D = {payload};
  const root = d3.select("#{div_id}");
  const draw = () => {{
    root.selectAll("*").remove();
    const width = Math.max(520, root.node().getBoundingClientRect().width || 800);
    const m = {{top: 8, right: 16, bottom: 36, left: 150}};
    const rowH = 34, height = m.top + m.bottom + rowH * D.models.length;
    const svg = root.append("svg").attr("width", width).attr("height", height)
      .attr("viewBox", `0 0 ${{width}} ${{height}}`).style("max-width", "100%");
    // fixed +/- 0.05 window: a few Rasch losses are far larger and would squash
    // everything else; those are drawn as triangles at the edge, values in the tip
    const lim = D.xlim || 0.05;
    const x = d3.scaleLinear().domain([-lim, lim]).range([m.left, width - m.right]).clamp(true);
    const clipped = d => Math.abs(d.imv) > lim;
    const y = d3.scalePoint().domain(D.models).range([m.top + rowH / 2, height - m.bottom - rowH / 2]);
    // zero line, row guides, axis
    svg.append("g").selectAll("line").data(D.models).join("line")
      .attr("x1", m.left).attr("x2", width - m.right).attr("y1", d => y(d)).attr("y2", d => y(d))
      .attr("stroke", "#e6e6e6");
    svg.append("line").attr("x1", x(0)).attr("x2", x(0)).attr("y1", m.top).attr("y2", height - m.bottom)
      .attr("stroke", "#555").attr("stroke-width", 1);
    svg.append("g").attr("transform", `translate(0,${{height - m.bottom}})`)
      .call(d3.axisBottom(x).ticks(9).tickFormat(d3.format("+.2f")))
      .call(g => g.select(".domain").remove()).selectAll("text").style("font-size", "11px");
    svg.append("text").attr("x", m.left).attr("y", height - 4).style("font-size", "11px").style("fill", "#555")
      .text(D.xlabel);
    svg.append("g").selectAll("text").data(D.models).join("text")
      .attr("x", m.left - 10).attr("y", d => y(d)).attr("dy", "0.35em").attr("text-anchor", "end")
      .style("font-size", "12px").text(d => D.labels[d] || d);
    // deterministic jitter per table so overlapping dots separate
    const tables = Array.from(new Set(D.records.map(d => d.table)));
    const jit = new Map(tables.map((t, i) => [t, ((i * 7) % tables.length) / tables.length - 0.5]));
    const cy = d => y(d.model) + jit.get(d.table) * (rowH - 14);
    const tri = d3.symbol().type(d3.symbolTriangle).size(70);
    const dots = svg.append("g").selectAll("path").data(D.records).join("path")
      .attr("d", d => clipped(d) ? tri() : d3.symbol().type(d3.symbolCircle).size(78)())
      .attr("transform", d => `translate(${{x(d.imv)}},${{cy(d)}})` + (clipped(d) ? (d.imv < 0 ? " rotate(-90)" : " rotate(90)") : ""))
      .attr("fill", d => D.colours[d.group]).attr("fill-opacity", 0.75)
      .attr("stroke", "#fff").attr("stroke-width", 1).style("cursor", "pointer");
    const link = svg.append("path").attr("fill", "none").attr("stroke", "#333").attr("stroke-width", 1)
      .attr("stroke-dasharray", "2,3").style("display", "none");
    const tip = root.append("div").attr("class", "tip");
    const byTable = d3.group(D.records, d => d.table);
    const fmt = d3.format("+.4f");
    function focus(t, ev) {{
      dots.attr("fill-opacity", d => d.table === t ? 1 : 0.12)
          .attr("stroke", d => d.table === t ? "#222" : "#fff").attr("stroke-width", d => d.table === t ? 1.5 : 1);
      const pts = D.models.map(mm => byTable.get(t).find(d => d.model === mm)).filter(Boolean);
      link.style("display", null).attr("d", d3.line().x(d => x(d.imv)).y(d => cy(d))(pts));
      const r = pts[0];
      tip.style("display", "block").html(`<b>${{r.short}}</b><span style="color:#666">${{r.group}}</span><br>` +
        pts.map(d => `<span class="val">${{fmt(d.imv)}}</span>&nbsp; ${{D.labels[d.model] || d.model}}`).join("<br>"));
      const [px, py] = d3.pointer(ev, root.node());
      const tw = tip.node().offsetWidth;
      tip.style("left", `${{Math.min(px + 14, width - tw - 4)}}px`).style("top", `${{Math.max(0, py - 10)}}px`);
    }}
    function blur() {{
      dots.attr("fill-opacity", 0.75).attr("stroke", "#fff").attr("stroke-width", 1);
      link.style("display", "none"); tip.style("display", "none");
    }}
    dots.on("mouseenter", (ev, d) => focus(d.table, ev)).on("mousemove", (ev, d) => focus(d.table, ev))
        .on("mouseleave", blur).on("click", (ev, d) => {{ ev.stopPropagation(); focus(d.table, ev); }});
    svg.on("click", blur);
    const leg = root.append("div").attr("class", "legend").style("margin", "4px 0 0 " + m.left + "px");
    Object.entries(D.colours).forEach(([g, c]) => leg.append("span").html(`<i style="background:${{c}}"></i>${{g}}`));
    const q = new URLSearchParams(location.search).get("focus");
    if (q && byTable.has(q)) focus(q, {{clientX: 0, clientY: 0}});
  }};
  draw(); window.addEventListener("resize", draw);
}})();
</script>
"""


def facet_scatter_html(records, panels, group_colour, div_id="facet-scatter",
                       xlabel="", ylabel="", notes=None, xlog=True):
    """Small multiples of a scatter, one panel per model, sharing both axes.

    records: list of {table, short, group, panel, x, y}. Hovering a point picks
    out that table in every panel and lists its value in each, the same gesture
    as the strip chart above. A dashed line per panel marks the panel mean.
    notes: optional {panel: subtitle} drawn under each panel title.
    """
    payload = json.dumps({
        "records": records, "panels": panels, "colours": group_colour,
        "xlabel": xlabel, "ylabel": ylabel, "notes": notes or {}, "xlog": bool(xlog),
    })
    return f"""
<div id="{div_id}" class="imv-strip"></div>
<script>window.__amdDefine = window.define; window.define = undefined;</script>
<script src="{D3_URL}"></script>
<script>window.define = window.__amdDefine;</script>
<script>
(function() {{
  const D = {payload};
  const root = d3.select("#{div_id}");
  const draw = () => {{
    root.selectAll("*").remove();
    const width = Math.max(560, root.node().getBoundingClientRect().width || 900);
    const m = {{top: 34, right: 8, bottom: 44, left: 56}}, gap = 12;
    const height = 250;
    const n = D.panels.length;
    const pw = (width - m.left - m.right - gap * (n - 1)) / n;
    const svg = root.append("svg").attr("width", width).attr("height", height)
      .attr("viewBox", `0 0 ${{width}} ${{height}}`).style("max-width", "100%");
    const xs = D.records.map(d => d.x), ys = D.records.map(d => d.y);
    const xdom = [d3.min(xs), d3.max(xs)];
    const pad = (d3.max(ys) - d3.min(ys)) * 0.08;
    const y = d3.scaleLinear().domain([d3.min(ys) - pad, d3.max(ys) + pad])
      .range([height - m.bottom, m.top]);
    const byPanel = d3.group(D.records, d => d.panel);
    const fmt = d3.format("+.4f");
    const tip = root.append("div").attr("class", "tip");
    const all = [];
    D.panels.forEach((pn, i) => {{
      const x0 = m.left + i * (pw + gap);
      const x = (D.xlog ? d3.scaleLog() : d3.scaleLinear()).domain(xdom).range([x0, x0 + pw]).nice();
      const g = svg.append("g");
      g.append("rect").attr("x", x0).attr("y", m.top).attr("width", pw).attr("height", height - m.top - m.bottom)
        .attr("fill", "none").attr("stroke", "#e6e6e6");
      g.append("line").attr("x1", x0).attr("x2", x0 + pw).attr("y1", y(0)).attr("y2", y(0))
        .attr("stroke", "#555").attr("stroke-width", 1);
      const rows = byPanel.get(pn) || [];
      const mu = d3.mean(rows, d => d.y);
      g.append("line").attr("x1", x0).attr("x2", x0 + pw).attr("y1", y(mu)).attr("y2", y(mu))
        .attr("stroke", "#eb6834").attr("stroke-width", 1.4).attr("stroke-dasharray", "4,3");
      g.append("text").attr("x", x0 + pw / 2).attr("y", 14).attr("text-anchor", "middle")
        .style("font-size", "12px").text(pn);
      if (D.notes[pn]) g.append("text").attr("x", x0 + pw / 2).attr("y", 27).attr("text-anchor", "middle")
        .style("font-size", "10.5px").style("fill", "#666").text(D.notes[pn]);
      g.append("g").attr("transform", `translate(0,${{height - m.bottom}})`)
        .call(d3.axisBottom(x).tickValues([20, 40, 80]).tickFormat(d3.format("d")).tickSize(3))
        .call(gg => gg.select(".domain").remove()).selectAll("text").style("font-size", "10px");
      if (i === 0) g.append("g").attr("transform", `translate(${{x0}},0)`)
        .call(d3.axisLeft(y).ticks(5).tickFormat(d3.format("+.2f")).tickSize(3))
        .call(gg => gg.select(".domain").remove()).selectAll("text").style("font-size", "10px");
      all.push(g.append("g").selectAll("circle").data(rows).join("circle")
        .attr("cx", d => x(d.x)).attr("cy", d => y(d.y)).attr("r", 4.2)
        .attr("fill", d => D.colours[d.group]).attr("fill-opacity", 0.75)
        .attr("stroke", "#fff").attr("stroke-width", 1).style("cursor", "pointer"));
    }});
    svg.append("text").attr("x", m.left).attr("y", height - 4)
      .style("font-size", "11px").style("fill", "#555").text(D.xlabel);
    svg.append("text").attr("transform", `rotate(-90)`).attr("x", -(m.top + height - m.bottom) / 2)
      .attr("y", 12).attr("text-anchor", "middle").style("font-size", "11px").style("fill", "#555")
      .text(D.ylabel);
    const byTable = d3.group(D.records, d => d.table);
    function focus(t, ev) {{
      all.forEach(sel => sel.attr("fill-opacity", d => d.table === t ? 1 : 0.12)
        .attr("stroke", d => d.table === t ? "#222" : "#fff")
        .attr("stroke-width", d => d.table === t ? 1.5 : 1)
        .attr("r", d => d.table === t ? 5.2 : 4.2));
      const pts = byTable.get(t);
      const r = pts[0];
      tip.style("display", "block").html(
        `<b>${{r.short}}</b><span style="color:#666">${{r.group}} &middot; ${{r.x}} responses/person</span><br>` +
        D.panels.map(pn => {{
          const d = pts.find(q => q.panel === pn);
          return d ? `<span class="val">${{fmt(d.y)}}</span>&nbsp; ${{pn}}` : "";
        }}).filter(Boolean).join("<br>"));
      const [px, py] = d3.pointer(ev, root.node());
      const tw = tip.node().offsetWidth;
      tip.style("left", `${{Math.min(px + 14, width - tw - 4)}}px`).style("top", `${{Math.max(0, py - 10)}}px`);
    }}
    function blur() {{
      all.forEach(sel => sel.attr("fill-opacity", 0.75).attr("stroke", "#fff")
        .attr("stroke-width", 1).attr("r", 4.2));
      tip.style("display", "none");
    }}
    all.forEach(sel => sel.on("mouseenter", (ev, d) => focus(d.table, ev))
      .on("mousemove", (ev, d) => focus(d.table, ev)).on("mouseleave", blur)
      .on("click", (ev, d) => {{ ev.stopPropagation(); focus(d.table, ev); }}));
    svg.on("click", blur);
    const leg = root.append("div").attr("class", "legend").style("margin", "2px 0 0 " + m.left + "px");
    Object.entries(D.colours).forEach(([g, c]) => leg.append("span").html(`<i style="background:${{c}}"></i>${{g}}`));
  }};
  draw(); window.addEventListener("resize", draw);
}})();
</script>
"""
