"""Interactive strip chart of IMV results for nn_irt.qmd: one row per model, a
dot per table. Hovering (or tapping) a dot highlights the same table in every
row and shows its values. Plain d3 from cdnjs, so the chart needs no notebook
widget and survives being frozen or copied out of the site.
"""
import json

D3_URL = "https://cdnjs.cloudflare.com/ajax/libs/d3/7.9.0/d3.min.js"


def imv_stripchart_html(records, models, labels, group_colour, div_id="imv-strip",
                        xlabel="IMV relative to PyTorch 2PL"):
    """records: list of {table, short, group, model, imv}; models: row order."""
    payload = json.dumps({
        "records": records,
        "models": models,
        "labels": labels,
        "colours": group_colour,
        "xlabel": xlabel,
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
<script src="{D3_URL}"></script>
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
