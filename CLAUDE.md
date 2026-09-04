# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **Item Response Warehouse (IRW)** website — a Quarto-based static documentation site published at https://itemresponsewarehouse.org/. It is not an R package; it is a publication-focused website with embedded R data analysis.

Project map: [`ARCHITECTURE.md`](https://github.com/ben-domingue/irw/blob/main/ARCHITECTURE.md) in `ben-domingue/irw` — which repo owns
what, where the data lives, and which document is authoritative when two disagree.

## Build Commands

```bash
quarto render                         # Full site build → _site/
quarto preview                        # Dev server on port 4200
quarto render vignettes/cfa.qmd       # Render a single file
```

The site is published by GitHub Actions (`.github/workflows/quarto_publish.yaml`) to `gh-pages`. This is **not** automatic: the workflow is `workflow_dispatch` only, so pushing to `main` does not deploy. Trigger a publish by hand — `gh workflow run quarto_publish.yaml`, or the "Render and Publish" workflow in the Actions UI.

## Environment Setup

This project uses **renv** for reproducible R environments:

```r
renv::restore()   # Install all pinned packages from renv.lock
```

Data is fetched live from **Redivis** using `REDIVIS_API_TOKEN` (required env var for data pages). Pages without Redivis calls render without it.

Some vignettes execute **Python**, not just R. `quarto render` of the whole site
will fail on `vignettes/irt_python.qmd` with `ModuleNotFoundError: No module
named 'mirt'` unless the Python that Quarto resolves has them installed:

```bash
pip install mirt girth scipy matplotlib pandas jupyter
pip install "git+https://github.com/itemresponsewarehouse/Python-pkg.git"
```

That is the same list `.github/workflows/quarto_publish.yaml` installs, so CI
always has it and a fresh maintainer machine usually does not. **A full local
render is therefore not a reliable pre-flight check for CI** — it can fail for a
reason CI does not have, and a local pass is no guarantee either. Rendering a
single page (`quarto render data.qmd`) is unaffected and is usually what you
want while iterating.

Note also that `_quarto.yml` declares a `post-render` step
(`landing/emit_landing_pages.R`, which generates the per-table pages under
`_site/tables/`). Post-render runs only after a **successful** render, so a
broken vignette silently means no landing pages are emitted.

## Architecture

### Content
- Root `.qmd` files are top-level pages (index, about, data, standards, etc.)
- `vignettes/` — standalone analysis tutorials; some have companion `*_compute.R` + `.Rout` files for pre-computed heavy outputs
- `components/` — reusable Quarto includes: `_hist.qmd` (Observable histogram), `_interval.qmd` (range slider), `_tol.qmd` (tolerance slider), `_style.qmd` (CSS setup for data explorer), `_source-links.qmd` (per-vignette "Source code for this page" links, backed by `components/source_links.R`, which is where the GitHub URL pattern lives)
- Partial files prefixed with `_` are included via `{{< include >}}` in other pages

### Data pipeline: R + OJS dual-layer
Data pages combine two languages:
- **R** — fetches metadata from Redivis (`_load-data.qmd`) and does preprocessing; results are passed to OJS via `ojs_define()`
- **OJS (Observable JavaScript)** — drives all interactive UI: the filterable dataset table, the Observable Plot charts, and the filter components

The data explorer is `data.qmd`. Its OJS lives inline in that file — the filter
inputs, the `Inputs.table` call, the plots, and the info/code-snippet callouts are
all chunks in `data.qmd` itself, not in separate partials. The only includes are
`_load-data-explore.qmd` (the R side: Redivis fetch, tag preprocessing,
`ojs_define()`) and the reusable widgets under `components/` (`_interval.qmd`,
`_hist.qmd`, `_tol.qmd`, `_style.qmd`). So changes to filter logic or table display
go in `data.qmd`; changes to what data reaches OJS go in `_load-data-explore.qmd`.

### Vignette compute pattern
Heavy statistical computations are offloaded to a companion `*_compute.R` script that writes `.rds` cache files (e.g., `vignettes/2pldata/2pl_across_datasets_results.rds`). The vignette `.qmd` then loads the cache with `readRDS(...)` instead of re-running the model. When adding a new computation-heavy vignette, follow this pattern: create `vignettes/<name>_compute.R`, run it locally to produce the cache, commit both the script and the `.rds`, then reference the cache in the `.qmd`.

### Execute defaults (`_quarto.yml`)
All code runs with `echo: false`, `message: false`, `warning: false`, `error: false` — the site is reader-facing, not a teaching document. Don't change these defaults for new pages. Individual vignettes may override locally (e.g., `echo: true` in tutorial-style pages like `imv.qmd`).

### Styling
- Base theme: Cosmo
- Custom SCSS: `resources/scss/main.scss` (main styles, data explorer layout) and `scss/global.scss` (callout margins)
- Body width: 1200px, no sidebar
- Custom CSS classes for interactive components: `.panel-input`, `.filters-container`, `.plot-container`, `.sparkbar`, `.pullout`

### Key R packages
- `redivis` — data access
- `mirt` — IRT modeling
- `imv` — InterModel Vigorish
- `plotly`, `ggplot2` — visualization
- `arrow` — Parquet support
- `lme4`, `BradleyTerry2`, `elo` — statistical modeling
- `lavaan` — SEM/CFA modeling

## TODOs

- [ ] Do more with `construct_name` (measure description) from the tags sheet — it is dropped by the `select(-construct_name)` in `_load-data-explore.qmd`'s `tag_vals` pipeline, so it never appears in filters or the table. Options: (1) add it to the `info` object behind the "Information on selected dataset" callout in `data.qmd`, (2) add a free-text search filter for it, (3) show it as a column in the dataset table

Vignette-branch status, merge history, and the survey-vignette harmonization scaffold/checklist are tracked in Claude's memory, not here — see `project_vignette_harmonization_2026_07_21.md` and `project_vignette_branches_status.md`.
