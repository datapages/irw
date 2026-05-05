# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **Item Response Warehouse (IRW)** website — a Quarto-based static documentation site published at https://itemresponsewarehouse.org/. It is not an R package; it is a publication-focused website with embedded R data analysis.

## Build Commands

```bash
quarto render                         # Full site build → _site/
quarto preview                        # Dev server on port 4200
quarto render vignettes/cfa.qmd       # Render a single file
```

The site deploys automatically via GitHub Actions (`.github/workflows/quarto_publish.yaml`) on push to `main`, publishing to `gh-pages`.

## Environment Setup

This project uses **renv** for reproducible R environments:

```r
renv::restore()   # Install all pinned packages from renv.lock
```

Data is fetched live from **Redivis** using `REDIVIS_API_TOKEN` (required env var for data pages). Pages without Redivis calls render without it.

## Architecture

### Content
- Root `.qmd` files are top-level pages (index, about, data, standards, etc.)
- `vignettes/` — standalone analysis tutorials; some have companion `*_compute.R` + `.Rout` files for pre-computed heavy outputs
- `components/` — reusable Quarto includes: `_hist.qmd` (Observable histogram), `_interval.qmd` (range slider), `_tol.qmd` (tolerance slider), `_style.qmd` (CSS setup for data explorer)
- Partial files prefixed with `_` are included via `{{< include >}}` in other pages

### Data pipeline: R + OJS dual-layer
Data pages combine two languages:
- **R** — fetches metadata from Redivis (`_load-data.qmd`) and does preprocessing; results are passed to OJS via `ojs_define()`
- **OJS (Observable JavaScript)** — drives all interactive UI: `_tbl-*.qmd` (filterable tables with live Redivis API calls), `_viz-*.qmd` (Observable Plot charts), and inline filter components

When editing data-explorer pages, changes to filter logic or table display are almost always in the `_tbl-*.qmd` or `_viz-*.qmd` OJS partials, not in R code.

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
