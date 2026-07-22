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

## TODOs

- [x] Re-enable P3M in CI once `imv 0.3` is synced to Posit Package Manager — switched `imv` in `renv.lock` from GitHub to CRAN source
- [x] Audit all vignettes and R scripts for manual IMV calculations and replace with `imv` R package calls (currently `vignettes/imv.qmd` already uses the package; check other vignettes for any ad-hoc implementations)
- [ ] Do more with `construct_name` (measure description) from the tags sheet — currently excluded from `tag_vals` in `_load-data-explore.qmd:96` so it never appears in filters or the table. Options: (1) add it to the info callout box in `index.qmd` (~line 378), (2) add a free-text search filter for it, (3) show it as a column in the dataset table
- [x] Apply card layout to the vignettes index page — same two-column grid style used on `research.qmd` (`.paper-grid`, `.paper-card`, etc.)
- [x] Add plotly hover annotations to other vignette figures — applied to `fig-dataset-medians` in `2pl_across_datasets.qmd` and `fig-independence-fit` in `dutch_identity.qmd`
- [x] Group the vignettes index into genres instead of one undifferentiated grid — done: Tutorials / Assumption audits across IRW / Replicating published findings / Cross-dataset analyses. Expanded card descriptions and added a key-paper cite/link where a specific paper anchors the vignette.
- [ ] Harmonize style across "empirical survey" vignettes (2pl_across_datasets, item_text_difficulty, speededness, gender_dif, dimensionality, dutch_identity, local_dependence, response_style, hf_reliability_paradox, rt_imv, il_hte, asymmetric_irt). Status as of 2026-07-21: dimensionality, local_dependence, gender_dif, response_style now merged to `main`; speededness and asymmetric_irt are feature-complete on their own branches but not yet merged — harmonize those two as part of merging rather than twice. Concrete sub-items from the 2026-07-20 review (re-verified 2026-07-21):
  - [ ] Pick one scaffold and apply it site-wide: newer vignettes (dimensionality, local_dependence, response_style) use `## Motivation` → `## Data and methods` → `## Results` → `## Limitations` → `## Reproducibility` → `## References`; older ones (2pl_across_datasets, item_text_difficulty, gender_dif) use numbered `## Step 1/2/3` + `## What to notice`, skip `## References` even when methods cite literature (e.g. gender_dif's MH/ETS Δ has no References section at all). `hf_reliability_paradox`/`rt_imv` are close but use `## Discussion` instead of a dedicated `## Limitations` heading.
  - [ ] Add a synthesis/limitations section to `il_hte.qmd` — jumps straight from "The identification problem" to "Reproducibility", the only survey vignette on `main` missing one.
  - [ ] Standardize the color palette as named vars (`irw_blue`, `irw_red`, `irw_grey`) in `2pl_across_datasets.qmd`, `gender_dif.qmd`, `item_text_difficulty.qmd` (and `speededness.qmd` at merge time) — they currently hardcode hex literals inline instead of matching the other 9 vignettes.
  - [ ] Finish rolling out `ggplotly()` interactivity to `item_text_difficulty.qmd`, `hf_reliability_paradox.qmd`, `rt_imv.qmd`, `il_hte.qmd` (and `speededness.qmd`/`asymmetric_irt.qmd` at merge time) — currently only dutch_identity, dimensionality, gender_dif, response_style, 2pl_across_datasets, local_dependence have it.
  - [ ] Fix bug: `2pl_across_datasets.qmd`'s Reproducibility snippet reads `source("2pl_across_datasets_compute.R")`, missing the `vignettes/` prefix every other vignette uses for a project-root-relative path.
