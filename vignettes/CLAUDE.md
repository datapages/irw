# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is the `vignettes/` subdirectory of the Item Response Warehouse (IRW) website — a Quarto-based academic site at `itemresponsewarehouse.org`. Vignettes are educational + reproducible demonstrations of IRW data and psychometric analysis workflows.

## Commands

```bash
# Preview the full site (from irw_site/ parent directory)
quarto preview                          # serves on localhost:4200

# Render the full site
quarto render

# Render a single vignette
quarto render vignettes/imv.qmd
quarto render vignettes/cfa.qmd

# Recompute the 2PL precomputed cache (slow, parallelized)
Rscript 2pl_across_datasets_compute.R  # run from vignettes/

# Restore R package environment
# In R console:
renv::restore()
```

Deployment is automatic via GitHub Actions on push to `main` — it renders the site and publishes to gh-pages.

## Architecture

Vignettes are `.qmd` files (Quarto markdown with embedded R code blocks). The site config lives in the parent directory (`../_quarto.yml`). R package dependencies are managed via `renv` (lock file at `../renv.lock`).

**Core pattern in all vignettes:**
1. Fetch data: `irw::irw_fetch("dataset_name")` (requires `REDIVIS_API_TOKEN` env var)
2. Reshape: `irw::irw_long2resp(df)` converts long format to wide response matrix
3. Analyze: Use `mirt`, `lavaan`, etc.
4. Visualize: `ggplot2`

**Precomputed caching pattern** — used when computation is expensive (e.g., `2pl_across_datasets`):
- A separate `_compute.R` script runs analysis using `furrr::future_map()` (4 parallel workers), writes per-dataset `.rds` files to `2pldata/fits/`, then combines into `2pldata/2pl_across_datasets_results.rds`
- The `.qmd` loads only the cached `.rds` at render time — it does not recompute
- To update results: run the compute script, commit the new `.rds`, then re-render

**Global Quarto settings** (from `../_quarto.yml`): `echo: false`, `message: false`, `warning: false`, `error: false` — code is hidden by default but togglable via `code-fold: true`.

## Key R Packages

- `irw` — IRW data package (fetch, filter, simulate): `irw_fetch()`, `irw_filter()`, `irw_long2resp()`, `irw_simu_diff()`
- `mirt` — IRT model fitting (1PL, 2PL); uses lognormal prior `lnorm, 0.0, 1.0` on discrimination
- `lavaan` — CFA/SEM with MLR estimator
- `imv` — InterModel Vigorish (cross-validated model comparison)
- `furrr` / `future` — parallel computation in compute scripts
