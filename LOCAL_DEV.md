# Running the site locally

This is a [Quarto](https://quarto.org) website. Pages are `.qmd` files (Markdown
plus R, Python and Observable JS chunks), and `quarto render` builds them to
static HTML in `_site/`. The live site at itemresponsewarehouse.org is rendered
by GitHub Actions (`.github/workflows/quarto_publish.yaml`), not from anyone's
laptop.

Adapted from Ayaan G.'s onboarding notes for an earlier fork of this repo.

## What you need

1. **Quarto**: https://quarto.org/docs/get-started/ (on Windows,
   `winget install Quarto.Quarto` also works).
2. **R 4.6.1**, the version CI uses and `renv.lock` declares:
   https://cran.r-project.org/. R packages are managed by **renv**, which plays
   the role `node_modules` plays in a JavaScript project.
3. **Rtools** (Windows only), needed to compile packages with C/C++ code:
   https://cran.r-project.org/bin/windows/Rtools/. Pick the version that
   matches your R and tick "Add rtools to the system PATH" in the installer.
   On Linux, CI installs these system libraries first:
   `r-base-dev libcurl4-openssl-dev libudunits2-dev libgdal-dev libglpk-dev libgsl-dev cmake libx11-dev`.
4. **Python 3.11**, for a few vignettes:
   `pip install irw mirt girth scipy matplotlib pandas jupyter`.

## First time: restore the R packages

From the repository root:

```sh
Rscript -e "renv::restore()"
```

This installs the packages pinned in `renv.lock`, including `redivis` and `irw`
itself. If it fails with `'make' not found` on Windows, Rtools is missing or not
on your PATH: install it, open a new terminal, and run the command again.

## Preview

```sh
quarto preview            # the whole site
quarto preview data.qmd   # one page, which is much faster
```

The server runs at http://localhost:4200 (`preview.port` in `_quarto.yml`).
A full render takes about 14 minutes in CI, so preview the page you are working
on instead of the whole site.

## Redivis credentials

A few pages fetch metadata from Redivis while they render: the data explorer
(`data.qmd`), `docs.qmd`, `itemtext_desc.qmd`, `tags_quality.qmd`, some
vignettes, and the per-table landing pages. To render them you need
a Redivis API token with **read** scope only (nothing in this repo writes to
Redivis):

```sh
export REDIVIS_API_TOKEN=...        # PowerShell: $env:REDIVIS_API_TOKEN = "..."
```

Without a token the other pages still build, and the per-table landing pages
(`landing/emit_landing_pages.R`) skip themselves. That skip applies only
locally: in CI a missing token fails the build on purpose, because publishing
without the landing pages would delete them from the live site.

## Two things that catch people out

- **A local render that passes proves little about CI.** CI installs exactly
  what `renv.lock` pins. If you install a newer package into your own library
  (for example a development build of `irw` to use a new function), your
  preview works and the CI render fails. When a page needs a newer `irw`, bump
  its `Version` and `RemoteSha` in `renv.lock` in the same pull request.
- **Commit before you switch branches.** Switching branches in the middle of a
  render gives spurious failures, and uncommitted changes can be lost.

## Where things live

`ARCHITECTURE.md` in [ben-domingue/irw](https://github.com/ben-domingue/irw/blob/main/ARCHITECTURE.md)
maps which repository owns what and which document wins when two disagree.
`CLAUDE.md` in this repo describes the site's internals (the R-to-OJS data flow,
the vignette cache pattern) in more detail. It is written for coding agents, but
it is accurate for people too.
