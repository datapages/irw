# Per-table landing pages

Emits one landing page per IRW table, with `schema.org/Dataset` JSON-LD and a
Croissant (MLCommons) description, so that individual tables are addressable and
discoverable by Google Dataset Search, Hugging Face, Kaggle and OpenML.

Tracking issue: [ben-domingue/irw#1706](https://github.com/ben-domingue/irw/issues/1706).
The measurements this design rests on are in that issue's 2026-09-03 scoping comment.

## Which tables get a page

`page_rules.R` decides, and both this emitter and `data.qmd` (which links to the
pages) source it, so the two cannot drift. The rules were settled by Ben on
2026-09-19 after a 25-table pilot:

- **Every live table gets a page, except one with no recorded licence.** A blank
  or `NA` `Derived_License` holds the page back until the licence is known. The
  held tables are listed in the build log; the queue is irw#2266.
- **Known-bad tables get a page with a banner.** `known_issues.tsv` lists tables
  with an open data defect. Their page names the issue and is kept out of search:
  `noindex`, no Dataset JSON-LD, no Croissant file, no sitemap entry. Delete the
  row in the PR that *releases* the fix.
- **Withdrawn tables keep their URL as a tombstone.** `withdrawn.tsv` lists them;
  the page says "Withdrawn" and the date, nothing more, and is `noindex`. Add the
  row in the same PR that withdraws a table, or its URL becomes a 404 on the next
  publish. The build log warns about any table on the live sitemap that is about
  to lose its page.

## The index at /tables/

`build_index()` emits one flat list of every table that has a page: **Table,
Responses, Licence, Redivis dataset**. A name filter and click-to-sort headings
are the only JavaScript on the site's static pages, both fixed literals so the
file stays byte-deterministic (rule 1). Sorting moves rows with `appendChild`,
which preserves each row's inline `display`, so a sort after a filter does not
resurrect the hidden rows; Responses sorts on a `data-n` attribute, because the
displayed `1,048,576` sorts lexically below `9,912`.

The licence is on the list, not only on the page, because it decides whether a
reader may use a table at all (Padma, 2026-09-21). Restrictive licences -- the
ones `licence_terms()` recognises, NC and ND -- carry the same amber the download
note uses. A table with no recorded licence gets no page at all, so the column is
never blank. Tables in `known_issues.tsv` carry a "known issue" flag linking the
issue.

**Faceted filtering stays on `data.qmd`.** The index deliberately does not grow
licence/size/tag facets: `Browse the IRW Data` is that surface, the index links
to it, and two browse surfaces would be two things to keep in agreement.

## The citation block

Each page carries the source paper's BibTeX -- the dictionary's `BibTex` column,
the same string `docs.qmd` hands out -- in a `<pre>` with a copy button, omitted
where the column is blank. It says nothing about citing the IRW itself: what
counts as a release is unsettled (irw#1870, irw#2317), and a citation form
invented here would have to be withdrawn from every page later.

## URLs

Pages live at `/tables/<slug>/`, served from `<slug>/index.html`, with the
Croissant file beside it at `<slug>/croissant.jsonld`. The slug is the table name
lowercased -- 308 of the corpus' names are not lowercase, and a case-sensitive
host would otherwise serve `Foo` and `foo` as two pages while a case-insensitive
one would collide them. The page displays the true name; only the path is folded,
and `.assert_no_slug_collisions()` fails the build if two names ever collapse.

Directory form rather than `<slug>.html` is deliberate: these URLs are meant to
be cited, and to be what a release DOI resolves to if irw#1870 lands. GitHub
Pages does not reliably serve an extensionless path for a `.html` file, and an
extension inside a citable identifier ages badly. Same file count either way.

## Croissant loading, and the 100MB limit

Each Croissant file's `contentUrl` is Redivis' `table.listRows` endpoint, which
serves a public table as CSV with no token (enabled by Redivis 2026-09-19; before
that it answered 401 and the files validated but read zero records). The URL is
pinned to the Redivis dataset version the page reports, and addresses the table
by name:

    https://redivis.com/api/v1/tables/datapages.<shard>:v7_0.<table>/rows?format=csv

Redivis refuses anonymous requests for tables over 100MB. The cutoff tracks the
table's `numBytes` property exactly (checked on 12 tables between 60MB and
160MB), so the emitter reads `numBytes` and, for a larger table, points
`contentUrl` at the Redivis page instead and says so in the file's description.
On 2026-09-19 that was 135 of 4,169 tables. Their files validate but do not load,
and their pages offer "Browse on Redivis" instead of a CSV download.

Croissant fields cover only `id`, `item` and `resp`. `irw_meta` stores other
column names lowercased while a table may not (`cov_Gender`), and a field naming a
column that does not exist makes the whole file fail to load. The CSV still has
every column.

The bar is records, not parsing: `mlc.Dataset(jsonld=f)` succeeds on a file
whose data URL is a web page. See the check below.

## How it runs

`_quarto.yml` declares it as a project `post-render` step, so pages regenerate
whenever the site is rendered. The site deploys by manual `workflow_dispatch`
(~2-3x/week), and that is the whole cadence -- there is no separate schedule.

Without `REDIVIS_API_TOKEN` the script prints a message and exits 0, so a local
preview without credentials still renders.

## Three rules

1. **Output is deterministic.** Identical inputs produce byte-identical files.
   No timestamps, no build ids, no unordered iteration. Every page is committed
   to `gh-pages`; if pages churn when their tables did not, a full-corpus
   emission would add ~34MB of git objects per publish. Verified by rendering
   twice and diffing.
2. **Versions are reported, never reconciled.** Each page states both the IRW
   version from `metadata/version_manifest.tsv` and the exact Redivis dataset
   version its facts came from. When those disagree the page shows both and the
   script warns; it never guesses.
3. **The manifest is read, not forked.** `metadata/version_manifest.tsv` in
   `ben-domingue/irw` is authoritative. When `renv.lock`'s `irw` pin is next
   bumped past `6ebce93a`, replace `.read_manifest()` with `irw::irw_version()`.

## Checks worth re-running after any change

```sh
# determinism -- must be byte-identical
Rscript landing/emit_landing_pages.R && cp -r _site/tables /tmp/run1 \
  && Rscript landing/emit_landing_pages.R && diff -r /tmp/run1 _site/tables

# Croissant -- must all parse
pip install mlcroissant
python -c "import mlcroissant as mlc, glob; [mlc.Dataset(jsonld=f) for f in glob.glob('_site/tables/*/croissant.jsonld')]"
# ...and records must actually load for tables under 100MB (spot-check a sample)
python -c "import mlcroissant as mlc, glob, itertools; print({f.split('/')[-2]: len(list(itertools.islice(mlc.Dataset(jsonld=f).records('responses'), 5))) for f in glob.glob('_site/tables/*/croissant.jsonld')})"
```
