# Per-table landing pages (pilot)

Emits one landing page per IRW table, with `schema.org/Dataset` JSON-LD and a
Croissant (MLCommons) description, so that individual tables are addressable and
discoverable by Google Dataset Search, Hugging Face, Kaggle and OpenML.

Tracking issue: [ben-domingue/irw#1706](https://github.com/ben-domingue/irw/issues/1706).
The measurements this design rests on are in that issue's 2026-09-03 scoping comment.

## Status: pilot, 25 tables

`pilot_tables.txt` lists them. They were chosen to span what could break the
generator rather than to look good: all six warehouse shards, tables with and
without item text, tagged and untagged, non-lowercase names, the longest name in
the corpus, the largest and smallest tables, and one table with no `biblio` row
at all. It deliberately excludes all 182 tables named in irw#1842 / irw#1779 --
a superset of the 153 that irw#1856 is repairing -- so the pilot does not
publish data already known to be wrong.

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

## Known limitation: Croissant validates but does not load

The Croissant `contentUrl` points at the Redivis *table page*, not at a data
file. Verified 2026-09-03: the Redivis API returns `401 "No credentials were
provided"` even for a public table, so there is no unauthenticated URL a loader
could read. `mlcroissant` therefore parses all 25 files successfully and would
then read zero records.

So sub-action 4.3 is **partly** delivered: the descriptions are valid and a
NeurIPS submission can point at one, but programmatic loading via `mlcroissant`
or TFDS needs a direct download URL that does not yet exist. Do not describe
Hugging Face / Kaggle / OpenML support as delivered.

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

# Croissant -- must be 25/25
pip install mlcroissant
python -c "import mlcroissant as mlc, glob; [mlc.Dataset(jsonld=f) for f in glob.glob('_site/tables/*.jsonld')]"
```
