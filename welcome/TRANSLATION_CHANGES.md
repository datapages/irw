# English changes awaiting translation

The English homepage prose changed in two rounds during October 2026. The 24
`welcome_<lang>.md` files have had **neither**. This file lists every change,
so the translation pass is a checklist rather than a re-read of a diff.

Run `python3 welcome/check_translations.py` for the current state. As of this
writing it reports all 24 behind on `code`, `columns` and `images`, and all 24
fine on `headings`, `links` and `switcher`.

The English now lives in `welcome/_prose_en.md` (shared by `welcome_en.md` and
the homepage). The translations are still whole files. See `README.md`.

**The translator note at the top of each file still governs.** Do not translate
column names in backticks, the contents of code blocks including their
comments, paper titles, URLs or DOIs, or the switcher include.

Change types: **(i)** wording · **(ii)** sentence added or removed ·
**(iii)** code, link or image · **(iv)** structural.

---

## 1 · "Why the IRW exists" — the corpus is thousands, not hundreds · (i)

A claim fix. The live page understates the corpus by an order of magnitude.

> **was** It brings together **hundreds** of existing datasets and reshapes them into one common format — so that a method tested on one dataset can easily be tested on **hundreds** of others.
>
> **now** It brings together **thousands** of existing datasets and reshapes them into one common format — so that a method tested on one dataset can easily be tested on **thousands** of others.

---

## 2 · "What is in the IRW" — thousands, and what a response is · (i)

Two changes in one sentence. The same count fix, and a definition that was
written around the core family and is false of the other four: competitions
have no item, a ranking response is a position over the whole set at once, and
a conjoint profile is rebuilt for every task.

> **was** The IRW contains **hundreds of datasets** ("tables"), each one a collection of individual responses. A response is generated whenever some person (or other unit) responds to some item (or other probe). Examples include:
>
> **now** The IRW contains **thousands of datasets** ("tables"), each one a collection of individual responses. A response is generated whenever some person (or other unit) meets a probe and the outcome is recorded. What counts as a probe differs by [data family](/overview.qmd): an item answered on its own, another entity met in a contest, a whole set of options put in order, or a profile assembled from randomly assigned attributes. Examples include:

The link target `/overview.qmd` is the same for every language; that page is
English only.

---

## 3 · "What is in the IRW" — three example bullets added · (ii)

The list named core settings only. Insert three bullets before the final
"Any other setting…" bullet, leaving the first three and the last as they are:

> - Games, matches and head-to-head comparisons, where two entities meet and one wins
> - Ranked-choice ballots, jury rankings and survey ranking questions
> - Conjoint experiments, where respondents choose between randomly built profiles

---

## 4 · "What is in the IRW" — the Interoperable bullet · (i)

Long format is shared across the families; the *structure* is not.

> **was** **Interoperable.** Each dataset is reshaped into the same simple long-format structure (described below), so the same analysis code can run across many datasets with little or no modification.
>
> **now** **Interoperable.** Each dataset is reshaped into long format, one row per response, following the standard for its family (described below), so the same analysis code can run across many datasets with little or no modification.

---

## 5 · "What is in the IRW" — response types, and a dropped sentence · (i) + (ii)

Two things. The response-type list named core types only. And the second
sentence repeated the **Findable** bullet three lines above it almost word for
word, so it goes.

> **was** Datasets vary widely in size (from a few hundred responses to many millions) and in response type (yes/no items, multi-category ratings, partial-credit scores, and more). ~~Each dataset also comes with pre-computed metadata — number of participants, number of items, response density, subject area, and other descriptive tags — so that researchers can find relevant datasets without first downloading and processing all of them.~~
>
> **now** Datasets vary widely in size (from a few hundred responses to many millions) and in response type (yes/no items, multi-category ratings, partial-credit scores, unordered categories, match outcomes, ranked positions, and more).

---

## 6 · "The data standard" — the whole section is replaced · (iv)

**The biggest change, and the one to do carefully.** Ben chose this from three
candidates. The old section described the core family only and said nothing
about the other four.

**Remove**: the cross-classification image, the three-row `id`/`item`/`resp`
table, the `id`/`item`/`resp`/`rt`/`cov_age`/`rater` worked example, the
paragraph about extra information in consistently named columns, and the
closing paragraph pointing at `standard.html`. The worked example is not lost;
it lives on `standard.html`.

**Replace with**, keeping the `## The data standard` heading:

> Every IRW table is in **long format**: one row per response. What a row *is* depends on the family the table belongs to, and so do the columns that carry it.
>
> `<img src="/figures/families/families-tree.svg" alt="…" class="standard-tree">`
>
> | Family | `source =` | Standard |
> |---|---|---|
> | Core | `"core"` | [Core](/standard.qmd) |
> | Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
> | Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
> | Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
> | Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
> | Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |
>
> Simsyn is not on the chart: it is defined by where the data come from, not by their shape, so a simulated table can have any of these shapes. The [overview of the standards](/overview.qmd) has the columns for each family.

Notes for the translator:

- **Translate** the family names in the first column, the paragraphs, and the
  `Standard` column's link text. **Do not translate** the `source =` values,
  the header `` `source =` ``, or the link targets.
- **The figure stays shared and stays English.** `families-tree.svg` has
  English labels. Translate the `alt` text only. Per-language SVGs are
  possible later — `figures/families/make_families_figure.py` takes its strings
  from one place — but that is a separate round.
- `/welcome/assets/diagram-cross-classification.svg` is **not deleted** from
  the repo. It simply stops being referenced once all 24 are converted.

---

## 7 · "How to use it" — a trimmed sentence · (ii)

The code block directly below it shows exactly this, item for item.

> **was** The `irw` package, available for both **R** and **Python**, gives simple functions for finding, filtering, and downloading data.
>
> **now** The `irw` package is available for both **R** and **Python**.

---

## 8 · "How to use it" — a `source =` line in the R block · (iii)

Append to the existing R block, after the `irw_fetch` line. **Not translated,
comment included:**

```r

# Other families are the same call with a source: comp, nom, conj, rank, sim
g <- irw_fetch("lichess_2013_blitz", source = "comp")
```

---

## 9 · "How to use it" — a `source =` line in the Python block · (iii)

Append to the existing Python block, after the `irw.fetch` line. **Not
translated, comment included:**

```python

# Other families are the same call with a source: comp, nom, conj, rank, sim
g = irw.fetch("lichess_2013_blitz", source="comp")
```

A note on these two, which does **not** need to reach the page: the `source`
argument differs between the packages for the core family and only for core —
R takes `"core"`, Python takes `"main"` and rejects `"core"`. The five
non-core values are identical in both, which is why these lines are correct as
written in either language's page.

---

## 10 · "How to use it" — two trimmed sentences · (ii)

The redesigned homepage's workflow cards carry both points above the prose.
**See the open question below before applying this one.**

> **was** The first time you use the package, you will be asked to log in with a free Redivis account. ~~After that, one line of code downloads any dataset directly into R or Python. From there, the data is ready for analysis with standard software — for example, item response theory or factor analysis packages.~~
>
> **now** The first time you use the package, you will be asked to log in with a free Redivis account.

---

## Open question for Ben: the trims assume sections the translated pages do not have

Changes **5** (second half) and **10** are not corrections. They are trims,
and they were justified by the redesigned homepage's new sections — the family
tiles and the six workflow cards, which sit above the prose and already say
what the trimmed sentences said.

**The 24 translated pages have none of those sections.** They are
`welcome_<lang>.md`, rendered on their own at `/welcome/welcome_fr.html` and
so on, with no hero, no tiles and no workflow cards. Applying these two trims
there would delete information and replace it with nothing, leaving
non-English readers with strictly less than they have today.

Two ways to go:

**(a) Apply only the corrections.** Changes 1, 2, 3, 4, 6, 7, 8, 9 and the
first half of 5. Skip the trims. Translated pages keep the fuller text. The
English and the translations diverge by two sentences on purpose, and
`check_translations.py` stays quiet about it because neither trim changes a
heading, a link, a code block or an image.

**(b) Give the translated pages the new sections too.** Then the trims apply
everywhere and the pages match. This is a much larger job: the tiles, workflow
cards and specimen card are English strings inside OJS lists in `preview.qmd`,
not prose, so it means extracting those strings per language, not translating
markdown.

I would do (a) now and treat (b) as its own round. But it is a judgement about
what non-English readers get, so it is Ben's.

---

## And when the text is settled

- One commit per language, keeping each file's existing structure.
- RTL files — `ar`, `fa`, `he`, `ur` — keep their direction markup, and check
  that the new table and the code blocks still render left-to-right where they
  do now.
- Re-run `python3 welcome/check_translations.py` until it reports no
  structural mismatches.
- Any translation produced by a model should be flagged as such, and the
  lower-resource languages — `bn`, `sw`, `ta`, `ur`, `ms`, `th` — are worth a
  native-speaker read before they go live.
