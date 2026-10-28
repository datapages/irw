# `welcome/`

The homepage prose and its translations.

## The English is split; the translations are not

`_prose_en.md` holds the shared English prose, from `## Why the IRW exists` to
the end. Two pages include it verbatim:

| file | what it adds |
|---|---|
| `welcome_en.md` | the title, tagline, "Read the paper" line and the language switcher, above the include |
| `index.qmd` | the homepage |

**Edit the prose in `_prose_en.md`.** It was split out so that a second page
could carry the same text without a copy of it drifting, which is exactly what
had happened before: `preview/_prose_en.md` was a copy and had diverged by nine
edits.

The include is written project-relative, `{{< include /welcome/_prose_en.md >}}`,
not `_prose_en.md`. A nested include resolves against the **outermost** file, so
the bare path works from `welcome_en.md` alone and breaks the moment another
page at the project root includes `welcome_en.md`.

The leading underscore keeps Quarto from rendering `_prose_en.md` as a page of
its own.

## The 24 translations

`welcome_<lang>.md` are whole files, each carrying its own translated copy of
the same prose. They are **not** split, and an edit to `_prose_en.md` does not
reach them. Carrying English changes across is a deliberate pass; see
`TRANSLATION_CHANGES.md` when it exists.

All 25 files share one structure — five `##` sections and the same
language-switcher block — and the switcher is identical verbatim in every one.
Do not hand-edit it per file: it computes the current language at runtime from
the page URL.

The translator note at the top of `welcome_en.md` lists what must never be
translated: column names in backticks, the contents of code blocks including
their comments, paper titles, URLs and DOIs, and the switcher block.
