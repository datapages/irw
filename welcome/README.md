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
`_TRANSLATION_CHANGES.md`.

All 25 files share one structure — five `##` sections and the same
language-switcher block — and the switcher is identical verbatim in every one.
Do not hand-edit it per file: it computes the current language at runtime from
the page URL.

The translator note at the top of `welcome_en.md` lists what must never be
translated: column names in backticks, the contents of code blocks including
their comments, paper titles, URLs and DOIs, and the switcher block.

## The language switcher is shared too

`_lang_switch.md` holds the switcher widget — the trigger button, the
`<noscript>` plain-link fallback and the script that computes the current
language from the page URL. All 25 `welcome_<lang>.md` include it:

```
{{< include /welcome/_lang_switch.md >}}
```

It used to be 137 lines copied verbatim into each of the 25 files. They were
byte-identical, and the translator note said never to hand-edit one — but
nothing enforced that. Now one file does. Edit it once and every welcome page
changes.

The widget contains no translatable text; the language names in the fallback
links are each written in their own language and are the same in every file.

## Why the notes files start with `_`

Quarto's `render:` list in `_quarto.yml` includes `"*.md"`, and that glob
reaches into subdirectories. `TRANSLATION_CHANGES.md` was therefore being
built into `_site/welcome/TRANSLATION_CHANGES.html` and would have been
published and indexed — a maintainer's working note on the public site.

The leading underscore is what keeps it out; Quarto skips those files, and no
`_quarto.yml` change is needed. `README.md` is safe without one because Quarto
ignores README files by default. Any new note in this directory should get the
underscore.
