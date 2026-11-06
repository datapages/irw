---
lang: en
pagetitle: "The Item Response Warehouse (IRW)"
# This page and the homepage render the same English prose -- both include
# welcome/_prose_en.md -- so without this they are two indexable URLs carrying
# identical text. The homepage is the one that should rank, so Quarto's own
# self-canonical is switched off here and replaced by one pointing at "/".
# The 24 translations keep their self-canonicals: each is the only page in its
# language and has nothing to be a duplicate of.
canonical-url: false
format:
  html:
    # A document-level include-in-header REPLACES the project one rather than
    # adding to it, so logos/_head.html has to be named again or this page
    # loses the site's head content entirely.
    include-in-header:
      - logos/_head.html
      - text: |
          <link rel="canonical" href="https://itemresponsewarehouse.org/">
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (R or Python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# The Item Response Warehouse (IRW)

**A free, open collection of harmonized item response data for psychometrics and measurement research.**

[Read the paper](https://doi.org/10.3758/s13428-025-02796-y) **(open access)**

{{< include /welcome/_lang_switch.md >}}

---

{{< include /welcome/_prose_en.md >}}
