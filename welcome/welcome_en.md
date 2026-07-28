---
lang: en
pagetitle: "The Item Response Warehouse (IRW)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# The Item Response Warehouse (IRW)

**A free, open collection of harmonized item response data for psychometrics and measurement research.**

[Read the paper](https://doi.org/10.3758/s13428-025-02796-y) **(open access)**

<div class="lang-switch"><button type="button" class="lang-switch-trigger" aria-haspopup="true" aria-expanded="false" aria-controls="lang-switch-panel" aria-label="Choose a language"><i class="bi bi-globe2" aria-hidden="true"></i><i class="bi bi-chevron-down" aria-hidden="true"></i></button><div class="lang-switch-panel" id="lang-switch-panel" hidden><label class="visually-hidden" for="lang-switch-search">Search languages</label><div class="lang-switch-search-wrap"><i class="bi bi-search" aria-hidden="true"></i><input type="text" id="lang-switch-search" class="lang-switch-search" autocomplete="off" placeholder="Search languages"></div><ul class="lang-switch-list" role="listbox"></ul></div></div>
<noscript>

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html) · [繁體中文](/welcome/welcome_zh-hant.html) · [বাংলা](/welcome/welcome_bn.html) · [Türkçe](/welcome/welcome_tr.html) · [Tiếng Việt](/welcome/welcome_vi.html) · [Italiano](/welcome/welcome_it.html) · [Nederlands](/welcome/welcome_nl.html) · [Polski](/welcome/welcome_pl.html) · [ไทย](/welcome/welcome_th.html) · [اردو](/welcome/welcome_ur.html) · [தமிழ்](/welcome/welcome_ta.html) · [فارسی](/welcome/welcome_fa.html) · [Kiswahili](/welcome/welcome_sw.html) · [עברית](/welcome/welcome_he.html) · [Bahasa Melayu](/welcome/welcome_ms.html)

</noscript>
<script>
(function () {
  var LANGS = [
    { native: "English",      en: "English",                lang: "en" },
    { native: "Français",     en: "French",                 lang: "fr" },
    { native: "Español",      en: "Spanish",                lang: "es" },
    { native: "中文",          en: "Chinese Simplified",     lang: "zh-hans" },
    { native: "한국어",        en: "Korean",                 lang: "ko" },
    { native: "العربية",      en: "Arabic",                 lang: "ar" },
    { native: "日本語",        en: "Japanese",               lang: "ja" },
    { native: "Português",    en: "Portuguese",             lang: "pt" },
    { native: "Deutsch",      en: "German",                 lang: "de" },
    { native: "हिन्दी",        en: "Hindi",                  lang: "hi" },
    { native: "Русский",      en: "Russian",                lang: "ru" },
    { native: "繁體中文",       en: "Chinese Traditional",    lang: "zh-hant" },
    { native: "বাংলা",        en: "Bengali",                lang: "bn" },
    { native: "Türkçe",       en: "Turkish",                lang: "tr" },
    { native: "Tiếng Việt",   en: "Vietnamese",             lang: "vi" },
    { native: "Italiano",     en: "Italian",                lang: "it" },
    { native: "Nederlands",   en: "Dutch",                  lang: "nl" },
    { native: "Polski",       en: "Polish",                 lang: "pl" },
    { native: "ไทย",          en: "Thai",                   lang: "th" },
    { native: "اردو",         en: "Urdu",                   lang: "ur" },
    { native: "தமிழ்",        en: "Tamil",                  lang: "ta" },
    { native: "فارسی",        en: "Persian",                lang: "fa" },
    { native: "Kiswahili",    en: "Swahili",                lang: "sw" },
    { native: "עברית",        en: "Hebrew",                 lang: "he" },
    { native: "Bahasa Melayu",en: "Malay",                  lang: "ms" }
  ];

  document.querySelectorAll(".lang-switch").forEach(function (root) {
    var trigger = root.querySelector(".lang-switch-trigger");
    var panel = root.querySelector(".lang-switch-panel");
    var search = root.querySelector(".lang-switch-search");
    var list = root.querySelector(".lang-switch-list");

    // Hrefs are read from the <noscript> fallback rather than hardcoded,
    // because Quarto resolves that markdown's links to correct relative
    // paths per page at build time; a literal string in this script would
    // stay absolute and break under file:// or non-root deployments.
    var hrefByNative = {};
    var noscriptEl = root.nextElementSibling;
    if (noscriptEl && noscriptEl.tagName === "NOSCRIPT") {
      var tmp = document.createElement("div");
      tmp.innerHTML = noscriptEl.textContent;
      Array.prototype.forEach.call(tmp.querySelectorAll("a"), function (a) {
        hrefByNative[a.textContent.trim()] = a.href;
      });
    }

    // Current language is read from <html lang="...">, which Quarto sets
    // from each page's own frontmatter, rather than guessed from the URL.
    var currentLang = (document.documentElement.lang || "").toLowerCase();

    function render(filterText) {
      list.innerHTML = "";
      var q = (filterText || "").trim().toLowerCase();
      var matches = LANGS.filter(function (entry) {
        return !q || entry.native.toLowerCase().indexOf(q) !== -1 || entry.en.toLowerCase().indexOf(q) !== -1;
      });

      if (matches.length === 0) {
        var empty = document.createElement("li");
        empty.className = "lang-switch-empty";
        empty.textContent = "No matches";
        list.appendChild(empty);
        return;
      }

      matches.forEach(function (entry) {
        var li = document.createElement("li");
        li.setAttribute("role", "option");
        if (entry.lang === currentLang) {
          li.className = "lang-switch-current";
          li.setAttribute("aria-current", "true");
          var check = document.createElement("i");
          check.className = "bi bi-check2";
          check.setAttribute("aria-hidden", "true");
          li.appendChild(check);
          li.appendChild(document.createTextNode(" " + entry.native));
        } else {
          var a = document.createElement("a");
          a.href = hrefByNative[entry.native] || "#";
          a.textContent = entry.native;
          li.appendChild(a);
        }
        list.appendChild(li);
      });
    }

    function openPanel() {
      panel.hidden = false;
      trigger.setAttribute("aria-expanded", "true");
      search.value = "";
      render("");
      search.focus();
    }

    function closePanel() {
      panel.hidden = true;
      trigger.setAttribute("aria-expanded", "false");
    }

    trigger.addEventListener("click", function (e) {
      e.stopPropagation();
      if (panel.hidden) { openPanel(); } else { closePanel(); }
    });

    search.addEventListener("input", function () {
      render(search.value);
    });

    panel.addEventListener("click", function (e) {
      e.stopPropagation();
    });

    document.addEventListener("click", function () {
      if (!panel.hidden) closePanel();
    });

    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape" && !panel.hidden) {
        closePanel();
        trigger.focus();
      }
    });

    render("");
  });
})();
</script>

---

## Why the IRW exists

Researchers who study measurement — in education, psychology, and related fields — need real data to test and compare their methods. That data already exists in large quantities. But it is scattered across many studies, stored in many different formats, and often hard to reuse because of unclear documentation or licensing.

This is a well-known problem. The research community has articulated a shared standard for solving it: data should be **FAIR** — Findable, Accessible, Interoperable, and Reusable (Wilkinson et al., 2016). Other fields put these principles into practice by building shared, standardized data resources. In computer science, the ImageNet collection of labeled images gave researchers a common benchmark and helped drive rapid progress in AI. Genetics and neuroscience built similar shared resources for their own data.

The Item Response Warehouse (IRW) applies the same FAIR principles to item response data. It brings together hundreds of existing datasets and reshapes them into one common format — so that a method tested on one dataset can easily be tested on hundreds of others.

## What is in the IRW

The IRW contains **hundreds of datasets** ("tables"), each one a collection of individual responses. A response is generated whenever some person (or other unit) responds to some item (or other probe). Examples include:

- Student answers on education and ability tests
- Survey items measuring personality or attitudes
- Ratings assigned by human raters
- Any other setting involving repeated responses to a set of measurement probes

Every dataset in the IRW is designed to be:

- **Findable.** Each dataset comes with pre-computed metadata — participant and item counts, response density, subject area, and other descriptive tags — so datasets can be located and filtered without downloading them first.
- **Accessible.** Every dataset can be retrieved through the web browser or the `irw` package, using a free account.
- **Interoperable.** Each dataset is reshaped into the same simple long-format structure (described below), so the same analysis code can run across many datasets with little or no modification.
- **Reusable.** Each dataset is openly licensed, its origin is documented, and the code used to convert it into the IRW format is public.

Datasets vary widely in size (from a few hundred responses to many millions) and in response type (yes/no items, multi-category ratings, partial-credit scores, and more). Each dataset also comes with pre-computed metadata — number of participants, number of items, response density, subject area, and other descriptive tags — so that researchers can find relevant datasets without first downloading and processing all of them.

## The data standard

<img src="/welcome/assets/diagram-cross-classification.svg" alt="Diagram showing a colored grid of id-by-item cells reshaped, via an arrow, into a long-format table of id, item, and resp columns, where each resp cell's color matches its source cell in the grid." class="welcome-figure">


Every IRW dataset is reshaped into **long format**: one row per response. At minimum, each row has three pieces of information:

| Column | Meaning |
|---|---|
| `id` | Who (or what) produced the response — typically a person |
| `item` | Which measurement probe produced the response — typically a question or task |
| `resp` | The response itself, stored as an ordinal score |

**Example:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

When a dataset includes extra information — response time, rater identity, covariates such as age — that information is stored in additional, consistently named columns. This one simple structure covers a huge range of measurement situations, which is what makes it possible to write analysis code once and apply it across the entire warehouse.

The full technical specification of the standard is available at [itemresponsewarehouse.org/standard.html](/standard.qmd). Related, more specialized standards also exist for item text, pairwise-competition data, and nominal (unordered category) responses.

## How to use it

There are three ways to get IRW data, depending on how much you want to automate.

**1. Browse in the web browser**
Explore datasets and their metadata directly on the [IRW data browser](/data.qmd) — no account needed. Downloading a full dataset requires a free [Redivis](https://redivis.com) account, since that is the platform that hosts the underlying data.

**2. Use the `irw` package (recommended)**
The `irw` package, available for both **R** and **Python**, gives simple functions for finding, filtering, and downloading data.

```r
# R
devtools::install_github("itemresponsewarehouse/Rpkg")
library(irw)

irw_info()                     # overview of the IRW
irw_list_tables()              # list all available tables
irw_filter(var = "rt")         # find tables that include response times
df <- irw_fetch("4thgrade_math_sirt")   # download one table
```

```python
# Python
# pip install "git+https://github.com/itemresponsewarehouse/Python-pkg.git"
import irw

irw.info()
irw.list_tables()
irw.filter(var="rt")
df = irw.fetch("4thgrade_math_sirt")
```

The first time you use the package, you will be asked to log in with a free Redivis account. After that, one line of code downloads any dataset directly into R or Python. From there, the data is ready for analysis with standard software — for example, item response theory or factor analysis packages.

**3. Use the Redivis client libraries directly**
For lower-level or non-R/Python workflows, the data can also be accessed through Redivis's own R and Python client libraries. See the [Getting Started guide](/getstarted.qmd) for details.

### Beyond downloading data

The IRW project also includes:

- A growing set of **[vignettes](/vignettes/index.qmd)** — worked examples applying classic and new measurement methods across many IRW datasets at once
- **Training resources and problem sets** for teaching psychometrics with real data
- A **contribution process** for researchers who want to add their own datasets to the warehouse

## Learn more

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Website: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

If you use IRW data in your work, please cite the original data (we have provided functionality for doing so). It would also be great if you cared to cite the introductory paper above. 

---

*Questions, feedback, or want to contribute a dataset? Visit the [Contact page](/contact.qmd) or open an issue on [GitHub](https://github.com/itemresponsewarehouse).*
