---
lang: nl
pagetitle: "Item Response Warehouse (IRW; Itemresponsmagazijn)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Itemresponsmagazijn)

**Een gratis, open verzameling van geharmoniseerde itemresponsdata voor psychometrisch en meetkundig onderzoek.**

[Lees het artikel](https://doi.org/10.3758/s13428-025-02796-y) **(open access)**

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

## Waarom het IRW bestaat

Onderzoekers die meting bestuderen — in de onderwijskunde, psychologie en verwante vakgebieden — hebben echte data nodig om hun methoden te testen en te vergelijken. Die data bestaat al in grote hoeveelheden. Maar ze is verspreid over veel studies, opgeslagen in veel verschillende formaten, en vaak lastig te hergebruiken door onduidelijke documentatie of licenties.

Dit is een bekend probleem. Andere vakgebieden hebben het opgelost door gedeelde, gestandaardiseerde databronnen te bouwen. In de informatica gaf de ImageNet-verzameling van gelabelde afbeeldingen onderzoekers een gemeenschappelijke benchmark en hielp dit de snelle vooruitgang in AI aan te jagen. Genetica en neurowetenschappen hebben vergelijkbare gedeelde bronnen gebouwd voor hun eigen data.

Het Item Response Warehouse (IRW) doet hetzelfde voor itemresponsdata. Het brengt honderden bestaande datasets samen en vormt ze om naar één gemeenschappelijk formaat — zodat een methode die op één dataset is getest, gemakkelijk op honderden andere kan worden getest.

## Wat zit er in het IRW

Het IRW bevat **honderden datasets** ("tabellen"), elk een verzameling van individuele responsen. Een respons ontstaat wanneer een persoon (of een andere eenheid) reageert op een item (of een andere meetopgave). Voorbeelden zijn onder meer:

- Antwoorden van leerlingen op onderwijs- en vaardigheidstoetsen
- Vragenlijstitems die persoonlijkheid of attitudes meten
- Beoordelingen gegeven door menselijke beoordelaars
- Elke andere situatie met herhaalde responsen op een reeks meetinstrumenten

Twee dingen gelden voor elke dataset in het IRW:

- **Open.** Elke dataset is gelicentieerd voor hergebruik. De herkomst ervan is gedocumenteerd, en de code die is gebruikt om de dataset naar het IRW-formaat om te zetten, is openbaar.
- **Geharmoniseerd.** Elke dataset wordt omgevormd naar dezelfde eenvoudige structuur (hieronder beschreven), zodat dezelfde analysecode met weinig of geen aanpassing op veel datasets kan worden toegepast.

Datasets verschillen sterk in omvang (van enkele honderden responsen tot vele miljoenen) en in responstype (ja/nee-items, meerkeuzebeoordelingen, deelscores, en meer). Elke dataset wordt ook geleverd met vooraf berekende metadata — aantal deelnemers, aantal items, responsdichtheid, vakgebied en andere beschrijvende labels — zodat onderzoekers relevante datasets kunnen vinden zonder ze eerst allemaal te downloaden en te verwerken.

## De datastandaard

<img src="/welcome/assets/diagram-cross-classification.svg" alt="Rasterdiagram dat laat zien dat elke respons zich op het snijpunt van een id en een item bevindt." class="welcome-figure">


Elke IRW-dataset wordt omgevormd naar **lang formaat (long format)**: één rij per respons. Elke rij bevat minimaal drie stukjes informatie:

| Kolom | Betekenis |
|---|---|
| `id` | Wie (of wat) de respons heeft geproduceerd — meestal een persoon |
| `item` | Welk meetinstrument de respons heeft geproduceerd — meestal een vraag of taak |
| `resp` | De respons zelf, opgeslagen als een ordinale score |

**Voorbeeld:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Wanneer een dataset extra informatie bevat — responstijd, identiteit van de beoordelaar, covariaten zoals leeftijd — wordt die informatie opgeslagen in extra, consistent benoemde kolommen. Deze ene eenvoudige structuur dekt een enorm scala aan meetsituaties, en dat maakt het mogelijk om analysecode één keer te schrijven en toe te passen op het hele magazijn.

De volledige technische specificatie van de standaard is beschikbaar op [itemresponsewarehouse.org/standard.html](/standard.qmd). Er bestaan ook verwante, meer gespecialiseerde standaarden voor itemtekst, paarsgewijze-vergelijkingsdata en nominale (ongeordende categorie) responsen.

## Hoe te gebruiken

Er zijn drie manieren om IRW-data te verkrijgen, afhankelijk van hoeveel u wilt automatiseren.

**1. Bekijk in de webbrowser**
Verken datasets en hun metadata rechtstreeks in de [IRW-databrowser](/data.qmd) — geen account nodig. Voor het downloaden van een volledige dataset is een gratis [Redivis](https://redivis.com)-account nodig, aangezien dat het platform is dat de onderliggende data host.

**2. Gebruik het `irw`-pakket (aanbevolen)**
Het `irw`-pakket, beschikbaar voor zowel **R** als **Python**, biedt eenvoudige functies om data te vinden, filteren en downloaden.

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

De eerste keer dat u het pakket gebruikt, wordt u gevraagd in te loggen met een gratis Redivis-account. Daarna downloadt één regel code elke dataset rechtstreeks naar R of Python. Vanaf dat moment is de data klaar voor analyse met standaardsoftware — bijvoorbeeld pakketten voor item response theory of factoranalyse.

**3. Gebruik de Redivis-clientbibliotheken rechtstreeks**
Voor lager-niveau workflows of workflows buiten R/Python is de data ook toegankelijk via Redivis' eigen R- en Python-clientbibliotheken. Zie de [Aan de slag-gids](/getstarted.qmd) voor details.

### Meer dan alleen data downloaden

Het IRW-project omvat ook:

- Een groeiende reeks **[vignetten](/vignettes/index.qmd)** — uitgewerkte voorbeelden die klassieke en nieuwe meetmethoden tegelijk toepassen op veel IRW-datasets
- **Onderwijsmateriaal en opgavensets** voor het onderwijzen van psychometrie met echte data
- Een **bijdrageproces** voor onderzoekers die hun eigen datasets aan het magazijn willen toevoegen

## Meer informatie

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Website: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Als u IRW-data in uw werk gebruikt, citeer dan alstublieft de oorspronkelijke data (wij hebben hiervoor de nodige functionaliteit geleverd). Het zou ook zeer op prijs worden gesteld als u het bovenstaande introductieartikel citeert.

---

*Vragen, feedback, of wilt u een dataset bijdragen? Bezoek de [contactpagina](/contact.qmd) of open een issue op [GitHub](https://github.com/itemresponsewarehouse).*
