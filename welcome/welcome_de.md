---
lang: de
pagetitle: "Das Item Response Warehouse (IRW; Lagerhaus für Item-Antworten)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Das Item Response Warehouse (IRW; Lagerhaus für Item-Antworten)

**Eine freie, offene Sammlung harmonisierter Item-Response-Daten für die psychometrische und messtheoretische Forschung.**

[Artikel lesen](https://doi.org/10.3758/s13428-025-02796-y) **(Open Access)**

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

## Warum es das IRW gibt

Forschende, die sich mit Messung beschäftigen — in der Bildungsforschung, Psychologie und verwandten Bereichen — benötigen echte Daten, um ihre Methoden zu testen und zu vergleichen. Solche Daten existieren bereits in großer Menge. Sie sind jedoch über viele Studien verstreut, in vielen unterschiedlichen Formaten gespeichert und aufgrund unklarer Dokumentation oder Lizenzierung oft schwer wiederzuverwenden.

Dies ist ein bekanntes Problem. Andere Fachbereiche haben es gelöst, indem sie gemeinsame, standardisierte Datenressourcen aufgebaut haben. In der Informatik bot die Sammlung beschrifteter Bilder ImageNet Forschenden einen gemeinsamen Bezugspunkt und trug zu raschen Fortschritten in der KI bei. Auch die Genetik und die Neurowissenschaften haben ähnliche gemeinsame Ressourcen für ihre eigenen Daten aufgebaut.

Das Item Response Warehouse (IRW) tut dasselbe für Item-Response-Daten. Es bringt Hunderte bestehender Datensätze zusammen, formt sie in ein gemeinsames Format um und macht sie frei zugänglich an einem einzigen Ort — sodass eine Methode, die an einem Datensatz getestet wurde, leicht an Hunderten anderer getestet werden kann.

## Was das IRW enthält

Das IRW enthält **Hunderte von Datensätzen** ("Tabellen"), von denen jeder eine Sammlung individueller Antworten ist. Eine Antwort entsteht immer dann, wenn eine Person (oder eine andere Einheit) auf ein Item (oder eine andere Messsonde) reagiert. Beispiele sind:

- Antworten von Schülerinnen und Schülern in Bildungs- und Leistungstests
- Fragebogenitems zur Messung von Persönlichkeit oder Einstellungen
- Bewertungen, die von menschlichen Beurteilenden vergeben werden
- Jeder andere Kontext mit wiederholten Antworten auf eine Reihe von Messsonden

Für jeden Datensatz im IRW gelten zwei Dinge:

- **Offen.** Jeder Datensatz ist für die Weiterverwendung lizenziert. Seine Herkunft ist dokumentiert, und der Code, der zur Umwandlung in das IRW-Format verwendet wurde, ist öffentlich zugänglich.
- **Harmonisiert.** Jeder Datensatz wird in dieselbe einfache Struktur umgeformt (unten beschrieben), sodass derselbe Analysecode mit wenig oder gar keiner Anpassung auf viele Datensätze angewendet werden kann.

Die Datensätze variieren stark in ihrer Größe (von einigen Hundert Antworten bis zu mehreren Millionen) und im Antworttyp (Ja/Nein-Items, mehrkategoriale Bewertungen, Teilpunktzahlen und mehr). Jeder Datensatz kommt zudem mit vorab berechneten Metadaten — Anzahl der Teilnehmenden, Anzahl der Items, Antwortdichte, Themengebiet und weitere beschreibende Tags —, sodass Forschende relevante Datensätze finden können, ohne zunächst alle herunterladen und verarbeiten zu müssen.

## Der Datenstandard

<img src="/welcome/assets/diagram-cross-classification.svg" alt="Diagramm, das ein farbiges Raster aus id-item-Zellen zeigt, das über einen Pfeil in eine Tabelle im Long-Format mit den Spalten id, item und resp umgeformt wird, wobei die Farbe jeder resp-Zelle der Farbe ihrer Ursprungszelle im Raster entspricht." class="welcome-figure">


Jeder IRW-Datensatz wird in ein **Long-Format** umgeformt: eine Zeile pro Antwort. Jede Zeile enthält mindestens drei Informationen:

| Spalte | Bedeutung |
|---|---|
| `id` | Wer (oder was) die Antwort erzeugt hat — in der Regel eine Person |
| `item` | Welche Messsonde die Antwort erzeugt hat — in der Regel eine Frage oder Aufgabe |
| `resp` | Die Antwort selbst, gespeichert als ordinaler Wert |

**Beispiel:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Wenn ein Datensatz zusätzliche Informationen enthält — etwa Antwortzeit, Identität der beurteilenden Person oder Kovariaten wie das Alter —, werden diese Informationen in zusätzlichen, einheitlich benannten Spalten gespeichert. Diese eine einfache Struktur deckt eine enorme Bandbreite an Messsituationen ab, was es ermöglicht, Analysecode nur einmal zu schreiben und auf das gesamte Warehouse anzuwenden.

Die vollständige technische Spezifikation des Standards ist verfügbar unter [itemresponsewarehouse.org/standard.html](/standard.qmd). Es gibt außerdem spezialisiertere Standards für Item-Text, paarweise Vergleichsdaten (Wettbewerbsdaten) und nominale (ungeordnete) Antworten.

## Wie man es nutzt

Es gibt drei Möglichkeiten, an IRW-Daten zu gelangen, je nachdem, wie viel Sie automatisieren möchten.

**1. Im Webbrowser durchsuchen**
Erkunden Sie Datensätze und ihre Metadaten direkt im [IRW-Datenbrowser](/data.qmd) — kein Konto erforderlich. Zum Herunterladen eines vollständigen Datensatzes ist ein kostenloses [Redivis](https://redivis.com)-Konto erforderlich, da diese Plattform die zugrunde liegenden Daten hostet.

**2. Das `irw`-Paket verwenden (empfohlen)**
Das `irw`-Paket, verfügbar sowohl für **R** als auch für **Python**, bietet einfache Funktionen zum Auffinden, Filtern und Herunterladen von Daten.

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

Beim ersten Gebrauch des Pakets werden Sie aufgefordert, sich mit einem kostenlosen Redivis-Konto anzumelden. Danach lädt eine einzige Codezeile jeden beliebigen Datensatz direkt in R oder Python herunter. Von da an sind die Daten bereit für die Analyse mit gängiger Software — etwa Paketen für Item-Response-Theorie oder Faktorenanalyse.

**3. Die Client-Bibliotheken von Redivis direkt verwenden**
Für Workflows auf niedrigerer Ebene oder außerhalb von R/Python kann auf die Daten auch über die eigenen R- und Python-Client-Bibliotheken von Redivis zugegriffen werden. Weitere Details finden Sie im [Einstiegsleitfaden](/getstarted.qmd).

### Mehr als nur Daten herunterladen

Das IRW-Projekt umfasst außerdem:

- Eine wachsende Sammlung von **[Vignetten](/vignettes/index.qmd)** — durchgearbeitete Beispiele, die klassische und neue Messmethoden gleichzeitig auf viele IRW-Datensätze anwenden
- **Schulungsmaterialien und Übungsaufgaben** für die Vermittlung von Psychometrie anhand echter Daten
- **Einen Beitragsprozess** für Forschende, die eigene Datensätze zum Warehouse hinzufügen möchten

## Mehr erfahren

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Website: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Wenn Sie IRW-Daten in Ihrer Arbeit verwenden, zitieren Sie bitte die Originaldaten (wir stellen dafür eine Funktion bereit). Es wäre außerdem großartig, wenn Sie auch den oben genannten Einführungsartikel zitieren würden.

---

*Fragen, Feedback, oder möchten Sie einen Datensatz beisteuern? Besuchen Sie die [Kontaktseite](/contact.qmd) oder eröffnen Sie ein Issue auf [GitHub](https://github.com/itemresponsewarehouse).*
