---
lang: pl
pagetitle: "Item Response Warehouse (IRW; Magazyn Odpowiedzi na Zadania)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Magazyn Odpowiedzi na Zadania)

**Bezpłatny, otwarty zbiór ujednoliconych danych o odpowiedziach na zadania (item response) do badań psychometrycznych i pomiarowych.**

[Przeczytaj artykuł](https://doi.org/10.3758/s13428-025-02796-y) **(otwarty dostęp)**

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

## Dlaczego powstał IRW

Badacze zajmujący się pomiarem — w edukacji, psychologii i pokrewnych dziedzinach — potrzebują rzeczywistych danych, aby testować i porównywać swoje metody. Takie dane już istnieją w dużych ilościach. Są jednak rozproszone w wielu badaniach, przechowywane w wielu różnych formatach i często trudne do ponownego wykorzystania z powodu niejasnej dokumentacji lub licencji.

To dobrze znany problem. Inne dziedziny rozwiązały go, budując wspólne, ustandaryzowane zasoby danych. W informatyce zbiór oznaczonych obrazów ImageNet dał badaczom wspólny punkt odniesienia (benchmark) i pomógł napędzić szybki postęp w AI. Genetyka i neuronauka zbudowały podobne wspólne zasoby dla swoich danych.

Item Response Warehouse (IRW) robi to samo dla danych o odpowiedziach na zadania. Gromadzi setki istniejących zbiorów danych i przekształca je w jeden wspólny format — dzięki czemu metoda przetestowana na jednym zbiorze danych może łatwo zostać przetestowana na setkach innych.

## Co zawiera IRW

IRW zawiera **setki zbiorów danych** ("tabel"), z których każdy jest zbiorem indywidualnych odpowiedzi. Odpowiedź powstaje za każdym razem, gdy jakaś osoba (lub inna jednostka) odpowiada na zadanie (item) lub inny bodziec pomiarowy. Przykłady obejmują:

- Odpowiedzi uczniów w testach edukacyjnych i testach zdolności
- Pozycje kwestionariuszy mierzące osobowość lub postawy
- Oceny przyznawane przez ludzkich sędziów (raterów)
- Każdą inną sytuację obejmującą powtarzane odpowiedzi na zestaw narzędzi pomiarowych

W przypadku każdego zbioru danych w IRW prawdziwe są dwie rzeczy:

- **Otwarty.** Każdy zbiór danych jest udostępniony na licencji zezwalającej na ponowne wykorzystanie. Jego pochodzenie jest udokumentowane, a kod użyty do przekształcenia go do formatu IRW jest publicznie dostępny.
- **Ujednolicony (harmonized).** Każdy zbiór danych jest przekształcany do tej samej prostej struktury (opisanej poniżej), dzięki czemu ten sam kod analityczny może działać na wielu zbiorach danych przy niewielkiej modyfikacji lub bez żadnej.

Zbiory danych różnią się znacznie pod względem wielkości (od kilkuset odpowiedzi do wielu milionów) oraz rodzaju odpowiedzi (zadania tak/nie, oceny wielokategorialne, wyniki częściowe i inne). Do każdego zbioru danych dołączone są również wstępnie obliczone metadane — liczba uczestników, liczba zadań, gęstość odpowiedzi, dziedzina tematyczna oraz inne opisowe etykiety — dzięki czemu badacze mogą znaleźć odpowiednie zbiory danych bez konieczności ich wcześniejszego pobierania i przetwarzania.

## Standard danych

<img src="/welcome/assets/diagram-cross-classification.svg" alt="Diagram siatki pokazujący, że każda odpowiedź znajduje się na przecięciu jednego id i jednego item." class="welcome-figure">


Każdy zbiór danych IRW jest przekształcany do **formatu długiego (long format)**: jeden wiersz na odpowiedź. Każdy wiersz zawiera co najmniej trzy informacje:

| Kolumna | Znaczenie |
|---|---|
| `id` | Kto (lub co) wygenerowało odpowiedź — zazwyczaj osoba |
| `item` | Które narzędzie pomiarowe wygenerowało odpowiedź — zazwyczaj pytanie lub zadanie |
| `resp` | Sama odpowiedź, przechowywana jako wynik porządkowy (ordinalny) |

**Przykład:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Gdy zbiór danych zawiera dodatkowe informacje — czas odpowiedzi, tożsamość ratera, kowarianty takie jak wiek — informacje te są przechowywane w dodatkowych, spójnie nazwanych kolumnach. Ta jedna prosta struktura obejmuje ogromny zakres sytuacji pomiarowych, co sprawia, że możliwe jest napisanie kodu analitycznego raz i zastosowanie go w całym magazynie.

Pełna specyfikacja techniczna standardu jest dostępna pod adresem [itemresponsewarehouse.org/standard.html](/standard.qmd). Istnieją także powiązane, bardziej wyspecjalizowane standardy dla tekstu zadań, danych porównań parami (pairwise) oraz odpowiedzi nominalnych (kategorii nieuporządkowanych).

## Jak z niego korzystać

Istnieją trzy sposoby pozyskania danych IRW, w zależności od tego, ile chcesz zautomatyzować.

**1. Przeglądaj w przeglądarce internetowej**
Przeglądaj zbiory danych i ich metadane bezpośrednio w [przeglądarce danych IRW](/data.qmd) — bez konieczności zakładania konta. Pobranie pełnego zbioru danych wymaga bezpłatnego konta [Redivis](https://redivis.com), ponieważ to ta platforma przechowuje bazowe dane.

**2. Użyj pakietu `irw` (zalecane)**
Pakiet `irw`, dostępny zarówno dla **R**, jak i **Pythona**, oferuje proste funkcje do wyszukiwania, filtrowania i pobierania danych.

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

Przy pierwszym użyciu pakietu zostaniesz poproszony o zalogowanie się za pomocą bezpłatnego konta Redivis. Od tego momentu jedna linijka kodu pobiera dowolny zbiór danych bezpośrednio do R lub Pythona. Od tego etapu dane są gotowe do analizy za pomocą standardowego oprogramowania — na przykład pakietów do teorii odpowiedzi na zadania (IRT) lub analizy czynnikowej.

**3. Użyj bezpośrednio bibliotek klienckich Redivis**
W przypadku bardziej niskopoziomowych przepływów pracy lub przepływów spoza R/Python dane można również uzyskać za pomocą własnych bibliotek klienckich R i Python firmy Redivis. Szczegóły znajdziesz w [Przewodniku dla początkujących](/getstarted.qmd).

### Poza pobieraniem danych

Projekt IRW obejmuje również:

- Rosnący zbiór **[przykładów (vignettes)](/vignettes/index.qmd)** — opracowanych przykładów stosujących klasyczne i nowe metody pomiarowe jednocześnie na wielu zbiorach danych IRW
- **Materiały szkoleniowe i zestawy zadań** do nauczania psychometrii na rzeczywistych danych
- **Proces zgłaszania danych** dla badaczy, którzy chcą dodać własne zbiory danych do magazynu

## Więcej informacji

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Strona internetowa: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Kod: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Jeśli korzystasz z danych IRW w swojej pracy, prosimy o cytowanie oryginalnych danych (udostępniliśmy odpowiednią funkcjonalność w tym celu). Bylibyśmy również bardzo wdzięczni za zacytowanie powyższego artykułu wprowadzającego.

---

*Masz pytania, uwagi lub chcesz dodać zbiór danych? Odwiedź [stronę kontaktową](/contact.qmd) lub otwórz zgłoszenie (issue) na [GitHubie](https://github.com/itemresponsewarehouse).*
