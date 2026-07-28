---
lang: sw
pagetitle: "Item Response Warehouse (IRW; Ghala la Majibu ya Vipengele)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Ghala la Majibu ya Vipengele)

**Mkusanyiko huru na wazi wa data ya majibu ya vipengele (item response) iliyosanifishwa, kwa ajili ya utafiti wa saikometriki na upimaji.**

[Soma karatasi](https://doi.org/10.3758/s13428-025-02796-y) **(ufikiaji huria)**

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

## Kwa nini IRW ipo

Watafiti wanaosoma upimaji — katika elimu, saikolojia, na fani zinazohusiana — wanahitaji data halisi ili kujaribu na kulinganisha mbinu zao. Data hiyo tayari ipo kwa wingi mkubwa. Lakini imetawanyika katika tafiti nyingi, imehifadhiwa katika miundo mingi tofauti, na mara nyingi ni ngumu kuitumia tena kwa sababu ya nyaraka au leseni zisizo wazi.

Hii ni tatizo linalojulikana sana. Jumuiya ya utafiti imeweka kiwango cha pamoja cha kulitatua: data inapaswa kuwa **FAIR** — inayopatikana kwa urahisi (Findable), inayofikiwa (Accessible), inayoweza kutumika pamoja na mifumo mingine (Interoperable), na inayoweza kutumika tena (Reusable) (Wilkinson et al., 2016). Fani nyingine zimeweka kanuni hizi katika vitendo kwa kujenga rasilimali za data zilizoshirikiwa na kusanifishwa. Katika sayansi ya kompyuta, mkusanyiko wa picha zilizowekwa lebo wa ImageNet uliwapa watafiti kigezo cha kawaida (benchmark) na ukasaidia kuchochea maendeleo ya haraka katika AI. Jenetiki na sayansi ya neva zimejenga rasilimali za pamoja zinazofanana kwa data zao wenyewe.

Item Response Warehouse (IRW) inatumia kanuni hizohizo za FAIR kwa data ya majibu ya vipengele. Inakusanya mamia ya seti za data zilizopo na kuzibadilisha kuwa muundo mmoja wa pamoja — ili mbinu iliyojaribiwa kwenye seti moja ya data iweze kujaribiwa kwa urahisi kwenye mamia ya seti nyingine.

## Kilichomo ndani ya IRW

IRW ina **mamia ya seti za data** ("majedwali"), kila moja ikiwa mkusanyiko wa majibu ya mtu mmoja mmoja. Jibu hutokea kila mara mtu (au kitengo kingine) anapojibu kipengele (item) au uchunguzi mwingine. Mifano ni pamoja na:

- Majibu ya wanafunzi katika mitihani ya elimu na uwezo
- Vipengele vya dodoso vinavyopima utu au mitazamo
- Alama zinazotolewa na wakadiriaji binadamu
- Hali nyingine yoyote inayohusisha majibu yanayojirudia kwa seti ya vifaa vya upimaji

Kila seti ya data katika IRW imeundwa iwe:

- **Inayopatikana kwa urahisi (Findable).** Kila seti ya data inakuja na metadata iliyokokotolewa tayari — idadi ya washiriki, idadi ya vipengele, msongamano wa majibu, uwanja wa somo, na lebo nyingine za maelezo — ili seti za data ziweze kupatikana na kuchujwa bila kuzipakua kwanza.
- **Inayofikiwa (Accessible).** Kila seti ya data inaweza kupatikana kupitia kivinjari cha wavuti au kifurushi cha `irw`, kwa akaunti ya bure.
- **Inayoweza kutumika pamoja na mifumo mingine (Interoperable).** Kila seti ya data hubadilishwa kuwa muundo mmoja rahisi (ulioelezwa hapa chini), ili msimbo huohuo wa uchambuzi uweze kutumika kwenye seti nyingi za data kwa mabadiliko kidogo au bila mabadiliko yoyote.
- **Inayoweza kutumika tena (Reusable).** Kila seti ya data ina leseni huria, asili yake imeandikwa, na msimbo uliotumika kuibadilisha kuwa muundo wa IRW ni wa umma.

Seti za data hutofautiana sana kwa ukubwa (kutoka majibu mia chache hadi mamilioni mengi) na kwa aina ya majibu (vipengele vya ndiyo/hapana, ukadiriaji wa makundi mengi, alama za sehemu, na mengineyo). Kila seti ya data pia huja na metadata iliyokokotolewa tayari — idadi ya washiriki, idadi ya vipengele, msongamano wa majibu, uwanja wa somo, na lebo nyingine za maelezo — ili watafiti waweze kupata seti za data zinazofaa bila kulazimika kuzipakua na kuzichambua zote kwanza.

## Kiwango cha data

<img src="/welcome/assets/diagram-cross-classification.svg" alt="Mchoro unaoonyesha gridi yenye rangi ya seli za id-item, iliyobadilishwa kupitia mshale kuwa jedwali la muundo mrefu lenye safu za id, item na resp, ambapo rangi ya kila seli ya resp inalingana na rangi ya seli yake asili kwenye gridi." class="welcome-figure">


Kila seti ya data ya IRW hubadilishwa kuwa **muundo mrefu (long format)**: mstari mmoja kwa kila jibu. Kwa kiwango cha chini kabisa, kila mstari una vipande vitatu vya taarifa:

| Safu | Maana |
|---|---|
| `id` | Nani (au nini) alizalisha jibu — kwa kawaida ni mtu |
| `item` | Ni kifaa gani cha upimaji kilichozalisha jibu — kwa kawaida ni swali au kazi |
| `resp` | Jibu lenyewe, likihifadhiwa kama alama ya mpangilio (ordinal) |

**Mfano:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Wakati seti ya data inapokuwa na taarifa za ziada — muda wa kujibu, utambulisho wa mkadiriaji, vigezo shirikishi kama umri — taarifa hizo huhifadhiwa katika safu za ziada zenye majina thabiti. Muundo huu mmoja rahisi unashughulikia wigo mkubwa wa hali za upimaji, na hii ndiyo inayowezesha kuandika msimbo wa uchambuzi mara moja na kuutumia katika ghala lote.

Maelezo kamili ya kiufundi ya kiwango yanapatikana kwenye [itemresponsewarehouse.org/standard.html](/standard.qmd). Kuna pia viwango vingine vinavyohusiana na maalum zaidi kwa maandishi ya vipengele, data ya kulinganisha kwa jozi (pairwise), na majibu ya nominali (makundi yasiyopangwa).

## Jinsi ya kuitumia

Kuna njia tatu za kupata data ya IRW, kutegemea unavyotaka kuchakata kiotomatiki.

**1. Vinjari kwenye kivinjari cha wavuti**
Chunguza seti za data na metadata yake moja kwa moja kwenye [kivinjari cha data cha IRW](/data.qmd) — hakuna akaunti inayohitajika. Kupakua seti kamili ya data kunahitaji akaunti huru ya [Redivis](https://redivis.com), kwani hicho ndicho jukwaa linalohifadhi data ya msingi.

**2. Tumia kifurushi cha `irw` (kinachopendekezwa)**
Kifurushi cha `irw`, kinachopatikana kwa **R** na **Python**, kinatoa vitendaji rahisi vya kutafuta, kuchuja, na kupakua data.

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

Mara ya kwanza kutumia kifurushi hiki, utaombwa kuingia (login) kwa akaunti huru ya Redivis. Baada ya hapo, mstari mmoja wa msimbo hupakua seti yoyote ya data moja kwa moja kwenye R au Python. Kuanzia hapo, data iko tayari kwa uchambuzi kwa kutumia programu za kawaida — kwa mfano vifurushi vya nadharia ya majibu ya vipengele (item response theory) au uchambuzi wa vipengele (factor analysis).

**3. Tumia moja kwa moja maktaba za mteja za Redivis**
Kwa mtiririko wa kazi wa kiwango cha chini zaidi au usio wa R/Python, data pia inaweza kupatikana kupitia maktaba za mteja za Redivis za R na Python. Angalia [Mwongozo wa Kuanza](/getstarted.qmd) kwa maelezo zaidi.

### Zaidi ya kupakua data

Mradi wa IRW pia unajumuisha:

- Mkusanyiko unaokua wa **[mifano ya kina (vignettes)](/vignettes/index.qmd)** — mifano iliyofanyiwa kazi inayotumia mbinu za kawaida na mpya za upimaji kwenye seti nyingi za data za IRW kwa wakati mmoja
- **Rasilimali za mafunzo na seti za mazoezi** kwa ajili ya kufundisha saikometriki kwa kutumia data halisi
- **Mchakato wa kuchangia** kwa watafiti wanaotaka kuongeza seti zao za data kwenye ghala

## Jifunze zaidi

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Tovuti: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Msimbo: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Ikiwa unatumia data ya IRW katika kazi yako, tafadhali taja (cite) data ya asili (tumetoa vitendaji vinavyowezesha hili). Tungefurahi pia kama ungetaja karatasi ya utangulizi iliyotajwa hapo juu.

---

*Una maswali, maoni, au unataka kuchangia seti ya data? Tembelea [ukurasa wa Mawasiliano](/contact.qmd) au fungua suala (issue) kwenye [GitHub](https://github.com/itemresponsewarehouse).*
