---
lang: ta
pagetitle: "Item Response Warehouse (IRW; உருப்படி பதில் தரவுக் களஞ்சியம்)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; உருப்படி பதில் தரவுக் களஞ்சியம்)

**உளவியல் அளவீடு (psychometrics) மற்றும் அளவீட்டு ஆய்வுக்காக, ஒருங்கிணைக்கப்பட்ட உருப்படி பதில் (item response) தரவுகளின் இலவச, திறந்த தொகுப்பு.**

[கட்டுரையைப் படிக்க](https://doi.org/10.3758/s13428-025-02796-y) **(திறந்த அணுகல்)**

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

## IRW ஏன் உள்ளது

அளவீட்டைப் பற்றி ஆய்வு செய்யும் ஆராய்ச்சியாளர்கள் — கல்வி, உளவியல் மற்றும் தொடர்புடைய துறைகளில் — தங்கள் முறைகளைச் சோதிக்கவும் ஒப்பிடவும் உண்மையான தரவு தேவைப்படுகிறது. அத்தகைய தரவு ஏற்கெனவே பெருமளவில் உள்ளது. ஆனால் அது பல ஆய்வுகளில் சிதறிக் கிடக்கிறது, பல்வேறு வடிவங்களில் சேமிக்கப்பட்டுள்ளது, மேலும் தெளிவற்ற ஆவணப்படுத்தல் அல்லது உரிமங்கள் காரணமாக மீண்டும் பயன்படுத்த பெரும்பாலும் கடினமாக உள்ளது.

இது நன்கு அறியப்பட்ட ஒரு பிரச்சினை. இதைத் தீர்க்க ஆராய்ச்சி சமூகம் ஒரு பொதுவான தரநிலையை வகுத்துள்ளது: தரவு **FAIR** ஆக இருக்க வேண்டும் — அதாவது கண்டுபிடிக்கக்கூடியது (Findable), அணுகக்கூடியது (Accessible), இயங்கக்கூடியது (Interoperable), மற்றும் மீண்டும் பயன்படுத்தக்கூடியது (Reusable) (Wilkinson et al., 2016). மற்ற துறைகள் இந்தக் கொள்கைகளை நடைமுறைப்படுத்தி, பகிரப்பட்ட, தரப்படுத்தப்பட்ட தரவு வளங்களை உருவாக்கியுள்ளன. கணினி அறிவியலில், லேபிள் இடப்பட்ட படங்களின் தொகுப்பான ImageNet, ஆராய்ச்சியாளர்களுக்கு ஒரு பொதுவான அளவுகோலை (benchmark) வழங்கியது, மேலும் AI-இல் விரைவான முன்னேற்றத்தை இயக்க உதவியது. மரபியல் மற்றும் நரம்பியல் அறிவியலும் தங்கள் சொந்த தரவுக்காக இதே போன்ற பகிரப்பட்ட வளங்களை உருவாக்கியுள்ளன.

Item Response Warehouse (IRW) அதே FAIR கொள்கைகளை உருப்படி பதில் தரவுக்குப் பயன்படுத்துகிறது. இது ஏற்கெனவே உள்ள நூற்றுக்கணக்கான தரவுத் தொகுப்புகளை ஒன்றிணைத்து, அவற்றை ஒரே பொதுவான வடிவமைப்பாக மாற்றுகிறது — இதனால் ஒரு தரவுத் தொகுப்பில் சோதிக்கப்பட்ட ஒரு முறையை, நூற்றுக்கணக்கான மற்ற தரவுத் தொகுப்புகளிலும் எளிதாக சோதிக்க முடியும்.

## IRW-இல் என்ன உள்ளது

IRW-இல் **நூற்றுக்கணக்கான தரவுத் தொகுப்புகள்** ("அட்டவணைகள்") உள்ளன, ஒவ்வொன்றும் தனிப்பட்ட பதில்களின் தொகுப்பாகும். ஒரு நபர் (அல்லது வேறு ஒரு அலகு) ஒரு உருப்படிக்கு (item) (அல்லது வேறு சோதனைக்கு) பதிலளிக்கும் போதெல்லாம் ஒரு பதில் உருவாகிறது. எடுத்துக்காட்டுகள்:

- கல்வி மற்றும் திறன் தேர்வுகளில் மாணவர்களின் பதில்கள்
- ஆளுமை அல்லது மனப்போக்குகளை அளவிடும் கணக்கெடுப்பு உருப்படிகள்
- மனித மதிப்பீட்டாளர்களால் வழங்கப்படும் மதிப்பெண்கள்
- அளவீட்டு கருவிகளின் தொகுப்புக்கு மீண்டும் மீண்டும் பதிலளிக்கும் வேறு எந்த சூழலும்

IRW-இல் உள்ள ஒவ்வொரு தரவுத் தொகுப்பும் பின்வருவனவற்றுக்காக வடிவமைக்கப்பட்டுள்ளது:

- **கண்டுபிடிக்கக்கூடியது (Findable).** ஒவ்வொரு தரவுத் தொகுப்பும் முன்கூட்டியே கணக்கிடப்பட்ட மேலிதரவுடன் (metadata) வருகிறது — பங்கேற்பாளர்களின் எண்ணிக்கை, உருப்படிகளின் எண்ணிக்கை, பதில் அடர்த்தி, பொருள் பரப்பு மற்றும் பிற விளக்கக் குறிச்சொற்கள் — இதனால் தரவுத் தொகுப்புகளை முதலில் பதிவிறக்காமலேயே கண்டறியவும் வடிகட்டவும் முடியும்.
- **அணுகக்கூடியது (Accessible).** ஒவ்வொரு தரவுத் தொகுப்பையும் இணைய உலாவி அல்லது `irw` தொகுப்பு மூலம், ஒரு இலவச கணக்கைப் பயன்படுத்தி பெறலாம்.
- **இயங்கக்கூடியது (Interoperable).** ஒவ்வொரு தரவுத் தொகுப்பும் அதே எளிய அமைப்பாக (கீழே விவரிக்கப்பட்டுள்ளது) மறுவடிவமைக்கப்படுகிறது, இதனால் ஒரே பகுப்பாய்வுக் குறியீட்டை, சிறிதளவு அல்லது எந்த மாற்றமும் இல்லாமல், பல தரவுத் தொகுப்புகளில் இயக்க முடியும்.
- **மீண்டும் பயன்படுத்தக்கூடியது (Reusable).** ஒவ்வொரு தரவுத் தொகுப்பும் திறந்த உரிமத்தின் கீழ் உள்ளது, அதன் தோற்றம் ஆவணப்படுத்தப்பட்டுள்ளது, மேலும் அதை IRW வடிவமைப்பாக மாற்ற பயன்படுத்தப்பட்ட குறியீடு பொதுவில் கிடைக்கிறது.

தரவுத் தொகுப்புகள் அளவில் (சில நூறு பதில்களிலிருந்து பல மில்லியன் வரை) மற்றும் பதில் வகையில் (ஆம்/இல்லை உருப்படிகள், பல-வகை மதிப்பீடுகள், பகுதி மதிப்பெண்கள் மற்றும் பல) பரவலாக மாறுபடுகின்றன. ஒவ்வொரு தரவுத் தொகுப்பும் முன்கூட்டியே கணக்கிடப்பட்ட மேலிதரவுடன் (metadata) வருகிறது — பங்கேற்பாளர்களின் எண்ணிக்கை, உருப்படிகளின் எண்ணிக்கை, பதில் அடர்த்தி, பொருள் பரப்பு மற்றும் பிற விளக்கக் குறிச்சொற்கள் — இதனால் ஆராய்ச்சியாளர்கள் முதலில் அனைத்தையும் பதிவிறக்கம் செய்து செயலாக்காமலேயே தொடர்புடைய தரவுத் தொகுப்புகளைக் கண்டறிய முடியும்.

## தரவு தரநிலை

<img src="/welcome/assets/diagram-cross-classification.svg" alt="id-item கலங்களைக் கொண்ட வண்ணமயமான கட்டத்தை, ஒரு அம்புக்குறியின் மூலம், id, item, resp நெடுவரிசைகளைக் கொண்ட நீள வடிவமைப்பு அட்டவணையாக மாற்றியதைக் காட்டும் வரைபடம்; ஒவ்வொரு resp கலத்தின் நிறமும் கட்டத்தில் உள்ள அதன் மூலக் கலத்தின் நிறத்துடன் பொருந்துகிறது." class="welcome-figure">


ஒவ்வொரு IRW தரவுத் தொகுப்பும் **நீள வடிவமைப்பாக (long format)** மறுவடிவமைக்கப்படுகிறது: ஒரு பதிலுக்கு ஒரு வரிசை. குறைந்தபட்சம், ஒவ்வொரு வரிசையிலும் மூன்று தகவல்கள் உள்ளன:

| நெடுவரிசை | பொருள் |
|---|---|
| `id` | பதிலை உருவாக்கியது யார் (அல்லது எது) — பொதுவாக ஒரு நபர் |
| `item` | எந்த அளவீட்டு கருவி பதிலை உருவாக்கியது — பொதுவாக ஒரு கேள்வி அல்லது பணி |
| `resp` | பதில் தானே, ஒரு வரிசை மதிப்பெண்ணாக (ordinal) சேமிக்கப்படுகிறது |

**எடுத்துக்காட்டு:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

ஒரு தரவுத் தொகுப்பில் கூடுதல் தகவல்கள் — பதில் நேரம், மதிப்பீட்டாளர் அடையாளம், வயது போன்ற இணை மாறிகள் — இருக்கும்போது, அந்தத் தகவல் ஒத்திசைவாகப் பெயரிடப்பட்ட கூடுதல் நெடுவரிசைகளில் சேமிக்கப்படுகிறது. இந்த ஒரே எளிய அமைப்பு அளவீட்டு சூழல்களின் மிகப் பரந்த வரம்பை உள்ளடக்குகிறது, இதுவே பகுப்பாய்வுக் குறியீட்டை ஒருமுறை எழுதி முழு களஞ்சியத்திலும் பயன்படுத்த முடியும் என்பதற்கான காரணமாகும்.

தரநிலையின் முழுமையான தொழில்நுட்பக் குறிப்பு [itemresponsewarehouse.org/standard.html](/standard.qmd) இல் கிடைக்கிறது. உருப்படி உரை, ஜோடி-ஒப்பீட்டு (pairwise) தரவு, மற்றும் பெயரடையாக (nominal, வரிசைப்படுத்தப்படாத வகைகள்) பதில்கள் ஆகியவற்றிற்கான தொடர்புடைய, மேலும் சிறப்புவாய்ந்த தரநிலைகளும் உள்ளன.

## எப்படிப் பயன்படுத்துவது

நீங்கள் எவ்வளவு தானியங்கிமயமாக்க விரும்புகிறீர்கள் என்பதைப் பொறுத்து IRW தரவைப் பெற மூன்று வழிகள் உள்ளன.

**1. இணைய உலாவியில் உலாவவும்**
[IRW தரவு உலாவி](/data.qmd) இல் தரவுத் தொகுப்புகளையும் அவற்றின் மேலிதரவையும் நேரடியாக ஆராயுங்கள் — கணக்கு தேவையில்லை. முழுமையான தரவுத் தொகுப்பைப் பதிவிறக்குவதற்கு இலவச [Redivis](https://redivis.com) கணக்கு தேவை, ஏனெனில் அடிப்படைத் தரவை ஏற்று வைத்திருப்பது அந்த தளமாகும்.

**2. `irw` தொகுப்பைப் பயன்படுத்தவும் (பரிந்துரைக்கப்படுகிறது)**
**R** மற்றும் **Python** இரண்டிற்கும் கிடைக்கும் `irw` தொகுப்பு, தரவைக் கண்டறிதல், வடிகட்டுதல் மற்றும் பதிவிறக்குதலுக்கான எளிய செயல்பாடுகளை வழங்குகிறது.

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

நீங்கள் இந்தத் தொகுப்பை முதன்முறையாகப் பயன்படுத்தும்போது, இலவச Redivis கணக்கு மூலம் உள்நுழையுமாறு கேட்கப்படுவீர்கள். அதன் பிறகு, ஒரே ஒரு வரி குறியீடு எந்தத் தரவுத் தொகுப்பையும் நேரடியாக R அல்லது Python-இல் பதிவிறக்குகிறது. அங்கிருந்து, தரவு நிலையான மென்பொருளைக் கொண்டு பகுப்பாய்வு செய்யத் தயாராக உள்ளது — உதாரணமாக, உருப்படி பதில் கோட்பாடு (item response theory) அல்லது காரணி பகுப்பாய்வு (factor analysis) தொகுப்புகள்.

**3. Redivis கிளையன்ட் நூலகங்களை நேரடியாகப் பயன்படுத்தவும்**
குறைந்த-நிலை அல்லது R/Python அல்லாத பணிப்பாய்வுகளுக்கு, Redivis-இன் சொந்த R மற்றும் Python கிளையன்ட் நூலகங்கள் மூலமும் தரவை அணுக முடியும். விவரங்களுக்கு [தொடங்குதல் வழிகாட்டி](/getstarted.qmd) ஐப் பார்க்கவும்.

### தரவைப் பதிவிறக்குவதற்கு அப்பால்

IRW திட்டத்தில் பின்வருவனவும் அடங்கும்:

- பல IRW தரவுத் தொகுப்புகளில் ஒரே நேரத்தில் பாரம்பரிய மற்றும் புதிய அளவீட்டு முறைகளைப் பயன்படுத்தும், வளர்ந்து வரும் **[சான்று ஆய்வுகளின் (vignettes)](/vignettes/index.qmd)** தொகுப்பு
- உண்மையான தரவுடன் உளவியல் அளவீட்டைக் கற்பிப்பதற்கான **பயிற்சி வளங்கள் மற்றும் பயிற்சிக் கேள்வித் தொகுப்புகள்**
- தங்கள் சொந்த தரவுத் தொகுப்புகளை களஞ்சியத்தில் சேர்க்க விரும்பும் ஆராய்ச்சியாளர்களுக்கான **பங்களிப்புச் செயல்முறை**

## மேலும் அறிய

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- இணையதளம்: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- குறியீடு: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

உங்கள் பணியில் IRW தரவைப் பயன்படுத்தினால், தயவுசெய்து மூலத் தரவை மேற்கோள் காட்டவும் (இதைச் செய்வதற்கான செயல்பாட்டை நாங்கள் வழங்கியுள்ளோம்). மேலே உள்ள அறிமுகக் கட்டுரையையும் நீங்கள் மேற்கோள் காட்டினால் அது எங்களுக்கு மிகவும் மதிப்புமிக்கதாக இருக்கும்.

---

*கேள்விகள், கருத்துகள் உள்ளதா, அல்லது ஒரு தரவுத் தொகுப்பிற்குப் பங்களிக்க விரும்புகிறீர்களா? [தொடர்பு பக்கத்தை](/contact.qmd) பார்வையிடவும் அல்லது [GitHub](https://github.com/itemresponsewarehouse) இல் ஒரு issue-ஐத் திறக்கவும்.*
