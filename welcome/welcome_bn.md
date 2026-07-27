---
lang: bn
pagetitle: "Item Response Warehouse (IRW; আইটেম রেসপন্স ওয়্যারহাউস)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; আইটেম রেসপন্স ওয়্যারহাউস)

**সাইকোমেট্রিক্স ও পরিমাপ গবেষণার জন্য সুসংগত (harmonized) আইটেম রেসপন্স ডেটার একটি বিনামূল্যের, উন্মুক্ত সংগ্রহ।**

[গবেষণাপত্রটি পড়ুন](https://doi.org/10.3758/s13428-025-02796-y) **(ওপেন অ্যাক্সেস)**

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

## IRW কেন প্রয়োজন

শিক্ষাবিজ্ঞান, মনোবিজ্ঞান এবং সংশ্লিষ্ট ক্ষেত্রে পরিমাপ নিয়ে গবেষণাকারী গবেষকদের নিজেদের পদ্ধতি পরীক্ষা ও তুলনা করার জন্য বাস্তব ডেটা প্রয়োজন। এই ধরনের ডেটা ইতিমধ্যেই প্রচুর পরিমাণে বিদ্যমান। কিন্তু তা বহু গবেষণায় বিক্ষিপ্তভাবে ছড়িয়ে আছে, বিভিন্ন ভিন্ন ফরম্যাটে সংরক্ষিত, এবং প্রায়শই অস্পষ্ট ডকুমেন্টেশন বা লাইসেন্সিংয়ের কারণে পুনরায় ব্যবহার করা কঠিন।

এটি একটি সুপরিচিত সমস্যা। অন্যান্য ক্ষেত্র শেয়ার্ড, প্রমিতকৃত (standardized) ডেটা রিসোর্স তৈরি করে এই সমস্যার সমাধান করেছে। কম্পিউটার বিজ্ঞানে, লেবেলযুক্ত ছবির সংগ্রহ ImageNet গবেষকদের একটি সাধারণ মানদণ্ড দিয়েছে এবং কৃত্রিম বুদ্ধিমত্তার দ্রুত অগ্রগতিতে সহায়ক হয়েছে। জেনেটিক্স ও নিউরোসায়েন্সও নিজেদের ডেটার জন্য একই ধরনের শেয়ার্ড রিসোর্স তৈরি করেছে।

আইটেম রেসপন্স ওয়্যারহাউস (IRW) আইটেম রেসপন্স ডেটার জন্য একই কাজটি করে। এটি শত শত বিদ্যমান ডেটাসেটকে একত্র করে একটি সাধারণ ফরম্যাটে রূপান্তরিত করে — যাতে একটি ডেটাসেটে পরীক্ষিত কোনো পদ্ধতি সহজেই অন্য শত শত ডেটাসেটে পরীক্ষা করা যায়।

## IRW-এ কী আছে

IRW-এ **শত শত ডেটাসেট** ("টেবিল") রয়েছে, প্রতিটি পৃথক রেসপন্সের একটি সংগ্রহ। কোনো ব্যক্তি (বা অন্য একক) কোনো আইটেমে (বা অন্য পরিমাপ প্রোবে) সাড়া দিলেই একটি রেসপন্স তৈরি হয়। উদাহরণস্বরূপ:

- শিক্ষা ও যোগ্যতা পরীক্ষায় শিক্ষার্থীদের উত্তর
- ব্যক্তিত্ব বা মনোভাব পরিমাপকারী জরিপের আইটেম
- মানব রেটারদের দেওয়া রেটিং
- পরিমাপ প্রোবের একটি সেটের প্রতি বারবার রেসপন্স জড়িত অন্য যেকোনো পরিস্থিতি

IRW-এর প্রতিটি ডেটাসেট সম্পর্কে দুটি বিষয় সত্য:

- **উন্মুক্ত।** প্রতিটি ডেটাসেট পুনরায় ব্যবহারের জন্য লাইসেন্সপ্রাপ্ত। এর উৎস ডকুমেন্টেড, এবং এটিকে IRW ফরম্যাটে রূপান্তর করতে ব্যবহৃত কোড সর্বজনীন।
- **সুসংগত (Harmonized)।** প্রতিটি ডেটাসেট একই সরল কাঠামোতে (নিচে বর্ণিত) রূপান্তরিত, যাতে একই বিশ্লেষণ কোড সামান্য বা কোনো পরিবর্তন ছাড়াই বহু ডেটাসেটে চালানো যায়।

ডেটাসেটগুলো আকারে ব্যাপকভাবে ভিন্ন (কয়েকশো রেসপন্স থেকে লক্ষ লক্ষ পর্যন্ত) এবং রেসপন্সের ধরনেও ভিন্ন (হ্যাঁ/না আইটেম, বহু-শ্রেণির রেটিং, আংশিক-নম্বর স্কোর, আরও অনেক কিছু)। প্রতিটি ডেটাসেটের সাথে পূর্ব-গণনাকৃত মেটাডেটাও থাকে — অংশগ্রহণকারীর সংখ্যা, আইটেমের সংখ্যা, রেসপন্স ঘনত্ব, বিষয়ক্ষেত্র এবং অন্যান্য বর্ণনামূলক ট্যাগ — যাতে গবেষকরা সবগুলো ডাউনলোড ও প্রক্রিয়াকরণ না করেই প্রাসঙ্গিক ডেটাসেট খুঁজে পেতে পারেন।

## ডেটা স্ট্যান্ডার্ড

<img src="/welcome/assets/diagram-cross-classification.svg" alt="একটি গ্রিড চিত্র যা দেখায় যে প্রতিটি প্রতিক্রিয়া একটি id এবং একটি item-এর সংযোগস্থলে অবস্থিত।" class="welcome-figure">


প্রতিটি IRW ডেটাসেট **লং ফরম্যাটে** রূপান্তরিত হয়: প্রতিটি রেসপন্সের জন্য একটি সারি। প্রতিটি সারিতে অন্তত তিনটি তথ্য থাকে:

| কলাম | অর্থ |
|---|---|
| `id` | কে (বা কী) রেসপন্স তৈরি করেছে — সাধারণত একজন ব্যক্তি |
| `item` | কোন পরিমাপ প্রোব রেসপন্স তৈরি করেছে — সাধারণত একটি প্রশ্ন বা কাজ |
| `resp` | রেসপন্স নিজেই, একটি অর্ডিনাল স্কোর হিসেবে সংরক্ষিত |

**উদাহরণ:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

কোনো ডেটাসেটে অতিরিক্ত তথ্য থাকলে — যেমন রেসপন্স টাইম, রেটারের পরিচয়, বয়সের মতো কোভ্যারিয়েট — সেই তথ্য অতিরিক্ত, সামঞ্জস্যপূর্ণভাবে নামকরণ করা কলামে সংরক্ষিত থাকে। এই একটি সরল কাঠামো বিস্তৃত পরিসরের পরিমাপ পরিস্থিতি কভার করে, আর এ কারণেই একবার বিশ্লেষণ কোড লিখে পুরো ওয়্যারহাউস জুড়ে তা প্রয়োগ করা সম্ভব হয়।

স্ট্যান্ডার্ডটির সম্পূর্ণ প্রযুক্তিগত বিবরণ পাওয়া যাবে [itemresponsewarehouse.org/standard.html](/standard.qmd)-এ। আইটেম টেক্সট, জোড়ায়-জোড়ায় প্রতিযোগিতার ডেটা, এবং নমিনাল (অক্রমিক শ্রেণির) রেসপন্সের জন্যও সম্পর্কিত, আরও বিশেষায়িত স্ট্যান্ডার্ড বিদ্যমান।

## কীভাবে ব্যবহার করবেন

আপনি কতটা স্বয়ংক্রিয় করতে চান তার উপর ভিত্তি করে IRW ডেটা পাওয়ার তিনটি উপায় আছে।

**১. ওয়েব ব্রাউজারে দেখুন**
কোনো অ্যাকাউন্ট ছাড়াই সরাসরি [IRW ডেটা ব্রাউজার](/data.qmd)-এ ডেটাসেট ও তাদের মেটাডেটা অন্বেষণ করুন। সম্পূর্ণ ডেটাসেট ডাউনলোড করতে একটি বিনামূল্যের [Redivis](https://redivis.com) অ্যাকাউন্ট প্রয়োজন, কারণ এই প্ল্যাটফর্মটিই মূল ডেটা হোস্ট করে।

**২. `irw` প্যাকেজ ব্যবহার করুন (প্রস্তাবিত)**
**R** ও **Python** উভয়ের জন্য উপলব্ধ `irw` প্যাকেজটি ডেটা খুঁজে বের করা, ফিল্টার করা এবং ডাউনলোড করার জন্য সহজ ফাংশন প্রদান করে।

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

প্যাকেজটি প্রথমবার ব্যবহারের সময়, একটি বিনামূল্যের Redivis অ্যাকাউন্ট দিয়ে লগ ইন করতে বলা হবে। তারপর, এক লাইনের কোড দিয়েই যেকোনো ডেটাসেট সরাসরি R বা Python-এ ডাউনলোড করা যায়। এরপর, স্ট্যান্ডার্ড সফটওয়্যার দিয়ে বিশ্লেষণের জন্য ডেটা প্রস্তুত থাকে — উদাহরণস্বরূপ, আইটেম রেসপন্স থিওরি বা ফ্যাক্টর অ্যানালাইসিস প্যাকেজ।

**৩. সরাসরি Redivis ক্লায়েন্ট লাইব্রেরি ব্যবহার করুন**
নিম্ন-স্তরের বা R/Python নয় এমন ওয়ার্কফ্লোর জন্য, Redivis-এর নিজস্ব R ও Python ক্লায়েন্ট লাইব্রেরির মাধ্যমেও ডেটা অ্যাক্সেস করা যায়। বিস্তারিত জানতে দেখুন [গেটিং স্টার্টেড গাইড](/getstarted.qmd)।

### শুধু ডেটা ডাউনলোডের বাইরেও

IRW প্রকল্পে আরও অন্তর্ভুক্ত রয়েছে:

- ক্রমবর্ধমান **[ভিনিয়েট (vignettes)](/vignettes/index.qmd)**-এর একটি সংগ্রহ — একসাথে বহু IRW ডেটাসেট জুড়ে ক্লাসিক ও নতুন পরিমাপ পদ্ধতি প্রয়োগের বিস্তারিত উদাহরণ
- বাস্তব ডেটা দিয়ে সাইকোমেট্রিক্স শেখানোর জন্য **প্রশিক্ষণ রিসোর্স ও অনুশীলন সেট**
- নিজেদের ডেটাসেট ওয়্যারহাউসে যোগ করতে ইচ্ছুক গবেষকদের জন্য একটি **অবদান প্রক্রিয়া (contribution process)**

## আরও জানুন

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- ওয়েবসাইট: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- কোড: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

আপনি যদি আপনার কাজে IRW-এর ডেটা ব্যবহার করেন, তাহলে অনুগ্রহ করে মূল ডেটা উদ্ধৃত (cite) করুন (আমরা এর জন্য প্রয়োজনীয় সুবিধা দিয়েছি)। উপরে উল্লিখিত পরিচিতিমূলক গবেষণাপত্রটিও উদ্ধৃত করলে আমরা কৃতজ্ঞ থাকব।

---

*প্রশ্ন, মতামত, অথবা কোনো ডেটাসেটে অবদান রাখতে চান? [যোগাযোগ পৃষ্ঠা](/contact.qmd) দেখুন অথবা [GitHub](https://github.com/itemresponsewarehouse)-এ একটি issue খুলুন।*
