---
lang: fa
dir: rtl
pagetitle: "انبار پاسخ به سؤال‌ها (Item Response Warehouse, IRW)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# انبار پاسخ به سؤال‌ها (Item Response Warehouse, IRW)

**مجموعه‌ای رایگان و آزاد از داده‌های پاسخ به سؤال (item response) هماهنگ‌شده، برای پژوهش‌های روان‌سنجی (psychometrics) و اندازه‌گیری.**

[مقاله را بخوانید](https://doi.org/10.3758/s13428-025-02796-y) **(دسترسی آزاد)**

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

## چرا IRW وجود دارد

پژوهشگرانی که اندازه‌گیری را مطالعه می‌کنند — در آموزش، روان‌شناسی و حوزه‌های مرتبط — برای آزمودن و مقایسه روش‌های خود به داده‌های واقعی نیاز دارند. این داده‌ها از قبل به مقدار زیاد وجود دارند. اما در میان پژوهش‌های بسیاری پراکنده‌اند، در قالب‌های بسیار متفاوتی ذخیره شده‌اند، و اغلب به دلیل مستندسازی یا مجوز نامشخص، بازاستفاده از آن‌ها دشوار است.

این مشکلی شناخته‌شده است. حوزه‌های دیگر با ساختن منابع داده‌ای مشترک و استانداردشده آن را حل کرده‌اند. در علوم رایانه، مجموعه تصاویر برچسب‌گذاری‌شده ImageNet به پژوهشگران یک معیار سنجش (benchmark) مشترک داد و به پیشرفت سریع در هوش مصنوعی کمک کرد. ژنتیک و علوم اعصاب نیز منابع مشترک مشابهی برای داده‌های خود ساخته‌اند.

انبار پاسخ به سؤال‌ها (IRW) همین کار را برای داده‌های پاسخ به سؤال انجام می‌دهد. این پروژه صدها مجموعه‌داده موجود را گرد هم می‌آورد و آن‌ها را به یک قالب مشترک تبدیل می‌کند — به‌طوری‌که روشی که روی یک مجموعه‌داده آزموده شده، به‌آسانی روی صدها مجموعه‌داده دیگر نیز قابل آزمودن باشد.

## در IRW چه چیزی وجود دارد

IRW شامل **صدها مجموعه‌داده** ("جدول") است که هر یک مجموعه‌ای از پاسخ‌های فردی است. هر بار که شخصی (یا واحدی دیگر) به یک سؤال (item) (یا آزمون دیگری) پاسخ می‌دهد، یک پاسخ تولید می‌شود. نمونه‌ها عبارت‌اند از:

- پاسخ‌های دانش‌آموزان در آزمون‌های آموزشی و توانایی
- سؤال‌های پرسش‌نامه‌ای که شخصیت یا نگرش‌ها را می‌سنجند
- نمره‌هایی که ارزیاب‌های انسانی می‌دهند
- هر موقعیت دیگری که شامل پاسخ‌های تکراری به مجموعه‌ای از ابزارهای اندازه‌گیری باشد

دو ویژگی درباره‌ی هر مجموعه‌داده در IRW صادق است:

- **آزاد.** هر مجموعه‌داده برای بازاستفاده مجوز دارد. منشأ آن مستند شده، و کدی که برای تبدیل آن به قالب IRW استفاده شده، عمومی است.
- **هماهنگ‌شده (Harmonized).** هر مجموعه‌داده به همان ساختار ساده (که در ادامه توضیح داده می‌شود) تبدیل می‌شود، به‌طوری‌که همان کد تحلیلی بتواند با تغییرات کم یا بدون تغییر، روی مجموعه‌داده‌های بسیاری اجرا شود.

مجموعه‌داده‌ها از نظر اندازه (از چند صد پاسخ تا میلیون‌ها پاسخ) و از نظر نوع پاسخ (سؤال‌های بله/خیر، رتبه‌بندی‌های چندگانه، نمره‌های جزئی و موارد دیگر) بسیار متفاوت‌اند. هر مجموعه‌داده همچنین همراه با فراداده‌های از‌پیش‌محاسبه‌شده می‌آید — تعداد شرکت‌کنندگان، تعداد سؤال‌ها، چگالی پاسخ، حوزه‌ی موضوعی و دیگر برچسب‌های توصیفی — تا پژوهشگران بتوانند مجموعه‌داده‌های مرتبط را بدون نیاز به دانلود و پردازش همه‌ی آن‌ها پیدا کنند.

## استاندارد داده

<img src="/welcome/assets/diagram-cross-classification.svg" alt="نموداری شبکه‌ای که نشان می‌دهد هر پاسخ در تقاطع یک id و یک item قرار دارد." class="welcome-figure">


هر مجموعه‌داده‌ی IRW به **قالب بلند (long format)** تبدیل می‌شود: یک ردیف برای هر پاسخ. حداقل، هر ردیف شامل سه بخش اطلاعات است:

| ستون | معنا |
|---|---|
| `id` | چه کسی (یا چه چیزی) پاسخ را تولید کرده — معمولاً یک شخص |
| `item` | کدام ابزار اندازه‌گیری پاسخ را تولید کرده — معمولاً یک سؤال یا تکلیف |
| `resp` | خود پاسخ، که به‌صورت یک نمره‌ی ترتیبی (ordinal) ذخیره می‌شود |

**مثال:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

هنگامی که مجموعه‌داده‌ای شامل اطلاعات اضافی باشد — زمان پاسخ، هویت ارزیاب، متغیرهای همراه مانند سن — این اطلاعات در ستون‌های اضافی و با نام‌گذاری یکدست ذخیره می‌شود. همین یک ساختار ساده، طیف بسیار گسترده‌ای از موقعیت‌های اندازه‌گیری را پوشش می‌دهد، و همین است که نوشتن کد تحلیلی را یک‌بار و اعمال آن را در سراسر انبار داده ممکن می‌سازد.

مشخصات فنی کامل این استاندارد در [itemresponsewarehouse.org/standard.html](/standard.qmd) در دسترس است. استانداردهای مرتبط و تخصصی‌تری نیز برای متن سؤال، داده‌های مقایسه‌ی دوتایی (pairwise) و پاسخ‌های اسمی (nominal، مقوله‌های بدون ترتیب) وجود دارد.

## چگونه از آن استفاده کنیم

بسته به میزان خودکارسازی که می‌خواهید، سه راه برای دریافت داده‌های IRW وجود دارد.

**۱. مرور در مرورگر وب**
مجموعه‌داده‌ها و فراداده‌های آن‌ها را مستقیماً در [مرورگر داده‌ی IRW](/data.qmd) کاوش کنید — نیازی به حساب کاربری نیست. دانلود یک مجموعه‌داده‌ی کامل به یک حساب رایگان [Redivis](https://redivis.com) نیاز دارد، زیرا این همان پلتفرمی است که داده‌های زیربنایی را میزبانی می‌کند.

**۲. از بسته‌ی `irw` استفاده کنید (توصیه‌شده)**
بسته‌ی `irw`، که هم برای **R** و هم برای **Python** در دسترس است، توابعی ساده برای یافتن، فیلتر کردن و دانلود داده ارائه می‌دهد.

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

نخستین باری که از این بسته استفاده می‌کنید، از شما خواسته می‌شود با یک حساب رایگان Redivis وارد شوید. پس از آن، یک خط کد هر مجموعه‌داده را مستقیماً در R یا Python دانلود می‌کند. از این پس، داده برای تحلیل با نرم‌افزارهای استاندارد آماده است — برای مثال بسته‌های نظریه‌ی پاسخ به سؤال (item response theory) یا تحلیل عاملی.

**۳. مستقیماً از کتابخانه‌های کلاینت Redivis استفاده کنید**
برای گردش‌کارهای سطح پایین‌تر یا خارج از R/Python، داده همچنین از طریق کتابخانه‌های کلاینت R و Python خود Redivis نیز در دسترس است. برای جزئیات، به [راهنمای شروع کار](/getstarted.qmd) مراجعه کنید.

### فراتر از دانلود داده

پروژه‌ی IRW همچنین شامل موارد زیر است:

- مجموعه‌ای رو‌به‌رشد از **[نمونه‌های کاربردی (vignettes)](/vignettes/index.qmd)** — نمونه‌های تشریح‌شده‌ای که روش‌های اندازه‌گیری کلاسیک و جدید را به‌طور هم‌زمان روی بسیاری از مجموعه‌داده‌های IRW به‌کار می‌گیرند
- **منابع آموزشی و مجموعه‌تمرین‌ها** برای آموزش روان‌سنجی با داده‌های واقعی
- یک **فرایند مشارکت** برای پژوهشگرانی که می‌خواهند مجموعه‌داده‌های خود را به انبار داده اضافه کنند

## بیشتر بدانید

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- وب‌سایت: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- کد: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

اگر از داده‌های IRW در کار خود استفاده می‌کنید، لطفاً به داده‌ی اصلی استناد دهید (امکانات لازم برای این کار را فراهم کرده‌ایم). همچنین بسیار ارزشمند خواهد بود اگر به مقاله‌ی معرفی بالا نیز استناد دهید.

---

*سؤال، بازخورد دارید، یا می‌خواهید در یک مجموعه‌داده مشارکت کنید؟ به [صفحه‌ی تماس](/contact.qmd) مراجعه کنید یا در [GitHub](https://github.com/itemresponsewarehouse) یک issue باز کنید.*
