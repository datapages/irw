---
lang: ur
dir: rtl
pagetitle: "آئٹم ریسپانس ویئر ہاؤس (Item Response Warehouse, IRW)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# آئٹم ریسپانس ویئر ہاؤس (Item Response Warehouse, IRW)

**نفسیاتی پیمائش (psychometrics) اور پیمائشی تحقیق کے لیے، ہم آہنگ کردہ آئٹم ریسپانس ڈیٹا کا ایک مفت، آزاد مجموعہ۔**

[مقالہ پڑھیں](https://doi.org/10.3758/s13428-025-02796-y) **(اوپن ایکسیس)**

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

## IRW کیوں موجود ہے

پیمائش پر تحقیق کرنے والے محققین — تعلیم، نفسیات اور متعلقہ شعبوں میں — کو اپنے طریقہ ہائے کار کو جانچنے اور ان کا موازنہ کرنے کے لیے حقیقی ڈیٹا کی ضرورت ہوتی ہے۔ یہ ڈیٹا پہلے سے ہی بڑی مقدار میں موجود ہے۔ لیکن یہ بہت سے مطالعات میں بکھرا ہوا ہے، مختلف فارمیٹس میں محفوظ ہے، اور اکثر غیر واضح دستاویزات یا لائسنسنگ کی وجہ سے دوبارہ استعمال کرنا مشکل ہوتا ہے۔

یہ ایک معروف مسئلہ ہے۔ دیگر شعبوں نے مشترکہ، معیاری ڈیٹا وسائل تعمیر کر کے اسے حل کیا ہے۔ کمپیوٹر سائنس میں، لیبل شدہ تصاویر کے مجموعے ImageNet نے محققین کو ایک مشترکہ معیار (benchmark) فراہم کیا اور AI میں تیز رفتار ترقی کو آگے بڑھانے میں مدد دی۔ جینیات اور اعصابی سائنس نے بھی اپنے ڈیٹا کے لیے اسی طرح کے مشترکہ وسائل تعمیر کیے ہیں۔

آئٹم ریسپانس ویئر ہاؤس (IRW) آئٹم ریسپانس ڈیٹا کے لیے بالکل یہی کام کرتا ہے۔ یہ سینکڑوں موجودہ ڈیٹاسیٹس کو اکٹھا کرتا ہے اور انہیں ایک مشترکہ فارمیٹ میں ڈھالتا ہے — تاکہ ایک ڈیٹاسیٹ پر جانچا گیا کوئی طریقہ آسانی سے سینکڑوں دیگر ڈیٹاسیٹس پر بھی آزمایا جا سکے۔

## IRW میں کیا شامل ہے

IRW میں **سینکڑوں ڈیٹاسیٹس** ("ٹیبلز") شامل ہیں، جن میں سے ہر ایک انفرادی جوابات کا مجموعہ ہے۔ جب بھی کوئی شخص (یا کوئی اور اکائی) کسی آئٹم (یا کسی اور پیمائشی سوال) کا جواب دیتا ہے تو ایک ریسپانس تخلیق ہوتا ہے۔ مثالوں میں شامل ہیں:

- تعلیمی اور صلاحیت کے امتحانات میں طلبہ کے جوابات
- شخصیت یا رویوں کی پیمائش کرنے والے سروے آئٹمز
- انسانی جانچنے والوں (raters) کی جانب سے دیے گئے نمبرات
- کسی بھی دوسری صورتحال میں پیمائشی آلات کے ایک مجموعے پر بار بار دیے گئے جوابات

IRW میں شامل ہر ڈیٹاسیٹ کے بارے میں دو باتیں سچ ہیں:

- **آزاد (Open)۔** ہر ڈیٹاسیٹ دوبارہ استعمال کے لیے لائسنس یافتہ ہے۔ اس کا ماخذ دستاویزی ہے، اور اسے IRW فارمیٹ میں تبدیل کرنے کے لیے استعمال ہونے والا کوڈ عوامی طور پر دستیاب ہے۔
- **ہم آہنگ (Harmonized)۔** ہر ڈیٹاسیٹ کو اسی سادہ ڈھانچے (نیچے بیان کیا گیا ہے) میں ڈھالا جاتا ہے، تاکہ ایک ہی تجزیاتی کوڈ بہت کم یا بغیر کسی ترمیم کے کئی ڈیٹاسیٹس پر چلایا جا سکے۔

ڈیٹاسیٹس اپنے حجم (چند سو جوابات سے لے کر کئی ملین تک) اور جواب کی قسم (ہاں/نہیں آئٹمز، متعدد زمروں کی درجہ بندی، جزوی نمبرات، اور مزید) کے لحاظ سے کافی مختلف ہوتے ہیں۔ ہر ڈیٹاسیٹ پہلے سے شمار شدہ میٹا ڈیٹا کے ساتھ بھی آتا ہے — شرکاء کی تعداد، آئٹمز کی تعداد، ریسپانس کی کثافت، موضوع کا شعبہ، اور دیگر وضاحتی ٹیگز — تاکہ محققین سب کچھ ڈاؤن لوڈ اور پراسیس کیے بغیر متعلقہ ڈیٹاسیٹس تلاش کر سکیں۔

## ڈیٹا کا معیار

<img src="/welcome/assets/diagram-cross-classification.svg" alt="ایک گرڈ خاکہ جو دکھاتا ہے کہ ہر ریسپانس ایک id اور ایک item کے تقاطع پر واقع ہے۔" class="welcome-figure">


ہر IRW ڈیٹاسیٹ کو **لانگ فارمیٹ (long format)** میں ڈھالا جاتا ہے: ہر ریسپانس کے لیے ایک قطار۔ کم از کم، ہر قطار میں تین معلومات ہوتی ہیں:

| کالم | مطلب |
|---|---|
| `id` | ریسپانس کس نے (یا کیا نے) پیدا کیا — عام طور پر ایک شخص |
| `item` | کس پیمائشی آلے نے یہ ریسپانس پیدا کیا — عام طور پر ایک سوال یا کام |
| `resp` | خود ریسپانس، جو ایک ترتیبی (ordinal) اسکور کے طور پر محفوظ کیا جاتا ہے |

**مثال:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

جب کسی ڈیٹاسیٹ میں اضافی معلومات شامل ہوں — جیسے ریسپانس ٹائم، جانچنے والے کی شناخت، عمر جیسے covariates — تو یہ معلومات اضافی، یکساں طور پر نامزد کالموں میں محفوظ کی جاتی ہیں۔ یہ ایک سادہ ڈھانچہ پیمائشی صورتحال کی ایک وسیع رینج کا احاطہ کرتا ہے، اور یہی وہ چیز ہے جو تجزیاتی کوڈ کو ایک بار لکھ کر پورے ویئر ہاؤس پر لاگو کرنا ممکن بناتی ہے۔

معیار کی مکمل تکنیکی تفصیل [itemresponsewarehouse.org/standard.html](/standard.qmd) پر دستیاب ہے۔ آئٹم ٹیکسٹ، جوڑے وار مقابلے (pairwise) کے ڈیٹا، اور نامزد (nominal، غیر ترتیب یافتہ زمرے) کے جوابات کے لیے متعلقہ، زیادہ خصوصی معیارات بھی موجود ہیں۔

## اسے کیسے استعمال کریں

آپ کتنا خودکار بنانا چاہتے ہیں اس پر منحصر، IRW ڈیٹا حاصل کرنے کے تین طریقے ہیں۔

**1. ویب براؤزر میں دیکھیں**
[IRW ڈیٹا براؤزر](/data.qmd) پر ڈیٹاسیٹس اور ان کے میٹا ڈیٹا کو براہ راست دیکھیں — کسی اکاؤنٹ کی ضرورت نہیں۔ مکمل ڈیٹاسیٹ ڈاؤن لوڈ کرنے کے لیے ایک مفت [Redivis](https://redivis.com) اکاؤنٹ درکار ہے، کیونکہ یہ وہی پلیٹ فارم ہے جو بنیادی ڈیٹا کو میزبانی کرتا ہے۔

**2. `irw` پیکیج استعمال کریں (تجویز کردہ)**
`irw` پیکیج، جو **R** اور **Python** دونوں کے لیے دستیاب ہے، ڈیٹا تلاش کرنے، فلٹر کرنے اور ڈاؤن لوڈ کرنے کے لیے سادہ فنکشنز فراہم کرتا ہے۔

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

پہلی بار جب آپ یہ پیکیج استعمال کریں گے، آپ سے ایک مفت Redivis اکاؤنٹ کے ذریعے لاگ ان کرنے کو کہا جائے گا۔ اس کے بعد، کوڈ کی ایک لائن کسی بھی ڈیٹاسیٹ کو براہ راست R یا Python میں ڈاؤن لوڈ کر دیتی ہے۔ اس مقام سے، ڈیٹا معیاری سافٹ ویئر — مثال کے طور پر آئٹم ریسپانس تھیوری یا فیکٹر تجزیہ کے پیکیجز — کے ساتھ تجزیے کے لیے تیار ہوتا ہے۔

**3. Redivis کی کلائنٹ لائبریریز براہ راست استعمال کریں**
نچلی سطح کے یا R/Python کے علاوہ ورک فلوز کے لیے، ڈیٹا کو Redivis کی اپنی R اور Python کلائنٹ لائبریریوں کے ذریعے بھی حاصل کیا جا سکتا ہے۔ تفصیلات کے لیے [شروعات کی رہنمائی](/getstarted.qmd) دیکھیں۔

### ڈیٹا ڈاؤن لوڈ کرنے سے آگے

IRW پروجیکٹ میں یہ بھی شامل ہے:

- **[وگنیٹس (vignettes)](/vignettes/index.qmd)** کا ایک بڑھتا ہوا مجموعہ — تیار کردہ مثالیں جو کلاسیکی اور نئے پیمائشی طریقوں کو بہت سے IRW ڈیٹاسیٹس پر ایک ساتھ لاگو کرتی ہیں
- حقیقی ڈیٹا کے ساتھ نفسیاتی پیمائش سکھانے کے لیے **تربیتی وسائل اور مشقی سیٹس**
- ان محققین کے لیے ایک **شراکتی عمل** جو اپنے ڈیٹاسیٹس کو ویئر ہاؤس میں شامل کرنا چاہتے ہیں

## مزید جانیں

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- ویب سائٹ: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- کوڈ: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

اگر آپ اپنے کام میں IRW ڈیٹا استعمال کرتے ہیں تو براہ کرم اصل ڈیٹا کا حوالہ دیں (ہم نے اس کے لیے ضروری فعالیت فراہم کی ہے)۔ اگر آپ اوپر دیے گئے تعارفی مقالے کا بھی حوالہ دیں تو یہ ہمارے لیے نہایت قابلِ قدر ہوگا۔

---

*سوالات، رائے ہے، یا کسی ڈیٹاسیٹ میں تعاون کرنا چاہتے ہیں؟ [رابطہ صفحہ](/contact.qmd) ملاحظہ کریں یا [GitHub](https://github.com/itemresponsewarehouse) پر ایک issue کھولیں۔*
