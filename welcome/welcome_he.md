---
lang: he
dir: rtl
pagetitle: "מחסן תגובות לפריטים (Item Response Warehouse, IRW)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# מחסן תגובות לפריטים (Item Response Warehouse, IRW)

**אוסף חופשי ופתוח של נתוני תגובה לפריט (item response) מתואמים, למחקר בפסיכומטריקה ובמדידה.**

[קראו את המאמר](https://doi.org/10.3758/s13428-025-02796-y) **(גישה פתוחה)**

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

## למה IRW קיים

חוקרים העוסקים במדידה — בחינוך, בפסיכולוגיה ובתחומים קרובים — זקוקים לנתונים אמיתיים כדי לבחון ולהשוות בין השיטות שלהם. נתונים כאלה כבר קיימים בכמויות גדולות. אך הם מפוזרים על פני מחקרים רבים, מאוחסנים בפורמטים שונים רבים, ולעיתים קרובות קשה להשתמש בהם שוב בשל תיעוד או רישוי לא ברורים.

זו בעיה ידועה היטב. קהילת המחקר גיבשה תקן משותף לפתרון הבעיה הזו: נתונים צריכים להיות **FAIR** — ניתנים לאיתור (Findable), נגישים (Accessible), בעלי יכולת פעולה הדדית (Interoperable), וניתנים לשימוש חוזר (Reusable) (Wilkinson et al., 2016). תחומים אחרים יישמו את העקרונות הללו הלכה למעשה על ידי בניית משאבי נתונים משותפים ומתוקננים. במדעי המחשב, אוסף התמונות המתויגות ImageNet סיפק לחוקרים אמת מידה (benchmark) משותפת וסייע להניע קִדמה מהירה בתחום הבינה המלאכותית. גם הגנטיקה ומדעי המוח בנו משאבים משותפים דומים עבור הנתונים שלהם.

מחסן תגובות לפריטים (IRW) מיישם את אותם עקרונות FAIR על נתוני תגובה לפריט. הוא מאגד מאות מערכי נתונים קיימים ומעצב אותם מחדש לפורמט משותף אחד — כך ששיטה שנבחנה על מערך נתונים אחד יכולה להיבחן בקלות על מאות מערכי נתונים אחרים.

## מה נמצא ב-IRW

IRW מכיל **מאות מערכי נתונים** ("טבלאות"), וכל אחד מהם הוא אוסף של תגובות בודדות. תגובה נוצרת בכל פעם שאדם כלשהו (או יחידה אחרת) מגיב לפריט (item) (או לבדיקה אחרת). דוגמאות כוללות:

- תשובות תלמידים במבחני חינוך ויכולת
- פריטי שאלון המודדים אישיות או עמדות
- ציונים שניתנו על ידי מעריכים אנושיים
- כל מצב אחר הכולל תגובות חוזרות ונשנות למערך של כלי מדידה

כל מערך נתונים ב-IRW מתוכנן להיות:

- **ניתן לאיתור (Findable).** לכל מערך נתונים מצורפים מטא-נתונים שחושבו מראש — מספר המשתתפים, מספר הפריטים, צפיפות התגובות, תחום הנושא ותגיות תיאוריות נוספות — כך שניתן לאתר ולסנן מערכי נתונים מבלי להוריד אותם קודם.
- **נגיש (Accessible).** ניתן לקבל כל מערך נתונים דרך דפדפן האינטרנט או חבילת `irw`, באמצעות חשבון חינמי.
- **בעל יכולת פעולה הדדית (Interoperable).** כל מערך נתונים מעוצב מחדש לאותו מבנה פשוט (המתואר בהמשך), כך שאותו קוד ניתוח יכול לפעול על מערכי נתונים רבים בשינוי מועט או ללא שינוי כלל.
- **ניתן לשימוש חוזר (Reusable).** לכל מערך נתונים יש רישיון פתוח, מקורו מתועד, והקוד ששימש להמרתו לפורמט IRW הוא ציבורי.

מערכי הנתונים שונים מאוד בגודלם (ממאות בודדות של תגובות ועד מיליונים רבים) ובסוג התגובה (פריטים של כן/לא, דירוגים רב-קטגוריאליים, ציונים חלקיים, ועוד). כל מערך נתונים מגיע גם עם מטא-נתונים מחושבים מראש — מספר המשתתפים, מספר הפריטים, צפיפות התגובות, תחום הנושא, ותוויות תיאוריות נוספות — כך שחוקרים יכולים למצוא מערכי נתונים רלוונטיים מבלי להוריד ולעבד את כולם קודם.

## תקן הנתונים

<img src="/welcome/assets/diagram-cross-classification.svg" alt="תרשים המציג רשת צבעונית של תאי id-item, המומרת באמצעות חץ לטבלה בפורמט ארוך עם עמודות id, item ו-resp, כאשר צבעו של כל תא resp תואם לצבע התא המקורי שלו ברשת." class="welcome-figure">


כל מערך נתונים של IRW מעוצב מחדש לפורמט **ארוך (long format)**: שורה אחת לכל תגובה. לכל הפחות, כל שורה מכילה שלושה פרטי מידע:

| עמודה | משמעות |
|---|---|
| `id` | מי (או מה) הפיק את התגובה — בדרך כלל אדם |
| `item` | איזה כלי מדידה הפיק את התגובה — בדרך כלל שאלה או משימה |
| `resp` | התגובה עצמה, המאוחסנת כציון סדר (ordinal) |

**דוגמה:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

כאשר מערך נתונים כולל מידע נוסף — זמן תגובה, זהות המעריך, משתני עזר כמו גיל — מידע זה מאוחסן בעמודות נוספות בעלות שמות עקביים. מבנה פשוט אחד זה מכסה מגוון עצום של מצבי מדידה, וזה מה שמאפשר לכתוב קוד ניתוח פעם אחת וליישם אותו על פני כל המחסן.

המפרט הטכני המלא של התקן זמין בכתובת [itemresponsewarehouse.org/standard.html](/standard.qmd). קיימים גם תקנים קשורים ומתמחים יותר לטקסט פריטים, לנתוני השוואה זוגית (pairwise) ולתגובות נומינליות (קטגוריות ללא סדר).

## איך להשתמש בו

יש שלוש דרכים לקבל נתוני IRW, בהתאם לרמת האוטומציה הרצויה לך.

**1. עיון בדפדפן האינטרנט**
חִקרו מערכי נתונים ואת המטא-נתונים שלהם ישירות ב[דפדפן הנתונים של IRW](/data.qmd) — אין צורך בחשבון. הורדת מערך נתונים מלא דורשת חשבון [Redivis](https://redivis.com) חינמי, מכיוון שזו הפלטפורמה המארחת את הנתונים הבסיסיים.

**2. השתמשו בחבילת `irw` (מומלץ)**
חבילת `irw`, הזמינה הן עבור **R** והן עבור **Python**, מספקת פונקציות פשוטות למציאה, סינון והורדה של נתונים.

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

בפעם הראשונה שתשתמשו בחבילה, תתבקשו להתחבר באמצעות חשבון Redivis חינמי. לאחר מכן, שורת קוד אחת מורידה כל מערך נתונים ישירות ל-R או ל-Python. מנקודה זו, הנתונים מוכנים לניתוח באמצעות תוכנה סטנדרטית — לדוגמה חבילות תיאוריית תגובה לפריט (item response theory) או ניתוח גורמים.

**3. השתמשו ישירות בספריות הלקוח של Redivis**
עבור זרימות עבודה ברמה נמוכה יותר או שאינן מבוססות R/Python, ניתן לגשת לנתונים גם דרך ספריות הלקוח של Redivis עצמה עבור R ו-Python. לפרטים, ראו את [מדריך תחילת העבודה](/getstarted.qmd).

### מעבר להורדת נתונים

פרויקט IRW כולל גם:

- אוסף הולך וגדל של **[דוגמאות מפורטות (vignettes)](/vignettes/index.qmd)** — דוגמאות מעובדות המיישמות שיטות מדידה קלאסיות וחדשות על פני מערכי נתונים רבים של IRW בו-זמנית
- **משאבי הדרכה ומערכי תרגילים** ללימוד פסיכומטריקה עם נתונים אמיתיים
- **תהליך תרומה** עבור חוקרים המעוניינים להוסיף מערכי נתונים משלהם למחסן

## מידע נוסף

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- אתר אינטרנט: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- קוד: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

אם אתם משתמשים בנתוני IRW בעבודתכם, אנא צטטו את הנתונים המקוריים (סיפקנו לשם כך את הפונקציונליות הנדרשת). נשמח מאוד גם אם תצטטו את מאמר המבוא שלעיל.

---

*יש לכם שאלות, משוב, או שברצונכם לתרום מערך נתונים? בקרו ב[דף יצירת הקשר](/contact.qmd) או פתחו issue ב-[GitHub](https://github.com/itemresponsewarehouse).*
