---
lang: ru
pagetitle: "Item Response Warehouse (IRW; Хранилище данных об ответах на задания)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Хранилище данных об ответах на задания)

**Бесплатная, открытая коллекция унифицированных данных об ответах на задания для исследований в области психометрики и измерений.**

[Читать статью](https://doi.org/10.3758/s13428-025-02796-y) **(открытый доступ)**

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

## Зачем существует IRW

Исследователям, изучающим измерения — в сфере образования, психологии и смежных областях, — нужны реальные данные для проверки и сравнения своих методов. Такие данные уже существуют в большом количестве. Но они разбросаны по множеству исследований, хранятся в самых разных форматах и зачастую с трудом поддаются повторному использованию из-за неясной документации или лицензирования.

Это хорошо известная проблема. Другие области решили её, создав общие, стандартизированные ресурсы данных. В компьютерных науках коллекция размеченных изображений ImageNet дала исследователям общий ориентир и способствовала стремительному прогрессу в области искусственного интеллекта. Генетика и нейронауки создали похожие общие ресурсы для собственных данных.

Item Response Warehouse (IRW) делает то же самое для данных об ответах на задания. Он объединяет сотни существующих наборов данных и преобразует их в единый общий формат — так что метод, проверенный на одном наборе данных, можно легко проверить на сотнях других.

## Что содержится в IRW

IRW содержит **сотни наборов данных** («таблиц»), каждый из которых представляет собой коллекцию отдельных ответов. Ответ создаётся всякий раз, когда какой-либо человек (или иная единица) реагирует на задание (или иной измерительный стимул). Примеры включают:

- Ответы учащихся на образовательные тесты и тесты способностей
- Пункты опросников, измеряющие личностные черты или установки
- Оценки, выставленные экспертами-людьми
- Любые другие ситуации, где присутствуют повторяющиеся ответы на набор измерительных стимулов

Для каждого набора данных в IRW верны два утверждения:

- **Открытость.** Каждый набор данных лицензирован для повторного использования. Его происхождение документировано, а код, использованный для преобразования в формат IRW, доступен публично.
- **Унифицированность.** Каждый набор данных преобразован в одну и ту же простую структуру (описана ниже), поэтому один и тот же код анализа можно применять к множеству наборов данных с минимальными изменениями или вовсе без них.

Наборы данных сильно различаются по объёму (от нескольких сотен ответов до нескольких миллионов) и по типу ответа (задания «да/нет», многокатегориальные оценки, баллы за частичное выполнение и другое). К каждому набору данных также прилагаются заранее рассчитанные метаданные — число участников, число заданий, плотность ответов, предметная область и другие описательные теги, — чтобы исследователи могли находить нужные наборы данных, не скачивая и не обрабатывая все подряд.

## Стандарт данных

<img src="/welcome/assets/diagram-cross-classification.svg" alt="Диаграмма, показывающая цветную сетку ячеек id-item, преобразованную с помощью стрелки в таблицу в длинном формате со столбцами id, item и resp, где цвет каждой ячейки resp соответствует цвету исходной ячейки в сетке." class="welcome-figure">


Каждый набор данных IRW преобразуется в **длинный формат (long format)**: одна строка на один ответ. Как минимум, каждая строка содержит три элемента информации:

| Столбец | Значение |
|---|---|
| `id` | Кто (или что) дал ответ — как правило, человек |
| `item` | Какой измерительный стимул вызвал ответ — как правило, вопрос или задача |
| `resp` | Сам ответ, сохранённый в виде порядкового балла |

**Пример:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Если набор данных содержит дополнительную информацию — время ответа, идентификатор эксперта, ковариаты, такие как возраст, — эта информация хранится в дополнительных, единообразно названных столбцах. Эта единая простая структура охватывает чрезвычайно широкий спектр измерительных ситуаций, что и позволяет написать код анализа один раз и применять его ко всему хранилищу.

Полная техническая спецификация стандарта доступна на странице [itemresponsewarehouse.org/standard.html](/standard.qmd). Также существуют более специализированные стандарты для текста заданий, данных попарных сравнений (соревнований) и номинальных (неупорядоченных категориальных) ответов.

## Как это использовать

Есть три способа получить данные IRW — в зависимости от того, насколько высокую степень автоматизации вы хотите.

**1. Просмотр в веб-браузере**
Изучайте наборы данных и их метаданные прямо в [браузере данных IRW](/data.qmd) — учётная запись не требуется. Для скачивания полного набора данных требуется бесплатная учётная запись [Redivis](https://redivis.com), поскольку именно эта платформа размещает исходные данные.

**2. Использование пакета `irw` (рекомендуется)**
Пакет `irw`, доступный как для **R**, так и для **Python**, предоставляет простые функции для поиска, фильтрации и скачивания данных.

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

При первом использовании пакета вам будет предложено войти с помощью бесплатной учётной записи Redivis. После этого одна строка кода позволяет скачать любой набор данных прямо в R или Python. После этого данные готовы к анализу с помощью стандартного программного обеспечения — например, пакетов теории ответа на задание или факторного анализа.

**3. Прямое использование клиентских библиотек Redivis**
Для рабочих процессов более низкого уровня или вне R/Python данные также можно получить через собственные клиентские библиотеки Redivis для R и Python. Подробности см. в [руководстве по началу работы](/getstarted.qmd).

### Не только скачивание данных

Проект IRW также включает:

- Постоянно растущий набор **[иллюстративных разборов (vignettes)](/vignettes/index.qmd)** — подробные примеры применения классических и новых методов измерения сразу на многих наборах данных IRW
- **Учебные материалы и наборы упражнений** для преподавания психометрики на реальных данных
- **Процесс подачи данных** для исследователей, желающих добавить собственные наборы данных в хранилище

## Узнать больше

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Сайт: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Код: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Если вы используете данные IRW в своей работе, пожалуйста, указывайте ссылку на исходные данные (мы предоставили для этого соответствующую функцию). Также будет очень хорошо, если вы процитируете вводную статью, упомянутую выше.

---

*Есть вопросы, отзывы, или хотите предложить набор данных? Посетите [страницу контактов](/contact.qmd) или создайте issue на [GitHub](https://github.com/itemresponsewarehouse).*
