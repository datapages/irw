---
lang: zh
pagetitle: "Item Response Warehouse（IRW；项目反应数据库）"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse（IRW；项目反应数据库）

**一个免费、开放的、经过统一格式化的项目反应数据集合，服务于心理测量学与测量学研究。**

[阅读论文](https://doi.org/10.3758/s13428-025-02796-y) **(开放获取)**

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

## IRW 存在的意义

研究测量问题的学者——无论在教育学、心理学还是相关领域——都需要真实数据来检验和比较他们的方法。这样的数据其实早已大量存在，但它们分散在众多研究中，存储格式各异，并且常常因为文档不清晰或授权不明确而难以重复使用。

这是一个众所周知的问题。学术界已经提出了解决这一问题的共同准则：数据应当具备 **FAIR** 特性——可发现（Findable）、可访问（Accessible）、可互操作（Interoperable）和可重用（Reusable）（Wilkinson et al., 2016）。其他学科正是通过践行这些原则,建立共享的、标准化的数据资源来解决这个问题的。在计算机科学领域，带标注的图像集合 ImageNet 为研究者提供了一个共同的基准,推动了人工智能的快速发展。遗传学和神经科学也为各自的数据建立了类似的共享资源。

Item Response Warehouse（IRW）将同样的 FAIR 原则应用于项目反应数据。它汇集了数百个已有数据集，将它们统一转换为同一种通用格式——这样一来，在某个数据集上验证过的方法，就能轻松地在数百个其他数据集上进行检验。

## IRW 包含哪些内容

IRW 包含**数百个数据集**（称为"表"），每个数据集都是一组个体反应记录。每当某个人（或其他单位）对某个项目（或其他测量探针）作出反应时，就会产生一条反应记录。例如：

- 学生在教育及能力测验中的作答
- 测量人格或态度的调查项目
- 人类评分者给出的评分
- 任何其他涉及对一组测量探针重复作出反应的情境

IRW 中的每一个数据集都力求做到：

- **可发现（Findable）。** 每个数据集都附带预先计算好的元数据——参与者人数、项目数量、反应密度、学科领域及其他描述性标签——使数据集无需先下载即可被定位和筛选。
- **可访问（Accessible）。** 每个数据集都可以通过网页浏览器或 `irw` 包获取，只需一个免费账户。
- **可互操作（Interoperable）。** 每个数据集都被转换为相同的简单结构（详见下文），使得同一套分析代码可以在众多数据集上运行，几乎不需要修改。
- **可重用（Reusable）。** 每个数据集都获得开放授权，其来源均有文档记录，将其转换为 IRW 格式所使用的代码也是公开的。

各数据集在规模上差异很大（从数百条反应记录到数百万条不等），在反应类型上也各不相同（是/否类项目、多类别评分、部分得分等）。每个数据集还附带预先计算好的元数据——参与者人数、项目数量、反应密度、学科领域及其他描述性标签——使研究者无需先下载并处理全部数据,即可找到相关数据集。

## 数据标准

<img src="/welcome/assets/diagram-cross-classification.svg" alt="示意图:展示一个按 id 与 item 着色的网格,通过箭头转换为包含 id、item、resp 列的长格式表格,其中每个 resp 单元格的颜色与网格中对应的源单元格颜色一致。" class="welcome-figure">


每个 IRW 数据集都被转换为**长格式**：每行对应一条反应记录。每行至少包含三项信息：

| 列 | 含义 |
|---|---|
| `id` | 谁（或什么）作出了该反应——通常是一个人 |
| `item` | 哪个测量探针引发了该反应——通常是一道题目或一项任务 |
| `resp` | 反应本身，以有序分数的形式存储 |

**示例：**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

当某个数据集包含额外信息——例如反应时间、评分者身份、年龄等协变量——这些信息会存储在额外的、命名方式一致的列中。这一种简单结构涵盖了极为广泛的测量情境,这也正是能够只编写一次分析代码、便可应用于整个数据库的原因。

该标准的完整技术规范可在 [itemresponsewarehouse.org/standard.html](/standard.qmd) 查阅。此外还有更专门的标准,分别针对项目文本、两两竞争数据以及名义（无序类别）反应数据。

## 如何使用

获取 IRW 数据有三种方式,可根据你希望自动化的程度进行选择。

**1. 在网页浏览器中浏览**
直接在 [IRW 数据浏览器](/data.qmd) 中探索数据集及其元数据——无需账号。若要下载完整数据集,则需要一个免费的 [Redivis](https://redivis.com) 账号,因为该平台是承载底层数据的平台。

**2. 使用 `irw` 软件包（推荐）**
`irw` 软件包同时提供 **R** 和 **Python** 版本,提供了简单的函数用于查找、筛选和下载数据。

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

首次使用该软件包时,系统会要求你使用免费的 Redivis 账号登录。此后,只需一行代码即可将任意数据集直接下载到 R 或 Python 中。此时,数据便已可以使用标准软件进行分析——例如项目反应理论或因子分析相关软件包。

**3. 直接使用 Redivis 的客户端库**
对于更底层的工作流程,或不使用 R/Python 的场景,也可以通过 Redivis 自身提供的 R 和 Python 客户端库访问数据。详见 [入门指南](/getstarted.qmd)。

### 不仅仅是下载数据

IRW 项目还包括:

- 一套不断增长的 **[范例集（vignettes）](/vignettes/index.qmd)** ——展示如何将经典及新兴的测量方法同时应用于多个 IRW 数据集的实例
- **培训资源与练习题**,用于以真实数据教授心理测量学
- **数据贡献流程**,供希望将自己的数据集加入该数据库的研究者使用

## 了解更多

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- 网站：[itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- 代码：[github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

如果你在工作中使用了 IRW 的数据,请引用原始数据（我们已提供相应的引用功能）。同时,我们也非常欢迎你引用上面提到的介绍性论文。

---

*有问题、反馈,或想贡献一个数据集？请访问 [联系我们页面](/contact.qmd),或在 [GitHub](https://github.com/itemresponsewarehouse) 上提交 issue。*
