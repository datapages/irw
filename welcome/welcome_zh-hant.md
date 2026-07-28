---
lang: zh-Hant
pagetitle: "Item Response Warehouse（IRW；項目反應資料倉儲）"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse（IRW；項目反應資料倉儲）

**一個免費、開放、經過統一格式化的項目反應資料集合，服務於心理計量學與測量研究。**

[閱讀論文](https://doi.org/10.3758/s13428-025-02796-y) **(開放取用)**

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

## IRW 存在的意義

研究測量問題的學者——無論在教育學、心理學還是相關領域——都需要真實資料來檢驗與比較他們的方法。這樣的資料其實早已大量存在，但它們分散在眾多研究中、儲存格式各異，並且常常因為文件不清楚或授權不明確而難以重複使用。

這是一個廣為人知的問題。學術界已提出解決此問題的共同準則：資料應具備 **FAIR** 特性——可發現（Findable）、可存取（Accessible）、可互通（Interoperable）與可再利用（Reusable）（Wilkinson et al., 2016）。其他學科正是透過落實這些原則，建立共享的、標準化的資料資源來解決這個問題。在電腦科學領域，帶標註的圖像集合 ImageNet 為研究者提供了共同的基準，推動了人工智慧的快速發展。遺傳學與神經科學也為各自的資料建立了類似的共享資源。

Item Response Warehouse（IRW）將同樣的 FAIR 原則應用於項目反應資料。它彙集了數百個既有資料集，將它們統一轉換為同一種通用格式——如此一來，在某個資料集上驗證過的方法，就能輕易地在數百個其他資料集上進行檢驗。

## IRW 包含哪些內容

IRW 包含**數百個資料集**（稱為「表」），每個資料集都是一組個別反應紀錄。每當某個人（或其他單位）對某個項目（或其他測量探針）作出反應時，就會產生一筆反應紀錄。例如：

- 學生在教育與能力測驗中的作答
- 測量人格或態度的調查項目
- 人類評分者給出的評分
- 任何其他涉及對一組測量探針重複作出反應的情境

IRW 中的每一個資料集都力求做到：

- **可發現（Findable）。** 每個資料集都附帶預先計算好的中繼資料——參與者人數、項目數量、反應密度、學科領域及其他描述性標籤——使資料集無需先下載即可被定位與篩選。
- **可存取（Accessible）。** 每個資料集都可以透過網頁瀏覽器或 `irw` 套件取得，只需一個免費帳戶。
- **可互通（Interoperable）。** 每個資料集都被轉換為相同的簡單結構（詳見下文），使得同一套分析程式碼可以在眾多資料集上執行，幾乎不需要修改。
- **可再利用（Reusable）。** 每個資料集都獲得開放授權，其來源均有文件記錄，將其轉換為 IRW 格式所使用的程式碼也是公開的。

各資料集在規模上差異很大（從數百筆反應紀錄到數百萬筆不等），在反應類型上也各不相同（是/否類項目、多類別評分、部分給分等）。每個資料集還附帶預先計算好的中繼資料——參與者人數、項目數量、反應密度、學科領域及其他描述性標籤——使研究者無需先下載並處理全部資料，即可找到相關資料集。

## 資料標準

<img src="/welcome/assets/diagram-cross-classification.svg" alt="示意圖:展示一個按 id 與 item 上色的網格,透過箭頭轉換為包含 id、item、resp 欄位的長格式表格,其中每個 resp 儲存格的顏色與網格中對應的來源儲存格顏色一致。" class="welcome-figure">


每個 IRW 資料集都被轉換為**長格式**：每列對應一筆反應紀錄。每列至少包含三項資訊：

| 欄位 | 意義 |
|---|---|
| `id` | 誰（或什麼）作出了該反應——通常是一個人 |
| `item` | 哪個測量探針引發了該反應——通常是一道題目或一項任務 |
| `resp` | 反應本身，以有序分數的形式儲存 |

**範例：**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

當某個資料集包含額外資訊——例如反應時間、評分者身分、年齡等共變數——這些資訊會儲存在額外的、命名方式一致的欄位中。這一種簡單結構涵蓋了極為廣泛的測量情境，這也正是能夠只撰寫一次分析程式碼、便可套用於整個資料倉儲的原因。

該標準的完整技術規範可在 [itemresponsewarehouse.org/standard.html](/standard.qmd) 查閱。此外還有更專門的標準，分別針對項目文字、兩兩競爭資料以及名義（無序類別）反應資料。

## 如何使用

取得 IRW 資料有三種方式，可依你希望自動化的程度來選擇。

**1. 在網頁瀏覽器中瀏覽**
直接在 [IRW 資料瀏覽器](/data.qmd) 中探索資料集及其中繼資料——無需帳號。若要下載完整資料集，則需要一個免費的 [Redivis](https://redivis.com) 帳號，因為該平台是承載底層資料的平台。

**2. 使用 `irw` 套件（建議）**
`irw` 套件同時提供 **R** 與 **Python** 版本，提供了簡單的函式用於尋找、篩選與下載資料。

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

首次使用該套件時，系統會要求你使用免費的 Redivis 帳號登入。此後，只需一行程式碼即可將任意資料集直接下載到 R 或 Python 中。此時，資料便已可以使用標準軟體進行分析——例如項目反應理論或因素分析相關套件。

**3. 直接使用 Redivis 的用戶端函式庫**
對於更底層的工作流程，或不使用 R/Python 的情境，也可以透過 Redivis 自身提供的 R 與 Python 用戶端函式庫存取資料。詳見 [入門指南](/getstarted.qmd)。

### 不僅僅是下載資料

IRW 專案還包括：

- 一套不斷增長的 **[範例集（vignettes）](/vignettes/index.qmd)**——展示如何將經典及新興的測量方法同時應用於多個 IRW 資料集的實例
- **訓練資源與練習題**，用於以真實資料教授心理計量學
- **資料貢獻流程**，供希望將自己的資料集加入該資料倉儲的研究者使用

## 了解更多

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- 網站：[itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- 程式碼：[github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

如果你在工作中使用了 IRW 的資料，請引用原始資料（我們已提供相應的引用功能）。同時，我們也非常歡迎你引用上面提到的介紹性論文。

---

*有問題、意見回饋，或想貢獻一個資料集？請造訪 [聯絡我們頁面](/contact.qmd)，或在 [GitHub](https://github.com/itemresponsewarehouse) 上提交 issue。*
