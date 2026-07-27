---
lang: ja
pagetitle: "Item Response Warehouse（IRW；項目反応データウェアハウス）"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse（IRW；項目反応データウェアハウス）

**心理測定学および測定研究のための、無料で公開された統一形式の項目反応データ集です。**

[論文を読む](https://doi.org/10.3758/s13428-025-02796-y) **(オープンアクセス)**

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

## IRW が存在する理由

教育学、心理学、および関連分野で測定を研究する研究者は、自らの手法を検証し比較するために実データを必要としています。そのようなデータはすでに大量に存在します。しかし、それらは多くの研究に分散しており、さまざまな形式で保存されており、文書化やライセンスが不明確なために再利用が難しいことがよくあります。

これはよく知られた問題です。他の分野では、共有された標準化データ資源を構築することでこの問題を解決してきました。コンピュータサイエンスの分野では、ラベル付き画像の集合である ImageNet が研究者に共通のベンチマークを提供し、人工知能の急速な発展を後押ししました。遺伝学や神経科学も、それぞれのデータのために同様の共有資源を構築しています。

Item Response Warehouse（IRW）は、項目反応データについて同じことを行います。数百の既存データセットを集約し、それらを一つの共通形式に再構成することで、あるデータセットで検証された手法を、他の数百のデータセットでも簡単に検証できるようにします。

## IRW に含まれるもの

IRW には**数百のデータセット**(「テーブル」)が含まれており、それぞれが個々の反応の集合です。反応は、ある人(または他の単位)がある項目(または他の測定プローブ)に応答するたびに生成されます。例えば:

- 教育・能力検査における学生の解答
- 性格や態度を測定する調査項目
- 人間の評価者による評定
- 一連の測定プローブに対して反復的に反応が行われる、その他あらゆる状況

IRW に含まれるすべてのデータセットについて、次の2点が当てはまります:

- **オープン。** 各データセットは再利用可能なライセンスの下にあります。その出所は文書化されており、IRW 形式へ変換するために使用されたコードも公開されています。
- **統一化。** 各データセットは同じ単純な構造(以下で説明)に再構成されており、同じ分析コードをほとんど、あるいはまったく変更せずに多くのデータセットで実行できます。

データセットは規模(数百件の反応から数百万件まで)や反応の種類(はい/いいえ形式の項目、多カテゴリ評定、部分点方式のスコアなど)において大きく異なります。各データセットには、参加者数、項目数、反応密度、主題分野、その他の記述的タグといった事前計算済みのメタデータも付属しており、研究者はすべてのデータセットをダウンロードして処理する前に、関連するデータセットを見つけることができます。

## データ標準

<img src="/welcome/assets/diagram-cross-classification.svg" alt="id と item のセルからなる色分けされたグリッドが、矢印によって id・item・resp 列を持つロング形式の表に変換される様子を示す図。各 resp セルの色は、グリッド内の元となったセルの色と一致する。" class="welcome-figure">


すべての IRW データセットは**ロング形式(long format)**に再構成されます:1反応につき1行です。各行には少なくとも次の3つの情報が含まれます:

| 列 | 意味 |
|---|---|
| `id` | 誰(または何)が反応を生成したか — 通常は人 |
| `item` | どの測定プローブが反応を生成したか — 通常は質問や課題 |
| `resp` | 反応そのもの。順序尺度のスコアとして保存される |

**例:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

データセットに反応時間、評価者の識別情報、年齢などの共変量といった追加情報が含まれる場合、その情報は一貫した命名規則に従った追加の列に保存されます。この一つの単純な構造は非常に幅広い測定状況をカバーしており、分析コードを一度書くだけでウェアハウス全体に適用できるのはそのためです。

この標準に関する完全な技術仕様は [itemresponsewarehouse.org/standard.html](/standard.qmd) で公開されています。項目テキスト、ペア比較(競合)データ、名義尺度(順序のないカテゴリ)反応についても、より専門的な標準が別途存在します。

## 使い方

自動化をどの程度望むかに応じて、IRW データを取得する方法は3つあります。

**1. Webブラウザで閲覧する**
[IRW データブラウザ](/data.qmd) で、アカウント不要でデータセットとそのメタデータを直接探索できます。完全なデータセットをダウンロードするには、無料の [Redivis](https://redivis.com) アカウントが必要です。これは、Redivis が基盤となるデータをホストしているプラットフォームだからです。

**2. `irw` パッケージを使う(推奨)**
`irw` パッケージは **R** と **Python** の両方で利用可能で、データの検索・フィルタリング・ダウンロードのための簡単な関数を提供します。

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

パッケージを初めて使用する際には、無料の Redivis アカウントでログインするよう求められます。その後は、コード1行で任意のデータセットを R または Python に直接ダウンロードできます。それ以降、データは標準的なソフトウェア(たとえば項目反応理論や因子分析のパッケージ)で分析できる状態になっています。

**3. Redivis のクライアントライブラリを直接使う**
より低レベルなワークフローや R/Python 以外の環境では、Redivis 自身が提供する R および Python クライアントライブラリを通じてもデータにアクセスできます。詳細は [はじめにガイド](/getstarted.qmd) を参照してください。

### データのダウンロード以外にも

IRW プロジェクトには次のようなものも含まれています:

- 拡大を続ける **[ビネット(vignettes)](/vignettes/index.qmd)** 集 — 古典的および新しい測定手法を、多数の IRW データセットに同時に適用する実践例
- 実データを用いて心理測定学を教えるための**トレーニング資料と演習問題**
- 自身のデータセットをウェアハウスに追加したい研究者のための**貢献プロセス**

## さらに詳しく

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- ウェブサイト: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- コード: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

研究で IRW のデータを使用される場合は、元データを引用してください(そのための機能を用意しています)。上記の紹介論文も併せて引用いただければ幸いです。

---

*ご質問やご意見がありますか?データセットの提供をご希望ですか? [お問い合わせページ](/contact.qmd) をご覧いただくか、[GitHub](https://github.com/itemresponsewarehouse) で issue を作成してください。*
