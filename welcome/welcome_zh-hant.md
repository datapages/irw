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

{{< include /welcome/_lang_switch.md >}}

---

## IRW 存在的意義

研究測量問題的學者——無論在教育學、心理學還是相關領域——都需要真實資料來檢驗與比較他們的方法。這樣的資料其實早已大量存在，但它們分散在眾多研究中、儲存格式各異，並且常常因為文件不清楚或授權不明確而難以重複使用。

這是一個廣為人知的問題。學術界已提出解決此問題的共同準則：資料應具備 **FAIR** 特性——可發現（Findable）、可存取（Accessible）、可互通（Interoperable）與可再利用（Reusable）（Wilkinson et al., 2016）。其他學科正是透過落實這些原則，建立共享的、標準化的資料資源來解決這個問題。在電腦科學領域，帶標註的圖像集合 ImageNet 為研究者提供了共同的基準，推動了人工智慧的快速發展。遺傳學與神經科學也為各自的資料建立了類似的共享資源。

Item Response Warehouse（IRW）將同樣的 FAIR 原則應用於項目反應資料。它彙集了數千個既有資料集，將它們統一轉換為同一種通用格式——如此一來，在某個資料集上驗證過的方法，就能輕易地在數千個其他資料集上進行檢驗。

## IRW 包含哪些內容

IRW 包含**數千個資料集**（稱為「表」），每個資料集都是一組個別反應紀錄。每當某個人（或其他單位）遇到一個測量探針並記錄下結果時，就會產生一筆反應紀錄。什麼算作探針取決於[資料族](/overview.qmd)：單獨作答的項目、在對戰中遇到的另一個主體、需要排序的一整組選項，或者由隨機分配的屬性組成的一份檔案。例如：

- 學生在教育與能力測驗中的作答
- 測量人格或態度的調查項目
- 人類評分者給出的評分
- 比賽、對局與直接比較，其中兩個主體交鋒、一方獲勝
- 偏好投票的選票、評審團排名以及問卷中的排序題
- 聯合分析實驗，受訪者在隨機生成的檔案之間做出選擇
- 任何其他涉及對一組測量探針重複作出反應的情境

IRW 中的每一個資料集都力求做到：

- **可發現（Findable）。** 每個資料集都附帶預先計算好的中繼資料——參與者人數、項目數量、反應密度、學科領域及其他描述性標籤——使資料集無需先下載即可被定位與篩選。
- **可存取（Accessible）。** 每個資料集都可以透過網頁瀏覽器或 `irw` 套件取得，只需一個免費帳戶。
- **可互通（Interoperable）。** 每個資料集都被轉換為長格式，每筆反應佔一列，並遵循其所屬資料族的標準（詳見下文），使得同一套分析程式碼可以在眾多資料集上執行，幾乎不需要修改。
- **可再利用（Reusable）。** 每個資料集都獲得開放授權，其來源均有文件記錄，將其轉換為 IRW 格式所使用的程式碼也是公開的。

各資料集在規模上差異很大（從數百筆反應紀錄到數百萬筆不等），在反應類型上也各不相同（是/否類項目、多類別評分、部分給分、無序類別、對戰結果、排名位次等）。

## 資料標準

每一張 IRW 表都採用**長格式**：每筆反應佔一列。一列具體*代表什麼*，取決於該表所屬的資料族，承載它的欄位也是如此。

<img src="/figures/families/families-tree.svg" alt="將資料集歸入某個 IRW 資料族的兩個問題。"
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| 資料族 | `source =` | 標準 |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn 未列入圖中：該資料族由資料的來源而非形式界定，因此一張模擬表可以具有其中任一種形式。各資料族的欄位請見[標準概覽](/overview.qmd)。


## 如何使用

取得 IRW 資料有三種方式，可依你希望自動化的程度來選擇。

**1. 在網頁瀏覽器中瀏覽**
直接在 [IRW 資料瀏覽器](/data.qmd) 中探索資料集及其中繼資料——無需帳號。若要下載完整資料集，則需要一個免費的 [Redivis](https://redivis.com) 帳號，因為該平台是承載底層資料的平台。

**2. 使用 `irw` 套件（建議）**
`irw` 套件同時提供 **R** 與 **Python** 版本。

```r
# R
install.packages("irw")
library(irw)

irw_info()                     # overview of the IRW
irw_list_tables()              # list all available tables
irw_filter(var = "rt")         # find tables that include response times
df <- irw_fetch("4thgrade_math_sirt")   # download one table

# Other families are the same call with a source: comp, nom, conj, rank, sim
g <- irw_fetch("lichess_2013_blitz", source = "comp")
```

```python
# Python
# pip install irw
import irw

irw.info()
irw.list_tables()
irw.filter(var="rt")
df = irw.fetch("4thgrade_math_sirt")

# Other families are the same call with a source: comp, nom, conj, rank, sim
g = irw.fetch("lichess_2013_blitz", source="comp")
```

首次使用該套件時，系統會要求你使用免費的 Redivis 帳號登入。

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
