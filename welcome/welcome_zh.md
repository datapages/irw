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

{{< include /welcome/_lang_switch.md >}}

---

## IRW 存在的意义

研究测量问题的学者——无论在教育学、心理学还是相关领域——都需要真实数据来检验和比较他们的方法。这样的数据其实早已大量存在，但它们分散在众多研究中，存储格式各异，并且常常因为文档不清晰或授权不明确而难以重复使用。

这是一个众所周知的问题。学术界已经提出了解决这一问题的共同准则：数据应当具备 **FAIR** 特性——可发现（Findable）、可访问（Accessible）、可互操作（Interoperable）和可重用（Reusable）（Wilkinson et al., 2016）。其他学科正是通过践行这些原则,建立共享的、标准化的数据资源来解决这个问题的。在计算机科学领域，带标注的图像集合 ImageNet 为研究者提供了一个共同的基准,推动了人工智能的快速发展。遗传学和神经科学也为各自的数据建立了类似的共享资源。

Item Response Warehouse（IRW）将同样的 FAIR 原则应用于项目反应数据。它汇集了数千个已有数据集，将它们统一转换为同一种通用格式——这样一来，在某个数据集上验证过的方法，就能轻松地在数千个其他数据集上进行检验。

## IRW 包含哪些内容

IRW 包含**数千个数据集**（称为"表"），每个数据集都是一组个体反应记录。每当某个人（或其他单位）遇到一个测量探针并记录下结果时，就会产生一条反应记录。什么算作探针取决于[数据族](/overview.qmd)：单独作答的项目、在对战中遇到的另一个主体、需要排序的一整组选项，或者由随机分配的属性组成的一份档案。例如：

- 学生在教育及能力测验中的作答
- 测量人格或态度的调查项目
- 人类评分者给出的评分
- 比赛、对局与直接比较，其中两个主体交锋、一方获胜
- 偏好投票的选票、评审团排名以及问卷中的排序题
- 联合分析实验，受访者在随机生成的档案之间做出选择
- 任何其他涉及对一组测量探针重复作出反应的情境

IRW 中的每一个数据集都力求做到：

- **可发现（Findable）。** 每个数据集都附带预先计算好的元数据——参与者人数、项目数量、反应密度、学科领域及其他描述性标签——使数据集无需先下载即可被定位和筛选。
- **可访问（Accessible）。** 每个数据集都可以通过网页浏览器或 `irw` 包获取，只需一个免费账户。
- **可互操作（Interoperable）。** 每个数据集都被转换为长格式，每条反应占一行，并遵循其所属数据族的标准（详见下文），使得同一套分析代码可以在众多数据集上运行，几乎不需要修改。
- **可重用（Reusable）。** 每个数据集都获得开放授权，其来源均有文档记录，将其转换为 IRW 格式所使用的代码也是公开的。

各数据集在规模上差异很大（从数百条反应记录到数百万条不等），在反应类型上也各不相同（是/否类项目、多类别评分、部分得分、无序类别、对战结果、排名位次等）。

## 数据标准

每一张 IRW 表都采用**长格式**：每条反应占一行。一行具体*代表什么*，取决于该表所属的数据族，承载它的列也是如此。

<img src="/figures/families/families-tree.svg" alt="将数据集归入某个 IRW 数据族的两个问题。"
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| 数据族 | `source =` | 标准 |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn 未列入图中：该数据族由数据的来源而非形式界定，因此一张模拟表可以具有其中任一种形式。各数据族的列请见[标准概览](/overview.qmd)。


## 如何使用

获取 IRW 数据有三种方式,可根据你希望自动化的程度进行选择。

**1. 在网页浏览器中浏览**
直接在 [IRW 数据浏览器](/data.qmd) 中探索数据集及其元数据——无需账号。若要下载完整数据集,则需要一个免费的 [Redivis](https://redivis.com) 账号,因为该平台是承载底层数据的平台。

**2. 使用 `irw` 软件包（推荐）**
`irw` 软件包同时提供 **R** 和 **Python** 版本。

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

首次使用该软件包时,系统会要求你使用免费的 Redivis 账号登录。

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
