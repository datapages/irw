---
lang: zh
pagetitle: "Item Response Warehouse（IRW；项目反应数据仓库）"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher link line below (English · Français · ...) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate or reorder it.
-->

# Item Response Warehouse（IRW；项目反应数据仓库）

**一个免费、开放的、经过统一格式化的项目反应数据集合，服务于心理测量学与测量学研究。**

[itemresponsewarehouse.org](https://itemresponsewarehouse.org) · [GitHub](https://github.com/itemresponsewarehouse) · [阅读论文](https://doi.org/10.3758/s13428-025-02796-y)

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html) · [繁體中文](/welcome/welcome_zh-hant.html) · [বাংলা](/welcome/welcome_bn.html) · [Türkçe](/welcome/welcome_tr.html) · [Tiếng Việt](/welcome/welcome_vi.html)

---

## IRW 存在的意义

研究测量问题的学者——无论在教育学、心理学还是相关领域——都需要真实数据来检验和比较他们的方法。这样的数据其实早已大量存在，但它们分散在众多研究中，存储格式各异，并且常常因为文档不清晰或授权不明确而难以重复使用。

这是一个众所周知的问题。其他学科通过建立共享的、标准化的数据资源解决了这个问题。在计算机科学领域，带标注的图像集合 ImageNet 为研究者提供了一个共同的基准,推动了人工智能的快速发展。遗传学和神经科学也为各自的数据建立了类似的共享资源。

Item Response Warehouse（IRW）为项目反应数据做了同样的事情。它汇集了数百个已有数据集，将它们统一转换为同一种通用格式——这样一来，在某个数据集上验证过的方法，就能轻松地在数百个其他数据集上进行检验。

## IRW 包含哪些内容

IRW 包含**数百个数据集**（称为"表"），每个数据集都是一组个体反应记录。每当某个人（或其他单位）对某个项目（或其他测量探针）作出反应时，就会产生一条反应记录。例如：

- 学生在教育及能力测验中的作答
- 测量人格或态度的调查项目
- 人类评分者给出的评分
- 任何其他涉及对一组测量探针重复作出反应的情境

关于 IRW 中的每一个数据集，以下两点始终成立：

- **开放。** 每个数据集都获得了可供重复使用的授权。其来源均有文档记录，将其转换为 IRW 格式所使用的代码也是公开的。
- **统一格式化。** 每个数据集都被转换为相同的简单结构（详见下文），使得同一套分析代码可以在众多数据集上运行，几乎不需要修改。

各数据集在规模上差异很大（从数百条反应记录到数百万条不等），在反应类型上也各不相同（是/否类项目、多类别评分、部分得分等）。每个数据集还附带预先计算好的元数据——参与者人数、项目数量、反应密度、学科领域及其他描述性标签——使研究者无需先下载并处理全部数据,即可找到相关数据集。

## 数据标准

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

当某个数据集包含额外信息——例如反应时间、评分者身份、年龄等协变量——这些信息会存储在额外的、命名方式一致的列中。这一种简单结构涵盖了极为广泛的测量情境,这也正是能够只编写一次分析代码、便可应用于整个数据仓库的原因。

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
- **数据贡献流程**,供希望将自己的数据集加入该数据仓库的研究者使用

## 了解更多

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- 网站：[itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- 代码：[github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

如果你在工作中使用了 IRW 的数据,请引用原始数据（我们已提供相应的引用功能）。同时,我们也非常欢迎你引用上面提到的介绍性论文。

---

*有问题、反馈,或想贡献一个数据集？请访问 [联系我们页面](/contact.qmd),或在 [GitHub](https://github.com/itemresponsewarehouse) 上提交 issue。*
