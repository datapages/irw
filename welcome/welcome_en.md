---
lang: en
pagetitle: "The Item Response Warehouse"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher link line below (English · Français · ...) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate or reorder it.
-->

# The Item Response Warehouse

**A free, open collection of harmonized item response data for psychometrics and measurement research.**

[itemresponsewarehouse.org](https://itemresponsewarehouse.org) · [GitHub](https://github.com/itemresponsewarehouse) · [Read the paper](https://doi.org/10.3758/s13428-025-02796-y)

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html) · [繁體中文](/welcome/welcome_zh-hant.html) · [বাংলা](/welcome/welcome_bn.html) · [Türkçe](/welcome/welcome_tr.html) · [Tiếng Việt](/welcome/welcome_vi.html)

---

## Why the IRW exists

Researchers who study measurement — in education, psychology, and related fields — need real data to test and compare their methods. That data already exists in large quantities. But it is scattered across many studies, stored in many different formats, and often hard to reuse because of unclear documentation or licensing.

This is a well-known problem. Other fields solved it by building shared, standardized data resources. In computer science, the ImageNet collection of labeled images gave researchers a common benchmark and helped drive rapid progress in AI. Genetics and neuroscience built similar shared resources for their own data.

The Item Response Warehouse (IRW) does the same thing for item response data. It brings together hundreds of existing datasets and reshapes them into one common format — so that a method tested on one dataset can easily be tested on hundreds of others.

## What is in the IRW

The IRW contains **hundreds of datasets** ("tables"), each one a collection of individual responses. A response is generated whenever some person (or other unit) responds to some item (or other probe). Examples include:

- Student answers on education and ability tests
- Survey items measuring personality or attitudes
- Ratings assigned by human raters
- Any other setting involving repeated responses to a set of measurement probes

Two things are true of every dataset in the IRW:

- **Open.** Each dataset is licensed for reuse. Its origin is documented, and the code used to convert it into the IRW format is public.
- **Harmonized.** Each dataset is reshaped into the same simple structure (described below), so that the same analysis code can run across many datasets with little or no modification.

Datasets vary widely in size (from a few hundred responses to many millions) and in response type (yes/no items, multi-category ratings, partial-credit scores, and more). Each dataset also comes with pre-computed metadata — number of participants, number of items, response density, subject area, and other descriptive tags — so that researchers can find relevant datasets without first downloading and processing all of them.

## The data standard

Every IRW dataset is reshaped into **long format**: one row per response. At minimum, each row has three pieces of information:

| Column | Meaning |
|---|---|
| `id` | Who (or what) produced the response — typically a person |
| `item` | Which measurement probe produced the response — typically a question or task |
| `resp` | The response itself, stored as an ordinal score |

**Example:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

When a dataset includes extra information — response time, rater identity, covariates such as age — that information is stored in additional, consistently named columns. This one simple structure covers a huge range of measurement situations, which is what makes it possible to write analysis code once and apply it across the entire warehouse.

The full technical specification of the standard is available at [itemresponsewarehouse.org/standard.html](/standard.qmd). Related, more specialized standards also exist for item text, pairwise-competition data, and nominal (unordered category) responses.

## How to use it

There are three ways to get IRW data, depending on how much you want to automate.

**1. Browse in the web browser**
Explore datasets and their metadata directly on the [IRW data browser](/data.qmd) — no account needed. Downloading a full dataset requires a free [Redivis](https://redivis.com) account, since that is the platform that hosts the underlying data.

**2. Use the `irw` package (recommended)**
The `irw` package, available for both **R** and **Python**, gives simple functions for finding, filtering, and downloading data.

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

The first time you use the package, you will be asked to log in with a free Redivis account. After that, one line of code downloads any dataset directly into R or Python. From there, the data is ready for analysis with standard software — for example, item response theory or factor analysis packages.

**3. Use the Redivis client libraries directly**
For lower-level or non-R/Python workflows, the data can also be accessed through Redivis's own R and Python client libraries. See the [Getting Started guide](/getstarted.qmd) for details.

### Beyond downloading data

The IRW project also includes:
- A growing set of **[vignettes](/vignettes/index.qmd)** — worked examples applying classic and new measurement methods across many IRW datasets at once
- **Training resources and problem sets** for teaching psychometrics with real data
- A **contribution process** for researchers who want to add their own datasets to the warehouse

## Learn more

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Website: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

If you use IRW data in your work, please cite the original data (we have provided functionality for doing so). It would also be great if you cared to cite the introductory paper above. 

---

*Questions, feedback, or want to contribute a dataset? Visit the [Contact page](/contact.qmd) or open an issue on [GitHub](https://github.com/itemresponsewarehouse).*
