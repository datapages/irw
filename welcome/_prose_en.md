## Why the IRW exists

Researchers who study measurement — in education, psychology, and related fields — need real data to test and compare their methods. That data already exists in large quantities. But it is scattered across many studies, stored in many different formats, and often hard to reuse because of unclear documentation or licensing.

This is a well-known problem. The research community has articulated a shared standard for solving it: data should be **FAIR** — Findable, Accessible, Interoperable, and Reusable (Wilkinson et al., 2016). Other fields put these principles into practice by building shared, standardized data resources. In computer science, the ImageNet collection of labeled images gave researchers a common benchmark and helped drive rapid progress in AI. Genetics and neuroscience built similar shared resources for their own data.

The Item Response Warehouse (IRW) applies the same FAIR principles to item response data. It brings together thousands of existing datasets and reshapes them into one common format — so that a method tested on one dataset can easily be tested on thousands of others.

## What is in the IRW

The IRW contains **thousands of datasets** ("tables"), each one a collection of individual responses. A response is generated whenever some person (or other unit) meets a probe and the outcome is recorded. What counts as a probe differs by [data family](/overview.qmd): an item answered on its own, another entity met in a contest, a whole set of options put in order, or a profile assembled from randomly assigned attributes. Examples include:

- Student answers on education and ability tests
- Survey items measuring personality or attitudes
- Ratings assigned by human raters
- Games, matches and head-to-head comparisons, where two entities meet and one wins
- Ranked-choice ballots, jury rankings and survey ranking questions
- Conjoint experiments, where respondents choose between randomly built profiles
- Any other setting involving repeated responses to a set of measurement probes

Every dataset in the IRW is designed to be:

- **Findable.** Each dataset comes with pre-computed metadata — participant and item counts, response density, subject area, and other descriptive tags — so datasets can be located and filtered without downloading them first.
- **Accessible.** Every dataset can be retrieved through the web browser or the `irw` package, using a free account.
- **Interoperable.** Each dataset is reshaped into long format, one row per response, following the standard for its family (described below), so the same analysis code can run across many datasets with little or no modification.
- **Reusable.** Each dataset is openly licensed, its origin is documented, and the code used to convert it into the IRW format is public.

Datasets vary widely in size (from a few hundred responses to many millions) and in response type (yes/no items, multi-category ratings, partial-credit scores, unordered categories, match outcomes, ranked positions, and more).

## The data standard

Every IRW table is in **long format**: one row per response. What a row *is* depends on the family the table belongs to, and so do the columns that carry it.

<img src="/figures/families/families-tree.svg" alt="Two questions that sort a dataset into an IRW family." class="standard-tree">

| Family | `source =` | Standard |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn is not on the chart: it is defined by where the data come from, not by their shape, so a simulated table can have any of these shapes. The [overview of the standards](/overview.qmd) has the columns for each family.

## How to use it

There are three ways to get IRW data, depending on how much you want to automate.

**1. Browse in the web browser**
Explore datasets and their metadata directly on the [IRW data browser](/data.qmd) — no account needed. Downloading a full dataset requires a free [Redivis](https://redivis.com) account, since that is the platform that hosts the underlying data.

**2. Use the `irw` package (recommended)**
The `irw` package is available for both **R** and **Python**.

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

The first time you use the package, you will be asked to log in with a free Redivis account.

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
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Website: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

If you use IRW data in your work, please cite the original data (we have provided functionality for doing so). It would also be great if you cared to cite the introductory paper above. 

---

*Questions, feedback, or want to contribute a dataset? Visit the [Contact page](/contact.qmd) or open an issue on [GitHub](https://github.com/itemresponsewarehouse).*
