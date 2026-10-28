---
lang: bn
pagetitle: "Item Response Warehouse (IRW; আইটেম রেসপন্স ওয়্যারহাউস)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; আইটেম রেসপন্স ওয়্যারহাউস)

**সাইকোমেট্রিক্স ও পরিমাপ গবেষণার জন্য সুসংগত (harmonized) আইটেম রেসপন্স ডেটার একটি বিনামূল্যের, উন্মুক্ত সংগ্রহ।**

[গবেষণাপত্রটি পড়ুন](https://doi.org/10.3758/s13428-025-02796-y) **(ওপেন অ্যাক্সেস)**

{{< include /welcome/_lang_switch.md >}}

---

## IRW কেন প্রয়োজন

শিক্ষাবিজ্ঞান, মনোবিজ্ঞান এবং সংশ্লিষ্ট ক্ষেত্রে পরিমাপ নিয়ে গবেষণাকারী গবেষকদের নিজেদের পদ্ধতি পরীক্ষা ও তুলনা করার জন্য বাস্তব ডেটা প্রয়োজন। এই ধরনের ডেটা ইতিমধ্যেই প্রচুর পরিমাণে বিদ্যমান। কিন্তু তা বহু গবেষণায় বিক্ষিপ্তভাবে ছড়িয়ে আছে, বিভিন্ন ভিন্ন ফরম্যাটে সংরক্ষিত, এবং প্রায়শই অস্পষ্ট ডকুমেন্টেশন বা লাইসেন্সিংয়ের কারণে পুনরায় ব্যবহার করা কঠিন।

এটি একটি সুপরিচিত সমস্যা। গবেষক সম্প্রদায় এটি সমাধানের জন্য একটি সাধারণ মানদণ্ড প্রণয়ন করেছে: ডেটা **FAIR** হওয়া উচিত — অর্থাৎ খুঁজে পাওয়া যায় এমন (Findable), সহজলভ্য (Accessible), আন্তঃকার্যযোগ্য (Interoperable), এবং পুনঃব্যবহারযোগ্য (Reusable) (Wilkinson et al., 2016)। অন্যান্য ক্ষেত্র এই নীতিগুলো বাস্তবায়ন করে শেয়ার্ড, প্রমিতকৃত (standardized) ডেটা রিসোর্স তৈরি করেছে। কম্পিউটার বিজ্ঞানে, লেবেলযুক্ত ছবির সংগ্রহ ImageNet গবেষকদের একটি সাধারণ মানদণ্ড দিয়েছে এবং কৃত্রিম বুদ্ধিমত্তার দ্রুত অগ্রগতিতে সহায়ক হয়েছে। জেনেটিক্স ও নিউরোসায়েন্সও নিজেদের ডেটার জন্য একই ধরনের শেয়ার্ড রিসোর্স তৈরি করেছে।

আইটেম রেসপন্স ওয়্যারহাউস (IRW) আইটেম রেসপন্স ডেটার ওপর একই FAIR নীতি প্রয়োগ করে। এটি হাজার হাজার বিদ্যমান ডেটাসেটকে একত্র করে একটি সাধারণ ফরম্যাটে রূপান্তরিত করে — যাতে একটি ডেটাসেটে পরীক্ষিত কোনো পদ্ধতি সহজেই অন্য হাজার হাজার ডেটাসেটে পরীক্ষা করা যায়।

## IRW-এ কী আছে

IRW-এ **হাজার হাজার ডেটাসেট** ("টেবিল") রয়েছে, প্রতিটি পৃথক রেসপন্সের একটি সংগ্রহ। কোনো ব্যক্তি (বা অন্য একক) কোনো পরিমাপ প্রোবের সামনে পড়লে এবং ফলাফল লিপিবদ্ধ হলেই একটি রেসপন্স তৈরি হয়। কোনটি প্রোব হিসেবে গণ্য হবে তা [ডেটা পরিবার](/overview.qmd) অনুসারে ভিন্ন হয়: আলাদাভাবে উত্তর দেওয়া একটি আইটেম, কোনো প্রতিযোগিতায় মুখোমুখি হওয়া অন্য একটি সত্তা, ক্রমে সাজানোর জন্য দেওয়া বিকল্পের পুরো একটি সেট, অথবা এলোমেলোভাবে দেওয়া বৈশিষ্ট্য থেকে গড়া একটি প্রোফাইল। উদাহরণস্বরূপ:

- শিক্ষা ও যোগ্যতা পরীক্ষায় শিক্ষার্থীদের উত্তর
- ব্যক্তিত্ব বা মনোভাব পরিমাপকারী জরিপের আইটেম
- মানব রেটারদের দেওয়া রেটিং
- খেলা, ম্যাচ এবং সরাসরি তুলনা, যেখানে দুটি সত্তা মুখোমুখি হয় এবং একটি জেতে
- পছন্দক্রমের ব্যালট, বিচারকমণ্ডলীর ক্রমতালিকা এবং জরিপের ক্রম নির্ধারণের প্রশ্ন
- কনজয়েন্ট পরীক্ষা, যেখানে উত্তরদাতারা এলোমেলোভাবে তৈরি প্রোফাইলের মধ্যে বেছে নেন
- পরিমাপ প্রোবের একটি সেটের প্রতি বারবার রেসপন্স জড়িত অন্য যেকোনো পরিস্থিতি

IRW-এর প্রতিটি ডেটাসেট এমনভাবে ডিজাইন করা হয়েছে যাতে তা:

- **খুঁজে পাওয়া যায় এমন (Findable)।** প্রতিটি ডেটাসেটের সাথে পূর্ব-গণনাকৃত মেটাডেটা থাকে — অংশগ্রহণকারীর সংখ্যা, আইটেমের সংখ্যা, রেসপন্স ঘনত্ব, বিষয়ক্ষেত্র এবং অন্যান্য বর্ণনামূলক ট্যাগ — যাতে ডেটাসেটগুলো আগে ডাউনলোড না করেই খুঁজে পাওয়া ও ফিল্টার করা যায়।
- **সহজলভ্য (Accessible)।** প্রতিটি ডেটাসেট ওয়েব ব্রাউজার বা `irw` প্যাকেজের মাধ্যমে, একটি বিনামূল্যের অ্যাকাউন্ট দিয়ে, সংগ্রহ করা যায়।
- **আন্তঃকার্যযোগ্য (Interoperable)।** প্রতিটি ডেটাসেট লং ফরম্যাটে, প্রতি রেসপন্সে একটি সারি, তার পরিবারের স্ট্যান্ডার্ড অনুসারে (নিচে বর্ণিত) রূপান্তরিত, যাতে একই বিশ্লেষণ কোড সামান্য বা কোনো পরিবর্তন ছাড়াই বহু ডেটাসেটে চালানো যায়।
- **পুনঃব্যবহারযোগ্য (Reusable)।** প্রতিটি ডেটাসেট উন্মুক্ত লাইসেন্সের অধীনে, এর উৎস ডকুমেন্টেড, এবং এটিকে IRW ফরম্যাটে রূপান্তর করতে ব্যবহৃত কোড সর্বজনীন।

ডেটাসেটগুলো আকারে ব্যাপকভাবে ভিন্ন (কয়েকশো রেসপন্স থেকে লক্ষ লক্ষ পর্যন্ত) এবং রেসপন্সের ধরনেও ভিন্ন (হ্যাঁ/না আইটেম, বহু-শ্রেণির রেটিং, আংশিক-নম্বর স্কোর, ক্রমহীন শ্রেণি, প্রতিযোগিতার ফলাফল, ক্রমতালিকার অবস্থান, আরও অনেক কিছু)।

## ডেটা স্ট্যান্ডার্ড

IRW-এর প্রতিটি টেবিল **লং ফরম্যাটে**: প্রতি রেসপন্সে একটি সারি। একটি সারি ঠিক *কী*, তা নির্ভর করে টেবিলটি কোন পরিবারের উপর, এবং এটি যে কলামগুলোতে ধরা হয় সেগুলোও।

<img src="/figures/families/families-tree.svg" alt="দুটি প্রশ্ন যা একটি ডেটাসেটকে IRW-এর একটি পরিবারে স্থাপন করে।"
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| পরিবার | `source =` | স্ট্যান্ডার্ড |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn চার্টে নেই: এই পরিবারটি ডেটার আকার দিয়ে নয়, উৎস দিয়ে নির্ধারিত, তাই একটি সিমুলেটেড টেবিল এই যেকোনো আকারের হতে পারে। প্রতিটি পরিবারের কলাম [স্ট্যান্ডার্ডের সংক্ষিপ্তসার](/overview.qmd)-এ দেওয়া আছে।


## কীভাবে ব্যবহার করবেন

আপনি কতটা স্বয়ংক্রিয় করতে চান তার উপর ভিত্তি করে IRW ডেটা পাওয়ার তিনটি উপায় আছে।

**১. ওয়েব ব্রাউজারে দেখুন**
কোনো অ্যাকাউন্ট ছাড়াই সরাসরি [IRW ডেটা ব্রাউজার](/data.qmd)-এ ডেটাসেট ও তাদের মেটাডেটা অন্বেষণ করুন। সম্পূর্ণ ডেটাসেট ডাউনলোড করতে একটি বিনামূল্যের [Redivis](https://redivis.com) অ্যাকাউন্ট প্রয়োজন, কারণ এই প্ল্যাটফর্মটিই মূল ডেটা হোস্ট করে।

**২. `irw` প্যাকেজ ব্যবহার করুন (প্রস্তাবিত)**
`irw` প্যাকেজটি **R** ও **Python** উভয়ের জন্যই উপলব্ধ।

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

প্যাকেজটি প্রথমবার ব্যবহারের সময়, একটি বিনামূল্যের Redivis অ্যাকাউন্ট দিয়ে লগ ইন করতে বলা হবে।

**৩. সরাসরি Redivis ক্লায়েন্ট লাইব্রেরি ব্যবহার করুন**
নিম্ন-স্তরের বা R/Python নয় এমন ওয়ার্কফ্লোর জন্য, Redivis-এর নিজস্ব R ও Python ক্লায়েন্ট লাইব্রেরির মাধ্যমেও ডেটা অ্যাক্সেস করা যায়। বিস্তারিত জানতে দেখুন [গেটিং স্টার্টেড গাইড](/getstarted.qmd)।

### শুধু ডেটা ডাউনলোডের বাইরেও

IRW প্রকল্পে আরও অন্তর্ভুক্ত রয়েছে:

- ক্রমবর্ধমান **[ভিনিয়েট (vignettes)](/vignettes/index.qmd)**-এর একটি সংগ্রহ — একসাথে বহু IRW ডেটাসেট জুড়ে ক্লাসিক ও নতুন পরিমাপ পদ্ধতি প্রয়োগের বিস্তারিত উদাহরণ
- বাস্তব ডেটা দিয়ে সাইকোমেট্রিক্স শেখানোর জন্য **প্রশিক্ষণ রিসোর্স ও অনুশীলন সেট**
- নিজেদের ডেটাসেট ওয়্যারহাউসে যোগ করতে ইচ্ছুক গবেষকদের জন্য একটি **অবদান প্রক্রিয়া (contribution process)**

## আরও জানুন

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- ওয়েবসাইট: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- কোড: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

আপনি যদি আপনার কাজে IRW-এর ডেটা ব্যবহার করেন, তাহলে অনুগ্রহ করে মূল ডেটা উদ্ধৃত (cite) করুন (আমরা এর জন্য প্রয়োজনীয় সুবিধা দিয়েছি)। উপরে উল্লিখিত পরিচিতিমূলক গবেষণাপত্রটিও উদ্ধৃত করলে আমরা কৃতজ্ঞ থাকব।

---

*প্রশ্ন, মতামত, অথবা কোনো ডেটাসেটে অবদান রাখতে চান? [যোগাযোগ পৃষ্ঠা](/contact.qmd) দেখুন অথবা [GitHub](https://github.com/itemresponsewarehouse)-এ একটি issue খুলুন।*
