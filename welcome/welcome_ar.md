---
lang: ar
dir: rtl
pagetitle: "مستودع استجابات البنود (Item Response Warehouse)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher link line below (English · Français · ...) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate or reorder it.
-->

# مستودع استجابات البنود (Item Response Warehouse)

**مجموعة مجانية ومفتوحة من بيانات استجابات البنود الموحّدة، مخصصة لأبحاث القياس النفسي والقياس بشكل عام.**

[itemresponsewarehouse.org](https://itemresponsewarehouse.org) · [GitHub](https://github.com/itemresponsewarehouse) · [قراءة البحث](https://doi.org/10.3758/s13428-025-02796-y)

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html)

---

## لماذا يوجد IRW

يحتاج الباحثون الذين يدرسون القياس — في التربية وعلم النفس والمجالات ذات الصلة — إلى بيانات حقيقية لاختبار مناهجهم ومقارنتها. هذه البيانات موجودة بالفعل بكميات كبيرة. لكنها مبعثرة عبر العديد من الدراسات، ومخزّنة بصيغ مختلفة كثيرة، وغالبًا ما يصعب إعادة استخدامها بسبب غموض التوثيق أو التراخيص.

هذه مشكلة معروفة جيدًا. حلّت مجالات أخرى هذه المشكلة عبر بناء موارد بيانات مشتركة وموحّدة المعايير. ففي علوم الحاسوب، وفّرت مجموعة الصور المُصنَّفة ImageNet للباحثين معيارًا مرجعيًا مشتركًا وساهمت في تسريع التقدّم في الذكاء الاصطناعي. كما بنت علوم الوراثة وعلم الأعصاب موارد مشتركة مماثلة لبياناتها الخاصة.

يقوم مستودع استجابات البنود (IRW) بالأمر نفسه لبيانات استجابات البنود. فهو يجمع مئات مجموعات البيانات الموجودة، ويعيد تشكيلها ضمن صيغة موحّدة واحدة — بحيث يمكن اختبار طريقة تم اختبارها على مجموعة بيانات واحدة بسهولة على مئات المجموعات الأخرى.

## ما الذي يحتويه IRW

يحتوي IRW على **مئات مجموعات البيانات** ("جداول")، كل واحدة منها مجموعة من الاستجابات الفردية. تُنتَج الاستجابة كلما استجاب شخص ما (أو وحدة أخرى) لبند ما (أو مسبار آخر). ومن الأمثلة على ذلك:

- إجابات الطلاب في اختبارات التعليم والقدرات
- بنود الاستبيانات التي تقيس الشخصية أو الاتجاهات
- التقييمات التي يمنحها مقيّمون بشريون
- أي سياق آخر ينطوي على استجابات متكررة لمجموعة من مسابر القياس

هناك أمران صحيحان بالنسبة لكل مجموعة بيانات في IRW:

- **مفتوحة.** كل مجموعة بيانات مرخّصة لإعادة الاستخدام. مصدرها موثَّق، والكود المستخدم لتحويلها إلى صيغة IRW متاح للعموم.
- **موحّدة.** يُعاد تشكيل كل مجموعة بيانات وفق البنية البسيطة نفسها (موضّحة أدناه)، بحيث يمكن تشغيل الكود التحليلي نفسه على العديد من مجموعات البيانات بتعديل بسيط أو دون أي تعديل.

تتفاوت مجموعات البيانات بشكل كبير من حيث الحجم (من بضع مئات من الاستجابات إلى عدة ملايين) ومن حيث نوع الاستجابة (بنود نعم/لا، تقييمات متعددة الفئات، درجات جزئية، وغيرها). كما تأتي كل مجموعة بيانات مصحوبة ببيانات وصفية محسوبة مسبقًا — عدد المشاركين، عدد البنود، كثافة الاستجابات، المجال الموضوعي، وعلامات وصفية أخرى — بحيث يمكن للباحثين إيجاد مجموعات البيانات ذات الصلة دون الحاجة إلى تنزيلها ومعالجتها جميعًا أولًا.

## معيار البيانات

يُعاد تشكيل كل مجموعة بيانات في IRW وفق **الصيغة الطويلة (long format)**: صف واحد لكل استجابة. يحتوي كل صف، كحد أدنى، على ثلاث معلومات:

| العمود | المعنى |
|---|---|
| `id` | من (أو ما) أنتج الاستجابة — عادةً شخص |
| `item` | أي مسبار قياس أنتج الاستجابة — عادةً سؤال أو مهمة |
| `resp` | الاستجابة نفسها، مخزَّنة كدرجة ترتيبية |

**مثال:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

عندما تتضمن مجموعة البيانات معلومات إضافية — مثل وقت الاستجابة، وهوية المقيّم، ومتغيرات مساعدة مثل العمر — تُخزَّن هذه المعلومات في أعمدة إضافية ذات تسمية متسقة. تغطي هذه البنية البسيطة الواحدة مجموعة واسعة جدًا من حالات القياس، وهذا ما يجعل من الممكن كتابة كود التحليل مرة واحدة وتطبيقه على المستودع بأكمله.

المواصفات التقنية الكاملة لهذا المعيار متاحة على [itemresponsewarehouse.org/standard.html](/standard.qmd). كما توجد معايير أكثر تخصصًا لنص البنود، وبيانات المنافسة الثنائية، والاستجابات الاسمية (الفئات غير المرتبة).

## كيفية الاستخدام

هناك ثلاث طرق للحصول على بيانات IRW، بحسب درجة الأتمتة التي ترغب بها.

**1. التصفح عبر متصفح الويب**
استكشف مجموعات البيانات وبياناتها الوصفية مباشرةً عبر [متصفح بيانات IRW](/data.qmd) — دون الحاجة إلى حساب. يتطلّب تنزيل مجموعة بيانات كاملة حسابًا مجانيًا في [Redivis](https://redivis.com)، بما أنها المنصة التي تستضيف البيانات الأساسية.

**2. استخدام حزمة `irw` (موصى به)**
توفّر حزمة `irw`، المتاحة لكل من **R** و**Python**، دوالًا بسيطة للبحث عن البيانات وتصفيتها وتنزيلها.

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

عند استخدام الحزمة لأول مرة، سيُطلب منك تسجيل الدخول بحساب Redivis مجاني. بعد ذلك، يكفي سطر واحد من الكود لتنزيل أي مجموعة بيانات مباشرةً إلى R أو Python. عندئذٍ، تصبح البيانات جاهزة للتحليل باستخدام برمجيات قياسية — مثل حزم نظرية استجابة البند أو التحليل العاملي.

**3. استخدام مكتبات عميل Redivis مباشرةً**
بالنسبة لسير العمل ذي المستوى الأدنى أو خارج بيئتي R/Python، يمكن أيضًا الوصول إلى البيانات عبر مكتبات عميل R وPython الخاصة بـ Redivis نفسها. راجع [دليل البدء](/getstarted.qmd) لمزيد من التفاصيل.

### ما وراء تنزيل البيانات

يشمل مشروع IRW أيضًا:
- مجموعة متنامية من **[العروض التوضيحية (vignettes)](/vignettes/index.qmd)** — أمثلة عملية تطبّق أساليب قياس كلاسيكية وحديثة على العديد من مجموعات بيانات IRW في آن واحد
- **موارد تدريبية وتمارين** لتدريس القياس النفسي باستخدام بيانات حقيقية
- **عملية مساهمة** للباحثين الراغبين في إضافة مجموعات بياناتهم الخاصة إلى المستودع

## لمزيد من المعلومات

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- الموقع الإلكتروني: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- الكود: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

إذا استخدمت بيانات IRW في عملك، يرجى الاستشهاد بالبيانات الأصلية (وقد وفّرنا وظيفة لذلك). سيكون من دواعي سرورنا أيضًا أن تستشهد بالبحث التعريفي المذكور أعلاه.

---

*لديك أسئلة أو ملاحظات، أو ترغب في المساهمة بمجموعة بيانات؟ زر [صفحة الاتصال](/contact.qmd) أو افتح "issue" على [GitHub](https://github.com/itemresponsewarehouse).*
