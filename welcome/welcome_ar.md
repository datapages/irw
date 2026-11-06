---
lang: ar
dir: rtl
pagetitle: "مستودع استجابات البنود (Item Response Warehouse, IRW)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# مستودع استجابات البنود (Item Response Warehouse, IRW)

**مجموعة مجانية ومفتوحة من بيانات استجابات البنود الموحّدة، مخصصة لأبحاث القياس النفسي والقياس بشكل عام.**

[قراءة البحث](https://doi.org/10.3758/s13428-025-02796-y) **(وصول مفتوح)**

{{< include /welcome/_lang_switch.md >}}

---

## لماذا يوجد IRW

يحتاج الباحثون الذين يدرسون القياس — في التربية وعلم النفس والمجالات ذات الصلة — إلى بيانات حقيقية لاختبار مناهجهم ومقارنتها. هذه البيانات موجودة بالفعل بكميات كبيرة. لكنها مبعثرة عبر العديد من الدراسات، ومخزّنة بصيغ مختلفة كثيرة، وغالبًا ما يصعب إعادة استخدامها بسبب غموض التوثيق أو التراخيص.

هذه مشكلة معروفة جيدًا. وقد صاغ مجتمع البحث معيارًا مشتركًا لحلّها: ينبغي أن تكون البيانات **FAIR** — أي قابلة للاكتشاف (Findable)، ويمكن الوصول إليها (Accessible)، وقابلة للتشغيل البيني (Interoperable)، وقابلة لإعادة الاستخدام (Reusable) (Wilkinson et al., 2016). وقد طبّقت مجالات أخرى هذه المبادئ عمليًا عبر بناء موارد بيانات مشتركة وموحّدة المعايير. ففي علوم الحاسوب، وفّرت مجموعة الصور المُصنَّفة ImageNet للباحثين معيارًا مرجعيًا مشتركًا وساهمت في تسريع التقدّم في الذكاء الاصطناعي. كما بنت علوم الوراثة وعلم الأعصاب موارد مشتركة مماثلة لبياناتها الخاصة.

يطبّق مستودع استجابات البنود (IRW) المبادئ ذاتها على بيانات استجابات البنود. فهو يجمع آلاف مجموعات البيانات الموجودة، ويعيد تشكيلها ضمن صيغة موحّدة واحدة — بحيث يمكن اختبار طريقة تم اختبارها على مجموعة بيانات واحدة بسهولة على آلاف المجموعات الأخرى.

## ما الذي يحتويه IRW

يحتوي IRW على **آلاف مجموعات البيانات** ("جداول")، كل واحدة منها مجموعة من الاستجابات الفردية. تُنتَج الاستجابة كلما واجه شخص ما (أو وحدة أخرى) مسبار قياس وسُجّلت النتيجة. وما يُعدّ مسبارًا يختلف باختلاف [عائلة البيانات](/overview.qmd): بند يُجاب عنه وحده، أو كيان آخر يُواجَه في منافسة، أو مجموعة كاملة من الخيارات تُرتَّب، أو ملف يُركَّب من سمات مُسنَدة عشوائيًا. ومن الأمثلة على ذلك:

- إجابات الطلاب في اختبارات التعليم والقدرات
- بنود الاستبيانات التي تقيس الشخصية أو الاتجاهات
- التقييمات التي يمنحها مقيّمون بشريون
- المباريات والمنافسات والمقارنات المباشرة، حيث يلتقي كيانان ويفوز أحدهما
- بطاقات الاقتراع التفضيلي، وترتيبات لجان التحكيم، وأسئلة الترتيب في الاستبيانات
- تجارب المفاضلة المركّبة (conjoint)، حيث يختار المشاركون بين ملفات مُركَّبة عشوائيًا
- أي سياق آخر ينطوي على استجابات متكررة لمجموعة من مسابر القياس

صُممت كل مجموعة بيانات في IRW لتكون:

- **قابلة للاكتشاف (Findable).** تأتي كل مجموعة بيانات مصحوبة بميتاداتا محسوبة مسبقًا — عدد المشاركين، عدد البنود، كثافة الاستجابات، المجال الموضوعي، وسمات وصفية أخرى — بحيث يمكن تحديد موقع مجموعات البيانات وتصفيتها دون الحاجة إلى تنزيلها أولاً.
- **يمكن الوصول إليها (Accessible).** يمكن الحصول على كل مجموعة بيانات عبر متصفح الويب أو حزمة `irw`، باستخدام حساب مجاني.
- **قابلة للتشغيل البيني (Interoperable).** يُعاد تشكيل كل مجموعة بيانات في صيغة طويلة، صفّ واحد لكل استجابة، وفق معيار عائلتها (موضّح أدناه)، بحيث يمكن تشغيل الكود التحليلي نفسه على العديد من مجموعات البيانات بتعديل بسيط أو دون أي تعديل.
- **قابلة لإعادة الاستخدام (Reusable).** كل مجموعة بيانات مرخّصة ترخيصًا مفتوحًا، ومصدرها موثَّق، والكود المستخدم لتحويلها إلى صيغة IRW متاح للعموم.

تتفاوت مجموعات البيانات بشكل كبير من حيث الحجم (من بضع مئات من الاستجابات إلى عدة ملايين) ومن حيث نوع الاستجابة (بنود نعم/لا، تقييمات متعددة الفئات، درجات جزئية، فئات غير مرتّبة، نتائج منافسات، مراتب ترتيب، وغيرها).

## معيار البيانات

كل جدول في IRW في **صيغة طويلة**: صفّ واحد لكل استجابة. وما *يمثّله* الصفّ يعتمد على العائلة التي ينتمي إليها الجدول، وكذلك الأعمدة التي تحمله.

<img src="/figures/families/families-tree.svg" alt="سؤالان يصنّفان مجموعة بيانات ضمن إحدى عائلات IRW."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| العائلة | `source =` | المعيار |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

لا يظهر Simsyn في المخطط: فهذه العائلة تُعرَّف بمصدر البيانات لا بشكلها، ولذلك قد يأخذ الجدول المحاكى أيًّا من هذه الأشكال. أعمدة كل عائلة مذكورة في [نظرة عامة على المعايير](/overview.qmd).


## كيفية الاستخدام

هناك ثلاث طرق للحصول على بيانات IRW، بحسب درجة الأتمتة التي ترغب بها.

**1. التصفح عبر متصفح الويب**
استكشف مجموعات البيانات وبياناتها الوصفية مباشرةً عبر [متصفح بيانات IRW](/data.qmd) — دون الحاجة إلى حساب. يتطلّب تنزيل مجموعة بيانات كاملة حسابًا مجانيًا في [Redivis](https://redivis.com)، بما أنها المنصة التي تستضيف البيانات الأساسية.

**2. استخدام حزمة `irw` (موصى به)**
حزمة `irw` متاحة لكل من **R** و**Python**.

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

عند استخدام الحزمة لأول مرة، سيُطلب منك تسجيل الدخول بحساب Redivis مجاني.

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
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- الموقع الإلكتروني: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- الكود: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

إذا استخدمت بيانات IRW في عملك، يرجى الاستشهاد بالبيانات الأصلية (وقد وفّرنا وظيفة لذلك). سيكون من دواعي سرورنا أيضًا أن تستشهد بالبحث التعريفي المذكور أعلاه.

---

*لديك أسئلة أو ملاحظات، أو ترغب في المساهمة بمجموعة بيانات؟ زر [صفحة الاتصال](/contact.qmd) أو افتح "issue" على [GitHub](https://github.com/itemresponsewarehouse).*
