---
lang: ur
dir: rtl
pagetitle: "آئٹم ریسپانس ویئر ہاؤس (Item Response Warehouse, IRW)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# آئٹم ریسپانس ویئر ہاؤس (Item Response Warehouse, IRW)

**نفسیاتی پیمائش (psychometrics) اور پیمائشی تحقیق کے لیے، ہم آہنگ کردہ آئٹم ریسپانس ڈیٹا کا ایک مفت، آزاد مجموعہ۔**

[مقالہ پڑھیں](https://doi.org/10.3758/s13428-025-02796-y) **(اوپن ایکسیس)**

{{< include /welcome/_lang_switch.md >}}

---

## IRW کیوں موجود ہے

پیمائش پر تحقیق کرنے والے محققین — تعلیم، نفسیات اور متعلقہ شعبوں میں — کو اپنے طریقہ ہائے کار کو جانچنے اور ان کا موازنہ کرنے کے لیے حقیقی ڈیٹا کی ضرورت ہوتی ہے۔ یہ ڈیٹا پہلے سے ہی بڑی مقدار میں موجود ہے۔ لیکن یہ بہت سے مطالعات میں بکھرا ہوا ہے، مختلف فارمیٹس میں محفوظ ہے، اور اکثر غیر واضح دستاویزات یا لائسنسنگ کی وجہ سے دوبارہ استعمال کرنا مشکل ہوتا ہے۔

یہ ایک معروف مسئلہ ہے۔ تحقیقی برادری نے اسے حل کرنے کے لیے ایک مشترکہ معیار وضع کیا ہے: ڈیٹا کو **FAIR** ہونا چاہیے — یعنی قابلِ دریافت (Findable)، قابلِ رسائی (Accessible)، باہم عمل پذیر (Interoperable)، اور دوبارہ قابلِ استعمال (Reusable) (Wilkinson et al., 2016)۔ دیگر شعبوں نے ان اصولوں کو عملی جامہ پہناتے ہوئے مشترکہ، معیاری ڈیٹا وسائل تعمیر کیے ہیں۔ کمپیوٹر سائنس میں، لیبل شدہ تصاویر کے مجموعے ImageNet نے محققین کو ایک مشترکہ معیار (benchmark) فراہم کیا اور AI میں تیز رفتار ترقی کو آگے بڑھانے میں مدد دی۔ جینیات اور اعصابی سائنس نے بھی اپنے ڈیٹا کے لیے اسی طرح کے مشترکہ وسائل تعمیر کیے ہیں۔

آئٹم ریسپانس ویئر ہاؤس (IRW) انہی FAIR اصولوں کو آئٹم ریسپانس ڈیٹا پر لاگو کرتا ہے۔ یہ ہزاروں موجودہ ڈیٹاسیٹس کو اکٹھا کرتا ہے اور انہیں ایک مشترکہ فارمیٹ میں ڈھالتا ہے — تاکہ ایک ڈیٹاسیٹ پر جانچا گیا کوئی طریقہ آسانی سے ہزاروں دیگر ڈیٹاسیٹس پر بھی آزمایا جا سکے۔

## IRW میں کیا شامل ہے

IRW میں **ہزاروں ڈیٹاسیٹس** ("ٹیبلز") شامل ہیں، جن میں سے ہر ایک انفرادی جوابات کا مجموعہ ہے۔ جب بھی کوئی شخص (یا کوئی اور اکائی) کسی پیمائشی آلے سے سامنا کرتا ہے اور نتیجہ ریکارڈ ہوتا ہے تو ایک ریسپانس تخلیق ہوتا ہے۔ پیمائشی آلہ کیا شمار ہوگا، یہ [ڈیٹا خاندان](/overview.qmd) کے مطابق مختلف ہوتا ہے: کوئی آئٹم جس کا جواب الگ سے دیا جائے، کوئی دوسری اکائی جس سے کسی مقابلے میں سامنا ہو، اختیارات کا پورا مجموعہ جسے ترتیب دینا ہو، یا کوئی پروفائل جو بے ترتیب طور پر دی گئی خصوصیات سے بنایا گیا ہو۔ مثالوں میں شامل ہیں:

- تعلیمی اور صلاحیت کے امتحانات میں طلبہ کے جوابات
- شخصیت یا رویوں کی پیمائش کرنے والے سروے آئٹمز
- انسانی جانچنے والوں (raters) کی جانب سے دیے گئے نمبرات
- مقابلے، میچ اور براہ راست موازنے، جہاں دو اکائیاں آمنے سامنے آتی ہیں اور ایک جیتتی ہے
- ترجیحی ووٹ کی پرچیاں، ججوں کی درجہ بندیاں اور سروے میں ترتیب دینے کے سوالات
- conjoint تجربات، جن میں جواب دہندگان بے ترتیب طور پر بنائے گئے پروفائلز میں سے انتخاب کرتے ہیں
- کسی بھی دوسری صورتحال میں پیمائشی آلات کے ایک مجموعے پر بار بار دیے گئے جوابات

IRW میں شامل ہر ڈیٹاسیٹ کو اس طرح ڈیزائن کیا گیا ہے کہ وہ:

- **قابلِ دریافت (Findable) ہو۔** ہر ڈیٹاسیٹ کے ساتھ پہلے سے شمار کردہ میٹا ڈیٹا موجود ہوتا ہے — شرکاء کی تعداد، آئٹمز کی تعداد، ریسپانس کثافت، موضوعی شعبہ، اور دیگر وضاحتی ٹیگز — تاکہ ڈیٹاسیٹس کو پہلے ڈاؤن لوڈ کیے بغیر ڈھونڈا اور فلٹر کیا جا سکے۔
- **قابلِ رسائی (Accessible) ہو۔** ہر ڈیٹاسیٹ کو ویب براؤزر یا `irw` پیکج کے ذریعے، ایک مفت اکاؤنٹ کے ساتھ، حاصل کیا جا سکتا ہے۔
- **باہم عمل پذیر (Interoperable) ہو۔** ہر ڈیٹاسیٹ کو لمبے فارمیٹ میں، ہر ریسپانس کے لیے ایک سطر، اپنے خاندان کے معیار کے مطابق (نیچے بیان کیا گیا ہے) ڈھالا جاتا ہے، تاکہ ایک ہی تجزیاتی کوڈ بہت کم یا بغیر کسی ترمیم کے کئی ڈیٹاسیٹس پر چلایا جا سکے۔
- **دوبارہ قابلِ استعمال (Reusable) ہو۔** ہر ڈیٹاسیٹ آزاد لائسنس کے تحت ہے، اس کا ماخذ دستاویزی ہے، اور اسے IRW فارمیٹ میں تبدیل کرنے کے لیے استعمال ہونے والا کوڈ عوامی طور پر دستیاب ہے۔

ڈیٹاسیٹس اپنے حجم (چند سو جوابات سے لے کر کئی ملین تک) اور جواب کی قسم (ہاں/نہیں آئٹمز، متعدد زمروں کی درجہ بندی، جزوی نمبرات، بلا ترتیب زمرے، مقابلوں کے نتائج، درجہ بندی کے مقامات، اور مزید) کے لحاظ سے کافی مختلف ہوتے ہیں۔

## ڈیٹا کا معیار

IRW کا ہر ٹیبل **لمبے فارمیٹ** میں ہے: ہر ریسپانس کے لیے ایک سطر۔ ایک سطر دراصل *کیا ہے*، یہ اس بات پر منحصر ہے کہ ٹیبل کس خاندان سے تعلق رکھتا ہے، اور اسے اٹھانے والے کالم بھی اسی پر۔

<img src="/figures/families/families-tree.svg" alt="دو سوالات جو کسی ڈیٹاسیٹ کو IRW کے کسی خاندان میں رکھتے ہیں۔"
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| خاندان | `source =` | معیار |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn خاکے میں نہیں ہے: یہ خاندان ڈیٹا کی شکل سے نہیں بلکہ اس کے ماخذ سے متعین ہوتا ہے، چنانچہ ایک مصنوعی ٹیبل ان میں سے کوئی بھی شکل رکھ سکتا ہے۔ ہر خاندان کے کالم [معیارات کا جائزہ](/overview.qmd) میں دیے گئے ہیں۔


## اسے کیسے استعمال کریں

آپ کتنا خودکار بنانا چاہتے ہیں اس پر منحصر، IRW ڈیٹا حاصل کرنے کے تین طریقے ہیں۔

**1. ویب براؤزر میں دیکھیں**
[IRW ڈیٹا براؤزر](/data.qmd) پر ڈیٹاسیٹس اور ان کے میٹا ڈیٹا کو براہ راست دیکھیں — کسی اکاؤنٹ کی ضرورت نہیں۔ مکمل ڈیٹاسیٹ ڈاؤن لوڈ کرنے کے لیے ایک مفت [Redivis](https://redivis.com) اکاؤنٹ درکار ہے، کیونکہ یہ وہی پلیٹ فارم ہے جو بنیادی ڈیٹا کو میزبانی کرتا ہے۔

**2. `irw` پیکیج استعمال کریں (تجویز کردہ)**
`irw` پیکیج **R** اور **Python** دونوں کے لیے دستیاب ہے۔

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

پہلی بار جب آپ یہ پیکیج استعمال کریں گے، آپ سے ایک مفت Redivis اکاؤنٹ کے ذریعے لاگ ان کرنے کو کہا جائے گا۔

**3. Redivis کی کلائنٹ لائبریریز براہ راست استعمال کریں**
نچلی سطح کے یا R/Python کے علاوہ ورک فلوز کے لیے، ڈیٹا کو Redivis کی اپنی R اور Python کلائنٹ لائبریریوں کے ذریعے بھی حاصل کیا جا سکتا ہے۔ تفصیلات کے لیے [شروعات کی رہنمائی](/getstarted.qmd) دیکھیں۔

### ڈیٹا ڈاؤن لوڈ کرنے سے آگے

IRW پروجیکٹ میں یہ بھی شامل ہے:

- **[وگنیٹس (vignettes)](/vignettes/index.qmd)** کا ایک بڑھتا ہوا مجموعہ — تیار کردہ مثالیں جو کلاسیکی اور نئے پیمائشی طریقوں کو بہت سے IRW ڈیٹاسیٹس پر ایک ساتھ لاگو کرتی ہیں
- حقیقی ڈیٹا کے ساتھ نفسیاتی پیمائش سکھانے کے لیے **تربیتی وسائل اور مشقی سیٹس**
- ان محققین کے لیے ایک **شراکتی عمل** جو اپنے ڈیٹاسیٹس کو ویئر ہاؤس میں شامل کرنا چاہتے ہیں

## مزید جانیں

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- ویب سائٹ: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- کوڈ: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

اگر آپ اپنے کام میں IRW ڈیٹا استعمال کرتے ہیں تو براہ کرم اصل ڈیٹا کا حوالہ دیں (ہم نے اس کے لیے ضروری فعالیت فراہم کی ہے)۔ اگر آپ اوپر دیے گئے تعارفی مقالے کا بھی حوالہ دیں تو یہ ہمارے لیے نہایت قابلِ قدر ہوگا۔

---

*سوالات، رائے ہے، یا کسی ڈیٹاسیٹ میں تعاون کرنا چاہتے ہیں؟ [رابطہ صفحہ](/contact.qmd) ملاحظہ کریں یا [GitHub](https://github.com/itemresponsewarehouse) پر ایک issue کھولیں۔*
