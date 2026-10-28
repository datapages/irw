---
lang: fa
dir: rtl
pagetitle: "انبار پاسخ به سؤال‌ها (Item Response Warehouse, IRW)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# انبار پاسخ به سؤال‌ها (Item Response Warehouse, IRW)

**مجموعه‌ای رایگان و آزاد از داده‌های پاسخ به سؤال (item response) هماهنگ‌شده، برای پژوهش‌های روان‌سنجی (psychometrics) و اندازه‌گیری.**

[مقاله را بخوانید](https://doi.org/10.3758/s13428-025-02796-y) **(دسترسی آزاد)**

{{< include /welcome/_lang_switch.md >}}

---

## چرا IRW وجود دارد

پژوهشگرانی که اندازه‌گیری را مطالعه می‌کنند — در آموزش، روان‌شناسی و حوزه‌های مرتبط — برای آزمودن و مقایسه روش‌های خود به داده‌های واقعی نیاز دارند. این داده‌ها از قبل به مقدار زیاد وجود دارند. اما در میان پژوهش‌های بسیاری پراکنده‌اند، در قالب‌های بسیار متفاوتی ذخیره شده‌اند، و اغلب به دلیل مستندسازی یا مجوز نامشخص، بازاستفاده از آن‌ها دشوار است.

این مشکلی شناخته‌شده است. جامعه پژوهشی استانداردی مشترک برای حل آن تدوین کرده است: داده‌ها باید **FAIR** باشند — یعنی قابل‌یافتن (Findable)، در دسترس (Accessible)، تعامل‌پذیر (Interoperable)، و قابل‌استفاده مجدد (Reusable) (Wilkinson et al., 2016). حوزه‌های دیگر با به‌کارگیری این اصول در عمل، منابع داده‌ای مشترک و استانداردشده ساخته‌اند. در علوم رایانه، مجموعه تصاویر برچسب‌گذاری‌شده ImageNet به پژوهشگران یک معیار سنجش (benchmark) مشترک داد و به پیشرفت سریع در هوش مصنوعی کمک کرد. ژنتیک و علوم اعصاب نیز منابع مشترک مشابهی برای داده‌های خود ساخته‌اند.

انبار پاسخ به سؤال‌ها (IRW) همین اصول FAIR را برای داده‌های پاسخ به سؤال به کار می‌گیرد. این پروژه هزاران مجموعه‌داده موجود را گرد هم می‌آورد و آن‌ها را به یک قالب مشترک تبدیل می‌کند — به‌طوری‌که روشی که روی یک مجموعه‌داده آزموده شده، به‌آسانی روی هزاران مجموعه‌داده دیگر نیز قابل آزمودن باشد.

## در IRW چه چیزی وجود دارد

IRW شامل **هزاران مجموعه‌داده** ("جدول") است که هر یک مجموعه‌ای از پاسخ‌های فردی است. هر بار که شخصی (یا واحدی دیگر) با یک ابزار اندازه‌گیری روبه‌رو می‌شود و نتیجه ثبت می‌گردد، یک پاسخ تولید می‌شود. اینکه چه چیزی ابزار اندازه‌گیری به شمار می‌آید به [خانواده‌ی داده](/overview.qmd) بستگی دارد: سؤالی که به‌تنهایی به آن پاسخ داده می‌شود، موجودیتی دیگر که در یک رویارویی با آن روبه‌رو می‌شوید، مجموعه‌ی کاملی از گزینه‌ها که باید مرتب شوند، یا نیم‌رخی که از ویژگی‌های تصادفی ساخته شده است. نمونه‌ها عبارت‌اند از:

- پاسخ‌های دانش‌آموزان در آزمون‌های آموزشی و توانایی
- سؤال‌های پرسش‌نامه‌ای که شخصیت یا نگرش‌ها را می‌سنجند
- نمره‌هایی که ارزیاب‌های انسانی می‌دهند
- بازی‌ها، مسابقه‌ها و مقایسه‌های رودررو، که در آن‌ها دو موجودیت روبه‌رو می‌شوند و یکی برنده می‌شود
- برگه‌های رأی ترجیحی، رتبه‌بندی‌های هیئت داوران و پرسش‌های رتبه‌بندی در نظرسنجی‌ها
- آزمایش‌های conjoint، که در آن‌ها پاسخ‌دهندگان میان نیم‌رخ‌های ساخته‌شده‌ی تصادفی انتخاب می‌کنند
- هر موقعیت دیگری که شامل پاسخ‌های تکراری به مجموعه‌ای از ابزارهای اندازه‌گیری باشد

هر مجموعه‌داده در IRW چنین طراحی شده که:

- **قابل‌یافتن (Findable) باشد.** هر مجموعه‌داده با فراداده‌های از پیش محاسبه‌شده همراه است — تعداد شرکت‌کنندگان، تعداد سؤال‌ها، تراکم پاسخ‌ها، حوزه موضوعی و سایر برچسب‌های توصیفی — تا مجموعه‌داده‌ها بدون نیاز به دانلود پیشین، قابل یافتن و فیلترکردن باشند.
- **در دسترس (Accessible) باشد.** هر مجموعه‌داده را می‌توان از طریق مرورگر وب یا بسته `irw`، با یک حساب کاربری رایگان، دریافت کرد.
- **تعامل‌پذیر (Interoperable) باشد.** هر مجموعه‌داده به قالب بلند، یک سطر برای هر پاسخ، و بر اساس استاندارد خانواده‌ی خود (که در ادامه توضیح داده می‌شود) تبدیل می‌شود، به‌طوری‌که همان کد تحلیلی بتواند با تغییرات کم یا بدون تغییر، روی مجموعه‌داده‌های بسیاری اجرا شود.
- **قابل‌استفاده مجدد (Reusable) باشد.** هر مجموعه‌داده دارای مجوز باز است، منشأ آن مستند شده، و کدی که برای تبدیل آن به قالب IRW استفاده شده، عمومی است.

مجموعه‌داده‌ها از نظر اندازه (از چند صد پاسخ تا میلیون‌ها پاسخ) و از نظر نوع پاسخ (سؤال‌های بله/خیر، رتبه‌بندی‌های چندگانه، نمره‌های جزئی، مقوله‌های بدون ترتیب، نتایج رویارویی‌ها، جایگاه‌های رتبه‌بندی و موارد دیگر) بسیار متفاوت‌اند.

## استاندارد داده

هر جدول IRW در **قالب بلند** است: یک سطر برای هر پاسخ. اینکه یک سطر *چیست* به خانواده‌ای بستگی دارد که جدول به آن تعلق دارد، و ستون‌هایی که آن را می‌سازند نیز همین‌طور.

<img src="/figures/families/families-tree.svg" alt="دو پرسش که یک مجموعه‌داده را در یکی از خانواده‌های IRW جای می‌دهند."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| خانواده | `source =` | استاندارد |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn در نمودار نیست: این خانواده بر پایه‌ی خاستگاه داده تعریف می‌شود، نه شکل آن، بنابراین یک جدول شبیه‌سازی‌شده می‌تواند هر یک از این شکل‌ها را داشته باشد. ستون‌های هر خانواده در [نمای کلی استانداردها](/overview.qmd) آمده است.


## چگونه از آن استفاده کنیم

بسته به میزان خودکارسازی که می‌خواهید، سه راه برای دریافت داده‌های IRW وجود دارد.

**۱. مرور در مرورگر وب**
مجموعه‌داده‌ها و فراداده‌های آن‌ها را مستقیماً در [مرورگر داده‌ی IRW](/data.qmd) کاوش کنید — نیازی به حساب کاربری نیست. دانلود یک مجموعه‌داده‌ی کامل به یک حساب رایگان [Redivis](https://redivis.com) نیاز دارد، زیرا این همان پلتفرمی است که داده‌های زیربنایی را میزبانی می‌کند.

**۲. از بسته‌ی `irw` استفاده کنید (توصیه‌شده)**
بسته‌ی `irw` هم برای **R** و هم برای **Python** در دسترس است.

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

نخستین باری که از این بسته استفاده می‌کنید، از شما خواسته می‌شود با یک حساب رایگان Redivis وارد شوید.

**۳. مستقیماً از کتابخانه‌های کلاینت Redivis استفاده کنید**
برای گردش‌کارهای سطح پایین‌تر یا خارج از R/Python، داده همچنین از طریق کتابخانه‌های کلاینت R و Python خود Redivis نیز در دسترس است. برای جزئیات، به [راهنمای شروع کار](/getstarted.qmd) مراجعه کنید.

### فراتر از دانلود داده

پروژه‌ی IRW همچنین شامل موارد زیر است:

- مجموعه‌ای رو‌به‌رشد از **[نمونه‌های کاربردی (vignettes)](/vignettes/index.qmd)** — نمونه‌های تشریح‌شده‌ای که روش‌های اندازه‌گیری کلاسیک و جدید را به‌طور هم‌زمان روی بسیاری از مجموعه‌داده‌های IRW به‌کار می‌گیرند
- **منابع آموزشی و مجموعه‌تمرین‌ها** برای آموزش روان‌سنجی با داده‌های واقعی
- یک **فرایند مشارکت** برای پژوهشگرانی که می‌خواهند مجموعه‌داده‌های خود را به انبار داده اضافه کنند

## بیشتر بدانید

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- وب‌سایت: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- کد: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

اگر از داده‌های IRW در کار خود استفاده می‌کنید، لطفاً به داده‌ی اصلی استناد دهید (امکانات لازم برای این کار را فراهم کرده‌ایم). همچنین بسیار ارزشمند خواهد بود اگر به مقاله‌ی معرفی بالا نیز استناد دهید.

---

*سؤال، بازخورد دارید، یا می‌خواهید در یک مجموعه‌داده مشارکت کنید؟ به [صفحه‌ی تماس](/contact.qmd) مراجعه کنید یا در [GitHub](https://github.com/itemresponsewarehouse) یک issue باز کنید.*
