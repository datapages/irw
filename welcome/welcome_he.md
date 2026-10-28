---
lang: he
dir: rtl
pagetitle: "מחסן תגובות לפריטים (Item Response Warehouse, IRW)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# מחסן תגובות לפריטים (Item Response Warehouse, IRW)

**אוסף חופשי ופתוח של נתוני תגובה לפריט (item response) מתואמים, למחקר בפסיכומטריקה ובמדידה.**

[קראו את המאמר](https://doi.org/10.3758/s13428-025-02796-y) **(גישה פתוחה)**

{{< include /welcome/_lang_switch.md >}}

---

## למה IRW קיים

חוקרים העוסקים במדידה — בחינוך, בפסיכולוגיה ובתחומים קרובים — זקוקים לנתונים אמיתיים כדי לבחון ולהשוות בין השיטות שלהם. נתונים כאלה כבר קיימים בכמויות גדולות. אך הם מפוזרים על פני מחקרים רבים, מאוחסנים בפורמטים שונים רבים, ולעיתים קרובות קשה להשתמש בהם שוב בשל תיעוד או רישוי לא ברורים.

זו בעיה ידועה היטב. קהילת המחקר גיבשה תקן משותף לפתרון הבעיה הזו: נתונים צריכים להיות **FAIR** — ניתנים לאיתור (Findable), נגישים (Accessible), בעלי יכולת פעולה הדדית (Interoperable), וניתנים לשימוש חוזר (Reusable) (Wilkinson et al., 2016). תחומים אחרים יישמו את העקרונות הללו הלכה למעשה על ידי בניית משאבי נתונים משותפים ומתוקננים. במדעי המחשב, אוסף התמונות המתויגות ImageNet סיפק לחוקרים אמת מידה (benchmark) משותפת וסייע להניע קִדמה מהירה בתחום הבינה המלאכותית. גם הגנטיקה ומדעי המוח בנו משאבים משותפים דומים עבור הנתונים שלהם.

מחסן תגובות לפריטים (IRW) מיישם את אותם עקרונות FAIR על נתוני תגובה לפריט. הוא מאגד מאות מערכי נתונים קיימים ומעצב אותם מחדש לפורמט משותף אחד — כך ששיטה שנבחנה על מערך נתונים אחד יכולה להיבחן בקלות על מאות מערכי נתונים אחרים.

## מה נמצא ב-IRW

IRW מכיל **מאות מערכי נתונים** ("טבלאות"), וכל אחד מהם הוא אוסף של תגובות בודדות. תגובה נוצרת בכל פעם שאדם כלשהו (או יחידה אחרת) מגיב לפריט (item) (או לבדיקה אחרת). דוגמאות כוללות:

- תשובות תלמידים במבחני חינוך ויכולת
- פריטי שאלון המודדים אישיות או עמדות
- ציונים שניתנו על ידי מעריכים אנושיים
- כל מצב אחר הכולל תגובות חוזרות ונשנות למערך של כלי מדידה

כל מערך נתונים ב-IRW מתוכנן להיות:

- **ניתן לאיתור (Findable).** לכל מערך נתונים מצורפים מטא-נתונים שחושבו מראש — מספר המשתתפים, מספר הפריטים, צפיפות התגובות, תחום הנושא ותגיות תיאוריות נוספות — כך שניתן לאתר ולסנן מערכי נתונים מבלי להוריד אותם קודם.
- **נגיש (Accessible).** ניתן לקבל כל מערך נתונים דרך דפדפן האינטרנט או חבילת `irw`, באמצעות חשבון חינמי.
- **בעל יכולת פעולה הדדית (Interoperable).** כל מערך נתונים מעוצב מחדש לאותו מבנה פשוט (המתואר בהמשך), כך שאותו קוד ניתוח יכול לפעול על מערכי נתונים רבים בשינוי מועט או ללא שינוי כלל.
- **ניתן לשימוש חוזר (Reusable).** לכל מערך נתונים יש רישיון פתוח, מקורו מתועד, והקוד ששימש להמרתו לפורמט IRW הוא ציבורי.

מערכי הנתונים שונים מאוד בגודלם (ממאות בודדות של תגובות ועד מיליונים רבים) ובסוג התגובה (פריטים של כן/לא, דירוגים רב-קטגוריאליים, ציונים חלקיים, ועוד). כל מערך נתונים מגיע גם עם מטא-נתונים מחושבים מראש — מספר המשתתפים, מספר הפריטים, צפיפות התגובות, תחום הנושא, ותוויות תיאוריות נוספות — כך שחוקרים יכולים למצוא מערכי נתונים רלוונטיים מבלי להוריד ולעבד את כולם קודם.

## תקן הנתונים

<img src="/welcome/assets/diagram-cross-classification.svg" alt="תרשים המציג רשת צבעונית של תאי id-item, המומרת באמצעות חץ לטבלה בפורמט ארוך עם עמודות id, item ו-resp, כאשר צבעו של כל תא resp תואם לצבע התא המקורי שלו ברשת." class="welcome-figure">


כל מערך נתונים של IRW מעוצב מחדש לפורמט **ארוך (long format)**: שורה אחת לכל תגובה. לכל הפחות, כל שורה מכילה שלושה פרטי מידע:

| עמודה | משמעות |
|---|---|
| `id` | מי (או מה) הפיק את התגובה — בדרך כלל אדם |
| `item` | איזה כלי מדידה הפיק את התגובה — בדרך כלל שאלה או משימה |
| `resp` | התגובה עצמה, המאוחסנת כציון סדר (ordinal) |

**דוגמה:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

כאשר מערך נתונים כולל מידע נוסף — זמן תגובה, זהות המעריך, משתני עזר כמו גיל — מידע זה מאוחסן בעמודות נוספות בעלות שמות עקביים. מבנה פשוט אחד זה מכסה מגוון עצום של מצבי מדידה, וזה מה שמאפשר לכתוב קוד ניתוח פעם אחת וליישם אותו על פני כל המחסן.

המפרט הטכני המלא של התקן זמין בכתובת [itemresponsewarehouse.org/standard.html](/standard.qmd). קיימים גם תקנים קשורים ומתמחים יותר לטקסט פריטים, לנתוני השוואה זוגית (pairwise) ולתגובות נומינליות (קטגוריות ללא סדר).

## איך להשתמש בו

יש שלוש דרכים לקבל נתוני IRW, בהתאם לרמת האוטומציה הרצויה לך.

**1. עיון בדפדפן האינטרנט**
חִקרו מערכי נתונים ואת המטא-נתונים שלהם ישירות ב[דפדפן הנתונים של IRW](/data.qmd) — אין צורך בחשבון. הורדת מערך נתונים מלא דורשת חשבון [Redivis](https://redivis.com) חינמי, מכיוון שזו הפלטפורמה המארחת את הנתונים הבסיסיים.

**2. השתמשו בחבילת `irw` (מומלץ)**
חבילת `irw`, הזמינה הן עבור **R** והן עבור **Python**, מספקת פונקציות פשוטות למציאה, סינון והורדה של נתונים.

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

בפעם הראשונה שתשתמשו בחבילה, תתבקשו להתחבר באמצעות חשבון Redivis חינמי. לאחר מכן, שורת קוד אחת מורידה כל מערך נתונים ישירות ל-R או ל-Python. מנקודה זו, הנתונים מוכנים לניתוח באמצעות תוכנה סטנדרטית — לדוגמה חבילות תיאוריית תגובה לפריט (item response theory) או ניתוח גורמים.

**3. השתמשו ישירות בספריות הלקוח של Redivis**
עבור זרימות עבודה ברמה נמוכה יותר או שאינן מבוססות R/Python, ניתן לגשת לנתונים גם דרך ספריות הלקוח של Redivis עצמה עבור R ו-Python. לפרטים, ראו את [מדריך תחילת העבודה](/getstarted.qmd).

### מעבר להורדת נתונים

פרויקט IRW כולל גם:

- אוסף הולך וגדל של **[דוגמאות מפורטות (vignettes)](/vignettes/index.qmd)** — דוגמאות מעובדות המיישמות שיטות מדידה קלאסיות וחדשות על פני מערכי נתונים רבים של IRW בו-זמנית
- **משאבי הדרכה ומערכי תרגילים** ללימוד פסיכומטריקה עם נתונים אמיתיים
- **תהליך תרומה** עבור חוקרים המעוניינים להוסיף מערכי נתונים משלהם למחסן

## מידע נוסף

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- אתר אינטרנט: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- קוד: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

אם אתם משתמשים בנתוני IRW בעבודתכם, אנא צטטו את הנתונים המקוריים (סיפקנו לשם כך את הפונקציונליות הנדרשת). נשמח מאוד גם אם תצטטו את מאמר המבוא שלעיל.

---

*יש לכם שאלות, משוב, או שברצונכם לתרום מערך נתונים? בקרו ב[דף יצירת הקשר](/contact.qmd) או פתחו issue ב-[GitHub](https://github.com/itemresponsewarehouse).*
