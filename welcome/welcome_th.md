---
lang: th
pagetitle: "Item Response Warehouse (IRW; คลังข้อมูลการตอบสนองต่อข้อสอบ)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; คลังข้อมูลการตอบสนองต่อข้อสอบ)

**คลังข้อมูลการตอบสนองต่อข้อสอบ (item response) ที่ผ่านการปรับให้เป็นมาตรฐานเดียวกัน เปิดให้ใช้งานฟรีและเปิดกว้าง สำหรับงานวิจัยด้านจิตวิทยาการวัด (psychometrics) และการวัดผล**

[อ่านบทความ](https://doi.org/10.3758/s13428-025-02796-y) **(เข้าถึงแบบเปิด)**

<div class="lang-switch"><button type="button" class="lang-switch-trigger" aria-haspopup="true" aria-expanded="false" aria-controls="lang-switch-panel" aria-label="Choose a language"><i class="bi bi-globe2" aria-hidden="true"></i><i class="bi bi-chevron-down" aria-hidden="true"></i></button><div class="lang-switch-panel" id="lang-switch-panel" hidden><label class="visually-hidden" for="lang-switch-search">Search languages</label><div class="lang-switch-search-wrap"><i class="bi bi-search" aria-hidden="true"></i><input type="text" id="lang-switch-search" class="lang-switch-search" autocomplete="off" placeholder="Search languages"></div><ul class="lang-switch-list" role="listbox"></ul></div></div>
<noscript>

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html) · [繁體中文](/welcome/welcome_zh-hant.html) · [বাংলা](/welcome/welcome_bn.html) · [Türkçe](/welcome/welcome_tr.html) · [Tiếng Việt](/welcome/welcome_vi.html) · [Italiano](/welcome/welcome_it.html) · [Nederlands](/welcome/welcome_nl.html) · [Polski](/welcome/welcome_pl.html) · [ไทย](/welcome/welcome_th.html) · [اردو](/welcome/welcome_ur.html) · [தமிழ்](/welcome/welcome_ta.html) · [فارسی](/welcome/welcome_fa.html) · [Kiswahili](/welcome/welcome_sw.html) · [עברית](/welcome/welcome_he.html) · [Bahasa Melayu](/welcome/welcome_ms.html)

</noscript>
<script>
(function () {
  var LANGS = [
    { native: "English",      en: "English",                lang: "en" },
    { native: "Français",     en: "French",                 lang: "fr" },
    { native: "Español",      en: "Spanish",                lang: "es" },
    { native: "中文",          en: "Chinese Simplified",     lang: "zh-hans" },
    { native: "한국어",        en: "Korean",                 lang: "ko" },
    { native: "العربية",      en: "Arabic",                 lang: "ar" },
    { native: "日本語",        en: "Japanese",               lang: "ja" },
    { native: "Português",    en: "Portuguese",             lang: "pt" },
    { native: "Deutsch",      en: "German",                 lang: "de" },
    { native: "हिन्दी",        en: "Hindi",                  lang: "hi" },
    { native: "Русский",      en: "Russian",                lang: "ru" },
    { native: "繁體中文",       en: "Chinese Traditional",    lang: "zh-hant" },
    { native: "বাংলা",        en: "Bengali",                lang: "bn" },
    { native: "Türkçe",       en: "Turkish",                lang: "tr" },
    { native: "Tiếng Việt",   en: "Vietnamese",             lang: "vi" },
    { native: "Italiano",     en: "Italian",                lang: "it" },
    { native: "Nederlands",   en: "Dutch",                  lang: "nl" },
    { native: "Polski",       en: "Polish",                 lang: "pl" },
    { native: "ไทย",          en: "Thai",                   lang: "th" },
    { native: "اردو",         en: "Urdu",                   lang: "ur" },
    { native: "தமிழ்",        en: "Tamil",                  lang: "ta" },
    { native: "فارسی",        en: "Persian",                lang: "fa" },
    { native: "Kiswahili",    en: "Swahili",                lang: "sw" },
    { native: "עברית",        en: "Hebrew",                 lang: "he" },
    { native: "Bahasa Melayu",en: "Malay",                  lang: "ms" }
  ];

  document.querySelectorAll(".lang-switch").forEach(function (root) {
    var trigger = root.querySelector(".lang-switch-trigger");
    var panel = root.querySelector(".lang-switch-panel");
    var search = root.querySelector(".lang-switch-search");
    var list = root.querySelector(".lang-switch-list");

    // Hrefs are read from the <noscript> fallback rather than hardcoded,
    // because Quarto resolves that markdown's links to correct relative
    // paths per page at build time; a literal string in this script would
    // stay absolute and break under file:// or non-root deployments.
    var hrefByNative = {};
    var noscriptEl = root.nextElementSibling;
    if (noscriptEl && noscriptEl.tagName === "NOSCRIPT") {
      var tmp = document.createElement("div");
      tmp.innerHTML = noscriptEl.textContent;
      Array.prototype.forEach.call(tmp.querySelectorAll("a"), function (a) {
        hrefByNative[a.textContent.trim()] = a.href;
      });
    }

    // Current language is read from <html lang="...">, which Quarto sets
    // from each page's own frontmatter, rather than guessed from the URL.
    var currentLang = (document.documentElement.lang || "").toLowerCase();

    function render(filterText) {
      list.innerHTML = "";
      var q = (filterText || "").trim().toLowerCase();
      var matches = LANGS.filter(function (entry) {
        return !q || entry.native.toLowerCase().indexOf(q) !== -1 || entry.en.toLowerCase().indexOf(q) !== -1;
      });

      if (matches.length === 0) {
        var empty = document.createElement("li");
        empty.className = "lang-switch-empty";
        empty.textContent = "No matches";
        list.appendChild(empty);
        return;
      }

      matches.forEach(function (entry) {
        var li = document.createElement("li");
        li.setAttribute("role", "option");
        if (entry.lang === currentLang) {
          li.className = "lang-switch-current";
          li.setAttribute("aria-current", "true");
          var check = document.createElement("i");
          check.className = "bi bi-check2";
          check.setAttribute("aria-hidden", "true");
          li.appendChild(check);
          li.appendChild(document.createTextNode(" " + entry.native));
        } else {
          var a = document.createElement("a");
          a.href = hrefByNative[entry.native] || "#";
          a.textContent = entry.native;
          li.appendChild(a);
        }
        list.appendChild(li);
      });
    }

    function openPanel() {
      panel.hidden = false;
      trigger.setAttribute("aria-expanded", "true");
      search.value = "";
      render("");
      search.focus();
    }

    function closePanel() {
      panel.hidden = true;
      trigger.setAttribute("aria-expanded", "false");
    }

    trigger.addEventListener("click", function (e) {
      e.stopPropagation();
      if (panel.hidden) { openPanel(); } else { closePanel(); }
    });

    search.addEventListener("input", function () {
      render(search.value);
    });

    panel.addEventListener("click", function (e) {
      e.stopPropagation();
    });

    document.addEventListener("click", function () {
      if (!panel.hidden) closePanel();
    });

    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape" && !panel.hidden) {
        closePanel();
        trigger.focus();
      }
    });

    render("");
  });
})();
</script>

---

## เหตุใด IRW จึงมีอยู่

นักวิจัยที่ศึกษาเรื่องการวัดผล — ทั้งในด้านการศึกษา จิตวิทยา และสาขาที่เกี่ยวข้อง — ต้องการข้อมูลจริงเพื่อทดสอบและเปรียบเทียบวิธีการของตน ข้อมูลลักษณะนี้มีอยู่แล้วเป็นจำนวนมาก แต่กระจัดกระจายอยู่ในงานวิจัยหลายชิ้น ถูกจัดเก็บในรูปแบบที่แตกต่างกันมากมาย และมักนำกลับมาใช้ใหม่ได้ยากเนื่องจากเอกสารประกอบหรือสัญญาอนุญาตที่ไม่ชัดเจน

นี่เป็นปัญหาที่รู้จักกันดี สาขาอื่น ๆ ได้แก้ปัญหานี้ด้วยการสร้างแหล่งข้อมูลที่ใช้ร่วมกันและเป็นมาตรฐานเดียวกัน ในวิทยาการคอมพิวเตอร์ ชุดข้อมูลภาพที่ติดป้ายกำกับอย่าง ImageNet ได้มอบมาตรฐานเปรียบเทียบ (benchmark) ร่วมกันให้แก่นักวิจัย และช่วยขับเคลื่อนความก้าวหน้าอย่างรวดเร็วในด้าน AI พันธุศาสตร์และประสาทวิทยาศาสตร์ก็ได้สร้างแหล่งข้อมูลร่วมที่คล้ายกันสำหรับข้อมูลของตนเอง

Item Response Warehouse (IRW) ทำสิ่งเดียวกันนี้สำหรับข้อมูลการตอบสนองต่อข้อสอบ โดยรวบรวมชุดข้อมูลที่มีอยู่แล้วหลายร้อยชุดและปรับให้อยู่ในรูปแบบเดียวกัน — เพื่อให้วิธีการที่ทดสอบกับชุดข้อมูลหนึ่งสามารถนำไปทดสอบกับชุดข้อมูลอื่นอีกหลายร้อยชุดได้อย่างง่ายดาย

## สิ่งที่มีอยู่ใน IRW

IRW ประกอบด้วย **ชุดข้อมูลหลายร้อยชุด** ("ตาราง") แต่ละชุดเป็นการรวบรวมคำตอบของแต่ละบุคคล คำตอบหนึ่งจะเกิดขึ้นทุกครั้งที่บุคคล (หรือหน่วยอื่น) ตอบสนองต่อข้อสอบ (item) หรือแบบวัดอื่นใด ตัวอย่างเช่น

- คำตอบของนักเรียนในแบบทดสอบทางการศึกษาและความสามารถ
- ข้อคำถามในแบบสอบถามที่วัดบุคลิกภาพหรือทัศนคติ
- คะแนนที่ผู้ประเมินซึ่งเป็นมนุษย์ให้ไว้
- สถานการณ์อื่นใดที่เกี่ยวข้องกับการตอบสนองซ้ำ ๆ ต่อชุดเครื่องมือวัดผล

มีสองสิ่งที่เป็นจริงสำหรับทุกชุดข้อมูลใน IRW:

- **เปิดกว้าง (Open)** ชุดข้อมูลแต่ละชุดได้รับอนุญาตให้นำกลับมาใช้ใหม่ได้ มีการบันทึกที่มาไว้อย่างชัดเจน และโค้ดที่ใช้แปลงให้เป็นรูปแบบ IRW ก็เปิดเผยต่อสาธารณะ
- **ปรับให้เป็นมาตรฐานเดียวกัน (Harmonized)** ชุดข้อมูลแต่ละชุดถูกปรับให้อยู่ในโครงสร้างง่าย ๆ แบบเดียวกัน (อธิบายไว้ด้านล่าง) เพื่อให้โค้ดวิเคราะห์ชุดเดียวกันสามารถใช้ได้กับหลายชุดข้อมูลโดยแทบไม่ต้องแก้ไข หรือไม่ต้องแก้ไขเลย

ชุดข้อมูลมีความหลากหลายอย่างมากทั้งในด้านขนาด (ตั้งแต่ไม่กี่ร้อยคำตอบไปจนถึงหลายล้านคำตอบ) และประเภทของคำตอบ (ข้อสอบแบบใช่/ไม่ใช่ การให้คะแนนแบบหลายหมวดหมู่ คะแนนบางส่วน และอื่น ๆ) ชุดข้อมูลแต่ละชุดยังมาพร้อมกับข้อมูลเมทาดาทาที่คำนวณไว้ล่วงหน้า — จำนวนผู้เข้าร่วม จำนวนข้อสอบ ความหนาแน่นของคำตอบ สาขาวิชา และป้ายกำกับเชิงพรรณนาอื่น ๆ — เพื่อให้นักวิจัยสามารถค้นหาชุดข้อมูลที่เกี่ยวข้องได้โดยไม่ต้องดาวน์โหลดและประมวลผลทั้งหมดก่อน

## มาตรฐานข้อมูล

<img src="/welcome/assets/diagram-cross-classification.svg" alt="แผนภาพแสดงกริดสีที่มีเซลล์ id-item ซึ่งถูกแปลงผ่านลูกศรให้เป็นตารางรูปแบบยาวที่มีคอลัมน์ id, item และ resp โดยสีของแต่ละเซลล์ resp ตรงกับสีของเซลล์ต้นทางในกริด" class="welcome-figure">


ชุดข้อมูล IRW ทุกชุดจะถูกปรับให้อยู่ใน **รูปแบบยาว (long format)**: หนึ่งแถวต่อหนึ่งคำตอบ อย่างน้อยที่สุด แต่ละแถวจะมีข้อมูลสามส่วน ได้แก่

| คอลัมน์ | ความหมาย |
|---|---|
| `id` | ใคร (หรืออะไร) เป็นผู้ให้คำตอบ — โดยทั่วไปคือบุคคล |
| `item` | เครื่องมือวัดผลใดที่ก่อให้เกิดคำตอบนี้ — โดยทั่วไปคือคำถามหรือภารกิจ |
| `resp` | คำตอบเอง ซึ่งจัดเก็บเป็นคะแนนแบบอันดับ (ordinal) |

**ตัวอย่าง:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

เมื่อชุดข้อมูลมีข้อมูลเพิ่มเติม — เช่น เวลาในการตอบ ตัวตนของผู้ประเมิน หรือตัวแปรร่วมอย่างอายุ — ข้อมูลเหล่านั้นจะถูกจัดเก็บไว้ในคอลัมน์เพิ่มเติมที่ตั้งชื่ออย่างสอดคล้องกัน โครงสร้างง่าย ๆ เพียงแบบเดียวนี้ครอบคลุมสถานการณ์การวัดผลที่หลากหลายอย่างมาก ซึ่งเป็นสิ่งที่ทำให้สามารถเขียนโค้ดวิเคราะห์เพียงครั้งเดียวแล้วนำไปใช้กับคลังข้อมูลทั้งหมดได้

ข้อกำหนดทางเทคนิคฉบับเต็มของมาตรฐานนี้สามารถดูได้ที่ [itemresponsewarehouse.org/standard.html](/standard.qmd) นอกจากนี้ยังมีมาตรฐานที่เกี่ยวข้องและเฉพาะทางมากขึ้นสำหรับข้อความของข้อสอบ ข้อมูลการเปรียบเทียบแบบคู่ (pairwise) และคำตอบแบบนามบัญญัติ (nominal, หมวดหมู่ที่ไม่มีลำดับ)

## วิธีใช้งาน

มีสามวิธีในการรับข้อมูล IRW ขึ้นอยู่กับว่าคุณต้องการให้กระบวนการเป็นอัตโนมัติมากน้อยเพียงใด

**1. เรียกดูในเว็บเบราว์เซอร์**
สำรวจชุดข้อมูลและเมทาดาทาของชุดข้อมูลโดยตรงที่ [เบราว์เซอร์ข้อมูล IRW](/data.qmd) — ไม่ต้องมีบัญชี การดาวน์โหลดชุดข้อมูลแบบเต็มต้องใช้บัญชี [Redivis](https://redivis.com) ที่ไม่มีค่าใช้จ่าย เนื่องจากเป็นแพลตฟอร์มที่จัดเก็บข้อมูลต้นทาง

**2. ใช้แพ็กเกจ `irw` (แนะนำ)**
แพ็กเกจ `irw` ซึ่งมีให้ใช้งานทั้งใน **R** และ **Python** มีฟังก์ชันง่าย ๆ สำหรับการค้นหา กรอง และดาวน์โหลดข้อมูล

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

ครั้งแรกที่คุณใช้แพ็กเกจนี้ ระบบจะขอให้คุณเข้าสู่ระบบด้วยบัญชี Redivis ที่ไม่มีค่าใช้จ่าย หลังจากนั้น โค้ดเพียงบรรทัดเดียวจะดาวน์โหลดชุดข้อมูลใดก็ได้เข้าสู่ R หรือ Python โดยตรง จากจุดนี้ ข้อมูลก็พร้อมสำหรับการวิเคราะห์ด้วยซอฟต์แวร์มาตรฐาน — เช่น แพ็กเกจทฤษฎีการตอบสนองข้อสอบ (item response theory) หรือการวิเคราะห์องค์ประกอบ (factor analysis)

**3. ใช้ไลบรารีไคลเอนต์ของ Redivis โดยตรง**
สำหรับขั้นตอนการทำงานระดับล่างหรือที่ไม่ได้ใช้ R/Python ก็สามารถเข้าถึงข้อมูลได้ผ่านไลบรารีไคลเอนต์ R และ Python ของ Redivis เองเช่นกัน ดูรายละเอียดได้ที่ [คู่มือเริ่มต้นใช้งาน](/getstarted.qmd)

### เหนือกว่าการดาวน์โหลดข้อมูล

โครงการ IRW ยังประกอบด้วย:

- ชุด **[บทความตัวอย่าง (vignettes)](/vignettes/index.qmd)** ที่กำลังเติบโตขึ้นเรื่อย ๆ ซึ่งนำวิธีการวัดผลแบบดั้งเดิมและแบบใหม่มาประยุกต์ใช้กับชุดข้อมูล IRW จำนวนมากพร้อมกัน
- **แหล่งข้อมูลสำหรับการฝึกอบรมและชุดแบบฝึกหัด** สำหรับการสอนจิตวิทยาการวัดด้วยข้อมูลจริง
- **กระบวนการมีส่วนร่วม** สำหรับนักวิจัยที่ต้องการเพิ่มชุดข้อมูลของตนเองเข้าสู่คลังข้อมูล

## เรียนรู้เพิ่มเติม

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- เว็บไซต์: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- โค้ด: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

หากคุณใช้ข้อมูล IRW ในงานของคุณ โปรดอ้างอิงข้อมูลต้นฉบับ (เราได้จัดเตรียมฟังก์ชันสำหรับการดำเนินการนี้ไว้แล้ว) และจะเป็นประโยชน์อย่างยิ่งหากคุณกรุณาอ้างอิงบทความแนะนำข้างต้นด้วยเช่นกัน

---

*มีคำถาม ข้อเสนอแนะ หรือต้องการร่วมส่งชุดข้อมูลหรือไม่? เยี่ยมชม [หน้าติดต่อเรา](/contact.qmd) หรือเปิดประเด็นปัญหา (issue) บน [GitHub](https://github.com/itemresponsewarehouse)*
