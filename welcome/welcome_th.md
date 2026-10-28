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

{{< include /welcome/_lang_switch.md >}}

---

## เหตุใด IRW จึงมีอยู่

นักวิจัยที่ศึกษาเรื่องการวัดผล — ทั้งในด้านการศึกษา จิตวิทยา และสาขาที่เกี่ยวข้อง — ต้องการข้อมูลจริงเพื่อทดสอบและเปรียบเทียบวิธีการของตน ข้อมูลลักษณะนี้มีอยู่แล้วเป็นจำนวนมาก แต่กระจัดกระจายอยู่ในงานวิจัยหลายชิ้น ถูกจัดเก็บในรูปแบบที่แตกต่างกันมากมาย และมักนำกลับมาใช้ใหม่ได้ยากเนื่องจากเอกสารประกอบหรือสัญญาอนุญาตที่ไม่ชัดเจน

นี่เป็นปัญหาที่รู้จักกันดี ชุมชนนักวิจัยได้กำหนดมาตรฐานร่วมกันเพื่อแก้ปัญหานี้ นั่นคือข้อมูลควรมีคุณสมบัติ **FAIR** — ค้นพบได้ (Findable) เข้าถึงได้ (Accessible) ทำงานร่วมกันได้ (Interoperable) และนำกลับมาใช้ใหม่ได้ (Reusable) (Wilkinson et al., 2016) สาขาอื่น ๆ ได้นำหลักการเหล่านี้ไปปฏิบัติจริงด้วยการสร้างแหล่งข้อมูลที่ใช้ร่วมกันและเป็นมาตรฐานเดียวกัน ในวิทยาการคอมพิวเตอร์ ชุดข้อมูลภาพที่ติดป้ายกำกับอย่าง ImageNet ได้มอบมาตรฐานเปรียบเทียบ (benchmark) ร่วมกันให้แก่นักวิจัย และช่วยขับเคลื่อนความก้าวหน้าอย่างรวดเร็วในด้าน AI พันธุศาสตร์และประสาทวิทยาศาสตร์ก็ได้สร้างแหล่งข้อมูลร่วมที่คล้ายกันสำหรับข้อมูลของตนเอง

Item Response Warehouse (IRW) นำหลักการ FAIR เดียวกันนี้มาใช้กับข้อมูลการตอบสนองต่อข้อสอบ โดยรวบรวมชุดข้อมูลที่มีอยู่แล้วหลายพันชุดและปรับให้อยู่ในรูปแบบเดียวกัน — เพื่อให้วิธีการที่ทดสอบกับชุดข้อมูลหนึ่งสามารถนำไปทดสอบกับชุดข้อมูลอื่นอีกหลายพันชุดได้อย่างง่ายดาย

## สิ่งที่มีอยู่ใน IRW

IRW ประกอบด้วย **ชุดข้อมูลหลายพันชุด** ("ตาราง") แต่ละชุดเป็นการรวบรวมคำตอบของแต่ละบุคคล คำตอบหนึ่งจะเกิดขึ้นทุกครั้งที่บุคคล (หรือหน่วยอื่น) พบกับแบบวัดหนึ่งและมีการบันทึกผลลัพธ์ สิ่งที่นับเป็นแบบวัดนั้นแตกต่างกันไปตาม[ตระกูลข้อมูล](/overview.qmd) ได้แก่ ข้อสอบที่ตอบทีละข้อ อีกหน่วยหนึ่งที่พบในการแข่งขัน ชุดของตัวเลือกทั้งหมดที่ต้องเรียงลำดับ หรือโปรไฟล์ที่ประกอบขึ้นจากคุณลักษณะที่กำหนดแบบสุ่ม ตัวอย่างเช่น

- คำตอบของนักเรียนในแบบทดสอบทางการศึกษาและความสามารถ
- ข้อคำถามในแบบสอบถามที่วัดบุคลิกภาพหรือทัศนคติ
- คะแนนที่ผู้ประเมินซึ่งเป็นมนุษย์ให้ไว้
- การแข่งขัน การประลอง และการเปรียบเทียบโดยตรง ซึ่งสองฝ่ายพบกันและมีผู้ชนะหนึ่งฝ่าย
- บัตรลงคะแนนแบบจัดลำดับ การจัดอันดับโดยคณะกรรมการ และคำถามแบบจัดอันดับในแบบสำรวจ
- การทดลองแบบ conjoint ที่ผู้ตอบเลือกระหว่างโปรไฟล์ที่สร้างขึ้นแบบสุ่ม
- สถานการณ์อื่นใดที่เกี่ยวข้องกับการตอบสนองซ้ำ ๆ ต่อชุดเครื่องมือวัดผล

ชุดข้อมูลทุกชุดใน IRW ได้รับการออกแบบให้:

- **ค้นพบได้ (Findable)** ชุดข้อมูลแต่ละชุดมาพร้อมกับข้อมูลเมทาดาทาที่คำนวณไว้ล่วงหน้า — จำนวนผู้เข้าร่วม จำนวนข้อสอบ ความหนาแน่นของคำตอบ สาขาวิชา และป้ายกำกับเชิงพรรณนาอื่น ๆ — เพื่อให้สามารถค้นหาและกรองชุดข้อมูลได้โดยไม่ต้องดาวน์โหลดก่อน
- **เข้าถึงได้ (Accessible)** ชุดข้อมูลแต่ละชุดสามารถรับได้ผ่านเว็บเบราว์เซอร์หรือแพ็กเกจ `irw` โดยใช้บัญชีฟรี
- **ทำงานร่วมกันได้ (Interoperable)** ชุดข้อมูลแต่ละชุดถูกปรับให้อยู่ในรูปแบบยาว หนึ่งแถวต่อหนึ่งคำตอบ ตามมาตรฐานของตระกูลนั้น (อธิบายไว้ด้านล่าง) เพื่อให้โค้ดวิเคราะห์ชุดเดียวกันสามารถใช้ได้กับหลายชุดข้อมูลโดยแทบไม่ต้องแก้ไข หรือไม่ต้องแก้ไขเลย
- **นำกลับมาใช้ใหม่ได้ (Reusable)** ชุดข้อมูลแต่ละชุดได้รับอนุญาตแบบเปิด มีการบันทึกที่มาไว้อย่างชัดเจน และโค้ดที่ใช้แปลงให้เป็นรูปแบบ IRW ก็เปิดเผยต่อสาธารณะ

ชุดข้อมูลมีความหลากหลายอย่างมากทั้งในด้านขนาด (ตั้งแต่ไม่กี่ร้อยคำตอบไปจนถึงหลายล้านคำตอบ) และประเภทของคำตอบ (ข้อสอบแบบใช่/ไม่ใช่ การให้คะแนนแบบหลายหมวดหมู่ คะแนนบางส่วน หมวดหมู่ที่ไม่มีลำดับ ผลการแข่งขัน อันดับที่จัดไว้ และอื่น ๆ)

## มาตรฐานข้อมูล

ตาราง IRW ทุกตารางอยู่ใน**รูปแบบยาว** คือหนึ่งแถวต่อหนึ่งคำตอบ สิ่งที่หนึ่งแถว*เป็น*ขึ้นอยู่กับตระกูลที่ตารางนั้นสังกัดอยู่ และคอลัมน์ที่รองรับก็เช่นกัน

<img src="/figures/families/families-tree.svg" alt="สองคำถามที่จัดชุดข้อมูลเข้าตระกูลของ IRW"
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| ตระกูล | `source =` | มาตรฐาน |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn ไม่ปรากฏในแผนภูมิ เพราะตระกูลนี้ถูกกำหนดด้วยที่มาของข้อมูล ไม่ใช่รูปร่าง ตารางจำลองจึงมีรูปร่างแบบใดก็ได้ในกลุ่มนี้ คอลัมน์ของแต่ละตระกูลดูได้ที่[ภาพรวมมาตรฐาน](/overview.qmd)


## วิธีใช้งาน

มีสามวิธีในการรับข้อมูล IRW ขึ้นอยู่กับว่าคุณต้องการให้กระบวนการเป็นอัตโนมัติมากน้อยเพียงใด

**1. เรียกดูในเว็บเบราว์เซอร์**
สำรวจชุดข้อมูลและเมทาดาทาของชุดข้อมูลโดยตรงที่ [เบราว์เซอร์ข้อมูล IRW](/data.qmd) — ไม่ต้องมีบัญชี การดาวน์โหลดชุดข้อมูลแบบเต็มต้องใช้บัญชี [Redivis](https://redivis.com) ที่ไม่มีค่าใช้จ่าย เนื่องจากเป็นแพลตฟอร์มที่จัดเก็บข้อมูลต้นทาง

**2. ใช้แพ็กเกจ `irw` (แนะนำ)**
แพ็กเกจ `irw` มีให้ใช้งานทั้งใน **R** และ **Python**

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

ครั้งแรกที่คุณใช้แพ็กเกจนี้ ระบบจะขอให้คุณเข้าสู่ระบบด้วยบัญชี Redivis ที่ไม่มีค่าใช้จ่าย

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
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- เว็บไซต์: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- โค้ด: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

หากคุณใช้ข้อมูล IRW ในงานของคุณ โปรดอ้างอิงข้อมูลต้นฉบับ (เราได้จัดเตรียมฟังก์ชันสำหรับการดำเนินการนี้ไว้แล้ว) และจะเป็นประโยชน์อย่างยิ่งหากคุณกรุณาอ้างอิงบทความแนะนำข้างต้นด้วยเช่นกัน

---

*มีคำถาม ข้อเสนอแนะ หรือต้องการร่วมส่งชุดข้อมูลหรือไม่? เยี่ยมชม [หน้าติดต่อเรา](/contact.qmd) หรือเปิดประเด็นปัญหา (issue) บน [GitHub](https://github.com/itemresponsewarehouse)*
