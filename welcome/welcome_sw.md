---
lang: sw
pagetitle: "Item Response Warehouse (IRW; Ghala la Majibu ya Vipengele)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Ghala la Majibu ya Vipengele)

**Mkusanyiko huru na wazi wa data ya majibu ya vipengele (item response) iliyosanifishwa, kwa ajili ya utafiti wa saikometriki na upimaji.**

[Soma karatasi](https://doi.org/10.3758/s13428-025-02796-y) **(ufikiaji huria)**

{{< include /welcome/_lang_switch.md >}}

---

## Kwa nini IRW ipo

Watafiti wanaosoma upimaji — katika elimu, saikolojia, na fani zinazohusiana — wanahitaji data halisi ili kujaribu na kulinganisha mbinu zao. Data hiyo tayari ipo kwa wingi mkubwa. Lakini imetawanyika katika tafiti nyingi, imehifadhiwa katika miundo mingi tofauti, na mara nyingi ni ngumu kuitumia tena kwa sababu ya nyaraka au leseni zisizo wazi.

Hii ni tatizo linalojulikana sana. Jumuiya ya utafiti imeweka kiwango cha pamoja cha kulitatua: data inapaswa kuwa **FAIR** — inayopatikana kwa urahisi (Findable), inayofikiwa (Accessible), inayoweza kutumika pamoja na mifumo mingine (Interoperable), na inayoweza kutumika tena (Reusable) (Wilkinson et al., 2016). Fani nyingine zimeweka kanuni hizi katika vitendo kwa kujenga rasilimali za data zilizoshirikiwa na kusanifishwa. Katika sayansi ya kompyuta, mkusanyiko wa picha zilizowekwa lebo wa ImageNet uliwapa watafiti kigezo cha kawaida (benchmark) na ukasaidia kuchochea maendeleo ya haraka katika AI. Jenetiki na sayansi ya neva zimejenga rasilimali za pamoja zinazofanana kwa data zao wenyewe.

Item Response Warehouse (IRW) inatumia kanuni hizohizo za FAIR kwa data ya majibu ya vipengele. Inakusanya maelfu ya seti za data zilizopo na kuzibadilisha kuwa muundo mmoja wa pamoja — ili mbinu iliyojaribiwa kwenye seti moja ya data iweze kujaribiwa kwa urahisi kwenye maelfu ya seti nyingine.

## Kilichomo ndani ya IRW

IRW ina **maelfu ya seti za data** ("majedwali"), kila moja ikiwa mkusanyiko wa majibu ya mtu mmoja mmoja. Jibu hutokea kila mara mtu (au kitengo kingine) anapokutana na kifaa cha upimaji na matokeo yakarekodiwa. Kinachohesabiwa kuwa kifaa cha upimaji hutofautiana kulingana na [familia ya data](/overview.qmd): kipengele kinachojibiwa peke yake, kitu kingine kinachokutana nacho katika mashindano, seti nzima ya chaguo inayopaswa kupangwa kwa mpangilio, au wasifu uliojengwa kutokana na sifa zilizotolewa kwa nasibu. Mifano ni pamoja na:

- Majibu ya wanafunzi katika mitihani ya elimu na uwezo
- Vipengele vya dodoso vinavyopima utu au mitazamo
- Alama zinazotolewa na wakadiriaji binadamu
- Mechi, michezo na ulinganishaji wa moja kwa moja, ambapo vitu viwili hukutana na kimoja hushinda
- Karatasi za kupiga kura kwa mpangilio wa upendeleo, upangaji wa majaji, na maswali ya kupanga katika tafiti
- Majaribio ya conjoint, ambapo wahojiwa huchagua kati ya wasifu uliojengwa kwa nasibu
- Hali nyingine yoyote inayohusisha majibu yanayojirudia kwa seti ya vifaa vya upimaji

Kila seti ya data katika IRW imeundwa iwe:

- **Inayopatikana kwa urahisi (Findable).** Kila seti ya data inakuja na metadata iliyokokotolewa tayari — idadi ya washiriki, idadi ya vipengele, msongamano wa majibu, uwanja wa somo, na lebo nyingine za maelezo — ili seti za data ziweze kupatikana na kuchujwa bila kuzipakua kwanza.
- **Inayofikiwa (Accessible).** Kila seti ya data inaweza kupatikana kupitia kivinjari cha wavuti au kifurushi cha `irw`, kwa akaunti ya bure.
- **Inayoweza kutumika pamoja na mifumo mingine (Interoperable).** Kila seti ya data hubadilishwa kuwa muundo mrefu, safu moja kwa kila jibu, kufuatana na kiwango cha familia yake (kilichoelezwa hapa chini), ili msimbo huohuo wa uchambuzi uweze kutumika kwenye seti nyingi za data kwa mabadiliko kidogo au bila mabadiliko yoyote.
- **Inayoweza kutumika tena (Reusable).** Kila seti ya data ina leseni huria, asili yake imeandikwa, na msimbo uliotumika kuibadilisha kuwa muundo wa IRW ni wa umma.

Seti za data hutofautiana sana kwa ukubwa (kutoka majibu mia chache hadi mamilioni mengi) na kwa aina ya majibu (vipengele vya ndiyo/hapana, ukadiriaji wa makundi mengi, alama za sehemu, makundi yasiyo na mpangilio, matokeo ya mashindano, nafasi za upangaji, na mengineyo).

## Kiwango cha data

Kila jedwali la IRW liko katika **muundo mrefu**: safu moja kwa kila jibu. Safu moja *ni nini* hutegemea familia ambayo jedwali hilo ni mali yake, na vivyo hivyo safu wima zinazoibeba.

<img src="/figures/families/families-tree.svg" alt="Maswali mawili yanayoweka seti ya data katika familia ya IRW."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| Familia | `source =` | Kiwango |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn haiko kwenye chati: familia hii hufafanuliwa na chanzo cha data, si umbo lake, hivyo jedwali la kuiga linaweza kuwa na umbo lolote kati ya haya. Safu wima za kila familia zimeorodheshwa katika [muhtasari wa viwango](/overview.qmd).


## Jinsi ya kuitumia

Kuna njia tatu za kupata data ya IRW, kutegemea unavyotaka kuchakata kiotomatiki.

**1. Vinjari kwenye kivinjari cha wavuti**
Chunguza seti za data na metadata yake moja kwa moja kwenye [kivinjari cha data cha IRW](/data.qmd) — hakuna akaunti inayohitajika. Kupakua seti kamili ya data kunahitaji akaunti huru ya [Redivis](https://redivis.com), kwani hicho ndicho jukwaa linalohifadhi data ya msingi.

**2. Tumia kifurushi cha `irw` (kinachopendekezwa)**
Kifurushi cha `irw` kinapatikana kwa **R** na **Python**.

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

Mara ya kwanza kutumia kifurushi hiki, utaombwa kuingia (login) kwa akaunti huru ya Redivis.

**3. Tumia moja kwa moja maktaba za mteja za Redivis**
Kwa mtiririko wa kazi wa kiwango cha chini zaidi au usio wa R/Python, data pia inaweza kupatikana kupitia maktaba za mteja za Redivis za R na Python. Angalia [Mwongozo wa Kuanza](/getstarted.qmd) kwa maelezo zaidi.

### Zaidi ya kupakua data

Mradi wa IRW pia unajumuisha:

- Mkusanyiko unaokua wa **[mifano ya kina (vignettes)](/vignettes/index.qmd)** — mifano iliyofanyiwa kazi inayotumia mbinu za kawaida na mpya za upimaji kwenye seti nyingi za data za IRW kwa wakati mmoja
- **Rasilimali za mafunzo na seti za mazoezi** kwa ajili ya kufundisha saikometriki kwa kutumia data halisi
- **Mchakato wa kuchangia** kwa watafiti wanaotaka kuongeza seti zao za data kwenye ghala

## Jifunze zaidi

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Tovuti: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Msimbo: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Ikiwa unatumia data ya IRW katika kazi yako, tafadhali taja (cite) data ya asili (tumetoa vitendaji vinavyowezesha hili). Tungefurahi pia kama ungetaja karatasi ya utangulizi iliyotajwa hapo juu.

---

*Una maswali, maoni, au unataka kuchangia seti ya data? Tembelea [ukurasa wa Mawasiliano](/contact.qmd) au fungua suala (issue) kwenye [GitHub](https://github.com/itemresponsewarehouse).*
