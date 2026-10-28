---
lang: nl
pagetitle: "Item Response Warehouse (IRW; Itemresponsmagazijn)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Itemresponsmagazijn)

**Een gratis, open verzameling van geharmoniseerde itemresponsdata voor psychometrisch en meetkundig onderzoek.**

[Lees het artikel](https://doi.org/10.3758/s13428-025-02796-y) **(open access)**

{{< include /welcome/_lang_switch.md >}}

---

## Waarom het IRW bestaat

Onderzoekers die meting bestuderen — in de onderwijskunde, psychologie en verwante vakgebieden — hebben echte data nodig om hun methoden te testen en te vergelijken. Die data bestaat al in grote hoeveelheden. Maar ze is verspreid over veel studies, opgeslagen in veel verschillende formaten, en vaak lastig te hergebruiken door onduidelijke documentatie of licenties.

Dit is een bekend probleem. De onderzoeksgemeenschap heeft een gedeelde norm geformuleerd om het op te lossen: data moet **FAIR** zijn — vindbaar (Findable), toegankelijk (Accessible), interoperabel (Interoperable) en herbruikbaar (Reusable) (Wilkinson et al., 2016). Andere vakgebieden hebben deze principes in de praktijk gebracht door gedeelde, gestandaardiseerde databronnen te bouwen. In de informatica gaf de ImageNet-verzameling van gelabelde afbeeldingen onderzoekers een gemeenschappelijke benchmark en hielp dit de snelle vooruitgang in AI aan te jagen. Genetica en neurowetenschappen hebben vergelijkbare gedeelde bronnen gebouwd voor hun eigen data.

Het Item Response Warehouse (IRW) past dezelfde FAIR-principes toe op itemresponsdata. Het brengt duizenden bestaande datasets samen en vormt ze om naar één gemeenschappelijk formaat — zodat een methode die op één dataset is getest, gemakkelijk op duizenden andere kan worden getest.

## Wat zit er in het IRW

Het IRW bevat **duizenden datasets** ("tabellen"), elk een verzameling van individuele responsen. Een respons ontstaat wanneer een persoon (of een andere eenheid) een meetopgave tegenkomt en de uitkomst wordt vastgelegd. Wat als meetopgave geldt, verschilt per [datafamilie](/overview.qmd): een item dat op zichzelf wordt beantwoord, een andere eenheid die in een confrontatie wordt ontmoet, een volledige reeks opties die op volgorde wordt gezet, of een profiel dat uit willekeurig toegewezen kenmerken is samengesteld. Voorbeelden zijn onder meer:

- Antwoorden van leerlingen op onderwijs- en vaardigheidstoetsen
- Vragenlijstitems die persoonlijkheid of attitudes meten
- Beoordelingen gegeven door menselijke beoordelaars
- Wedstrijden, partijen en directe vergelijkingen, waarbij twee eenheden tegenover elkaar staan en er één wint
- Voorkeursstembiljetten, juryranglijsten en rangschikkingsvragen in vragenlijsten
- Conjoint-experimenten, waarbij respondenten kiezen tussen willekeurig samengestelde profielen
- Elke andere situatie met herhaalde responsen op een reeks meetinstrumenten

Elke dataset in het IRW is ontworpen om te voldoen aan vier eigenschappen:

- **Vindbaar (Findable).** Elke dataset wordt geleverd met vooraf berekende metadata — aantal deelnemers, aantal items, responsdichtheid, vakgebied en andere beschrijvende labels — zodat datasets kunnen worden gevonden en gefilterd zonder ze eerst te downloaden.
- **Toegankelijk (Accessible).** Elke dataset kan worden opgehaald via de webbrowser of het `irw`-package, met een gratis account.
- **Interoperabel (Interoperable).** Elke dataset wordt omgevormd naar het lange formaat, één rij per respons, volgens de standaard van zijn familie (hieronder beschreven), zodat dezelfde analysecode met weinig of geen aanpassing op veel datasets kan worden toegepast.
- **Herbruikbaar (Reusable).** Elke dataset is openlijk gelicentieerd, de herkomst ervan is gedocumenteerd, en de code die is gebruikt om de dataset naar het IRW-formaat om te zetten, is openbaar.

Datasets verschillen sterk in omvang (van enkele honderden responsen tot vele miljoenen) en in responstype (ja/nee-items, meerkeuzebeoordelingen, deelscores, ongeordende categorieën, wedstrijduitslagen, rangposities, en meer).

## De datastandaard

Elke IRW-tabel staat in het **lange formaat**: één rij per respons. Wat een rij *is*, hangt af van de familie waartoe de tabel behoort, en dat geldt ook voor de kolommen die haar dragen.

<img src="/figures/families/families-tree.svg" alt="Twee vragen die een dataset in een IRW-familie plaatsen."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| Familie | `source =` | Standaard |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn staat niet in het schema: die familie wordt bepaald door waar de data vandaan komen, niet door hun vorm, zodat een gesimuleerde tabel elk van deze vormen kan hebben. Het [overzicht van de standaarden](/overview.qmd) geeft de kolommen van elke familie.


## Hoe te gebruiken

Er zijn drie manieren om IRW-data te verkrijgen, afhankelijk van hoeveel u wilt automatiseren.

**1. Bekijk in de webbrowser**
Verken datasets en hun metadata rechtstreeks in de [IRW-databrowser](/data.qmd) — geen account nodig. Voor het downloaden van een volledige dataset is een gratis [Redivis](https://redivis.com)-account nodig, aangezien dat het platform is dat de onderliggende data host.

**2. Gebruik het `irw`-pakket (aanbevolen)**
Het `irw`-pakket is beschikbaar voor zowel **R** als **Python**.

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

De eerste keer dat u het pakket gebruikt, wordt u gevraagd in te loggen met een gratis Redivis-account.

**3. Gebruik de Redivis-clientbibliotheken rechtstreeks**
Voor lager-niveau workflows of workflows buiten R/Python is de data ook toegankelijk via Redivis' eigen R- en Python-clientbibliotheken. Zie de [Aan de slag-gids](/getstarted.qmd) voor details.

### Meer dan alleen data downloaden

Het IRW-project omvat ook:

- Een groeiende reeks **[vignetten](/vignettes/index.qmd)** — uitgewerkte voorbeelden die klassieke en nieuwe meetmethoden tegelijk toepassen op veel IRW-datasets
- **Onderwijsmateriaal en opgavensets** voor het onderwijzen van psychometrie met echte data
- Een **bijdrageproces** voor onderzoekers die hun eigen datasets aan het magazijn willen toevoegen

## Meer informatie

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Website: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Als u IRW-data in uw werk gebruikt, citeer dan alstublieft de oorspronkelijke data (wij hebben hiervoor de nodige functionaliteit geleverd). Het zou ook zeer op prijs worden gesteld als u het bovenstaande introductieartikel citeert.

---

*Vragen, feedback, of wilt u een dataset bijdragen? Bezoek de [contactpagina](/contact.qmd) of open een issue op [GitHub](https://github.com/itemresponsewarehouse).*
