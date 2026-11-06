---
lang: de
pagetitle: "Das Item Response Warehouse (IRW; Lagerhaus für Item-Antworten)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Das Item Response Warehouse (IRW; Lagerhaus für Item-Antworten)

**Eine freie, offene Sammlung harmonisierter Item-Response-Daten für die psychometrische und messtheoretische Forschung.**

[Artikel lesen](https://doi.org/10.3758/s13428-025-02796-y) **(Open Access)**

{{< include /welcome/_lang_switch.md >}}

---

## Warum es das IRW gibt

Forschende, die sich mit Messung beschäftigen — in der Bildungsforschung, Psychologie und verwandten Bereichen — benötigen echte Daten, um ihre Methoden zu testen und zu vergleichen. Solche Daten existieren bereits in großer Menge. Sie sind jedoch über viele Studien verstreut, in vielen unterschiedlichen Formaten gespeichert und aufgrund unklarer Dokumentation oder Lizenzierung oft schwer wiederzuverwenden.

Dies ist ein bekanntes Problem. Die Forschungsgemeinschaft hat einen gemeinsamen Standard zu dessen Lösung formuliert: Daten sollen **FAIR** sein — auffindbar (Findable), zugänglich (Accessible), interoperabel (Interoperable) und wiederverwendbar (Reusable) (Wilkinson et al., 2016). Andere Fachbereiche haben diese Prinzipien in die Praxis umgesetzt, indem sie gemeinsame, standardisierte Datenressourcen aufgebaut haben. In der Informatik bot die Sammlung beschrifteter Bilder ImageNet Forschenden einen gemeinsamen Bezugspunkt und trug zu raschen Fortschritten in der KI bei. Auch die Genetik und die Neurowissenschaften haben ähnliche gemeinsame Ressourcen für ihre eigenen Daten aufgebaut.

Das Item Response Warehouse (IRW) wendet dieselben FAIR-Prinzipien auf Item-Response-Daten an. Es bringt Tausende bestehender Datensätze zusammen, formt sie in ein gemeinsames Format um und macht sie frei zugänglich an einem einzigen Ort — sodass eine Methode, die an einem Datensatz getestet wurde, leicht an Tausenden anderer getestet werden kann.

## Was das IRW enthält

Das IRW enthält **Tausende von Datensätzen** ("Tabellen"), von denen jeder eine Sammlung individueller Antworten ist. Eine Antwort entsteht immer dann, wenn eine Person (oder eine andere Einheit) auf eine Messsonde trifft und das Ergebnis festgehalten wird. Was als Messsonde zählt, hängt von der [Datenfamilie](/overview.qmd) ab: ein einzeln beantwortetes Item, eine andere Einheit, der man im Wettbewerb begegnet, ein ganzer Satz von Optionen, der in eine Reihenfolge gebracht wird, oder ein Profil, das aus zufällig zugewiesenen Merkmalen zusammengesetzt ist. Beispiele sind:

- Antworten von Schülerinnen und Schülern in Bildungs- und Leistungstests
- Fragebogenitems zur Messung von Persönlichkeit oder Einstellungen
- Bewertungen, die von menschlichen Beurteilenden vergeben werden
- Spiele, Partien und direkte Vergleiche, bei denen zwei Einheiten aufeinandertreffen und eine gewinnt
- Präferenzwahlzettel, Jury-Rangfolgen und Rangordnungsfragen in Befragungen
- Conjoint-Experimente, bei denen Befragte zwischen zufällig zusammengestellten Profilen wählen
- Jeder andere Kontext mit wiederholten Antworten auf eine Reihe von Messsonden

Jeder Datensatz im IRW ist darauf ausgelegt:

- **Auffindbar (Findable).** Jeder Datensatz kommt mit vorab berechneten Metadaten — Anzahl der Teilnehmenden, Anzahl der Items, Antwortdichte, Themengebiet und weitere beschreibende Tags —, sodass Datensätze gefunden und gefiltert werden können, ohne sie zuvor herunterzuladen.
- **Zugänglich (Accessible).** Jeder Datensatz kann über den Webbrowser oder das `irw`-Paket abgerufen werden, mit einem kostenlosen Konto.
- **Interoperabel (Interoperable).** Jeder Datensatz wird in das Langformat umgeformt, eine Zeile pro Antwort, nach dem Standard seiner Familie (unten beschrieben), sodass derselbe Analysecode mit wenig oder gar keiner Anpassung auf viele Datensätze angewendet werden kann.
- **Wiederverwendbar (Reusable).** Jeder Datensatz ist offen lizenziert, seine Herkunft ist dokumentiert, und der Code, der zur Umwandlung in das IRW-Format verwendet wurde, ist öffentlich zugänglich.

Die Datensätze variieren stark in ihrer Größe (von einigen Hundert Antworten bis zu mehreren Millionen) und im Antworttyp (Ja/Nein-Items, mehrkategoriale Bewertungen, Teilpunktzahlen, ungeordnete Kategorien, Wettkampfergebnisse, Rangplätze und mehr).

## Der Datenstandard

Jede IRW-Tabelle liegt im **Langformat** vor: eine Zeile pro Antwort. Was eine Zeile *ist*, hängt von der Familie ab, zu der die Tabelle gehört, und ebenso die Spalten, die sie tragen.

<img src="/figures/families/families-tree.svg" alt="Zwei Fragen, die einen Datensatz einer IRW-Familie zuordnen."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| Familie | `source =` | Standard |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn steht nicht im Diagramm: Diese Familie ist über die Herkunft der Daten definiert, nicht über ihre Form, sodass eine simulierte Tabelle jede dieser Formen haben kann. Die [Übersicht der Standards](/overview.qmd) nennt die Spalten jeder Familie.


## Wie man es nutzt

Es gibt drei Möglichkeiten, an IRW-Daten zu gelangen, je nachdem, wie viel Sie automatisieren möchten.

**1. Im Webbrowser durchsuchen**
Erkunden Sie Datensätze und ihre Metadaten direkt im [IRW-Datenbrowser](/data.qmd) — kein Konto erforderlich. Zum Herunterladen eines vollständigen Datensatzes ist ein kostenloses [Redivis](https://redivis.com)-Konto erforderlich, da diese Plattform die zugrunde liegenden Daten hostet.

**2. Das `irw`-Paket verwenden (empfohlen)**
Das `irw`-Paket ist sowohl für **R** als auch für **Python** verfügbar.

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

Beim ersten Gebrauch des Pakets werden Sie aufgefordert, sich mit einem kostenlosen Redivis-Konto anzumelden.

**3. Die Client-Bibliotheken von Redivis direkt verwenden**
Für Workflows auf niedrigerer Ebene oder außerhalb von R/Python kann auf die Daten auch über die eigenen R- und Python-Client-Bibliotheken von Redivis zugegriffen werden. Weitere Details finden Sie im [Einstiegsleitfaden](/getstarted.qmd).

### Mehr als nur Daten herunterladen

Das IRW-Projekt umfasst außerdem:

- Eine wachsende Sammlung von **[Vignetten](/vignettes/index.qmd)** — durchgearbeitete Beispiele, die klassische und neue Messmethoden gleichzeitig auf viele IRW-Datensätze anwenden
- **Schulungsmaterialien und Übungsaufgaben** für die Vermittlung von Psychometrie anhand echter Daten
- **Einen Beitragsprozess** für Forschende, die eigene Datensätze zum Warehouse hinzufügen möchten

## Mehr erfahren

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Website: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Wenn Sie IRW-Daten in Ihrer Arbeit verwenden, zitieren Sie bitte die Originaldaten (wir stellen dafür eine Funktion bereit). Es wäre außerdem großartig, wenn Sie auch den oben genannten Einführungsartikel zitieren würden.

---

*Fragen, Feedback, oder möchten Sie einen Datensatz beisteuern? Besuchen Sie die [Kontaktseite](/contact.qmd) oder eröffnen Sie ein Issue auf [GitHub](https://github.com/itemresponsewarehouse).*
