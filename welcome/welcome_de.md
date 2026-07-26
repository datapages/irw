---
lang: de
pagetitle: "Das Item Response Warehouse"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher link line below (English · Français · ...) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate or reorder it.
-->

# Das Item Response Warehouse

**Eine freie, offene Sammlung harmonisierter Item-Response-Daten für die psychometrische und messtheoretische Forschung.**

[itemresponsewarehouse.org](https://itemresponsewarehouse.org) · [GitHub](https://github.com/itemresponsewarehouse) · [Artikel lesen](https://doi.org/10.3758/s13428-025-02796-y)

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html) · [繁體中文](/welcome/welcome_zh-hant.html) · [বাংলা](/welcome/welcome_bn.html) · [Türkçe](/welcome/welcome_tr.html) · [Tiếng Việt](/welcome/welcome_vi.html)

---

## Warum es das IRW gibt

Forschende, die sich mit Messung beschäftigen — in der Bildungsforschung, Psychologie und verwandten Bereichen — benötigen echte Daten, um ihre Methoden zu testen und zu vergleichen. Solche Daten existieren bereits in großer Menge. Sie sind jedoch über viele Studien verstreut, in vielen unterschiedlichen Formaten gespeichert und aufgrund unklarer Dokumentation oder Lizenzierung oft schwer wiederzuverwenden.

Dies ist ein bekanntes Problem. Andere Fachbereiche haben es gelöst, indem sie gemeinsame, standardisierte Datenressourcen aufgebaut haben. In der Informatik bot die Sammlung beschrifteter Bilder ImageNet Forschenden einen gemeinsamen Bezugspunkt und trug zu raschen Fortschritten in der KI bei. Auch die Genetik und die Neurowissenschaften haben ähnliche gemeinsame Ressourcen für ihre eigenen Daten aufgebaut.

Das Item Response Warehouse (IRW) tut dasselbe für Item-Response-Daten. Es bringt Hunderte bestehender Datensätze zusammen, formt sie in ein gemeinsames Format um und macht sie frei zugänglich an einem einzigen Ort — sodass eine Methode, die an einem Datensatz getestet wurde, leicht an Hunderten anderer getestet werden kann.

## Was das IRW enthält

Das IRW enthält **Hunderte von Datensätzen** ("Tabellen"), von denen jeder eine Sammlung individueller Antworten ist. Eine Antwort entsteht immer dann, wenn eine Person (oder eine andere Einheit) auf ein Item (oder eine andere Messsonde) reagiert. Beispiele sind:

- Antworten von Schülerinnen und Schülern in Bildungs- und Leistungstests
- Fragebogenitems zur Messung von Persönlichkeit oder Einstellungen
- Bewertungen, die von menschlichen Beurteilenden vergeben werden
- Jeder andere Kontext mit wiederholten Antworten auf eine Reihe von Messsonden

Für jeden Datensatz im IRW gelten zwei Dinge:

- **Offen.** Jeder Datensatz ist für die Weiterverwendung lizenziert. Seine Herkunft ist dokumentiert, und der Code, der zur Umwandlung in das IRW-Format verwendet wurde, ist öffentlich zugänglich.
- **Harmonisiert.** Jeder Datensatz wird in dieselbe einfache Struktur umgeformt (unten beschrieben), sodass derselbe Analysecode mit wenig oder gar keiner Anpassung auf viele Datensätze angewendet werden kann.

Die Datensätze variieren stark in ihrer Größe (von einigen Hundert Antworten bis zu mehreren Millionen) und im Antworttyp (Ja/Nein-Items, mehrkategoriale Bewertungen, Teilpunktzahlen und mehr). Jeder Datensatz kommt zudem mit vorab berechneten Metadaten — Anzahl der Teilnehmenden, Anzahl der Items, Antwortdichte, Themengebiet und weitere beschreibende Tags —, sodass Forschende relevante Datensätze finden können, ohne zunächst alle herunterladen und verarbeiten zu müssen.

## Der Datenstandard

Jeder IRW-Datensatz wird in ein **Long-Format** umgeformt: eine Zeile pro Antwort. Jede Zeile enthält mindestens drei Informationen:

| Spalte | Bedeutung |
|---|---|
| `id` | Wer (oder was) die Antwort erzeugt hat — in der Regel eine Person |
| `item` | Welche Messsonde die Antwort erzeugt hat — in der Regel eine Frage oder Aufgabe |
| `resp` | Die Antwort selbst, gespeichert als ordinaler Wert |

**Beispiel:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Wenn ein Datensatz zusätzliche Informationen enthält — etwa Antwortzeit, Identität der beurteilenden Person oder Kovariaten wie das Alter —, werden diese Informationen in zusätzlichen, einheitlich benannten Spalten gespeichert. Diese eine einfache Struktur deckt eine enorme Bandbreite an Messsituationen ab, was es ermöglicht, Analysecode nur einmal zu schreiben und auf das gesamte Warehouse anzuwenden.

Die vollständige technische Spezifikation des Standards ist verfügbar unter [itemresponsewarehouse.org/standard.html](/standard.qmd). Es gibt außerdem spezialisiertere Standards für Item-Text, paarweise Vergleichsdaten (Wettbewerbsdaten) und nominale (ungeordnete) Antworten.

## Wie man es nutzt

Es gibt drei Möglichkeiten, an IRW-Daten zu gelangen, je nachdem, wie viel Sie automatisieren möchten.

**1. Im Webbrowser durchsuchen**
Erkunden Sie Datensätze und ihre Metadaten direkt im [IRW-Datenbrowser](/data.qmd) — kein Konto erforderlich. Zum Herunterladen eines vollständigen Datensatzes ist ein kostenloses [Redivis](https://redivis.com)-Konto erforderlich, da diese Plattform die zugrunde liegenden Daten hostet.

**2. Das `irw`-Paket verwenden (empfohlen)**
Das `irw`-Paket, verfügbar sowohl für **R** als auch für **Python**, bietet einfache Funktionen zum Auffinden, Filtern und Herunterladen von Daten.

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

Beim ersten Gebrauch des Pakets werden Sie aufgefordert, sich mit einem kostenlosen Redivis-Konto anzumelden. Danach lädt eine einzige Codezeile jeden beliebigen Datensatz direkt in R oder Python herunter. Von da an sind die Daten bereit für die Analyse mit gängiger Software — etwa Paketen für Item-Response-Theorie oder Faktorenanalyse.

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
- Website: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Wenn Sie IRW-Daten in Ihrer Arbeit verwenden, zitieren Sie bitte die Originaldaten (wir stellen dafür eine Funktion bereit). Es wäre außerdem großartig, wenn Sie auch den oben genannten Einführungsartikel zitieren würden.

---

*Fragen, Feedback, oder möchten Sie einen Datensatz beisteuern? Besuchen Sie die [Kontaktseite](/contact.qmd) oder eröffnen Sie ein Issue auf [GitHub](https://github.com/itemresponsewarehouse).*
