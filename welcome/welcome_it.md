---
lang: it
pagetitle: "Item Response Warehouse (IRW; Magazzino delle Risposte agli Item)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Magazzino delle Risposte agli Item)

**Una raccolta gratuita e aperta di dati di risposta agli item armonizzati per la psicometria e la ricerca sulla misurazione.**

[Leggi l'articolo](https://doi.org/10.3758/s13428-025-02796-y) **(accesso aperto)**

{{< include /welcome/_lang_switch.md >}}

---

## Perché esiste l'IRW

Chi si occupa di ricerca sulla misurazione — in ambito educativo, psicologico e nei campi affini — ha bisogno di dati reali per testare e confrontare i propri metodi. Questi dati esistono già in grandi quantità. Ma sono dispersi in molti studi diversi, conservati in formati eterogenei e spesso difficili da riutilizzare a causa di documentazione o licenze poco chiare.

È un problema ben noto. La comunità scientifica ha definito uno standard condiviso per risolverlo: i dati devono essere **FAIR** — reperibili (Findable), accessibili (Accessible), interoperabili (Interoperable) e riutilizzabili (Reusable) (Wilkinson et al., 2016). Altri campi hanno messo in pratica questi principi costruendo risorse di dati condivise e standardizzate. Nell'informatica, la raccolta di immagini etichettate ImageNet ha fornito ai ricercatori un benchmark comune e ha contribuito a un rapido progresso nell'IA. La genetica e le neuroscienze hanno costruito risorse condivise analoghe per i propri dati.

L'Item Response Warehouse (IRW) applica gli stessi principi FAIR ai dati di risposta agli item. Riunisce migliaia di dataset esistenti e li riorganizza in un unico formato comune — così che un metodo testato su un dataset possa essere facilmente testato su migliaia di altri.

## Cosa contiene l'IRW

L'IRW contiene **migliaia di dataset** ("tabelle"), ciascuno una raccolta di risposte individuali. Una risposta viene generata ogni volta che una persona (o un'altra unità) incontra una prova e l'esito viene registrato. Che cosa sia una prova dipende dalla [famiglia di dati](/overview.qmd): un item a cui si risponde singolarmente, un'altra entità incontrata in un confronto, un intero insieme di opzioni da mettere in ordine, oppure un profilo composto da attributi assegnati casualmente. Alcuni esempi:

- Risposte degli studenti a test di istruzione e abilità
- Item di questionari che misurano personalità o atteggiamenti
- Valutazioni assegnate da valutatori umani
- Partite, incontri e confronti diretti, in cui due entità si affrontano e una vince
- Schede a voto preferenziale, graduatorie di giurie e domande di ordinamento nei questionari
- Esperimenti conjoint, in cui i rispondenti scelgono tra profili costruiti casualmente
- Qualsiasi altra situazione che comporti risposte ripetute a un insieme di strumenti di misurazione

Ogni dataset dell'IRW è progettato per essere:

- **Reperibile (Findable).** Ogni dataset è accompagnato da metadati precalcolati — numero di partecipanti, numero di item, densità delle risposte, area disciplinare e altre etichette descrittive — così i dataset possono essere individuati e filtrati senza doverli prima scaricare.
- **Accessibile (Accessible).** Ogni dataset può essere ottenuto tramite il browser web o il pacchetto `irw`, con un account gratuito.
- **Interoperabile (Interoperable).** Ogni dataset viene riorganizzato in formato lungo, una riga per risposta, secondo lo standard della sua famiglia (descritto di seguito), in modo che lo stesso codice di analisi possa essere eseguito su molti dataset con poche o nessuna modifica.
- **Riutilizzabile (Reusable).** Ogni dataset ha una licenza aperta, la sua origine è documentata e il codice usato per convertirlo nel formato IRW è pubblico.

I dataset variano molto per dimensione (da poche centinaia di risposte a molti milioni) e per tipo di risposta (item sì/no, valutazioni multi-categoria, punteggi parziali, categorie non ordinate, esiti di confronti, posizioni in classifica e altro).

## Lo standard dei dati

Ogni tabella dell'IRW è in **formato lungo**: una riga per risposta. Che cosa *sia* una riga dipende dalla famiglia a cui appartiene la tabella, e lo stesso vale per le colonne che la compongono.

<img src="/figures/families/families-tree.svg" alt="Due domande che collocano un dataset in una famiglia dell'IRW."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| Famiglia | `source =` | Standard |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn non compare nello schema: questa famiglia è definita dall'origine dei dati e non dalla loro forma, quindi una tabella simulata può avere una qualsiasi di queste forme. La [panoramica degli standard](/overview.qmd) riporta le colonne di ciascuna famiglia.


## Come usarlo

Esistono tre modi per ottenere i dati dell'IRW, a seconda di quanto si desidera automatizzare.

**1. Esplora nel browser web**
Esplora i dataset e i loro metadati direttamente nel [browser dati IRW](/data.qmd) — non serve un account. Scaricare un dataset completo richiede un account gratuito [Redivis](https://redivis.com), poiché è quella la piattaforma che ospita i dati sottostanti.

**2. Usa il pacchetto `irw` (consigliato)**
Il pacchetto `irw` è disponibile sia per **R** che per **Python**.

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

La prima volta che usi il pacchetto, ti verrà chiesto di accedere con un account Redivis gratuito.

**3. Usa direttamente le librerie client di Redivis**
Per flussi di lavoro di livello inferiore o non basati su R/Python, i dati sono accessibili anche tramite le librerie client R e Python di Redivis. Per i dettagli, vedi la [Guida introduttiva](/getstarted.qmd).

### Oltre il download dei dati

Il progetto IRW include anche:

- Una raccolta crescente di **[esempi pratici (vignette)](/vignettes/index.qmd)** che applicano metodi di misurazione classici e nuovi su molti dataset IRW contemporaneamente
- **Risorse didattiche ed esercizi** per insegnare la psicometria con dati reali
- Un **processo di contribuzione** per i ricercatori che desiderano aggiungere i propri dataset al magazzino

## Per saperne di più

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Sito web: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Codice: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Se utilizzi i dati dell'IRW nel tuo lavoro, ti preghiamo di citare i dati originali (abbiamo previsto la funzionalità necessaria per farlo). Sarebbe inoltre molto apprezzato se citassi anche l'articolo introduttivo sopra indicato.

---

*Domande, un feedback o vuoi contribuire con un dataset? Visita la [pagina dei contatti](/contact.qmd) oppure apri una issue su [GitHub](https://github.com/itemresponsewarehouse).*
