---
lang: fr
pagetitle: "L'Item Response Warehouse (IRW; Entrepôt de réponse à l’item)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher link line below (English · Français · ...) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate or reorder it.
-->

# L'Item Response Warehouse (IRW; Entrepôt de réponse à l’item)

**Une collection libre et ouverte de données de réponses aux items harmonisées, destinée à la recherche en psychométrie et en mesure.**

[itemresponsewarehouse.org](https://itemresponsewarehouse.org) · [GitHub](https://github.com/itemresponsewarehouse) · [Lire l'article](https://doi.org/10.3758/s13428-025-02796-y)

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html) · [繁體中文](/welcome/welcome_zh-hant.html) · [বাংলা](/welcome/welcome_bn.html) · [Türkçe](/welcome/welcome_tr.html) · [Tiếng Việt](/welcome/welcome_vi.html)

---

## Pourquoi l'IRW existe

Les chercheurs qui étudient la mesure — en éducation, en psychologie et dans les domaines connexes — ont besoin de données réelles pour tester et comparer leurs méthodes. Ces données existent déjà en grande quantité. Mais elles sont dispersées entre de nombreuses études, stockées dans des formats très divers, et souvent difficiles à réutiliser en raison d'une documentation ou d'une licence peu claire.

Il s'agit d'un problème bien connu. D'autres disciplines l'ont résolu en construisant des ressources de données partagées et normalisées. En informatique, la collection d'images étiquetées ImageNet a fourni aux chercheurs un référentiel commun et a contribué à accélérer les progrès de l'intelligence artificielle. La génétique et les neurosciences ont construit des ressources partagées similaires pour leurs propres données.

L'Item Response Warehouse (IRW) fait la même chose pour les données de réponses aux items. Il rassemble des centaines de jeux de données existants et les remet en forme selon un format commun unique — de sorte qu'une méthode testée sur un jeu de données puisse facilement être testée sur des centaines d'autres.

## Ce que contient l'IRW

L'IRW contient **des centaines de jeux de données** (« tables »), chacun étant une collection de réponses individuelles. Une réponse est générée chaque fois qu'une personne (ou une autre unité) répond à un item (ou une autre sonde). Voici quelques exemples :

- Réponses d'élèves à des tests d'éducation et d'aptitude
- Items de sondage mesurant la personnalité ou les attitudes
- Évaluations attribuées par des évaluateurs humains
- Tout autre contexte impliquant des réponses répétées à un ensemble de sondes de mesure

Deux choses sont vraies pour chaque jeu de données de l'IRW :

- **Ouvert.** Chaque jeu de données est sous licence permettant sa réutilisation. Son origine est documentée, et le code utilisé pour le convertir au format de l'IRW est public.
- **Harmonisé.** Chaque jeu de données est remis en forme selon la même structure simple (décrite ci-dessous), de sorte que le même code d'analyse puisse s'exécuter sur de nombreux jeux de données avec peu ou pas de modification.

Les jeux de données varient considérablement en taille (de quelques centaines de réponses à plusieurs millions) et en type de réponse (items binaires, évaluations à catégories multiples, scores à crédit partiel, et plus encore). Chaque jeu de données est également accompagné de métadonnées précalculées — nombre de participants, nombre d'items, densité des réponses, domaine du sujet, et d'autres étiquettes descriptives — afin que les chercheurs puissent trouver les jeux de données pertinents sans devoir d'abord tous les télécharger et les traiter.

## Le standard de données

Chaque jeu de données de l'IRW est remis en forme au **format long** : une ligne par réponse. Au minimum, chaque ligne comporte trois éléments d'information :

| Colonne | Signification |
|---|---|
| `id` | Qui (ou quoi) a produit la réponse — généralement une personne |
| `item` | Quelle sonde de mesure a produit la réponse — généralement une question ou une tâche |
| `resp` | La réponse elle-même, stockée sous forme de score ordinal |

**Exemple :**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Lorsqu'un jeu de données comprend des informations supplémentaires — temps de réponse, identité de l'évaluateur, covariables telles que l'âge — ces informations sont stockées dans des colonnes additionnelles, nommées de façon cohérente. Cette structure simple unique couvre un très large éventail de situations de mesure, ce qui permet d'écrire le code d'analyse une seule fois et de l'appliquer à l'ensemble de l'entrepôt.

La spécification technique complète du standard est disponible sur [itemresponsewarehouse.org/standard.html](/standard.qmd). Des standards plus spécialisés existent également pour le texte des items, les données de compétition par paires, et les réponses nominales (catégories non ordonnées).

## Comment l'utiliser

Il existe trois façons d'obtenir les données de l'IRW, selon le degré d'automatisation souhaité.

**1. Parcourir dans le navigateur web**
Explorez les jeux de données et leurs métadonnées directement sur le [navigateur de données de l'IRW](/data.qmd) — aucun compte requis. Le téléchargement d'un jeu de données complet nécessite un compte gratuit [Redivis](https://redivis.com), puisque c'est la plateforme qui héberge les données sous-jacentes.

**2. Utiliser le paquet `irw` (recommandé)**
Le paquet `irw`, disponible pour **R** et **Python**, fournit des fonctions simples pour trouver, filtrer et télécharger des données.

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

La première fois que vous utilisez le paquet, il vous sera demandé de vous connecter avec un compte Redivis gratuit. Ensuite, une seule ligne de code télécharge n'importe quel jeu de données directement dans R ou Python. À partir de là, les données sont prêtes à être analysées avec des logiciels standards — par exemple, des paquets de théorie de réponse aux items ou d'analyse factorielle.

**3. Utiliser directement les bibliothèques clientes de Redivis**
Pour des flux de travail de plus bas niveau ou hors R/Python, les données peuvent également être consultées via les propres bibliothèques clientes R et Python de Redivis. Consultez le [guide de démarrage](/getstarted.qmd) pour plus de détails.

### Au-delà du téléchargement des données

Le projet IRW comprend également :
- Un ensemble croissant de **[vignettes](/vignettes/index.qmd)** — des exemples pratiques appliquant des méthodes de mesure classiques et nouvelles à de nombreux jeux de données de l'IRW à la fois
- **Des ressources de formation et des exercices** pour enseigner la psychométrie avec des données réelles
- **Un processus de contribution** pour les chercheurs souhaitant ajouter leurs propres jeux de données à l'entrepôt

## En savoir plus

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Site web : [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code : [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Si vous utilisez des données de l'IRW dans vos travaux, merci de citer les données originales (nous fournissons une fonctionnalité permettant de le faire). Il serait également très apprécié que vous citiez l'article introductif ci-dessus.

---

*Des questions, des commentaires, ou vous souhaitez contribuer un jeu de données ? Visitez la [page Contact](/contact.qmd) ou ouvrez un « issue » sur [GitHub](https://github.com/itemresponsewarehouse).*
