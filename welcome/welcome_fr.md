---
lang: fr
pagetitle: "L'Entreposage de Réponse à l'Item"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# L'Entreposage de Réponse à l'Item (Item Response Warehouse, IRW)

**Des ressources libres et ouvertes de données de réponses à l'item harmonisées, destinées à la recherche en psychométrie et en mesure.**

[Lire l'article](https://doi.org/10.3758/s13428-025-02796-y) **(libre accès)**

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

## Pourquoi l'IRW existe?

Les chercheurs qui étudient la mesure, en éducation, en psychologie et dans les domaines connexes, ont besoin de données réelles pour tester et comparer leurs méthodes. Ces données existent déjà en grande quantité, mais elles sont dispersées entre de nombreuses études, stockées dans des formats très divers et souvent difficiles à réutiliser en raison d'une documentation ou d'une licence qui n'est pas claire.

Il s'agit d'un problème bien connu. La communauté scientifique a formulé une norme commune pour le résoudre : les données doivent être **FAIR** — Trouvables (Findable), Accessibles, Interopérables et Réutilisables (Wilkinson et al., 2016). D'autres disciplines ont mis ces principes en pratique en construisant des ressources de données partagées et normalisées. En informatique, la collection d'images étiquetée ImageNet a fourni aux chercheurs un référentiel commun et a contribué à accélérer les progrès de l'intelligence artificielle. La génétique et les neurosciences ont construit des ressources partagées similaires pour leurs propres données.

L'Entreposage de Réponse à l'Item (nous allons utiliser l'acronyme IRW pour être cohérent avec les écrits anglophones sur le sujet) applique les mêmes principes FAIR aux données de réponses aux items. Il rassemble des centaines de jeux de données existants et les remet en forme selon un format commun unique de sorte qu'une méthode testée sur un jeu de données puisse facilement être testée sur des centaines d'autres.

## Contenu de l'IRW

L'IRW contient **des centaines de jeux de données** (« tableaux »), chacun étant un ensemble de réponses individuelles. Une réponse est générée chaque fois qu'une personne (ou une unité) répond à un item (ou une variable). Voici quelques exemples :

- Réponses d'élèves à des tests d'éducation et d'aptitude
- Items de sondage mesurant la personnalité ou les attitudes
- Résultats attribués par des évaluateurs humains
- Tout autre contexte impliquant des réponses répétées à un ensemble de mesure

Chaque jeu de données de l'IRW est conçu pour être :

- **Trouvable (Findable).** Chaque jeu de données est accompagné de métadonnées précalculées — nombre de participants, nombre d'items, densité des réponses, domaine du sujet et autres étiquettes descriptives — afin que les jeux de données puissent être localisés et filtrés sans devoir d'abord être téléchargés.
- **Accessible.** Chaque jeu de données peut être récupéré via le navigateur web ou le package `irw`, avec un compte gratuit.
- **Interopérable.** Chaque jeu de données est remis en forme selon une même structure simplifiée (décrite ci-dessous), de sorte que le même code d'analyse puisse s'exécuter sur de nombreux jeux de données avec peu ou pas de modification.
- **Réutilisable.** Chaque jeu de données est sous licence ouverte, son origine est documentée et le code utilisé pour le convertir au format de l'IRW est public.

Les jeux de données varient considérablement en taille (allant de quelques dizaines de réponses à plusieurs millions) et en type de réponse (items binaires, évaluations à catégories multiples, scores à crédit partiel, etc.). Chaque jeu de données est également accompagné de métadonnées pré-calculées (nombre de participants, nombre d'items, densité des réponses, domaine du sujet et d'autres étiquettes descriptives pertinentes) afin que les chercheurs puissent trouver les jeux de données pertinents sans devoir d'abord tous les télécharger et les traiter.

## Des données standardisées

<img src="/welcome/assets/diagram-cross-classification.svg" alt="Schéma montrant une grille colorée de cellules id-item transformée, via une flèche, en un tableau au format long comportant les colonnes id, item et resp, où la couleur de chaque cellule resp correspond à sa cellule d'origine dans la grille." class="welcome-figure">


Chaque jeu de données de l'IRW est remis en forme au **format long** : une ligne par réponse. Au minimum, chaque ligne comporte trois éléments d'information :

| Colonne | Signification |
|---|---|
| `id` | Qui (ou quoi) a produit la réponse (généralement une personne) |
| `item` | Quelle mesure a produit la réponse (généralement une question ou une tâche) |
| `resp` | La réponse elle-même, stockée sous forme de score ordinal |

**Exemple :**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Lorsqu'un jeu de données comprend des informations supplémentaires (par exemple, temps de réponse, identité de l'évaluateur, covariables telles que l'âge) ces informations sont stockées dans des colonnes additionnelles, nommées de façon cohérente. Cette structure simple et unique couvre un très large éventail de situations en mesure, ce qui permet d'écrire le code d'analyse une seule fois et de l'appliquer à l'ensemble de l'entrepôt de réponse.

La spécification technique complète de la standardisation est disponible sur [itemresponsewarehouse.org/standard.html](/standard.qmd). Des standards plus spécialisés existent également pour le contenu textuel des items, les données par paires et les réponses nominales (catégories qui ne sont pas ordonnées).

## Comment l'utiliser?

Il existe trois façons d'obtenir les données de l'IRW selon le degré d'automatisation souhaité.

**1. Parcourir dans le navigateur web**
Explorez les jeux de données et leurs métadonnées directement sur le [navigateur de données de l'IRW](/data.qmd) — aucun compte n'est requis. Le téléchargement d'un jeu de données complet nécessite un compte gratuit [Redivis](https://redivis.com), puisque c'est la plateforme qui héberge les données sous-jacentes.

**2. Utiliser le paquet `irw` (recommandé)**
Le package `irw`, disponible pour **R** et **Python**, fournit des fonctions simples pour trouver, filtrer et télécharger des données.

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

La première fois que vous utilisez le package, il vous sera demandé de vous connecter avec un compte Redivis gratuit. Ensuite, une seule ligne de code télécharge n'importe quel jeu de données directement dans R ou Python. À partir de là, les données sont prêtes à être analysées, par exemple, avec la théorie de réponse aux items ou l'analyse factorielle.

**3. Utiliser directement les bibliothèques clientes de Redivis**
Pour des flux de travail de plus bas niveau ou hors R/Python, les données peuvent également être consultées via les propres bibliothèques clientes R et Python de Redivis. Consultez le [guide de démarrage](/getstarted.qmd) pour plus de détails.

### Au-delà du téléchargement des données

Le projet IRW comprend également :

- Un ensemble croissant de **[vignettes](/vignettes/index.qmd)** et des exemples pratiques appliquant des méthodes de mesure classiques, et nouvelles, à de nombreux jeux de données de l'IRW à la fois
- **Des ressources de formation et des exercices** pour enseigner la psychométrie avec des données réelles
- **Un processus de contribution** pour les chercheurs souhaitant ajouter leurs propres jeux de données à l'entrepôt

## En savoir plus

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Site web : [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Code : [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Si vous utilisez des données de l'IRW dans vos travaux, merci de citer les données originales (nous fournissons une fonctionnalité permettant de le faire). Il serait également apprécié que vous citiez l'article introductif mentionné plus haut.

---

*Vous avez des questions, des commentaires ou vous souhaitez contribuer à un jeu de données ? Visitez la page [page Contact](/contact.qmd) ou ouvrez un « issue » sur [GitHub](https://github.com/itemresponsewarehouse).*
