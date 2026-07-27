---
lang: es
pagetitle: "El Item Response Warehouse (IRW; Almacén de respuestas a ítems)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# El Item Response Warehouse (IRW; Almacén de respuestas a ítems)

**Una colección libre y abierta de datos de respuestas a ítems armonizados, para la investigación en psicometría y medición.**

[Leer el artículo](https://doi.org/10.3758/s13428-025-02796-y) **(acceso abierto)**

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

## Por qué existe el IRW

Los investigadores que estudian la medición —en educación, psicología y campos afines— necesitan datos reales para probar y comparar sus métodos. Esos datos ya existen en grandes cantidades. Pero están dispersos entre muchos estudios, almacenados en formatos muy diversos, y a menudo son difíciles de reutilizar debido a una documentación o licencia poco clara.

Este es un problema bien conocido. Otras disciplinas lo resolvieron construyendo recursos de datos compartidos y estandarizados. En informática, la colección de imágenes etiquetadas ImageNet ofreció a los investigadores un referente común y ayudó a acelerar el progreso en inteligencia artificial. La genética y la neurociencia construyeron recursos compartidos similares para sus propios datos.

El Item Response Warehouse (IRW) hace lo mismo para los datos de respuestas a ítems. Reúne cientos de conjuntos de datos existentes y los reformatea en un formato común único —de modo que un método probado en un conjunto de datos pueda probarse fácilmente en cientos de otros.

## Qué contiene el IRW

El IRW contiene **cientos de conjuntos de datos** ("tablas"), cada uno una colección de respuestas individuales. Se genera una respuesta cada vez que una persona (u otra unidad) responde a un ítem (u otra sonda). Algunos ejemplos:

- Respuestas de estudiantes en pruebas de educación y aptitud
- Ítems de encuesta que miden personalidad o actitudes
- Calificaciones asignadas por evaluadores humanos
- Cualquier otro contexto que implique respuestas repetidas a un conjunto de sondas de medición

Dos cosas son ciertas para cada conjunto de datos del IRW:

- **Abierto.** Cada conjunto de datos tiene licencia para su reutilización. Su origen está documentado, y el código usado para convertirlo al formato del IRW es público.
- **Armonizado.** Cada conjunto de datos se reformatea según la misma estructura simple (descrita más abajo), de modo que el mismo código de análisis pueda ejecutarse en muchos conjuntos de datos con poca o ninguna modificación.

Los conjuntos de datos varían ampliamente en tamaño (desde unos pocos cientos de respuestas hasta varios millones) y en tipo de respuesta (ítems de sí/no, calificaciones de varias categorías, puntuaciones de crédito parcial, y más). Cada conjunto de datos también viene con metadatos precalculados —número de participantes, número de ítems, densidad de respuestas, área temática y otras etiquetas descriptivas— para que los investigadores puedan encontrar conjuntos de datos relevantes sin tener que descargarlos y procesarlos todos primero.

## El estándar de datos

<img src="/welcome/assets/diagram-cross-classification.svg" alt="Diagrama de cuadrícula que muestra que cada respuesta se encuentra en la intersección de un id y un item." class="welcome-figure">


Cada conjunto de datos del IRW se reformatea en **formato largo**: una fila por respuesta. Como mínimo, cada fila contiene tres elementos de información:

| Columna | Significado |
|---|---|
| `id` | Quién (o qué) produjo la respuesta — típicamente una persona |
| `item` | Qué sonda de medición produjo la respuesta — típicamente una pregunta o tarea |
| `resp` | La respuesta misma, almacenada como una puntuación ordinal |

**Ejemplo:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Cuando un conjunto de datos incluye información adicional —tiempo de respuesta, identidad del evaluador, covariables como la edad— esa información se almacena en columnas adicionales, nombradas de forma consistente. Esta única estructura simple cubre una enorme variedad de situaciones de medición, lo que permite escribir el código de análisis una sola vez y aplicarlo a todo el almacén.

La especificación técnica completa del estándar está disponible en [itemresponsewarehouse.org/standard.html](/standard.qmd). También existen estándares más especializados para el texto de los ítems, los datos de competencia por pares y las respuestas nominales (categorías no ordenadas).

## Cómo usarlo

Hay tres formas de obtener datos del IRW, según cuánto quieras automatizar.

**1. Explorar en el navegador web**
Explora los conjuntos de datos y sus metadatos directamente en el [navegador de datos del IRW](/data.qmd) — no se necesita cuenta. Descargar un conjunto de datos completo requiere una cuenta gratuita de [Redivis](https://redivis.com), ya que es la plataforma que aloja los datos subyacentes.

**2. Usar el paquete `irw` (recomendado)**
El paquete `irw`, disponible tanto para **R** como para **Python**, ofrece funciones simples para buscar, filtrar y descargar datos.

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

La primera vez que uses el paquete, se te pedirá que inicies sesión con una cuenta gratuita de Redivis. Después de eso, una sola línea de código descarga cualquier conjunto de datos directamente en R o Python. A partir de ahí, los datos están listos para el análisis con software estándar —por ejemplo, paquetes de teoría de respuesta al ítem o de análisis factorial.

**3. Usar directamente las bibliotecas cliente de Redivis**
Para flujos de trabajo de más bajo nivel o fuera de R/Python, los datos también se pueden acceder mediante las propias bibliotecas cliente de R y Python de Redivis. Consulta la [guía de introducción](/getstarted.qmd) para más detalles.

### Más allá de la descarga de datos

El proyecto IRW también incluye:

- Un conjunto creciente de **[viñetas](/vignettes/index.qmd)** — ejemplos prácticos que aplican métodos de medición clásicos y nuevos a muchos conjuntos de datos del IRW a la vez
- **Recursos de capacitación y ejercicios** para enseñar psicometría con datos reales
- **Un proceso de contribución** para investigadores que deseen añadir sus propios conjuntos de datos al almacén

## Más información

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Sitio web: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Código: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Si usas datos del IRW en tu trabajo, por favor cita los datos originales (hemos incluido una función para hacerlo). También agradeceríamos mucho que citaras el artículo introductorio mencionado arriba.

---

*¿Preguntas, comentarios, o quieres contribuir con un conjunto de datos? Visita la [página de Contacto](/contact.qmd) o abre un "issue" en [GitHub](https://github.com/itemresponsewarehouse).*
