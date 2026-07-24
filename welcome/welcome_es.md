---
lang: es
pagetitle: "El Item Response Warehouse"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher link line below (English · Français · ...) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate or reorder it.
-->

# El Item Response Warehouse

**Una colección libre y abierta de datos de respuestas a ítems armonizados, para la investigación en psicometría y medición.**

[itemresponsewarehouse.org](https://itemresponsewarehouse.org) · [GitHub](https://github.com/itemresponsewarehouse) · [Leer el artículo](https://doi.org/10.3758/s13428-025-02796-y)

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html)

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
