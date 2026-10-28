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

{{< include /welcome/_lang_switch.md >}}

---

## Por qué existe el IRW

Los investigadores que estudian la medición —en educación, psicología y campos afines— necesitan datos reales para probar y comparar sus métodos. Esos datos ya existen en grandes cantidades. Pero están dispersos entre muchos estudios, almacenados en formatos muy diversos, y a menudo son difíciles de reutilizar debido a una documentación o licencia poco clara.

Este es un problema bien conocido. La comunidad investigadora ha articulado una norma compartida para resolverlo: los datos deben ser **FAIR** — Encontrables (Findable), Accesibles, Interoperables y Reutilizables (Wilkinson et al., 2016). Otras disciplinas pusieron estos principios en práctica construyendo recursos de datos compartidos y estandarizados. En informática, la colección de imágenes etiquetadas ImageNet ofreció a los investigadores un referente común y ayudó a acelerar el progreso en inteligencia artificial. La genética y la neurociencia construyeron recursos compartidos similares para sus propios datos.

El Item Response Warehouse (IRW) aplica los mismos principios FAIR a los datos de respuestas a ítems. Reúne miles de conjuntos de datos existentes y los reformatea en un formato común único —de modo que un método probado en un conjunto de datos pueda probarse fácilmente en miles de otros.

## Qué contiene el IRW

El IRW contiene **miles de conjuntos de datos** ("tablas"), cada uno una colección de respuestas individuales. Se genera una respuesta cada vez que una persona (u otra unidad) se enfrenta a una sonda y se registra el resultado. Lo que cuenta como sonda varía según la [familia de datos](/overview.qmd): un ítem respondido por sí solo, otra entidad encontrada en un enfrentamiento, un conjunto completo de opciones puesto en orden, o un perfil compuesto por atributos asignados al azar. Algunos ejemplos:

- Respuestas de estudiantes en pruebas de educación y aptitud
- Ítems de encuesta que miden personalidad o actitudes
- Calificaciones asignadas por evaluadores humanos
- Partidas, encuentros y comparaciones directas, donde dos entidades se enfrentan y una gana
- Papeletas de voto preferencial, clasificaciones de jurados y preguntas de ordenación en encuestas
- Experimentos conjoint, donde las personas eligen entre perfiles construidos al azar
- Cualquier otro contexto que implique respuestas repetidas a un conjunto de sondas de medición

Cada conjunto de datos del IRW está diseñado para ser:

- **Encontrable (Findable).** Cada conjunto de datos viene con metadatos precalculados —número de participantes, número de ítems, densidad de respuestas, área temática y otras etiquetas descriptivas— para que los conjuntos de datos puedan localizarse y filtrarse sin necesidad de descargarlos primero.
- **Accesible.** Cada conjunto de datos puede obtenerse a través del navegador web o del paquete `irw`, usando una cuenta gratuita.
- **Interoperable.** Cada conjunto de datos se reformatea en formato largo, una fila por respuesta, siguiendo el estándar de su familia (descrito más abajo), de modo que el mismo código de análisis pueda ejecutarse en muchos conjuntos de datos con poca o ninguna modificación.
- **Reutilizable.** Cada conjunto de datos tiene una licencia abierta, su origen está documentado, y el código usado para convertirlo al formato del IRW es público.

Los conjuntos de datos varían ampliamente en tamaño (desde unos pocos cientos de respuestas hasta varios millones) y en tipo de respuesta (ítems de sí/no, calificaciones de varias categorías, puntuaciones de crédito parcial, categorías sin orden, resultados de enfrentamientos, posiciones en un ranking, y más).

## El estándar de datos

Cada tabla del IRW está en **formato largo**: una fila por respuesta. Lo que *es* una fila depende de la familia a la que pertenece la tabla, y lo mismo ocurre con las columnas que la componen.

<img src="/figures/families/families-tree.svg" alt="Dos preguntas que clasifican un conjunto de datos en una familia del IRW."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| Familia | `source =` | Estándar |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn no aparece en el diagrama: esa familia se define por el origen de los datos, no por su forma, de modo que una tabla simulada puede tener cualquiera de estas formas. La [visión general de los estándares](/overview.qmd) da las columnas de cada familia.


## Cómo usarlo

Hay tres formas de obtener datos del IRW, según cuánto quieras automatizar.

**1. Explorar en el navegador web**
Explora los conjuntos de datos y sus metadatos directamente en el [navegador de datos del IRW](/data.qmd) — no se necesita cuenta. Descargar un conjunto de datos completo requiere una cuenta gratuita de [Redivis](https://redivis.com), ya que es la plataforma que aloja los datos subyacentes.

**2. Usar el paquete `irw` (recomendado)**
El paquete `irw` está disponible tanto para **R** como para **Python**.

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

La primera vez que uses el paquete, se te pedirá que inicies sesión con una cuenta gratuita de Redivis.

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
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Sitio web: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Código: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Si usas datos del IRW en tu trabajo, por favor cita los datos originales (hemos incluido una función para hacerlo). También agradeceríamos mucho que citaras el artículo introductorio mencionado arriba.

---

*¿Preguntas, comentarios, o quieres contribuir con un conjunto de datos? Visita la [página de Contacto](/contact.qmd) o abre un "issue" en [GitHub](https://github.com/itemresponsewarehouse).*
