#!/usr/bin/env python3
"""Apply TRANSLATION_CHANGES.md 1-7 and 10 to one welcome_<lang>.md.

Scratch tooling for the Stage B pass, kept beside the files it edits so the
pass is reproducible and reviewable. Changes 8 and 9 (the code lines) were
applied separately and mechanically; they are not here.

Each language supplies a dict of {change: (old, new)}. Every `old` must appear
exactly once in the file or the run aborts without writing, so a translation
that has drifted from the expected wording fails loudly rather than being
half-edited.

Run:  python3 welcome/_stage_b_edits.py fr es de
      python3 welcome/_stage_b_edits.py --all
"""

import io
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))

# The six family rows are the same in every language: the names are tied to the
# `source` values and to English-only standard pages, so they are not
# translated. Only the two column headers and the surrounding prose are.
ROWS = """| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |"""


def section(lang_heading, p1, alt, col_family, col_standard, p2):
    """The whole replacement body for the data-standard section."""
    return """%s

%s

<img src="/figures/families/families-tree.svg" alt="%s"\n     class="standard-tree" style="display:block;width:100%%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| %s | `source =` | %s |
|---|---|---|
%s

%s
""" % (lang_heading, p1, alt, col_family, col_standard, ROWS, p2)


# ---------------------------------------------------------------------------
# Anchored edits.
#
# Retyping a whole foreign-language paragraph as the `old` side of a
# replacement is how you introduce typos you cannot see: two of mine in the
# zh-hant strings were a wrong codepoint and a wrong word, and the exact-match
# assert is the only reason they surfaced. So from here the `old` side is a
# short, distinctive ANCHOR and the tool slices the real text out of the file.
#
#   para(anchor, new)  replace the blank-line-delimited block containing anchor
#   line(anchor, new)  replace the single line containing anchor
#
# The anchor must still occur exactly once, so an ambiguous one fails loudly.

def para(anchor, new):
    return ("para", anchor, new)


def line(anchor, new):
    return ("line", anchor, new)


def _resolve(s, spec):
    """Turn an anchored spec into the literal (old, new) pair."""
    if not (isinstance(spec, tuple) and len(spec) == 3):
        return spec                                  # already literal
    kind, anchor, new = spec
    if s.count(anchor) != 1:
        raise LookupError("anchor %r matched %d times" % (anchor[:40], s.count(anchor)))
    i = s.index(anchor)
    if kind == "line":
        a = s.rfind("\n", 0, i) + 1
        b = s.find("\n", i)
        return s[a:b if b != -1 else len(s)], new
    a = s.rfind("\n\n", 0, i)
    a = 0 if a == -1 else a + 2
    b = s.find("\n\n", i)
    return s[a:b if b != -1 else len(s)], new


EDITS = {}

# --------------------------------------------------------------------- fr ---
EDITS["fr"] = {
 "1": ("Il rassemble des centaines de jeux de données existants et les remet en forme selon un format commun unique de sorte qu'une méthode testée sur un jeu de données puisse facilement être testée sur des centaines d'autres.",
       "Il rassemble des milliers de jeux de données existants et les remet en forme selon un format commun unique de sorte qu'une méthode testée sur un jeu de données puisse facilement être testée sur des milliers d'autres."),
 "2": ("L'IRW contient **des centaines de jeux de données** (« tableaux »), chacun étant un ensemble de réponses individuelles. Une réponse est générée chaque fois qu'une personne (ou une unité) répond à un item (ou une variable). Voici quelques exemples :",
       "L'IRW contient **des milliers de jeux de données** (« tableaux »), chacun étant un ensemble de réponses individuelles. Une réponse est générée chaque fois qu'une personne (ou une autre unité) est confrontée à une sollicitation et que le résultat est enregistré. La nature de cette sollicitation varie selon la [famille de données](/overview.qmd) : un item auquel on répond isolément, une autre entité rencontrée lors d'une confrontation, un ensemble complet d'options à ordonner, ou un profil composé d'attributs attribués aléatoirement. Voici quelques exemples :"),
 "3": ("- Résultats attribués par des évaluateurs humains\n- Tout autre contexte impliquant des réponses répétées à un ensemble de mesure",
       "- Résultats attribués par des évaluateurs humains\n- Parties, matchs et comparaisons directes, où deux entités s'affrontent et l'une l'emporte\n- Bulletins de vote préférentiel, classements de jurys et questions de classement dans les sondages\n- Expériences conjointes, où les répondants choisissent entre des profils construits aléatoirement\n- Tout autre contexte impliquant des réponses répétées à un ensemble de mesure"),
 "4": ("- **Interopérable.** Chaque jeu de données est remis en forme selon une même structure simplifiée (décrite ci-dessous), de sorte que le même code d'analyse puisse s'exécuter sur de nombreux jeux de données avec peu ou pas de modification.",
       "- **Interopérable.** Chaque jeu de données est remis en forme au format long, une ligne par réponse, selon le standard propre à sa famille (décrit ci-dessous), de sorte que le même code d'analyse puisse s'exécuter sur de nombreux jeux de données avec peu ou pas de modification."),
 "5": ("Les jeux de données varient considérablement en taille (allant de quelques dizaines de réponses à plusieurs millions) et en type de réponse (items binaires, évaluations à catégories multiples, scores à crédit partiel, etc.). Chaque jeu de données est également accompagné de métadonnées pré-calculées (nombre de participants, nombre d'items, densité des réponses, domaine du sujet et d'autres étiquettes descriptives pertinentes) afin que les chercheurs puissent trouver les jeux de données pertinents sans devoir d'abord tous les télécharger et les traiter.",
       "Les jeux de données varient considérablement en taille (allant de quelques dizaines de réponses à plusieurs millions) et en type de réponse (items binaires, évaluations à catégories multiples, scores à crédit partiel, catégories non ordonnées, résultats de confrontations, rangs, etc.)."),
 "7": ("Le package `irw`, disponible pour **R** et **Python**, fournit des fonctions simples pour trouver, filtrer et télécharger des données.",
       "Le package `irw` est disponible pour **R** et **Python**."),
 "10": ("La première fois que vous utilisez le package, il vous sera demandé de vous connecter avec un compte Redivis gratuit. Ensuite, une seule ligne de code télécharge n'importe quel jeu de données directement dans R ou Python. À partir de là, les données sont prêtes à être analysées, par exemple, avec la théorie de réponse aux items ou l'analyse factorielle.",
        "La première fois que vous utilisez le package, il vous sera demandé de vous connecter avec un compte Redivis gratuit."),
 "6": section(
     "## Des données standardisées",
     "Chaque tableau de l'IRW est au **format long** : une ligne par réponse. Ce que *représente* une ligne dépend de la famille à laquelle appartient le tableau, tout comme les colonnes qui la portent.",
     "Deux questions qui permettent de classer un jeu de données dans une famille de l'IRW.",
     "Famille", "Standard",
     "Simsyn ne figure pas sur le schéma : cette famille est définie par l'origine des données et non par leur forme, de sorte qu'un tableau simulé peut prendre n'importe laquelle de ces formes. L'[aperçu des standards](/overview.qmd) donne les colonnes de chaque famille."),
}


# --------------------------------------------------------------------- es ---
EDITS["es"] = {
 "1": ("Reúne cientos de conjuntos de datos existentes y los reformatea en un formato común único —de modo que un método probado en un conjunto de datos pueda probarse fácilmente en cientos de otros.",
       "Reúne miles de conjuntos de datos existentes y los reformatea en un formato común único —de modo que un método probado en un conjunto de datos pueda probarse fácilmente en miles de otros."),
 "2": ('El IRW contiene **cientos de conjuntos de datos** ("tablas"), cada uno una colección de respuestas individuales. Se genera una respuesta cada vez que una persona (u otra unidad) responde a un ítem (u otra sonda). Algunos ejemplos:',
       'El IRW contiene **miles de conjuntos de datos** ("tablas"), cada uno una colección de respuestas individuales. Se genera una respuesta cada vez que una persona (u otra unidad) se enfrenta a una sonda y se registra el resultado. Lo que cuenta como sonda varía según la [familia de datos](/overview.qmd): un ítem respondido por sí solo, otra entidad encontrada en un enfrentamiento, un conjunto completo de opciones puesto en orden, o un perfil compuesto por atributos asignados al azar. Algunos ejemplos:'),
 "3": ("- Calificaciones asignadas por evaluadores humanos\n- Cualquier otro contexto que implique respuestas repetidas a un conjunto de sondas de medición",
       "- Calificaciones asignadas por evaluadores humanos\n- Partidas, encuentros y comparaciones directas, donde dos entidades se enfrentan y una gana\n- Papeletas de voto preferencial, clasificaciones de jurados y preguntas de ordenación en encuestas\n- Experimentos conjoint, donde las personas eligen entre perfiles construidos al azar\n- Cualquier otro contexto que implique respuestas repetidas a un conjunto de sondas de medición"),
 "4": ("- **Interoperable.** Cada conjunto de datos se reformatea según la misma estructura simple (descrita más abajo), de modo que el mismo código de análisis pueda ejecutarse en muchos conjuntos de datos con poca o ninguna modificación.",
       "- **Interoperable.** Cada conjunto de datos se reformatea en formato largo, una fila por respuesta, siguiendo el estándar de su familia (descrito más abajo), de modo que el mismo código de análisis pueda ejecutarse en muchos conjuntos de datos con poca o ninguna modificación."),
 "5": ("Los conjuntos de datos varían ampliamente en tamaño (desde unos pocos cientos de respuestas hasta varios millones) y en tipo de respuesta (ítems de sí/no, calificaciones de varias categorías, puntuaciones de crédito parcial, y más). Cada conjunto de datos también viene con metadatos precalculados —número de participantes, número de ítems, densidad de respuestas, área temática y otras etiquetas descriptivas— para que los investigadores puedan encontrar conjuntos de datos relevantes sin tener que descargarlos y procesarlos todos primero.",
       "Los conjuntos de datos varían ampliamente en tamaño (desde unos pocos cientos de respuestas hasta varios millones) y en tipo de respuesta (ítems de sí/no, calificaciones de varias categorías, puntuaciones de crédito parcial, categorías sin orden, resultados de enfrentamientos, posiciones en un ranking, y más)."),
 "7": ("El paquete `irw`, disponible tanto para **R** como para **Python**, ofrece funciones simples para buscar, filtrar y descargar datos.",
       "El paquete `irw` está disponible tanto para **R** como para **Python**."),
 "10": ("La primera vez que uses el paquete, se te pedirá que inicies sesión con una cuenta gratuita de Redivis. Después de eso, una sola línea de código descarga cualquier conjunto de datos directamente en R o Python. A partir de ahí, los datos están listos para el análisis con software estándar —por ejemplo, paquetes de teoría de respuesta al ítem o de análisis factorial.",
        "La primera vez que uses el paquete, se te pedirá que inicies sesión con una cuenta gratuita de Redivis."),
 "6": section(
     "## El estándar de datos",
     "Cada tabla del IRW está en **formato largo**: una fila por respuesta. Lo que *es* una fila depende de la familia a la que pertenece la tabla, y lo mismo ocurre con las columnas que la componen.",
     "Dos preguntas que clasifican un conjunto de datos en una familia del IRW.",
     "Familia", "Estándar",
     "Simsyn no aparece en el diagrama: esa familia se define por el origen de los datos, no por su forma, de modo que una tabla simulada puede tener cualquiera de estas formas. La [visión general de los estándares](/overview.qmd) da las columnas de cada familia."),
}

# --------------------------------------------------------------------- de ---
EDITS["de"] = {
 "1": ("Es bringt Hunderte bestehender Datensätze zusammen, formt sie in ein gemeinsames Format um und macht sie frei zugänglich an einem einzigen Ort — sodass eine Methode, die an einem Datensatz getestet wurde, leicht an Hunderten anderer getestet werden kann.",
       "Es bringt Tausende bestehender Datensätze zusammen, formt sie in ein gemeinsames Format um und macht sie frei zugänglich an einem einzigen Ort — sodass eine Methode, die an einem Datensatz getestet wurde, leicht an Tausenden anderer getestet werden kann."),
 "2": ('Das IRW enthält **Hunderte von Datensätzen** ("Tabellen"), von denen jeder eine Sammlung individueller Antworten ist. Eine Antwort entsteht immer dann, wenn eine Person (oder eine andere Einheit) auf ein Item (oder eine andere Messsonde) reagiert. Beispiele sind:',
       'Das IRW enthält **Tausende von Datensätzen** ("Tabellen"), von denen jeder eine Sammlung individueller Antworten ist. Eine Antwort entsteht immer dann, wenn eine Person (oder eine andere Einheit) auf eine Messsonde trifft und das Ergebnis festgehalten wird. Was als Messsonde zählt, hängt von der [Datenfamilie](/overview.qmd) ab: ein einzeln beantwortetes Item, eine andere Einheit, der man im Wettbewerb begegnet, ein ganzer Satz von Optionen, der in eine Reihenfolge gebracht wird, oder ein Profil, das aus zufällig zugewiesenen Merkmalen zusammengesetzt ist. Beispiele sind:'),
 "3": ("- Bewertungen, die von menschlichen Beurteilenden vergeben werden\n- Jeder andere Kontext mit wiederholten Antworten auf eine Reihe von Messsonden",
       "- Bewertungen, die von menschlichen Beurteilenden vergeben werden\n- Spiele, Partien und direkte Vergleiche, bei denen zwei Einheiten aufeinandertreffen und eine gewinnt\n- Präferenzwahlzettel, Jury-Rangfolgen und Rangordnungsfragen in Befragungen\n- Conjoint-Experimente, bei denen Befragte zwischen zufällig zusammengestellten Profilen wählen\n- Jeder andere Kontext mit wiederholten Antworten auf eine Reihe von Messsonden"),
 "4": ("- **Interoperabel (Interoperable).** Jeder Datensatz wird in dieselbe einfache Struktur umgeformt (unten beschrieben), sodass derselbe Analysecode mit wenig oder gar keiner Anpassung auf viele Datensätze angewendet werden kann.",
       "- **Interoperabel (Interoperable).** Jeder Datensatz wird in das Langformat umgeformt, eine Zeile pro Antwort, nach dem Standard seiner Familie (unten beschrieben), sodass derselbe Analysecode mit wenig oder gar keiner Anpassung auf viele Datensätze angewendet werden kann."),
 "5": ("Die Datensätze variieren stark in ihrer Größe (von einigen Hundert Antworten bis zu mehreren Millionen) und im Antworttyp (Ja/Nein-Items, mehrkategoriale Bewertungen, Teilpunktzahlen und mehr). Jeder Datensatz kommt zudem mit vorab berechneten Metadaten — Anzahl der Teilnehmenden, Anzahl der Items, Antwortdichte, Themengebiet und weitere beschreibende Tags —, sodass Forschende relevante Datensätze finden können, ohne zunächst alle herunterladen und verarbeiten zu müssen.",
       "Die Datensätze variieren stark in ihrer Größe (von einigen Hundert Antworten bis zu mehreren Millionen) und im Antworttyp (Ja/Nein-Items, mehrkategoriale Bewertungen, Teilpunktzahlen, ungeordnete Kategorien, Wettkampfergebnisse, Rangplätze und mehr)."),
 "7": ("Das `irw`-Paket, verfügbar sowohl für **R** als auch für **Python**, bietet einfache Funktionen zum Auffinden, Filtern und Herunterladen von Daten.",
       "Das `irw`-Paket ist sowohl für **R** als auch für **Python** verfügbar."),
 "10": ("Beim ersten Gebrauch des Pakets werden Sie aufgefordert, sich mit einem kostenlosen Redivis-Konto anzumelden. Danach lädt eine einzige Codezeile jeden beliebigen Datensatz direkt in R oder Python herunter. Von da an sind die Daten bereit für die Analyse mit gängiger Software — etwa Paketen für Item-Response-Theorie oder Faktorenanalyse.",
        "Beim ersten Gebrauch des Pakets werden Sie aufgefordert, sich mit einem kostenlosen Redivis-Konto anzumelden."),
 "6": section(
     "## Der Datenstandard",
     "Jede IRW-Tabelle liegt im **Langformat** vor: eine Zeile pro Antwort. Was eine Zeile *ist*, hängt von der Familie ab, zu der die Tabelle gehört, und ebenso die Spalten, die sie tragen.",
     "Zwei Fragen, die einen Datensatz einer IRW-Familie zuordnen.",
     "Familie", "Standard",
     "Simsyn steht nicht im Diagramm: Diese Familie ist über die Herkunft der Daten definiert, nicht über ihre Form, sodass eine simulierte Tabelle jede dieser Formen haben kann. Die [Übersicht der Standards](/overview.qmd) nennt die Spalten jeder Familie."),
}


# --------------------------------------------------------------------- it ---
EDITS["it"] = {
 "1": ("Riunisce centinaia di dataset esistenti e li riorganizza in un unico formato comune — così che un metodo testato su un dataset possa essere facilmente testato su centinaia di altri.",
       "Riunisce migliaia di dataset esistenti e li riorganizza in un unico formato comune — così che un metodo testato su un dataset possa essere facilmente testato su migliaia di altri."),
 "2": ('L\'IRW contiene **centinaia di dataset** ("tabelle"), ciascuno una raccolta di risposte individuali. Una risposta viene generata ogni volta che una persona (o un\'altra unità) risponde a un item (o un\'altra prova). Alcuni esempi:',
       'L\'IRW contiene **migliaia di dataset** ("tabelle"), ciascuno una raccolta di risposte individuali. Una risposta viene generata ogni volta che una persona (o un\'altra unità) incontra una prova e l\'esito viene registrato. Che cosa sia una prova dipende dalla [famiglia di dati](/overview.qmd): un item a cui si risponde singolarmente, un\'altra entità incontrata in un confronto, un intero insieme di opzioni da mettere in ordine, oppure un profilo composto da attributi assegnati casualmente. Alcuni esempi:'),
 "3": ("- Valutazioni assegnate da valutatori umani\n- Qualsiasi altra situazione che comporti risposte ripetute a un insieme di strumenti di misurazione",
       "- Valutazioni assegnate da valutatori umani\n- Partite, incontri e confronti diretti, in cui due entità si affrontano e una vince\n- Schede a voto preferenziale, graduatorie di giurie e domande di ordinamento nei questionari\n- Esperimenti conjoint, in cui i rispondenti scelgono tra profili costruiti casualmente\n- Qualsiasi altra situazione che comporti risposte ripetute a un insieme di strumenti di misurazione"),
 "4": ("- **Interoperabile (Interoperable).** Ogni dataset viene riorganizzato nella stessa struttura semplice (descritta di seguito), in modo che lo stesso codice di analisi possa essere eseguito su molti dataset con poche o nessuna modifica.",
       "- **Interoperabile (Interoperable).** Ogni dataset viene riorganizzato in formato lungo, una riga per risposta, secondo lo standard della sua famiglia (descritto di seguito), in modo che lo stesso codice di analisi possa essere eseguito su molti dataset con poche o nessuna modifica."),
 "5": ("I dataset variano molto per dimensione (da poche centinaia di risposte a molti milioni) e per tipo di risposta (item sì/no, valutazioni multi-categoria, punteggi parziali e altro). Ogni dataset include anche metadati precalcolati — numero di partecipanti, numero di item, densità delle risposte, area disciplinare e altre etichette descrittive — così i ricercatori possono trovare i dataset rilevanti senza doverli prima scaricare e processare tutti.",
       "I dataset variano molto per dimensione (da poche centinaia di risposte a molti milioni) e per tipo di risposta (item sì/no, valutazioni multi-categoria, punteggi parziali, categorie non ordinate, esiti di confronti, posizioni in classifica e altro)."),
 "7": ("Il pacchetto `irw`, disponibile sia per **R** che per **Python**, offre funzioni semplici per trovare, filtrare e scaricare i dati.",
       "Il pacchetto `irw` è disponibile sia per **R** che per **Python**."),
 "10": ("La prima volta che usi il pacchetto, ti verrà chiesto di accedere con un account Redivis gratuito. Da quel momento, una singola riga di codice scarica qualsiasi dataset direttamente in R o Python. Da lì, i dati sono pronti per l\'analisi con software standard — ad esempio pacchetti di teoria della risposta all\'item o di analisi fattoriale.",
        "La prima volta che usi il pacchetto, ti verrà chiesto di accedere con un account Redivis gratuito."),
 "6": section(
     "## Lo standard dei dati",
     "Ogni tabella dell\'IRW è in **formato lungo**: una riga per risposta. Che cosa *sia* una riga dipende dalla famiglia a cui appartiene la tabella, e lo stesso vale per le colonne che la compongono.",
     "Due domande che collocano un dataset in una famiglia dell\'IRW.",
     "Famiglia", "Standard",
     "Simsyn non compare nello schema: questa famiglia è definita dall\'origine dei dati e non dalla loro forma, quindi una tabella simulata può avere una qualsiasi di queste forme. La [panoramica degli standard](/overview.qmd) riporta le colonne di ciascuna famiglia."),
}

# --------------------------------------------------------------------- pt ---
EDITS["pt"] = {
 "1": ("Ele reúne centenas de conjuntos de dados existentes e os reformata em um formato comum único — de modo que um método testado em um conjunto de dados possa ser facilmente testado em centenas de outros.",
       "Ele reúne milhares de conjuntos de dados existentes e os reformata em um formato comum único — de modo que um método testado em um conjunto de dados possa ser facilmente testado em milhares de outros."),
 "2": ('O IRW contém **centenas de conjuntos de dados** ("tabelas"), cada um sendo uma coleção de respostas individuais. Uma resposta é gerada sempre que uma pessoa (ou outra unidade) responde a um item (ou outra sonda). Exemplos incluem:',
       'O IRW contém **milhares de conjuntos de dados** ("tabelas"), cada um sendo uma coleção de respostas individuais. Uma resposta é gerada sempre que uma pessoa (ou outra unidade) se depara com uma sonda e o resultado é registrado. O que conta como sonda varia conforme a [família de dados](/overview.qmd): um item respondido isoladamente, outra entidade enfrentada em uma disputa, um conjunto inteiro de opções colocado em ordem, ou um perfil montado a partir de atributos atribuídos ao acaso. Exemplos incluem:'),
 "3": ("- Avaliações atribuídas por avaliadores humanos\n- Qualquer outro contexto que envolva respostas repetidas a um conjunto de sondas de medição",
       "- Avaliações atribuídas por avaliadores humanos\n- Partidas, jogos e comparações diretas, em que duas entidades se enfrentam e uma vence\n- Cédulas de voto preferencial, classificações de júris e questões de ordenação em pesquisas\n- Experimentos conjoint, em que os respondentes escolhem entre perfis construídos ao acaso\n- Qualquer outro contexto que envolva respostas repetidas a um conjunto de sondas de medição"),
 "4": ("- **Interoperável (Interoperable).** Cada conjunto de dados é reformatado segundo a mesma estrutura simples (descrita abaixo), de modo que o mesmo código de análise possa ser executado em muitos conjuntos de dados com pouca ou nenhuma modificação.",
       "- **Interoperável (Interoperable).** Cada conjunto de dados é reformatado em formato longo, uma linha por resposta, seguindo o padrão de sua família (descrito abaixo), de modo que o mesmo código de análise possa ser executado em muitos conjuntos de dados com pouca ou nenhuma modificação."),
 "5": ("Os conjuntos de dados variam amplamente em tamanho (de algumas centenas de respostas a vários milhões) e em tipo de resposta (itens sim/não, avaliações de múltiplas categorias, pontuações de crédito parcial, entre outros). Cada conjunto de dados também vem com metadados pré-calculados — número de participantes, número de itens, densidade de respostas, área temática e outras etiquetas descritivas — para que os pesquisadores possam encontrar conjuntos de dados relevantes sem precisar baixá-los e processá-los todos primeiro.",
       "Os conjuntos de dados variam amplamente em tamanho (de algumas centenas de respostas a vários milhões) e em tipo de resposta (itens sim/não, avaliações de múltiplas categorias, pontuações de crédito parcial, categorias sem ordem, resultados de disputas, posições em rankings, entre outros)."),
 "7": ("O pacote `irw`, disponível tanto para **R** quanto para **Python**, oferece funções simples para encontrar, filtrar e baixar dados.",
       "O pacote `irw` está disponível tanto para **R** quanto para **Python**."),
 "10": ("Na primeira vez que você usar o pacote, será solicitado que faça login com uma conta gratuita do Redivis. Depois disso, uma única linha de código baixa qualquer conjunto de dados diretamente no R ou Python. A partir daí, os dados estão prontos para análise com software padrão — por exemplo, pacotes de teoria de resposta ao item ou análise fatorial.",
        "Na primeira vez que você usar o pacote, será solicitado que faça login com uma conta gratuita do Redivis."),
 "6": section(
     "## O padrão de dados",
     "Cada tabela do IRW está em **formato longo**: uma linha por resposta. O que uma linha *é* depende da família à qual a tabela pertence, e o mesmo vale para as colunas que a compõem.",
     "Duas perguntas que classificam um conjunto de dados em uma família do IRW.",
     "Família", "Padrão",
     "Simsyn não aparece no diagrama: essa família é definida pela origem dos dados, não por sua forma, de modo que uma tabela simulada pode ter qualquer uma destas formas. A [visão geral dos padrões](/overview.qmd) traz as colunas de cada família."),
}

# --------------------------------------------------------------------- nl ---
EDITS["nl"] = {
 "1": ("Het brengt honderden bestaande datasets samen en vormt ze om naar één gemeenschappelijk formaat — zodat een methode die op één dataset is getest, gemakkelijk op honderden andere kan worden getest.",
       "Het brengt duizenden bestaande datasets samen en vormt ze om naar één gemeenschappelijk formaat — zodat een methode die op één dataset is getest, gemakkelijk op duizenden andere kan worden getest."),
 "2": ('Het IRW bevat **honderden datasets** ("tabellen"), elk een verzameling van individuele responsen. Een respons ontstaat wanneer een persoon (of een andere eenheid) reageert op een item (of een andere meetopgave). Voorbeelden zijn onder meer:',
       'Het IRW bevat **duizenden datasets** ("tabellen"), elk een verzameling van individuele responsen. Een respons ontstaat wanneer een persoon (of een andere eenheid) een meetopgave tegenkomt en de uitkomst wordt vastgelegd. Wat als meetopgave geldt, verschilt per [datafamilie](/overview.qmd): een item dat op zichzelf wordt beantwoord, een andere eenheid die in een confrontatie wordt ontmoet, een volledige reeks opties die op volgorde wordt gezet, of een profiel dat uit willekeurig toegewezen kenmerken is samengesteld. Voorbeelden zijn onder meer:'),
 "3": ("- Beoordelingen gegeven door menselijke beoordelaars\n- Elke andere situatie met herhaalde responsen op een reeks meetinstrumenten",
       "- Beoordelingen gegeven door menselijke beoordelaars\n- Wedstrijden, partijen en directe vergelijkingen, waarbij twee eenheden tegenover elkaar staan en er één wint\n- Voorkeursstembiljetten, juryranglijsten en rangschikkingsvragen in vragenlijsten\n- Conjoint-experimenten, waarbij respondenten kiezen tussen willekeurig samengestelde profielen\n- Elke andere situatie met herhaalde responsen op een reeks meetinstrumenten"),
 "4": ("- **Interoperabel (Interoperable).** Elke dataset wordt omgevormd naar dezelfde eenvoudige structuur (hieronder beschreven), zodat dezelfde analysecode met weinig of geen aanpassing op veel datasets kan worden toegepast.",
       "- **Interoperabel (Interoperable).** Elke dataset wordt omgevormd naar het lange formaat, één rij per respons, volgens de standaard van zijn familie (hieronder beschreven), zodat dezelfde analysecode met weinig of geen aanpassing op veel datasets kan worden toegepast."),
 "5": ("Datasets verschillen sterk in omvang (van enkele honderden responsen tot vele miljoenen) en in responstype (ja/nee-items, meerkeuzebeoordelingen, deelscores, en meer). Elke dataset wordt ook geleverd met vooraf berekende metadata — aantal deelnemers, aantal items, responsdichtheid, vakgebied en andere beschrijvende labels — zodat onderzoekers relevante datasets kunnen vinden zonder ze eerst allemaal te downloaden en te verwerken.",
       "Datasets verschillen sterk in omvang (van enkele honderden responsen tot vele miljoenen) en in responstype (ja/nee-items, meerkeuzebeoordelingen, deelscores, ongeordende categorieën, wedstrijduitslagen, rangposities, en meer)."),
 "7": ("Het `irw`-pakket, beschikbaar voor zowel **R** als **Python**, biedt eenvoudige functies om data te vinden, filteren en downloaden.",
       "Het `irw`-pakket is beschikbaar voor zowel **R** als **Python**."),
 "10": ("De eerste keer dat u het pakket gebruikt, wordt u gevraagd in te loggen met een gratis Redivis-account. Daarna downloadt één regel code elke dataset rechtstreeks naar R of Python. Vanaf dat moment is de data klaar voor analyse met standaardsoftware — bijvoorbeeld pakketten voor item response theory of factoranalyse.",
        "De eerste keer dat u het pakket gebruikt, wordt u gevraagd in te loggen met een gratis Redivis-account."),
 "6": section(
     "## De datastandaard",
     "Elke IRW-tabel staat in het **lange formaat**: één rij per respons. Wat een rij *is*, hangt af van de familie waartoe de tabel behoort, en dat geldt ook voor de kolommen die haar dragen.",
     "Twee vragen die een dataset in een IRW-familie plaatsen.",
     "Familie", "Standaard",
     "Simsyn staat niet in het schema: die familie wordt bepaald door waar de data vandaan komen, niet door hun vorm, zodat een gesimuleerde tabel elk van deze vormen kan hebben. Het [overzicht van de standaarden](/overview.qmd) geeft de kolommen van elke familie."),
}


# --------------------------------------------------------------------- ja ---
EDITS["ja"] = {
 "1": ("数百の既存データセットを集約し、それらを一つの共通形式に再構成することで、あるデータセットで検証された手法を、他の数百のデータセットでも簡単に検証できるようにします。",
       "数千の既存データセットを集約し、それらを一つの共通形式に再構成することで、あるデータセットで検証された手法を、他の数千のデータセットでも簡単に検証できるようにします。"),
 "2": ("IRW には**数百のデータセット**(「テーブル」)が含まれており、それぞれが個々の反応の集合です。反応は、ある人(または他の単位)がある項目(または他の測定プローブ)に応答するたびに生成されます。例えば:",
       "IRW には**数千のデータセット**(「テーブル」)が含まれており、それぞれが個々の反応の集合です。反応は、ある人(または他の単位)が測定プローブに出会い、その結果が記録されるたびに生成されます。何が測定プローブにあたるかは[データファミリー](/overview.qmd)によって異なります。単独で回答する項目、対決で出会う他の主体、順序をつける選択肢の集合全体、あるいは無作為に割り当てられた属性から構成されたプロファイルなどです。例えば:"),
 "3": ("- 人間の評価者による評定\n- 一連の測定プローブに対して反復的に反応が行われる、その他あらゆる状況",
       "- 人間の評価者による評定\n- 試合や直接比較など、二つの主体が対決して一方が勝つ場面\n- 優先順位投票の投票用紙、審査員による順位付け、調査の順位付け質問\n- コンジョイント実験。回答者が無作為に構成されたプロファイルの間で選択します\n- 一連の測定プローブに対して反復的に反応が行われる、その他あらゆる状況"),
 "4": ("- **相互運用可能(Interoperable)。** 各データセットは同じ単純な構造(以下で説明)に再構成されており、同じ分析コードをほとんど、あるいはまったく変更せずに多くのデータセットで実行できます。",
       "- **相互運用可能(Interoperable)。** 各データセットは、反応1件につき1行のロング形式で、そのファミリーの標準(以下で説明)に従って再構成されており、同じ分析コードをほとんど、あるいはまったく変更せずに多くのデータセットで実行できます。"),
 "5": ("データセットは規模(数百件の反応から数百万件まで)や反応の種類(はい/いいえ形式の項目、多カテゴリ評定、部分点方式のスコアなど)において大きく異なります。各データセットには、参加者数、項目数、反応密度、主題分野、その他の記述的タグといった事前計算済みのメタデータも付属しており、研究者はすべてのデータセットをダウンロードして処理する前に、関連するデータセットを見つけることができます。",
       "データセットは規模(数百件の反応から数百万件まで)や反応の種類(はい/いいえ形式の項目、多カテゴリ評定、部分点方式のスコア、順序のないカテゴリ、対決の結果、順位など)において大きく異なります。"),
 "7": ("`irw` パッケージは **R** と **Python** の両方で利用可能で、データの検索・フィルタリング・ダウンロードのための簡単な関数を提供します。",
       "`irw` パッケージは **R** と **Python** の両方で利用できます。"),
 "10": ("パッケージを初めて使用する際には、無料の Redivis アカウントでログインするよう求められます。その後は、コード1行で任意のデータセットを R または Python に直接ダウンロードできます。それ以降、データは標準的なソフトウェア(たとえば項目反応理論や因子分析のパッケージ)で分析できる状態になっています。",
        "パッケージを初めて使用する際には、無料の Redivis アカウントでログインするよう求められます。"),
 "6": section(
     "## データ標準",
     "IRW のすべてのテーブルは**ロング形式**です。反応1件につき1行です。1行が*何を表すか*は、そのテーブルが属するファミリーによって異なり、それを担う列も同様です。",
     "データセットを IRW のファミリーに分類する二つの問い。",
     "ファミリー", "標準",
     "Simsyn は図に含まれていません。このファミリーはデータの形ではなく出所によって定義されるため、シミュレーションによるテーブルはこれらのどの形をとることもあります。各ファミリーの列については[標準の概要](/overview.qmd)をご覧ください。"),
}

# --------------------------------------------------------------------- ko ---
EDITS["ko"] = {
 "1": ("수백 개의 기존 데이터셋을 한데 모아 하나의 공통 형식으로 재구성함으로써, 한 데이터셋에서 검증된 방법을 수백 개의 다른 데이터셋에서도 손쉽게 검증할 수 있게 합니다.",
       "수천 개의 기존 데이터셋을 한데 모아 하나의 공통 형식으로 재구성함으로써, 한 데이터셋에서 검증된 방법을 수천 개의 다른 데이터셋에서도 손쉽게 검증할 수 있게 합니다."),
 "2": ('IRW는 **수백 개의 데이터셋**("테이블")을 포함하며, 각 데이터셋은 개별 반응들의 모음입니다. 반응은 어떤 사람(또는 다른 단위)이 어떤 문항(또는 다른 측정 도구)에 응답할 때마다 생성됩니다. 예를 들면:',
       'IRW는 **수천 개의 데이터셋**("테이블")을 포함하며, 각 데이터셋은 개별 반응들의 모음입니다. 반응은 어떤 사람(또는 다른 단위)이 측정 도구와 마주하고 그 결과가 기록될 때마다 생성됩니다. 무엇이 측정 도구에 해당하는지는 [데이터 계열](/overview.qmd)에 따라 다릅니다. 단독으로 응답하는 문항, 대결에서 만나는 다른 개체, 순서를 매기는 선택지 전체, 또는 무작위로 할당된 속성으로 구성된 프로필 등입니다. 예를 들면:'),
 "3": ("- 인간 평가자가 부여한 평정\n- 측정 도구 집합에 대해 반복적으로 반응이 이루어지는 그 밖의 모든 상황",
       "- 인간 평가자가 부여한 평정\n- 경기와 직접 비교처럼 두 개체가 맞붙어 한쪽이 이기는 상황\n- 선호 투표 용지, 심사위원단 순위, 설문의 순위 매기기 문항\n- 컬조인트 실험. 응답자가 무작위로 구성된 프로필 중에서 선택합니다\n- 측정 도구 집합에 대해 반복적으로 반응이 이루어지는 그 밖의 모든 상황"),
 "4": ("- **상호운용 가능함(Interoperable).** 각 데이터셋은 동일한 단순 구조(아래에서 설명)로 재구성되어 있어, 동일한 분석 코드를 거의 또는 전혀 수정하지 않고 여러 데이터셋에 걸쳐 실행할 수 있습니다.",
       "- **상호운용 가능함(Interoperable).** 각 데이터셋은 반응 하나당 한 행인 롱 형식으로, 해당 계열의 표준(아래에서 설명)에 따라 재구성되어 있어, 동일한 분석 코드를 거의 또는 전혀 수정하지 않고 여러 데이터셋에 걸쳐 실행할 수 있습니다."),
 "5": ("데이터셋은 규모(수백 건의 반응부터 수백만 건까지)와 반응 유형(예/아니오 문항, 다범주 평정, 부분 점수 등)에서 매우 다양합니다. 각 데이터셋에는 참가자 수, 문항 수, 반응 밀도, 주제 분야 및 기타 서술적 태그와 같은 사전 계산된 메타데이터가 함께 제공되어, 연구자들이 모든 데이터셋을 먼저 다운로드하고 처리하지 않고도 관련 데이터셋을 찾을 수 있습니다.",
       "데이터셋은 규모(수백 건의 반응부터 수백만 건까지)와 반응 유형(예/아니오 문항, 다범주 평정, 부분 점수, 순서 없는 범주, 대결 결과, 순위 등)에서 매우 다양합니다."),
 "7": ("**R**과 **Python** 양쪽에서 모두 사용 가능한 `irw` 패키지는 데이터를 찾고, 필터링하고, 다운로드하는 간단한 함수들을 제공합니다.",
       "`irw` 패키지는 **R**과 **Python** 양쪽에서 모두 사용할 수 있습니다."),
 "10": ("패키지를 처음 사용할 때는 무료 Redivis 계정으로 로그인하라는 안내를 받게 됩니다. 이후에는 코드 한 줄이면 어떤 데이터셋이든 R이나 Python으로 직접 다운로드할 수 있습니다. 그러면 데이터는 표준 소프트웨어—예를 들어 문항 반응 이론이나 요인분석 패키지—로 바로 분석할 준비가 됩니다.",
        "패키지를 처음 사용할 때는 무료 Redivis 계정으로 로그인하라는 안내를 받게 됩니다."),
 "6": section(
     "## 데이터 표준",
     "모든 IRW 테이블은 **롱 형식**입니다. 반응 하나당 한 행입니다. 한 행이 *무엇인지*는 그 테이블이 속한 계열에 따라 다르며, 그것을 담는 열도 마찬가지입니다.",
     "데이터셋을 IRW 계열로 분류하는 두 가지 질문.",
     "계열", "표준",
     "Simsyn은 도표에 없습니다. 이 계열은 데이터의 형태가 아니라 출처로 정의되므로, 시뮬레이션 테이블은 이 형태들 중 어느 것이든 가질 수 있습니다. 각 계열의 열은 [표준 개요](/overview.qmd)에 정리되어 있습니다."),
}


# --------------------------------------------------------------------- zh ---
EDITS["zh"] = {
 "1": ("它汇集了数百个已有数据集，将它们统一转换为同一种通用格式——这样一来，在某个数据集上验证过的方法，就能轻松地在数百个其他数据集上进行检验。",
       "它汇集了数千个已有数据集，将它们统一转换为同一种通用格式——这样一来，在某个数据集上验证过的方法，就能轻松地在数千个其他数据集上进行检验。"),
 '2': ('IRW 包含**数百个数据集**（称为"表"），每个数据集都是一组个体反应记录。每当某个人（或其他单位）对某个项目（或其他测量探针）作出反应时，就会产生一条反应记录。例如：',
       'IRW 包含**数千个数据集**（称为"表"），每个数据集都是一组个体反应记录。每当某个人（或其他单位）遇到一个测量探针并记录下结果时，就会产生一条反应记录。什么算作探针取决于[数据族](/overview.qmd)：单独作答的项目、在对战中遇到的另一个主体、需要排序的一整组选项，或者由随机分配的属性组成的一份档案。例如：'),
 "3": ("- 人类评分者给出的评分\n- 任何其他涉及对一组测量探针重复作出反应的情境",
       "- 人类评分者给出的评分\n- 比赛、对局与直接比较，其中两个主体交锋、一方获胜\n- 偏好投票的选票、评审团排名以及问卷中的排序题\n- 联合分析实验，受访者在随机生成的档案之间做出选择\n- 任何其他涉及对一组测量探针重复作出反应的情境"),
 "4": ("- **可互操作（Interoperable）。** 每个数据集都被转换为相同的简单结构（详见下文），使得同一套分析代码可以在众多数据集上运行，几乎不需要修改。",
       "- **可互操作（Interoperable）。** 每个数据集都被转换为长格式，每条反应占一行，并遵循其所属数据族的标准（详见下文），使得同一套分析代码可以在众多数据集上运行，几乎不需要修改。"),
 "5": ("各数据集在规模上差异很大（从数百条反应记录到数百万条不等），在反应类型上也各不相同（是/否类项目、多类别评分、部分得分等）。每个数据集还附带预先计算好的元数据——参与者人数、项目数量、反应密度、学科领域及其他描述性标签——使研究者无需先下载并处理全部数据,即可找到相关数据集。",
       "各数据集在规模上差异很大（从数百条反应记录到数百万条不等），在反应类型上也各不相同（是/否类项目、多类别评分、部分得分、无序类别、对战结果、排名位次等）。"),
 "7": ("`irw` 软件包同时提供 **R** 和 **Python** 版本,提供了简单的函数用于查找、筛选和下载数据。",
       "`irw` 软件包同时提供 **R** 和 **Python** 版本。"),
 "10": ("首次使用该软件包时,系统会要求你使用免费的 Redivis 账号登录。此后,只需一行代码即可将任意数据集直接下载到 R 或 Python 中。此时,数据便已可以使用标准软件进行分析——例如项目反应理论或因子分析相关软件包。",
        "首次使用该软件包时,系统会要求你使用免费的 Redivis 账号登录。"),
 "6": section(
     "## 数据标准",
     "每一张 IRW 表都采用**长格式**：每条反应占一行。一行具体*代表什么*，取决于该表所属的数据族，承载它的列也是如此。",
     "将数据集归入某个 IRW 数据族的两个问题。",
     "数据族", "标准",
     "Simsyn 未列入图中：该数据族由数据的来源而非形式界定，因此一张模拟表可以具有其中任一种形式。各数据族的列请见[标准概览](/overview.qmd)。"),
}

# ---------------------------------------------------------------- zh-hant ---
EDITS["zh-hant"] = {
 "1": ("它彙集了數百個既有資料集，將它們統一轉換為同一種通用格式——如此一來，在某個資料集上驗證過的方法，就能輕易地在數百個其他資料集上進行檢驗。",
       "它彙集了數千個既有資料集，將它們統一轉換為同一種通用格式——如此一來，在某個資料集上驗證過的方法，就能輕易地在數千個其他資料集上進行檢驗。"),
 "2": ("IRW 包含**數百個資料集**（稱為「表」），每個資料集都是一組個別反應紀錄。每當某個人（或其他單位）對某個項目（或其他測量探針）作出反應時，就會產生一筆反應紀錄。例如：",
       "IRW 包含**數千個資料集**（稱為「表」），每個資料集都是一組個別反應紀錄。每當某個人（或其他單位）遇到一個測量探針並記錄下結果時，就會產生一筆反應紀錄。什麼算作探針取決於[資料族](/overview.qmd)：單獨作答的項目、在對戰中遇到的另一個主體、需要排序的一整組選項，或者由隨機分配的屬性組成的一份檔案。例如："),
 "3": ("- 人類評分者給出的評分\n- 任何其他涉及對一組測量探針重複作出反應的情境",
       "- 人類評分者給出的評分\n- 比賽、對局與直接比較，其中兩個主體交鋒、一方獲勝\n- 偏好投票的選票、評審團排名以及問卷中的排序題\n- 聯合分析實驗，受訪者在隨機生成的檔案之間做出選擇\n- 任何其他涉及對一組測量探針重複作出反應的情境"),
 "4": ("- **可互通（Interoperable）。** 每個資料集都被轉換為相同的簡單結構（詳見下文），使得同一套分析程式碼可以在眾多資料集上執行，幾乎不需要修改。",
       "- **可互通（Interoperable）。** 每個資料集都被轉換為長格式，每筆反應佔一列，並遵循其所屬資料族的標準（詳見下文），使得同一套分析程式碼可以在眾多資料集上執行，幾乎不需要修改。"),
 "5": ("各資料集在規模上差異很大（從數百筆反應紀錄到數百萬筆不等），在反應類型上也各不相同（是/否類項目、多類別評分、部分給分等）。每個資料集還附帶預先計算好的中繼資料——參與者人數、項目數量、反應密度、學科領域及其他描述性標籤——使研究者無需先下載並處理全部資料，即可找到相關資料集。",
       "各資料集在規模上差異很大（從數百筆反應紀錄到數百萬筆不等），在反應類型上也各不相同（是/否類項目、多類別評分、部分給分、無序類別、對戰結果、排名位次等）。"),
 "7": ("`irw` 套件同時提供 **R** 與 **Python** 版本，提供了簡單的函式用於尋找、篩選與下載資料。",
       "`irw` 套件同時提供 **R** 與 **Python** 版本。"),
 "10": ("首次使用該套件時，系統會要求你使用免費的 Redivis 帳號登入。此後，只需一行程式碼即可將任意資料集直接下載到 R 或 Python 中。此時，資料便已可以使用標準軟體進行分析——例如項目反應理論或因素分析相關套件。",
        "首次使用該套件時，系統會要求你使用免費的 Redivis 帳號登入。"),
 "6": section(
     "## 資料標準",
     "每一張 IRW 表都採用**長格式**：每筆反應佔一列。一列具體*代表什麼*，取決於該表所屬的資料族，承載它的欄位也是如此。",
     "將資料集歸入某個 IRW 資料族的兩個問題。",
     "資料族", "標準",
     "Simsyn 未列入圖中：該資料族由資料的來源而非形式界定，因此一張模擬表可以具有其中任一種形式。各資料族的欄位請見[標準概覽](/overview.qmd)。"),
}


# --------------------------------------------------------------------- ru ---
EDITS["ru"] = {
 "1": para("Он объединяет сотни",
     "Item Response Warehouse (IRW) применяет те же принципы FAIR к данным об ответах на задания. Он объединяет тысячи существующих наборов данных и преобразует их в единый общий формат — так что метод, проверенный на одном наборе данных, можно легко проверить на тысячах других."),
 "2": para("IRW содержит **сотни",
     "IRW содержит **тысячи наборов данных** («таблиц»), каждый из которых представляет собой коллекцию отдельных ответов. Ответ создаётся всякий раз, когда какой-либо человек (или иная единица) сталкивается с измерительным стимулом и результат фиксируется. Что именно считается стимулом, зависит от [семейства данных](/overview.qmd): задание, на которое отвечают по отдельности; другой участник, с которым сводит состязание; целый набор вариантов, которые нужно упорядочить; или профиль, собранный из случайно назначенных признаков. Примеры включают:"),
 "3": line("Любые другие ситуации",
     "- Игры, матчи и прямые сравнения, где два участника встречаются и один побеждает\n- Бюллетени рейтингового голосования, ранжирования жюри и вопросы на упорядочивание в опросах\n- Конджойнт-эксперименты, где респонденты выбирают между случайно собранными профилями\n- Любые другие ситуации, где присутствуют повторяющиеся ответы на набор измерительных стимулов"),
 "4": line("**Совместимым (Interoperable).**",
     "- **Совместимым (Interoperable).** Каждый набор данных преобразован в длинный формат, по одной строке на ответ, в соответствии со стандартом своего семейства (описан ниже), поэтому один и тот же код анализа можно применять к множеству наборов данных с минимальными изменениями или вовсе без них."),
 "5": para("Наборы данных сильно различаются по объёму",
     "Наборы данных сильно различаются по объёму (от нескольких сотен ответов до нескольких миллионов) и по типу ответа (задания «да/нет», многокатегориальные оценки, баллы за частичное выполнение, неупорядоченные категории, исходы состязаний, места в ранжировке и другое)."),
 "7": line("Пакет `irw`, доступный как для",
     "Пакет `irw` доступен как для **R**, так и для **Python**."),
 "10": para("При первом использовании пакета",
     "При первом использовании пакета вам будет предложено войти с помощью бесплатной учётной записи Redivis."),
 "6": section(
     "## Стандарт данных",
     "Каждая таблица IRW имеет **длинный формат**: по одной строке на ответ. Что *именно* представляет собой строка, зависит от семейства, к которому относится таблица, и то же самое касается столбцов, которые её несут.",
     "Два вопроса, которые относят набор данных к семейству IRW.",
     "Семейство", "Стандарт",
     "Simsyn нет на схеме: это семейство определяется происхождением данных, а не их формой, поэтому смоделированная таблица может иметь любую из этих форм. Столбцы каждого семейства приведены в [обзоре стандартов](/overview.qmd)."),
}


# --------------------------------------------------------------------- pl ---
EDITS["pl"] = {
 "1": para("Gromadzi setki istniejących",
     "Item Response Warehouse (IRW) stosuje te same zasady FAIR do danych o odpowiedziach na zadania. Gromadzi tysiące istniejących zbiorów danych i przekształca je w jeden wspólny format — dzięki czemu metoda przetestowana na jednym zbiorze danych może łatwo zostać przetestowana na tysiącach innych."),
 "2": para("IRW zawiera **setki zbiorów danych**",
     'IRW zawiera **tysiące zbiorów danych** ("tabel"), z których każdy jest zbiorem indywidualnych odpowiedzi. Odpowiedź powstaje za każdym razem, gdy jakaś osoba (lub inna jednostka) styka się z bodźcem pomiarowym i wynik zostaje zapisany. To, co jest bodźcem, zależy od [rodziny danych](/overview.qmd): zadanie, na które odpowiada się osobno; inny podmiot napotkany w starciu; cały zestaw opcji, który należy uporządkować; albo profil złożony z losowo przypisanych cech. Przykłady obejmują:'),
 "3": line("Każdą inną sytuację",
     "- Mecze, partie i bezpośrednie porównania, w których dwa podmioty mierzą się ze sobą i jeden wygrywa\n- Karty do głosowania preferencyjnego, rankingi jury oraz pytania rankingowe w ankietach\n- Eksperymenty conjoint, w których respondenci wybierają spośród losowo zbudowanych profili\n- Każdą inną sytuację obejmującą powtarzane odpowiedzi na zestaw narzędzi pomiarowych"),
 "4": line("**Interoperacyjny (Interoperable).**",
     "- **Interoperacyjny (Interoperable).** Każdy zbiór danych jest przekształcany do formatu długiego, jeden wiersz na odpowiedź, zgodnie ze standardem swojej rodziny (opisanym poniżej), dzięki czemu ten sam kod analityczny może działać na wielu zbiorach danych przy niewielkiej modyfikacji lub bez żadnej."),
 "5": para("Zbiory danych różnią się znacznie",
     "Zbiory danych różnią się znacznie pod względem wielkości (od kilkuset odpowiedzi do wielu milionów) oraz rodzaju odpowiedzi (zadania tak/nie, oceny wielokategorialne, wyniki częściowe, kategorie nieuporządkowane, wyniki starć, pozycje w rankingu i inne)."),
 "7": line("Pakiet `irw`, dostępny zarówno dla",
     "Pakiet `irw` jest dostępny zarówno dla **R**, jak i **Pythona**."),
 "10": para("Przy pierwszym użyciu pakietu",
     "Przy pierwszym użyciu pakietu zostaniesz poproszony o zalogowanie się za pomocą bezpłatnego konta Redivis."),
 "6": section(
     "## Standard danych",
     "Każda tabela IRW jest w **formacie długim**: jeden wiersz na odpowiedź. To, czym *jest* wiersz, zależy od rodziny, do której należy tabela, podobnie jak kolumny, które go niosą.",
     "Dwa pytania, które przypisują zbiór danych do rodziny IRW.",
     "Rodzina", "Standard",
     "Simsyn nie występuje na schemacie: ta rodzina jest określona przez pochodzenie danych, a nie przez ich kształt, więc tabela symulowana może mieć dowolny z tych kształtów. Kolumny każdej rodziny podaje [przegląd standardów](/overview.qmd)."),
}

# --------------------------------------------------------------------- tr ---
EDITS["tr"] = {
 "1": para("Yüzlerce mevcut veri kümesini",
     "Item Response Warehouse (IRW), aynı FAIR ilkelerini madde tepki verilerine uygular. Binlerce mevcut veri kümesini bir araya getirir ve bunları tek bir ortak biçime dönüştürür — böylece bir veri kümesi üzerinde test edilen bir yöntem, binlerce başka veri kümesi üzerinde de kolayca test edilebilir."),
 "2": para("**yüzlerce veri kümesi**",
     'IRW, her biri bireysel tepkilerden oluşan bir koleksiyon olan **binlerce veri kümesi** ("tablo") içerir. Bir kişi (veya başka bir birim) bir ölçme aracıyla karşılaştığında ve sonuç kaydedildiğinde bir tepki oluşur. Neyin ölçme aracı sayıldığı [veri ailesine](/overview.qmd) göre değişir: tek başına yanıtlanan bir madde, bir karşılaşmada karşısına çıkılan başka bir varlık, sıraya konması gereken seçeneklerin tamamı ya da rastgele atanmış özelliklerden oluşturulmuş bir profil. Örnekler şunları içerir:'),
 "3": line("tekrarlanan tepkilerin verildiği diğer her türlü durum",
     "- İki varlığın karşı karşıya geldiği ve birinin kazandığı maçlar, oyunlar ve doğrudan karşılaştırmalar\n- Tercihli oy pusulaları, jüri sıralamaları ve anketlerdeki sıralama soruları\n- Katılımcıların rastgele oluşturulmuş profiller arasında seçim yaptığı conjoint deneyleri\n- Bir dizi ölçme aracına tekrarlanan tepkilerin verildiği diğer her türlü durum"),
 "4": line("**Birlikte çalışabilir (Interoperable).**",
     "- **Birlikte çalışabilir (Interoperable).** Her veri kümesi, tepki başına bir satır olacak şekilde uzun biçime ve ait olduğu ailenin standardına (aşağıda açıklanmıştır) göre dönüştürülür, böylece aynı analiz kodu birçok veri kümesinde çok az değişiklikle veya hiç değişiklik yapılmadan çalıştırılabilir."),
 "5": para("Veri kümeleri boyut açısından",
     "Veri kümeleri boyut açısından (birkaç yüz tepkiden milyonlarcasına kadar) ve tepki türü açısından (evet/hayır maddeleri, çok kategorili puanlamalar, kısmi puanlar, sırasız kategoriler, karşılaşma sonuçları, sıralama konumları ve daha fazlası) büyük farklılıklar gösterir."),
 "7": line("Hem **R** hem de **Python** için mevcut olan `irw` paketi",
     "`irw` paketi hem **R** hem de **Python** için mevcuttur."),
 "10": para("Paketi ilk kullandığınızda",
     "Paketi ilk kullandığınızda, ücretsiz bir Redivis hesabıyla giriş yapmanız istenir."),
 "6": section(
     "## Veri standardı",
     "Her IRW tablosu **uzun biçimdedir**: tepki başına bir satır. Bir satırın *ne olduğu* tablonun ait olduğu aileye bağlıdır; onu taşıyan sütunlar da öyle.",
     "Bir veri kümesini bir IRW ailesine yerleştiren iki soru.",
     "Aile", "Standart",
     "Simsyn şemada yer almaz: bu aile verinin biçimiyle değil nereden geldiğiyle tanımlanır, dolayısıyla simülasyon tablosu bu biçimlerden herhangi birine sahip olabilir. Her ailenin sütunları için [standartlara genel bakış](/overview.qmd) sayfasına bakın."),
}

# --------------------------------------------------------------------- vi ---
EDITS["vi"] = {
 "1": para("Nó tập hợp hàng trăm",
     "Item Response Warehouse (IRW) áp dụng cùng các nguyên tắc FAIR đó cho dữ liệu phản hồi câu hỏi. Nó tập hợp hàng nghìn bộ dữ liệu đã có và định dạng lại chúng thành một định dạng chung duy nhất — nhờ đó một phương pháp đã được kiểm nghiệm trên một bộ dữ liệu có thể dễ dàng được kiểm nghiệm trên hàng nghìn bộ dữ liệu khác."),
 "2": para("IRW chứa **hàng trăm bộ dữ liệu**",
     'IRW chứa **hàng nghìn bộ dữ liệu** (gọi là "bảng"), mỗi bộ là một tập hợp các phản hồi riêng lẻ. Một phản hồi được tạo ra mỗi khi một người (hoặc đơn vị khác) gặp một đầu dò đo lường và kết quả được ghi lại. Điều gì được coi là đầu dò tùy thuộc vào [họ dữ liệu](/overview.qmd): một câu hỏi được trả lời riêng lẻ, một thực thể khác gặp trong một cuộc đọi đầu, toàn bộ một tập lựa chọn cần được xếp thứ tự, hoặc một hồ sơ được ghép từ các thuộc tính gán ngẫu nhiên. Ví dụ bao gồm:'),
 "3": line("Bất kỳ tình huống nào khác",
     "- Các trận đấu, ván đấu và so sánh trực tiếp, nơi hai thực thể đối đầu và một bên thắng\n- Lá phiếu bầu ưu tiên, bảng xếp hạng của ban giám khảo và câu hỏi xếp hạng trong khảo sát\n- Thí nghiệm conjoint, nơi người trả lời chọn giữa các hồ sơ được tạo ngẫu nhiên\n- Bất kỳ tình huống nào khác liên quan đến các phản hồi lặp lại đối với một tập hợp đầu dò đo lường"),
 "4": line("**Có thể tương tác (Interoperable).**",
     "- **Có thể tương tác (Interoperable).** Mỗi bộ dữ liệu được định dạng lại theo định dạng dài, một dòng cho mỗi phản hồi, theo chuẩn của họ dữ liệu của nó (mô tả bên dưới), để cùng một đoạn mã phân tích có thể chạy trên nhiều bộ dữ liệu mà chỉ cần chỉnh sửa rất ít hoặc không cần chỉnh sửa."),
 "5": para("Các bộ dữ liệu có sự khác biệt lớn",
     "Các bộ dữ liệu có sự khác biệt lớn về quy mô (từ vài trăm phản hồi đến hàng triệu) và về loại phản hồi (mục có/không, đánh giá nhiều hạng mục, điểm số theo phần, hạng mục không có thứ tự, kết quả đọi đầu, vị trí xếp hạng, và nhiều hơn nữa)."),
 "7": line("Gói `irw`, có sẵn cho cả",
     "Gói `irw` có sẵn cho cả **R** và **Python**."),
 "10": para("Lần đầu tiên sử dụng gói này",
     "Lần đầu tiên sử dụng gói này, bạn sẽ được yêu cầu đăng nhập bằng tài khoản Redivis miễn phí."),
 "6": section(
     "## Chuẩn dữ liệu",
     "Mọi bảng IRW đều ở **định dạng dài**: một dòng cho mỗi phản hồi. Một dòng *là gì* tùy thuộc vào họ mà bảng đó thuộc về, và các cột mang nó cũng vậy.",
     "Hai câu hỏi để xếp một bộ dữ liệu vào một họ của IRW.",
     "Họ", "Chuẩn",
     "Simsyn không có trong sơ đồ: họ này được xác định bởi nguồn gốc của dữ liệu chứ không phải hình dạng, nên một bảng mô phỏng có thể mang bất kỳ hình dạng nào trong số này. Các cột của từng họ được liệt kê trong [tổng quan về các chuẩn](/overview.qmd)."),
}


# --------------------------------------------------------------------- th ---
EDITS["th"] = {
 "1": para("โดยรวบรวมชุดข้อมูลที่มีอยู่แล้วหลายร้อยชุด",
     "Item Response Warehouse (IRW) นำหลักการ FAIR เดียวกันนี้มาใช้กับข้อมูลการตอบสนองต่อข้อสอบ โดยรวบรวมชุดข้อมูลที่มีอยู่แล้วหลายพันชุดและปรับให้อยู่ในรูปแบบเดียวกัน — เพื่อให้วิธีการที่ทดสอบกับชุดข้อมูลหนึ่งสามารถนำไปทดสอบกับชุดข้อมูลอื่นอีกหลายพันชุดได้อย่างง่ายดาย"),
 "2": para("**ชุดข้อมูลหลายร้อยชุด**",
     'IRW ประกอบด้วย **ชุดข้อมูลหลายพันชุด** ("ตาราง") แต่ละชุดเป็นการรวบรวมคำตอบของแต่ละบุคคล คำตอบหนึ่งจะเกิดขึ้นทุกครั้งที่บุคคล (หรือหน่วยอื่น) พบกับแบบวัดหนึ่งและมีการบันทึกผลลัพธ์ สิ่งที่นับเป็นแบบวัดนั้นแตกต่างกันไปตาม[ตระกูลข้อมูล](/overview.qmd) ได้แก่ ข้อสอบที่ตอบทีละข้อ อีกหน่วยหนึ่งที่พบในการแข่งขัน ชุดของตัวเลือกทั้งหมดที่ต้องเรียงลำดับ หรือโปรไฟล์ที่ประกอบขึ้นจากคุณลักษณะที่กำหนดแบบสุ่ม ตัวอย่างเช่น'),
 "3": line("สถานการณ์อื่นใดที่เกี่ยวข้อง",
     "- การแข่งขัน การประลอง และการเปรียบเทียบโดยตรง ซึ่งสองฝ่ายพบกันและมีผู้ชนะหนึ่งฝ่าย\n- บัตรลงคะแนนแบบจัดลำดับ การจัดอันดับโดยคณะกรรมการ และคำถามแบบจัดอันดับในแบบสำรวจ\n- การทดลองแบบ conjoint ที่ผู้ตอบเลือกระหว่างโปรไฟล์ที่สร้างขึ้นแบบสุ่ม\n- สถานการณ์อื่นใดที่เกี่ยวข้องกับการตอบสนองซ้ำ ๆ ต่อชุดเครื่องมือวัดผล"),
 "4": line("**ทำงานร่วมกันได้ (Interoperable)**",
     "- **ทำงานร่วมกันได้ (Interoperable)** ชุดข้อมูลแต่ละชุดถูกปรับให้อยู่ในรูปแบบยาว หนึ่งแถวต่อหนึ่งคำตอบ ตามมาตรฐานของตระกูลนั้น (อธิบายไว้ด้านล่าง) เพื่อให้โค้ดวิเคราะห์ชุดเดียวกันสามารถใช้ได้กับหลายชุดข้อมูลโดยแทบไม่ต้องแก้ไข หรือไม่ต้องแก้ไขเลย"),
 "5": para("ชุดข้อมูลมีความหลากหลายอย่างมาก",
     "ชุดข้อมูลมีความหลากหลายอย่างมากทั้งในด้านขนาด (ตั้งแต่ไม่กี่ร้อยคำตอบไปจนถึงหลายล้านคำตอบ) และประเภทของคำตอบ (ข้อสอบแบบใช่/ไม่ใช่ การให้คะแนนแบบหลายหมวดหมู่ คะแนนบางส่วน หมวดหมู่ที่ไม่มีลำดับ ผลการแข่งขัน อันดับที่จัดไว้ และอื่น ๆ)"),
 "7": line("แพ็กเกจ `irw` ซึ่งมีให้ใช้งานทั้งใน",
     "แพ็กเกจ `irw` มีให้ใช้งานทั้งใน **R** และ **Python**"),
 "10": para("ครั้งแรกที่คุณใช้แพ็กเกจนี้",
     "ครั้งแรกที่คุณใช้แพ็กเกจนี้ ระบบจะขอให้คุณเข้าสู่ระบบด้วยบัญชี Redivis ที่ไม่มีค่าใช้จ่าย"),
 "6": section(
     "## มาตรฐานข้อมูล",
     "ตาราง IRW ทุกตารางอยู่ใน**รูปแบบยาว** คือหนึ่งแถวต่อหนึ่งคำตอบ สิ่งที่หนึ่งแถว*เป็น*ขึ้นอยู่กับตระกูลที่ตารางนั้นสังกัดอยู่ และคอลัมน์ที่รองรับก็เช่นกัน",
     "สองคำถามที่จัดชุดข้อมูลเข้าตระกูลของ IRW",
     "ตระกูล", "มาตรฐาน",
     "Simsyn ไม่ปรากฏในแผนภูมิ เพราะตระกูลนี้ถูกกำหนดด้วยที่มาของข้อมูล ไม่ใช่รูปร่าง ตารางจำลองจึงมีรูปร่างแบบใดก็ได้ในกลุ่มนี้ คอลัมน์ของแต่ละตระกูลดูได้ที่[ภาพรวมมาตรฐาน](/overview.qmd)"),
}

# --------------------------------------------------------------------- hi ---
EDITS["hi"] = {
 "1": para("यह सैकड़ों मौजूदा डेटासेट",
     "Item Response Warehouse (IRW) आइटम-प्रतिक्रिया डेटा पर यही FAIR सिद्धांत लागू करता है। यह हज़ारों मौजूदा डेटासेट को एक साथ लाता है और उन्हें एक साझा प्रारूप में ढालता है — ताकि किसी एक डेटासेट पर परखी गई विधि को हज़ारों अन्य डेटासेट पर भी आसानी से परखा जा सके।"),
 "2": para("IRW में **सैकड़ों डेटासेट**",
     'IRW में **हज़ारों डेटासेट** ("टेबल") हैं, जिनमें से प्रत्येक व्यक्तिगत प्रतिक्रियाओं का एक संग्रह है। जब भी कोई व्यक्ति (या कोई अन्य इकाई) किसी मापन-प्रोब से सामना करता है और परिणाम दर्ज होता है, तो एक प्रतिक्रिया उत्पन्न होती है। प्रोब क्या है, यह [डेटा परिवार](/overview.qmd) पर निर्भर करता है: अकेले उत्तर दिया गया कोई आइटम, किसी मुकाबले में मिली कोई दूसरी इकाई, विकल्पों का पूरा समूह जिसे क्रम में रखना हो, या यादृच्छिक रूप से निर्धारित विशेषताओं से बना कोई प्रोफ़ाइल। उदाहरणों में शामिल हैं:'),
 "3": line("कोई भी अन्य स्थिति जिसमें मापन-प्रोब",
     "- खेल, मुकाबले और आमने-सामने की तुलनाएँ, जहाँ दो इकाइयाँ भिड़ती हैं और एक जीतती है\n- वरीयता-क्रम वाले मतपत्र, निर्णायक मंडल की वरीयताएँ और सर्वेक्षण के क्रम-निर्धारण प्रश्न\n- कॉन्जॉइंट प्रयोग, जिनमें उत्तरदाता यादृच्छिक रूप से बनी प्रोफ़ाइलों में से चुनते हैं\n- कोई भी अन्य स्थिति जिसमें मापन-प्रोब के एक समूह पर बार-बार प्रतिक्रियाएं दी जाती हों"),
 "4": line("**अंतरसंचालनीय (Interoperable) हो।**",
     "- **अंतरसंचालनीय (Interoperable) हो।** प्रत्येक डेटासेट को लॉन्ग प्रारूप में, प्रति प्रतिक्रिया एक पंक्ति, उसके परिवार के मानक (नीचे वर्णित) के अनुसार ढाला गया है, जिससे एक ही विश्लेषण कोड को कई डेटासेट पर बिना या न्यूनतम बदलाव के चलाया जा सकता है।"),
 "5": para("डेटासेट आकार में",
     "डेटासेट आकार में (कुछ सौ प्रतिक्रियाओं से लेकर कई लाख तक) और प्रतिक्रिया के प्रकार में (हाँ/नहीं आइटम, बहु-श्रेणी रेटिंग, आंशिक-अंक स्कोर, क्रमरहित श्रेणियाँ, मुकाबलों के परिणाम, वरीयता-स्थान, और अन्य) व्यापक रूप से भिन्न होते हैं।"),
 "7": line("`irw` पैकेज, जो **R** और **Python** दोनों",
     "`irw` पैकेज **R** और **Python** दोनों के लिए उपलब्ध है।"),
 "10": para("पैकेज का पहली बार उपयोग",
     "पैकेज का पहली बार उपयोग करने पर, आपसे एक निःशुल्क Redivis खाते से लॉग इन करने के लिए कहा जाएगा।"),
 "6": section(
     "## डेटा मानक",
     "हर IRW टेबल **लॉन्ग प्रारूप** में है: प्रति प्रतिक्रिया एक पंक्ति। एक पंक्ति *क्या है*, यह इस पर निर्भर करता है कि टेबल किस परिवार की है, और उसे ढोने वाले कॉलम भी उसी पर।",
     "दो प्रश्न जो किसी डेटासेट को IRW के किसी परिवार में रखते हैं।",
     "परिवार", "मानक",
     "Simsyn चार्ट में नहीं है: यह परिवार डेटा के आकार से नहीं, बल्कि उसके स्रोत से परिभाषित होता है, इसलिए कोई सिमुलेटेड टेबल इनमें से किसी भी आकार की हो सकती है। हर परिवार के कॉलम [मानकों के अवलोकन](/overview.qmd) में दिए गए हैं।"),
}


# --------------------------------------------------------------------- ms ---
EDITS["ms"] = {
 "1": para("Ia menghimpunkan ratusan set data",
     "Item Response Warehouse (IRW) menerapkan prinsip FAIR yang sama untuk data respons item. Ia menghimpunkan ribuan set data sedia ada dan membentuk semula ke dalam satu format yang sama — supaya sesuatu kaedah yang diuji ke atas satu set data boleh diuji dengan mudah ke atas ribuan set data yang lain."),
 "2": para("IRW mengandungi **ratusan set data**",
     'IRW mengandungi **ribuan set data** ("jadual"), setiap satunya adalah koleksi respons individu. Satu respons dihasilkan setiap kali seseorang (atau unit lain) berdepan dengan sesuatu alat pengukuran dan hasilnya direkodkan. Apa yang dikira sebagai alat pengukuran berbeza mengikut [keluarga data](/overview.qmd): sesuatu item yang dijawab secara berasingan, entiti lain yang ditemui dalam sesuatu pertandingan, keseluruhan set pilihan yang perlu disusun mengikut urutan, atau profil yang dibentuk daripada ciri-ciri yang diberikan secara rawak. Contohnya termasuk:'),
 "3": line("Mana-mana situasi lain yang melibatkan respons berulang",
     "- Perlawanan, permainan dan perbandingan secara langsung, di mana dua entiti bertemu dan satu menang\n- Kertas undi keutamaan, susunan kedudukan oleh panel juri, dan soalan menyusun kedudukan dalam tinjauan\n- Eksperimen conjoint, di mana responden memilih antara profil yang dibina secara rawak\n- Mana-mana situasi lain yang melibatkan respons berulang kepada satu set alat pengukuran"),
 "4": line("**Boleh berinteroperasi (Interoperable).**",
     "- **Boleh berinteroperasi (Interoperable).** Setiap set data dibentuk semula ke dalam format panjang, satu baris bagi setiap respons, mengikut standard keluarganya (diterangkan di bawah), supaya kod analisis yang sama boleh dijalankan ke atas banyak set data dengan sedikit atau tiada pengubahsuaian."),
 "5": para("Set data berbeza dengan ketara dari segi saiz",
     "Set data berbeza dengan ketara dari segi saiz (daripada beberapa ratus respons kepada berjuta-juta) dan jenis respons (item ya/tidak, penilaian pelbagai kategori, skor separa, kategori tanpa urutan, keputusan pertandingan, kedudukan dalam susunan, dan banyak lagi)."),
 "7": line("Pakej `irw`, yang tersedia untuk kedua-dua",
     "Pakej `irw` tersedia untuk kedua-dua **R** dan **Python**."),
 "10": para("Pada kali pertama anda menggunakan pakej ini",
     "Pada kali pertama anda menggunakan pakej ini, anda akan diminta untuk log masuk dengan akaun Redivis percuma."),
 "6": section(
     "## Standard data",
     "Setiap jadual IRW berada dalam **format panjang**: satu baris bagi setiap respons. Apa yang sesuatu baris itu *wakili* bergantung pada keluarga jadual tersebut, begitu juga lajur-lajur yang membawanya.",
     "Dua soalan yang meletakkan sesuatu set data dalam keluarga IRW.",
     "Keluarga", "Standard",
     "Simsyn tiada dalam carta: keluarga ini ditakrifkan oleh asal usul data, bukan bentuknya, jadi jadual simulasi boleh mempunyai mana-mana daripada bentuk ini. Lajur bagi setiap keluarga ada dalam [gambaran keseluruhan standard](/overview.qmd)."),
}

# --------------------------------------------------------------------- sw ---
EDITS["sw"] = {
 "1": para("Inakusanya mamia ya seti za data",
     "Item Response Warehouse (IRW) inatumia kanuni hizohizo za FAIR kwa data ya majibu ya vipengele. Inakusanya maelfu ya seti za data zilizopo na kuzibadilisha kuwa muundo mmoja wa pamoja — ili mbinu iliyojaribiwa kwenye seti moja ya data iweze kujaribiwa kwa urahisi kwenye maelfu ya seti nyingine."),
 "2": para("IRW ina **mamia ya seti za data**",
     'IRW ina **maelfu ya seti za data** ("majedwali"), kila moja ikiwa mkusanyiko wa majibu ya mtu mmoja mmoja. Jibu hutokea kila mara mtu (au kitengo kingine) anapokutana na kifaa cha upimaji na matokeo yakarekodiwa. Kinachohesabiwa kuwa kifaa cha upimaji hutofautiana kulingana na [familia ya data](/overview.qmd): kipengele kinachojibiwa peke yake, kitu kingine kinachokutana nacho katika mashindano, seti nzima ya chaguo inayopaswa kupangwa kwa mpangilio, au wasifu uliojengwa kutokana na sifa zilizotolewa kwa nasibu. Mifano ni pamoja na:'),
 "3": line("Hali nyingine yoyote inayohusisha majibu yanayojirudia",
     "- Mechi, michezo na ulinganishaji wa moja kwa moja, ambapo vitu viwili hukutana na kimoja hushinda\n- Karatasi za kupiga kura kwa mpangilio wa upendeleo, upangaji wa majaji, na maswali ya kupanga katika tafiti\n- Majaribio ya conjoint, ambapo wahojiwa huchagua kati ya wasifu uliojengwa kwa nasibu\n- Hali nyingine yoyote inayohusisha majibu yanayojirudia kwa seti ya vifaa vya upimaji"),
 "4": line("**Inayoweza kutumika pamoja na mifumo mingine (Interoperable).**",
     "- **Inayoweza kutumika pamoja na mifumo mingine (Interoperable).** Kila seti ya data hubadilishwa kuwa muundo mrefu, safu moja kwa kila jibu, kufuatana na kiwango cha familia yake (kilichoelezwa hapa chini), ili msimbo huohuo wa uchambuzi uweze kutumika kwenye seti nyingi za data kwa mabadiliko kidogo au bila mabadiliko yoyote."),
 "5": para("Seti za data hutofautiana sana kwa ukubwa",
     "Seti za data hutofautiana sana kwa ukubwa (kutoka majibu mia chache hadi mamilioni mengi) na kwa aina ya majibu (vipengele vya ndiyo/hapana, ukadiriaji wa makundi mengi, alama za sehemu, makundi yasiyo na mpangilio, matokeo ya mashindano, nafasi za upangaji, na mengineyo)."),
 "7": line("Kifurushi cha `irw`, kinachopatikana kwa",
     "Kifurushi cha `irw` kinapatikana kwa **R** na **Python**."),
 "10": para("Mara ya kwanza kutumia kifurushi hiki",
     "Mara ya kwanza kutumia kifurushi hiki, utaombwa kuingia (login) kwa akaunti huru ya Redivis."),
 "6": section(
     "## Kiwango cha data",
     "Kila jedwali la IRW liko katika **muundo mrefu**: safu moja kwa kila jibu. Safu moja *ni nini* hutegemea familia ambayo jedwali hilo ni mali yake, na vivyo hivyo safu wima zinazoibeba.",
     "Maswali mawili yanayoweka seti ya data katika familia ya IRW.",
     "Familia", "Kiwango",
     "Simsyn haiko kwenye chati: familia hii hufafanuliwa na chanzo cha data, si umbo lake, hivyo jedwali la kuiga linaweza kuwa na umbo lolote kati ya haya. Safu wima za kila familia zimeorodheshwa katika [muhtasari wa viwango](/overview.qmd)."),
}


# ----------------------------------------------------------------- bn, ta ---
EDITS["bn"] = {
 "1": para("এটি শত শত বিদ্যমান",
     "আইটেম রেসপন্স ওয়্যারহাউস (IRW) আইটেম রেসপন্স ডেটার ওপর একই FAIR নীতি প্রয়োগ করে। এটি হাজার হাজার বিদ্যমান ডেটাসেটকে একত্র করে একটি সাধারণ ফরম্যাটে রূপান্তরিত করে — যাতে একটি ডেটাসেটে পরীক্ষিত কোনো পদ্ধতি সহজেই অন্য হাজার হাজার ডেটাসেটে পরীক্ষা করা যায়।"),
 "2": para("IRW-এ **শত শত ডেটাসেট**",
     'IRW-এ **হাজার হাজার ডেটাসেট** ("টেবিল") রয়েছে, প্রতিটি পৃথক রেসপন্সের একটি সংগ্রহ। কোনো ব্যক্তি (বা অন্য একক) কোনো পরিমাপ প্রোবের সামনে পড়লে এবং ফলাফল লিপিবদ্ধ হলেই একটি রেসপন্স তৈরি হয়। কোনটি প্রোব হিসেবে গণ্য হবে তা [ডেটা পরিবার](/overview.qmd) অনুসারে ভিন্ন হয়: আলাদাভাবে উত্তর দেওয়া একটি আইটেম, কোনো প্রতিযোগিতায় মুখোমুখি হওয়া অন্য একটি সত্তা, ক্রমে সাজানোর জন্য দেওয়া বিকল্পের পুরো একটি সেট, অথবা এলোমেলোভাবে দেওয়া বৈশিষ্ট্য থেকে গড়া একটি প্রোফাইল। উদাহরণস্বরূপ:'),
 "3": line("পরিমাপ প্রোবের একটি সেটের প্রতি বারবার",
     "- খেলা, ম্যাচ এবং সরাসরি তুলনা, যেখানে দুটি সত্তা মুখোমুখি হয় এবং একটি জেতে\n- পছন্দক্রমের ব্যালট, বিচারকমণ্ডলীর ক্রমতালিকা এবং জরিপের ক্রম নির্ধারণের প্রশ্ন\n- কনজয়েন্ট পরীক্ষা, যেখানে উত্তরদাতারা এলোমেলোভাবে তৈরি প্রোফাইলের মধ্যে বেছে নেন\n- পরিমাপ প্রোবের একটি সেটের প্রতি বারবার রেসপন্স জড়িত অন্য যেকোনো পরিস্থিতি"),
 "4": line("**আন্তঃকার্যযোগ্য (Interoperable)।**",
     "- **আন্তঃকার্যযোগ্য (Interoperable)।** প্রতিটি ডেটাসেট লং ফরম্যাটে, প্রতি রেসপন্সে একটি সারি, তার পরিবারের স্ট্যান্ডার্ড অনুসারে (নিচে বর্ণিত) রূপান্তরিত, যাতে একই বিশ্লেষণ কোড সামান্য বা কোনো পরিবর্তন ছাড়াই বহু ডেটাসেটে চালানো যায়।"),
 "5": para("ডেটাসেটগুলো আকারে ব্যাপকভাবে ভিন্ন",
     "ডেটাসেটগুলো আকারে ব্যাপকভাবে ভিন্ন (কয়েকশো রেসপন্স থেকে লক্ষ লক্ষ পর্যন্ত) এবং রেসপন্সের ধরনেও ভিন্ন (হ্যাঁ/না আইটেম, বহু-শ্রেণির রেটিং, আংশিক-নম্বর স্কোর, ক্রমহীন শ্রেণি, প্রতিযোগিতার ফলাফল, ক্রমতালিকার অবস্থান, আরও অনেক কিছু)।"),
 "7": line("**R** ও **Python** উভয়ের জন্য উপলব্ধ `irw` প্যাকেজটি",
     "`irw` প্যাকেজটি **R** ও **Python** উভয়ের জন্যই উপলব্ধ।"),
 "10": para("প্যাকেজটি প্রথমবার ব্যবহারের সময়",
     "প্যাকেজটি প্রথমবার ব্যবহারের সময়, একটি বিনামূল্যের Redivis অ্যাকাউন্ট দিয়ে লগ ইন করতে বলা হবে।"),
 "6": section(
     "## ডেটা স্ট্যান্ডার্ড",
     "IRW-এর প্রতিটি টেবিল **লং ফরম্যাটে**: প্রতি রেসপন্সে একটি সারি। একটি সারি ঠিক *কী*, তা নির্ভর করে টেবিলটি কোন পরিবারের উপর, এবং এটি যে কলামগুলোতে ধরা হয় সেগুলোও।",
     "দুটি প্রশ্ন যা একটি ডেটাসেটকে IRW-এর একটি পরিবারে স্থাপন করে।",
     "পরিবার", "স্ট্যান্ডার্ড",
     "Simsyn চার্টে নেই: এই পরিবারটি ডেটার আকার দিয়ে নয়, উৎস দিয়ে নির্ধারিত, তাই একটি সিমুলেটেড টেবিল এই যেকোনো আকারের হতে পারে। প্রতিটি পরিবারের কলাম [স্ট্যান্ডার্ডের সংক্ষিপ্তসার](/overview.qmd)-এ দেওয়া আছে।"),
}

EDITS["ta"] = {
 "1": para("இது ஏற்கெனவே உள்ள நூற்றுக்கணக்கான",
     "Item Response Warehouse (IRW) அதே FAIR கொள்கைகளை உருப்படி பதில் தரவுக்குப் பயன்படுத்துகிறது. இது ஏற்கெனவே உள்ள ஆயிரக்கணக்கான தரவுத் தொகுப்புகளை ஒன்றிணைத்து, அவற்றை ஒரே பொதுவான வடிவமைப்பாக மாற்றுகிறது — இதனால் ஒரு தரவுத் தொகுப்பில் சோதிக்கப்பட்ட ஒரு முறையை, ஆயிரக்கணக்கான மற்ற தரவுத் தொகுப்புகளிலும் எளிதாக சோதிக்க முடியும்."),
 "2": para("IRW-இல் **நூற்றுக்கணக்கான தரவுத் தொகுப்புகள்**",
     'IRW-இல் **ஆயிரக்கணக்கான தரவுத் தொகுப்புகள்** ("அட்டவணைகள்") உள்ளன, ஒவ்வொன்றும் தனிப்பட்ட பதில்களின் தொகுப்பாகும். ஒரு நபர் (அல்லது வேறு ஒரு அலகு) ஒரு அளவீட்டுக் கருவியை எதிர்கொள்ளும்போதும், முடிவு பதிவு செய்யப்படும்போதும் ஒரு பதில் உருவாகிறது. எது அளவீட்டுக் கருவி என்பது [தரவுக் குடும்பத்தை](/overview.qmd) பொறுத்து மாறுபடுகிறது: தனியே பதிலளிக்கப்படும் ஒரு உருப்படி, ஒரு போட்டியில் எதிர்கொள்ளும் மற்றொரு அமைப்பு, வரிசைப்படுத்த வேண்டிய முழுமையான தேர்வுகள், அல்லது சீரற்ற முறையில் வழங்கப்பட்ட பண்புகளிலிருந்து உருவாக்கப்பட்ட ஒரு சுருக்குவிவரம். எடுத்துக்காட்டுகள்:'),
 "3": line("அளவீட்டு கருவிகளின் தொகுப்புக்கு மீண்டும் மீண்டும்",
     "- இரண்டு தரப்பினர் முட்டி ஒருவர் வெல்லும் போட்டிகள், ஆட்டங்கள் மற்றும் நேரடி ஒப்பீடுகள்\n- வரிசைப் வாக்குச் சீட்டுகள், நடுவர் குழு தரவரிசைகள் மற்றும் கருத்தாய்வு தரவரிசைக் கேள்விகள்\n- conjoint சோதனைகள், அவற்றில் பதிலளிப்பவர்கள் சீரற்ற முறையில் உருவாக்கப்பட்ட சுருக்குவிவரங்களுக்கிடையே தேர்கிறார்கள்\n- அளவீட்டு கருவிகளின் தொகுப்புக்கு மீண்டும் மீண்டும் பதிலளிக்கும் வேறு எந்த சூழலும்"),
 "4": line("**இயங்கக்கூடியது (Interoperable).**",
     "- **இயங்கக்கூடியது (Interoperable).** ஒவ்வொரு தரவுத் தொகுப்பும் நீள் வடிவிற்கு, ஒரு பதிலுக்கு ஒரு வரிசை என, அதன் குடும்பத்தின் தரநிலையின்படி (கீழே விவரிக்கப்பட்டுள்ளது) மறுவடிவமைக்கப்படுகிறது, இதனால் ஒரே பகுப்பாய்வுக் குறியீட்டை, சிறிதளவு அல்லது எந்த மாற்றமும் இல்லாமல், பல தரவுத் தொகுப்புகளில் இயக்க முடியும்."),
 "5": para("தரவுத் தொகுப்புகள் அளவில்",
     "தரவுத் தொகுப்புகள் அளவில் (சில நூறு பதில்களிலிருந்து பல மில்லியன் வரை) மற்றும் பதில் வகையில் (ஆம்/இல்லை உருப்படிகள், பல-வகை மதிப்பீடுகள், பகுதி மதிப்பெண்கள், வரிசையற்ற வகைகள், போட்டி முடிவுகள், தரவரிசை இடங்கள் மற்றும் பல) பரவலாக மாறுபடுகின்றன."),
 "7": line("**R** மற்றும் **Python** இரண்டிற்கும் கிடைக்கும் `irw` தொகுப்பு",
     "`irw` தொகுப்பு **R** மற்றும் **Python** இரண்டிற்கும் கிடைக்கிறது."),
 "10": para("நீங்கள் இந்தத் தொகுப்பை முதன்முறையாக",
     "நீங்கள் இந்தத் தொகுப்பை முதன்முறையாகப் பயன்படுத்தும்போது, இலவச Redivis கணக்கு மூலம் உள்நுழையுமாறு கேட்கப்படுவீர்கள்."),
 "6": section(
     "## தரவு தரநிலை",
     "ஒவ்வொரு IRW அட்டவணையும் **நீள் வடிவில்** உள்ளது: ஒரு பதிலுக்கு ஒரு வரிசை. ஒரு வரிசை *எதைக் குறிக்கிறது* என்பது அந்த அட்டவணை சேர்ந்த குடும்பத்தைப் பொருத்தது; அதைச் சுமக்கும் நிரல்களும் அதே போல்.",
     "ஒரு தரவுத் தொகுப்பை IRW குடும்பத்தில் சேர்க்கும் இரண்டு கேள்விகள்.",
     "குடும்பம்", "தரநிலை",
     "Simsyn வரைபடத்தில் இல்லை: இந்தக் குடும்பம் தரவின் வடிவால் அல்ல, அதன் மூலத்தால் வரையறுக்கப்படுகிறது, எனவே ஒரு உருவகம் அட்டவணை இவற்றில் எந்த வடிவில் வேண்டுமானாலும் இருக்கலாம். ஒவ்வொரு குடும்பத்தின் நிரல்கள் [தரநிலைகளின் கண்ணோட்டம்](/overview.qmd) பக்கத்தில் உள்ளன."),
}


def apply(lang):
    path = os.path.join(HERE, "welcome_%s.md" % lang)
    s = io.open(path, encoding="utf-8", newline="").read()
    spec = EDITS[lang]

    # change 6 replaces the whole data-standard section, found by heading
    body = spec["6"]
    heading = body.split("\n", 1)[0]
    starts = [m.start() for m in re.finditer(r"^## ", s, re.M)]
    try:
        i = next(p for p in starts if s.startswith(heading, p))
    except StopIteration:
        return "heading not found: %s" % heading
    j = next((p for p in starts if p > i), len(s))
    s = s[:i] + body + "\n\n" + s[j:]

    for key in sorted(k for k in spec if k != "6"):
        try:
            old, new = _resolve(s, spec[key])
        except LookupError as exc:
            return "change %s: %s" % (key, exc)
        n = s.count(old)
        if n != 1:
            return "change %s matched %d times" % (key, n)
        s = s.replace(old, new, 1)

    io.open(path, "w", encoding="utf-8", newline="").write(s)
    return None


def main():
    args = sys.argv[1:]
    langs = sorted(EDITS) if args == ["--all"] else args
    bad = 0
    for lang in langs:
        if lang not in EDITS:
            print("  %-7s no edits defined" % lang)
            bad += 1
            continue
        err = apply(lang)
        print("  %-7s %s" % (lang, err or "ok"))
        bad += bool(err)
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
