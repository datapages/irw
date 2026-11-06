---
lang: pt
pagetitle: "O Item Response Warehouse (IRW; Armazém de respostas a itens)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# O Item Response Warehouse (IRW; Armazém de respostas a itens)

**Uma coleção livre e aberta de dados de resposta a itens harmonizados, para pesquisa em psicometria e medição.**

[Ler o artigo](https://doi.org/10.3758/s13428-025-02796-y) **(acesso aberto)**

{{< include /welcome/_lang_switch.md >}}

---

## Por que o IRW existe

Pesquisadores que estudam medição — em educação, psicologia e áreas afins — precisam de dados reais para testar e comparar seus métodos. Esses dados já existem em grande quantidade. Mas estão dispersos em muitos estudos, armazenados em formatos muito diversos, e frequentemente difíceis de reutilizar devido a documentação ou licenciamento pouco claros.

Este é um problema bem conhecido. A comunidade de pesquisa articulou um padrão compartilhado para resolvê-lo: os dados devem ser **FAIR** — Localizáveis (Findable), Acessíveis, Interoperáveis e Reutilizáveis (Wilkinson et al., 2016). Outras áreas colocaram esses princípios em prática construindo recursos de dados compartilhados e padronizados. Na ciência da computação, a coleção de imagens rotuladas ImageNet deu aos pesquisadores um referencial comum e ajudou a acelerar o progresso em inteligência artificial. A genética e a neurociência construíram recursos compartilhados semelhantes para seus próprios dados.

O Item Response Warehouse (IRW) aplica os mesmos princípios FAIR aos dados de resposta a itens. Ele reúne milhares de conjuntos de dados existentes e os reformata em um formato comum único — de modo que um método testado em um conjunto de dados possa ser facilmente testado em milhares de outros.

## O que há no IRW

O IRW contém **milhares de conjuntos de dados** ("tabelas"), cada um sendo uma coleção de respostas individuais. Uma resposta é gerada sempre que uma pessoa (ou outra unidade) se depara com uma sonda e o resultado é registrado. O que conta como sonda varia conforme a [família de dados](/overview.qmd): um item respondido isoladamente, outra entidade enfrentada em uma disputa, um conjunto inteiro de opções colocado em ordem, ou um perfil montado a partir de atributos atribuídos ao acaso. Exemplos incluem:

- Respostas de estudantes em testes de educação e aptidão
- Itens de pesquisa que medem personalidade ou atitudes
- Avaliações atribuídas por avaliadores humanos
- Partidas, jogos e comparações diretas, em que duas entidades se enfrentam e uma vence
- Cédulas de voto preferencial, classificações de júris e questões de ordenação em pesquisas
- Experimentos conjoint, em que os respondentes escolhem entre perfis construídos ao acaso
- Qualquer outro contexto que envolva respostas repetidas a um conjunto de sondas de medição

Todo conjunto de dados no IRW é projetado para ser:

- **Localizável (Findable).** Cada conjunto de dados vem com metadados pré-calculados — número de participantes, número de itens, densidade de respostas, área temática e outras etiquetas descritivas — para que os conjuntos de dados possam ser localizados e filtrados sem a necessidade de baixá-los primeiro.
- **Acessível (Accessible).** Cada conjunto de dados pode ser obtido pelo navegador web ou pelo pacote `irw`, usando uma conta gratuita.
- **Interoperável (Interoperable).** Cada conjunto de dados é reformatado em formato longo, uma linha por resposta, seguindo o padrão de sua família (descrito abaixo), de modo que o mesmo código de análise possa ser executado em muitos conjuntos de dados com pouca ou nenhuma modificação.
- **Reutilizável (Reusable).** Cada conjunto de dados tem licença aberta, sua origem é documentada, e o código usado para convertê-lo ao formato IRW é público.

Os conjuntos de dados variam amplamente em tamanho (de algumas centenas de respostas a vários milhões) e em tipo de resposta (itens sim/não, avaliações de múltiplas categorias, pontuações de crédito parcial, categorias sem ordem, resultados de disputas, posições em rankings, entre outros).

## O padrão de dados

Cada tabela do IRW está em **formato longo**: uma linha por resposta. O que uma linha *é* depende da família à qual a tabela pertence, e o mesmo vale para as colunas que a compõem.

<img src="/figures/families/families-tree.svg" alt="Duas perguntas que classificam um conjunto de dados em uma família do IRW."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| Família | `source =` | Padrão |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn não aparece no diagrama: essa família é definida pela origem dos dados, não por sua forma, de modo que uma tabela simulada pode ter qualquer uma destas formas. A [visão geral dos padrões](/overview.qmd) traz as colunas de cada família.


## Como usar

Existem três formas de obter dados do IRW, dependendo de quanto você deseja automatizar.

**1. Navegar no navegador web**
Explore os conjuntos de dados e seus metadados diretamente no [navegador de dados do IRW](/data.qmd) — sem necessidade de conta. Baixar um conjunto de dados completo requer uma conta gratuita no [Redivis](https://redivis.com), já que essa é a plataforma que hospeda os dados subjacentes.

**2. Usar o pacote `irw` (recomendado)**
O pacote `irw` está disponível tanto para **R** quanto para **Python**.

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

Na primeira vez que você usar o pacote, será solicitado que faça login com uma conta gratuita do Redivis.

**3. Usar diretamente as bibliotecas cliente do Redivis**
Para fluxos de trabalho de nível mais baixo ou fora de R/Python, os dados também podem ser acessados por meio das próprias bibliotecas cliente R e Python do Redivis. Veja o [guia de introdução](/getstarted.qmd) para mais detalhes.

### Além do download de dados

O projeto IRW também inclui:

- Um conjunto crescente de **[vinhetas](/vignettes/index.qmd)** — exemplos práticos que aplicam métodos de medição clássicos e novos a muitos conjuntos de dados do IRW ao mesmo tempo
- **Recursos de treinamento e listas de exercícios** para ensinar psicometria com dados reais
- **Um processo de contribuição** para pesquisadores que desejam adicionar seus próprios conjuntos de dados ao repositório

## Saiba mais

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Site: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Código: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Se você usar dados do IRW em seu trabalho, cite os dados originais (fornecemos uma funcionalidade para isso). Também seria ótimo se você citasse o artigo introdutório acima.

---

*Perguntas, sugestões, ou quer contribuir com um conjunto de dados? Visite a [página de Contato](/contact.qmd) ou abra uma "issue" no [GitHub](https://github.com/itemresponsewarehouse).*
