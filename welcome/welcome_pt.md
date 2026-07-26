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
5. The language-switcher link line below (English · Français · ...) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate or reorder it.
-->

# O Item Response Warehouse (IRW; Armazém de respostas a itens)

**Uma coleção livre e aberta de dados de resposta a itens harmonizados, para pesquisa em psicometria e medição.**

[itemresponsewarehouse.org](https://itemresponsewarehouse.org) · [GitHub](https://github.com/itemresponsewarehouse) · [Ler o artigo](https://doi.org/10.3758/s13428-025-02796-y)

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html) · [繁體中文](/welcome/welcome_zh-hant.html) · [বাংলা](/welcome/welcome_bn.html) · [Türkçe](/welcome/welcome_tr.html) · [Tiếng Việt](/welcome/welcome_vi.html)

---

## Por que o IRW existe

Pesquisadores que estudam medição — em educação, psicologia e áreas afins — precisam de dados reais para testar e comparar seus métodos. Esses dados já existem em grande quantidade. Mas estão dispersos em muitos estudos, armazenados em formatos muito diversos, e frequentemente difíceis de reutilizar devido a documentação ou licenciamento pouco claros.

Este é um problema bem conhecido. Outras áreas o resolveram construindo recursos de dados compartilhados e padronizados. Na ciência da computação, a coleção de imagens rotuladas ImageNet deu aos pesquisadores um referencial comum e ajudou a acelerar o progresso em inteligência artificial. A genética e a neurociência construíram recursos compartilhados semelhantes para seus próprios dados.

O Item Response Warehouse (IRW) faz o mesmo para dados de resposta a itens. Ele reúne centenas de conjuntos de dados existentes e os reformata em um formato comum único — de modo que um método testado em um conjunto de dados possa ser facilmente testado em centenas de outros.

## O que há no IRW

O IRW contém **centenas de conjuntos de dados** ("tabelas"), cada um sendo uma coleção de respostas individuais. Uma resposta é gerada sempre que uma pessoa (ou outra unidade) responde a um item (ou outra sonda). Exemplos incluem:

- Respostas de estudantes em testes de educação e aptidão
- Itens de pesquisa que medem personalidade ou atitudes
- Avaliações atribuídas por avaliadores humanos
- Qualquer outro contexto que envolva respostas repetidas a um conjunto de sondas de medição

Duas coisas são verdadeiras para todo conjunto de dados no IRW:

- **Aberto.** Cada conjunto de dados é licenciado para reutilização. Sua origem é documentada, e o código usado para convertê-lo ao formato IRW é público.
- **Harmonizado.** Cada conjunto de dados é reformatado segundo a mesma estrutura simples (descrita abaixo), de modo que o mesmo código de análise possa ser executado em muitos conjuntos de dados com pouca ou nenhuma modificação.

Os conjuntos de dados variam amplamente em tamanho (de algumas centenas de respostas a vários milhões) e em tipo de resposta (itens sim/não, avaliações de múltiplas categorias, pontuações de crédito parcial, entre outros). Cada conjunto de dados também vem com metadados pré-calculados — número de participantes, número de itens, densidade de respostas, área temática e outras etiquetas descritivas — para que os pesquisadores possam encontrar conjuntos de dados relevantes sem precisar baixá-los e processá-los todos primeiro.

## O padrão de dados

Todo conjunto de dados do IRW é reformatado em **formato longo**: uma linha por resposta. No mínimo, cada linha contém três informações:

| Coluna | Significado |
|---|---|
| `id` | Quem (ou o quê) produziu a resposta — tipicamente uma pessoa |
| `item` | Qual sonda de medição produziu a resposta — tipicamente uma pergunta ou tarefa |
| `resp` | A própria resposta, armazenada como uma pontuação ordinal |

**Exemplo:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Quando um conjunto de dados inclui informações adicionais — tempo de resposta, identidade do avaliador, covariáveis como idade — essas informações são armazenadas em colunas adicionais, nomeadas de forma consistente. Essa única estrutura simples cobre uma ampla variedade de situações de medição, o que torna possível escrever o código de análise uma única vez e aplicá-lo a todo o repositório.

A especificação técnica completa do padrão está disponível em [itemresponsewarehouse.org/standard.html](/standard.qmd). Também existem padrões mais especializados para texto de itens, dados de competição par a par, e respostas nominais (categorias não ordenadas).

## Como usar

Existem três formas de obter dados do IRW, dependendo de quanto você deseja automatizar.

**1. Navegar no navegador web**
Explore os conjuntos de dados e seus metadados diretamente no [navegador de dados do IRW](/data.qmd) — sem necessidade de conta. Baixar um conjunto de dados completo requer uma conta gratuita no [Redivis](https://redivis.com), já que essa é a plataforma que hospeda os dados subjacentes.

**2. Usar o pacote `irw` (recomendado)**
O pacote `irw`, disponível tanto para **R** quanto para **Python**, oferece funções simples para encontrar, filtrar e baixar dados.

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

Na primeira vez que você usar o pacote, será solicitado que faça login com uma conta gratuita do Redivis. Depois disso, uma única linha de código baixa qualquer conjunto de dados diretamente no R ou Python. A partir daí, os dados estão prontos para análise com software padrão — por exemplo, pacotes de teoria de resposta ao item ou análise fatorial.

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
- Site: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Código: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Se você usar dados do IRW em seu trabalho, cite os dados originais (fornecemos uma funcionalidade para isso). Também seria ótimo se você citasse o artigo introdutório acima.

---

*Perguntas, sugestões, ou quer contribuir com um conjunto de dados? Visite a [página de Contato](/contact.qmd) ou abra uma "issue" no [GitHub](https://github.com/itemresponsewarehouse).*
