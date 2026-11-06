---
lang: ja
pagetitle: "Item Response Warehouse（IRW；項目反応データウェアハウス）"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse（IRW；項目反応データウェアハウス）

**心理測定学および測定研究のための、無料で公開された統一形式の項目反応データ集です。**

[論文を読む](https://doi.org/10.3758/s13428-025-02796-y) **(オープンアクセス)**

{{< include /welcome/_lang_switch.md >}}

---

## IRW が存在する理由

教育学、心理学、および関連分野で測定を研究する研究者は、自らの手法を検証し比較するために実データを必要としています。そのようなデータはすでに大量に存在します。しかし、それらは多くの研究に分散しており、さまざまな形式で保存されており、文書化やライセンスが不明確なために再利用が難しいことがよくあります。

これはよく知られた問題です。研究コミュニティはこれを解決するための共通の基準を打ち立てました。データは **FAIR** であるべきだという原則です——発見可能(Findable)、アクセス可能(Accessible)、相互運用可能(Interoperable)、再利用可能(Reusable)であること(Wilkinson et al., 2016)。他の分野では、これらの原則を実践に移し、共有された標準化データ資源を構築することでこの問題を解決してきました。コンピュータサイエンスの分野では、ラベル付き画像の集合である ImageNet が研究者に共通のベンチマークを提供し、人工知能の急速な発展を後押ししました。遺伝学や神経科学も、それぞれのデータのために同様の共有資源を構築しています。

Item Response Warehouse（IRW）は、項目反応データに同じ FAIR 原則を適用します。数千の既存データセットを集約し、それらを一つの共通形式に再構成することで、あるデータセットで検証された手法を、他の数千のデータセットでも簡単に検証できるようにします。

## IRW に含まれるもの

IRW には**数千のデータセット**(「テーブル」)が含まれており、それぞれが個々の反応の集合です。反応は、ある人(または他の単位)が測定プローブに出会い、その結果が記録されるたびに生成されます。何が測定プローブにあたるかは[データファミリー](/overview.qmd)によって異なります。単独で回答する項目、対決で出会う他の主体、順序をつける選択肢の集合全体、あるいは無作為に割り当てられた属性から構成されたプロファイルなどです。例えば:

- 教育・能力検査における学生の解答
- 性格や態度を測定する調査項目
- 人間の評価者による評定
- 試合や直接比較など、二つの主体が対決して一方が勝つ場面
- 優先順位投票の投票用紙、審査員による順位付け、調査の順位付け質問
- コンジョイント実験。回答者が無作為に構成されたプロファイルの間で選択します
- 一連の測定プローブに対して反復的に反応が行われる、その他あらゆる状況

IRW に含まれるすべてのデータセットは、次のように設計されています:

- **発見可能(Findable)。** 各データセットには事前計算済みのメタデータ——参加者数、項目数、反応密度、主題分野、その他の記述的タグ——が付属しており、事前にダウンロードすることなくデータセットを探し、絞り込むことができます。
- **アクセス可能(Accessible)。** 各データセットは、無料アカウントさえあれば、ウェブブラウザまたは `irw` パッケージを通じて取得できます。
- **相互運用可能(Interoperable)。** 各データセットは、反応1件につき1行のロング形式で、そのファミリーの標準(以下で説明)に従って再構成されており、同じ分析コードをほとんど、あるいはまったく変更せずに多くのデータセットで実行できます。
- **再利用可能(Reusable)。** 各データセットはオープンライセンスの下にあり、その出所は文書化されており、IRW 形式へ変換するために使用されたコードも公開されています。

データセットは規模(数百件の反応から数百万件まで)や反応の種類(はい/いいえ形式の項目、多カテゴリ評定、部分点方式のスコア、順序のないカテゴリ、対決の結果、順位など)において大きく異なります。

## データ標準

IRW のすべてのテーブルは**ロング形式**です。反応1件につき1行です。1行が*何を表すか*は、そのテーブルが属するファミリーによって異なり、それを担う列も同様です。

<img src="/figures/families/families-tree.svg" alt="データセットを IRW のファミリーに分類する二つの問い。"
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| ファミリー | `source =` | 標準 |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn は図に含まれていません。このファミリーはデータの形ではなく出所によって定義されるため、シミュレーションによるテーブルはこれらのどの形をとることもあります。各ファミリーの列については[標準の概要](/overview.qmd)をご覧ください。


## 使い方

自動化をどの程度望むかに応じて、IRW データを取得する方法は3つあります。

**1. Webブラウザで閲覧する**
[IRW データブラウザ](/data.qmd) で、アカウント不要でデータセットとそのメタデータを直接探索できます。完全なデータセットをダウンロードするには、無料の [Redivis](https://redivis.com) アカウントが必要です。これは、Redivis が基盤となるデータをホストしているプラットフォームだからです。

**2. `irw` パッケージを使う(推奨)**
`irw` パッケージは **R** と **Python** の両方で利用できます。

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

パッケージを初めて使用する際には、無料の Redivis アカウントでログインするよう求められます。

**3. Redivis のクライアントライブラリを直接使う**
より低レベルなワークフローや R/Python 以外の環境では、Redivis 自身が提供する R および Python クライアントライブラリを通じてもデータにアクセスできます。詳細は [はじめにガイド](/getstarted.qmd) を参照してください。

### データのダウンロード以外にも

IRW プロジェクトには次のようなものも含まれています:

- 拡大を続ける **[ビネット(vignettes)](/vignettes/index.qmd)** 集 — 古典的および新しい測定手法を、多数の IRW データセットに同時に適用する実践例
- 実データを用いて心理測定学を教えるための**トレーニング資料と演習問題**
- 自身のデータセットをウェアハウスに追加したい研究者のための**貢献プロセス**

## さらに詳しく

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- ウェブサイト: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- コード: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

研究で IRW のデータを使用される場合は、元データを引用してください(そのための機能を用意しています)。上記の紹介論文も併せて引用いただければ幸いです。

---

*ご質問やご意見がありますか?データセットの提供をご希望ですか? [お問い合わせページ](/contact.qmd) をご覧いただくか、[GitHub](https://github.com/itemresponsewarehouse) で issue を作成してください。*
