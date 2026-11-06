---
lang: ko
pagetitle: "Item Response Warehouse (IRW; 문항 반응 데이터 저장소)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; 문항 반응 데이터 저장소)

**심리측정학 및 측정 연구를 위한, 무료로 공개된 통합 문항 반응 데이터 모음입니다.**

[논문 읽기](https://doi.org/10.3758/s13428-025-02796-y) **(오픈 액세스)**

{{< include /welcome/_lang_switch.md >}}

---

## IRW가 존재하는 이유

교육학, 심리학 및 관련 분야에서 측정을 연구하는 연구자들은 자신의 방법을 검증하고 비교하기 위해 실제 데이터가 필요합니다. 그러한 데이터는 이미 방대한 양으로 존재합니다. 하지만 여러 연구에 걸쳐 흩어져 있고, 서로 다른 형식으로 저장되어 있으며, 문서화나 라이선스가 불분명하여 재사용하기 어려운 경우가 많습니다.

이는 잘 알려진 문제입니다. 연구 공동체는 이를 해결하기 위한 공통 기준을 마련했습니다. 데이터는 **FAIR**해야 한다는 것, 즉 찾을 수 있고(Findable), 접근할 수 있고(Accessible), 상호운용 가능하며(Interoperable), 재사용할 수 있어야(Reusable) 한다는 원칙입니다(Wilkinson et al., 2016). 다른 분야들은 이러한 원칙을 실제로 적용하여 공유되고 표준화된 데이터 자원을 구축함으로써 이 문제를 해결했습니다. 컴퓨터 과학 분야에서는 라벨이 지정된 이미지 모음인 ImageNet이 연구자들에게 공통의 기준을 제공하여 인공지능 분야의 빠른 발전을 이끌었습니다. 유전학과 신경과학 역시 각자의 데이터를 위한 유사한 공유 자원을 구축했습니다.

Item Response Warehouse(IRW)는 문항 반응 데이터에 동일한 FAIR 원칙을 적용합니다. 수천 개의 기존 데이터셋을 한데 모아 하나의 공통 형식으로 재구성함으로써, 한 데이터셋에서 검증된 방법을 수천 개의 다른 데이터셋에서도 손쉽게 검증할 수 있게 합니다.

## IRW에 포함된 내용

IRW는 **수천 개의 데이터셋**("테이블")을 포함하며, 각 데이터셋은 개별 반응들의 모음입니다. 반응은 어떤 사람(또는 다른 단위)이 측정 도구와 마주하고 그 결과가 기록될 때마다 생성됩니다. 무엇이 측정 도구에 해당하는지는 [데이터 계열](/overview.qmd)에 따라 다릅니다. 단독으로 응답하는 문항, 대결에서 만나는 다른 개체, 순서를 매기는 선택지 전체, 또는 무작위로 할당된 속성으로 구성된 프로필 등입니다. 예를 들면:

- 교육 및 능력 검사에서의 학생 응답
- 성격이나 태도를 측정하는 설문 문항
- 인간 평가자가 부여한 평정
- 경기와 직접 비교처럼 두 개체가 맞붙어 한쪽이 이기는 상황
- 선호 투표 용지, 심사위원단 순위, 설문의 순위 매기기 문항
- 컬조인트 실험. 응답자가 무작위로 구성된 프로필 중에서 선택합니다
- 측정 도구 집합에 대해 반복적으로 반응이 이루어지는 그 밖의 모든 상황

IRW의 모든 데이터셋은 다음과 같이 설계되어 있습니다:

- **찾을 수 있음(Findable).** 각 데이터셋에는 참가자 수, 문항 수, 반응 밀도, 주제 분야 및 기타 서술적 태그와 같은 사전 계산된 메타데이터가 포함되어 있어, 먼저 다운로드하지 않고도 데이터셋을 찾고 필터링할 수 있습니다.
- **접근 가능함(Accessible).** 각 데이터셋은 무료 계정만 있으면 웹 브라우저 또는 `irw` 패키지를 통해 받을 수 있습니다.
- **상호운용 가능함(Interoperable).** 각 데이터셋은 반응 하나당 한 행인 롱 형식으로, 해당 계열의 표준(아래에서 설명)에 따라 재구성되어 있어, 동일한 분석 코드를 거의 또는 전혀 수정하지 않고 여러 데이터셋에 걸쳐 실행할 수 있습니다.
- **재사용 가능함(Reusable).** 각 데이터셋은 개방형 라이선스가 부여되어 있고, 출처가 문서화되어 있으며, 이를 IRW 형식으로 변환하는 데 사용된 코드도 공개되어 있습니다.

데이터셋은 규모(수백 건의 반응부터 수백만 건까지)와 반응 유형(예/아니오 문항, 다범주 평정, 부분 점수, 순서 없는 범주, 대결 결과, 순위 등)에서 매우 다양합니다.

## 데이터 표준

모든 IRW 테이블은 **롱 형식**입니다. 반응 하나당 한 행입니다. 한 행이 *무엇인지*는 그 테이블이 속한 계열에 따라 다르며, 그것을 담는 열도 마찬가지입니다.

<img src="/figures/families/families-tree.svg" alt="데이터셋을 IRW 계열로 분류하는 두 가지 질문."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| 계열 | `source =` | 표준 |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn은 도표에 없습니다. 이 계열은 데이터의 형태가 아니라 출처로 정의되므로, 시뮬레이션 테이블은 이 형태들 중 어느 것이든 가질 수 있습니다. 각 계열의 열은 [표준 개요](/overview.qmd)에 정리되어 있습니다.


## 사용 방법

자동화를 얼마나 원하는지에 따라 IRW 데이터를 얻는 세 가지 방법이 있습니다.

**1. 웹 브라우저에서 탐색하기**
[IRW 데이터 브라우저](/data.qmd)에서 계정 없이도 데이터셋과 그 메타데이터를 바로 탐색할 수 있습니다. 전체 데이터셋을 다운로드하려면 무료 [Redivis](https://redivis.com) 계정이 필요합니다. 이는 Redivis가 실제 데이터를 호스팅하는 플랫폼이기 때문입니다.

**2. `irw` 패키지 사용하기 (권장)**
`irw` 패키지는 **R**과 **Python** 양쪽에서 모두 사용할 수 있습니다.

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

패키지를 처음 사용할 때는 무료 Redivis 계정으로 로그인하라는 안내를 받게 됩니다.

**3. Redivis 클라이언트 라이브러리 직접 사용하기**
더 낮은 수준의 작업 흐름이나 R/Python을 사용하지 않는 경우, Redivis 자체의 R 및 Python 클라이언트 라이브러리를 통해서도 데이터에 접근할 수 있습니다. 자세한 내용은 [시작하기 가이드](/getstarted.qmd)를 참고하세요.

### 데이터 다운로드를 넘어서

IRW 프로젝트에는 다음도 포함됩니다:

- 점점 늘어나는 **[비네트(vignettes)](/vignettes/index.qmd)** 모음 — 여러 IRW 데이터셋에 걸쳐 고전적 및 새로운 측정 방법을 동시에 적용해 보는 실습 예제
- 실제 데이터를 활용해 심리측정학을 가르치기 위한 **교육 자료 및 연습 문제**
- 자신의 데이터셋을 저장소에 추가하고자 하는 연구자를 위한 **기여 절차**

## 더 알아보기

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- 웹사이트: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- 코드: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

연구에 IRW 데이터를 사용하신다면, 원본 데이터를 인용해 주시기 바랍니다(이를 위한 기능을 제공하고 있습니다). 위의 소개 논문도 함께 인용해 주시면 매우 감사하겠습니다.

---

*질문이나 의견이 있으신가요? 데이터셋 기여를 원하시나요? [문의 페이지](/contact.qmd)를 방문하거나 [GitHub](https://github.com/itemresponsewarehouse)에 이슈를 등록해 주세요.*
