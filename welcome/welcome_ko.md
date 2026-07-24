---
lang: ko
pagetitle: "Item Response Warehouse (문항 반응 데이터 저장소)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher link line below (English · Français · ...) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate or reorder it.
-->

# Item Response Warehouse (문항 반응 데이터 저장소)

**심리측정학 및 측정 연구를 위한, 무료로 공개된 통합 문항 반응 데이터 모음입니다.**

[itemresponsewarehouse.org](https://itemresponsewarehouse.org) · [GitHub](https://github.com/itemresponsewarehouse) · [논문 읽기](https://doi.org/10.3758/s13428-025-02796-y)

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html)

---

## IRW가 존재하는 이유

교육학, 심리학 및 관련 분야에서 측정을 연구하는 연구자들은 자신의 방법을 검증하고 비교하기 위해 실제 데이터가 필요합니다. 그러한 데이터는 이미 방대한 양으로 존재합니다. 하지만 여러 연구에 걸쳐 흩어져 있고, 서로 다른 형식으로 저장되어 있으며, 문서화나 라이선스가 불분명하여 재사용하기 어려운 경우가 많습니다.

이는 잘 알려진 문제입니다. 다른 분야들은 공유되고 표준화된 데이터 자원을 구축하여 이 문제를 해결했습니다. 컴퓨터 과학 분야에서는 라벨이 지정된 이미지 모음인 ImageNet이 연구자들에게 공통의 기준을 제공하여 인공지능 분야의 빠른 발전을 이끌었습니다. 유전학과 신경과학 역시 각자의 데이터를 위한 유사한 공유 자원을 구축했습니다.

Item Response Warehouse(IRW)는 문항 반응 데이터에 대해 동일한 역할을 합니다. 수백 개의 기존 데이터셋을 한데 모아 하나의 공통 형식으로 재구성함으로써, 한 데이터셋에서 검증된 방법을 수백 개의 다른 데이터셋에서도 손쉽게 검증할 수 있게 합니다.

## IRW에 포함된 내용

IRW는 **수백 개의 데이터셋**("테이블")을 포함하며, 각 데이터셋은 개별 반응들의 모음입니다. 반응은 어떤 사람(또는 다른 단위)이 어떤 문항(또는 다른 측정 도구)에 응답할 때마다 생성됩니다. 예를 들면:

- 교육 및 능력 검사에서의 학생 응답
- 성격이나 태도를 측정하는 설문 문항
- 인간 평가자가 부여한 평정
- 측정 도구 집합에 대해 반복적으로 반응이 이루어지는 그 밖의 모든 상황

IRW의 모든 데이터셋에 대해 다음 두 가지가 성립합니다:

- **개방성.** 각 데이터셋은 재사용이 가능하도록 라이선스가 부여되어 있습니다. 출처가 문서화되어 있으며, 이를 IRW 형식으로 변환하는 데 사용된 코드도 공개되어 있습니다.
- **통합성.** 각 데이터셋은 동일한 단순 구조(아래에서 설명)로 재구성되어 있어, 동일한 분석 코드를 거의 또는 전혀 수정하지 않고 여러 데이터셋에 걸쳐 실행할 수 있습니다.

데이터셋은 규모(수백 건의 반응부터 수백만 건까지)와 반응 유형(예/아니오 문항, 다범주 평정, 부분 점수 등)에서 매우 다양합니다. 각 데이터셋에는 참가자 수, 문항 수, 반응 밀도, 주제 분야 및 기타 서술적 태그와 같은 사전 계산된 메타데이터가 함께 제공되어, 연구자들이 모든 데이터셋을 먼저 다운로드하고 처리하지 않고도 관련 데이터셋을 찾을 수 있습니다.

## 데이터 표준

모든 IRW 데이터셋은 **롱 포맷(long format)**으로 재구성됩니다: 반응 하나당 한 행입니다. 각 행은 최소한 다음 세 가지 정보를 담고 있습니다:

| 열 | 의미 |
|---|---|
| `id` | 누가(또는 무엇이) 반응을 생성했는가 — 일반적으로 한 사람 |
| `item` | 어떤 측정 도구가 반응을 생성했는가 — 일반적으로 하나의 질문이나 과제 |
| `resp` | 반응 자체, 서열 점수로 저장됨 |

**예시:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

데이터셋에 반응 시간, 평가자 신원, 나이와 같은 공변량 등 추가 정보가 포함된 경우, 해당 정보는 일관된 이름 규칙을 따르는 추가 열에 저장됩니다. 이 하나의 단순한 구조는 매우 다양한 측정 상황을 포괄하며, 이 덕분에 분석 코드를 한 번만 작성하여 저장소 전체에 적용할 수 있습니다.

이 표준에 대한 전체 기술 명세는 [itemresponsewarehouse.org/standard.html](/standard.qmd)에서 확인할 수 있습니다. 문항 텍스트, 쌍대 비교(경쟁) 데이터, 명목형(순서 없는 범주) 반응을 위한 보다 전문화된 표준도 별도로 존재합니다.

## 사용 방법

자동화를 얼마나 원하는지에 따라 IRW 데이터를 얻는 세 가지 방법이 있습니다.

**1. 웹 브라우저에서 탐색하기**
[IRW 데이터 브라우저](/data.qmd)에서 계정 없이도 데이터셋과 그 메타데이터를 바로 탐색할 수 있습니다. 전체 데이터셋을 다운로드하려면 무료 [Redivis](https://redivis.com) 계정이 필요합니다. 이는 Redivis가 실제 데이터를 호스팅하는 플랫폼이기 때문입니다.

**2. `irw` 패키지 사용하기 (권장)**
**R**과 **Python** 양쪽에서 모두 사용 가능한 `irw` 패키지는 데이터를 찾고, 필터링하고, 다운로드하는 간단한 함수들을 제공합니다.

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

패키지를 처음 사용할 때는 무료 Redivis 계정으로 로그인하라는 안내를 받게 됩니다. 이후에는 코드 한 줄이면 어떤 데이터셋이든 R이나 Python으로 직접 다운로드할 수 있습니다. 그러면 데이터는 표준 소프트웨어—예를 들어 문항 반응 이론이나 요인분석 패키지—로 바로 분석할 준비가 됩니다.

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
- 웹사이트: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- 코드: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

연구에 IRW 데이터를 사용하신다면, 원본 데이터를 인용해 주시기 바랍니다(이를 위한 기능을 제공하고 있습니다). 위의 소개 논문도 함께 인용해 주시면 매우 감사하겠습니다.

---

*질문이나 의견이 있으신가요? 데이터셋 기여를 원하시나요? [문의 페이지](/contact.qmd)를 방문하거나 [GitHub](https://github.com/itemresponsewarehouse)에 이슈를 등록해 주세요.*
