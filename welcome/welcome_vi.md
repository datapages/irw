---
lang: vi
pagetitle: "Item Response Warehouse (IRW; Kho Dữ liệu Phản hồi Câu hỏi)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher link line below (English · Français · ...) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate or reorder it.
-->

# Item Response Warehouse (IRW; Kho Dữ liệu Phản hồi Câu hỏi)

**Một bộ sưu tập miễn phí, mở, gồm dữ liệu phản hồi câu hỏi (item response) đã được chuẩn hóa, phục vụ nghiên cứu đo lường tâm lý học (psychometrics) và đo lường nói chung.**

[itemresponsewarehouse.org](https://itemresponsewarehouse.org) · [GitHub](https://github.com/itemresponsewarehouse) · [Đọc bài báo](https://doi.org/10.3758/s13428-025-02796-y)

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html) · [繁體中文](/welcome/welcome_zh-hant.html) · [বাংলা](/welcome/welcome_bn.html) · [Türkçe](/welcome/welcome_tr.html) · [Tiếng Việt](/welcome/welcome_vi.html)

---

## Vì sao IRW ra đời

Các nhà nghiên cứu về đo lường — trong giáo dục, tâm lý học và các lĩnh vực liên quan — cần dữ liệu thực tế để kiểm nghiệm và so sánh phương pháp của mình. Loại dữ liệu đó đã tồn tại với số lượng lớn. Nhưng nó lại nằm rải rác trong nhiều nghiên cứu, được lưu trữ theo nhiều định dạng khác nhau, và thường khó tái sử dụng vì tài liệu mô tả không rõ ràng hoặc vấn đề bản quyền/giấy phép không minh bạch.

Đây là một vấn đề đã được biết đến rộng rãi. Các lĩnh vực khác đã giải quyết vấn đề này bằng cách xây dựng các nguồn dữ liệu dùng chung, được chuẩn hóa. Trong khoa học máy tính, bộ sưu tập hình ảnh có gán nhãn ImageNet đã mang lại cho các nhà nghiên cứu một chuẩn so sánh (benchmark) chung và góp phần thúc đẩy sự tiến bộ nhanh chóng của trí tuệ nhân tạo. Di truyền học và khoa học thần kinh cũng đã xây dựng những nguồn tài nguyên dùng chung tương tự cho dữ liệu của riêng mình.

Item Response Warehouse (IRW) làm điều tương tự cho dữ liệu phản hồi câu hỏi. Nó tập hợp hàng trăm bộ dữ liệu đã có và định dạng lại chúng thành một định dạng chung duy nhất — nhờ đó một phương pháp đã được kiểm nghiệm trên một bộ dữ liệu có thể dễ dàng được kiểm nghiệm trên hàng trăm bộ dữ liệu khác.

## IRW chứa những gì

IRW chứa **hàng trăm bộ dữ liệu** (gọi là "bảng"), mỗi bộ là một tập hợp các phản hồi riêng lẻ. Một phản hồi được tạo ra bất cứ khi nào một người (hoặc đơn vị khác) trả lời một câu hỏi (hoặc một dạng đầu dò đo lường khác). Ví dụ bao gồm:

- Câu trả lời của học sinh trong các bài kiểm tra giáo dục và năng lực
- Các mục khảo sát đo tính cách hoặc thái độ
- Điểm số do người chấm đưa ra
- Bất kỳ tình huống nào khác liên quan đến các phản hồi lặp lại đối với một tập hợp đầu dò đo lường

Hai điều sau đây luôn đúng với mọi bộ dữ liệu trong IRW:

- **Mở.** Mỗi bộ dữ liệu đều được cấp phép để tái sử dụng. Nguồn gốc của nó được ghi chép đầy đủ, và mã nguồn dùng để chuyển đổi nó sang định dạng IRW được công khai.
- **Đã chuẩn hóa (Harmonized).** Mỗi bộ dữ liệu được định dạng lại theo cùng một cấu trúc đơn giản (mô tả bên dưới), để cùng một đoạn mã phân tích có thể chạy trên nhiều bộ dữ liệu mà chỉ cần chỉnh sửa rất ít hoặc không cần chỉnh sửa.

Các bộ dữ liệu có sự khác biệt lớn về quy mô (từ vài trăm phản hồi đến hàng triệu) và về loại phản hồi (mục có/không, đánh giá nhiều hạng mục, điểm số theo phần, và nhiều hơn nữa). Mỗi bộ dữ liệu cũng đi kèm với siêu dữ liệu (metadata) đã được tính toán sẵn — số lượng người tham gia, số lượng câu hỏi, mật độ phản hồi, lĩnh vực chủ đề và các thẻ mô tả khác — để các nhà nghiên cứu có thể tìm bộ dữ liệu phù hợp mà không cần tải xuống và xử lý tất cả trước.

## Chuẩn dữ liệu

Mọi bộ dữ liệu IRW đều được định dạng lại thành **định dạng dài (long format)**: mỗi dòng ứng với một phản hồi. Mỗi dòng tối thiểu chứa ba thông tin:

| Cột | Ý nghĩa |
|---|---|
| `id` | Ai (hoặc cái gì) đã tạo ra phản hồi — thường là một người |
| `item` | Đầu dò đo lường nào đã tạo ra phản hồi — thường là một câu hỏi hoặc nhiệm vụ |
| `resp` | Bản thân phản hồi, được lưu trữ dưới dạng điểm số thứ bậc (ordinal) |

**Ví dụ:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Khi một bộ dữ liệu bao gồm thông tin bổ sung — thời gian phản hồi, danh tính người chấm, các biến hiệp phương sai (covariate) như tuổi tác — thông tin đó được lưu trong các cột bổ sung, được đặt tên nhất quán. Cấu trúc đơn giản duy nhất này bao quát một phạm vi rất rộng các tình huống đo lường, và đó chính là điều giúp cho việc viết mã phân tích một lần rồi áp dụng cho toàn bộ kho dữ liệu trở nên khả thi.

Đặc tả kỹ thuật đầy đủ của chuẩn này có tại [itemresponsewarehouse.org/standard.html](/standard.qmd). Ngoài ra còn có các chuẩn chuyên biệt hơn, liên quan, dành cho văn bản câu hỏi, dữ liệu thi đấu theo cặp (pairwise), và phản hồi danh nghĩa (nominal, không có thứ tự hạng mục).

## Cách sử dụng

Có ba cách để lấy dữ liệu IRW, tùy thuộc vào mức độ tự động hóa mà bạn mong muốn.

**1. Duyệt trên trình duyệt web**
Khám phá các bộ dữ liệu và siêu dữ liệu của chúng trực tiếp trên [trình duyệt dữ liệu IRW](/data.qmd) — không cần tài khoản. Việc tải xuống toàn bộ một bộ dữ liệu yêu cầu tài khoản [Redivis](https://redivis.com) miễn phí, vì đó là nền tảng lưu trữ dữ liệu gốc.

**2. Sử dụng gói `irw` (khuyến nghị)**
Gói `irw`, có sẵn cho cả **R** và **Python**, cung cấp các hàm đơn giản để tìm kiếm, lọc và tải xuống dữ liệu.

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

Lần đầu tiên sử dụng gói này, bạn sẽ được yêu cầu đăng nhập bằng tài khoản Redivis miễn phí. Sau đó, chỉ cần một dòng mã là có thể tải bất kỳ bộ dữ liệu nào trực tiếp vào R hoặc Python. Từ đó, dữ liệu đã sẵn sàng để phân tích bằng phần mềm tiêu chuẩn — ví dụ như các gói lý thuyết phản hồi câu hỏi (item response theory) hoặc phân tích nhân tố.

**3. Sử dụng trực tiếp các thư viện client của Redivis**
Đối với các quy trình làm việc cấp thấp hơn hoặc không dùng R/Python, dữ liệu cũng có thể được truy cập thông qua các thư viện client R và Python riêng của Redivis. Xem [Hướng dẫn Bắt đầu](/getstarted.qmd) để biết chi tiết.

### Không chỉ dừng lại ở việc tải dữ liệu

Dự án IRW còn bao gồm:
- Một bộ **[bài minh họa (vignettes)](/vignettes/index.qmd)** đang ngày càng mở rộng — các ví dụ thực tế áp dụng các phương pháp đo lường cổ điển và mới trên nhiều bộ dữ liệu IRW cùng lúc
- **Tài nguyên đào tạo và bộ bài tập** để giảng dạy đo lường tâm lý học bằng dữ liệu thực tế
- Một **quy trình đóng góp** dành cho các nhà nghiên cứu muốn thêm bộ dữ liệu của riêng mình vào kho dữ liệu

## Tìm hiểu thêm

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Trang web: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Mã nguồn: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Nếu bạn sử dụng dữ liệu IRW trong công việc của mình, vui lòng trích dẫn dữ liệu gốc (chúng tôi đã cung cấp chức năng để làm việc này). Chúng tôi cũng rất trân trọng nếu bạn trích dẫn bài báo giới thiệu nêu trên.

---

*Có câu hỏi, phản hồi, hay muốn đóng góp một bộ dữ liệu? Hãy ghé thăm [trang Liên hệ](/contact.qmd) hoặc mở một issue trên [GitHub](https://github.com/itemresponsewarehouse).*
