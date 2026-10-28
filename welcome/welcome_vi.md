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
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Kho Dữ liệu Phản hồi Câu hỏi)

**Một bộ sưu tập miễn phí, mở, gồm dữ liệu phản hồi câu hỏi (item response) đã được chuẩn hóa, phục vụ nghiên cứu đo lường tâm lý học (psychometrics) và đo lường nói chung.**

[Đọc bài báo](https://doi.org/10.3758/s13428-025-02796-y) **(truy cập mở)**

{{< include /welcome/_lang_switch.md >}}

---

## Vì sao IRW ra đời

Các nhà nghiên cứu về đo lường — trong giáo dục, tâm lý học và các lĩnh vực liên quan — cần dữ liệu thực tế để kiểm nghiệm và so sánh phương pháp của mình. Loại dữ liệu đó đã tồn tại với số lượng lớn. Nhưng nó lại nằm rải rác trong nhiều nghiên cứu, được lưu trữ theo nhiều định dạng khác nhau, và thường khó tái sử dụng vì tài liệu mô tả không rõ ràng hoặc vấn đề bản quyền/giấy phép không minh bạch.

Đây là một vấn đề đã được biết đến rộng rãi. Cộng đồng nghiên cứu đã đề ra một chuẩn mực chung để giải quyết vấn đề này: dữ liệu cần phải **FAIR** — có thể tìm thấy (Findable), có thể truy cập (Accessible), có thể tương tác (Interoperable), và có thể tái sử dụng (Reusable) (Wilkinson et al., 2016). Các lĩnh vực khác đã áp dụng các nguyên tắc này vào thực tế bằng cách xây dựng các nguồn dữ liệu dùng chung, được chuẩn hóa. Trong khoa học máy tính, bộ sưu tập hình ảnh có gán nhãn ImageNet đã mang lại cho các nhà nghiên cứu một chuẩn so sánh (benchmark) chung và góp phần thúc đẩy sự tiến bộ nhanh chóng của trí tuệ nhân tạo. Di truyền học và khoa học thần kinh cũng đã xây dựng những nguồn tài nguyên dùng chung tương tự cho dữ liệu của riêng mình.

Item Response Warehouse (IRW) áp dụng cùng các nguyên tắc FAIR đó cho dữ liệu phản hồi câu hỏi. Nó tập hợp hàng nghìn bộ dữ liệu đã có và định dạng lại chúng thành một định dạng chung duy nhất — nhờ đó một phương pháp đã được kiểm nghiệm trên một bộ dữ liệu có thể dễ dàng được kiểm nghiệm trên hàng nghìn bộ dữ liệu khác.

## IRW chứa những gì

IRW chứa **hàng nghìn bộ dữ liệu** (gọi là "bảng"), mỗi bộ là một tập hợp các phản hồi riêng lẻ. Một phản hồi được tạo ra mỗi khi một người (hoặc đơn vị khác) gặp một đầu dò đo lường và kết quả được ghi lại. Điều gì được coi là đầu dò tùy thuộc vào [họ dữ liệu](/overview.qmd): một câu hỏi được trả lời riêng lẻ, một thực thể khác gặp trong một cuộc đọi đầu, toàn bộ một tập lựa chọn cần được xếp thứ tự, hoặc một hồ sơ được ghép từ các thuộc tính gán ngẫu nhiên. Ví dụ bao gồm:

- Câu trả lời của học sinh trong các bài kiểm tra giáo dục và năng lực
- Các mục khảo sát đo tính cách hoặc thái độ
- Điểm số do người chấm đưa ra
- Các trận đấu, ván đấu và so sánh trực tiếp, nơi hai thực thể đối đầu và một bên thắng
- Lá phiếu bầu ưu tiên, bảng xếp hạng của ban giám khảo và câu hỏi xếp hạng trong khảo sát
- Thí nghiệm conjoint, nơi người trả lời chọn giữa các hồ sơ được tạo ngẫu nhiên
- Bất kỳ tình huống nào khác liên quan đến các phản hồi lặp lại đối với một tập hợp đầu dò đo lường

Mỗi bộ dữ liệu trong IRW được thiết kế để:

- **Có thể tìm thấy (Findable).** Mỗi bộ dữ liệu đi kèm với siêu dữ liệu đã được tính toán sẵn — số lượng người tham gia, số lượng câu hỏi, mật độ phản hồi, lĩnh vực chủ đề và các thẻ mô tả khác — để có thể tìm và lọc bộ dữ liệu mà không cần tải xuống trước.
- **Có thể truy cập (Accessible).** Mỗi bộ dữ liệu có thể được lấy qua trình duyệt web hoặc gói `irw`, chỉ cần một tài khoản miễn phí.
- **Có thể tương tác (Interoperable).** Mỗi bộ dữ liệu được định dạng lại theo định dạng dài, một dòng cho mỗi phản hồi, theo chuẩn của họ dữ liệu của nó (mô tả bên dưới), để cùng một đoạn mã phân tích có thể chạy trên nhiều bộ dữ liệu mà chỉ cần chỉnh sửa rất ít hoặc không cần chỉnh sửa.
- **Có thể tái sử dụng (Reusable).** Mỗi bộ dữ liệu có giấy phép mở, nguồn gốc của nó được ghi chép đầy đủ, và mã nguồn dùng để chuyển đổi nó sang định dạng IRW được công khai.

Các bộ dữ liệu có sự khác biệt lớn về quy mô (từ vài trăm phản hồi đến hàng triệu) và về loại phản hồi (mục có/không, đánh giá nhiều hạng mục, điểm số theo phần, hạng mục không có thứ tự, kết quả đọi đầu, vị trí xếp hạng, và nhiều hơn nữa).

## Chuẩn dữ liệu

Mọi bảng IRW đều ở **định dạng dài**: một dòng cho mỗi phản hồi. Một dòng *là gì* tùy thuộc vào họ mà bảng đó thuộc về, và các cột mang nó cũng vậy.

<img src="/figures/families/families-tree.svg" alt="Hai câu hỏi để xếp một bộ dữ liệu vào một họ của IRW."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| Họ | `source =` | Chuẩn |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn không có trong sơ đồ: họ này được xác định bởi nguồn gốc của dữ liệu chứ không phải hình dạng, nên một bảng mô phỏng có thể mang bất kỳ hình dạng nào trong số này. Các cột của từng họ được liệt kê trong [tổng quan về các chuẩn](/overview.qmd).


## Cách sử dụng

Có ba cách để lấy dữ liệu IRW, tùy thuộc vào mức độ tự động hóa mà bạn mong muốn.

**1. Duyệt trên trình duyệt web**
Khám phá các bộ dữ liệu và siêu dữ liệu của chúng trực tiếp trên [trình duyệt dữ liệu IRW](/data.qmd) — không cần tài khoản. Việc tải xuống toàn bộ một bộ dữ liệu yêu cầu tài khoản [Redivis](https://redivis.com) miễn phí, vì đó là nền tảng lưu trữ dữ liệu gốc.

**2. Sử dụng gói `irw` (khuyến nghị)**
Gói `irw` có sẵn cho cả **R** và **Python**.

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

Lần đầu tiên sử dụng gói này, bạn sẽ được yêu cầu đăng nhập bằng tài khoản Redivis miễn phí.

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
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Trang web: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Mã nguồn: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Nếu bạn sử dụng dữ liệu IRW trong công việc của mình, vui lòng trích dẫn dữ liệu gốc (chúng tôi đã cung cấp chức năng để làm việc này). Chúng tôi cũng rất trân trọng nếu bạn trích dẫn bài báo giới thiệu nêu trên.

---

*Có câu hỏi, phản hồi, hay muốn đóng góp một bộ dữ liệu? Hãy ghé thăm [trang Liên hệ](/contact.qmd) hoặc mở một issue trên [GitHub](https://github.com/itemresponsewarehouse).*
