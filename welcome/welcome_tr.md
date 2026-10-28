---
lang: tr
pagetitle: "Item Response Warehouse (IRW; Madde Tepki Deposu)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Madde Tepki Deposu)

**Psikometri ve ölçme araştırmaları için ücretsiz, açık ve uyumlaştırılmış madde tepki verilerinden oluşan bir koleksiyon.**

[Makaleyi okuyun](https://doi.org/10.3758/s13428-025-02796-y) **(açık erişim)**

{{< include /welcome/_lang_switch.md >}}

---

## IRW neden var

Ölçme konusunu çalışan araştırmacıların — eğitim, psikoloji ve ilgili alanlarda — yöntemlerini sınamak ve karşılaştırmak için gerçek verilere ihtiyacı vardır. Bu tür veriler zaten büyük miktarlarda mevcuttur. Ancak birçok çalışmaya dağılmış, birçok farklı biçimde saklanmış ve genellikle belirsiz belgeleme veya lisanslama nedeniyle yeniden kullanımı zor durumdadır.

Bu iyi bilinen bir sorundur. Araştırma topluluğu bu sorunu çözmek için ortak bir standart ortaya koymuştur: veriler **FAIR** olmalıdır — bulunabilir (Findable), erişilebilir (Accessible), birlikte çalışabilir (Interoperable) ve yeniden kullanılabilir (Reusable) (Wilkinson et al., 2016). Diğer alanlar, bu ilkeleri paylaşılan ve standartlaştırılmış veri kaynakları oluşturarak uygulamaya koymuştur. Bilgisayar biliminde, etiketlenmiş görüntülerden oluşan ImageNet koleksiyonu araştırmacılara ortak bir kıyaslama noktası (benchmark) sağlamış ve yapay zekada hızlı ilerlemeyi desteklemiştir. Genetik ve nörobilim de kendi verileri için benzer paylaşılan kaynaklar oluşturmuştur.

Item Response Warehouse (IRW), aynı FAIR ilkelerini madde tepki verilerine uygular. Yüzlerce mevcut veri kümesini bir araya getirir ve bunları tek bir ortak biçime dönüştürür — böylece bir veri kümesi üzerinde test edilen bir yöntem, yüzlerce başka veri kümesi üzerinde de kolayca test edilebilir.

## IRW'de neler var

IRW, her biri bireysel tepkilerden oluşan bir koleksiyon olan **yüzlerce veri kümesi** ("tablo") içerir. Bir kişi (veya başka bir birim) bir maddeye (veya başka bir ölçme aracına) tepki verdiğinde bir tepki (response) oluşur. Örnekler şunları içerir:

- Öğrencilerin eğitim ve yetenek testlerindeki cevapları
- Kişiliği veya tutumları ölçen anket maddeleri
- İnsan değerlendiriciler tarafından verilen puanlar
- Bir dizi ölçme aracına tekrarlanan tepkilerin verildiği diğer her türlü durum

IRW'deki her veri kümesi şu şekilde tasarlanmıştır:

- **Bulunabilir (Findable).** Her veri kümesi, önceden hesaplanmış meta verilerle birlikte gelir — katılımcı sayısı, madde sayısı, tepki yoğunluğu, konu alanı ve diğer açıklayıcı etiketler — böylece veri kümeleri önce indirilmeden bulunabilir ve filtrelenebilir.
- **Erişilebilir (Accessible).** Her veri kümesine, ücretsiz bir hesapla, web tarayıcısı veya `irw` paketi üzerinden erişilebilir.
- **Birlikte çalışabilir (Interoperable).** Her veri kümesi aynı basit yapıya (aşağıda açıklanmıştır) dönüştürülür, böylece aynı analiz kodu birçok veri kümesinde çok az değişiklikle veya hiç değişiklik yapılmadan çalıştırılabilir.
- **Yeniden kullanılabilir (Reusable).** Her veri kümesi açık lisanslıdır, kökeni belgelenmiştir ve onu IRW biçimine dönüştürmek için kullanılan kod herkese açıktır.

Veri kümeleri boyut açısından (birkaç yüz tepkiden milyonlarcasına kadar) ve tepki türü açısından (evet/hayır maddeleri, çok kategorili puanlamalar, kısmi puanlar ve daha fazlası) büyük farklılıklar gösterir. Her veri kümesi ayrıca önceden hesaplanmış meta verilerle birlikte gelir — katılımcı sayısı, madde sayısı, tepki yoğunluğu, konu alanı ve diğer açıklayıcı etiketler — böylece araştırmacılar önce hepsini indirip işlemeden ilgili veri kümelerini bulabilir.

## Veri standardı

<img src="/welcome/assets/diagram-cross-classification.svg" alt="id-item hücrelerinden oluşan renkli bir ızgaranın, bir okla id, item ve resp sütunlarına sahip uzun format bir tabloya dönüştürüldüğünü gösteren şema; her resp hücresinin rengi, ızgaradaki kaynak hücresinin rengiyle eşleşir." class="welcome-figure">


Her IRW veri kümesi **uzun format (long format)**'a dönüştürülür: her tepki için bir satır. Her satır en az üç bilgi parçası içerir:

| Sütun | Anlamı |
|---|---|
| `id` | Tepkiyi kim (veya ne) ürettiği — genellikle bir kişi |
| `item` | Tepkiyi hangi ölçme aracının ürettiği — genellikle bir soru veya görev |
| `resp` | Tepkinin kendisi, sıralı (ordinal) bir puan olarak saklanır |

**Örnek:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Bir veri kümesi ek bilgiler içerdiğinde — tepki süresi, değerlendirici kimliği, yaş gibi kovaryatlar — bu bilgiler tutarlı biçimde adlandırılmış ek sütunlarda saklanır. Bu tek basit yapı, çok geniş bir ölçme durumları yelpazesini kapsar; bu da analiz kodunu bir kez yazıp tüm depoya uygulayabilmeyi mümkün kılan şeydir.

Standardın tam teknik özellikleri [itemresponsewarehouse.org/standard.html](/standard.qmd) adresinde mevcuttur. Madde metni, ikili karşılaştırma (pairwise) verileri ve nominal (sırasız kategori) tepkiler için de ilgili, daha uzmanlaşmış standartlar bulunmaktadır.

## Nasıl kullanılır

Ne kadar otomatikleştirmek istediğinize bağlı olarak IRW verisini almanın üç yolu vardır.

**1. Web tarayıcısında gözden geçirin**
[IRW veri tarayıcısı](/data.qmd) üzerinde veri kümelerini ve meta verilerini doğrudan keşfedin — hesap gerekmez. Tam bir veri kümesini indirmek için ücretsiz bir [Redivis](https://redivis.com) hesabı gerekir, çünkü temel verileri barındıran platform budur.

**2. `irw` paketini kullanın (önerilir)**
Hem **R** hem de **Python** için mevcut olan `irw` paketi, veri bulma, filtreleme ve indirme için basit fonksiyonlar sunar.

```r
# R
install.packages("irw")
library(irw)

irw_info()                     # overview of the IRW
irw_list_tables()              # list all available tables
irw_filter(var = "rt")         # find tables that include response times
df <- irw_fetch("4thgrade_math_sirt")   # download one table
```

```python
# Python
# pip install irw
import irw

irw.info()
irw.list_tables()
irw.filter(var="rt")
df = irw.fetch("4thgrade_math_sirt")
```

Paketi ilk kullandığınızda, ücretsiz bir Redivis hesabıyla giriş yapmanız istenir. Bundan sonra, tek bir kod satırı herhangi bir veri kümesini doğrudan R veya Python'a indirir. Bu noktadan itibaren veri, standart yazılımlarla — örneğin madde tepki kuramı veya faktör analizi paketleriyle — analiz edilmeye hazırdır.

**3. Redivis istemci kütüphanelerini doğrudan kullanın**
Daha düşük seviyeli veya R/Python dışı iş akışları için veriye, Redivis'in kendi R ve Python istemci kütüphaneleri aracılığıyla da erişilebilir. Ayrıntılar için [Başlangıç Kılavuzu](/getstarted.qmd)'na bakın.

### Veri indirmenin ötesinde

IRW projesi ayrıca şunları da içerir:

- Klasik ve yeni ölçme yöntemlerini birçok IRW veri kümesi üzerinde aynı anda uygulayan, giderek büyüyen bir **[örnek çalışmalar (vignettes)](/vignettes/index.qmd)** kümesi
- Gerçek verilerle psikometri öğretimi için **eğitim kaynakları ve alıştırma setleri**
- Kendi veri kümelerini depoya eklemek isteyen araştırmacılar için bir **katkı süreci**

## Daha fazla bilgi

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Web sitesi: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Kod: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Çalışmanızda IRW verilerini kullanıyorsanız, lütfen orijinal veriyi kaynak gösterin (bunun için gerekli işlevselliği sağladık). Yukarıdaki tanıtım makalesini de kaynak göstermeniz bizim için çok değerli olur.

---

*Sorularınız, geri bildiriminiz mi var ya da bir veri kümesine katkıda mı bulunmak istiyorsunuz? [İletişim sayfasını](/contact.qmd) ziyaret edin veya [GitHub](https://github.com/itemresponsewarehouse) üzerinde bir issue açın.*
