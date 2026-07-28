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

<div class="lang-switch"><button type="button" class="lang-switch-trigger" aria-haspopup="true" aria-expanded="false" aria-controls="lang-switch-panel" aria-label="Choose a language"><i class="bi bi-globe2" aria-hidden="true"></i><i class="bi bi-chevron-down" aria-hidden="true"></i></button><div class="lang-switch-panel" id="lang-switch-panel" hidden><label class="visually-hidden" for="lang-switch-search">Search languages</label><div class="lang-switch-search-wrap"><i class="bi bi-search" aria-hidden="true"></i><input type="text" id="lang-switch-search" class="lang-switch-search" autocomplete="off" placeholder="Search languages"></div><ul class="lang-switch-list" role="listbox"></ul></div></div>
<noscript>

[English](/) · [Français](/welcome/welcome_fr.html) · [Español](/welcome/welcome_es.html) · [中文](/welcome/welcome_zh.html) · [한국어](/welcome/welcome_ko.html) · [العربية](/welcome/welcome_ar.html) · [日本語](/welcome/welcome_ja.html) · [Português](/welcome/welcome_pt.html) · [Deutsch](/welcome/welcome_de.html) · [हिन्दी](/welcome/welcome_hi.html) · [Русский](/welcome/welcome_ru.html) · [繁體中文](/welcome/welcome_zh-hant.html) · [বাংলা](/welcome/welcome_bn.html) · [Türkçe](/welcome/welcome_tr.html) · [Tiếng Việt](/welcome/welcome_vi.html) · [Italiano](/welcome/welcome_it.html) · [Nederlands](/welcome/welcome_nl.html) · [Polski](/welcome/welcome_pl.html) · [ไทย](/welcome/welcome_th.html) · [اردو](/welcome/welcome_ur.html) · [தமிழ்](/welcome/welcome_ta.html) · [فارسی](/welcome/welcome_fa.html) · [Kiswahili](/welcome/welcome_sw.html) · [עברית](/welcome/welcome_he.html) · [Bahasa Melayu](/welcome/welcome_ms.html)

</noscript>
<script>
(function () {
  var LANGS = [
    { native: "English",      en: "English",                lang: "en" },
    { native: "Français",     en: "French",                 lang: "fr" },
    { native: "Español",      en: "Spanish",                lang: "es" },
    { native: "中文",          en: "Chinese Simplified",     lang: "zh-hans" },
    { native: "한국어",        en: "Korean",                 lang: "ko" },
    { native: "العربية",      en: "Arabic",                 lang: "ar" },
    { native: "日本語",        en: "Japanese",               lang: "ja" },
    { native: "Português",    en: "Portuguese",             lang: "pt" },
    { native: "Deutsch",      en: "German",                 lang: "de" },
    { native: "हिन्दी",        en: "Hindi",                  lang: "hi" },
    { native: "Русский",      en: "Russian",                lang: "ru" },
    { native: "繁體中文",       en: "Chinese Traditional",    lang: "zh-hant" },
    { native: "বাংলা",        en: "Bengali",                lang: "bn" },
    { native: "Türkçe",       en: "Turkish",                lang: "tr" },
    { native: "Tiếng Việt",   en: "Vietnamese",             lang: "vi" },
    { native: "Italiano",     en: "Italian",                lang: "it" },
    { native: "Nederlands",   en: "Dutch",                  lang: "nl" },
    { native: "Polski",       en: "Polish",                 lang: "pl" },
    { native: "ไทย",          en: "Thai",                   lang: "th" },
    { native: "اردو",         en: "Urdu",                   lang: "ur" },
    { native: "தமிழ்",        en: "Tamil",                  lang: "ta" },
    { native: "فارسی",        en: "Persian",                lang: "fa" },
    { native: "Kiswahili",    en: "Swahili",                lang: "sw" },
    { native: "עברית",        en: "Hebrew",                 lang: "he" },
    { native: "Bahasa Melayu",en: "Malay",                  lang: "ms" }
  ];

  document.querySelectorAll(".lang-switch").forEach(function (root) {
    var trigger = root.querySelector(".lang-switch-trigger");
    var panel = root.querySelector(".lang-switch-panel");
    var search = root.querySelector(".lang-switch-search");
    var list = root.querySelector(".lang-switch-list");

    // Hrefs are read from the <noscript> fallback rather than hardcoded,
    // because Quarto resolves that markdown's links to correct relative
    // paths per page at build time; a literal string in this script would
    // stay absolute and break under file:// or non-root deployments.
    var hrefByNative = {};
    var noscriptEl = root.nextElementSibling;
    if (noscriptEl && noscriptEl.tagName === "NOSCRIPT") {
      var tmp = document.createElement("div");
      tmp.innerHTML = noscriptEl.textContent;
      Array.prototype.forEach.call(tmp.querySelectorAll("a"), function (a) {
        hrefByNative[a.textContent.trim()] = a.href;
      });
    }

    // Current language is read from <html lang="...">, which Quarto sets
    // from each page's own frontmatter, rather than guessed from the URL.
    var currentLang = (document.documentElement.lang || "").toLowerCase();

    function render(filterText) {
      list.innerHTML = "";
      var q = (filterText || "").trim().toLowerCase();
      var matches = LANGS.filter(function (entry) {
        return !q || entry.native.toLowerCase().indexOf(q) !== -1 || entry.en.toLowerCase().indexOf(q) !== -1;
      });

      if (matches.length === 0) {
        var empty = document.createElement("li");
        empty.className = "lang-switch-empty";
        empty.textContent = "No matches";
        list.appendChild(empty);
        return;
      }

      matches.forEach(function (entry) {
        var li = document.createElement("li");
        li.setAttribute("role", "option");
        if (entry.lang === currentLang) {
          li.className = "lang-switch-current";
          li.setAttribute("aria-current", "true");
          var check = document.createElement("i");
          check.className = "bi bi-check2";
          check.setAttribute("aria-hidden", "true");
          li.appendChild(check);
          li.appendChild(document.createTextNode(" " + entry.native));
        } else {
          var a = document.createElement("a");
          a.href = hrefByNative[entry.native] || "#";
          a.textContent = entry.native;
          li.appendChild(a);
        }
        list.appendChild(li);
      });
    }

    function openPanel() {
      panel.hidden = false;
      trigger.setAttribute("aria-expanded", "true");
      search.value = "";
      render("");
      search.focus();
    }

    function closePanel() {
      panel.hidden = true;
      trigger.setAttribute("aria-expanded", "false");
    }

    trigger.addEventListener("click", function (e) {
      e.stopPropagation();
      if (panel.hidden) { openPanel(); } else { closePanel(); }
    });

    search.addEventListener("input", function () {
      render(search.value);
    });

    panel.addEventListener("click", function (e) {
      e.stopPropagation();
    });

    document.addEventListener("click", function () {
      if (!panel.hidden) closePanel();
    });

    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape" && !panel.hidden) {
        closePanel();
        trigger.focus();
      }
    });

    render("");
  });
})();
</script>

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
