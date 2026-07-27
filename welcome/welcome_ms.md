---
lang: ms
pagetitle: "Item Response Warehouse (IRW; Gudang Respons Item)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; Gudang Respons Item)

**Koleksi percuma dan terbuka bagi data respons item yang telah diseragamkan (harmonized), untuk penyelidikan psikometrik dan pengukuran.**

[Baca kertas kerja](https://doi.org/10.3758/s13428-025-02796-y) **(akses terbuka)**

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

## Mengapa IRW wujud

Penyelidik yang mengkaji pengukuran — dalam bidang pendidikan, psikologi, dan bidang berkaitan — memerlukan data sebenar untuk menguji dan membandingkan kaedah mereka. Data sedemikian sudah wujud dalam jumlah yang besar. Namun ia bertaburan merentasi banyak kajian, disimpan dalam pelbagai format yang berbeza, dan sering sukar digunakan semula kerana dokumentasi atau lesen yang tidak jelas.

Ini adalah masalah yang sudah lama diketahui. Bidang-bidang lain telah menyelesaikannya dengan membina sumber data yang dikongsi dan diseragamkan. Dalam sains komputer, koleksi imej berlabel ImageNet memberikan penyelidik satu penanda aras (benchmark) yang sama dan membantu mempercepatkan kemajuan dalam AI. Genetik dan sains saraf turut membina sumber terkongsi yang serupa untuk data masing-masing.

Item Response Warehouse (IRW) melakukan perkara yang sama untuk data respons item. Ia menghimpunkan ratusan set data sedia ada dan membentuk semula ke dalam satu format yang sama — supaya sesuatu kaedah yang diuji ke atas satu set data boleh diuji dengan mudah ke atas ratusan set data yang lain.

## Apa yang terdapat dalam IRW

IRW mengandungi **ratusan set data** ("jadual"), setiap satunya adalah koleksi respons individu. Satu respons dihasilkan setiap kali seseorang (atau unit lain) memberikan respons kepada sesuatu item (atau ujian lain). Contohnya termasuk:

- Jawapan pelajar dalam ujian pendidikan dan kebolehan
- Item soal selidik yang mengukur personaliti atau sikap
- Pemarkahan yang diberikan oleh penilai manusia
- Mana-mana situasi lain yang melibatkan respons berulang kepada satu set alat pengukuran

Dua perkara adalah benar bagi setiap set data dalam IRW:

- **Terbuka.** Setiap set data dilesenkan untuk penggunaan semula. Asal-usulnya didokumentasikan, dan kod yang digunakan untuk menukarkannya ke format IRW adalah terbuka kepada umum.
- **Diseragamkan (harmonized).** Setiap set data dibentuk semula ke dalam struktur mudah yang sama (diterangkan di bawah), supaya kod analisis yang sama boleh dijalankan ke atas banyak set data dengan sedikit atau tiada pengubahsuaian.

Set data berbeza dengan ketara dari segi saiz (daripada beberapa ratus respons kepada berjuta-juta) dan jenis respons (item ya/tidak, penilaian pelbagai kategori, skor separa, dan banyak lagi). Setiap set data juga disertakan dengan metadata yang telah dikira terlebih dahulu — bilangan peserta, bilangan item, kepadatan respons, bidang subjek, dan label deskriptif lain — supaya penyelidik dapat mencari set data yang berkaitan tanpa perlu memuat turun dan memproses kesemuanya terlebih dahulu.

## Standard data

<img src="/welcome/assets/diagram-cross-classification.svg" alt="Rajah grid menunjukkan bahawa setiap respons berada pada persilangan satu id dan satu item." class="welcome-figure">


Setiap set data IRW dibentuk semula ke dalam **format panjang (long format)**: satu baris bagi setiap respons. Sekurang-kurangnya, setiap baris mengandungi tiga maklumat:

| Lajur | Makna |
|---|---|
| `id` | Siapa (atau apa) yang menghasilkan respons — biasanya seseorang individu |
| `item` | Alat pengukuran mana yang menghasilkan respons — biasanya satu soalan atau tugasan |
| `resp` | Respons itu sendiri, disimpan sebagai skor ordinal |

**Contoh:**

| `id` | `item` | `resp` | `rt` | `cov_age` | `rater` |
|----|------|------|-----|-----|-------|
| 1  | Q1   | 1    | 2.3 | 26  | a     |
| 1  | Q2   | 0    | 1.8 | 26  | a     |
| 2  | Q1   | 1    | 2.1 | 31  | b     |
| 2  | Q2   | 1    | 2.5 | 31  | b     |

Apabila sesuatu set data mengandungi maklumat tambahan — masa tindak balas, identiti penilai, kovariat seperti umur — maklumat tersebut disimpan dalam lajur tambahan yang dinamakan secara konsisten. Satu struktur mudah ini merangkumi pelbagai situasi pengukuran yang sangat luas, dan inilah yang membolehkan kod analisis ditulis sekali sahaja dan digunakan merentasi keseluruhan gudang data.

Spesifikasi teknikal penuh bagi standard ini boleh didapati di [itemresponsewarehouse.org/standard.html](/standard.qmd). Standard berkaitan yang lebih khusus turut wujud untuk teks item, data perbandingan berpasangan (pairwise), dan respons nominal (kategori tidak tersusun).

## Cara menggunakannya

Terdapat tiga cara untuk mendapatkan data IRW, bergantung kepada tahap automasi yang anda inginkan.

**1. Semak imbas dalam pelayar web**
Terokai set data dan metadatanya terus dalam [pelayar data IRW](/data.qmd) — tiada akaun diperlukan. Memuat turun set data lengkap memerlukan akaun percuma [Redivis](https://redivis.com), kerana itulah platform yang menyimpan data asas.

**2. Gunakan pakej `irw` (disyorkan)**
Pakej `irw`, yang tersedia untuk kedua-dua **R** dan **Python**, menyediakan fungsi mudah untuk mencari, menapis, dan memuat turun data.

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

Pada kali pertama anda menggunakan pakej ini, anda akan diminta untuk log masuk dengan akaun Redivis percuma. Selepas itu, satu baris kod sahaja memuat turun mana-mana set data terus ke dalam R atau Python. Dari situ, data sudah sedia untuk dianalisis menggunakan perisian standard — contohnya pakej teori respons item atau analisis faktor.

**3. Gunakan pustaka klien Redivis secara terus**
Bagi aliran kerja peringkat rendah atau bukan R/Python, data juga boleh diakses melalui pustaka klien R dan Python milik Redivis sendiri. Rujuk [Panduan Memulakan](/getstarted.qmd) untuk maklumat lanjut.

### Lebih daripada sekadar memuat turun data

Projek IRW juga merangkumi:

- Satu koleksi **[vignette](/vignettes/index.qmd)** yang semakin berkembang — contoh terperinci yang menerapkan kaedah pengukuran klasik dan baharu ke atas banyak set data IRW secara serentak
- **Sumber latihan dan set masalah** untuk mengajar psikometrik menggunakan data sebenar
- **Proses sumbangan** untuk penyelidik yang ingin menambah set data mereka sendiri ke dalam gudang data

## Ketahui lebih lanjut

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Laman web: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Kod: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Jika anda menggunakan data IRW dalam kajian anda, sila petik (cite) data asal (kami telah menyediakan kefungsian untuk berbuat demikian). Kami turut amat menghargai jika anda memetik kertas kerja pengenalan di atas.

---

*Ada soalan, maklum balas, atau ingin menyumbang set data? Layari [halaman Hubungi Kami](/contact.qmd) atau buka isu (issue) di [GitHub](https://github.com/itemresponsewarehouse).*
