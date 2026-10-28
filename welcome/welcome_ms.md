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

{{< include /welcome/_lang_switch.md >}}

---

## Mengapa IRW wujud

Penyelidik yang mengkaji pengukuran — dalam bidang pendidikan, psikologi, dan bidang berkaitan — memerlukan data sebenar untuk menguji dan membandingkan kaedah mereka. Data sedemikian sudah wujud dalam jumlah yang besar. Namun ia bertaburan merentasi banyak kajian, disimpan dalam pelbagai format yang berbeza, dan sering sukar digunakan semula kerana dokumentasi atau lesen yang tidak jelas.

Ini adalah masalah yang sudah lama diketahui. Komuniti penyelidikan telah menggariskan piawaian bersama untuk menyelesaikannya: data perlu **FAIR** — mudah dijumpai (Findable), boleh diakses (Accessible), boleh berinteroperasi (Interoperable), dan boleh digunakan semula (Reusable) (Wilkinson et al., 2016). Bidang-bidang lain telah melaksanakan prinsip-prinsip ini dengan membina sumber data yang dikongsi dan diseragamkan. Dalam sains komputer, koleksi imej berlabel ImageNet memberikan penyelidik satu penanda aras (benchmark) yang sama dan membantu mempercepatkan kemajuan dalam AI. Genetik dan sains saraf turut membina sumber terkongsi yang serupa untuk data masing-masing.

Item Response Warehouse (IRW) menerapkan prinsip FAIR yang sama untuk data respons item. Ia menghimpunkan ribuan set data sedia ada dan membentuk semula ke dalam satu format yang sama — supaya sesuatu kaedah yang diuji ke atas satu set data boleh diuji dengan mudah ke atas ribuan set data yang lain.

## Apa yang terdapat dalam IRW

IRW mengandungi **ribuan set data** ("jadual"), setiap satunya adalah koleksi respons individu. Satu respons dihasilkan setiap kali seseorang (atau unit lain) berdepan dengan sesuatu alat pengukuran dan hasilnya direkodkan. Apa yang dikira sebagai alat pengukuran berbeza mengikut [keluarga data](/overview.qmd): sesuatu item yang dijawab secara berasingan, entiti lain yang ditemui dalam sesuatu pertandingan, keseluruhan set pilihan yang perlu disusun mengikut urutan, atau profil yang dibentuk daripada ciri-ciri yang diberikan secara rawak. Contohnya termasuk:

- Jawapan pelajar dalam ujian pendidikan dan kebolehan
- Item soal selidik yang mengukur personaliti atau sikap
- Pemarkahan yang diberikan oleh penilai manusia
- Perlawanan, permainan dan perbandingan secara langsung, di mana dua entiti bertemu dan satu menang
- Kertas undi keutamaan, susunan kedudukan oleh panel juri, dan soalan menyusun kedudukan dalam tinjauan
- Eksperimen conjoint, di mana responden memilih antara profil yang dibina secara rawak
- Mana-mana situasi lain yang melibatkan respons berulang kepada satu set alat pengukuran

Setiap set data dalam IRW direka untuk:

- **Mudah dijumpai (Findable).** Setiap set data disertakan dengan metadata yang telah dikira terlebih dahulu — bilangan peserta, bilangan item, kepadatan respons, bidang subjek, dan label deskriptif lain — supaya set data dapat dicari dan ditapis tanpa perlu memuat turunnya terlebih dahulu.
- **Boleh diakses (Accessible).** Setiap set data boleh diperoleh melalui pelayar web atau pakej `irw`, dengan akaun percuma.
- **Boleh berinteroperasi (Interoperable).** Setiap set data dibentuk semula ke dalam format panjang, satu baris bagi setiap respons, mengikut standard keluarganya (diterangkan di bawah), supaya kod analisis yang sama boleh dijalankan ke atas banyak set data dengan sedikit atau tiada pengubahsuaian.
- **Boleh digunakan semula (Reusable).** Setiap set data mempunyai lesen terbuka, asal-usulnya didokumentasikan, dan kod yang digunakan untuk menukarkannya ke format IRW adalah terbuka kepada umum.

Set data berbeza dengan ketara dari segi saiz (daripada beberapa ratus respons kepada berjuta-juta) dan jenis respons (item ya/tidak, penilaian pelbagai kategori, skor separa, kategori tanpa urutan, keputusan pertandingan, kedudukan dalam susunan, dan banyak lagi).

## Standard data

Setiap jadual IRW berada dalam **format panjang**: satu baris bagi setiap respons. Apa yang sesuatu baris itu *wakili* bergantung pada keluarga jadual tersebut, begitu juga lajur-lajur yang membawanya.

<img src="/figures/families/families-tree.svg" alt="Dua soalan yang meletakkan sesuatu set data dalam keluarga IRW."
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| Keluarga | `source =` | Standard |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn tiada dalam carta: keluarga ini ditakrifkan oleh asal usul data, bukan bentuknya, jadi jadual simulasi boleh mempunyai mana-mana daripada bentuk ini. Lajur bagi setiap keluarga ada dalam [gambaran keseluruhan standard](/overview.qmd).


## Cara menggunakannya

Terdapat tiga cara untuk mendapatkan data IRW, bergantung kepada tahap automasi yang anda inginkan.

**1. Semak imbas dalam pelayar web**
Terokai set data dan metadatanya terus dalam [pelayar data IRW](/data.qmd) — tiada akaun diperlukan. Memuat turun set data lengkap memerlukan akaun percuma [Redivis](https://redivis.com), kerana itulah platform yang menyimpan data asas.

**2. Gunakan pakej `irw` (disyorkan)**
Pakej `irw` tersedia untuk kedua-dua **R** dan **Python**.

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

Pada kali pertama anda menggunakan pakej ini, anda akan diminta untuk log masuk dengan akaun Redivis percuma.

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
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- Laman web: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- Kod: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

Jika anda menggunakan data IRW dalam kajian anda, sila petik (cite) data asal (kami telah menyediakan kefungsian untuk berbuat demikian). Kami turut amat menghargai jika anda memetik kertas kerja pengenalan di atas.

---

*Ada soalan, maklum balas, atau ingin menyumbang set data? Layari [halaman Hubungi Kami](/contact.qmd) atau buka isu (issue) di [GitHub](https://github.com/itemresponsewarehouse).*
