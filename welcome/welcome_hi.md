---
lang: hi
pagetitle: "Item Response Warehouse (IRW; आइटम रिस्पॉन्स वेयरहाउस)"
---

<!--
Translator note: please do NOT translate the following —
1. Column names in backticks/tables (`id`, `item`, `resp`, `rt`, `age`, `rater`) — these are literal data field names.
2. The contents of fenced code blocks (```r / ```python), including comments.
3. Paper titles in the "Learn more" citations — translate only surrounding text, not the titles themselves.
4. URLs and DOIs.
5. The language-switcher dropdown block below (HTML/CSS/JS, no visible text to translate) — it is identical, verbatim, across every welcome_<lang>.md file. Do not translate, reorder, or hand-edit it per file; the "current language" state is computed at runtime by the script from the page URL.
-->

# Item Response Warehouse (IRW; आइटम रिस्पॉन्स वेयरहाउस)

**मनोमिति (साइकोमेट्रिक्स) और मापन अनुसंधान के लिए, समरूप बनाए गए आइटम-प्रतिक्रिया डेटा का एक निःशुल्क, खुला संग्रह।**

[शोध-पत्र पढ़ें](https://doi.org/10.3758/s13428-025-02796-y) **(ओपन एक्सेस)**

{{< include /welcome/_lang_switch.md >}}

---

## IRW क्यों मौजूद है

शिक्षा, मनोविज्ञान और संबंधित क्षेत्रों में मापन का अध्ययन करने वाले शोधकर्ताओं को अपनी विधियों का परीक्षण और तुलना करने के लिए वास्तविक डेटा की आवश्यकता होती है। यह डेटा पहले से ही बड़ी मात्रा में मौजूद है। लेकिन यह कई अध्ययनों में बिखरा हुआ है, अनेक अलग-अलग प्रारूपों में संग्रहीत है, और अस्पष्ट दस्तावेज़ीकरण या लाइसेंसिंग के कारण अक्सर इसका पुनः उपयोग करना कठिन होता है।

यह एक सुपरिचित समस्या है। शोध समुदाय ने इसे हल करने के लिए एक साझा मानक तैयार किया है: डेटा को **FAIR** होना चाहिए — यानी खोजने योग्य (Findable), सुलभ (Accessible), अंतरसंचालनीय (Interoperable), और पुनः प्रयोग योग्य (Reusable) (Wilkinson et al., 2016)। अन्य क्षेत्रों ने इन सिद्धांतों को अमल में लाकर साझा, मानकीकृत डेटा संसाधन बनाए हैं। कंप्यूटर विज्ञान में, लेबल किए गए चित्रों के संग्रह ImageNet ने शोधकर्ताओं को एक साझा मानदंड प्रदान किया और कृत्रिम बुद्धिमत्ता (AI) में तेज़ प्रगति लाने में मदद की। आनुवंशिकी और तंत्रिका विज्ञान ने भी अपने-अपने डेटा के लिए इसी तरह के साझा संसाधन बनाए हैं।

Item Response Warehouse (IRW) आइटम-प्रतिक्रिया डेटा पर यही FAIR सिद्धांत लागू करता है। यह हज़ारों मौजूदा डेटासेट को एक साथ लाता है और उन्हें एक साझा प्रारूप में ढालता है — ताकि किसी एक डेटासेट पर परखी गई विधि को हज़ारों अन्य डेटासेट पर भी आसानी से परखा जा सके।

## IRW में क्या है

IRW में **हज़ारों डेटासेट** ("टेबल") हैं, जिनमें से प्रत्येक व्यक्तिगत प्रतिक्रियाओं का एक संग्रह है। जब भी कोई व्यक्ति (या कोई अन्य इकाई) किसी मापन-प्रोब से सामना करता है और परिणाम दर्ज होता है, तो एक प्रतिक्रिया उत्पन्न होती है। प्रोब क्या है, यह [डेटा परिवार](/overview.qmd) पर निर्भर करता है: अकेले उत्तर दिया गया कोई आइटम, किसी मुकाबले में मिली कोई दूसरी इकाई, विकल्पों का पूरा समूह जिसे क्रम में रखना हो, या यादृच्छिक रूप से निर्धारित विशेषताओं से बना कोई प्रोफ़ाइल। उदाहरणों में शामिल हैं:

- शिक्षा और योग्यता परीक्षणों में छात्रों के उत्तर
- व्यक्तित्व या दृष्टिकोण मापने वाले सर्वेक्षण आइटम
- मानव मूल्यांकनकर्ताओं द्वारा दी गई रेटिंग
- खेल, मुकाबले और आमने-सामने की तुलनाएँ, जहाँ दो इकाइयाँ भिड़ती हैं और एक जीतती है
- वरीयता-क्रम वाले मतपत्र, निर्णायक मंडल की वरीयताएँ और सर्वेक्षण के क्रम-निर्धारण प्रश्न
- कॉन्जॉइंट प्रयोग, जिनमें उत्तरदाता यादृच्छिक रूप से बनी प्रोफ़ाइलों में से चुनते हैं
- कोई भी अन्य स्थिति जिसमें मापन-प्रोब के एक समूह पर बार-बार प्रतिक्रियाएं दी जाती हों

IRW का हर डेटासेट इस तरह डिज़ाइन किया गया है कि वह:

- **खोजने योग्य (Findable) हो।** प्रत्येक डेटासेट के साथ पूर्व-गणना की गई मेटाडेटा आती है — प्रतिभागियों की संख्या, आइटमों की संख्या, प्रतिक्रिया घनत्व, विषय क्षेत्र, और अन्य वर्णनात्मक टैग — ताकि डेटासेट को पहले डाउनलोड किए बिना ढूँढ़ा और फ़िल्टर किया जा सके।
- **सुलभ (Accessible) हो।** प्रत्येक डेटासेट को वेब ब्राउज़र या `irw` पैकेज के ज़रिए, एक निःशुल्क खाते के साथ, प्राप्त किया जा सकता है।
- **अंतरसंचालनीय (Interoperable) हो।** प्रत्येक डेटासेट को लॉन्ग प्रारूप में, प्रति प्रतिक्रिया एक पंक्ति, उसके परिवार के मानक (नीचे वर्णित) के अनुसार ढाला गया है, जिससे एक ही विश्लेषण कोड को कई डेटासेट पर बिना या न्यूनतम बदलाव के चलाया जा सकता है।
- **पुनः प्रयोग योग्य (Reusable) हो।** प्रत्येक डेटासेट खुले लाइसेंस के तहत है, इसका स्रोत दस्तावेज़ीकृत है, और इसे IRW प्रारूप में बदलने के लिए उपयोग किया गया कोड सार्वजनिक है।

डेटासेट आकार में (कुछ सौ प्रतिक्रियाओं से लेकर कई लाख तक) और प्रतिक्रिया के प्रकार में (हाँ/नहीं आइटम, बहु-श्रेणी रेटिंग, आंशिक-अंक स्कोर, क्रमरहित श्रेणियाँ, मुकाबलों के परिणाम, वरीयता-स्थान, और अन्य) व्यापक रूप से भिन्न होते हैं।

## डेटा मानक

हर IRW टेबल **लॉन्ग प्रारूप** में है: प्रति प्रतिक्रिया एक पंक्ति। एक पंक्ति *क्या है*, यह इस पर निर्भर करता है कि टेबल किस परिवार की है, और उसे ढोने वाले कॉलम भी उसी पर।

<img src="/figures/families/families-tree.svg" alt="दो प्रश्न जो किसी डेटासेट को IRW के किसी परिवार में रखते हैं।"
     class="standard-tree" style="display:block;width:100%;max-width:480px;height:auto;margin:1rem auto 1.25rem">

| परिवार | `source =` | मानक |
|---|---|---|
| Core | `"core"` | [Core](/standard.qmd) |
| Nominal | `"nom"` | [Nominal](/nominal_standard.qmd) |
| Competitions | `"comp"` | [Competitions](/comps_standard.qmd) |
| Rankings | `"rank"` | [Rankings](/rankings_standard.qmd) |
| Conjoint | `"conj"` | [Conjoint](/conjoint_standard.qmd) |
| Simulated and synthetic | `"sim"` | [Simsyn](/simsyn.qmd) |

Simsyn चार्ट में नहीं है: यह परिवार डेटा के आकार से नहीं, बल्कि उसके स्रोत से परिभाषित होता है, इसलिए कोई सिमुलेटेड टेबल इनमें से किसी भी आकार की हो सकती है। हर परिवार के कॉलम [मानकों के अवलोकन](/overview.qmd) में दिए गए हैं।


## इसका उपयोग कैसे करें

आप कितना स्वचालन चाहते हैं, इस पर निर्भर करते हुए IRW डेटा प्राप्त करने के तीन तरीके हैं।

**1. वेब ब्राउज़र में ब्राउज़ करें**
[IRW डेटा ब्राउज़र](/data.qmd) पर सीधे डेटासेट और उनके मेटाडेटा का अन्वेषण करें — किसी खाते की आवश्यकता नहीं। किसी संपूर्ण डेटासेट को डाउनलोड करने के लिए एक निःशुल्क [Redivis](https://redivis.com) खाता आवश्यक है, क्योंकि यही वह प्लेटफ़ॉर्म है जो अंतर्निहित डेटा को होस्ट करता है।

**2. `irw` पैकेज का उपयोग करें (अनुशंसित)**
`irw` पैकेज **R** और **Python** दोनों के लिए उपलब्ध है।

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

पैकेज का पहली बार उपयोग करने पर, आपसे एक निःशुल्क Redivis खाते से लॉग इन करने के लिए कहा जाएगा।

**3. Redivis की क्लाइंट लाइब्रेरी का सीधे उपयोग करें**
निम्न-स्तरीय वर्कफ़्लो या R/Python से बाहर के वर्कफ़्लो के लिए, डेटा को Redivis की अपनी R और Python क्लाइंट लाइब्रेरी के माध्यम से भी एक्सेस किया जा सकता है। अधिक जानकारी के लिए [शुरुआत करें मार्गदर्शिका](/getstarted.qmd) देखें।

### केवल डेटा डाउनलोड करने से आगे

IRW परियोजना में यह भी शामिल है:

- **[विग्नेट्स (vignettes)](/vignettes/index.qmd)** का एक बढ़ता हुआ संग्रह — क्लासिक और नई मापन विधियों को एक साथ कई IRW डेटासेट पर लागू करने वाले व्यावहारिक उदाहरण
- वास्तविक डेटा के साथ मनोमिति पढ़ाने के लिए **प्रशिक्षण संसाधन और अभ्यास सेट**
- अपने स्वयं के डेटासेट वेयरहाउस में जोड़ने के इच्छुक शोधकर्ताओं के लिए **एक योगदान प्रक्रिया**

## अधिक जानें

- Domingue et al. (2025). *An introduction to the Item Response Warehouse (IRW): A resource for enhancing data usage in psychometrics.* Behavior Research Methods. [doi:10.3758/s13428-025-02796-y](https://doi.org/10.3758/s13428-025-02796-y)
- Nadela, Lee, Jain, Gupta, Zhang & Domingue (2026). *The Item Response Warehouse: What It Is, How to Use It, and Targets for Potential Improvements.* Chinese/English Journal of Educational Measurement and Evaluation. [doi:10.59863/CIJG4549](https://doi.org/10.59863/CIJG4549)
- Wilkinson et al. (2016). *The FAIR Guiding Principles for scientific data management and stewardship.* Scientific Data. [doi:10.1038/sdata.2016.18](https://doi.org/10.1038/sdata.2016.18)
- वेबसाइट: [itemresponsewarehouse.org](https://itemresponsewarehouse.org)
- कोड: [github.com/itemresponsewarehouse](https://github.com/itemresponsewarehouse)

यदि आप अपने कार्य में IRW डेटा का उपयोग करते हैं, तो कृपया मूल डेटा को उद्धृत करें (हमने ऐसा करने के लिए एक सुविधा प्रदान की है)। यह भी बहुत अच्छा होगा यदि आप ऊपर दिए गए परिचयात्मक शोध-पत्र को भी उद्धृत करें।

---

*कोई प्रश्न, प्रतिक्रिया, या किसी डेटासेट में योगदान देना चाहते हैं? [संपर्क पृष्ठ](/contact.qmd) पर जाएँ या [GitHub](https://github.com/itemresponsewarehouse) पर एक "issue" खोलें।*
