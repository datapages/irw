# IRW logo kit: ogive warehouse

A warehouse holding a 4×3 response matrix. A pink item characteristic curve (a logistic ogive) climbs from one pink node to the other, and the right wall comes down and lands on the right-hand stack of data. Colors are the site's: purple #8352FF, pink #FF4D8D, ink #1C1C22, cream #FFF3E2. The name is Montserrat Medium capitals, converted to outlines. All SVGs have tight viewBoxes, role="img" and a <title>, with no rasters or filters.

| File | Use |
| --- | --- |
| `irw-lockup.svg` / `-dark` | Two-line lockup with the heavier frame. Navbar and anything under ~96px tall. |
| `irw-lockup-oneline.svg` / `-dark` | One-line lockup, heavier frame. |
| `irw-icon.svg` / `-dark` | Icon, heavier frame. |
| `irw-lockup-large.svg` / `-dark` | Thin-frame lockup for large uses (≥ ~96px): home page, slides, posters, social cards. |
| `irw-icon-large.svg` / `-dark` | Thin-frame icon for large uses. |
| `irw-lockup-outlined.svg`, `irw-icon-outlined.svg` (+ `-dark`) | Copies of the `-large` files under the previous kit's names, so existing references keep working. Safe to delete once the site points at `-large`. |
| `irw-wordmark.svg` / `-dark` | "IRW" in Montserrat SemiBold. |
| `irw-favicon-16.svg`, `irw-favicon-32.svg` | Hand-tuned, pixel-aligned icons for 16 and 32px. |
| `favicon.ico` | 16 (hand-tuned) + 32 (hand-tuned) + 48. Also copied to the site root. |
| `favicon-16.png`, `favicon-32.png` | PNG favicons. |
| `apple-touch-icon.png` | 180×180 on cream. |
| `icon-192.png`, `icon-512.png` | Manifest icons, transparent. |
| `og-image.png` / `og-image-dark.png` | 1200×630 social cards on cream / ink. |
| `reference/kit-navbar-test.png`, `reference/favicon-test.png` | Test renders at 1x (100% zoom). Not published. |
| `reference/irw-brand-kit.png` | One-page overview. Not published. |

On dark backgrounds the frame and the text turn cream; the nodes stay purple and pink.

```html
<link rel="icon" href="/logos/favicon.ico" sizes="any">
<link rel="icon" type="image/png" sizes="32x32" href="/logos/favicon-32.png">
<link rel="icon" type="image/png" sizes="16x16" href="/logos/favicon-16.png">
<link rel="apple-touch-icon" href="/logos/apple-touch-icon.png">
<meta property="og:image" content="https://itemresponsewarehouse.org/logos/og-image.png">
<meta name="twitter:card" content="summary_large_image">
```
