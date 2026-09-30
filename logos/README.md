# IRW brand kit (site palette)

Same file names as the previous kit, so these drop straight in. Colors are the site's: purple #8352FF, yellow #FFCA3A, ink #1C1C22, cream #FFF3E2. Text is Fredoka SemiBold/Bold, converted to outlines. All SVGs have tight viewBoxes, role="img" and a <title>, with no rasters or filters.

| File | Use |
| --- | --- |
| `irw-lockup.svg` / `-dark` | Flat lockup (icon, rule, two-line name). Navbar and anything under ~96px tall. |
| `irw-lockup-oneline.svg` / `-dark` | Flat one-line lockup. |
| `irw-icon.svg` / `-dark` | Flat icon. |
| `irw-wordmark.svg` / `-dark` | "IRW" wordmark (Fredoka Bold). |
| `irw-lockup-outlined.svg` / `-dark` | **New.** Sticker style: ink outline + offset shadow (cream on dark). Use only at large sizes (≥ ~96px tall): hero, slides, posters, social cards. |
| `irw-icon-outlined.svg` / `-dark` | **New.** Sticker-style icon, large sizes only. |
| `irw-favicon-16.svg`, `irw-favicon-32.svg` | Hand-tuned, pixel-aligned icons for 16 and 32px. |
| `favicon.ico` | 16 (hand-tuned) + 32 (hand-tuned) + 48. Also copied to the site root. |
| `favicon-16.png`, `favicon-32.png` | PNG favicons. |
| `apple-touch-icon.png` | 180×180, flat icon on cream. |
| `icon-192.png`, `icon-512.png` | Manifest icons, flat, transparent. |
| `og-image.png` | 1200×630 social card: outlined lockup on cream. |
| `og-image-dark.png` | Same on ink. |
| `reference/kit-navbar-test.png`, `reference/favicon-test.png` | Test renders at 1x (100% zoom). Not published. |
| `reference/irw-brand-kit.png` | One-page overview. Not published. |

On dark backgrounds the flat icon's frame turns cream, and the outlined versions use cream outlines and shadow.

```html
<link rel="icon" href="/logos/favicon.ico" sizes="any">
<link rel="icon" type="image/png" sizes="32x32" href="/logos/favicon-32.png">
<link rel="icon" type="image/png" sizes="16x16" href="/logos/favicon-16.png">
<link rel="apple-touch-icon" href="/logos/apple-touch-icon.png">
<meta property="og:image" content="https://itemresponsewarehouse.org/logos/og-image.png">
<meta name="twitter:card" content="summary_large_image">
```
