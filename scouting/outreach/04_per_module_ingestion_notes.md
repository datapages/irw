# Per-module ingestion notes

Prepared for the NORC/ANES requests. **Item counts and exact item selections are not
determinable without Online Appendix E**, which is paywalled (HTTP 403). Everything below
about module *content* comes from the paper's Table 8; everything about *format* comes
from general knowledge of GSS/ANES instruments and should be verified against the GSS
variable index before ingestion.

## Summary

| Module | Wave | Response format | Ordinal as-is? | IRW handling |
|---|---|---|---|---|
| Science, religion, spiritualism | 1988 GSS | Likert agreement | yes | standard |
| Occupational prestige | 1989 GSS | **1–100 rating** | **see below** | **needs decision** |
| Music tastes | 1993 GSS | like/dislike scale | yes | standard |
| Economic attitudes | 1996 GSS | Likert agreement | yes | standard |
| Mental health causes | 2006 GSS | likelihood rating | yes | standard |
| Job ideals | 2016 GSS | importance rating | yes | standard |
| Government spending | 2016 GSS | too little / about right / too much | yes (3-cat) | standard, but see note |
| Trust in institutions | 2016 GSS | confidence rating | yes (3-cat) | standard, but see note |
| Political attitudes | 2012 ANES | mixed | **verify** | verify |

## Occupational prestige (1989) — the one that needs a decision

Respondents rate occupations on a **1–100 prestige scale**. This is a continuous or
near-continuous rating, not a Likert item, and it is the module that made their Table 9
an outlier (OverallSD 2.288 against 0.691–1.584 for everything else; VarColPC1 47.99
against 1.34–11.31).

Three things follow:

1. **IRW has precedent.** The eligible-table scan found tables at `n_categories = 101`
   already in the archive (`test_taking_much_2025_cm`, `climatechange_geiger_2025`). So a
   1–100 response is representable under the existing standard — this is not a new
   structural problem.
2. **It should not be collapsed to a coarser scale.** Its structural distinctiveness is
   precisely what makes it valuable in a corpus dominated by 4–7 category Likert items,
   and binning it would destroy that.
3. **It changes what "item" means.** In the other modules an item is a survey question. In
   the prestige module, the rated *occupation* is the item and the prestige score is the
   response. Worth stating explicitly in the table description so users don't treat it as
   an attitude battery.

## Three-category modules (government spending, trust in institutions)

Both use 3-point response scales. These are ordinal and ingest cleanly, but note they fall
**below the `n_categories >= 4` cutoff** used in the eligibility scan for RCA/CCA work —
the reasoning being that on very short ordinal scales the person-by-person similarity
matrices become badly tied. They are perfectly good IRW tables; they would just not be
eligible for that particular analysis.

## ANES 2012 political attitudes

The paper describes this as a mixed module of political attitude items. Response formats
across ANES batteries vary considerably (7-point ideology scales, feeling thermometers at
0–100, binary items). **Verify per-item before ingestion** — a feeling thermometer and a
7-point agreement item should not be pooled into one table without noting the mix, and
under the IRW standard (`contribute.qmd`, point 4) responses from distinct scales should
be split into separate files anyway.

Access terms are separately unresolved — see `02_anes_2012_note.md`.

## General notes for all GSS modules

- **Missing-value codes must be stripped.** GSS uses negative and reserved codes for
  don't-know / refused / not-applicable. The IRW standard is explicit that missing values
  cannot be coded as numbers (`contribute.qmd`, point 1). These need converting to `NA`,
  not carried through as responses.
- **One table per scale.** Where a wave's module spans more than one distinct instrument,
  split it (`contribute.qmd`, point 4). This is also what would make the modules
  merge-eligible under the DOI-keyed grouping used elsewhere in this scouting run.
- **Respondent ids must be stable within a wave** if any cross-module merging is intended.
  GSS respondent ids are wave-specific; do not assume comparability across waves.
- **Item text** is available for GSS and would be worth including via the itemtext
  pipeline, since the modules are well documented.
