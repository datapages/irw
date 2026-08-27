# Draft — permission request to NORC

**To:** GSS@norc.org (cc: contracts@norc.org)
**From:** Ben Domingue, Stanford Graduate School of Education
**Status:** DRAFT — not sent

---

**Subject:** Permission request — reposting item-level GSS responses in an open research archive

Dear GSS team,

I direct the Item Response Warehouse (IRW, <https://itemresponsewarehouse.org/>), a
public archive of harmonized item-level response data used for psychometric and
measurement research. The archive currently holds roughly 2,900 tables drawn from
published studies, each reformatted to a common standard so that methods work can be run
consistently across many datasets.

I am writing to ask what would be required to include item-level responses from several
GSS modules in the archive, and whether NORC would be willing to grant permission to do
so.

**What we would like to include.** Item-level responses from the following waves and
modules:

| Wave | Module |
|---|---|
| 1988 | Science, religion, and spiritualism |
| 1989 | Occupational prestige ratings |
| 1993 | Music tastes |
| 1996 | Economic attitudes |
| 2006 | Perceived causes of mental illness |
| 2016 | Job ideals; government spending; trust in institutions |

**What "included" means in practice.** For each module we would host a table containing
one row per respondent-item pair: a respondent identifier, an item identifier, and the
numeric response. We do not host respondent demographics, geography, weights, or any
other GSS variables, and we do not host anything that would support re-identification. We
would not redistribute GSS data files as such — only the reformatted item responses for
the specific modules above.

**Attribution and provenance.** Every IRW table carries the originating citation, a link
to the original source, and an explicit license field, all shown to users alongside the
data. GSS tables would be attributed to NORC at the University of Chicago with the
standard GSS citation, and would link back to the GSS site as the authoritative source.
We would record whatever license or permission terms you specify, and would state them on
every page where the data appears.

**Why we are asking.** NORC's terms state that no part of the contents of NORC websites
may be reproduced, stored, or transmitted without express written consent. We read that
as covering what we would be doing, which is why we are requesting consent rather than
proceeding on the basis that the files are publicly downloadable. The archive already
includes 111 datasets held under case-by-case permission from their originators, so this
is a route we use routinely.

**What the data would be used for.** IRW exists to support methodological research —
comparing measurement models, testing psychometric assumptions at scale, and similar
work. It is non-commercial and freely accessible. The GSS attitude modules are of
particular interest because they have been central to a line of work on
relational/correlational class analysis, and because they differ structurally from the
Likert self-report instruments that dominate the archive.

We would of course be glad to work within whatever constraints you would want to place on
this — a subset of waves, specific attribution language, a stated license, or a review of
the tables before they are posted.

Thank you for your time, and for maintaining the GSS.

Best regards,

Ben Domingue
Associate Professor, Graduate School of Education, Stanford University
itemresponsewarehouse@stanford.edu

---

## Notes for Ben before sending

- **Verify the module/wave list.** These come from the paper's Table 8 descriptions, not
  from GSS documentation directly. Worth a sanity check against the GSS variable index.
- **Occupational prestige (1989) may warrant separate treatment** — it is a 1–100 rating
  task rather than a survey attitude battery, and may sit under different documentation.
  See `04_per_module_ingestion_notes.md`.
- The framing here is reconstructed, not precedent-based — see `README.md`.
- Consider whether to mention Redivis explicitly as the hosting platform; I left it out to
  keep the ask simple, but NORC may want to know where the data would physically sit.
