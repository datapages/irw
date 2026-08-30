# Draft — permission request to ANES

**To:** anes@electionstudies.org
**From:** Ben Domingue, Stanford Graduate School of Education
**Status:** SENT 2026-08-29 by ben-domingue. Awaiting response.
**Resolves:** the open question in `02_anes_2012_note.md` (redistribution terms), asked
directly rather than inferred from the DUA.

---

**Subject:** Permission request — including ANES feeling-thermometer responses in an open research archive

Dear ANES team,

I direct the Item Response Warehouse (IRW, <https://itemresponsewarehouse.org/>), a public
archive of harmonized item-level response data used for psychometric and measurement
research. The archive holds roughly 2,900 tables drawn from published studies, each
reformatted to a common standard so that methods work can be run consistently across many
datasets.

I am writing to ask whether ANES would permit us to include item-level responses from the
2016 Time Series feeling-thermometer batteries, and if so, on what terms.

**What we would like to include.** The post-election feeling thermometers toward social
groups and institutions (V162095–V162113, and the CASI/web racial-group thermometers
V162310–V162314), and the corresponding pre-election thermometers (V161086–V161096) as a
second wave. Roughly 4,270 respondents, with about 3,600 valid responses per item.

**What "included" means in practice.** For each battery we would host a table with one row
per respondent-item pair: a respondent identifier, an item identifier, and the numeric
0–100 response. We would not host demographics, geography, survey weights, or any other
ANES variables, and nothing we host would support re-identification. We would not
redistribute the ANES data files as such — only the reformatted responses for the
batteries above. If ANES would prefer a narrower scope, or would like to review the tables
before they are posted, we would be glad to work that way.

**Why we are asking rather than proceeding.** ANES makes the public-use files freely
available, but behind a registration step. An archive that reposts the responses removes
that step, and we did not want to make that decision on your behalf — the licensing
question and the process question seem to us to be separate, and both are yours to answer.
We would note that we encountered the 2016 Time Series file redistributed in a replication
package on Harvard Dataverse under a CC0 label
(<https://doi.org/10.7910/DVN/OHRGWV>). We have not treated that as authorization, since
the depositors were licensing their own code rather than your data, but you may want to
know that it is out there.

**Attribution and provenance.** Every IRW table carries the originating citation, a link to
the authoritative source, and an explicit license or permission field, all shown to users
alongside the data. ANES tables would carry the standard ANES citation and link back to
electionstudies.org, and we would record and display whatever terms you specify. The
archive already includes 111 datasets held under case-by-case permission from their
originators, so this is a route we use routinely. One ANES-adjacent table is already in the
archive from the 2000 NES.

**What the data would be used for.** IRW exists to support methodological research —
comparing measurement models, testing psychometric assumptions at scale, and similar work.
It is non-commercial and freely accessible. The thermometers are of particular interest
because they are continuous 0–100 ratings of a large common set of targets, which is
structurally unlike the short Likert instruments that dominate the archive and therefore
unusually useful for testing whether measurement methods generalize.

Finally: if you are able to grant this, would the same answer extend to the
feeling-thermometer batteries in other public-use Time Series waves, or would you prefer we
ask per wave?

Thank you for your time, and for maintaining the ANES.

Best regards,

Ben Domingue
Associate Professor, Graduate School of Education, Stanford University
itemresponsewarehouse@stanford.edu

---

## Notes for Ben before sending

- **Verify the address.** `anes@electionstudies.org` is the address ANES publishes, but its
  documented purpose is reporting inadvertent respondent identification, not general
  correspondence. electionstudies.org 403s every programmatic request, so I could not read
  the contact page directly. Worth opening
  <https://electionstudies.org/about-us/contact-us/> in a browser to check for a better
  address before sending; there is also a phone line, 1-800-759-7947.
- **Variable list is verified**, read from the 2016 Stata file itself (1,842 variables;
  labels confirm the thermometer targets). The −9 / 998 / 999 sentinels would be dropped.
- The framing follows `01_norc_gss_permission.md`, which is reconstructed rather than
  precedent-based — see `README.md`. If the real precedent language is in your email
  archive, prefer it.
- Consider whether to mention Redivis as the hosting platform; left out to keep the ask
  simple, as with NORC.
