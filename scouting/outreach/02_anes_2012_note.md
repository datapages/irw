# ANES 2012 — access and terms: what we could and could not determine

**Status:** ASKED DIRECTLY. ben-domingue emailed ANES 2026-08-29 rather than reading the
DUA — see `05_anes_2016_permission.md`. Awaiting response. Everything below stands as the
reasoning behind that email.

## What the 403 was

Every automated request to `electionstudies.org` returned **HTTP 403 Forbidden**:

| URL | Result |
|---|---|
| `https://electionstudies.org/papers-documents/policy-on-access-to-anes-data/` | 403 |
| `https://electionstudies.org/data-center/` | 403 |
| `https://electionstudies.org/faq/` | 403 (empty response) |

This is **bot-blocking, not authentication**. The pages are publicly readable in a normal
browser; the site rejects programmatic fetches. So this is not evidence that the terms are
restrictive — it is simply an inability to read them from here. Per the scouting brief,
recorded as an open item rather than worked around.

## What is known from secondary sources

From search results summarizing ANES policy (not from the DUA itself):

- ANES public-use data are **free** but access is **gated behind registration** — users
  create an account before downloading.
- Users must **cite** ANES data and documentation, and are asked to send citations of
  published work to ANES for their bibliography.
- Use of the data to **learn the identity of any respondent is prohibited**, and users
  must report any inadvertent identification.
- A separate, stricter track exists for **Restricted-Use Data**, handled through ICPSR's
  Virtual Data Enclave under a signed Data Use Agreement with IRB documentation. The 2012
  Time Series public-use file is *not* in this category.

## What is NOT known

**Whether redistribution to a third-party repository is permitted.** None of the secondary
sources addresses reposting. The registration requirement is itself a complication
distinct from GSS's: even if reuse is liberal, an archive that redistributes the responses
removes the registration step that ANES has deliberately put in place, which they may
object to on process grounds regardless of licensing.

## Recommended next step

One person, one browser, ten minutes: open the ANES account-registration flow and read the
terms presented at download. The specific question to answer is whether the terms permit
redistribution of derived item-level response files, or are silent on it (in which case
ask ANES directly, as with NORC).

**Do not assume ANES terms resemble GSS terms.** They are different organizations with
different access models, and the one ANES-adjacent table already in IRW
(`polca_election`) is from the **2000** NES, not 2012 — so it is not a usable precedent
for this module either.
