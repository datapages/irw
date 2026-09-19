# Draft — note to Ramina Sotoudeh on the released code

**To:** ramina.sotoudeh@yale.edu
**Status:** DRAFT — not sent

---

**Subject:** Some notes on the metafeature_RCA.R code from "Coping With Plenitude"

Dear Dr. Sotoudeh,

I've been evaluating whether the meta-feature approach from "Coping With Plenitude" could
be applied to the Item Response Warehouse, an archive of item-level response datasets I
maintain. In the course of that I worked closely through `metafeature_RCA.R` and the
associated files in the linked Drive folder, and I found four things I thought you would
want to know about. I'm writing to report them, not to argue anything about the paper's
conclusions.

For reference, all line numbers below refer to the copy of `metafeature_RCA.R` I retrieved
on 27 August 2026 (494 lines, SHA-256 `1677519764...c231cf`).

**1. `num_vars` defaults to 10 and is not inferred from the data.**
`evaluate.metafeatures()` (line 213) takes `num_vars = 10` as a default, and uses it as the
denominator for `intrinsicDemnsionalityProp` (line 262) and `PercentOut` (line 268). Since
the simulations used 10 items throughout, this is correct there. But `select.method()`
(lines 282–287) calls `evaluate.metafeatures(data)` without passing `num_vars`, and the
README directs users to `select.method()`. So a user applying the released tool to their
own data at any other item count silently gets two features computed against the wrong
denominator. On an 18-item matrix, for instance, `PercentOut` comes out as 170 — a
percentage above 100 — and `intrinsicDemnsionalityProp` saturates at 1.000 instead of
0.556. The other twelve features are unaffected.

**2. `OverallRightKurt` is computed on a logical vector.**
Line 233 reads `overall_right_kurt = Kurt(df > mean(df, na.rm = T))`. The argument is a
logical matrix, so `Kurt` coerces to 0/1 and the result is the excess kurtosis of a
Bernoulli indicator — a deterministic function of the proportion of cells above the mean.
On a test matrix the released expression gives −1.8479, which matches the Bernoulli closed
form (1−6p(1−p))/(p(1−p)) at p = 0.4042 to three decimals. The helper `right_half_kurt()`
defined just above at line 217 computes what I take to be the intended quantity
(−1.9999 on the same input), but it does not appear to be called anywhere in the file.

**3. Community detection is fixed at Louvain.**
`metaRCA()` (line 289) exposes only `data` and `measure`; `cluster_louvain` is called
directly at line 345, and again in `dyadic_vote_ensemble` at line 446. Given that the
paper discusses the leading-eigenvector/Louvain switch as consequential for the comparison
with Boutyline's results, it may be worth exposing that as an argument for users of the
released code.

**4. Table 9's `PercentOut` column and the released formula appear to be on different
scales.**
`PercentOut` is `(nrOut/num_vars) * 100`, where `mfe`'s `nrOutliers` is a count of
attributes having outliers. That makes the feature a multiple of 100/`num_vars` on [0, 100]
— the smallest attainable non-zero value at any plausible item count is around 2.5. The
published Table 9 column runs 0.000 to 0.048. I could not find a parameterization of the
released formula that produces those magnitudes. For what it's worth, computing the
proportion of individual *cells* that are Tukey outliers on comparable datasets gives
values in the same range (median about 0.018 across a dozen of ours), so a cell-proportion
rather than a variable-proportion would fit the published numbers.

I'll note the corresponding intrinsic-dimensionality column in Table 9 resolves to
fractions like 3/7, 5/14 and 4/7, which indicates `num_vars` *was* set to the real item
count for the empirical analysis. So point 1 above looks like an issue with the released
tool rather than with the paper's own numbers.

I'd be glad to be told I've misread any of this — I was working from the released files
alone, without the empirical matrices, so I can't check any of it against your actual
pipeline. If it would be useful I'm happy to share the small reproduction script I used.

Thank you for making the code and the fitted models available in the first place; it's
more than most papers provide, and none of this would have been checkable otherwise.

Best regards,

Ben Domingue
Associate Professor, Graduate School of Education, Stanford University

---

## Notes for Ben before sending

- **No conclusions about the published tables are asserted**, per your instruction. Point 4
  is stated as "appear to be on different scales" and offers the cell-proportion
  possibility as a fit, not as an accusation. Task B2 (which would settle it) is blocked on
  Appendix E, and the note says we couldn't check against her pipeline.
- Point 1 explicitly exonerates the simulation results and the paper's Table 9 — worth
  keeping, since it's the difference between "your tool has a bug" and "your paper is
  wrong," and the evidence supports only the former.
- If you'd rather lead with the offer of the repro script, move that paragraph up.
- She may be the fastest route to Appendix E's item selections, which would unblock the
  Table 9 reproduction. I did not ask for it here — adding an ask changes the tone of the
  note from collegial to transactional. Consider a follow-up if she replies.
