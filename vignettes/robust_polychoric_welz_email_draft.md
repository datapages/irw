# Draft email to Max Welz

Draft only — Ben sends, the agent does not. Attachment:
`robust_polychoric_robcat_note.md` (or its rendered HTML/PDF).

---

**Subject:** robcat: default initialization landing in a worse local optimum — a
reproducible case

Dear Max,

Thank you for the earlier reply — it was genuinely useful, and it changed how we
framed the work. The point that the C-estimator is asymptotically equivalent to ML
under correct specification, so that it functions as a sanity check on modelling
assumptions rather than as a competitor, became the organising idea: we stopped
asking "which estimator is right" and started treating a robust-vs-ML gap as a
diagnostic for observations the single-bivariate-normal model does not describe.
The pointer to Section 8 was also well taken, and we have written the caveat that
both estimators can fail together into our draft.

We have now run `polycor()` and `polycor_mle()` across 1,694 item pairs from 26
ordinal tables in the Item Response Warehouse. Before going further I wanted to
raise something we ran into, since it affects whether our headline number means
anything.

A handful of pairs returned robust estimates near -0.95 where ML gave +0.65 to
+0.80. Restarting from a grid of initial correlations shows these are local optima
with objective values 7x to 28x worse than the solution found from most other
starting values — so they are optimizer behaviour rather than estimates. In two of
the four cases the package's own default initialization is the one that lands
there.

The part that concerns me more is quieter. Even for pairs where every start
converges to a positive rho, the estimates disagree: in one case they range from
0.702 to 0.797 depending only on the starting value. That spread of about 0.095 is
larger than the median |rho_robust - rho_ML| of 0.021 across our whole scan. So we
cannot presently separate the divergence we are trying to measure from
starting-value noise.

The attached note has the details: contingency tables, per-start convergence and
objective values for four cases, and a short self-contained snippet. Because the
fits depend only on the contingency tables, the example reproduces with `robcat`
alone — no access to our data required.

Our questions are at the end of the note, but in short: is multi-start intended
usage, or should `initialize_param()` be reliable enough on its own? If
multi-start is right, is minimum objective the correct selection rule? Is there a
diagnostic you would recommend for flagging a suspect fit automatically? And are
heavily concentrated tables like these — most of the mass in one corner cell —
outside the regime the estimator targets, in which case we would rather exclude
them by a stated rule than fit and discard.

Two other things. We would be glad to take you up on the offer of the SEM code and
results whenever convenient; robustly estimating a polychoric matrix and carrying
it into a factor model is the natural next step for this line on our side. And we
would not publish anything from this scan without sharing it with you first — if
the vignette is useful to you as an application, or if you would rather we held
off until the SEM paper is out, just say.

With thanks,
Ben

---

## Notes before sending

- The note describes `HAQ1b` and `HAQ8a` as "both HAQ disability items" — inferred
  from the table and item names, not from item text. Soften if you would rather
  not assert it.
- The email commits to sharing results before publishing and offers to hold the
  vignette. Both are easy to drop if you would rather keep it a narrow technical
  question.
- Worth attaching the rendered HTML rather than raw markdown if he is not a
  markdown reader.
