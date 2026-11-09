# Non-inferiority test type and margin: procedure

This document describes how the ClinicalTrials.gov pipeline builds three
columns of `data/clinicaltrialsgov.rds`, and how to review the descriptions
the rules cannot settle. Follow it whenever the pipeline reports
non-inferiority descriptions in the review queue, and whenever the rules
change.

- `non_inferior_desc_exists`: whether the analysis has a
  `non_inferiority_description`.
- `non_inferior_type_constructed`: the type of hypothesis the analysis
  tests: `SUPERIORITY`, `NON_INFERIORITY`, `EQUIVALENCE`,
  `NON_INFERIORITY_OR_EQUIVALENCE` or `UNKNOWN`.
- `non_inferior_margin`: the margin or threshold stated in the description, as
  text.

The columns apply to author-reported rows. Raw-derived rows have `NA` in all
three.

Files:

| File | Role |
|---|---|
| `lib/non_inferiority.R` | Rules: type mapping, margin extraction, description classification |
| `process/07_build_author_reported_candidates.R` | Adds the rule-based columns; saves one row per description to `intermediate/non_inferiority_descriptions.rds` |
| `process/09_merge_author_and_raw.R` | Carries the columns into the final file |
| `process/10_integrate_manual_review.R` | Applies recorded decisions; writes the review queue |
| `lib/manual_review/non_inferiority_review.csv` | Recorded decisions (tracked) |
| `data_raw/clinicaltrials.gov/validation/manual_review/non_inferiority_review_queue.csv` | Descriptions waiting for review (not tracked) |
| `review/ni_print_queue.R`, `review/ni_record_decisions.R`, `review/ni_compare_rules.R` | Helper scripts for review and rule changes |

The rules and the first review were developed in
`explore/aact_snapshot/non_inferiority_margins/` (October 2026 snapshot);
its changelog records how they evolved.

## 1. Creating the columns

### 1.1 Description exists

`non_inferior_desc_exists` is `TRUE` when `non_inferiority_description` is not
missing and not blank.

### 1.2 Test type from `non_inferiority_type` (rows without a description)

Rows without a description take the mapping of `non_inferiority_type`:

| `non_inferiority_type` | Standardized type |
|---|---|
| `SUPERIORITY`, `SUPERIORITY_OR_OTHER`, `SUPERIORITY_OR_OTHER_LEGACY`, `OTHER` | `SUPERIORITY` |
| `NON_INFERIORITY` | `NON_INFERIORITY` |
| `EQUIVALENCE` | `EQUIVALENCE` |
| `NON_INFERIORITY_OR_EQUIVALENCE`, `NON_INFERIORITY_OR_EQUIVALENCE_LEGACY` | `NON_INFERIORITY_OR_EQUIVALENCE` |
| missing or anything else | `UNKNOWN` |

`OTHER` maps to `SUPERIORITY` because a sample of described `OTHER` rows were
mostly tests against no difference or estimation only.

### 1.3 Test type from the description (rows with a description)

When a row has a description, the description decides, not
`non_inferiority_type`. Classification is done once per distinct description
(step 07, `ctgov_ni_classify_descriptions()`). The registry escapes some
characters with a backslash (`\<`, `\>`, `\[`); these are removed before
matching. The first rule that applies wins:

1. Mentions both non-inferiority and equivalence:
   `NON_INFERIORITY_OR_EQUIVALENCE`.
2. Mentions non-inferiority ("non-inferior", "noninferior", "non inferior",
   "not inferior"): `NON_INFERIORITY`.
3. Mentions equivalence ("equivalence", "equivalency", "bioequivalen...",
   "two one-sided tests", "TOST"): `EQUIVALENCE`. The word "equivalent" alone
   is not a signal; non-inferiority text often uses it ("equivalently").
4. The extracted margin is a range (ratio range or a range around zero):
   `EQUIVALENCE`.
5. Mentions superiority: `SUPERIORITY`.
6. Has an extracted margin but none of the keywords above: `FROM_TYPE`. The
   row keeps the mapping of its own `non_inferiority_type`, since a stated
   margin agrees with a non-inferiority or equivalence type.
7. Describes a test against no difference: `SUPERIORITY`. Signals include a
   p-value threshold ("p < 0.05"), "statistically significant", "significance
   level", "no (significant) difference", a null hypothesis of equality, no
   difference, no effect or zero, "H0: a = b", "power to detect", "is there a
   difference", "not equal", and named difference tests (t-test, Wilcoxon,
   Mann-Whitney, chi-square, Fisher's exact, log-rank, main or interaction
   effect).
8. Describes a comparison of formulations' pharmacokinetics (an AUC, Cmax,
   pharmacokinetic or bioavailability term together with bioavailability,
   formulation, test versus reference, tablet, capsule, generic or brand):
   `EQUIVALENCE`. Other pharmacokinetic analyses, such as drug interaction
   or organ impairment studies, are not a signal.
9. Otherwise: `NO_SIGNAL`. The description says nothing about the test.

`FROM_TYPE` and `NO_SIGNAL` are resolved per row: `FROM_TYPE` takes the
row's mapping from section 1.2; `NO_SIGNAL` gives `SUPERIORITY` when that
mapping is `SUPERIORITY` and `UNKNOWN` otherwise. One description can appear
with several `non_inferiority_type` values, so these two codes are kept until
the row is known.

### 1.4 Margin from the description

`non_inferior_margin` holds the margin or threshold as stated in the
description, as text (`ctgov_ni_extract_margin()`). Values are not converted
to a common scale. The first rule that finds a value wins:

1. Ratio range: two numbers forming an equivalence range on the ratio scale,
   0.5 to 1 and 1 to 2 (e.g. 0.80-1.25), or on the percent scale with a "%"
   sign (e.g. 80%-125%; the standard 80-125 also counts without "%").
   Written as `0.80-1.25 (ratio)`. The first qualifying pair counts, so
   "AUC0-192" does not hide a later "80% to 125%".
2. Range around zero after a margin word ("margin", "limit", "bound",
   "range", "within"), e.g. "equivalence limits of -1.0 and 1.0". Written as
   `-1.0 to 1.0`. A comma separates the bounds only inside brackets or before
   a "+" sign, so "a margin of -15%, 5% type 1 error" is not a range.
3. Bracketed range symmetric around zero anywhere in the text, e.g.
   "[-5%;5%] margin". Confidence intervals are rarely exactly symmetric.
4. Value before a margin word: "a 4-letter margin", "0.4% noninferiority
   margin", "a 0.10 non-inferiority margin". Written as `margin 0.4%`.
5. Value after a margin word: "non-inferiority margin of -10%", "margin of
   non-inferiority is 0.5", "non-inferiority limit of 0.4%",
   "Δ = 10 points", "δ is the 0.5 mm non-inferiority margin". Δ and δ count
   only when defined (followed by "=", "is", "of", "was" or ":"), not in
   variable names such as ΔQTcF. Written as `margin -10%`.
6. Confidence bound compared with a value: "the lower limit of the 95% CI ...
   greater than -10%", "upper bound ... below or equal to 0.4%", "lower limit
   ... -12% or greater". Written as `lower bound > -10%` or
   `upper bound < 0.4%`. When the text states several bounds, the one stated
   first counts (an inverse non-inferiority test later in the text does not
   win).
7. "within ±X": written as `margin ±X`.
8. Performance goal, objective performance criterion or comparison value:
   written as `threshold X`.

Units are kept when stated (%, percentage points, points, letters, mmHg,
ms, mm, cm, L/min, mL, g/dL, logMAR, log10, days, months, SD, fold, units).

Guards against false matches:

- Numbers that are confidence levels or sidedness ("95% CI", "2-sided") are
  skipped.
- Numbers that define the outcome are skipped: "≥ 0.1 IU/mL", "≥ 1:8",
  "< 50 copies/mL", "≥ 4-fold rise", "≥ 8 ED50", "> 39.0°C", and numbers that
  start a range ("within 0-14 days").
- "Margin of error" is not a margin.
- Words between a margin word and its value may not introduce another
  quantity (p-value, alpha, power, significance, confidence, CI, sided,
  sample, SD). Numbers attached to a word ("IGF-1", "HbA1c", "FOSQ-10") are
  skipped, not read as values.
- Bound phrases stop at ";" and at ". " followed by a capital letter, but
  continue through decimals ("97.5% CI") and abbreviations ("S. pneumoniae",
  "vs.").
- Ratio ranges on the percent scale need a "%" sign (except 80-125), so
  sample sizes such as "between 65 and 124 participants" are not ranges.

### 1.5 Review flags

Each description gets a review reason when the rules cannot settle it
(first that applies):

1. No test keyword and no margin, on a non-inferiority or equivalence type,
   or classified `EQUIVALENCE` only by the pharmacokinetic rule.
2. Both non-inferiority and equivalence mentioned.
3. Superiority mentioned together with non-inferiority or equivalence.
4. Threshold or single-arm test (performance goal, one-sample, single-arm).
5. The description's type disagrees with `non_inferiority_type`.
6. The description is used with several `non_inferiority_type` values.
7. Non-inferiority or equivalence mentioned, a margin word present, but no
   margin extracted.

### 1.6 Applying recorded decisions (step 10)

Step 10 joins the decisions file to the descriptions by `description_id`, the
md5 hash of the description text (`ctgov_ni_description_id()`, which hashes
the text itself, not a serialised R object). A recorded decision replaces
both the rule-based type and the rule-based margin of that description; an
empty `reviewed_margin` means no margin. The row's type is then resolved as
in section 1.3. Descriptions with a review reason and no decision keep the
rule-based values and are written to the review queue. Step 10 reports how
many descriptions have a decision, how many recorded decisions the snapshot
does not use, and how many descriptions (and rows) need review. It checks
that the row count is unchanged, types are valid, raw-derived rows are
empty, and margins appear only with a description. It gives the same result
when rerun.

Decisions are kept even when a snapshot does not use them; a description
that changes by one character gets a new id and returns to the queue.

### 1.7 Reviewing the queue

When step 10 reports descriptions that need review:

1. Print the queue grouped into near-duplicates:
   `Rscript process/clinicaltrials.gov/review/ni_print_queue.R [from] [to] [max_chars]`.
   The queue (`non_inferiority_review_queue.csv`) lists, per description, the
   review reason, the number of rows, the `non_inferiority_type` values, the
   rule-based type and margin, and the text. Groups share their first 70
   letters, ignoring case, digits and punctuation; templated text from one
   sponsor is common.
2. Read every description in full, or at least the part around each number.
   When the members of a group differ in rule type or margin, read each
   member. Decide the type and margin following section 2.
3. Record the decisions with
   `Rscript process/clinicaltrials.gov/review/ni_record_decisions.R decisions.tsv`.
   The TSV has no header and four columns: target (a group such as `g12` from
   the last print, a description id, or an id prefix of at least 8
   characters), type (full name or S, NI, EQ, NE, U, T, N), margin (empty for
   none) and note (short reason, required for anything not obvious). The
   script writes rows of `non_inferiority_review.csv` with
   `description_id`, `reviewed_type`, `reviewed_margin`, `note` and
   `non_inferiority_description` (the text, so the file stays readable and
   can be re-keyed), and replaces any earlier decision for the same
   description.
4. Do not edit the output data directly; decisions are applied only through
   step 10.
5. Rerun step 10 (`process/10_integrate_manual_review.R`) and confirm that
   the queue is empty and the checks pass.
6. If many descriptions share a pattern the rules miss, change the rules
   instead (section 1.8) and review only what remains.
7. Add a dated entry to the changelog with the number of decisions and any
   new convention.

### 1.8 Changing the rules

The rules in `lib/non_inferiority.R` affect every description, so check each
change against the previous output:

1. Before the change, save the current rule output:
   `Rscript process/clinicaltrials.gov/review/ni_compare_rules.R --save`.
2. Change the rules and rerun step 07. Then
   `Rscript process/clinicaltrials.gov/review/ni_compare_rules.R [max_chars]`
   lists every description whose rule type or rule margin changed, was lost
   or is new (also written to `non_inferiority_rule_changes.csv`). Read each
   against its text. During the
   October 2026 work this was about 760 descriptions; it caught overreaching
   rules (CI levels, outcome thresholds, variable names, sample-size ranges,
   sentence-crossing bound phrases) that a sample would have missed.
3. Fix false matches in the rules where a pattern recurs; otherwise record a
   decision for the single description.
4. Reviewed descriptions keep their decisions, so a rule change only affects
   unreviewed ones.
5. Update section 1 of this document to match the rules, and rerun steps 07,
   09 and 10 (or the full pipeline).

### 1.9 Decisions file

| Column | Content |
|---|---|
| `description_id` | md5 of the description text |
| `reviewed_type` | `SUPERIORITY`, `NON_INFERIORITY`, `EQUIVALENCE`, `NON_INFERIORITY_OR_EQUIVALENCE`, `UNKNOWN`, `FROM_TYPE` (keep the row's type mapping) or `NO_SIGNAL` (`SUPERIORITY` on superiority types, `UNKNOWN` otherwise) |
| `reviewed_margin` | Margin as text, empty for none |
| `note` | Reason for the decision |
| `non_inferiority_description` | The text |

`FROM_TYPE` and `NO_SIGNAL` exist because one description can be used with
several `non_inferiority_type` values; prefer an explicit type when the
description settles it.

Margin text uses the forms of section 1.4: `margin X`, `lower bound > X`,
`upper bound < X`, `a to b`, `lo-hi (ratio)`, `threshold X`. When a
description states several tests, prefix each value with its test and
separate them with ";", e.g. `NI -15%; superiority 0`. A measure may be named
when the text gives it (`margin 1.30 (HR)`, `margin 0.5 (Cohen's d)`).
Decisions from the first review round use some freer labels (`NI margin 5%`,
`HR 1.3`, `performance goal ...`); they are valid as stated.

As of 30 October 2026 the file holds 2,032 decisions covering 4,153 rows of
the 13 October 2026 snapshot: 913 `SUPERIORITY`, 226 `NON_INFERIORITY`, 230
`EQUIVALENCE`, 2 `NON_INFERIORITY_OR_EQUIVALENCE`, 113 `UNKNOWN`, 37
`FROM_TYPE` and 511 `NO_SIGNAL`.

## 2. Choices made in manual classification

These are the conventions followed in the October 2026 review. Apply them
the same way in later reviews; record a new convention in this section when
a case is not covered.

### 2.1 General principles

- The description decides the type, not `non_inferiority_type`. The type is
  only used when the description defers to it (`FROM_TYPE`) or says nothing
  about the test (`NO_SIGNAL`).
- The question is what the reported p-value or interval tests: a difference
  from zero (`SUPERIORITY`), a one-sided margin (`NON_INFERIORITY`), a
  two-sided margin (`EQUIVALENCE`), or both
  (`NON_INFERIORITY_OR_EQUIVALENCE`).
- `UNKNOWN` when the description does not let you tell which test the
  reported result belongs to. Do not guess from the trial design.
- Margins are recorded as stated, not converted or normalised. Only plainly
  wrong signs and directions are corrected (section 2.4).
- Every non-obvious decision gets a short note.

### 2.2 Test type

**Tests against no difference: `SUPERIORITY`.** This includes:

- Named difference tests: t-test, paired t-test, Wilcoxon, Mann-Whitney,
  Kruskal-Wallis, Friedman, chi-square, Fisher's exact, McNemar, log-rank,
  Cox model tests of HR = 1, F-tests.
- Null hypotheses of equality, no difference, no effect, no change, or a
  value of zero (risk ratio equal to 1, log odds equal to 0), including
  within-group tests of change from baseline and one-sample tests.
- Wording that says "equivalent" but describes a difference test: "p < 0.05
  required to be considered nonequivalent", "considered equivalent if p >
  0.05", "the null hypothesis was that the treatments were equivalent ... to
  be rejected in favour of a difference", "groups not significantly different
  at p < 0.05". The reported p-value tests a difference, whatever the label.
- Power calculations to detect a difference, an effect size or a reduction.
- Descriptions that only state a two-sided p-value for a comparison ("2-sided
  P-value in the analysis"), and comparisons of interventions by ANOVA or
  regression when the text says what is compared ("ANOVA to compare the
  three interventions", "regression to examine intervention effects").
- Comparisons in observational or baseline settings ("no difference at
  baseline expected"; "baseline check, not the test").
- Superiority steps whose margin is zero or positive: superiority by a
  margin keeps `SUPERIORITY` with the margin (e.g. "lower bound > 10
  percentage points", `margin 0.35`).

**`NON_INFERIORITY`.** Explicit non-inferiority, "not inferior", "at least as
effective", "does not worsen more", a null hypothesis of inferiority, a
one-sided margin on the unfavourable side, or a non-inferiority method
(Farrington-Manning). A test called "superiority" whose bound is negative
("superiority if the lower limit is greater than -5 points") is
non-inferiority by its margin. Single-arm comparisons with a historical rate
that the text calls non-inferiority keep `NON_INFERIORITY`.

**`EQUIVALENCE`.** Explicit equivalence, bioequivalence, two one-sided tests,
"similarity" with a two-sided margin, "no better and no worse", lot-to-lot
consistency (CI of the ratio within a range), "no effect range" for an
interaction, and concordance used to show bioequivalence. A non-inferiority
step followed by a non-superiority step on the other side is equivalence
(`-15% to +35.5%`).

**`NON_INFERIORITY_OR_EQUIVALENCE`.** Only when the description itself uses
both for the same test (e.g. "Non-inferiority margin of (-2, 2) ... equivalence
margin of (-2, 2)"). This is rare (2 decisions).

**Hierarchical and multi-test descriptions.** When a description describes
several tests (non-inferiority then superiority, equivalence then
non-inferiority), use the test the reported p-value belongs to when this is
stated, including through `non_inferiority_type` ("superiority step; type
says SUPERIORITY", "NI step of hierarchy", "first step of hierarchy").
Otherwise `UNKNOWN`, recording all margins with their tests
(`NI -15%; superiority 0`). Also `UNKNOWN`: descriptions that present both
non-inferiority and superiority p-values; non-inferiority primary objective
with superiority in the power calculation; a generic note that mentions
non-inferiority alongside two-sided tests; a superiority power calculation
that mentions a non-inferiority margin; mixed shifted and unshifted
hypotheses; garbled hypotheses; and within-group changes where
non-inferiority or equivalence is mentioned without a margin.

**Threshold and performance-goal tests.** Single-arm tests against a fixed
value (performance goal, objective performance criterion, historical rate,
benchmark, success threshold, vaccine efficacy above a value, kappa against
0.90) are classified by their stated direction, usually `SUPERIORITY`, with
the value as `threshold X` or as the stated bound. An assay sensitivity test
(e.g. moxifloxacin versus placebo lower bound ≥ 5 ms) is `SUPERIORITY` with
its bound. A one-sided test against a value whose direction is not stated is
`UNKNOWN` with the value (`margin 0.35`).

**Pharmacokinetic analyses.**

- Comparisons of formulations (test versus reference, tablet versus capsule,
  fixed-dose combination versus free combination, generic versus brand,
  relative bioavailability), and bioequivalence power statements:
  `EQUIVALENCE`, with the acceptance range when stated.
- Interaction, organ impairment, food effect, absolute bioavailability (oral
  versus IV), dose proportionality and exposure-response analyses: not an
  equivalence test by default. `UNKNOWN` (or `NO_SIGNAL`) unless the text
  states the test; an interaction study with a null of equal exposure is
  `SUPERIORITY`.
- "No formal testing" or "exploratory" relative bioavailability: `UNKNOWN`.
- An ANOVA or mixed model on log AUC and Cmax without a stated comparison or
  range: `NO_SIGNAL`.
- When the description pairs a 90% CI of the test/reference ratio with a
  separate difference test (e.g. Wilcoxon for Tmax), `UNKNOWN`.

**Descriptions that say nothing about the test: `NO_SIGNAL`.** Model
specifications without a comparison ("Model includes effects for sequence,
period and treatment"), significance levels or power alone ("alpha = 0.05",
"80% power"), labels (a serotype, strain, antigen, time point), references to
other text ("see above", "as previously described"), statements that no power
calculation was done, and textbook definitions of trial types.

**Equivalence types without a margin.** A description on an equivalence type
that says equivalence was tested but gives no margin ("No defined equivalence
margin", "bioequivalence bounds not stated") stays `EQUIVALENCE` with no
margin. An observational description that says there is "no true equivalence
margin" and tests a difference is `SUPERIORITY`.

### 2.3 Margins

**What counts as a margin.** The value that the confidence bound or test
statistic is compared with: a non-inferiority margin, equivalence limits, an
acceptance range, a performance goal or a superiority margin. Not a margin:
confidence levels, significance levels, power, effect sizes assumed in a
power calculation, minimal clinically important differences used only for
power, outcome definitions (seroprotection cut-offs, ≥ 4-fold rise), time
windows, citation page numbers, "margin of error", and "95% margin" (likely
the CI level).

**Form.** As stated, with the unit and measure when given. Bounds keep the
direction of the comparison (`lower bound > -10%`, `upper bound < 1.5`).
Fractions stay fractions (`lower bound > 1/2`, `lower bound > 1/1.5`).
Non-numeric definitions are not recorded as values: "the margin is the
standard error", "half of the placebo effect", "the median of the fentanyl
group + 30", "M1" without a value; the note says so.

**Several values.** Record the margin of the primary test. When a description
restates the margin in another unit, keep the primary one ("non-inferiority
margin of +1.6 units corresponds to an 8% margin": `margin +1.6 units`). When
it gives a primary margin and a later, stricter one, keep the primary one
(`margin 20%` when 20% is primary and 12% secondary). Hierarchical
descriptions keep all values with their test labels (section 1.9).

**Sources other than a test criterion.** Precision targets in sample-size
text ("the 95% CI will stay within ± 45 PRU (non-inferiority margin)") are
recorded when the text calls them the margin. A null hypothesis value
("H0: HR ≥ 1.33", "H0: difference ≥ 3 mmHg") is the margin
(`margin 1.33`, `margin 3 mmHg`). "Exclude a two-fold risk" is
`margin 2 (two-fold risk)`. A description consisting only of a value
("-12.5%") is that margin.

**Not stated.** When the description mentions a margin without its value
("the margin was selected based on literature", "δ" undefined, text
truncated before the value), record the type with an empty margin and a note.

### 2.4 Corrections

Margins are kept as written except where the text plainly means something
else. Corrected so far (9 descriptions):

- "Excluding a decrease ≥ X" read as a positive lower bound: recorded as
  `lower bound > -X` (e.g. -5 and -10 percentage points).
- A non-inferiority test whose text gives a positive lower bound that can only
  mean a negative one: "greater (closer to zero) than 15%" (`-15%`), "≥ 7
  percentage points" with "at least as effective" (`-7 percentage points`),
  "greater than 12%" for a non-inferiority difference (`-12%`), and a text that
  states both "≥ 10%" and "≥ -0.10" (`-0.10`).
- "Excluding a decrease of 1.5-fold" on the ratio scale: `lower bound > 1/1.5`.
- Typos: "> 05" recorded as `lower bound > 0.5`. A typo whose intended value
  is unclear is kept as stated with a note ("Δ = -0.0.068").

Not corrected: positive lower bounds that are correct, such as single-arm
success rates, ratio bounds (0.5, 2/3), vaccine efficacy above a value, and
superiority by a margin; and texts whose sign convention differs but whose
value is clear ("margin 10%" for a 10% non-inferiority margin).
