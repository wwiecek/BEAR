# SCORE Processing

This note documents how the raw SCORE replication package under
`data_raw/SCORE` is processed into the BEAR-ready datasets used by the build.

Run the two canonical workflows from the BEAR project root:

```r
source("process/score_all_claims.R")
source("process/score_replications.R")
```

Both scripts read:

```text
data_raw/SCORE/replication/analyst data.RData
```

The main raw objects are `claims_non_significant_bushels`, `orig_outcomes`,
`repli_outcomes`, and `paper_metadata`. The replication package is already a
curated analyst database: original and replication claims have been matched,
converted effect sizes are supplied where possible, version-of-record
replications are identified, and manuscript replication-success indicators are
included.

## Canonical Outputs

The scripts write two durable BEAR-ready files:

```text
data/SCORE_all_claims.rds
data/SCORE_replications.rds
```

`SCORE_all_claims.rds` contains 3,066 rows, one selected claim-text statistic
per `claim4_id`.

`SCORE_replications.rds` contains 548 rows: 274 original rows and 274 matched
replication rows from the non-COVID version-of-record subset.

Both outputs begin with the BEAR-facing fields:

```text
dataset, metaid, studyid, estimate_id, paper_id, claim_id, report_id,
citation, journal, discipline, year, source, subset, measure, z, abs_z,
z_operator, p, b, se, ss, weights, significant
```

For both SCORE datasets, `discipline` is copied from
`paper_metadata$COS_pub_category`, `studyid` is the paper DOI, and `metaid` is
left missing. The build reads these files in `workflow/build_bear.R` and keeps
`source` and `subset` as SCORE-specific grouping fields.

## All-Claims Dataset

`process/score_all_claims.R` starts from the 3,066-row
`claims_non_significant_bushels` table. It splits `coded_stat_evidence` on
semicolons, parses each statistical fragment, and selects one fragment per
`claim4_id`.

Each fragment is parsed with this statistic priority:

1. Reported `z`.
2. Reported `t`, with or without degrees of freedom.
3. Coefficient or effect divided by `SE`.
4. `F(1, df)` transformed to `sqrt(F)`, using the best available sign hint.
5. 95% confidence interval plus a point estimate.
6. Two-sided p-value transformed as `qnorm(1 - p / 2)`.

The parser keeps provenance fields in the in-memory working table, including
`z_source`, `effect_source`, `se_source`, `sign_source`, and `parse_status`.
Reported effects are heterogeneous and are not harmonized to a common
effect-size scale.

For claim-level selection, each parsed fragment receives a
`fragment_significant` flag. P-values are used first: exact `p` values are
significant when `p <= 0.05`; `p < x` is significant only when `x <= 0.05`;
`p > x` is non-significant only when `x >= 0.05`; other p-value inequalities
are ambiguous. If no p-value is available, exact `z` is significant when
`abs(z) >= 1.96`; lower and upper bounds are used only when the bound itself
settles the decision.

The SCORE claim coding is represented as:

```r
selected_significant = nonsig != "T"
```

Within each claim, fragments are ranked as follows:

1. Prefer fragments whose significance decision agrees with
   `selected_significant`.
2. Prefer stronger statistic provenance: `reported_z`, `reported_t`,
   `coef_over_se`, `sqrt_f_df1_eq_1`, `ci_95`, `two_sided_p`, then
   `unparsed`.
3. Prefer exact statistics over bounded inequalities.
4. Prefer fragments with an extracted effect size `b`.
5. Prefer fragments with an extracted standard error `se`.
6. Use the earliest fragment as the final tie-breaker.

The selected row receives one of three `claim_selection_status` values:

- `aligned_with_score_code`: the selected fragment has a significance decision
  and agrees with the SCORE claim-level coding.
- `parsed_mismatch`: the selected fragment has a significance decision but no
  parsed fragment in the claim agrees with the SCORE coding.
- `unparsed`: no fragment in the claim has a non-ambiguous significance
  decision.

Expected validation checks include 3,066 raw claim rows, more fragment rows
than raw claim rows, 3,066 claim-level output rows, unique `claim4_id` values,
no missing `discipline`, `z_operator` limited to `=`, `<`, `>`, or missing, and
a selected-statistic significance rate within 2 percentage points of the SCORE
`nonsig != "T"` rate.

## Matched Replication Dataset

`process/score_replications.R` reads `orig_outcomes`, `repli_outcomes`, and
`paper_metadata`. It keeps only non-COVID version-of-record replication reports
and their matched original claims.

For both original and replication rows, `z` is derived with this hierarchy:

1. If a coefficient and standard error are available, use `coef / se`.
2. Else if a reported `z` or `t` statistic is available, use it directly.
3. Else if an `F` statistic is available with numerator df equal to 1, use
   `sign_hint * sqrt(F)`.
4. Else if a p-value and sign hint are available, derive `z` from a two-sided
   p-value.

The sign hint is taken from the best available direction information:
coefficient sign, effect-size sign, converted-`r` sign, or reported-statistic
sign. Very large or infinite z-values are truncated to `[-20, 20]`.

The canonical replication output uses the replication package's converted
correlation scale where available:

- `b` is `orig_conv_r` or `repli_conv_r`.
- `se` is implied from converted-`r` confidence bounds when possible, otherwise
  from `abs(b / z)`.
- `measure = "r"`.

Expected validation checks include 825 raw original rows, 427 raw replication
rows, 274 non-COVID version-of-record replication rows, 274 matched original
rows, 548 canonical long rows, SCORE success rate equal to `151 / 274`,
replication p-value significance rate matching the raw package, no missing
`discipline`, and weights summing to the number of distinct papers represented.

## Interpretation Notes

The all-claims table is text-derived. Its reported effects can include
coefficients, correlations, standardized effects, beta coefficients, and
partial eta-squared values. Treat `b` and `se` as auditable provenance fields,
not as a harmonized effect-size scale.

The matched replication table is more comparable because it uses SCORE's
converted correlation scale where available, but it still reflects the SCORE
package's conversion rules rather than a fresh BEAR-side harmonization.

The old `nosek_replicate_bear/output` files were generated audit and staging
artifacts, not required inputs. The durable processing record is now this file,
the two `process/score_*.R` scripts, `process/score_helpers.R`, the raw SCORE
package, and the two canonical `.rds` outputs under `data/`.
