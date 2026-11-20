# ClinicalTrials.gov derivation notes

The [runner](run_pipeline.R) defines execution order. See the
[README](README.md) for the command and [changelog](changelog.md) for
snapshot history and dated counts.

## Eligibility and row units

The author-reported path uses analyses of primary outcomes in completed
studies where a z-value can be recovered. The raw endpoint path uses
arm-level measurements for primary outcomes in completed, randomised,
parallel-group interventional trials. This latter population is also the
denominator for the primary coverage audit, including trials with a primary
outcome and no usable arm-level measurement.

Paths in the table are relative to `data_raw/clinicaltrials.gov/` unless
shown from the repository root.

| File | One row per |
|---|---|
| `derived/intermediate/outcome_measurements_long.rds` | Reduced AACT measurement, joined to study, outcome, result group and counts |
| `derived/endpoint_source_rows.rds`, `derived/endpoint_source_rows_classified.rds` | Endpoint source measurement. The classified file adds audit classes for unused rows |
| `derived/raw_endpoint_effect_pairs.rds` | Statistic for an eligible arm pair, including diagnostic pairs |
| `derived/raw_endpoint_effects.rds`, `derived/endpoint_candidates.rds` | Prioritised endpoint statistic, with multiple scales retained. The latter adds validation metadata |
| `derived/raw_endpoint_selected_for_bear.rds` | Selected statistic per eligible contrast |
| `derived/author_reported_raw.rds`, `derived/author_reported_candidates.rds` | Source analysis in the first file and eligible analysis in the second, keyed by trial and analysis ID |
| `derived/intermediate/non_inferiority_descriptions.rds` | Distinct non-inferiority description |
| `derived/prepost_measurement_pairs.rds`, `derived/prepost_contrast_candidates.rds` | Group-level pre/post pair and diagnostic difference-in-change contrast, respectively |
| `validation/trial_coverage_audit.csv`, `derived/trial_characteristics.rds` | Trial in the workflow, including the coverage denominator and trials represented in other inputs or audits |
| `derived/clinicaltrialsgov_merged_candidates.rds`, repository `data/clinicaltrialsgov.rds` | Author-reported analysis or selected raw contrast. The first file retains additional audit fields |

The measurement joins use `nct_id`, `outcome_id`, `result_group_id` and
measurement stratum fields. Step 07 requires the author analysis extract
from step 00. The older August 2025 intermediate lacks the non-inferiority
fields.

## Raw endpoint derivation

Continuous estimates use means or least-squares means with suitable SDs,
SEs or confidence intervals. For a mean difference, the estimate is
$\bar y_t - \bar y_c$ and the SE is $\sqrt{SE_t^2 + SE_c^2}$. The z-value
is the estimate divided by its SE. When arm means and SDs permit it, the
selected statistic is the standardised mean difference with Hedges'
correction to the pooled-SD estimate.

Binary inputs come from participant counts, complementary categories,
participant-number rows or usable event proportions. The pipeline computes
risk differences, log risk ratios and log odds ratios when defined. For
import it uses a probit difference, $\Phi^{-1}(p_t) - \Phi^{-1}(p_c)$, with
a delta-method SE. This expresses the result on a latent-normal scale.
Its interpretation depends on the event rates, and it is undefined at
boundary proportions. Event counts and denominators remain available for
alternative calculations.

Arm orientation uses group prefixes, comparator labels and author analysis
links. Rows with ambiguous direction or dependent contrasts can remain in
the public file with flags. Pre/post pairs are diagnostic. Internal pairs
match trial, outcome and result group. Baseline-table pairs require exact
group title and description matches. Neither contributes effects to the
endpoint merge.

## Author-reported effects

The shared [derivation helper](../../R/z_derivation_helpers.R) chooses
between a z-value reconstructed from a sufficiently symmetric and otherwise plausible
Wald interval and one reconstructed from a reported p-value. Inspect
`z_source` in the author candidate table to see which was used.

The derivation recognises percentage units in `param_type`, for example
`Ratio (%)`, `Ratio x 100` and `Difference in percentages`. A percentage
in a confidence level or in a component of a ratio does not specify the
units of the estimate. Ratios reported as percentages are divided by 100
before taking logs, so 100 maps to the null. For additive effects, the
estimate and SE are divided by 100 after deriving the z-value. This
preserves the CI symmetry decision. Already logged estimates retain their
units. The source estimate and bounds remain available, and
`percent_rescaled` records conversion in the author candidate table.
The rule cannot identify unlabelled percentage units.

The [non-inferiority fields](non_inferiority_review_procedure.md) record
the test type and any stated margin. Step 10 applies the manual decisions
after deriving effects. A z-value reconstructed from a non-inferiority or
equivalence p-value can therefore reflect a margin-based test. It cannot
generally be interpreted as evidence against no effect.

## Merge and interpretation

An overlap requires the same trial, outcome and linked result-group pair.
The public file retains both sources and sets `include_in_bear = FALSE`
for the overlapping raw row. The shared `result_id` identifies the linked
comparison. Several measurements or time points can share that ID, so
it does not identify the specific measurement used by the authors.
The main BEAR build further restricts imports to randomised interventional
studies with fewer than 20 candidate effect rows per study.

`measure_class` gives the effect family and `scale` gives the units used
for `b` and `se`. The raw standardised mean difference is Hedges' g.
The exact estimator often cannot be identified for author-reported SMDs.
For raw effects, the public file keeps arm sample sizes, counts, means,
SDs and result-group IDs and labels where available. The flag
`raw_multi_arm_trial` refers to eligible groups for the outcome, while
`is_multi_arm` refers to the study.

The overlap diagnostic groups author and raw results by outcome, arm pair
and, in its scale-aware summary, measure labels. Its CI-based author
statistic uses the reported estimate and bounds on their original scale,
which can use the wrong null for a ratio. The comparison is diagnostic
and does not establish agreement on the estimand or null hypothesis.

## Validation records

Under `data_raw/clinicaltrials.gov/validation/`, use
`snapshot_counts.csv` for counts by snapshot,
`clinicaltrialsgov_merge_summary.csv` for source and inclusion counts, and
`clinicaltrialsgov_merge_checks.csv` for final checks. The cutoff impact
table shows the effect of excluding studies at different candidate row
counts. Endpoint and pre/post QC, coverage, overlap and raw-scale
diagnostics are written by their corresponding runner stages.
