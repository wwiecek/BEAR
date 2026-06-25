# ClinicalTrials.gov Result Derivation Notes

These notes document the canonical AACT ClinicalTrials.gov workflow for BEAR.
The workflow builds author-reported effects and raw-input-derived effects
separately, validates their overlaps, and merges them only at the end. The full
auditable merge is written under `data_raw/clinicaltrials.gov/derived/`. The
public `data/clinicaltrialsgov.rds` keeps author-reported and raw-derived rows
separate, marks rows used by the main BEAR build with `include_in_bear`, adds
selected raw-only effects where no author-reported effect is available for the
same outcome and result-group pair, and keeps completed studies in the public
analytic file. The main BEAR build separately excludes studies with 20 or more
candidate effect rows.

## Source Snapshot

The local AACT snapshot is:

`data_raw/clinicaltrials.gov/12082025/a1hlrix83cqvhi5qn0jya3f8mdik.zip`

Scripts assume they are run from the BEAR project root.

## Script Layout

Run the canonical pipeline with:

```sh
Rscript --vanilla process/clinicaltrials.gov/run_pipeline.R
```

The runner executes:

1. `process/01_prepare_aact_extracts.R`
2. `process/07_build_author_reported_candidates.R`
3. `process/02_derive_endpoint_effects.R`
4. `validate/01_validate_endpoint_effects.R`
5. `process/03_classify_endpoint_source_rows.R`
6. `process/04_extract_prepost_side_tables.R`
7. `validate/02_validate_prepost_side_tables.R`
8. `validate/03_validate_trial_coverage.R`
9. `validate/04_validate_author_overlap.R`
10. `process/05_build_trial_characteristics.R`
11. `process/06_build_endpoint_candidates.R`
12. `process/08_select_raw_endpoint_scale.R`
13. `process/09_merge_author_and_raw.R`
14. `validate/05_validate_final_merge.R`

`process/clinicaltrials.gov/lib/paths.R` defines the shared source and output
roots. The domain mapping used by trial-characteristics construction is
`process/clinicaltrials.gov/lib/domain_mesh_map.csv`.

## Output Roots

Durable datasets are written to `data_raw/clinicaltrials.gov/derived/`.
Reusable reduced extracts are written to
`data_raw/clinicaltrials.gov/derived/intermediate/`. Validation summaries,
diagnostics, and audit tables are written to
`data_raw/clinicaltrials.gov/validation/`. Sampled manual-review queues are
written to `data_raw/clinicaltrials.gov/validation/manual_review/`.

`data_raw/clinicaltrials.gov/derived/legacy_analysis_output/` is legacy output
from the earlier script layout. Once the canonical pipeline has been verified
on a checkout, that directory is disposable; the canonical scripts do not write
new outputs there.

## Row Units And Keys

`derived/intermediate/outcome_measurements_long.rds` has one row per reduced
AACT outcome-measurement row after source-family screening and joins to study,
outcome, result-group, and count metadata. Main join keys are `nct_id`,
`outcome_id`, `result_group_id`, and measurement stratum fields.

`derived/endpoint_source_rows.rds` has one row per eligible endpoint source row.
It is the source-row audit table used by endpoint derivation.

`derived/raw_endpoint_effect_pairs.rds` has one row per derived statistic for
all eligible treatment-control pairs, including diagnostic all-pair rows.

`derived/raw_endpoint_effects.rds` has one row per prioritized derived endpoint
statistic. It preserves multiple effect scales rather than choosing a single
BEAR-facing estimand.

`derived/endpoint_source_rows_classified.rds` preserves the source-row audit
table and adds final audit classes for rows that remain outside endpoint
derivation.

`derived/prepost_measurement_pairs.rds` has one row per group-level pre/post
pair. `derived/prepost_contrast_candidates.rds` has one row per diagnostic
difference-in-change contrast candidate. These are side tables only.

`validation/trial_coverage_audit.csv` has one row per trial in the workflow
audit universe. This includes the strict primary denominator plus trials present
in old BEAR ClinicalTrials.gov rows, author-analysis links, raw endpoint
effects, pre/post side tables, or unknown-row audits.

`derived/trial_characteristics.rds` has one row per `nct_id` in the same
workflow-union trial universe. `trial_domains_long.rds` and
`trial_interventions_long.rds` are long side tables keyed by `nct_id`.

`derived/endpoint_candidates.rds` has one row per prioritized raw endpoint
effect with validation metadata attached. It remains the all-scale raw
candidate artifact.

`derived/author_reported_raw.rds` has one row per reduced author-reported
AACT `outcome_analyses` row joined to trial and outcome metadata. It is reused
by default. Regeneration should be requested explicitly because it requires
returning to the source AACT tables.

`derived/author_reported_candidates.rds` has one row per author-reported
completed primary-outcome row with recoverable `z`. It preserves `outcome_id`,
`outcome_analysis_id`, linked result-group metadata where available,
`measure_class`, `b`, `se`, `z`, `z_operator`, and source metadata.

`derived/raw_endpoint_selected_for_bear.rds` has one selected raw-derived row
per eligible raw contrast. Continuous rows use standardized mean difference
when calculable. Binary rows use a probit-scale difference when calculable.
`derived/raw_endpoint_nonselected_scales.rds` retains non-selected raw scales
for validation and later sensitivity analyses.

`derived/clinicaltrialsgov_merged_candidates.rds` is the auditable final merge
candidate before public trimming. `data/clinicaltrialsgov.rds` is the public
BEAR-facing copy after retaining author and selected raw rows separately,
marking `include_in_bear`, and keeping completed studies. The main BEAR build
applies the fewer-than-20 candidate-row rule downstream.

## Endpoint Source Classes And Formulas

The endpoint workflow uses arm-level AACT outcome measurements from primary
outcomes in completed randomized parallel interventional trials. It retains
continuous rows with means plus SD, SE, or confidence intervals; least-squares
mean rows with SE or confidence intervals; and conservative binary rows from
explicit participant counts, complement pairs, participant-number rows, or
percent/proportion rows.

Continuous mean-difference rows use:

`b = mean_t - mean_c`

`se = sqrt(se_t^2 + se_c^2)`

`z = b / se`

Continuous standardized mean differences are computed only for mean plus SD
source rows, using the pooled SD and Hedges correction.

Binary rows produce risk difference, log risk ratio, and log odds ratio when
the relevant cells and standard errors are valid. Boundary cells are flagged
and excluded from log-ratio estimands when the formula is undefined.

For BEAR import, raw-only binary rows currently use a probit difference,
`qnorm(p_t) - qnorm(p_c)`, with a delta-method standard error. This puts binary
effects on a latent-normal scale that is more comparable to standardized mean
differences than log odds ratios, log risk ratios, or risk differences. The
tradeoff is weaker direct interpretability, dependence on event rates, and
loss of rows with boundary event proportions unless a later continuity policy
is chosen.

Arm orientation is inferred from ClinicalTrials.gov group prefixes, comparator
keywords, and exact author-analysis group links. When direction is not
recoverable, the row is retained with explicit warning flags rather than
dropped solely for direction ambiguity.

## Pre/Post Side Tables

Pre/post side tables are diagnostic and sensitivity-oriented. They do not alter
endpoint-effect derivation. Internal outcome-measurement pairs are matched
within the same `nct_id`, `outcome_id`, and `result_group_id`. Baseline-table
matches require exact group title/description matching. Reported change rows
are used for overlap diagnostics, not as pre/post pair inputs.

## Trial Coverage

The primary denominator in `validate/03_validate_trial_coverage.R` is completed
randomized parallel interventional trials with at least one primary outcome in
the local AACT snapshot.

The verified current counts are:

- 250,444 prioritized endpoint candidate rows.
- 41,608 rows in `validation/trial_coverage_audit.csv`.
- 31,076 trials in the primary denominator.
- 9,415 overlapping trials between old BEAR ClinicalTrials.gov rows and raw
  endpoint effects.
- 41,608 rows in `derived/trial_characteristics.rds`, one per workflow-union
  `nct_id`.

Left joins from `data/clinicaltrialsgov.rds` and
`derived/endpoint_candidates.rds` to `derived/trial_characteristics.rds`
preserve their original row counts.

The final auditable author/raw candidate merge and public analytic file
currently have 151,313 rows from completed studies. The main BEAR build
excludes studies with 20 or more candidate effect rows, which removes 1,046
studies and 69,968 rows in the current snapshot, leaving 81,345 rows from
26,400 studies for the BEAR ClinicalTrials.gov component. A looser rule allowing
up to 99 candidate rows would retain 116,546 rows. The excluded rows are mostly
from studies where a primary outcome is recorded as many submeasurements, such
as laboratory panels, symptom panels, or repeated time-specific measurements.

## Memory-Safety Rules

Large AACT tables should be streamed or reduced before joining. Keep IDs as
character in reduced extracts. Do not retain full raw chunks after filtering;
drop large objects with `rm()` where useful and call `gc()` between heavy
pipeline stages. Reuse `derived/intermediate/` extracts when the source
snapshot and extraction rules have not changed.

Do not materialize both legacy `derived/legacy_analysis_output/` artifacts and
canonical derived artifacts in memory in the same script. The legacy directory
is for comparison or disposal only.

Reuse existing author and raw artifacts unless a missing artifact or an
explicit request requires regeneration. Consult the maintainer before any step
likely to exceed five minutes, including reopening large AACT flat files or
rerunning the full endpoint extraction.

## Validation Outputs

Endpoint validation writes `validation/endpoint_effects_qc.csv` and checks
required columns, unique effect IDs, recomputed `z`, primary-outcome status,
valid standard errors and sample sizes, source classifications, binary count
ranges, and absence of safety totals.

Pre/post validation writes `validation/prepost_qc.csv` and checks pair IDs,
source tiers, time roles, contrast IDs, endpoint-row preservation, and
source-row preservation.

Coverage validation writes `validation/trial_coverage_audit.csv`,
`validation/trial_coverage_summary.csv`, and manual-review queues for trials
without raw endpoint effects or with unknown-only rows.

Author-overlap validation writes
`validation/author_overlap_diagnostic_summary.csv`,
`validation/author_overlap_disagreement_examples.csv`, and
`validation/manual_review/author_overlap.csv`. Final validation also writes
`validation/author_raw_scale_aware_summary.csv` and
`validation/manual_review/author_raw_scale_aware_disagreements.csv`, which
separate genuinely comparable author/raw scales from non-comparable pairs
before summarising disagreements.

Raw scale selection writes `validation/raw_scale_comparison_summary.csv`. It
summarises how often the selected scale changes sign, 0.05-significance side,
and absolute-z magnitude relative to available alternative scales. Continuous
comparisons focus on standardized mean difference versus mean difference.
Binary comparisons focus on probit versus log odds ratio, log risk ratio, and
risk difference where those scales are calculable.

The final merge writes `validation/clinicaltrialsgov_merge_summary.csv`,
`validation/clinicaltrialsgov_merge_checks.csv`, and
`validation/clinicaltrialsgov_row_count_cutoff_impact.csv`. Integrity checks
require no duplicate author keys within `nct_id + outcome_analysis_id`, no
duplicate raw `effect_id`, exact final-row reconciliation, preservation of all
author rows in the audit artifact, explicit author-preferred reasons for
raw overlaps not used by BEAR, the exclusion of studies with 20 or more effect
rows in the main BEAR surface, completed-only public rows, and the presence of
required BEAR fields plus raw-input fields.

## Merge Policy

Author-reported effects are the default source for linked overlaps. Raw overlap
is currently defined by `nct_id`, `outcome_id`, and the linked result-group
pair when that pair is available. Raw rows that overlap an author-reported row
are retained as public rows but marked `include_in_bear = FALSE` with
`import_decision = "exclude_author_preferred_overlap"`. The shared
`author_raw_overlap_key` links author and raw rows with the same `nct_id`,
`outcome_id`, and result-group pair. This link is not proof that the
author-reported effect was calculated from a particular raw measurement row:
ClinicalTrials.gov can store multiple measurement strata, categories, or
timepoints under the same outcome and groups. Selected raw rows with no
author-reported overlap are added as raw-derived effect rows and marked
`include_in_bear = TRUE`.

The merged file retains raw calculation inputs where available:

- treatment and control event counts: `raw_event_t`, `raw_event_c`;
- treatment and control sample sizes: `raw_n_t`, `raw_n_c`;
- treatment and control means: `raw_mean_t`, `raw_mean_c`;
- treatment and control SDs: `raw_sd_t`, `raw_sd_c`;
- treatment and control result-group IDs and labels.

Pre/post treatment and control means and SDs are not part of the current
endpoint import. They remain in the separate pre/post side artifacts until a
pre/post effect policy is chosen.

## Settled Merge Policies

Direction-unknown, author-selected-pair, and shared-comparator raw rows remain
in the public file with explicit flags. Binary raw-only rows use probit
difference for the BEAR-facing scale; raw event counts and denominators are
retained so users can recalculate alternative binary scales.
