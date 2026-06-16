# ClinicalTrials.gov Raw Result Derivation Notes

These notes document the canonical AACT ClinicalTrials.gov side-data workflow.
The workflow builds auditable endpoint and trial-level artifacts for later BEAR
integration, but it does not modify `data/clinicaltrialsgov.rds`, `BEAR.rds`,
paper outputs, or site outputs.

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
2. `process/02_derive_endpoint_effects.R`
3. `validate/01_validate_endpoint_effects.R`
4. `process/03_classify_endpoint_source_rows.R`
5. `process/04_extract_prepost_side_tables.R`
6. `validate/02_validate_prepost_side_tables.R`
7. `validate/03_validate_trial_coverage.R`
8. `validate/04_validate_author_overlap.R`
9. `process/05_build_trial_characteristics.R`
10. `process/06_build_endpoint_candidates.R`

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
effect with validation metadata attached. It is the clean candidate artifact
for later merge decisions, not a BEAR import.

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

## Memory-Safety Rules

Large AACT tables should be streamed or reduced before joining. Keep IDs as
character in reduced extracts. Do not retain full raw chunks after filtering;
drop large objects with `rm()` where useful and call `gc()` between heavy
pipeline stages. Reuse `derived/intermediate/` extracts when the source
snapshot and extraction rules have not changed.

Do not materialize both legacy `derived/legacy_analysis_output/` artifacts and
canonical derived artifacts in memory in the same script. The legacy directory
is for comparison or disposal only.

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
`validation/manual_review/author_overlap.csv`.

## Future Author-Reported Endpoint Spec

The existing BEAR-facing author-reported workflow in
`process/clinicaltrials_prepare_data.R` and
`process/clinicaltrials_process_data.R` should be migrated into this side-data
layout before any combined ClinicalTrials.gov import is attempted.

The migrated author-reported endpoint artifact should:

- Preserve `outcome_id` and `outcome_analysis_id`.
- Use `R/z_derivation_helpers.R` for p-value, confidence-interval, and
  statistic-to-`z` derivation.
- Write a joinable artifact under `data_raw/clinicaltrials.gov/derived/`.
- Retain enough metadata to compare author-reported rows to raw endpoint
  candidates by `nct_id`, `outcome_id`, effect family, and available group
  links.
- Defer any changes to `data/clinicaltrialsgov.rds`, `BEAR.rds`, and BEAR
  selection rules to a later explicit integration step.

Open merge decisions remain: choose whether BEAR should keep one row per
contrast, one preferred scale per contrast, or a separate side dataset; define
which validity tiers are admissible; and decide how to handle multi-arm and
direction-unknown rows.
