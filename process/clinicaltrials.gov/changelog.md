# Changelog

## 2026-06-25 completed-only and global row-count rule

- Restricted selected raw-derived imports to completed studies, matching the
  author-reported path.
- Kept the public analytic file unrestricted by study row count, while applying
  the fewer-than-20 candidate-row rule in the main BEAR build for both
  author-reported and raw-derived rows.
- Added `direction_unknown` to the public file so raw-derived sign ambiguity is
  visible without interpreting role-confidence labels.

## 2026-06-18 author/raw merge pipeline

- Added author-reported candidate construction inside the canonical
  `process/clinicaltrials.gov` workflow, preserving `outcome_id`,
  `outcome_analysis_id`, z-derivation metadata, and linked result-group pairs.
- Added raw scale selection for BEAR import. Continuous raw-only contrasts use
  standardized mean difference; binary raw-only contrasts use probit
  differences. Non-selected raw scales are retained as side artifacts.
- Added a final author-preferred merge that writes
  `data_raw/clinicaltrials.gov/derived/clinicaltrialsgov_merged_candidates.rds`
  and replaces `data/clinicaltrialsgov.rds`.
- Added final merge checks and documentation for retained raw calculation
  inputs, overlap policy, scale-comparison summaries, and memory-safety rules.

## 2026-06-12 canonical pipeline cleanup

- Moved the active ClinicalTrials.gov side-data workflow into `process/`,
  `validate/`, and `lib/`, with `run_pipeline.R` as the canonical entry point.
- Moved new durable outputs out of `derived/legacy_analysis_output/`: durable datasets now
  live under `data_raw/clinicaltrials.gov/derived/`, reduced reusable extracts
  under `derived/intermediate/`, diagnostics under
  `data_raw/clinicaltrials.gov/validation/`, and review queues under
  `validation/manual_review/`.
- Renamed canonical scripts and output files to remove visible first/second/
  third-pass version suffixes.
- Kept the old first-pass scripts and notes under `analysis/legacy/`; they are
  not in the canonical run order.
- Rewrote `derivation_notes.md` as the self-contained canonical workflow
  document, including output units, formulas, memory-safety rules, validation
  counts, and the future author-reported endpoint integration spec.
- Verified the canonical pipeline from the reduced extracts through final
  candidate assembly: 250,444 endpoint candidates, 41,608 coverage rows,
  31,076 primary-denominator trials, 9,415 old/raw overlapping trials, and
  41,608 one-row-per-trial characteristics rows.

## 2026-06-12

- Added `12_build_trial_characteristics.R`, a streamed side workflow that
  builds one trial-level characteristics row per workflow-union `nct_id`.
- Added `spec/domain_mesh_map.csv` as the versioned compact domain mapping used
  with AACT browse-condition terms and text fallback.
- Wrote trial-characteristics, long domain, long intervention, dictionary, QC,
  and manual-review outputs under `derived/legacy_analysis_output/`.
- Verified that left joins to `data/clinicaltrialsgov.rds` and
  `raw_endpoint_candidates_v3.rds` preserve their original row counts.

## 2026-06-11

- Added a validation layer before any BEAR merge: trial-level coverage audit,
  author-overlap diagnostic bands, and a validation-annotated raw endpoint
  candidate dataset that preserves all 250,444 prioritized v2 rows.
- Defined the primary coverage denominator as completed randomized parallel
  interventional trials with primary outcomes, while keeping raw-v2 trials
  outside that denominator in the audit table with `in_primary_denominator`.
- Rewrote `derivation_notes.md` as the self-contained workflow document and
  reduced `README.md` to purpose, run order, and key outputs.
- Added a v3 side workflow that leaves v2 endpoint effects unchanged while
  splitting v2 `unknown` source rows into explicit audit classes.
- Added strict diagnostic-only missing-category binary candidates for manual
  review; these are not merged into endpoint effects.
- Added pre/post group-level side tables from internal outcome measurements and
  exact baseline-measurement matches, plus sensitivity-only difference-in-change
  contrast candidates.
- Added v3 QC covering pair identity, source-tier constraints, time roles,
  statistical validity, contrast IDs, v2 endpoint row-count preservation, and
  source-row preservation.
- Added a v2 endpoint-only derivation script that reuses the existing reduced
  v1 outcome-measurement indexes instead of rereading the source AACT flat
  files.
- Improved arm orientation with ClinicalTrials.gov group-title prefix parsing,
  expanded comparator/treatment keywords, a single-clear-comparator rule, and
  explicit separation of direction from multi-arm dependency.
- Added author-linked pair marking from `outcome_analysis_groups.txt` and wrote
  both an all-pairs diagnostic output and a prioritized v2 endpoint output.
- Reworked binary count classification to keep title-driven event counts,
  positive event categories, and two-category complement pairs while continuing
  to exclude demographic, baseline, severity, missingness, and non-participant
  rows.
- Kept safety totals out of scope. The v2 QC script checks that no reported
  event totals or `safety_binary` rows enter the endpoint outputs.
- Wrote v2 audit outputs, manual review samples, author-overlap diagnostics,
  and comprehensive derivation notes.

## 2026-06-09

- Added a standalone first-pass analysis workflow for deriving candidate
  raw arm-level effects from AACT ClinicalTrials.gov outcome-measurement
  tables.
- Kept safety totals and baseline covariates out of scope for this pass.
- Wrote outputs under `derived/legacy_analysis_output/` so the existing BEAR-facing
  ClinicalTrials.gov dataset remains unchanged.
- Switched the preparation script to stream `outcome_measurements.txt` in
  chunks and retain only `MEAN` + `Standard Deviation` and
  `COUNT_OF_PARTICIPANTS` candidates before joining metadata.
- Excluded baseline-only measurement strata from the primary derived-effect
  outputs. The current QC-passing primary output contains 234,135 raw-derived
  candidate effects.
- Added `derivation_notes.md` with maintainer notes, current output counts,
  proposed BEAR integration steps, and validity checks to run before import.
