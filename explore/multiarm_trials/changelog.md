# Changelog

## 2026-07-08

- Clarified that this is a BEAR-internal, LLM-assisted exploratory workflow.
- Kept generated CSV, RDS, and note outputs out of the tracked folder; scripts
  and maintainer docs are the durable source.

## 2026-04-15

- Added `screen_multiarm_candidates_v1.R` to screen BEAR datasets for
  multi-arm and multi-outcome structure.
- Added a minimal `README.md` plus generated summary artifacts for quick
  reuse in follow-up exploratory work on within-trial correlations.
- Added `build_ctgov_shared_comparator_v1.R` to construct a cleaned
  clinicaltrials.gov multi-arm dataset with a single shared comparator per
  retained trial.
- Added a Cochrane check note documenting why Cochrane was dropped for this
  specific shared-comparator workflow.
