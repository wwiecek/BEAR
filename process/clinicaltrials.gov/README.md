# ClinicalTrials.gov Result Analysis

This folder contains the canonical ClinicalTrials.gov AACT result pipeline for
BEAR. It builds author-reported effects and raw-input-derived effects
separately, validates them, and merges them only at the end. The final merge
prefers author-reported effects for linked overlaps and adds selected raw-only
effects where no author-reported effect is available.

Run from the BEAR project root, with `CTGOV_SNAPSHOT_ZIP` set to the AACT
pipe-delimited flat-file zip (no unzipping needed):

```sh
CTGOV_SNAPSHOT_ZIP=data_raw/clinicaltrials.gov/<DDMMYYYY>/<file>.zip \
  Rscript --vanilla process/clinicaltrials.gov/run_pipeline.R
```

Any snapshot with the same tables and columns can be used. Cached derived
files are rebuilt automatically when the snapshot changes, and
snapshot-specific counts are written to
`data_raw/clinicaltrials.gov/validation/snapshot_counts.csv`.
`LLM_run_pipeline.md` gives step-by-step instructions for processing a new
snapshot, written for an LLM coding agent.

## Layout

- `process/`: dataset-building scripts.
- `validate/`: checks, diagnostics, and manual-review queues.
- `lib/`: shared paths, snapshot handling (`snapshot.R`), non-inferiority
  rules (`non_inferiority.R`), and small workflow assets.
- `lib/manual_review/`: tracked decisions from manual review, applied by step
  10.
- `review/`: helper scripts for manual review.
- `run_pipeline.R`: canonical run order.

Step 10 applies the recorded non-inferiority decisions and reports
descriptions that still need review. See
`non_inferiority_review_procedure.md`.

## Output Roots

- `data_raw/clinicaltrials.gov/derived/`: durable side datasets.
- `data_raw/clinicaltrials.gov/derived/intermediate/`: reusable reduced AACT
  extracts.
- `data_raw/clinicaltrials.gov/validation/`: QC summaries and diagnostics.
- `data_raw/clinicaltrials.gov/validation/manual_review/`: sampled review
  queues and the non-inferiority review queue.
- `data/clinicaltrialsgov.rds`: public BEAR-facing author-preferred merge,
  keeping completed studies without the main-BEAR row-count restriction.

`data_raw/clinicaltrials.gov/derived/legacy_analysis_output/` is legacy output
from the earlier script layout. New canonical scripts do not write there.

See `derivation_notes.md` for row units, formulas, source classes, validation
counts, and integration notes.
