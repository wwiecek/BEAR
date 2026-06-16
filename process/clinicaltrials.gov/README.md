# ClinicalTrials.gov Raw Result Analysis

This folder contains the canonical side-data pipeline for ClinicalTrials.gov
AACT result tables that are not yet merged into BEAR. The workflow does not
modify `data/clinicaltrialsgov.rds`, `BEAR.rds`, paper outputs, or site outputs.

Run from the BEAR project root:

```sh
Rscript --vanilla process/clinicaltrials.gov/run_pipeline.R
```

## Layout

- `process/`: dataset-building scripts.
- `validate/`: checks, diagnostics, and manual-review queues.
- `lib/`: shared paths and small workflow assets.
- `run_pipeline.R`: canonical run order.

## Output Roots

- `data_raw/clinicaltrials.gov/derived/`: durable side datasets.
- `data_raw/clinicaltrials.gov/derived/intermediate/`: reusable reduced AACT
  extracts.
- `data_raw/clinicaltrials.gov/validation/`: QC summaries and diagnostics.
- `data_raw/clinicaltrials.gov/validation/manual_review/`: sampled review
  queues.

`data_raw/clinicaltrials.gov/derived/legacy_analysis_output/` is legacy output
from the earlier script layout. New canonical scripts do not write there.

See `derivation_notes.md` for row units, formulas, source classes, validation
counts, and integration notes.
