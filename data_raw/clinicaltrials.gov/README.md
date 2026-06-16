# ClinicalTrials.gov AACT snapshot

This folder stores the local AACT flat-file snapshot used to prepare the
ClinicalTrials.gov BEAR dataset. The current snapshot is:

- date: 12 August 2025
- source: <https://aact.ctti-clinicaltrials.org/downloads/>
- local zip: `12082025/a1hlrix83cqvhi5qn0jya3f8mdik.zip`

## Refreshing the snapshot

AACT snapshots can be refreshed manually:

1. Download the current AACT pipe-delimited flat-file zip from the AACT
   downloads page.
2. Create a dated folder under `data_raw/clinicaltrials.gov/`.
3. Put the downloaded zip in that folder and unzip it there, so files such as
   `studies.txt`, `designs.txt`, `outcomes.txt`, and `outcome_analyses.txt`
   are available directly in the dated folder.
4. Update the snapshot path in the relevant `process/` script or path helper.
5. Run the relevant processing script from `process/`. For the raw-result
   side-data workflow, run
   `Rscript --vanilla process/clinicaltrials.gov/run_pipeline.R`.
6. Verify row counts before rebuilding `BEAR.rds`.

The current BEAR extraction uses pre-computed comparison rows from
`outcome_analyses.txt`. Trials with posted results may still have structured
arm-level results in other AACT tables, especially `outcome_measurements.txt`,
`outcome_counts.txt`, and `result_groups.txt`, but those tables are not used by
the current ClinicalTrials.gov BEAR processor.

The `derived/` folder contains intermediate outputs created from the local
snapshot. These files should be regenerated when the snapshot changes.

Processing code and maintainer notes for the raw-result side-data workflow
live in `process/clinicaltrials.gov/`. This raw-data folder should contain the
downloaded snapshot, generated data artifacts, validation artifacts, and this
README only.
