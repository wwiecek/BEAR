# ClinicalTrials.gov pipeline

This pipeline builds BEAR's ClinicalTrials.gov dataset from an AACT snapshot.
It derives effects from author-reported analyses and arm-level measurements.
When both sources cover the same outcome and result-group pair, BEAR uses
the author-reported result and the standalone file retains both.

## Run and check

From the BEAR project root, set the path to the AACT pipe-delimited ZIP.
The scripts read it directly.

```sh
export CTGOV_SNAPSHOT_ZIP='data_raw/clinicaltrials.gov/DDMMYYYY/snapshot.zip'
Rscript --vanilla process/clinicaltrials.gov/run_pipeline.R
```

The runner replaces `data/clinicaltrialsgov.rds` and
`data_summary/clinicaltrialsgov.md` **before final validation**. Save a copy
for comparison before running. For a trial run, use a separate checkout
with its own output directories. The runner does not rebuild `BEAR.rds`.

- Look for `ClinicalTrials.gov pipeline completed.` in the log and check
  that all validation checks passed. The final validator checks the
  output columns against `doc/datasets/clinicaltrials_gov_dictionary.csv`.
- Compare row and trial counts with the previous file. Snapshot counts and
  merge checks are under `data_raw/clinicaltrials.gov/validation/`.
- Check step 10 for descriptions that need non-inferiority review.

The pipeline rebuilds cached extracts when the ZIP identity changes. For
cache recovery and release steps, see the runbook.

## Further reading

- [Snapshot runbook](LLM_run_pipeline.md) for preparation, checks and publication.
- [Non-inferiority review](non_inferiority_review_procedure.md#reviewing-the-queue)
  for reviewing the queue and recording decisions.
- [Derivation notes](derivation_notes.md) for row units, formulas, eligibility
  and interpretation.
- [Runner](run_pipeline.R) for execution order and [changelog](changelog.md)
  for dated changes and counts.
