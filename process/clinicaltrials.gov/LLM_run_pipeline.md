# Run and publish an AACT snapshot

Use the [README](README.md#run-and-check) for the command and first checks.
Work from the BEAR project root.

## Prepare

- Save the AACT pipe-delimited ZIP under
  `data_raw/clinicaltrials.gov/DDMMYYYY/`. Record its date and filename.
  Check the archive with `unzip -Z1`. The runner needs the following tables
  (each with a `.txt` extension):

  ```text
  baseline_counts baseline_measurements browse_conditions browse_interventions
  conditions design_groups designs interventions keywords outcome_analyses
  outcome_analysis_groups outcome_counts outcome_measurements outcomes
  result_groups sponsors studies
  ```

- Check the branch and working tree before running. For a release, use a
  branch for the snapshot and account for existing changes.
- Copy the current `data/clinicaltrialsgov.rds` outside the output paths
  and record its snapshot if known. The cache stamp identifies the extracts
  used by the pipeline and may refer to a different run from the saved RDS.

## Run and compare

- Run the README command and save stdout and stderr to a log. Use the
  completion message and validation tables to decide whether the run passed.
- If a stage fails, diagnose the error before retrying. An interrupted
  extraction can leave partial cache files. Clear the affected extracts
  before restarting. After changing extraction code, clear
  `data_raw/clinicaltrials.gov/derived/intermediate/` so it is rebuilt.
- Compare the saved and new RDS files for columns, rows, distinct `nct_id`
  values, rows by `effect_source` and rows flagged `include_in_bear`. For the
  BEAR subset, use the filters in
  [`workflow/build_bear.R`](../../workflow/build_bear.R).
- Investigate schema changes, declines in coverage and large shifts in
  source mix before publishing. As a guide, inspect a loss of more than
  1% of previous trials or a change of more than 2 percentage points in the
  author-reported share. Compare the data summaries for changes in the
  distributions of effects and study characteristics.
- Follow the [non-inferiority review procedure](non_inferiority_review_procedure.md#reviewing-the-queue)
  if step 10 reports a queue.

## Publish

- Confirm that the [dictionary CSV](../../doc/datasets/clinicaltrials_gov_dictionary.csv)
  lists the output columns in order. If the schema changed, resolve the
  dictionary mismatch as part of validation. Once the checks pass, regenerate
  the rendered dictionary:

  ```sh
  Rscript --vanilla doc/datasets/build_clinicaltrials_gov_dictionary.R
  ```

- Update the [dataset page](../../doc/datasets/clinicaltrials_gov.Rmd), the
  bibliography year if needed, and the development entry in
  [NEWS](../../NEWS.md). Use the validated output for the source mix, BEAR
  subset, per-trial counts and non-inferiority summaries reported on the page.
- Record the snapshot, comparison counts and review decisions in the
  [changelog](changelog.md). Keep the run log with the local validation files.
- Review the diff, then publish the standalone RDS through the BEAR_data
  repository. Rebuild `BEAR.rds` only after reviewing the row counts and
  approving the standalone output. Keep raw files and caches local.
