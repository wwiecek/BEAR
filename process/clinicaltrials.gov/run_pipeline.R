# Run the canonical ClinicalTrials.gov result pipeline.
# Set CTGOV_SNAPSHOT_ZIP to the AACT flat-file zip before running, e.g.
# CTGOV_SNAPSHOT_ZIP=data_raw/clinicaltrials.gov/<DDMMYYYY>/<file>.zip \
#   Rscript --vanilla process/clinicaltrials.gov/run_pipeline.R

source("process/clinicaltrials.gov/lib/paths.R")

message("Snapshot: ", ctgov_snapshot_zip)
ctgov_reset_caches_if_snapshot_changed()

pipeline_scripts <- c(
  "process/clinicaltrials.gov/process/00_prepare_author_analyses.R",
  "process/clinicaltrials.gov/process/01_prepare_aact_extracts.R",
  "process/clinicaltrials.gov/process/07_build_author_reported_candidates.R",
  "process/clinicaltrials.gov/process/02_derive_endpoint_effects.R",
  "process/clinicaltrials.gov/validate/01_validate_endpoint_effects.R",
  "process/clinicaltrials.gov/process/03_classify_endpoint_source_rows.R",
  "process/clinicaltrials.gov/process/04_extract_prepost_side_tables.R",
  "process/clinicaltrials.gov/validate/02_validate_prepost_side_tables.R",
  "process/clinicaltrials.gov/validate/03_validate_trial_coverage.R",
  "process/clinicaltrials.gov/validate/04_validate_author_overlap.R",
  "process/clinicaltrials.gov/process/05_build_trial_characteristics.R",
  "process/clinicaltrials.gov/process/06_build_endpoint_candidates.R",
  "process/clinicaltrials.gov/process/08_select_raw_endpoint_scale.R",
  "process/clinicaltrials.gov/process/09_merge_author_and_raw.R",
  "process/clinicaltrials.gov/process/10_integrate_manual_review.R",
  "process/clinicaltrials.gov/validate/05_validate_final_merge.R"
)

for (script in pipeline_scripts) {
  message("\n--- Running ", script, " ---")
  sys.source(script, envir = new.env(parent = globalenv()))
  invisible(gc())
}

message("\nClinicalTrials.gov pipeline completed. Snapshot-specific counts: ",
        ctgov_counts_path)
