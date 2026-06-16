# Run the canonical ClinicalTrials.gov raw-result side-data pipeline.

source("process/clinicaltrials.gov/lib/paths.R")

pipeline_scripts <- c(
  "process/clinicaltrials.gov/process/01_prepare_aact_extracts.R",
  "process/clinicaltrials.gov/process/02_derive_endpoint_effects.R",
  "process/clinicaltrials.gov/validate/01_validate_endpoint_effects.R",
  "process/clinicaltrials.gov/process/03_classify_endpoint_source_rows.R",
  "process/clinicaltrials.gov/process/04_extract_prepost_side_tables.R",
  "process/clinicaltrials.gov/validate/02_validate_prepost_side_tables.R",
  "process/clinicaltrials.gov/validate/03_validate_trial_coverage.R",
  "process/clinicaltrials.gov/validate/04_validate_author_overlap.R",
  "process/clinicaltrials.gov/process/05_build_trial_characteristics.R",
  "process/clinicaltrials.gov/process/06_build_endpoint_candidates.R"
)

for (script in pipeline_scripts) {
  message("\n--- Running ", script, " ---")
  sys.source(script, envir = new.env(parent = globalenv()))
  invisible(gc())
}

message("\nClinicalTrials.gov pipeline completed.")
