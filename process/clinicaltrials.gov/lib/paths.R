# Shared paths for the ClinicalTrials.gov raw-result workflow.

ctgov_root <- "data_raw/clinicaltrials.gov"
ctgov_process_dir <- "process/clinicaltrials.gov"
ctgov_snapshot_zip <- file.path(
  ctgov_root, "12082025", "a1hlrix83cqvhi5qn0jya3f8mdik.zip"
)

ctgov_derived_dir <- file.path(ctgov_root, "derived")
ctgov_intermediate_dir <- file.path(ctgov_derived_dir, "intermediate")
ctgov_validation_dir <- file.path(ctgov_root, "validation")
ctgov_manual_review_dir <- file.path(ctgov_validation_dir, "manual_review")

ctgov_domain_map_path <- file.path(ctgov_process_dir, "lib", "domain_mesh_map.csv")

dir.create(ctgov_derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ctgov_intermediate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ctgov_validation_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ctgov_manual_review_dir, recursive = TRUE, showWarnings = FALSE)
