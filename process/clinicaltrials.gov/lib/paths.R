# Shared paths for the ClinicalTrials.gov raw-result workflow.
# The AACT snapshot is set with the CTGOV_SNAPSHOT_ZIP environment variable:
# the path to the pipe-delimited flat-file zip downloaded from AACT, e.g.
# data_raw/clinicaltrials.gov/<DDMMYYYY>/<file>.zip. Any snapshot with the same
# tables and columns can be used without code changes.

ctgov_root <- "data_raw/clinicaltrials.gov"
ctgov_process_dir <- "process/clinicaltrials.gov"

ctgov_snapshot_zip <- Sys.getenv("CTGOV_SNAPSHOT_ZIP")
if (!nzchar(ctgov_snapshot_zip)) {
  stop(
    "Set CTGOV_SNAPSHOT_ZIP to the AACT pipe-delimited flat-file zip, e.g. ",
    "CTGOV_SNAPSHOT_ZIP=data_raw/clinicaltrials.gov/<DDMMYYYY>/<file>.zip",
    call. = FALSE
  )
}
if (!file.exists(ctgov_snapshot_zip)) {
  stop("CTGOV_SNAPSHOT_ZIP does not point to an existing file: ",
       ctgov_snapshot_zip, call. = FALSE)
}

ctgov_derived_dir <- file.path(ctgov_root, "derived")
ctgov_intermediate_dir <- file.path(ctgov_derived_dir, "intermediate")
ctgov_validation_dir <- file.path(ctgov_root, "validation")
ctgov_manual_review_dir <- file.path(ctgov_validation_dir, "manual_review")

ctgov_domain_map_path <- file.path(ctgov_process_dir, "lib", "domain_mesh_map.csv")
# Tracked decisions from manual review, applied by step 10
ctgov_review_decisions_dir <- file.path(ctgov_process_dir, "lib", "manual_review")
ctgov_ni_review_path <- file.path(ctgov_review_decisions_dir, "non_inferiority_review.csv")

dir.create(ctgov_derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ctgov_intermediate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ctgov_validation_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ctgov_manual_review_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(ctgov_process_dir, "lib", "snapshot.R"))
