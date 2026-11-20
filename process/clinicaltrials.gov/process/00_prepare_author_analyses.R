# Prepare the reduced author-reported analysis table from the AACT snapshot.
# Joins outcome analyses to outcome, study and design metadata. Reads directly
# from the snapshot zip set by CTGOV_SNAPSHOT_ZIP; no unzipping is needed.
# Replaces process/clinicaltrials_prepare_data.R.

library(readr)
library(dplyr)

source("process/clinicaltrials.gov/lib/paths.R")

ctgov_reset_caches_if_snapshot_changed()

author_raw_path <- file.path(ctgov_derived_dir, "author_reported_raw.rds")

read_snapshot_table <- function(table, ...) {
  message("Reading ", table)
  read_delim(unz(ctgov_snapshot_zip, table), delim = "|", progress = FALSE,
             show_col_types = FALSE, ...)
}

# Read -----

studies <- read_snapshot_table(
  "studies.txt",
  col_select = c(nct_id, brief_title, overall_status, completion_date,
                 study_type, phase, enrollment, number_of_arms)
) %>%
  mutate(completion_date = as.Date(completion_date))

designs <- read_snapshot_table(
  "designs.txt",
  col_types = cols(.default = "c"),
  col_select = c(nct_id, allocation, intervention_model, observational_model,
                 primary_purpose, masking)
)

outcomes <- read_snapshot_table(
  "outcomes.txt",
  col_types = cols(.default = "c"),
  col_select = c(nct_id, id, outcome_type, title, time_frame)
) %>%
  rename(outcome_id = id, outcome_title = title, outcome_time_frame = time_frame)

oa <- read_snapshot_table(
  "outcome_analyses.txt",
  col_select = c(nct_id, outcome_id, id, param_type, param_value, ci_percent,
                 ci_n_sides, ci_lower_limit, ci_upper_limit, p_value,
                 p_value_modifier, non_inferiority_type,
                 non_inferiority_description)
) %>%
  mutate(outcome_id = as.character(outcome_id)) %>%
  rename(outcome_analysis_id = id) %>%
  mutate(ci_n_sides = case_when(
    ci_n_sides == "ONE_SIDED" ~ 1,
    ci_n_sides == "TWO_SIDED" ~ 2,
    TRUE ~ NA
  ))

# Join -----

author_raw <- oa %>%
  left_join(outcomes, by = c("nct_id", "outcome_id")) %>%
  left_join(studies, by = "nct_id") %>%
  left_join(designs, by = "nct_id") %>%
  select(
    nct_id, brief_title, overall_status, completion_date,
    phase, enrollment, number_of_arms,
    allocation, study_type,
    intervention_model, observational_model, primary_purpose, masking,
    outcome_id, outcome_type, outcome_title, outcome_time_frame,
    outcome_analysis_id,
    param_type, param_value,
    ci_percent, ci_n_sides, ci_lower_limit, ci_upper_limit,
    p_value, p_value_modifier,
    non_inferiority_type, non_inferiority_description
  )

stopifnot(
  nrow(author_raw) == nrow(oa),
  anyDuplicated(author_raw$outcome_analysis_id) == 0
)

# Save -----

saveRDS(author_raw, author_raw_path)
ctgov_record_counts("00_prepare_author_analyses", c(
  outcome_analysis_rows = nrow(author_raw),
  outcome_analysis_trials = n_distinct(author_raw$nct_id)
))
message("Saved ", nrow(author_raw), " author analysis rows to ", author_raw_path)
