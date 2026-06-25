# Validate trial-level coverage before any ClinicalTrials.gov raw endpoint
# merge. The denominator is completed randomized parallel interventional trials
# with at least one primary outcome in the local AACT snapshot.

library(dplyr)
library(readr)
library(stringr)
library(tibble)

source("process/clinicaltrials.gov/lib/paths.R")

snapshot_zip <- ctgov_snapshot_zip
input_dir <- ctgov_intermediate_dir
out_dir <- ctgov_derived_dir
validation_dir <- ctgov_validation_dir
manual_review_dir <- ctgov_manual_review_dir

# Helpers -----

read_aact <- function(file, col_select) {
  message("Reading ", file)
  read_delim(
    unz(snapshot_zip, file),
    delim = "|",
    col_types = cols(.default = col_character()),
    col_select = all_of(col_select),
    na = c("", "NA", "N/A", "NULL", "null"),
    quote = "\"",
    trim_ws = FALSE,
    progress = FALSE,
    show_col_types = FALSE
  )
}

norm_text <- function(x) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  x %>%
    str_replace_all("[\\u2010-\\u2015]", "-") %>%
    str_replace_all("\\u00a0", " ") %>%
    str_squish() %>%
    str_to_lower()
}

flag_summary <- function(df, flag, label) {
  df %>%
    summarise(
      section = "flag_counts",
      category = label,
      n_trials = sum(.data[[flag]], na.rm = TRUE),
      n_old_bear_trials = sum(.data[[flag]] & in_old_bear, na.rm = TRUE),
      n_raw_endpoint_trials = sum(.data[[flag]] & has_raw_endpoint, na.rm = TRUE),
      .groups = "drop"
    )
}

# Load inputs -----

studies <- read_aact(
  "studies.txt",
  c(
    "nct_id", "brief_title", "overall_status", "study_type", "phase",
    "enrollment", "number_of_arms", "number_of_groups"
  )
)
designs <- read_aact(
  "designs.txt",
  c("nct_id", "allocation", "intervention_model", "primary_purpose", "masking")
)
outcomes <- read_aact("outcomes.txt", c("id", "nct_id", "outcome_type"))

old_bear <- readRDS(file.path(out_dir, "author_reported_candidates.rds"))
raw_endpoint <- readRDS(file.path(out_dir, "raw_endpoint_effects.rds"))
raw_source_v3 <- readRDS(file.path(out_dir, "endpoint_source_rows_classified.rds"))
prepost <- readRDS(file.path(out_dir, "prepost_measurement_pairs.rds"))
author_links <- readRDS(file.path(input_dir, "author_analysis_links.rds"))

# Build denominator and coverage flags -----

primary_outcome_trials <- outcomes %>%
  mutate(outcome_type_norm = norm_text(outcome_type)) %>%
  filter(outcome_type_norm == "primary") %>%
  count(nct_id, name = "n_primary_outcomes")

study_design_index <- studies %>%
  left_join(designs, by = "nct_id") %>%
  left_join(primary_outcome_trials, by = "nct_id") %>%
  mutate(
    across(
      c(overall_status, study_type, allocation, intervention_model),
      norm_text,
      .names = "{.col}_norm"
    ),
    n_primary_outcomes = coalesce(n_primary_outcomes, 0L),
    completed = overall_status_norm == "completed",
    randomized = allocation_norm == "randomized",
    parallel = intervention_model_norm == "parallel",
    interventional = study_type_norm == "interventional",
    has_primary_outcome = n_primary_outcomes > 0,
    in_primary_denominator =
      completed & randomized & parallel & interventional & has_primary_outcome
  ) %>%
  distinct(nct_id, .keep_all = TRUE)

audit_trial_ids <- bind_rows(
  study_design_index %>%
    filter(in_primary_denominator) %>%
    distinct(nct_id),
  old_bear %>% distinct(nct_id),
  raw_endpoint %>% distinct(nct_id),
  raw_source_v3 %>% distinct(nct_id),
  prepost %>% distinct(nct_id),
  author_links %>% distinct(nct_id)
) %>%
  filter(!is.na(nct_id)) %>%
  distinct(nct_id)

trial_flags <- audit_trial_ids %>%
  left_join(study_design_index, by = "nct_id") %>%
  transmute(
    nct_id, in_primary_denominator, brief_title, overall_status, study_type, allocation,
    intervention_model, primary_purpose, masking, phase, enrollment,
    number_of_arms, number_of_groups, n_primary_outcomes
  ) %>%
  left_join(
    old_bear %>%
      count(nct_id, name = "n_old_bear_rows") %>%
      mutate(in_old_bear = TRUE),
    by = "nct_id"
  ) %>%
  left_join(
    author_links %>%
      count(nct_id, name = "n_author_analysis_rows") %>%
      mutate(has_author_analysis = TRUE),
    by = "nct_id"
  ) %>%
  left_join(
    raw_endpoint %>%
      count(nct_id, name = "n_raw_endpoint_rows") %>%
      mutate(has_raw_endpoint = TRUE),
    by = "nct_id"
  ) %>%
  left_join(
    raw_source_v3 %>%
      filter(source_row_class == "unknown") %>%
      group_by(nct_id) %>%
      summarise(
        n_unknown_rows = n(),
        n_unknown_classes = n_distinct(source_row_class_audit),
        has_unknown_only_rows = n() > 0,
        .groups = "drop"
      ),
    by = "nct_id"
  ) %>%
  left_join(
    prepost %>%
      count(nct_id, name = "n_prepost_pairs") %>%
      mutate(has_prepost_rows = TRUE),
    by = "nct_id"
  ) %>%
  mutate(
    across(
      c(
        in_old_bear, has_author_analysis, has_raw_endpoint,
        has_unknown_only_rows, has_prepost_rows
      ),
      ~ coalesce(.x, FALSE)
    ),
    in_primary_denominator = coalesce(in_primary_denominator, FALSE),
    across(
      c(
        n_old_bear_rows, n_author_analysis_rows, n_raw_endpoint_rows,
        n_unknown_rows, n_unknown_classes, n_prepost_pairs
      ),
      ~ coalesce(.x, 0L)
    ),
    no_usable_data_reason = case_when(
      has_raw_endpoint ~ NA_character_,
      in_old_bear ~ "old_author_analysis_only",
      has_author_analysis ~ "author_analysis_present_no_raw_endpoint",
      has_prepost_rows ~ "prepost_side_table_only",
      has_unknown_only_rows ~ "unknown_source_rows_only",
      TRUE ~ "no_author_or_raw_endpoint_data"
    ),
    coverage_category = case_when(
      has_raw_endpoint & in_old_bear ~ "old_bear_and_raw_endpoint",
      has_raw_endpoint ~ "raw_endpoint_only",
      in_old_bear ~ "old_bear_only",
      has_author_analysis ~ "author_analysis_only",
      has_prepost_rows ~ "prepost_side_table_only",
      has_unknown_only_rows ~ "unknown_source_rows_only",
      TRUE ~ "no_structured_effect_source_found"
    )
  ) %>%
  arrange(nct_id)

coverage_summary <- bind_rows(
  tibble(
    section = "denominator",
    category = "completed_randomized_parallel_interventional_primary_outcome",
    n_trials = sum(trial_flags$in_primary_denominator),
    n_old_bear_trials = sum(trial_flags$in_primary_denominator & trial_flags$in_old_bear),
    n_raw_endpoint_trials = sum(
      trial_flags$in_primary_denominator & trial_flags$has_raw_endpoint
    )
  ),
  trial_flags %>%
    filter(in_primary_denominator) %>%
    count(section = "coverage_category", category = coverage_category, name = "n_trials") %>%
    mutate(
      n_old_bear_trials = NA_integer_,
      n_raw_endpoint_trials = NA_integer_
    ),
  bind_rows(
    flag_summary(
      filter(trial_flags, in_primary_denominator),
      "in_old_bear", "old_bear_presence"
    ),
    flag_summary(
      filter(trial_flags, in_primary_denominator),
      "has_author_analysis", "author_analysis_presence"
    ),
    flag_summary(
      filter(trial_flags, in_primary_denominator),
      "has_raw_endpoint", "raw_endpoint_presence"
    ),
    flag_summary(
      filter(trial_flags, in_primary_denominator),
      "has_unknown_only_rows", "unknown_rows_presence"
    ),
    flag_summary(
      filter(trial_flags, in_primary_denominator),
      "has_prepost_rows", "prepost_rows_presence"
    )
  ),
  trial_flags %>%
    count(
      section = "audit_table_scope",
      category = if_else(
        in_primary_denominator,
        "in_primary_denominator",
        "outside_primary_denominator_with_validation_artifact"
      ),
      name = "n_trials"
    ) %>%
    mutate(
      n_old_bear_trials = NA_integer_,
      n_raw_endpoint_trials = NA_integer_
    ),
  tibble(
    section = "baseline_overlap_check",
    category = c(
      "old_bear_rows", "old_bear_trials", "raw_endpoint_trials",
      "old_bear_raw_endpoint_overlapping_trials"
    ),
    n_trials = c(
      nrow(old_bear), n_distinct(old_bear$nct_id), n_distinct(raw_endpoint$nct_id),
      n_distinct(intersect(old_bear$nct_id, raw_endpoint$nct_id))
    ),
    n_old_bear_trials = NA_integer_,
    n_raw_endpoint_trials = NA_integer_
  )
) %>%
  arrange(section, category)

raw_trial_check <- raw_endpoint %>%
  distinct(nct_id) %>%
  left_join(trial_flags %>% count(nct_id, name = "n_coverage_rows"), by = "nct_id") %>%
  mutate(n_coverage_rows = coalesce(n_coverage_rows, 0L))

stopifnot(
  nrow(old_bear) == 69992,
  n_distinct(old_bear$nct_id) == 20342,
  n_distinct(raw_endpoint$nct_id) == 20286,
  n_distinct(intersect(old_bear$nct_id, raw_endpoint$nct_id)) == 9415,
  all(raw_trial_check$n_coverage_rows == 1L)
)

# Save -----

write_csv(trial_flags, file.path(validation_dir, "trial_coverage_audit.csv"))
write_csv(coverage_summary, file.path(validation_dir, "trial_coverage_summary.csv"))
write_csv(
  trial_flags %>%
    filter(in_primary_denominator, !has_raw_endpoint) %>%
    arrange(no_usable_data_reason, nct_id),
  file.path(manual_review_dir, "no_raw_effect_trials.csv")
)
write_csv(
  trial_flags %>%
    filter(in_primary_denominator, !has_raw_endpoint, has_unknown_only_rows) %>%
    arrange(desc(n_unknown_rows), nct_id),
  file.path(manual_review_dir, "unknown_only_trials.csv")
)

message("Saved trial coverage audit outputs to ", validation_dir)
