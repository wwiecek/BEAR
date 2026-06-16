# QC first-pass ClinicalTrials.gov raw outcome-measurement effect outputs.

library(dplyr)
library(readr)
library(tibble)

out_dir <- "data_raw/clinicaltrials.gov/derived/legacy_analysis_output"

continuous_primary <- readRDS(
  file.path(out_dir, "raw_continuous_mean_sd_effects_primary.rds")
)
binary_primary <- readRDS(
  file.path(out_dir, "raw_binary_count_effects_primary.rds")
)
raw_primary <- readRDS(
  file.path(out_dir, "raw_outcome_measurement_effects_primary.rds")
)
input_row_counts <- read_csv(
  file.path(out_dir, "input_row_counts.csv"),
  show_col_types = FALSE
)
exclusions <- read_csv(
  file.path(out_dir, "raw_outcome_measurement_exclusions.csv"),
  show_col_types = FALSE
)

required_cols <- c(
  "effect_id", "nct_id", "outcome_id", "b", "se", "z", "z_operator",
  "measure", "effect_family", "source_family", "ss"
)

qc_checks <- tibble(
  check = c(
    "required_columns",
    "unique_effect_id",
    "valid_z_operator",
    "all_rows_have_core_statistics",
    "all_rows_are_primary_outcomes",
    "all_rows_are_outcome_measurement_source",
    "positive_standard_errors",
    "positive_sample_sizes"
  ),
  passed = c(
    all(required_cols %in% names(raw_primary)),
    n_distinct(raw_primary$effect_id) == nrow(raw_primary),
    all(raw_primary$z_operator %in% c("=", "<", ">")),
    all(!is.na(raw_primary$nct_id)) &
      all(!is.na(raw_primary$outcome_id)) &
      all(!is.na(raw_primary$b)) &
      all(!is.na(raw_primary$se)) &
      all(!is.na(raw_primary$z)),
    all(raw_primary$outcome_type_norm == "primary"),
    all(raw_primary$source_family == "raw_outcome_measurement"),
    all(raw_primary$se > 0),
    all(raw_primary$ss > 0)
  )
)

effect_summary <- raw_primary %>%
  count(effect_family, measure, scale, validity_tier, name = "n") %>%
  arrange(effect_family, measure, validity_tier)

design_summary <- raw_primary %>%
  count(phase, effect_family, name = "n") %>%
  arrange(effect_family, desc(n))

qc_summary <- bind_rows(
  tibble(section = "qc_check", qc_checks),
  effect_summary %>%
    mutate(
      section = "effect_summary",
      check = paste(effect_family, measure, scale, validity_tier, sep = "|"),
      passed = TRUE
    ) %>%
    select(section, check, passed, n),
  exclusions %>%
    mutate(
      section = "exclusion_summary",
      check = paste(source, reason, sep = "|"),
      passed = TRUE
    ) %>%
    select(section, check, passed, n)
)

print(qc_checks, n = nrow(qc_checks), width = Inf)
print(effect_summary, n = nrow(effect_summary), width = Inf)
print(input_row_counts, n = nrow(input_row_counts), width = Inf)
print(utils::head(raw_primary, 10), width = Inf)

write_csv(qc_summary, file.path(out_dir, "raw_effects_qc_summary.csv"))
write_csv(design_summary, file.path(out_dir, "raw_effects_phase_summary.csv"))

stopifnot(all(qc_checks$passed))

message("QC passed for ", nrow(raw_primary), " primary raw-derived effects.")
