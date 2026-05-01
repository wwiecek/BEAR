# Process completed primary-outcome ClinicalTrials.gov rows into BEAR-ready
# z-values, using the shared trial p-value/CI derivation workflow.

library(dplyr)
library(stringr)
library(lubridate)

source("R/z_derivation_helpers.R")

in_path <- "data_raw/clinicaltrials.gov/clinicaltrials.gov_aug2025.rds"
out_path <- "data/clinicaltrialsgov.rds"

eff <- readRDS(in_path)

eff_clean <- eff %>%
  rename(
    lower = ci_lower_limit,
    upper = ci_upper_limit,
    estimate = param_value
  ) %>%
  mutate(across(c(lower, upper, estimate), as.numeric)) %>%
  filter(
    overall_status == "COMPLETED",
    outcome_type == "PRIMARY"
  )

z_table <- eff_clean %>%
  bind_cols(
    derive_trial_z(
      estimate = eff_clean$estimate,
      lower = eff_clean$lower,
      upper = eff_clean$upper,
      p_value = eff_clean$p_value,
      p_operator = eff_clean$p_value_modifier,
      ci_level = eff_clean$ci_percent,
      ci_sides = eff_clean$ci_n_sides,
      measure_label = eff_clean$param_type
    )
  ) %>%
  mutate(
    effect = b,
    year = year(completion_date)
  )

z_table_small <- z_table %>%
  transmute(
    nct_id, brief_title, year, enrollment,
    allocation, study_type, phase, intervention_model, primary_purpose, masking,
    measure_class, scale,
    effect, b, se, z,
    z_ci, z_p, z_source, use_ci, sym_ratio, z_operator, p_sides
  ) %>%
  filter(!is.na(z))

validation_checks <- tibble::tibble(
  check = c(
    "required_columns",
    "valid_z_operator_values",
    "all_saved_rows_have_z",
    "no_output_row_gain"
  ),
  passed = c(
    all(c("nct_id", "measure_class", "z", "b", "se", "z_operator") %in%
          names(z_table_small)),
    all(is.na(z_table_small$z_operator) |
          z_table_small$z_operator %in% c("=", "<", ">")),
    all(!is.na(z_table_small$z)),
    nrow(z_table_small) <= nrow(eff_clean)
  )
)

print(validation_checks, n = nrow(validation_checks), width = Inf)
stopifnot(all(validation_checks$passed))

z_table_small %>%
  mutate_if(is.character, as.factor) %>%
  summary()

saveRDS(z_table_small, out_path)
