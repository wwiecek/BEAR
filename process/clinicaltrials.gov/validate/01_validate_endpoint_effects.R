# QC ClinicalTrials.gov raw endpoint effect outputs.

library(dplyr)
library(readr)
library(stringr)
library(tibble)

source("process/clinicaltrials.gov/lib/paths.R")

out_dir <- ctgov_derived_dir
validation_dir <- ctgov_validation_dir

raw_primary <- readRDS(file.path(out_dir, "raw_endpoint_effects.rds"))
raw_all_pairs <- readRDS(
  file.path(out_dir, "raw_endpoint_effect_pairs.rds")
)
source_rows <- readRDS(file.path(out_dir, "endpoint_source_rows.rds"))

required_cols <- c(
  "effect_id", "nct_id", "outcome_id", "measurement_stratum_id",
  "source_family", "source_row_class", "effect_family", "measure", "b", "se", "z",
  "z_operator", "p", "n_t", "n_c", "group_t_id", "group_c_id",
  "direction_known", "orientation_rule", "pair_selection_rule",
  "pair_dependency_class", "author_linked_pair", "validity_tier",
  "warning_flags", "derivation_rule_id"
)

continuous_rows <- raw_primary %>% filter(effect_family == "continuous")
binary_rows <- raw_primary %>% filter(effect_family == "binary")
continuous_md_rows <- continuous_rows %>% filter(measure == "mean_difference")
continuous_smd_rows <- continuous_rows %>%
  filter(measure == "standardized_mean_difference")
ci_source_rows <- source_rows %>%
  filter(source_row_class %in% c("continuous_mean_ci", "continuous_lsm_ci"))
se_ci_effect_rows <- continuous_md_rows %>%
  filter(source_row_class %in% c(
    "continuous_mean_se", "continuous_mean_ci",
    "continuous_lsm_se", "continuous_lsm_ci"
  ))
number_binary_rows <- binary_rows %>%
  filter(source_row_class == "binary_number_count")
percent_binary_rows <- binary_rows %>%
  filter(source_row_class == "binary_percent_explicit")

qc_checks <- tibble(
  check = c(
    "required_columns",
    "unique_effect_id",
    "valid_z_operator",
    "core_statistics_present",
    "z_recomputes",
    "primary_outcome_rows",
    "outcome_measurement_source_only",
    "positive_standard_errors",
    "positive_sample_sizes",
    "distinct_pair_groups",
    "continuous_inputs_valid",
    "continuous_ci_levels_valid",
    "continuous_se_ci_rows_valid",
    "smd_from_sd_only",
    "binary_inputs_valid",
    "binary_number_counts_integer_like",
    "binary_percent_counts_in_range",
    "fractional_counts_flagged",
    "no_safety_totals",
    "source_rows_have_classifier"
  ),
  passed = c(
    all(required_cols %in% names(raw_primary)),
    n_distinct(raw_primary$effect_id) == nrow(raw_primary),
    all(raw_primary$z_operator %in% c("=", "<", ">")),
    all(!is.na(raw_primary$nct_id)) &
      all(!is.na(raw_primary$b)) &
      all(!is.na(raw_primary$se)) &
      all(!is.na(raw_primary$z)),
    all(abs(raw_primary$z - raw_primary$b / raw_primary$se) < 1e-8),
    all(raw_primary$outcome_type_norm == "primary"),
    all(raw_primary$source_family == "raw_outcome_measurement"),
    all(raw_primary$se > 0),
    all(raw_primary$n_t > 0 & raw_primary$n_c > 0),
    all(raw_primary$group_t_id != raw_primary$group_c_id),
    all(continuous_md_rows$n_t > 1 & continuous_md_rows$n_c > 1) &
      all(continuous_smd_rows$sd_t > 0 & continuous_smd_rows$sd_c > 0),
    all(ci_source_rows$ci_level > 0 & ci_source_rows$ci_level < 100) &
      all(ci_source_rows$group_mean_se > 0),
    all(is.finite(se_ci_effect_rows$b)) &
      all(is.finite(se_ci_effect_rows$se)) &
      all(is.finite(se_ci_effect_rows$z)) &
      all(se_ci_effect_rows$se > 0),
    all(continuous_smd_rows$source_row_class == "continuous_mean_sd"),
    all(binary_rows$x_t >= 0 & binary_rows$x_t <= binary_rows$n_t) &
      all(binary_rows$x_c >= 0 & binary_rows$x_c <= binary_rows$n_c),
    all(abs(number_binary_rows$x_t - round(number_binary_rows$x_t)) < 1e-8) &
      all(abs(number_binary_rows$x_c - round(number_binary_rows$x_c)) < 1e-8),
    all(percent_binary_rows$x_t >= 0 & percent_binary_rows$x_t <= percent_binary_rows$n_t) &
      all(percent_binary_rows$x_c >= 0 & percent_binary_rows$x_c <= percent_binary_rows$n_c),
    all(!str_detect(
      coalesce(binary_rows$warning_flags, ""),
      "x_is_fractional"
    ) | abs(binary_rows$x_t - round(binary_rows$x_t)) > 1e-8 |
      abs(binary_rows$x_c - round(binary_rows$x_c)) > 1e-8),
    !any(raw_primary$source_family == "reported_event_total") &
      !any(raw_primary$effect_family == "safety_binary"),
    all(!is.na(source_rows$source_row_class)) &
      all(!is.na(source_rows$classifier_rule_id))
  )
)

effect_summary <- raw_primary %>%
  count(
    source_family, source_row_class, effect_family, measure, validity_tier,
    name = "n"
  ) %>%
  arrange(source_family, source_row_class, effect_family, measure, validity_tier)

all_pairs_summary <- raw_all_pairs %>%
  count(effect_family, measure, pair_selection_rule, validity_tier, name = "n") %>%
  arrange(effect_family, measure, pair_selection_rule, validity_tier)

source_summary <- source_rows %>%
  count(param_type_norm, dispersion_type_norm, source_row_class, name = "n") %>%
  arrange(desc(n))

qc_summary <- bind_rows(
  tibble(section = "qc_check", qc_checks),
  effect_summary %>%
    mutate(
      section = "effect_summary",
      check = paste(
        source_family, source_row_class, effect_family, measure,
        validity_tier, sep = "|"
      ),
      passed = TRUE
    ) %>%
    select(section, check, passed, n),
  all_pairs_summary %>%
    mutate(
      section = "all_pairs_summary",
      check = paste(effect_family, measure, pair_selection_rule, validity_tier, sep = "|"),
      passed = TRUE
    ) %>%
    select(section, check, passed, n),
  source_summary %>%
    mutate(
      section = "source_row_summary",
      check = paste(param_type_norm, dispersion_type_norm, source_row_class, sep = "|"),
      passed = TRUE
    ) %>%
    select(section, check, passed, n)
)

print(qc_checks, n = nrow(qc_checks), width = Inf)
print(effect_summary, n = nrow(effect_summary), width = Inf)
print(all_pairs_summary, n = nrow(all_pairs_summary), width = Inf)

write_csv(qc_summary, file.path(validation_dir, "endpoint_effects_qc.csv"))

stopifnot(all(qc_checks$passed))

message("QC passed for ", nrow(raw_primary), " primary endpoint effects.")
