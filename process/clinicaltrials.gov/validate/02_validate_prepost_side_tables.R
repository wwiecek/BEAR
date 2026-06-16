# QC ClinicalTrials.gov pre/post side-table outputs.

library(dplyr)
library(readr)
library(stringr)
library(tibble)

source("process/clinicaltrials.gov/lib/paths.R")

out_dir <- ctgov_derived_dir
validation_dir <- ctgov_validation_dir

prepost <- readRDS(file.path(out_dir, "prepost_measurement_pairs.rds"))
contrasts <- readRDS(file.path(out_dir, "prepost_contrast_candidates.rds"))
endpoint_source_rows <- readRDS(file.path(out_dir, "endpoint_source_rows.rds"))
source_rows_classified <- readRDS(file.path(out_dir, "endpoint_source_rows_classified.rds"))
raw_effects_v2 <- readRDS(file.path(out_dir, "raw_endpoint_effects.rds"))

required_cols <- c(
  "prepost_pair_id", "nct_id", "outcome_id", "result_group_id",
  "pre_row_id", "post_row_id", "prepost_source", "prepost_match_tier",
  "construct_key", "pair_selection_rank", "preferred_post",
  "time_role_pre", "time_role_post", "time_pre_label", "time_post_label",
  "time_post_days", "mean_pre", "mean_post", "sd_pre", "sd_post",
  "se_pre", "se_post", "change_mean", "x_pre", "n_pre", "risk_pre",
  "x_post", "n_post", "risk_post", "risk_change", "warning_flags",
  "group_match_method", "time_role_rule_id"
)

source_a <- prepost %>%
  filter(prepost_source == "outcome_measurements_internal")
source_b <- prepost %>%
  filter(prepost_source == "baseline_measurements_matched")
continuous <- prepost %>% filter(pair_type == "continuous")
binary <- prepost %>% filter(pair_type == "binary")

qc_checks <- tibble(
  check = c(
    "required_columns",
    "unique_prepost_pair_id",
    "unique_pre_post_row_mapping",
    "valid_sources_and_tiers",
    "source_a_same_outcome_group",
    "source_b_exact_group_match",
    "valid_time_roles",
    "no_reported_change_rows_in_pairs",
    "continuous_values_finite",
    "continuous_change_recomputes",
    "continuous_sensitivity_se_monotone",
    "binary_counts_in_range",
    "binary_risks_in_range",
    "fractional_counts_from_percent_only",
    "contrast_ids_unique",
    "endpoint_rows_unchanged",
    "classified_source_rows_preserve_endpoint_rows"
  ),
  passed = c(
    all(required_cols %in% names(prepost)),
    n_distinct(prepost$prepost_pair_id) == nrow(prepost),
    n_distinct(prepost$pre_row_id, prepost$post_row_id) == nrow(prepost),
    all(prepost$prepost_source %in% c(
      "outcome_measurements_internal", "baseline_measurements_matched"
    )) & all(prepost$prepost_match_tier %in% c("A", "B")),
    all(source_a$prepost_match_tier == "A") &
      all(source_a$group_match_method == "same_result_group_id"),
    all(source_b$prepost_match_tier == "B") &
      all(source_b$group_match_method == "exact_group_title_description"),
    all(prepost$time_role_pre == "baseline") &
      all(prepost$time_role_post == "post"),
    !any(prepost$time_role_pre == "reported_change") &
      !any(prepost$time_role_post == "reported_change"),
    all(is.finite(continuous$mean_pre)) &
      all(is.finite(continuous$mean_post)) &
      all(is.finite(continuous$n_pre)) &
      all(is.finite(continuous$n_post)),
    all(abs(continuous$change_mean -
      (continuous$mean_post - continuous$mean_pre)) < 1e-8),
    all(
      is.na(continuous$se_change_rho_0) |
        (
          continuous$se_change_rho_0 >= continuous$se_change_rho_025 &
            continuous$se_change_rho_025 >= continuous$se_change_rho_05 &
            continuous$se_change_rho_05 >= continuous$se_change_rho_075
        )
    ),
    all(binary$x_pre >= 0 & binary$x_pre <= binary$n_pre) &
      all(binary$x_post >= 0 & binary$x_post <= binary$n_post),
    all(binary$risk_pre >= 0 & binary$risk_pre <= 1) &
      all(binary$risk_post >= 0 & binary$risk_post <= 1),
    all(
      !(coalesce(binary$x_is_fractional_pre, FALSE) |
          coalesce(binary$x_is_fractional_post, FALSE)) |
        coalesce(binary$derived_from_percentage_pre, FALSE) |
        coalesce(binary$derived_from_percentage_post, FALSE)
    ),
    nrow(contrasts) == 0 ||
      n_distinct(contrasts$prepost_contrast_candidate_id) == nrow(contrasts),
    nrow(raw_effects_v2) == 250444,
    nrow(source_rows_classified) == nrow(endpoint_source_rows) &
      all(source_rows_classified$outcome_measurement_id == endpoint_source_rows$outcome_measurement_id)
  )
)

pair_summary <- prepost %>%
  count(
    prepost_source, prepost_match_tier, pair_type, source_row_class_pre,
    source_row_class_post, se_change_status, preferred_post, name = "n"
  ) %>%
  mutate(
    section = "pair_summary",
    check = paste(
      prepost_source, prepost_match_tier, pair_type, source_row_class_pre,
      source_row_class_post, se_change_status, preferred_post, sep = "|"
    ),
    passed = TRUE
  ) %>%
  select(section, check, passed, n)

contrast_summary <- contrasts %>%
  count(
    prepost_source, prepost_match_tier, pair_type, did_measure,
    did_validity, direction_known, pair_selection_rule, name = "n"
  ) %>%
  mutate(
    section = "contrast_summary",
    check = paste(
      prepost_source, prepost_match_tier, pair_type, did_measure,
      did_validity, direction_known, pair_selection_rule, sep = "|"
    ),
    passed = TRUE
  ) %>%
  select(section, check, passed, n)

qc_summary <- bind_rows(
  qc_checks %>% mutate(section = "qc_check", n = NA_integer_),
  pair_summary,
  contrast_summary
)

write_csv(qc_summary, file.path(validation_dir, "prepost_qc.csv"))

print(qc_checks, n = nrow(qc_checks), width = Inf)

stopifnot(all(qc_checks$passed))

message("QC passed for ", nrow(prepost), " pre/post group-level pairs.")
