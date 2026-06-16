# Derive first-pass raw arm-level effects from ClinicalTrials.gov
# outcome_measurements using conservative continuous and binary rules.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

out_dir <- "data_raw/clinicaltrials.gov/derived/legacy_analysis_output"

# Helpers -----

control_roles <- c(
  "placebo_comparator", "sham_comparator", "usual_care",
  "control_unspecified"
)

treatment_roles <- c("experimental", "treatment_unspecified")

safe_divide <- function(x, y) ifelse(y != 0 & is.finite(y), x / y, NA_real_)

role_priority <- function(role) {
  case_when(
    role %in% control_roles ~ 0L,
    role %in% treatment_roles ~ 1L,
    TRUE ~ 2L
  )
}

warning_list <- function(...) {
  flags <- list(...)
  out <- names(flags)[vapply(flags, isTRUE, logical(1))]
  if (length(out) == 0) NA_character_ else str_c(out, collapse = ";")
}

make_effect_id <- function(prefix, n) {
  sprintf("%s_%09d", prefix, seq_len(n))
}

classify_binary_count <- function(classification, category, title, description,
                                  outcome_title) {
  x <- str_squish(str_to_lower(str_c(
    classification, category, title, description, outcome_title,
    sep = " "
  )))
  case_when(
    str_detect(x, "sex|gender|race|ethnic|age group|age category|region") ~
      "baseline_or_covariate_like",
    str_detect(x, "remission|responder|response|cure|success") ~
      "responder_count",
    str_detect(x, "failure|recurrence|progression|relapse") ~ "event_count",
    str_detect(x, "death|mortality|infection|hospitali[sz]ation") ~
      "event_count",
    str_detect(x, "withdrawal|discontinuation|event|adverse|\\bae\\b|teae") ~
      "event_count",
    str_detect(x, "pregnancy") ~ "event_count",
    TRUE ~ "ambiguous_count"
  )
}

is_complement_category <- function(category) {
  x <- str_squish(str_to_lower(ifelse(is.na(category), "", category)))
  str_detect(x, "^(no|none|not|non|without|negative|absent)\\b|no event")
}

is_baseline_only_measurement <- function(classification, category) {
  x <- str_squish(str_to_lower(str_c(
    coalesce(classification, ""),
    coalesce(category, ""),
    sep = " "
  )))
  baselineish <- str_detect(
    x,
    "\\b(baseline|enrollment|screening|pre[- ]?treatment|pre[- ]?dose)\\b"
  )
  followupish <- str_detect(
    x,
    "\\b(change|from baseline|post|week|month|day|follow[- ]?up|endpoint)\\b"
  )
  baselineish & !followupish
}

pair_group_rows <- function(df) {
  keys <- c(
    "nct_id", "outcome_id", "measurement_stratum_id", "outcome_type",
    "outcome_type_norm", "outcome_title", "outcome_time_frame",
    "classification", "category", "measurement_title", "measurement_units",
    "brief_title", "year", "phase", "enrollment", "number_of_arms",
    "number_of_groups", "study_type", "allocation", "intervention_model",
    "primary_purpose", "masking", "is_first_pass_design_eligible",
    "n_eligible_groups"
  )

  group_cols <- c(
    "result_group_id", "ctgov_group_code", "group_title",
    "group_description", "arm_role_guess", "param_value_num",
    "dispersion_value_num", "outcome_count"
  )

  keep_cols <- c(keys, group_cols)
  a <- df %>%
    select(all_of(keep_cols)) %>%
    rename_with(~ paste0(.x, "_a"), all_of(group_cols))
  b <- df %>%
    select(all_of(keep_cols)) %>%
    rename_with(~ paste0(.x, "_b"), all_of(group_cols))

  inner_join(a, b, by = keys, relationship = "many-to-many") %>%
    filter(result_group_id_a < result_group_id_b)
}

orient_pairs <- function(pairs) {
  pairs %>%
    mutate(
      a_control = arm_role_guess_a %in% control_roles,
      b_control = arm_role_guess_b %in% control_roles,
      a_treatment = arm_role_guess_a %in% treatment_roles,
      b_treatment = arm_role_guess_b %in% treatment_roles,
      direction_known = (a_treatment & b_control) | (b_treatment & a_control),
      use_a_as_treatment = case_when(
        a_treatment & b_control ~ TRUE,
        b_treatment & a_control ~ FALSE,
        TRUE ~ TRUE
      ),
      group_t_id = if_else(use_a_as_treatment, result_group_id_a, result_group_id_b),
      group_c_id = if_else(use_a_as_treatment, result_group_id_b, result_group_id_a),
      group_t_code = if_else(use_a_as_treatment, ctgov_group_code_a, ctgov_group_code_b),
      group_c_code = if_else(use_a_as_treatment, ctgov_group_code_b, ctgov_group_code_a),
      group_t_title = if_else(use_a_as_treatment, group_title_a, group_title_b),
      group_c_title = if_else(use_a_as_treatment, group_title_b, group_title_a),
      role_assignment = case_when(
        direction_known ~ "treatment_control_inferred",
        arm_role_guess_a == "unknown" | arm_role_guess_b == "unknown" ~
          "unknown_direction",
        TRUE ~ "non_comparator_or_ambiguous_direction"
      ),
      mean_t = if_else(use_a_as_treatment, param_value_num_a, param_value_num_b),
      mean_c = if_else(use_a_as_treatment, param_value_num_b, param_value_num_a),
      sd_t = if_else(
        use_a_as_treatment,
        dispersion_value_num_a,
        dispersion_value_num_b
      ),
      sd_c = if_else(
        use_a_as_treatment,
        dispersion_value_num_b,
        dispersion_value_num_a
      ),
      n_t = if_else(use_a_as_treatment, outcome_count_a, outcome_count_b),
      n_c = if_else(use_a_as_treatment, outcome_count_b, outcome_count_a),
      x_t = mean_t,
      x_c = mean_c
    )
}

pair_count_summary <- function(df) {
  df %>%
    group_by(nct_id, outcome_id, measurement_stratum_id) %>%
    summarise(n_eligible_groups = n_distinct(result_group_id), .groups = "drop")
}

# Load prepared data -----

measurements <- readRDS(file.path(out_dir, "outcome_measurements_long.rds"))
author_links <- readRDS(file.path(out_dir, "author_analysis_links.rds"))

author_overlap <- author_links %>%
  filter(!is.na(outcome_analysis_id)) %>%
  distinct(nct_id, outcome_id) %>%
  mutate(has_author_analysis = TRUE)

# Continuous mean + SD effects -----

continuous_candidates <- measurements %>%
  mutate(
    baseline_only_measurement =
      is_baseline_only_measurement(classification, category)
  ) %>%
  filter(
    is_first_pass_design_eligible,
    !baseline_only_measurement,
    param_type_norm == "mean",
    dispersion_type_norm == "sd",
    !is.na(param_value_num),
    !is.na(dispersion_value_num),
    dispersion_value_num > 0,
    !is.na(outcome_count),
    outcome_count > 1,
    is_participant_count,
    !is_overall_group,
    result_type_norm == "outcome"
  ) %>%
  left_join(author_overlap, by = c("nct_id", "outcome_id")) %>%
  mutate(has_author_analysis = coalesce(has_author_analysis, FALSE))

continuous_counts <- pair_count_summary(continuous_candidates)

continuous_pair_input <- continuous_candidates %>%
  left_join(continuous_counts, by = c("nct_id", "outcome_id", "measurement_stratum_id")) %>%
  filter(n_eligible_groups >= 2, n_eligible_groups <= 12)

continuous_pairs <- pair_group_rows(continuous_pair_input) %>%
  orient_pairs() %>%
  mutate(
    multi_arm_trial = n_eligible_groups > 2 |
      coalesce(number_of_arms, 0) > 2 | coalesce(number_of_groups, 0) > 2,
    classification_or_category_present =
      !is.na(classification) | !is.na(category),
    possible_repeated_structure = classification_or_category_present,
    b_md = mean_t - mean_c,
    se_md = sqrt(sd_t^2 / n_t + sd_c^2 / n_c),
    z_md = safe_divide(b_md, se_md),
    pooled_sd = sqrt(
      ((n_t - 1) * sd_t^2 + (n_c - 1) * sd_c^2) / (n_t + n_c - 2)
    ),
    hedges_j = 1 - 3 / (4 * (n_t + n_c) - 9),
    b_smd = hedges_j * safe_divide(mean_t - mean_c, pooled_sd),
    se_smd = sqrt(
      (n_t + n_c) / (n_t * n_c) + b_smd^2 / (2 * (n_t + n_c - 2))
    ),
    z_smd = safe_divide(b_smd, se_smd)
  )

continuous_md <- continuous_pairs %>%
  transmute(
    effect_id = make_effect_id("ctgov_raw_cont_md", n()),
    nct_id, outcome_id, measurement_stratum_id, brief_title, year, phase,
    enrollment, number_of_arms, number_of_groups, study_type, allocation,
    intervention_model, primary_purpose, masking, outcome_type,
    outcome_type_norm, outcome_title, outcome_time_frame, classification,
    category, measurement_title, measurement_units, group_t_id, group_c_id,
    group_t_code, group_c_code, group_t_title, group_c_title,
    effect_family = "continuous_mean_sd",
    source_family = "raw_outcome_measurement",
    derivation_method = "mean_difference_from_arm_mean_sd",
    measure = "Mean Difference",
    scale = "raw",
    b = b_md,
    se = se_md,
    z = z_md,
    z_abs = abs(z_md),
    z_operator = "=",
    ss = n_t + n_c,
    n_t, n_c, mean_t, mean_c, sd_t, sd_c,
    direction_known, role_assignment, multi_arm_trial,
    participant_denominator = TRUE,
    validity_tier = if_else(direction_known & !multi_arm_trial, 2L, 3L),
    warning_flags = pmap_chr(
      list(
        direction_unknown = !direction_known,
        multi_arm_trial = multi_arm_trial,
        classification_or_category_present = classification_or_category_present,
        possible_repeated_structure = possible_repeated_structure
      ),
      warning_list
    )
  ) %>%
  filter(is.finite(b), is.finite(se), se > 0, !is.na(z))

continuous_smd <- continuous_pairs %>%
  transmute(
    effect_id = make_effect_id("ctgov_raw_cont_smd", n()),
    nct_id, outcome_id, measurement_stratum_id, brief_title, year, phase,
    enrollment, number_of_arms, number_of_groups, study_type, allocation,
    intervention_model, primary_purpose, masking, outcome_type,
    outcome_type_norm, outcome_title, outcome_time_frame, classification,
    category, measurement_title, measurement_units, group_t_id, group_c_id,
    group_t_code, group_c_code, group_t_title, group_c_title,
    effect_family = "continuous_mean_sd",
    source_family = "raw_outcome_measurement",
    derivation_method = "hedges_g_from_arm_mean_sd",
    measure = "Standardized Mean Difference",
    scale = "smd",
    b = b_smd,
    se = se_smd,
    z = z_smd,
    z_abs = abs(z_smd),
    z_operator = "=",
    ss = n_t + n_c,
    n_t, n_c, mean_t, mean_c, sd_t, sd_c,
    direction_known, role_assignment, multi_arm_trial,
    participant_denominator = TRUE,
    validity_tier = if_else(direction_known & !multi_arm_trial, 2L, 3L),
    warning_flags = pmap_chr(
      list(
        direction_unknown = !direction_known,
        multi_arm_trial = multi_arm_trial,
        classification_or_category_present = classification_or_category_present,
        possible_repeated_structure = possible_repeated_structure
      ),
      warning_list
    )
  ) %>%
  filter(is.finite(b), is.finite(se), se > 0, !is.na(z))

continuous_effects_all <- bind_rows(continuous_md, continuous_smd)
continuous_effects_primary <- continuous_effects_all %>%
  filter(outcome_type_norm == "primary")

# Binary count effects -----

binary_candidates <- measurements %>%
  mutate(
    baseline_only_measurement =
      is_baseline_only_measurement(classification, category)
  ) %>%
  filter(
    is_first_pass_design_eligible,
    !baseline_only_measurement,
    param_type_norm == "count_of_participants",
    !is.na(param_value_num),
    !is.na(outcome_count),
    outcome_count > 0,
    param_value_num >= 0,
    param_value_num <= outcome_count,
    is_participant_count,
    !is_overall_group,
    result_type_norm == "outcome"
  ) %>%
  mutate(
    binary_count_type = classify_binary_count(
      classification, category, measurement_title, measurement_description,
      outcome_title
    ),
    category_present = !is.na(category_norm) & category_norm != "",
    complement_category = is_complement_category(category_norm)
  ) %>%
  group_by(nct_id, outcome_id, classification_norm, measurement_title_norm) %>%
  mutate(
    n_categories_in_measure = n_distinct(category_norm[category_present]),
    multicategory_measure = n_categories_in_measure > 2
  ) %>%
  ungroup() %>%
  filter(
    !multicategory_measure,
    !complement_category,
    binary_count_type %in% c("event_count", "responder_count")
  ) %>%
  left_join(author_overlap, by = c("nct_id", "outcome_id")) %>%
  mutate(has_author_analysis = coalesce(has_author_analysis, FALSE))

binary_counts <- pair_count_summary(binary_candidates)

binary_pair_input <- binary_candidates %>%
  left_join(binary_counts, by = c("nct_id", "outcome_id", "measurement_stratum_id")) %>%
  filter(n_eligible_groups >= 2, n_eligible_groups <= 12)

binary_pairs <- pair_group_rows(binary_pair_input) %>%
  orient_pairs() %>%
  mutate(
    x_t = mean_t,
    x_c = mean_c,
    p_t = x_t / n_t,
    p_c = x_c / n_c,
    zero_or_boundary_cell =
      x_t == 0 | x_c == 0 | x_t == n_t | x_c == n_c,
    multi_arm_trial = n_eligible_groups > 2 |
      coalesce(number_of_arms, 0) > 2 | coalesce(number_of_groups, 0) > 2,
    rd = p_t - p_c,
    se_rd = sqrt(p_t * (1 - p_t) / n_t + p_c * (1 - p_c) / n_c),
    z_rd = safe_divide(rd, se_rd),
    log_rr = if_else(x_t > 0 & x_c > 0, log(p_t / p_c), NA_real_),
    se_log_rr = if_else(
      x_t > 0 & x_c > 0,
      sqrt(1 / x_t - 1 / n_t + 1 / x_c - 1 / n_c),
      NA_real_
    ),
    z_log_rr = safe_divide(log_rr, se_log_rr),
    log_or = if_else(
      x_t > 0 & x_c > 0 & x_t < n_t & x_c < n_c,
      log((x_t / (n_t - x_t)) / (x_c / (n_c - x_c))),
      NA_real_
    ),
    se_log_or = if_else(
      x_t > 0 & x_c > 0 & x_t < n_t & x_c < n_c,
      sqrt(1 / x_t + 1 / (n_t - x_t) + 1 / x_c + 1 / (n_c - x_c)),
      NA_real_
    ),
    z_log_or = safe_divide(log_or, se_log_or)
  )

binary_rd <- binary_pairs %>%
  transmute(
    effect_id = make_effect_id("ctgov_raw_bin_rd", n()),
    nct_id, outcome_id, measurement_stratum_id, brief_title, year, phase,
    enrollment, number_of_arms, number_of_groups, study_type, allocation,
    intervention_model, primary_purpose, masking, outcome_type,
    outcome_type_norm, outcome_title, outcome_time_frame, classification,
    category, measurement_title, measurement_units, group_t_id, group_c_id,
    group_t_code, group_c_code, group_t_title, group_c_title,
    effect_family = "binary_count",
    source_family = "raw_outcome_measurement",
    derivation_method = "risk_difference_from_counts",
    measure = "Risk Difference",
    scale = "raw",
    b = rd,
    se = se_rd,
    z = z_rd,
    z_abs = abs(z_rd),
    z_operator = "=",
    ss = n_t + n_c,
    n_t, n_c, x_t, x_c, p_t, p_c,
    direction_known, role_assignment, multi_arm_trial,
    participant_denominator = TRUE,
    validity_tier = if_else(direction_known & !multi_arm_trial, 2L, 3L),
    warning_flags = pmap_chr(
      list(
        direction_unknown = !direction_known,
        multi_arm_trial = multi_arm_trial,
        zero_or_boundary_cell = zero_or_boundary_cell
      ),
      warning_list
    )
  ) %>%
  filter(is.finite(b), is.finite(se), se > 0, !is.na(z))

binary_rr <- binary_pairs %>%
  transmute(
    effect_id = make_effect_id("ctgov_raw_bin_rr", n()),
    nct_id, outcome_id, measurement_stratum_id, brief_title, year, phase,
    enrollment, number_of_arms, number_of_groups, study_type, allocation,
    intervention_model, primary_purpose, masking, outcome_type,
    outcome_type_norm, outcome_title, outcome_time_frame, classification,
    category, measurement_title, measurement_units, group_t_id, group_c_id,
    group_t_code, group_c_code, group_t_title, group_c_title,
    effect_family = "binary_count",
    source_family = "raw_outcome_measurement",
    derivation_method = "log_risk_ratio_from_counts",
    measure = "Risk Ratio",
    scale = "log",
    b = log_rr,
    se = se_log_rr,
    z = z_log_rr,
    z_abs = abs(z_log_rr),
    z_operator = "=",
    ss = n_t + n_c,
    n_t, n_c, x_t, x_c, p_t, p_c,
    direction_known, role_assignment, multi_arm_trial,
    participant_denominator = TRUE,
    validity_tier = if_else(direction_known & !multi_arm_trial, 2L, 3L),
    warning_flags = pmap_chr(
      list(
        direction_unknown = !direction_known,
        multi_arm_trial = multi_arm_trial,
        zero_or_boundary_cell = zero_or_boundary_cell
      ),
      warning_list
    )
  ) %>%
  filter(is.finite(b), is.finite(se), se > 0, !is.na(z))

binary_or <- binary_pairs %>%
  transmute(
    effect_id = make_effect_id("ctgov_raw_bin_or", n()),
    nct_id, outcome_id, measurement_stratum_id, brief_title, year, phase,
    enrollment, number_of_arms, number_of_groups, study_type, allocation,
    intervention_model, primary_purpose, masking, outcome_type,
    outcome_type_norm, outcome_title, outcome_time_frame, classification,
    category, measurement_title, measurement_units, group_t_id, group_c_id,
    group_t_code, group_c_code, group_t_title, group_c_title,
    effect_family = "binary_count",
    source_family = "raw_outcome_measurement",
    derivation_method = "log_odds_ratio_from_counts",
    measure = "Odds Ratio",
    scale = "log",
    b = log_or,
    se = se_log_or,
    z = z_log_or,
    z_abs = abs(z_log_or),
    z_operator = "=",
    ss = n_t + n_c,
    n_t, n_c, x_t, x_c, p_t, p_c,
    direction_known, role_assignment, multi_arm_trial,
    participant_denominator = TRUE,
    validity_tier = if_else(direction_known & !multi_arm_trial, 2L, 3L),
    warning_flags = pmap_chr(
      list(
        direction_unknown = !direction_known,
        multi_arm_trial = multi_arm_trial,
        zero_or_boundary_cell = zero_or_boundary_cell
      ),
      warning_list
    )
  ) %>%
  filter(is.finite(b), is.finite(se), se > 0, !is.na(z))

binary_effects_all <- bind_rows(binary_rd, binary_rr, binary_or)
binary_effects_primary <- binary_effects_all %>%
  filter(outcome_type_norm == "primary")

raw_effects_primary <- bind_rows(
  continuous_effects_primary,
  binary_effects_primary
) %>%
  arrange(nct_id, outcome_id, measurement_stratum_id, effect_family, measure) %>%
  mutate(effect_id = make_effect_id("ctgov_raw_outcome_measurement", n()))

exclusion_summary <- bind_rows(
  tibble(
    source = "continuous_mean_sd",
    reason = "candidate_group_rows",
    n = nrow(continuous_candidates)
  ),
  tibble(
    source = "continuous_mean_sd",
    reason = "strata_with_2_to_12_groups",
    n = n_distinct(continuous_pair_input$measurement_stratum_id)
  ),
  tibble(
    source = "continuous_mean_sd",
    reason = "derived_effect_rows_all",
    n = nrow(continuous_effects_all)
  ),
  tibble(
    source = "continuous_mean_sd",
    reason = "derived_effect_rows_primary",
    n = nrow(continuous_effects_primary)
  ),
  tibble(
    source = "binary_count",
    reason = "candidate_group_rows",
    n = nrow(binary_candidates)
  ),
  tibble(
    source = "binary_count",
    reason = "strata_with_2_to_12_groups",
    n = n_distinct(binary_pair_input$measurement_stratum_id)
  ),
  tibble(
    source = "binary_count",
    reason = "derived_effect_rows_all",
    n = nrow(binary_effects_all)
  ),
  tibble(
    source = "binary_count",
    reason = "derived_effect_rows_primary",
    n = nrow(binary_effects_primary)
  )
)

# Save -----

saveRDS(
  continuous_effects_all,
  file.path(out_dir, "raw_continuous_mean_sd_effects_all.rds")
)
saveRDS(
  continuous_effects_primary,
  file.path(out_dir, "raw_continuous_mean_sd_effects_primary.rds")
)
saveRDS(
  binary_effects_all,
  file.path(out_dir, "raw_binary_count_effects_all.rds")
)
saveRDS(
  binary_effects_primary,
  file.path(out_dir, "raw_binary_count_effects_primary.rds")
)
saveRDS(
  raw_effects_primary,
  file.path(out_dir, "raw_outcome_measurement_effects_primary.rds")
)
write_csv(
  exclusion_summary,
  file.path(out_dir, "raw_outcome_measurement_exclusions.csv")
)

message("Saved raw-derived outcome-measurement effects to ", out_dir)
