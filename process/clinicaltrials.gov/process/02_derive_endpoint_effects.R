# Derive second-pass endpoint effects from already reduced AACT
# ClinicalTrials.gov outcome-measurement indexes. This keeps the v1 streaming
# outputs intact and avoids rereading source flat files.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(tibble)

source("process/clinicaltrials.gov/lib/paths.R")

input_dir <- ctgov_intermediate_dir
out_dir <- ctgov_derived_dir
validation_dir <- ctgov_validation_dir
manual_review_dir <- ctgov_manual_review_dir
set.seed(20260611)

# Helpers -----

safe_divide <- function(x, y) ifelse(y != 0 & is.finite(y), x / y, NA_real_)

make_effect_id <- function(prefix, n) sprintf("%s_%09d", prefix, seq_len(n))

ci_level_from_dispersion <- function(x) {
  out <- str_match(x, "^ci_([0-9.]+)$")[, 2]
  suppressWarnings(as.numeric(out))
}

norm_text <- function(x) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  x %>%
    str_replace_all("[\\u2010-\\u2015]", "-") %>%
    str_replace_all("\\u00a0", " ") %>%
    str_squish() %>%
    str_to_lower()
}

warning_list <- function(...) {
  flags <- list(...)
  out <- names(flags)[vapply(flags, isTRUE, logical(1))]
  if (length(out) == 0) NA_character_ else str_c(out, collapse = ";")
}

text_has <- function(x, pattern) !is.na(x) & str_detect(x, pattern)

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

group_prefix <- function(title) {
  raw <- str_match(coalesce(title, ""), "^\\s*([^:]{1,80}):")[, 2]
  norm_text(raw)
}

classify_group_role <- function(title, description) {
  prefix <- group_prefix(title)
  x <- norm_text(str_c(title, description, sep = " "))

  prefix_role <- case_when(
    prefix == "experimental" ~ "active",
    prefix == "placebo comparator" ~ "comparator_placebo",
    prefix == "sham comparator" ~ "comparator_sham",
    prefix == "no intervention" ~ "comparator_no_intervention",
    prefix == "active comparator" ~ "comparator_active",
    prefix == "other" ~ "unknown_or_other",
    TRUE ~ NA_character_
  )

  keyword_role <- case_when(
    text_has(x, "\\bmatched placebo\\b|\\bplacebo\\b|\\bvehicle\\b") ~
      "comparator_placebo",
    text_has(x, "\\bsham\\b") ~ "comparator_sham",
    text_has(x, "usual care|standard care|standard of care|\\bsoc\\b") ~
      "comparator_usual_care",
    text_has(x, "waitlist|waiting list|observation|no intervention|no treatment") ~
      "comparator_no_intervention",
    text_has(x, "\\bcontrol\\b|\\bsaline\\b") ~ "comparator_control",
    text_has(
      x,
      paste(
        "\\bexperimental\\b|\\bintervention\\b|\\btreatment\\b|\\btherapy\\b",
        "\\bdrug\\b|\\bdose\\b|\\bmg\\b|\\bmcg\\b|\\bvaccine\\b|\\bdevice\\b",
        "\\bprocedure\\b|\\bsurgery\\b|behavioral intervention|\\bexercise\\b",
        "\\beducation\\b|\\bprogram\\b",
        sep = "|"
      )
    ) ~ "active",
    TRUE ~ "unknown"
  )

  role <- coalesce(prefix_role, keyword_role)
  tibble(
    group_prefix_raw = prefix,
    group_prefix_norm = prefix,
    group_title_without_prefix = str_squish(str_remove(coalesce(title, ""), "^\\s*[^:]{1,80}:")),
    group_role = role,
    group_role_source = if_else(!is.na(prefix_role), "prefix", "keyword_or_unknown")
  )
}

positive_category <- function(x) {
  text_has(
    x,
    paste(
      "^yes$|responder|^response$|healed|recovered|remission|recurrence",
      "relapse|progression|event|adverse event|death|positive|success",
      "achieved|improved|failure|infection|hospitali[sz]ation",
      sep = "|"
    )
  )
}

negative_category <- function(x) {
  text_has(
    x,
    paste(
      "^no$|non[- ]?responder|no response|not healed|unhealed",
      "not recovered|no remission|no recurrence|no relapse",
      "no progression|no event|no adverse event|alive|negative",
      "failure|not achieved|not improved|without|absent",
      sep = "|"
    )
  )
}

binary_event_text <- function(x) {
  text_has(
    x,
    paste(
      "number of participants (with|who)|percentage of participants",
      "proportion of participants|participants achieving",
      "participants experiencing|participants reporting|incidence of",
      "occurrence of|response rate|remission|relapse|recurrence",
      "progression|treatment failure|hospitali[sz]ation|death|mortality",
      "infection|healed|recovered|success|responder|adverse event|\\bae\\b",
      sep = "|"
    )
  )
}

binary_exclusion_reason <- function(x, units, baseline_only) {
  case_when(
    baseline_only ~ "excluded_demographic_or_baseline",
    text_has(units, "eyes?|lesions?|teeth|implants?|samples?|sites?|schools?|clinics?|hospitals?|procedures?|cycles?|visits?|observations?") ~
      "excluded_nonparticipant_units",
    text_has(x, "sex|gender|race|ethnic|age group|age category|bmi category|region|country|smoking status|alcohol use|prior therapy|baseline severity|genotype|mutation") ~
      "excluded_demographic_or_baseline",
    text_has(x, "mild|moderate|severe|grade [1-5]|normal|abnormal|\\blow\\b|medium|\\bhigh\\b|quartile|tertile|category [1-9]|level [1-9]") ~
      "excluded_severity_or_grade",
    text_has(x, "missing|unknown|not available|not assessed|unevaluable|indeterminate") ~
      "excluded_missingness",
    TRUE ~ NA_character_
  )
}

pair_key <- function(a, b) {
  if_else(a < b, str_c(a, b, sep = "||"), str_c(b, a, sep = "||"))
}

pair_group_rows <- function(df) {
  keys <- c(
    "nct_id", "outcome_id", "measurement_stratum_id_v2", "outcome_type",
    "outcome_type_norm", "outcome_title", "outcome_description",
    "outcome_time_frame", "classification", "category",
    "measurement_title", "measurement_units", "source_row_class",
    "classifier_rule_id", "brief_title", "year", "phase", "enrollment",
    "number_of_arms", "number_of_groups", "study_type", "allocation",
    "intervention_model", "primary_purpose", "masking",
    "is_first_pass_design_eligible", "n_eligible_groups"
  )

  group_cols <- c(
    "result_group_id", "ctgov_group_code", "group_title",
    "group_description", "group_role", "group_role_source",
    "group_prefix_raw", "group_prefix_norm", "group_title_without_prefix",
    "param_value_num", "dispersion_value_num", "dispersion_lower_limit",
    "dispersion_upper_limit", "outcome_count", "group_mean_se",
    "x_eff", "x_is_fractional", "derived_from_number",
    "derived_from_percentage"
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

orient_pairs <- function(pairs, author_pairs) {
  comparator_roles <- c(
    "comparator_placebo", "comparator_sham", "comparator_no_intervention",
    "comparator_usual_care", "comparator_control", "comparator_active"
  )
  clear_comparator_roles <- setdiff(comparator_roles, "comparator_active")

  stratum_roles <- pairs %>%
    select(
      nct_id, outcome_id, measurement_stratum_id_v2, n_eligible_groups,
      result_group_id_a, group_role_a, result_group_id_b, group_role_b
    ) %>%
    tidyr::pivot_longer(
      c(result_group_id_a, result_group_id_b, group_role_a, group_role_b),
      names_to = c(".value", "pair_side"),
      names_pattern = "(result_group_id|group_role)_(a|b)"
    ) %>%
    distinct() %>%
    group_by(nct_id, outcome_id, measurement_stratum_id_v2) %>%
    summarise(
      n_clear_comparators = sum(group_role %in% clear_comparator_roles),
      n_active = sum(group_role == "active"),
      .groups = "drop"
    )

  pairs %>%
    left_join(stratum_roles, by = c("nct_id", "outcome_id", "measurement_stratum_id_v2")) %>%
    mutate(
      pair_group_key = pair_key(result_group_id_a, result_group_id_b),
      a_comparator = group_role_a %in% comparator_roles,
      b_comparator = group_role_b %in% comparator_roles,
      a_clear_comparator = group_role_a %in% clear_comparator_roles,
      b_clear_comparator = group_role_b %in% clear_comparator_roles,
      a_active = group_role_a == "active",
      b_active = group_role_b == "active"
    ) %>%
    left_join(
      author_pairs,
      by = c("nct_id", "outcome_id", "pair_group_key")
    ) %>%
    mutate(
      author_linked_pair = coalesce(author_linked_pair, FALSE),
      direction_known = (a_active & b_comparator) | (b_active & a_comparator) |
        (n_clear_comparators == 1 & (a_clear_comparator | b_clear_comparator)),
      use_a_as_treatment = case_when(
        a_active & b_comparator ~ TRUE,
        b_active & a_comparator ~ FALSE,
        n_clear_comparators == 1 & a_clear_comparator ~ FALSE,
        n_clear_comparators == 1 & b_clear_comparator ~ TRUE,
        TRUE ~ TRUE
      ),
      orientation_rule = case_when(
        a_active & b_comparator | b_active & a_comparator ~
          "explicit_active_comparator",
        n_clear_comparators == 1 & (a_clear_comparator | b_clear_comparator) ~
          "single_clear_comparator",
        TRUE ~ "unordered_pair"
      ),
      role_confidence = case_when(
        orientation_rule == "explicit_active_comparator" &
          (group_role_source_a == "prefix" | group_role_source_b == "prefix") ~
          "high",
        orientation_rule == "explicit_active_comparator" ~ "moderate",
        orientation_rule == "single_clear_comparator" &
          (a_active | b_active) ~ "high",
        orientation_rule == "single_clear_comparator" ~ "moderate",
        TRUE ~ "unknown"
      ),
      group_t_id = if_else(use_a_as_treatment, result_group_id_a, result_group_id_b),
      group_c_id = if_else(use_a_as_treatment, result_group_id_b, result_group_id_a),
      group_t_code = if_else(use_a_as_treatment, ctgov_group_code_a, ctgov_group_code_b),
      group_c_code = if_else(use_a_as_treatment, ctgov_group_code_b, ctgov_group_code_a),
      group_t_title = if_else(use_a_as_treatment, group_title_a, group_title_b),
      group_c_title = if_else(use_a_as_treatment, group_title_b, group_title_a),
      group_t_role = if_else(use_a_as_treatment, group_role_a, group_role_b),
      group_c_role = if_else(use_a_as_treatment, group_role_b, group_role_a),
      mean_t = if_else(use_a_as_treatment, param_value_num_a, param_value_num_b),
      mean_c = if_else(use_a_as_treatment, param_value_num_b, param_value_num_a),
      sd_t = if_else(use_a_as_treatment, dispersion_value_num_a, dispersion_value_num_b),
      sd_c = if_else(use_a_as_treatment, dispersion_value_num_b, dispersion_value_num_a),
      group_se_t = if_else(use_a_as_treatment, group_mean_se_a, group_mean_se_b),
      group_se_c = if_else(use_a_as_treatment, group_mean_se_b, group_mean_se_a),
      n_t = if_else(use_a_as_treatment, outcome_count_a, outcome_count_b),
      n_c = if_else(use_a_as_treatment, outcome_count_b, outcome_count_a),
      x_t = if_else(use_a_as_treatment, x_eff_a, x_eff_b),
      x_c = if_else(use_a_as_treatment, x_eff_b, x_eff_a),
      x_t_is_fractional = if_else(
        use_a_as_treatment, x_is_fractional_a, x_is_fractional_b
      ),
      x_c_is_fractional = if_else(
        use_a_as_treatment, x_is_fractional_b, x_is_fractional_a
      ),
      derived_from_number = coalesce(
        derived_from_number_a, FALSE
      ) | coalesce(derived_from_number_b, FALSE),
      derived_from_percentage = coalesce(
        derived_from_percentage_a, FALSE
      ) | coalesce(derived_from_percentage_b, FALSE),
      multi_arm_trial = n_eligible_groups > 2 |
        coalesce(number_of_arms, 0) > 2 | coalesce(number_of_groups, 0) > 2,
      pair_dependency_class = case_when(
        author_linked_pair ~ "author_selected_pair",
        !multi_arm_trial ~ "independent_two_arm",
        direction_known & (a_clear_comparator | b_clear_comparator) ~
          "multi_arm_shared_comparator",
        a_active & b_active ~ "multi_arm_active_active",
        TRUE ~ "multi_arm_unknown"
      ),
      pair_selection_rule = case_when(
        author_linked_pair ~ "author_analysis_groups_exact_pair",
        direction_known & (a_clear_comparator | b_clear_comparator) ~
          "active_vs_clear_comparator",
        !direction_known & !multi_arm_trial ~ "unordered_two_arm",
        TRUE ~ "all_pairs_diagnostic"
      ),
      validity_tier = case_when(
        direction_known & !multi_arm_trial ~ "2",
        direction_known & multi_arm_trial ~ "2b",
        TRUE ~ "3"
      )
    )
}

pair_count_summary <- function(df) {
  df %>%
    group_by(nct_id, outcome_id, measurement_stratum_id_v2) %>%
    summarise(n_eligible_groups = n_distinct(result_group_id), .groups = "drop")
}

prioritize_pairs <- function(effects) {
  effects %>%
    group_by(
      nct_id, outcome_id, measurement_stratum_id, effect_family, measure,
      derivation_rule_id
    ) %>%
    group_modify(function(x, ...) {
      if (any(x$author_linked_pair)) {
        filter(x, author_linked_pair)
      } else if (any(x$pair_selection_rule == "active_vs_clear_comparator")) {
        filter(x, pair_selection_rule == "active_vs_clear_comparator")
      } else if (any(x$pair_selection_rule == "unordered_two_arm")) {
        filter(x, pair_selection_rule == "unordered_two_arm")
      } else {
        slice(x, 0)
      }
    }) %>%
    ungroup()
}

sample_rows <- function(df, n = 100) {
  if (nrow(df) <= n) df else slice_sample(df, n = n)
}

# Load already reduced indexes -----

measurement_index_path <- file.path(input_dir, "outcome_measurements_long.rds")
if (!file.exists(measurement_index_path)) {
  measurement_index_path <- file.path(input_dir, "outcome_measurements_first_pass_long.rds")
}

message("Reading reduced outcome-measurement index: ", measurement_index_path)
measurements <- readRDS(measurement_index_path)
if (!"source_row_candidate_class" %in% names(measurements)) {
  measurements <- measurements %>%
    mutate(source_row_candidate_class = NA_character_)
}
measurements <- measurements %>%
  mutate(
    source_row_candidate_class = coalesce(
      source_row_candidate_class,
      case_when(
        param_type_norm == "mean" & dispersion_type_norm == "sd" ~
          "continuous_mean_sd",
        param_type_norm == "count_of_participants" ~ "binary_count_explicit",
        TRUE ~ NA_character_
      )
    )
  ) %>%
  filter(
    is_first_pass_design_eligible,
    outcome_type_norm == "primary",
    is_participant_count,
    !is_overall_group,
    !is_analysis_set_group,
    result_type_norm == "outcome"
  )

message("Classifying ", nrow(measurements), " primary eligible source rows")
author_links <- readRDS(file.path(input_dir, "author_analysis_links.rds"))

author_pairs <- author_links %>%
  filter(n_linked_groups == 2, !is.na(linked_result_group_ids)) %>%
  mutate(
    linked_ids = str_split(linked_result_group_ids, ";"),
    pair_group_key = map_chr(linked_ids, ~ pair_key(.x[1], .x[2])),
    author_linked_pair = TRUE
  ) %>%
  distinct(nct_id, outcome_id, pair_group_key, author_linked_pair)

group_roles <- classify_group_role(
  measurements$group_title,
  measurements$group_description
)

source_rows <- measurements %>%
  bind_cols(group_roles) %>%
  mutate(
    baseline_only_measurement =
      is_baseline_only_measurement(classification, category),
    combined_text = norm_text(str_c(
      classification, category, measurement_title, measurement_description,
      outcome_title, outcome_description,
      sep = " "
    )),
    source_family = "raw_outcome_measurement",
    effect_family = case_when(
      source_row_candidate_class %in% c(
        "continuous_mean_sd", "continuous_mean_se", "continuous_mean_ci",
        "continuous_lsm_se", "continuous_lsm_ci"
      ) ~ "continuous",
      source_row_candidate_class %in% c(
        "binary_count_explicit", "binary_number_count",
        "binary_percent_explicit"
      ) ~ "binary",
      TRUE ~ "unknown"
    ),
    ci_level = ci_level_from_dispersion(dispersion_type_norm),
    group_mean_se = case_when(
      source_row_candidate_class == "continuous_mean_sd" &
        outcome_count > 0 ~ dispersion_value_num / sqrt(outcome_count),
      source_row_candidate_class %in% c(
        "continuous_mean_se", "continuous_lsm_se"
      ) ~ dispersion_value_num,
      source_row_candidate_class %in% c(
        "continuous_mean_ci", "continuous_lsm_ci"
      ) & !is.na(ci_level) & ci_level > 0 & ci_level < 100 ~
        (dispersion_upper_limit - dispersion_lower_limit) /
        (2 * qnorm(1 - (1 - ci_level / 100) / 2)),
      TRUE ~ NA_real_
    ),
    x_eff = case_when(
      source_row_candidate_class %in% c(
        "binary_count_explicit", "binary_number_count"
      ) ~ param_value_num,
      source_row_candidate_class == "binary_percent_explicit" &
        param_value_num >= 0 & param_value_num <= 1 ~
        param_value_num * outcome_count,
      source_row_candidate_class == "binary_percent_explicit" &
        param_value_num > 1 & param_value_num <= 100 ~
        param_value_num / 100 * outcome_count,
      TRUE ~ NA_real_
    ),
    x_is_fractional = !is.na(x_eff) & abs(x_eff - round(x_eff)) > 1e-8,
    derived_from_number = source_row_candidate_class == "binary_number_count",
    derived_from_percentage =
      source_row_candidate_class == "binary_percent_explicit",
    binary_base_exclusion = binary_exclusion_reason(
      combined_text, measurement_units_norm, baseline_only_measurement
    ),
    binary_set_id = str_c(
      nct_id, outcome_id, coalesce(classification_norm, ""),
      coalesce(measurement_title_norm, ""),
      coalesce(measurement_description_norm, ""),
      coalesce(measurement_units_norm, ""),
      sep = "|"
    ),
    category_positive = positive_category(category_norm),
    category_negative = negative_category(category_norm),
    title_driven_event = binary_event_text(combined_text)
  )

complement_sets <- source_rows %>%
  filter(
    param_type_norm == "count_of_participants",
    is.na(binary_base_exclusion),
    !is.na(category_norm), category_norm != "",
    param_value_num >= 0,
    param_value_num <= outcome_count
  ) %>%
  group_by(binary_set_id, result_group_id) %>%
  summarise(
    n_categories = n_distinct(category_norm),
    sum_count = sum(param_value_num, na.rm = TRUE),
    outcome_count = first(outcome_count),
    has_positive = any(category_positive),
    has_negative = any(category_negative),
    .groups = "drop"
  ) %>%
  mutate(group_has_complement = n_categories == 2 &
    abs(sum_count - outcome_count) < 1e-8 & has_positive & has_negative)

endpoint_source_rows <- source_rows %>%
  left_join(
    complement_sets %>%
      select(binary_set_id, result_group_id, group_has_complement),
    by = c("binary_set_id", "result_group_id")
  ) %>%
  mutate(
    group_has_complement = coalesce(group_has_complement, FALSE),
    source_row_class = case_when(
      source_row_candidate_class %in% c(
        "continuous_mean_sd", "continuous_mean_se", "continuous_mean_ci",
        "continuous_lsm_se", "continuous_lsm_ci"
      ) & !baseline_only_measurement ~ source_row_candidate_class,
      param_type_norm == "count_of_participants" & !is.na(binary_base_exclusion) ~
        binary_base_exclusion,
      param_type_norm == "count_of_participants" & group_has_complement &
        category_positive ~ "binary_complement_pair",
      source_row_candidate_class == "binary_count_explicit" & title_driven_event &
        !category_negative ~ "binary_count_explicit",
      source_row_candidate_class %in% c(
        "binary_count_explicit", "binary_number_count",
        "binary_percent_explicit"
      ) & category_positive & !category_negative ~
        source_row_candidate_class,
      param_type_norm == "count_of_participants" & group_has_complement &
        category_negative ~ "excluded_complement_negative_category",
      param_type_norm == "count_of_participants" ~ "unknown",
      source_row_candidate_class %in% c(
        "binary_number_count", "binary_percent_explicit"
      ) ~ source_row_candidate_class,
      TRUE ~ "unknown"
    ),
    classifier_rule_id = case_when(
      source_row_class == "continuous_mean_sd" ~ "continuous_mean_sd",
      source_row_class == "continuous_mean_se" ~ "continuous_mean_se",
      source_row_class == "continuous_mean_ci" ~ "continuous_mean_ci",
      source_row_class == "continuous_lsm_se" ~ "continuous_lsm_se",
      source_row_class == "continuous_lsm_ci" ~ "continuous_lsm_ci",
      source_row_class == "binary_complement_pair" ~
        "binary_complement_sum_to_n",
      source_row_class == "binary_number_count" ~ "binary_number_count",
      source_row_class == "binary_percent_explicit" ~
        "binary_percent_or_proportion",
      source_row_class == "binary_count_explicit" & title_driven_event ~
        "binary_title_driven_count",
      source_row_class == "binary_count_explicit" ~
        "binary_positive_category_count",
      str_starts(source_row_class, "excluded_") ~ source_row_class,
      TRUE ~ "unclassified_existing_reduced_row"
    ),
    exclusion_reason = if_else(
      str_starts(source_row_class, "excluded_") |
        source_row_class %in% c("unknown"),
      source_row_class,
      NA_character_
    ),
    measurement_stratum_id_v2 = if_else(
      source_row_class == "binary_complement_pair",
      str_c(binary_set_id, "positive_category", category_norm, sep = "|"),
      measurement_stratum_id
    )
  )

# Continuous mean + SD effects -----

continuous_candidates <- endpoint_source_rows %>%
  filter(
    source_row_class %in% c(
      "continuous_mean_sd", "continuous_mean_se", "continuous_mean_ci",
      "continuous_lsm_se", "continuous_lsm_ci"
    ),
    !is.na(param_value_num),
    !is.na(group_mean_se),
    group_mean_se > 0,
    !is.na(outcome_count),
    outcome_count > 1,
    is_participant_count
  )

message("Pairing continuous source rows")
continuous_pairs <- continuous_candidates %>%
  left_join(
    pair_count_summary(continuous_candidates),
    by = c("nct_id", "outcome_id", "measurement_stratum_id_v2")
  ) %>%
  filter(n_eligible_groups >= 2, n_eligible_groups <= 12) %>%
  pair_group_rows() %>%
  orient_pairs(author_pairs) %>%
  mutate(
    b_md = mean_t - mean_c,
    se_md = sqrt(group_se_t^2 + group_se_c^2),
    z_md = safe_divide(b_md, se_md),
    pooled_sd = sqrt(
      ((n_t - 1) * sd_t^2 + (n_c - 1) * sd_c^2) / (n_t + n_c - 2)
    ),
    hedges_j = 1 - 3 / (4 * (n_t + n_c) - 9),
    b_smd = hedges_j * safe_divide(mean_t - mean_c, pooled_sd),
    se_smd = sqrt(
      (n_t + n_c) / (n_t * n_c) + b_smd^2 / (2 * (n_t + n_c - 2))
    ),
    z_smd = safe_divide(b_smd, se_smd),
    classification_or_category_present =
      !is.na(classification) | !is.na(category)
  )

continuous_md <- continuous_pairs %>%
  transmute(
    effect_id = make_effect_id("ctgov_raw_v2_cont_md", n()),
    nct_id, outcome_id, outcome_type, outcome_type_norm, outcome_title,
    measurement_stratum_id = measurement_stratum_id_v2,
    classification, category,
    source_family = "raw_outcome_measurement",
    source_row_class,
    effect_family = "continuous",
    measure = "mean_difference",
    b = b_md, se = se_md, z = z_md, z_operator = "=",
    p = 2 * pnorm(-abs(z_md)),
    n_t, n_c, x_t = NA_real_, x_c = NA_real_,
    mean_t, mean_c,
    sd_t = if_else(source_row_class == "continuous_mean_sd", sd_t, NA_real_),
    sd_c = if_else(source_row_class == "continuous_mean_sd", sd_c, NA_real_),
    group_t_id, group_c_id, group_t_title, group_c_title,
    group_t_role, group_c_role, direction_known, role_confidence,
    orientation_rule, pair_selection_rule, multi_arm_trial,
    pair_dependency_class, author_linked_pair, validity_tier,
    warning_flags = pmap_chr(
      list(
        direction_unknown = !direction_known,
        multi_arm_trial = multi_arm_trial,
        classification_or_category_present = classification_or_category_present,
        derived_from_standard_error =
          source_row_class %in% c("continuous_mean_se", "continuous_lsm_se"),
        derived_from_confidence_interval =
          source_row_class %in% c("continuous_mean_ci", "continuous_lsm_ci")
      ),
      warning_list
    ),
    derivation_rule_id = str_c(source_row_class, "_mean_difference"),
    year, phase, enrollment, number_of_arms, number_of_groups,
    intervention_model, masking, outcome_time_frame, measurement_title,
    measurement_units
  ) %>%
  filter(is.finite(b), is.finite(se), se > 0, is.finite(z))

continuous_smd <- continuous_pairs %>%
  filter(source_row_class == "continuous_mean_sd") %>%
  transmute(
    effect_id = make_effect_id("ctgov_raw_v2_cont_smd", n()),
    nct_id, outcome_id, outcome_type, outcome_type_norm, outcome_title,
    measurement_stratum_id = measurement_stratum_id_v2,
    classification, category,
    source_family = "raw_outcome_measurement",
    source_row_class,
    effect_family = "continuous",
    measure = "standardized_mean_difference",
    b = b_smd, se = se_smd, z = z_smd, z_operator = "=",
    p = 2 * pnorm(-abs(z_smd)),
    n_t, n_c, x_t = NA_real_, x_c = NA_real_,
    mean_t, mean_c, sd_t, sd_c,
    group_t_id, group_c_id, group_t_title, group_c_title,
    group_t_role, group_c_role, direction_known, role_confidence,
    orientation_rule, pair_selection_rule, multi_arm_trial,
    pair_dependency_class, author_linked_pair, validity_tier,
    warning_flags = pmap_chr(
      list(
        direction_unknown = !direction_known,
        multi_arm_trial = multi_arm_trial,
        classification_or_category_present = classification_or_category_present
      ),
      warning_list
    ),
    derivation_rule_id = "continuous_mean_sd_hedges_g",
    year, phase, enrollment, number_of_arms, number_of_groups,
    intervention_model, masking, outcome_time_frame, measurement_title,
    measurement_units
  ) %>%
  filter(is.finite(b), is.finite(se), se > 0, is.finite(z))

# Binary count effects -----

binary_candidates <- endpoint_source_rows %>%
  filter(
    source_row_class %in% c(
      "binary_count_explicit", "binary_complement_pair",
      "binary_number_count", "binary_percent_explicit"
    ),
    !is.na(x_eff),
    !is.na(outcome_count),
    outcome_count > 0,
    x_eff >= 0,
    x_eff <= outcome_count,
    source_row_class != "binary_number_count" |
      abs(x_eff - round(x_eff)) < 1e-8,
    is_participant_count
  )

message("Pairing binary source rows")
binary_pairs <- binary_candidates %>%
  left_join(
    pair_count_summary(binary_candidates),
    by = c("nct_id", "outcome_id", "measurement_stratum_id_v2")
  ) %>%
  filter(n_eligible_groups >= 2, n_eligible_groups <= 12) %>%
  pair_group_rows() %>%
  orient_pairs(author_pairs) %>%
  mutate(
    p_t = x_t / n_t,
    p_c = x_c / n_c,
    zero_or_boundary_cell =
      x_t == 0 | x_c == 0 | x_t == n_t | x_c == n_c,
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

binary_effect <- function(pairs, measure_name, b_col, se_col, z_col, rule_id) {
  pairs %>%
    transmute(
      effect_id = make_effect_id(str_c("ctgov_raw_v2_bin_", rule_id), n()),
      nct_id, outcome_id, outcome_type, outcome_type_norm, outcome_title,
      measurement_stratum_id = measurement_stratum_id_v2,
      classification, category,
      source_family = "raw_outcome_measurement",
      source_row_class,
      effect_family = "binary",
      measure = measure_name,
      b = .data[[b_col]], se = .data[[se_col]], z = .data[[z_col]],
      z_operator = "=",
      p = 2 * pnorm(-abs(.data[[z_col]])),
      n_t, n_c, x_t, x_c,
      mean_t = NA_real_, mean_c = NA_real_, sd_t = NA_real_, sd_c = NA_real_,
      group_t_id, group_c_id, group_t_title, group_c_title,
      group_t_role, group_c_role, direction_known, role_confidence,
      orientation_rule, pair_selection_rule, multi_arm_trial,
      pair_dependency_class, author_linked_pair, validity_tier,
      warning_flags = pmap_chr(
        list(
          direction_unknown = !direction_known,
          multi_arm_trial = multi_arm_trial,
          zero_or_boundary_cell = zero_or_boundary_cell,
          event_category_chosen_from_complement =
            source_row_class == "binary_complement_pair",
          derived_from_number = derived_from_number,
          derived_from_percentage = derived_from_percentage,
          x_is_fractional = x_t_is_fractional | x_c_is_fractional
        ),
        warning_list
      ),
      derivation_rule_id = str_c(classifier_rule_id, "_", rule_id),
      year, phase, enrollment, number_of_arms, number_of_groups,
      intervention_model, masking, outcome_time_frame, measurement_title,
      measurement_units
    ) %>%
    filter(is.finite(b), is.finite(se), se > 0, is.finite(z))
}

binary_effects <- bind_rows(
  binary_effect(binary_pairs, "risk_difference", "rd", "se_rd", "z_rd", "rd"),
  binary_effect(binary_pairs, "log_risk_ratio", "log_rr", "se_log_rr", "z_log_rr", "rr"),
  binary_effect(binary_pairs, "log_odds_ratio", "log_or", "se_log_or", "z_log_or", "or")
)

raw_endpoint_effect_pairs <- bind_rows(
  continuous_md,
  continuous_smd,
  binary_effects
) %>%
  arrange(nct_id, outcome_id, measurement_stratum_id, effect_family, measure) %>%
  mutate(effect_id = make_effect_id("ctgov_raw_endpoint_v2_all", n()))

raw_endpoint_effects <- raw_endpoint_effect_pairs %>%
  prioritize_pairs() %>%
  arrange(nct_id, outcome_id, measurement_stratum_id, effect_family, measure) %>%
  mutate(effect_id = make_effect_id("ctgov_raw_endpoint_v2", n()))

# Validation and audit outputs -----

author_overlap_validation <- raw_endpoint_effect_pairs %>%
  filter(author_linked_pair) %>%
  left_join(
    author_links %>%
      filter(n_linked_groups == 2) %>%
      mutate(
        linked_ids = str_split(linked_result_group_ids, ";"),
        pair_group_key = map_chr(linked_ids, ~ pair_key(.x[1], .x[2]))
      ),
    by = c("nct_id", "outcome_id"),
    relationship = "many-to-many"
  ) %>%
  filter(pair_key(group_t_id, group_c_id) == pair_group_key) %>%
  mutate(
    p_for_z = if_else(p_value_num > 0 & p_value_num <= 1, p_value_num, NA_real_),
    author_z_from_p = qnorm(1 - p_for_z / 2),
    valid_author_ci = !is.na(analysis_param_value_num) &
      !is.na(ci_lower_limit) & !is.na(ci_upper_limit) &
      !is.na(ci_percent) & ci_percent > 0 & ci_percent < 100 &
      ci_upper_limit > ci_lower_limit,
    author_ci_z = qnorm(
      1 - (1 - if_else(valid_author_ci, ci_percent, NA_real_) / 100) / 2
    ),
    author_z_from_ci = if_else(
      valid_author_ci,
      analysis_param_value_num /
        ((ci_upper_limit - ci_lower_limit) /
          (2 * author_ci_z)),
      NA_real_
    ),
    author_z = coalesce(author_z_from_ci, author_z_from_p),
    abs_z_difference = abs(abs(z) - abs(author_z)),
    p_side_agrees_005 = (p < 0.05) == (p_value_num < 0.05),
    notes = if_else(
      is.na(author_z),
      "author_z_not_recovered",
      "diagnostic_overlap_only"
    )
  ) %>%
  transmute(
    nct_id, outcome_id, outcome_analysis_id, raw_measure = measure,
    raw_source_row_class = source_row_class,
    raw_b = b, raw_se = se, raw_z = z,
    author_param_type = analysis_param_type,
    author_param_value = analysis_param_value,
    author_p = p_value_num,
    author_ci_lower = ci_lower_limit,
    author_ci_upper = ci_upper_limit,
    author_z, method, same_sign = NA,
    abs_z_difference, p_side_agrees_005, notes
  )

endpoint_summary <- raw_endpoint_effects %>%
  count(
    source_family, source_row_class, effect_family, measure, validity_tier,
    name = "n"
  ) %>%
  arrange(source_family, source_row_class, effect_family, measure, validity_tier)

binary_classifier_stage_counts <- endpoint_source_rows %>%
  filter(effect_family == "binary" | str_starts(source_row_class, "excluded_")) %>%
  count(
    param_type_norm, source_row_class, classifier_rule_id, outcome_type_norm,
    name = "n"
  ) %>%
  arrange(desc(n))

continuous_classifier_stage_counts <- endpoint_source_rows %>%
  filter(effect_family == "continuous") %>%
  count(
    param_type_norm, source_row_class, dispersion_type_norm,
    outcome_type_norm, name = "n"
  ) %>%
  arrange(desc(n))

orientation_stage_counts <- raw_endpoint_effect_pairs %>%
  count(
    direction_known, orientation_rule, pair_selection_rule,
    pair_dependency_class, validity_tier, name = "n"
  ) %>%
  arrange(desc(n))

review_cols <- c(
  "nct_id", "outcome_id", "outcome_title", "outcome_time_frame",
  "classification", "category", "measurement_title", "measurement_units",
  "group_t_title", "group_c_title", "group_t_role", "group_c_role",
  "direction_known", "orientation_rule", "pair_selection_rule",
  "pair_dependency_class", "measure", "b", "se", "z", "n_t", "n_c",
  "x_t", "x_c", "warning_flags", "derivation_rule_id"
)

manual_review_binary_kept <- raw_endpoint_effects %>%
  filter(effect_family == "binary") %>%
  select(any_of(review_cols)) %>%
  sample_rows()

manual_review_binary_excluded <- endpoint_source_rows %>%
  filter(
    effect_family == "binary" | str_starts(source_row_class, "excluded_"),
    !source_row_class %in% c(
      "binary_count_explicit", "binary_complement_pair",
      "binary_number_count", "binary_percent_explicit"
    )
  ) %>%
  select(
    nct_id, outcome_id, outcome_title, outcome_time_frame, classification,
    category, measurement_title, measurement_description, measurement_units,
    param_value_num, outcome_count, source_row_class, classifier_rule_id,
    exclusion_reason
  ) %>%
  sample_rows()

manual_review_by_source_class <- raw_endpoint_effects %>%
  filter(source_row_class %in% c(
    "continuous_mean_se", "continuous_mean_ci", "continuous_lsm_se",
    "continuous_lsm_ci", "binary_number_count", "binary_percent_explicit"
  )) %>%
  select(any_of(review_cols)) %>%
  group_by(derivation_rule_id) %>%
  group_modify(~ sample_rows(.x, 25)) %>%
  ungroup()

manual_review_direction <- raw_endpoint_effect_pairs %>%
  select(any_of(review_cols)) %>%
  group_by(direction_known) %>%
  group_modify(~ sample_rows(.x, 50)) %>%
  ungroup()

# Save -----

source_rows_out <- endpoint_source_rows %>%
  rename(endpoint_measurement_stratum_id = measurement_stratum_id_v2)

saveRDS(source_rows_out, file.path(out_dir, "endpoint_source_rows.rds"))
saveRDS(
  raw_endpoint_effect_pairs,
  file.path(out_dir, "raw_endpoint_effect_pairs.rds")
)
saveRDS(raw_endpoint_effects, file.path(out_dir, "raw_endpoint_effects.rds"))

write_csv(endpoint_summary, file.path(validation_dir, "endpoint_effects_summary.csv"))
write_csv(
  binary_classifier_stage_counts,
  file.path(validation_dir, "binary_classifier_stage_counts.csv")
)
write_csv(
  continuous_classifier_stage_counts,
  file.path(validation_dir, "continuous_classifier_stage_counts.csv")
)
write_csv(
  orientation_stage_counts,
  file.path(validation_dir, "orientation_stage_counts.csv")
)
write_csv(
  author_overlap_validation,
  file.path(validation_dir, "author_overlap_validation.csv")
)
write_csv(
  manual_review_binary_kept,
  file.path(manual_review_dir, "sample_binary_kept.csv")
)
write_csv(
  manual_review_binary_excluded,
  file.path(manual_review_dir, "sample_binary_excluded.csv")
)
write_csv(
  manual_review_direction,
  file.path(manual_review_dir, "sample_direction.csv")
)
write_csv(
  manual_review_by_source_class,
  file.path(manual_review_dir, "sample_source_classes.csv")
)

message("Saved raw endpoint effects to ", out_dir)
