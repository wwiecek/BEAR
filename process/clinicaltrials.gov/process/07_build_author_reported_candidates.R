# Build BEAR-candidate author-reported ClinicalTrials.gov effects.
# Reads the reduced author analysis table written by step 00.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)

source("process/clinicaltrials.gov/lib/paths.R")
source("process/clinicaltrials.gov/lib/non_inferiority.R")
source("R/z_derivation_helpers.R")

author_raw_path <- file.path(ctgov_derived_dir, "author_reported_raw.rds")
legacy_author_raw_path <- file.path(ctgov_derived_dir, "clinicaltrials.gov_aug2025.rds")
author_candidate_path <- file.path(ctgov_derived_dir, "author_reported_candidates.rds")
ni_descriptions_path <- file.path(ctgov_intermediate_dir, "non_inferiority_descriptions.rds")

# The legacy file name is read only for setups prepared before step 00 existed;
# both are removed when the snapshot changes, so neither can be stale.
if (file.exists(author_raw_path)) {
  author_raw <- readRDS(author_raw_path)
} else if (file.exists(legacy_author_raw_path)) {
  author_raw <- readRDS(legacy_author_raw_path)
  saveRDS(author_raw, author_raw_path)
} else {
  stop(
    "Missing author raw artifact. Run ",
    "process/clinicaltrials.gov/process/00_prepare_author_analyses.R first."
  )
}

author_links_path <- file.path(ctgov_intermediate_dir, "author_analysis_links.rds")
author_links <- if (file.exists(author_links_path)) {
  readRDS(author_links_path) %>%
    mutate(
      linked_ids = str_split(coalesce(linked_result_group_ids, ""), ";"),
      author_group_pair_key = if_else(
        n_linked_groups == 2,
        map_chr(linked_ids, ~ str_c(sort(.x), collapse = "||")),
        NA_character_
      )
    ) %>%
    select(
      nct_id, outcome_analysis_id, linked_result_group_ids,
      linked_group_codes, n_linked_groups, author_group_pair_key, method
    ) %>%
    distinct()
} else {
  tibble(
    nct_id = character(), outcome_analysis_id = character(),
    linked_result_group_ids = character(), linked_group_codes = character(),
    n_linked_groups = integer(), author_group_pair_key = character(),
    method = character()
  )
}

eff_clean <- author_raw %>%
  rename(lower = ci_lower_limit, upper = ci_upper_limit, estimate = param_value) %>%
  mutate(
    across(c(lower, upper, estimate), as.numeric),
    outcome_id = as.character(outcome_id),
    outcome_analysis_id = as.character(outcome_analysis_id)
  ) %>%
  filter(overall_status == "COMPLETED", outcome_type == "PRIMARY")

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
    year = year(completion_date),
    author_effect_id = str_c("ctgov_author_", outcome_analysis_id),
    source_path = "author_reported_outcome_analyses",
    import_source = "author_reported",
    effect_id = author_effect_id
  ) %>%
  left_join(author_links, by = c("nct_id", "outcome_analysis_id")) %>%
  select(
    effect_id, author_effect_id, import_source, source_path,
    nct_id, outcome_id, outcome_analysis_id, linked_result_group_ids,
    linked_group_codes, n_linked_groups, author_group_pair_key,
    brief_title, year, enrollment, allocation, study_type, phase,
    intervention_model, observational_model, primary_purpose, masking,
    outcome_type, outcome_title, outcome_time_frame, method,
    param_type, estimate, lower, upper, p_value, p_value_modifier,
    ci_percent, ci_n_sides, non_inferiority_type, non_inferiority_description,
    measure_class, percent_rescaled, scale,
    effect, b, se, z, z_ci, z_p, z_source, use_ci, sym_ratio,
    z_operator, p_sides
  ) %>%
  filter(!is.na(z))

# Rule-based test type and margin from non_inferiority_type and
# non_inferiority_description; recorded manual review decisions are applied
# in step 10. The description-level table feeds that review
ni_descriptions <- ctgov_ni_classify_descriptions(z_table)

z_table <- z_table %>%
  left_join(ni_descriptions %>% select(non_inferiority_description, rule_type, rule_margin),
            by = "non_inferiority_description") %>%
  mutate(
    non_inferior_desc_exists = !is.na(non_inferiority_description) &
      str_trim(non_inferiority_description) != "",
    non_inferior_type_constructed = ctgov_ni_resolve_type(
      rule_type, non_inferiority_type, non_inferior_desc_exists),
    non_inferior_margin = if_else(non_inferior_desc_exists, rule_margin, NA_character_)
  ) %>%
  select(-rule_type, -rule_margin)

validation_checks <- tibble::tibble(
  check = c(
    "required_columns",
    "valid_z_operator_values",
    "no_duplicate_outcome_analysis_id",
    "all_saved_rows_have_z",
    "valid_non_inferior_type_constructed",
    "margin_only_with_description"
  ),
  passed = c(
    all(c(
      "nct_id", "outcome_id", "outcome_analysis_id", "measure_class",
      "b", "se", "z", "z_operator"
    ) %in% names(z_table)),
    all(is.na(z_table$z_operator) | z_table$z_operator %in% c("=", "<", ">")),
    anyDuplicated(z_table %>% select(nct_id, outcome_analysis_id)) == 0,
    all(!is.na(z_table$z)),
    all(z_table$non_inferior_type_constructed %in% ctgov_ni_types),
    all(z_table$non_inferior_desc_exists | is.na(z_table$non_inferior_margin))
  )
)

print(validation_checks, n = nrow(validation_checks), width = Inf)
stopifnot(all(validation_checks$passed))

saveRDS(z_table, author_candidate_path)
saveRDS(ni_descriptions, ni_descriptions_path)
message("Saved ", nrow(z_table), " author-reported candidate rows; ",
        nrow(ni_descriptions), " distinct non-inferiority descriptions.")
