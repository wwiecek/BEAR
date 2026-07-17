# Merge author-reported and selected raw-derived ClinicalTrials.gov effects.
# Author rows are preferred for linked overlaps; selected raw-only rows fill gaps.

library(dplyr)
library(readr)
library(stringr)

source("process/clinicaltrials.gov/lib/paths.R")

author_path <- file.path(ctgov_derived_dir, "author_reported_candidates.rds")
raw_path <- file.path(ctgov_derived_dir, "raw_endpoint_selected_for_bear.rds")
trial_characteristics_path <- file.path(ctgov_derived_dir, "trial_characteristics.rds")
merged_audit_path <- file.path(ctgov_derived_dir, "clinicaltrialsgov_merged_candidates.rds")
merged_summary_path <- file.path(ctgov_validation_dir, "clinicaltrialsgov_merge_summary.csv")
row_cutoff_impact_path <- file.path(
  ctgov_validation_dir, "clinicaltrialsgov_row_count_cutoff_impact.csv"
)
out_path <- "data/clinicaltrialsgov.rds"

author <- readRDS(author_path)
selected_raw <- readRDS(raw_path)
trial_characteristics <- readRDS(trial_characteristics_path)

raw_overlap_keys <- selected_raw %>%
  transmute(
    effect_id,
    nct_id,
    outcome_id = as.character(outcome_id),
    raw_group_pair_key,
    raw_overlap_key = case_when(
      !is.na(raw_group_pair_key) ~ str_c(nct_id, outcome_id, raw_group_pair_key, sep = "||"),
      TRUE ~ NA_character_
    )
  )

author_overlap_keys <- author %>%
  transmute(
    author_effect_id,
    nct_id,
    outcome_id = as.character(outcome_id),
    outcome_analysis_id = as.character(outcome_analysis_id),
    author_group_pair_key,
    author_overlap_key = case_when(
      !is.na(author_group_pair_key) ~ str_c(nct_id, outcome_id, author_group_pair_key, sep = "||"),
      TRUE ~ NA_character_
    )
  )

raw_author_overlap <- raw_overlap_keys %>%
  left_join(
    author_overlap_keys %>%
      filter(!is.na(author_overlap_key)) %>%
      distinct(author_overlap_key) %>%
      mutate(has_author_pair_overlap = TRUE),
    by = c("raw_overlap_key" = "author_overlap_key")
  ) %>%
  mutate(
    has_author_pair_overlap = coalesce(has_author_pair_overlap, FALSE),
    overlap_reason = if_else(
      has_author_pair_overlap,
      "author_preferred_same_outcome_and_result_group_pair",
      NA_character_
    )
  )

raw_for_import <- selected_raw %>%
  left_join(
    raw_author_overlap %>% select(effect_id, has_author_pair_overlap, overlap_reason),
    by = "effect_id"
  ) %>%
  mutate(
    has_author_pair_overlap = coalesce(has_author_pair_overlap, FALSE),
    excluded_by_author_preference = has_author_pair_overlap,
    import_decision = if_else(
      excluded_by_author_preference,
      "exclude_author_preferred_overlap",
      "include_raw_only"
    )
  )

raw_overlap_match_counts <- raw_for_import %>%
  filter(!is.na(raw_group_pair_key)) %>%
  count(nct_id, outcome_id = as.character(outcome_id), raw_group_pair_key,
        name = "n_raw_overlap_matches")

author_import <- author %>%
  left_join(
    raw_overlap_match_counts %>%
      rename(author_group_pair_key = raw_group_pair_key),
    by = c("nct_id", "outcome_id", "author_group_pair_key")
  ) %>%
  mutate(
    import_decision = "include_author_preferred",
    include_in_bear = TRUE,
    excluded_by_author_preference = FALSE,
    overlap_reason = NA_character_,
    n_raw_overlap_matches = coalesce(n_raw_overlap_matches, 0L),
    author_raw_overlap_key = case_when(
      !is.na(author_group_pair_key) ~
        str_c(nct_id, outcome_id, author_group_pair_key, sep = "||"),
      TRUE ~ NA_character_
    ),
    raw_effect_id = NA_character_, raw_measure = NA_character_,
    raw_effect_family = NA_character_, raw_group_pair_key = NA_character_,
    raw_event_t = NA_real_, raw_event_c = NA_real_,
    raw_n_t = NA_real_, raw_n_c = NA_real_,
    raw_mean_t = NA_real_, raw_mean_c = NA_real_,
    raw_sd_t = NA_real_, raw_sd_c = NA_real_,
    raw_group_t_id = NA_character_, raw_group_c_id = NA_character_,
    raw_group_t_title = NA_character_, raw_group_c_title = NA_character_,
    raw_group_t_role = NA_character_, raw_group_c_role = NA_character_,
    raw_role_confidence = NA_character_,
    raw_pair_dependency_class = NA_character_,
    raw_multi_arm_trial = NA,
    measurement_stratum_id = NA_character_, measurement_title = NA_character_,
    measurement_units = NA_character_, validity_tier = NA_character_,
    warning_flags = NA_character_, derivation_rule_id = NA_character_,
    direction_unknown = NA
  )

raw_import <- raw_for_import %>%
  mutate(
    raw_effect_id = effect_id,
    author_effect_id = NA_character_,
    n_raw_overlap_matches = NA_integer_,
    include_in_bear = import_decision == "include_raw_only",
    author_raw_overlap_key = case_when(
      !is.na(raw_group_pair_key) ~
        str_c(nct_id, outcome_id, raw_group_pair_key, sep = "||"),
      TRUE ~ NA_character_
    ),
    outcome_analysis_id = NA_character_,
    linked_result_group_ids = NA_character_, linked_group_codes = NA_character_,
    n_linked_groups = NA_integer_, author_group_pair_key = NA_character_,
    param_type = NA_character_, estimate = NA_real_, lower = NA_real_,
    upper = NA_real_, p_value = NA_real_, p_value_modifier = NA_character_,
    ci_percent = NA_real_, ci_n_sides = NA_integer_, z_ci = NA_real_,
    z_p = NA_real_, z_source = "raw_endpoint_formula", use_ci = NA,
    sym_ratio = NA_real_, p_sides = 2L, study_type = "INTERVENTIONAL",
    primary_purpose = NA_character_, observational_model = NA_character_,
    raw_group_t_role = group_t_role,
    raw_group_c_role = group_c_role,
    direction_unknown = !direction_known,
    raw_role_confidence = role_confidence,
    raw_pair_dependency_class = pair_dependency_class,
    raw_multi_arm_trial = multi_arm_trial
  )

common_cols <- union(names(author_import), names(raw_import))
add_missing_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  for (col in missing) df[[col]] <- NA
  df[, cols]
}

merged <- bind_rows(
  add_missing_cols(author_import, common_cols),
  add_missing_cols(raw_import, common_cols)
) %>%
  mutate(
    effect = b,
    effect_source = import_source,
    clinicaltrials_merge_source = import_source,
    clinicaltrials_import_decision = import_decision
  ) %>%
  arrange(nct_id, outcome_id, desc(import_source == "author_reported"), effect_id)

study_multiplicity <- merged %>%
  group_by(nct_id) %>%
  summarise(
    n_effect_rows_per_study = n(),
    n_outcomes_per_study = n_distinct(outcome_id),
    .groups = "drop"
  )

study_source_rows <- merged %>%
  filter(include_in_bear) %>%
  count(nct_id, effect_source, name = "n") %>%
  tidyr::pivot_wider(
    names_from = effect_source, values_from = n, values_fill = 0,
    names_prefix = "rows_"
  )

row_cutoff_impact <- tibble::tibble(
  row_count_cutoff = c(19, 49, 99, 149, 199, 299, 499, 999)
) %>%
  tidyr::crossing(
    study_multiplicity %>%
      left_join(study_source_rows, by = "nct_id") %>%
      mutate(
        rows_author_reported = coalesce(rows_author_reported, 0L),
        rows_raw_derived = coalesce(rows_raw_derived, 0L)
      )
  ) %>%
  mutate(remove_study = n_effect_rows_per_study > row_count_cutoff) %>%
  group_by(row_count_cutoff) %>%
  summarise(
    studies_removed = sum(remove_study),
    rows_removed = sum(n_effect_rows_per_study[remove_study]),
    included_author_rows_removed = sum(rows_author_reported[remove_study]),
    included_raw_rows_removed = sum(rows_raw_derived[remove_study]),
    pct_rows_removed = rows_removed / nrow(merged),
    rows_retained = nrow(merged) - rows_removed,
    .groups = "drop"
  )

merge_summary <- tibble::tibble(
  check = c(
    "author_rows",
    "selected_raw_rows",
    "raw_author_overlaps_marked_not_in_bear",
    "raw_only_rows_included_in_bear",
    "final_candidate_rows"
  ),
  n = c(
    nrow(author),
    nrow(selected_raw),
    sum(raw_for_import$excluded_by_author_preference),
    sum(raw_for_import$import_decision == "include_raw_only"),
    nrow(merged)
  )
)

stopifnot(
  nrow(merged) == nrow(author) + nrow(selected_raw),
  all(author$effect_id %in% merged$effect_id),
  all(raw_for_import$overlap_reason[raw_for_import$excluded_by_author_preference] ==
        "author_preferred_same_outcome_and_result_group_pair"),
  all(c("nct_id", "study_type", "allocation", "measure_class", "z", "b", "se",
        "z_operator", "raw_event_t", "raw_event_c", "raw_n_t", "raw_n_c",
        "raw_mean_t", "raw_mean_c", "raw_sd_t", "raw_sd_c") %in% names(merged))
)

saveRDS(merged, merged_audit_path)
write_csv(merge_summary, merged_summary_path)
write_csv(row_cutoff_impact, row_cutoff_impact_path)

trial_characteristic_cols <- c(
  "nct_id", "brief_title", "official_title", "study_type", "phase",
  "overall_status", "enrollment", "enrollment_type", "number_of_arms",
  "number_of_groups", "results_first_posted_year", "primary_completion_year",
  "allocation", "intervention_model",
  "primary_purpose", "masking", "is_factorial", "is_crossover",
  "n_design_groups", "has_placebo_comparator",
  "comparator_summary", "domain_all", "domain_n",
  "domain_source", "domain_oncology", "domain_primary",
  "intervention_types_all", "intervention_type_n",
  "intervention_type_primary", "is_drug_trial", "is_biological_trial",
  "is_device_trial", "is_procedure_trial", "is_behavioral_trial",
  "is_vaccine_trial", "is_cell_or_gene_therapy_trial",
  "is_diagnostic_trial", "lead_sponsor_name", "lead_sponsor_class",
  "has_industry_sponsor", "is_multi_arm", "has_baseline_measurements"
)

public_cols <- c(
  "effect_id", "author_effect_id", "raw_effect_id", "effect_source",
  "include_in_bear", "import_decision", "author_raw_overlap_key", "nct_id",
  "outcome_id", "outcome_analysis_id",
  "brief_title", "official_title", "study_type", "phase", "overall_status",
  "year", "enrollment", "enrollment_type", "number_of_arms",
  "number_of_groups", "results_first_posted_year", "primary_completion_year",
  "n_effect_rows_per_study", "n_outcomes_per_study",
  "allocation", "intervention_model",
  "primary_purpose", "observational_model", "masking", "is_factorial",
  "is_crossover", "n_design_groups", "has_placebo_comparator",
  "comparator_summary", "is_multi_arm", "has_baseline_measurements",
  "domain_all", "domain_n", "domain_source", "domain_oncology",
  "domain_primary", "intervention_types_all", "intervention_type_n",
  "intervention_type_primary", "is_drug_trial", "is_biological_trial",
  "is_device_trial", "is_procedure_trial", "is_behavioral_trial",
  "is_vaccine_trial", "is_cell_or_gene_therapy_trial",
  "is_diagnostic_trial", "lead_sponsor_name", "lead_sponsor_class",
  "has_industry_sponsor",
  "outcome_type", "outcome_title", "outcome_time_frame",
  "measurement_stratum_id", "measurement_title", "measurement_units",
  "measure_class", "measure_detailed", "scale", "effect", "b", "se", "z", "z_operator",
  "linked_result_group_ids", "linked_group_codes",
  "n_linked_groups", "author_group_pair_key", "method", "param_type",
  "estimate", "lower", "upper", "p_value", "p_value_modifier",
  "ci_percent", "ci_n_sides", "raw_measure", "raw_effect_family",
  "n_raw_overlap_matches",
  "raw_event_t", "raw_event_c", "raw_n_t", "raw_n_c",
  "raw_mean_t", "raw_mean_c", "raw_sd_t", "raw_sd_c",
  "raw_group_t_id", "raw_group_c_id",
  "raw_group_t_title", "raw_group_c_title",
  "direction_unknown", "raw_multi_arm_trial"
)

public_trial_characteristics <- trial_characteristics %>%
  select(any_of(trial_characteristic_cols)) %>%
  distinct(nct_id, .keep_all = TRUE) %>%
  rename_with(
    ~ paste0(.x, "_trial"),
    any_of(setdiff(intersect(trial_characteristic_cols, names(merged)), "nct_id"))
  )

normalise_trial_phase <- function(x) {
  x <- str_to_upper(x)
  x <- str_replace_all(x, "PHASE1_PHASE2", "PHASE1/PHASE2")
  str_replace_all(x, "PHASE2_PHASE3", "PHASE2/PHASE3")
}

public <- merged %>%
  left_join(study_multiplicity, by = "nct_id") %>%
  left_join(public_trial_characteristics, by = "nct_id") %>%
  mutate(
    brief_title = coalesce(brief_title, brief_title_trial),
    study_type = coalesce(study_type, str_to_upper(study_type_trial)),
    phase = coalesce(phase, normalise_trial_phase(phase_trial)),
    enrollment = coalesce(enrollment, enrollment_trial),
    number_of_arms = coalesce(number_of_arms, number_of_arms_trial),
    number_of_groups = coalesce(number_of_groups, number_of_groups_trial),
    allocation = coalesce(allocation, str_to_upper(allocation_trial)),
    intervention_model = coalesce(
      intervention_model, str_to_upper(intervention_model_trial)
    ),
    primary_purpose = coalesce(primary_purpose, str_to_upper(primary_purpose_trial)),
    masking = coalesce(masking, str_to_upper(masking_trial)),
    year = coalesce(year, primary_completion_year, results_first_posted_year),
    measure_detailed = case_when(
      measure_class == "Standardized Mean Difference" &
        effect_source == "raw_derived" ~ "SMD (Hedges' g)",
      measure_class == "Standardized Mean Difference" &
        effect_source == "author_reported" ~ "SMD",
      TRUE ~ measure_class
    )
  ) %>%
  select(all_of(public_cols))

stopifnot(
  all(c("nct_id", "study_type", "allocation", "measure_class", "z", "b",
        "se", "z_operator", "raw_event_t", "raw_event_c", "raw_n_t",
        "raw_n_c", "raw_mean_t", "raw_mean_c", "raw_sd_t", "raw_sd_c") %in%
        names(public)),
  !anyDuplicated(names(public)),
  all(public$measure_detailed[which(
    public$measure_class == "Standardized Mean Difference" &
      public$effect_source == "raw_derived"
  )] == "SMD (Hedges' g)"),
  all(public$measure_detailed[which(
    public$measure_class == "Standardized Mean Difference" &
      public$effect_source == "author_reported"
  )] == "SMD")
)

saveRDS(public, out_path)

message("Saved merged ClinicalTrials.gov audit file with ", nrow(merged), " rows.")
message("Saved public ClinicalTrials.gov file with ", nrow(public), " rows and ",
        ncol(public), " columns.")
