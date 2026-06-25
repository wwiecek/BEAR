# Validate the final author-preferred ClinicalTrials.gov merge.

library(dplyr)
library(readr)
library(stringr)

source("process/clinicaltrials.gov/lib/paths.R")

author <- readRDS(file.path(ctgov_derived_dir, "author_reported_candidates.rds"))
selected_raw <- readRDS(file.path(ctgov_derived_dir, "raw_endpoint_selected_for_bear.rds"))
merged <- readRDS(file.path(ctgov_derived_dir, "clinicaltrialsgov_merged_candidates.rds"))
public <- readRDS("data/clinicaltrialsgov.rds")
dictionary <- read_csv(
  "doc/datasets/clinicaltrials_gov_dictionary.csv",
  show_col_types = FALSE
)
comparison <- read_csv(
  file.path(ctgov_validation_dir, "raw_scale_comparison_summary.csv"),
  show_col_types = FALSE
)
author_overlap <- read_csv(
  file.path(ctgov_manual_review_dir, "author_overlap.csv"),
  show_col_types = FALSE
)
raw_pairs <- readRDS(file.path(ctgov_derived_dir, "raw_endpoint_effect_pairs.rds"))

merge_summary <- read_csv(
  file.path(ctgov_validation_dir, "clinicaltrialsgov_merge_summary.csv"),
  show_col_types = FALSE
)
row_cutoff_impact <- read_csv(
  file.path(ctgov_validation_dir, "clinicaltrialsgov_row_count_cutoff_impact.csv"),
  show_col_types = FALSE
)

dictionary_groups <- c(
  "Row and source identifiers",
  "Study characteristics",
  "Trial categories",
  "Outcome descriptors",
  "BEAR effect fields",
  "Author-reported analysis inputs",
  "Raw endpoint calculation inputs",
  "Raw result-group metadata"
)

expected_public_cols <- dictionary$variable
bear_input <- public %>%
  filter(include_in_bear) %>%
  filter(study_type == "INTERVENTIONAL" & allocation == "RANDOMIZED") %>%
  filter(n_effect_rows_per_study < 20)

scale_aware_overlap <- author_overlap %>%
  mutate(
    author_param_type_norm = str_squish(str_to_lower(coalesce(author_param_type, ""))),
    comparable_scale = case_when(
      raw_measure == "mean_difference" &
        str_detect(author_param_type_norm, "mean.*difference|ls.*mean|least square") ~
        "author_mean_difference_vs_raw_mean_difference",
      raw_measure == "standardized_mean_difference" &
        str_detect(author_param_type_norm, "standard|smd|cohen|hedges") ~
        "author_smd_like_vs_raw_smd",
      raw_measure == "log_odds_ratio" &
        str_detect(author_param_type_norm, "odds\\s*ratio|\\bor\\b") ~
        "author_log_or_vs_raw_log_or",
      raw_measure == "log_risk_ratio" &
        str_detect(author_param_type_norm, "risk\\s*ratio|relative\\s*risk|\\brr\\b") ~
        "author_log_rr_vs_raw_log_rr",
      raw_measure == "risk_difference" &
        str_detect(author_param_type_norm, "risk\\s*difference|difference.*percent|difference.*proportion") ~
        "author_risk_difference_vs_raw_risk_difference",
      TRUE ~ NA_character_
    ),
    scale_comparison_status = if_else(
      is.na(comparable_scale), "not_scale_comparable", "scale_comparable"
    )
  )

scale_aware_summary <- scale_aware_overlap %>%
  count(
    scale_comparison_status, comparable_scale, author_overlap_band,
    name = "n"
  ) %>%
  arrange(scale_comparison_status, comparable_scale, author_overlap_band)

scale_aware_disagreements <- scale_aware_overlap %>%
  filter(
    scale_comparison_status == "scale_comparable",
    author_overlap_band %in% c("moderate_disagreement", "large_disagreement")
  ) %>%
  left_join(
    raw_pairs %>%
      select(
        raw_effect_id = effect_id,
        n_t, n_c, x_t, x_c, mean_t, mean_c, sd_t, sd_c,
        group_t_title, group_c_title, warning_flags
      ),
    by = "raw_effect_id"
  ) %>%
  arrange(desc(abs_z_difference), nct_id, outcome_id)

write_csv(
  scale_aware_summary,
  file.path(ctgov_validation_dir, "author_raw_scale_aware_summary.csv")
)
write_csv(
  scale_aware_disagreements,
  file.path(ctgov_manual_review_dir, "author_raw_scale_aware_disagreements.csv")
)

checks <- tibble::tibble(
  check = c(
    "no_duplicate_author_keys",
    "no_duplicate_raw_effect_id",
    "all_author_rows_preserved",
    "final_row_reconciliation",
    "audit_fields_still_present",
    "public_schema_matches_dictionary",
    "public_preserves_completed_merged_rows",
    "public_contains_completed_studies_only",
    "dictionary_has_one_row_per_public_column",
    "dictionary_groups_ordered",
    "required_public_bear_fields_present",
    "public_raw_input_fields_present",
    "author_rows_retain_author_inputs",
    "raw_rows_retain_selected_raw_inputs",
    "build_bear_clinicaltrials_surface_complete",
    "raw_scale_comparison_written",
    "scale_aware_author_raw_validation_written"
  ),
  passed = c(
    anyDuplicated(author %>% select(nct_id, outcome_analysis_id)) == 0,
    anyDuplicated(selected_raw$effect_id) == 0,
    all(author$effect_id %in% merged$effect_id),
    nrow(merged) == merge_summary$n[merge_summary$check == "author_rows"] +
      merge_summary$n[merge_summary$check == "selected_raw_rows"],
    all(c("validation_stage", "import_recommendation", "coverage_category",
          "author_overlap_band", "orientation_rule", "pair_selection_rule",
          "p_t", "q_t", "phi_t") %in% names(merged)),
    identical(names(public), expected_public_cols),
    nrow(public) == nrow(merged),
    all(str_to_lower(public$overall_status) == "completed"),
    setequal(names(public), dictionary$variable) &&
      all(table(dictionary$variable) == 1),
    identical(unique(dictionary$group), dictionary_groups),
    all(c("nct_id", "study_type", "allocation", "measure_class", "z", "b",
          "se", "z_operator") %in% names(public)),
    all(c("raw_event_t", "raw_event_c", "raw_n_t", "raw_n_c", "raw_mean_t",
          "raw_mean_c", "raw_sd_t", "raw_sd_c") %in% names(public)),
    public %>%
      filter(effect_source == "author_reported") %>%
      summarise(
        ok = all(!is.na(author_effect_id)) &&
          all(!is.na(estimate) | !is.na(p_value) |
                (!is.na(lower) & !is.na(upper)))
      ) %>%
      pull(ok),
    public %>%
      filter(effect_source != "author_reported") %>%
      summarise(
        ok = all(!is.na(raw_effect_id)) &&
          all(!is.na(raw_n_t) & !is.na(raw_n_c)) &&
          all(!is.na(raw_event_t) | !is.na(raw_mean_t))
      ) %>%
      pull(ok),
    nrow(bear_input) > 0 &&
      all(bear_input$include_in_bear) &&
      all(!is.na(bear_input$z)) &&
      all(!is.na(bear_input$z_operator)),
    nrow(comparison) >= 2,
    nrow(scale_aware_summary) > 0
  )
)

write_csv(checks, file.path(ctgov_validation_dir, "clinicaltrialsgov_merge_checks.csv"))
print(checks, n = nrow(checks), width = Inf)
stopifnot(all(checks$passed))

message("Final ClinicalTrials.gov merge validation passed.")
