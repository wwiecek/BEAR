# Add diagnostic bands and stratified summaries for author-overlap validation.
# This is a validation layer only; it does not accept or reject endpoint rows.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

source("process/clinicaltrials.gov/lib/paths.R")

input_dir <- ctgov_intermediate_dir
out_dir <- ctgov_derived_dir
validation_dir <- ctgov_validation_dir
manual_review_dir <- ctgov_manual_review_dir

# Helpers -----

pair_key <- function(a, b) {
  if_else(a < b, str_c(a, b, sep = "||"), str_c(b, a, sep = "||"))
}

classify_band <- function(abs_z_difference, p_side_agrees_005, author_z) {
  case_when(
    is.na(author_z) ~ "author_z_unavailable",
    !is.na(abs_z_difference) & abs_z_difference <= 0.25 &
      coalesce(p_side_agrees_005, FALSE) ~ "exact_or_near_agreement",
    !is.na(abs_z_difference) & abs_z_difference <= 1 ~ "moderate_disagreement",
    TRUE ~ "large_disagreement"
  )
}

sample_rows <- function(df, n = 250) {
  if (nrow(df) <= n) df else slice_sample(df, n = n)
}

# Rebuild validation rows with retained raw-effect metadata -----

raw_all_pairs <- readRDS(file.path(out_dir, "raw_endpoint_effect_pairs.rds"))
author_links <- readRDS(file.path(input_dir, "author_analysis_links.rds"))
author_overlap_base <- read_csv(
  file.path(validation_dir, "author_overlap_validation.csv"),
  show_col_types = FALSE
)

author_pairs <- author_links %>%
  filter(n_linked_groups == 2) %>%
  mutate(
    linked_ids = str_split(linked_result_group_ids, ";"),
    pair_group_key = map_chr(linked_ids, ~ pair_key(.x[1], .x[2]))
  )

author_overlap <- raw_all_pairs %>%
  filter(author_linked_pair) %>%
  left_join(author_pairs, by = c("nct_id", "outcome_id"), relationship = "many-to-many") %>%
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
    same_sign = !is.na(analysis_param_value_num) &
      !is.na(b) & sign(analysis_param_value_num) == sign(b),
    author_overlap_band = classify_band(
      abs_z_difference, p_side_agrees_005, author_z
    ),
    notes = if_else(
      is.na(author_z),
      "author_z_not_recovered",
      "diagnostic_overlap_only"
    )
  ) %>%
  transmute(
    nct_id, outcome_id, outcome_analysis_id,
    raw_effect_id = effect_id,
    raw_measure = measure,
    raw_effect_family = effect_family,
    raw_source_row_class = source_row_class,
    raw_validity_tier = validity_tier,
    raw_orientation_rule = orientation_rule,
    raw_pair_selection_rule = pair_selection_rule,
    raw_pair_dependency_class = pair_dependency_class,
    raw_warning_flags = warning_flags,
    raw_b = b, raw_se = se, raw_z = z,
    author_param_type = analysis_param_type,
    author_param_value = analysis_param_value,
    author_p = p_value_num,
    author_ci_lower = ci_lower_limit,
    author_ci_upper = ci_upper_limit,
    author_z, method, same_sign,
    abs_z_difference, p_side_agrees_005,
    author_overlap_band, notes
  )

stopifnot(nrow(author_overlap) == nrow(author_overlap_base))

# Summaries and manual-review extracts -----

diagnostic_summary <- bind_rows(
  author_overlap %>%
    count(
      section = "overall_band",
      raw_source_row_class = NA_character_,
      raw_measure = NA_character_,
      raw_validity_tier = NA_character_,
      raw_orientation_rule = NA_character_,
      raw_pair_selection_rule = NA_character_,
      method = NA_character_,
      raw_warning_flags = NA_character_,
      author_overlap_band,
      name = "n"
    ),
  author_overlap %>%
    count(
      section = "stratified",
      raw_source_row_class, raw_measure, raw_validity_tier,
      raw_orientation_rule, raw_pair_selection_rule, method,
      raw_warning_flags = coalesce(raw_warning_flags, "none"),
      author_overlap_band,
      name = "n"
    )
) %>%
  arrange(section, raw_source_row_class, raw_measure, author_overlap_band)

disagreement_examples <- author_overlap %>%
  filter(author_overlap_band %in% c("moderate_disagreement", "large_disagreement")) %>%
  arrange(desc(abs_z_difference), nct_id, outcome_id) %>%
  group_by(author_overlap_band, raw_source_row_class, raw_measure) %>%
  group_modify(~ sample_rows(.x, 25)) %>%
  ungroup()

manual_review <- author_overlap %>%
  filter(
    author_overlap_band %in% c(
      "large_disagreement", "moderate_disagreement", "author_z_unavailable"
    )
  ) %>%
  arrange(
    factor(
      author_overlap_band,
      levels = c(
        "large_disagreement", "moderate_disagreement", "author_z_unavailable"
      )
    ),
    desc(abs_z_difference),
    nct_id,
    outcome_id
  )

write_csv(
  diagnostic_summary,
  file.path(validation_dir, "author_overlap_diagnostic_summary.csv")
)
write_csv(
  disagreement_examples,
  file.path(validation_dir, "author_overlap_disagreement_examples.csv")
)
write_csv(
  manual_review,
  file.path(manual_review_dir, "author_overlap.csv")
)

message("Saved author-overlap diagnostic outputs to ", validation_dir)
