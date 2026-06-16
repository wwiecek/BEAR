# Assemble a validation-annotated raw endpoint candidate side dataset.
# This preserves all prioritized endpoint rows and all effect scales.

library(dplyr)
library(readr)
library(stringr)
library(tibble)

source("process/clinicaltrials.gov/lib/paths.R")

out_dir <- ctgov_derived_dir
validation_dir <- ctgov_validation_dir

# Helpers -----

band_rank <- function(x) {
  case_when(
    x == "exact_or_near_agreement" ~ 1L,
    x == "moderate_disagreement" ~ 2L,
    x == "large_disagreement" ~ 3L,
    x == "author_z_unavailable" ~ 4L,
    TRUE ~ 5L
  )
}

# Load inputs -----

raw_endpoint <- readRDS(file.path(out_dir, "raw_endpoint_effects.rds"))
raw_source_v3 <- readRDS(file.path(out_dir, "endpoint_source_rows_classified.rds"))
coverage <- read_csv(file.path(validation_dir, "trial_coverage_audit.csv"), show_col_types = FALSE)
author_overlap_base <- read_csv(
  file.path(validation_dir, "author_overlap_validation.csv"),
  show_col_types = FALSE
)
author_overlap_base <- author_overlap_base %>%
  mutate(across(c(nct_id, outcome_id), as.character))

# Trial/source metadata joins -----

source_class_lookup <- raw_source_v3 %>%
  distinct(source_row_class_endpoint, source_row_class_audit) %>%
  group_by(source_row_class_endpoint) %>%
  summarise(
    source_row_class_audit_values =
      str_c(sort(unique(source_row_class_audit)), collapse = ";"),
    .groups = "drop"
  )

author_band_by_source <- author_overlap_base %>%
  mutate(
    author_overlap_band = case_when(
      is.na(author_z) ~ "author_z_unavailable",
      !is.na(abs_z_difference) & abs_z_difference <= 0.25 &
        coalesce(p_side_agrees_005, FALSE) ~ "exact_or_near_agreement",
      !is.na(abs_z_difference) & abs_z_difference <= 1 ~
        "moderate_disagreement",
      TRUE ~ "large_disagreement"
    )
  ) %>%
  transmute(
    nct_id, outcome_id, measure = raw_measure,
    source_row_class = raw_source_row_class,
    author_overlap_band
  ) %>%
  filter(!is.na(author_overlap_band)) %>%
  group_by(nct_id, outcome_id, measure, source_row_class) %>%
  summarise(
    n_author_overlap_rows = n(),
    worst_author_overlap_band =
      author_overlap_band[which.max(band_rank(author_overlap_band))],
    best_author_overlap_band =
      author_overlap_band[which.min(band_rank(author_overlap_band))],
    .groups = "drop"
  )

candidate <- raw_endpoint %>%
  left_join(
    coverage %>%
      select(
        nct_id, coverage_category, in_old_bear, has_author_analysis,
        has_raw_endpoint, has_unknown_only_rows, has_prepost_rows,
        n_old_bear_rows, n_author_analysis_rows, n_raw_endpoint_rows,
        n_unknown_rows, n_prepost_pairs
      ),
    by = "nct_id"
  ) %>%
  left_join(
    source_class_lookup,
    by = c("source_row_class" = "source_row_class_endpoint")
  ) %>%
  left_join(
    author_band_by_source,
    by = c("nct_id", "outcome_id", "measure", "source_row_class")
  ) %>%
  mutate(
    author_overlap_band = coalesce(
      worst_author_overlap_band,
      if_else(author_linked_pair, "author_overlap_not_recovered", "no_author_overlap")
    ),
    n_author_overlap_rows = coalesce(n_author_overlap_rows, 0L),
    import_recommendation = "pending_validation",
    validation_stage = "candidate_side_dataset",
    source_row_class_audit_values = coalesce(
      source_row_class_audit_values,
      source_row_class
    )
  ) %>%
  select(
    effect_id, validation_stage, import_recommendation,
    coverage_category, author_overlap_band, best_author_overlap_band,
    worst_author_overlap_band, n_author_overlap_rows,
    everything()
  ) %>%
  arrange(nct_id, outcome_id, measurement_stratum_id, effect_family, measure)

candidate_summary <- bind_rows(
  candidate %>%
    count(
      section = "effect_summary",
      source_row_class, effect_family, measure, validity_tier,
      orientation_rule, pair_selection_rule, author_overlap_band,
      coverage_category,
      name = "n"
    ),
  tibble(
    section = "required_field_missing",
    source_row_class = NA_character_,
    effect_family = NA_character_,
    measure = NA_character_,
    validity_tier = NA_character_,
    orientation_rule = NA_character_,
    pair_selection_rule = NA_character_,
    author_overlap_band = NA_character_,
    coverage_category = c(
      "measure", "effect_family", "source_row_class",
      "validity_tier", "orientation_rule", "pair_selection_rule"
    ),
    n = c(
      sum(is.na(candidate$measure)),
      sum(is.na(candidate$effect_family)),
      sum(is.na(candidate$source_row_class)),
      sum(is.na(candidate$validity_tier)),
      sum(is.na(candidate$orientation_rule)),
      sum(is.na(candidate$pair_selection_rule))
    )
  )
) %>%
  arrange(section, source_row_class, effect_family, measure)

stopifnot(
  nrow(candidate) == 250444,
  all(c(
    "measure", "effect_family", "source_row_class", "validity_tier",
    "orientation_rule", "pair_selection_rule", "author_overlap_band",
    "coverage_category", "import_recommendation"
  ) %in% names(candidate)),
  all(!is.na(candidate$measure)),
  all(!is.na(candidate$effect_family)),
  all(!is.na(candidate$source_row_class)),
  all(!is.na(candidate$validity_tier)),
  all(!is.na(candidate$orientation_rule))
)

saveRDS(candidate, file.path(out_dir, "endpoint_candidates.rds"))
write_csv(
  candidate_summary,
  file.path(out_dir, "endpoint_candidates_summary.csv")
)

message("Saved ", nrow(candidate), " validation-annotated endpoint candidates.")
