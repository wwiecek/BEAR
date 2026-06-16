# Refine unknown source rows into audit-only exclusion classes.
# This script does not change endpoint effect derivation or merge any recovered
# rows into the default endpoint outputs.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

source("process/clinicaltrials.gov/lib/paths.R")

out_dir <- ctgov_derived_dir
validation_dir <- ctgov_validation_dir
manual_review_dir <- ctgov_manual_review_dir
set.seed(20260611)

# Helpers -----

norm_text <- function(x) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  x %>%
    str_replace_all("[\\u2010-\\u2015]", "-") %>%
    str_replace_all("\\u00a0", " ") %>%
    str_squish() %>%
    str_to_lower()
}

text_has <- function(x, pattern) !is.na(x) & str_detect(x, pattern)

sample_rows <- function(df, n = 150) {
  if (nrow(df) <= n) df else slice_sample(df, n = n)
}

top_label <- function(x, n = 1) {
  tab <- tibble(x = x) %>%
    filter(!is.na(x), x != "") %>%
    count(x, sort = TRUE)
  if (nrow(tab) == 0) NA_character_ else str_c(head(tab$x, n), collapse = "; ")
}

audit_class <- function(text, category_norm, title_norm, param_type_norm) {
  missing_category <- is.na(category_norm) | category_norm == ""

  case_when(
    missing_category & param_type_norm == "count_of_participants" ~
      "excluded_unknown_missing_positive_category",
    text_has(
      text,
      paste(
        "adverse event|serious adverse|treatment emergent|\\bteae\\b|\\bsae\\b",
        "solicited|unsolicited|reactogenicity|toxicity|side effect",
        sep = "|"
      )
    ) ~ "excluded_unknown_safety_or_reactogenicity",
    text_has(
      text,
      paste(
        "laborator|chemistry|hematolog|abnormalit|\\bqtc\\b|\\becg\\b|\\bekg\\b",
        "electrocardiogram|alanine|aspartate|bilirubin|creatinine",
        "hemoglobin|neutrophil|platelet|glucose|cholesterol|enzyme",
        sep = "|"
      )
    ) ~ "excluded_unknown_laboratory_abnormality",
    text_has(
      text,
      paste(
        "\\bgrade [1-5]\\b|severity|mild|moderate|severe|normal|abnormal",
        "\\blow\\b|medium|\\bhigh\\b|quartile|tertile|category [0-9]",
        "level [0-9]|stage [0-9]|class [ivx]+|score category",
        sep = "|"
      )
    ) ~ "excluded_unknown_ordinal_or_multicategory",
    text_has(
      text,
      paste(
        "questionnaire|likert|agree|disagree|satisfaction|quality of life",
        "\\bqol\\b|scale response|item response|domain score|subscale",
        sep = "|"
      )
    ) ~ "excluded_unknown_score_response_category",
    text_has(
      text,
      paste(
        "withdraw|discontinu|dropout|lost to follow|adherence|compliance",
        "completion|completed treatment|protocol deviation|dose reduction",
        "missed dose|retention|attendance",
        sep = "|"
      )
    ) ~ "excluded_unknown_disposition_or_adherence",
    text_has(
      text,
      paste(
        "baseline|demographic|medical history|prior therapy|previous",
        "sex|gender|race|ethnic|age group|body mass|\\bbmi\\b",
        "smoking|alcohol|region|country|mutation|genotype",
        sep = "|"
      )
    ) ~ "excluded_unknown_demographic_or_baseline",
    text_has(title_norm, "^number$|^percentage$|^percent$|^participants$") |
      nchar(coalesce(title_norm, "")) < 8 ~
      "excluded_unknown_sparse_or_generic",
    TRUE ~ "unknown_still_unclassified"
  )
}

strong_event_text <- function(x) {
  text_has(
    x,
    paste(
      "number of participants with|participants with|subjects with",
      "patients with|responders?|achieved|healed|remission|response",
      "cure|failure|death|mortality|hospitali[sz]ation|infection",
      "relapse|recurrence",
      sep = "|"
    )
  )
}

strict_exclusion_text <- function(x) {
  text_has(
    x,
    paste(
      "adverse event|treatment emergent|serious adverse|\\bteae\\b|\\bsae\\b",
      "solicited|reactogenicity|toxicity|laborator|chemistry|hematolog",
      "abnormal|\\bqtc\\b|\\becg\\b|\\bekg\\b|grade|severity|mild|moderate",
      "severe|normal|questionnaire|likert|agree|disagree|satisfaction",
      "withdraw|discontinu|adherence|compliance|completion|baseline",
      "demographic|medical history|prior|sex|gender|race|ethnic|age group",
      sep = "|"
    )
  )
}

# Load and classify -----

endpoint_source_rows <- readRDS(file.path(out_dir, "endpoint_source_rows.rds"))

source_rows_classified <- endpoint_source_rows %>%
  mutate(
    source_row_class_endpoint = source_row_class,
    audit_text = norm_text(str_c(
      coalesce(classification, ""), coalesce(category, ""),
      coalesce(measurement_title, ""), coalesce(measurement_description, ""),
      coalesce(measurement_units, ""), coalesce(outcome_title, ""),
      coalesce(outcome_description, ""),
      sep = " "
    )),
    outcome_title_norm_audit = norm_text(outcome_title),
    source_row_class_audit = if_else(
      source_row_class_endpoint == "unknown",
      audit_class(
        audit_text, category_norm, measurement_title_norm, param_type_norm
      ),
      source_row_class_endpoint
    ),
    classifier_rule_id_audit = case_when(
      source_row_class_endpoint != "unknown" ~ classifier_rule_id,
      source_row_class_audit == "excluded_unknown_missing_positive_category" ~
        "unknown_missing_category_count",
      source_row_class_audit == "unknown_still_unclassified" ~
        "unknown_still_unclassified",
      TRUE ~ source_row_class_audit
    ),
    endpoint_inclusion = source_row_class_endpoint != "unknown"
  )

unknown_rows_classified <- source_rows_classified %>%
  filter(source_row_class_endpoint == "unknown")

unknown_summary <- bind_rows(
  unknown_rows_classified %>%
    mutate(category_missing = is.na(category_norm) | category_norm == "") %>%
    count(
      section = "class_summary", source_row_candidate_class,
      source_row_class_endpoint, source_row_class_audit, param_type_norm,
      measurement_units_norm, category_missing, name = "n"
    ) %>%
    arrange(desc(n)),
  unknown_rows_classified %>%
    group_by(source_row_class_audit) %>%
    summarise(
      section = "top_titles",
      source_row_candidate_class = NA_character_,
      source_row_class_endpoint = "unknown",
      param_type_norm = NA_character_,
      measurement_units_norm = NA_character_,
      category_missing = NA,
      n = n(),
      top_measurement_title_norm = top_label(measurement_title_norm, 5),
      top_outcome_title_norm = top_label(outcome_title_norm_audit, 5),
      .groups = "drop"
    )
) %>%
  mutate(
    top_measurement_title_norm = coalesce(top_measurement_title_norm, NA_character_),
    top_outcome_title_norm = coalesce(top_outcome_title_norm, NA_character_)
  )

manual_review_unknown <- unknown_rows_classified %>%
  select(
    nct_id, outcome_id, outcome_title, outcome_time_frame, classification,
    category, measurement_title, measurement_description, measurement_units,
    param_type, param_value, param_value_num, outcome_count, group_title,
    source_row_candidate_class, source_row_class_audit, classifier_rule_id_audit
  ) %>%
  group_by(source_row_class_audit) %>%
  group_modify(~ sample_rows(.x, 40)) %>%
  ungroup()

binary_candidates <- source_rows_classified %>%
  filter(
    source_row_class_endpoint == "unknown",
    param_type_norm == "count_of_participants",
    is.na(category_norm) | category_norm == "",
    is_participant_count,
    !is.na(outcome_count), outcome_count > 0,
    !is.na(param_value_num), param_value_num >= 0,
    param_value_num <= outcome_count,
    strong_event_text(audit_text),
    !strict_exclusion_text(audit_text)
  ) %>%
  mutate(
    source_row_class_audit = "binary_missing_category_title_event_candidate",
    classifier_rule_id_audit =
      "strict_missing_category_title_event_diagnostic",
    x_eff = param_value_num,
    x_is_fractional = abs(x_eff - round(x_eff)) > 1e-8,
    diagnostic_only = TRUE
  )

manual_review_binary_candidates <- binary_candidates %>%
  select(
    nct_id, outcome_id, outcome_title, outcome_time_frame, classification,
    category, measurement_title, measurement_description, measurement_units,
    param_value_num, outcome_count, group_title, group_description,
    source_row_class_audit, classifier_rule_id_audit
  ) %>%
  group_by(source_row_class_audit) %>%
  group_modify(~ sample_rows(.x, 150)) %>%
  ungroup()

# Save -----

saveRDS(source_rows_classified, file.path(out_dir, "endpoint_source_rows_classified.rds"))
saveRDS(
  binary_candidates,
  file.path(out_dir, "binary_missing_category_title_event_candidates.rds")
)

write_csv(
  unknown_summary,
  file.path(validation_dir, "unknown_source_rows_summary.csv")
)
write_csv(
  manual_review_unknown,
  file.path(manual_review_dir, "sample_unknown_source_rows.csv")
)
write_csv(
  manual_review_binary_candidates,
  file.path(manual_review_dir, "binary_missing_category_candidates.csv")
)

message("Saved unknown-row audit outputs to ", out_dir)
message("Strict diagnostic missing-category candidates: ", nrow(binary_candidates))
