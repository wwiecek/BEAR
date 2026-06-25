# Prepare compact AACT ClinicalTrials.gov indexes and reduced
# outcome-measurement candidate rows for raw effect derivation.

library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(tibble)

source("process/clinicaltrials.gov/lib/paths.R")

snapshot_zip <- ctgov_snapshot_zip
out_dir <- ctgov_intermediate_dir
candidate_csv <- file.path(out_dir, "outcome_measurements_first_pass_candidates.csv")
candidate_csv_endpoint <- file.path(out_dir, "outcome_measurements_candidates.csv")
outcome_measurements_long_path <- file.path(
  out_dir, "outcome_measurements_first_pass_long.rds"
)
outcome_measurements_long_endpoint_path <- file.path(out_dir, "outcome_measurements_long.rds")

# Helpers -----

read_aact <- function(file, col_select) {
  message("Reading ", file)
  read_delim(
    unz(snapshot_zip, file),
    delim = "|",
    col_types = cols(.default = col_character()),
    col_select = all_of(col_select),
    na = c("", "NA", "N/A", "NULL", "null"),
    quote = "\"",
    trim_ws = FALSE,
    progress = FALSE,
    show_col_types = FALSE
  )
}

read_generated_csv <- function(path) {
  data.table::fread(path, na.strings = c("", "NA", "N/A", "NULL", "null")) %>%
    as_tibble() %>%
    mutate(across(everything(), as.character))
}

as_num <- function(x) suppressWarnings(as.numeric(x))

norm_text <- function(x) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  x %>%
    str_replace_all("[\\u2010-\\u2015]", "-") %>%
    str_replace_all("\\u00a0", " ") %>%
    str_squish() %>%
    str_to_lower()
}

norm_code <- function(x) {
  norm_text(x) %>%
    str_replace_all("_", " ") %>%
    str_squish()
}

norm_param_type <- function(x) {
  x0 <- norm_code(x)
  case_when(
    is.na(x0) ~ NA_character_,
    x0 == "mean" ~ "mean",
    x0 == "least squares mean" ~ "least_squares_mean",
    x0 == "geometric mean" ~ "geometric_mean",
    x0 == "geometric least squares mean" ~ "geometric_least_squares_mean",
    x0 == "log mean" ~ "log_mean",
    x0 == "median" ~ "median",
    x0 == "number" ~ "number",
    x0 == "count of participants" ~ "count_of_participants",
    x0 == "count of units" ~ "count_of_units",
    TRUE ~ x0
  )
}

norm_dispersion_type <- function(x) {
  x0 <- norm_code(x)
  case_when(
    is.na(x0) ~ NA_character_,
    x0 == "standard deviation" ~ "sd",
    x0 == "standard error" ~ "se",
    x0 == "full range" ~ "full_range",
    x0 == "inter-quartile range" ~ "iqr",
    x0 == "inter quartile range" ~ "iqr",
    x0 == "geometric coefficient of variation" ~ "geometric_cv",
    str_detect(x0, "^[0-9.]+% confidence interval$") ~
      paste0("ci_", str_extract(x0, "^[0-9.]+")),
    TRUE ~ x0
  )
}

ci_level_from_dispersion <- function(x) {
  out <- str_match(x, "^ci_([0-9.]+)$")[, 2]
  as_num(out)
}

is_participant_unit <- function(x) {
  x0 <- norm_code(x)
  !is.na(x0) & str_detect(
    x0,
    "^(participants?|subjects?|patients?)( analyzed)?$|^number of participants$"
  )
}

has_participant_count_text <- function(x) {
  !is.na(x) & str_detect(
    x,
    paste(
      "number of participants|participants? (with|who|achieving|experiencing)",
      "subjects? (with|who)|patients? (with|who)|responders?|events?",
      "incidence|occurrence|remission|relapse|recurrence|death|mortality",
      "infection|hospitali[sz]ation|adverse event|\\bae\\b",
      sep = "|"
    )
  )
}

has_percent_text <- function(x) {
  !is.na(x) & str_detect(
    x,
    "percent|percentage|proportion|rate|ratio|probability|^%$"
  )
}

classify_source_candidate <- function(param_type_norm, dispersion_type_norm,
                                      combined_text, units_norm,
                                      param_value_num,
                                      dispersion_value_num,
                                      dispersion_lower_limit,
                                      dispersion_upper_limit) {
  ci_level <- ci_level_from_dispersion(dispersion_type_norm)
  count_text <- has_participant_count_text(combined_text) |
    is_participant_unit(units_norm)
  percent_text <- has_percent_text(combined_text) | has_percent_text(units_norm)

  case_when(
    param_type_norm == "mean" & dispersion_type_norm == "sd" &
      !is.na(param_value_num) & !is.na(dispersion_value_num) &
      dispersion_value_num > 0 ~ "continuous_mean_sd",
    param_type_norm == "mean" & dispersion_type_norm == "se" &
      !is.na(param_value_num) & !is.na(dispersion_value_num) &
      dispersion_value_num > 0 ~ "continuous_mean_se",
    param_type_norm == "mean" & !is.na(ci_level) &
      !is.na(param_value_num) & !is.na(dispersion_lower_limit) &
      !is.na(dispersion_upper_limit) &
      dispersion_upper_limit > dispersion_lower_limit ~
      "continuous_mean_ci",
    param_type_norm == "least_squares_mean" & dispersion_type_norm == "se" &
      !is.na(param_value_num) & !is.na(dispersion_value_num) &
      dispersion_value_num > 0 ~ "continuous_lsm_se",
    param_type_norm == "least_squares_mean" & !is.na(ci_level) &
      !is.na(param_value_num) & !is.na(dispersion_lower_limit) &
      !is.na(dispersion_upper_limit) &
      dispersion_upper_limit > dispersion_lower_limit ~
      "continuous_lsm_ci",
    param_type_norm == "count_of_participants" & !is.na(param_value_num) ~
      "binary_count_explicit",
    param_type_norm == "number" & !is.na(param_value_num) & percent_text ~
      "binary_percent_explicit",
    param_type_norm == "number" & !is.na(param_value_num) & count_text ~
      "binary_number_count",
    TRUE ~ NA_character_
  )
}

is_overall_label <- function(title, description) {
  x <- str_c(title, description, sep = " ")
  !is.na(x) & str_detect(x, "\\boverall\\b|all participants|total")
}

infer_arm_role <- function(title, description) {
  x <- str_c(title, description, sep = " ")
  case_when(
    is_overall_label(title, description) ~ "overall",
    str_detect(x, "\\bplacebo\\b") ~ "placebo_comparator",
    str_detect(x, "\\bsham\\b") ~ "sham_comparator",
    str_detect(x, "usual care|standard care|standard of care") ~ "usual_care",
    str_detect(x, "no intervention|no treatment|control") ~
      "control_unspecified",
    str_detect(x, "\\bexperimental\\b|\\bintervention\\b") ~ "experimental",
    str_detect(x, "\\btreatment\\b|treated") ~ "treatment_unspecified",
    TRUE ~ "unknown"
  )
}

count_rows <- function(df, table) {
  tibble(
    table = table,
    n_rows = nrow(df),
    n_distinct_nct_id =
      if ("nct_id" %in% names(df)) n_distinct(df$nct_id) else NA_integer_
  )
}

stream_filtered_table <- function(file, col_select, keep_fun, out_path,
                                  chunk_size = 100000) {
  if (file.exists(out_path)) file.remove(out_path)
  totals <- list(n_read = 0L, n_kept = 0L)

  callback <- DataFrameCallback$new(function(x, pos) {
    totals$n_read <<- totals$n_read + nrow(x)
    kept <- x %>%
      select(any_of(col_select)) %>%
      keep_fun()
    totals$n_kept <<- totals$n_kept + nrow(kept)
    if (nrow(kept) > 0) {
      write_csv(
        kept,
        out_path,
        append = file.exists(out_path),
        col_names = !file.exists(out_path)
      )
    }
  })

  message("Streaming ", file)
  read_delim_chunked(
    unz(snapshot_zip, file),
    callback = callback,
    chunk_size = chunk_size,
    delim = "|",
    col_types = cols(.default = col_character()),
    na = c("", "NA", "N/A", "NULL", "null"),
    quote = "\"",
    trim_ws = FALSE,
    progress = FALSE,
    show_col_types = FALSE
  )

  tibble(table = file, n_rows = totals$n_read, n_kept = totals$n_kept)
}

stream_result_groups <- function(group_ids, out_path, chunk_size = 200000) {
  stream_filtered_table(
    "result_groups.txt",
    c(
      "id", "nct_id", "ctgov_group_code", "result_type", "title",
      "description", "outcome_id"
    ),
    function(x) x %>% filter(id %in% group_ids),
    out_path,
    chunk_size = chunk_size
  )
}

stream_outcomes <- function(outcome_ids, out_path, chunk_size = 200000) {
  stream_filtered_table(
    "outcomes.txt",
    c(
      "id", "nct_id", "outcome_type", "title", "description", "time_frame",
      "population", "units", "units_analyzed", "dispersion_type", "param_type"
    ),
    function(x) x %>% filter(id %in% outcome_ids),
    out_path,
    chunk_size = chunk_size
  )
}

# Early v2 screen -----

message("Reading study/design screen for broad v2 candidate stream")
eligible_design_nct_ids <- read_aact(
  "studies.txt",
  c("nct_id", "overall_status", "study_type")
) %>%
  mutate(
    overall_status_norm = norm_code(overall_status),
    study_type_norm = norm_code(study_type),
    is_interventional = study_type_norm == "interventional",
    is_completed = overall_status_norm == "completed"
  ) %>%
  left_join(
    read_aact(
      "designs.txt",
      c("nct_id", "allocation", "intervention_model")
    ) %>%
      mutate(
        allocation_norm = norm_code(allocation),
        intervention_model_norm = norm_code(intervention_model)
      ),
    by = "nct_id"
  ) %>%
  filter(
    is_completed,
    is_interventional,
    allocation_norm == "randomized",
    intervention_model_norm %in% c("parallel", "parallel assignment")
  ) %>%
  pull(nct_id)

primary_outcomes_screen_csv <- file.path(out_dir, "outcomes_primary_screen.csv")
if (!file.exists(primary_outcomes_screen_csv)) {
  stream_filtered_table(
    "outcomes.txt",
    c(
      "id", "nct_id", "outcome_type", "title", "description", "time_frame",
      "population", "units", "units_analyzed", "dispersion_type", "param_type"
    ),
    function(x) {
      x %>%
        mutate(outcome_type_norm = norm_code(outcome_type)) %>%
        filter(nct_id %in% eligible_design_nct_ids, outcome_type_norm == "primary")
    },
    primary_outcomes_screen_csv,
    chunk_size = 200000
  )
}

primary_outcome_ids <- read_csv(
  primary_outcomes_screen_csv,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  pull(id)

# Stream and reduce outcome_measurements -----

if (file.exists(candidate_csv)) {
  message("Using existing ", candidate_csv)
  measurement_counts <- tibble(
    table = "outcome_measurements.txt",
    n_rows = NA_integer_,
    n_kept = NA_integer_
  )
} else {
  measurement_counts <- stream_filtered_table(
    "outcome_measurements.txt",
    c(
      "id", "nct_id", "outcome_id", "result_group_id", "ctgov_group_code",
      "classification", "category", "title", "description", "units",
      "param_type", "param_value", "param_value_num", "dispersion_type",
      "dispersion_value", "dispersion_value_num", "dispersion_lower_limit",
      "dispersion_upper_limit", "explanation_of_na",
      "dispersion_upper_limit_raw", "dispersion_lower_limit_raw"
    ),
    function(x) {
      x %>%
        rename(
          outcome_measurement_id = id,
          measurement_title = title,
          measurement_description = description,
          measurement_units = units
        ) %>%
        mutate(
          param_value_num = as_num(param_value_num),
          dispersion_value_num = as_num(dispersion_value_num),
          dispersion_lower_limit = as_num(dispersion_lower_limit),
          dispersion_upper_limit = as_num(dispersion_upper_limit),
          classification_norm = norm_text(classification),
          category_norm = norm_text(category),
          measurement_title_norm = norm_text(measurement_title),
          measurement_description_norm = norm_text(measurement_description),
          measurement_units_norm = norm_code(measurement_units),
          param_type_norm = norm_param_type(param_type),
          dispersion_type_norm = norm_dispersion_type(dispersion_type),
          measurement_stratum_id = str_c(
            nct_id, outcome_id, coalesce(classification_norm, ""),
            coalesce(category_norm, ""), coalesce(measurement_title_norm, ""),
            coalesce(measurement_units_norm, ""), coalesce(param_type_norm, ""),
            sep = "|"
          )
        ) %>%
        filter(
          (
            param_type_norm == "mean" &
              dispersion_type_norm == "sd" &
              !is.na(param_value_num) &
              !is.na(dispersion_value_num) &
              dispersion_value_num > 0
          ) |
            (
              param_type_norm == "count_of_participants" &
                !is.na(param_value_num)
            )
        )
    },
    candidate_csv
  )
}

if (file.exists(candidate_csv_endpoint)) {
  message("Using existing ", candidate_csv_endpoint)
  measurement_counts_endpoint <- tibble(
    table = "outcome_measurements.txt",
    n_rows = NA_integer_,
    n_kept = NA_integer_
  )
} else {
  measurement_counts_endpoint <- stream_filtered_table(
    "outcome_measurements.txt",
    c(
      "id", "nct_id", "outcome_id", "result_group_id", "ctgov_group_code",
      "classification", "category", "title", "description", "units",
      "param_type", "param_value", "param_value_num", "dispersion_type",
      "dispersion_value", "dispersion_value_num", "dispersion_lower_limit",
      "dispersion_upper_limit", "explanation_of_na",
      "dispersion_upper_limit_raw", "dispersion_lower_limit_raw"
    ),
    function(x) {
      x %>%
        rename(
          outcome_measurement_id = id,
          measurement_title = title,
          measurement_description = description,
          measurement_units = units
        ) %>%
        filter(
          nct_id %in% eligible_design_nct_ids,
          outcome_id %in% primary_outcome_ids
        ) %>%
        mutate(
          param_value_num = as_num(param_value_num),
          dispersion_value_num = as_num(dispersion_value_num),
          dispersion_lower_limit = as_num(dispersion_lower_limit),
          dispersion_upper_limit = as_num(dispersion_upper_limit),
          classification_norm = norm_text(classification),
          category_norm = norm_text(category),
          measurement_title_norm = norm_text(measurement_title),
          measurement_description_norm = norm_text(measurement_description),
          measurement_units_norm = norm_code(measurement_units),
          param_type_norm = norm_param_type(param_type),
          dispersion_type_norm = norm_dispersion_type(dispersion_type),
          combined_candidate_text = norm_text(str_c(
            classification, category, measurement_title,
            measurement_description, measurement_units,
            sep = " "
          )),
          source_row_candidate_class = classify_source_candidate(
            param_type_norm, dispersion_type_norm, combined_candidate_text,
            measurement_units_norm, param_value_num, dispersion_value_num,
            dispersion_lower_limit, dispersion_upper_limit
          ),
          measurement_stratum_id = str_c(
            nct_id, outcome_id, coalesce(classification_norm, ""),
            coalesce(category_norm, ""), coalesce(measurement_title_norm, ""),
            coalesce(measurement_units_norm, ""), coalesce(param_type_norm, ""),
            source_row_candidate_class,
            sep = "|"
          )
        ) %>%
        filter(!is.na(source_row_candidate_class)) %>%
        select(-combined_candidate_text)
    },
    candidate_csv_endpoint
  )
}

if (file.exists(outcome_measurements_long_path)) {
  message("Using existing ", outcome_measurements_long_path)
  outcome_measurements_long_existing <- readRDS(outcome_measurements_long_path)
  candidate_keys <- outcome_measurements_long_existing %>%
    distinct(nct_id, outcome_id, result_group_id, ctgov_group_code)
  candidate_nct_ids_first_pass <- unique(outcome_measurements_long_existing$nct_id)
  candidate_outcome_ids_first_pass <- unique(outcome_measurements_long_existing$outcome_id)
  candidate_group_ids_first_pass <- unique(outcome_measurements_long_existing$result_group_id)
  measurement_candidates_n <- nrow(outcome_measurements_long_existing)
  measurement_candidates_nct <- n_distinct(outcome_measurements_long_existing$nct_id)
} else {
  message("Reading reduced outcome-measurement candidates")
  measurement_candidates <- read_generated_csv(candidate_csv) %>%
    mutate(
      param_value_num = as_num(param_value_num),
      dispersion_value_num = as_num(dispersion_value_num),
      dispersion_lower_limit = as_num(dispersion_lower_limit),
      dispersion_upper_limit = as_num(dispersion_upper_limit)
    )
  candidate_keys <- measurement_candidates %>%
    distinct(nct_id, outcome_id, result_group_id, ctgov_group_code)
  candidate_nct_ids_first_pass <- unique(measurement_candidates$nct_id)
  candidate_outcome_ids_first_pass <- unique(measurement_candidates$outcome_id)
  candidate_group_ids_first_pass <- unique(measurement_candidates$result_group_id)
  measurement_candidates_n <- nrow(measurement_candidates)
  measurement_candidates_nct <- n_distinct(measurement_candidates$nct_id)
}

message("Reading broad v2 outcome-measurement candidates")
measurement_candidates_endpoint <- read_generated_csv(candidate_csv_endpoint) %>%
  mutate(
    param_value_num = as_num(param_value_num),
    dispersion_value_num = as_num(dispersion_value_num),
    dispersion_lower_limit = as_num(dispersion_lower_limit),
    dispersion_upper_limit = as_num(dispersion_upper_limit)
  )

candidate_keys_endpoint <- measurement_candidates_endpoint %>%
  distinct(nct_id, outcome_id, result_group_id, ctgov_group_code)

candidate_keys_all <- bind_rows(candidate_keys, candidate_keys_endpoint) %>%
  distinct()

candidate_nct_ids <- union(candidate_nct_ids_first_pass, unique(measurement_candidates_endpoint$nct_id))
candidate_outcome_ids <- union(candidate_outcome_ids_first_pass, unique(measurement_candidates_endpoint$outcome_id))
candidate_group_ids <- union(candidate_group_ids_first_pass, unique(measurement_candidates_endpoint$result_group_id))

# Read and reduce supporting tables -----

studies <- read_aact(
  "studies.txt",
  c(
    "nct_id", "brief_title", "official_title", "overall_status",
    "completion_date", "primary_completion_date", "study_type", "phase",
    "enrollment", "enrollment_type", "number_of_arms", "number_of_groups",
    "results_first_posted_date"
  )
) %>%
  filter(nct_id %in% candidate_nct_ids)

designs <- read_aact(
  "designs.txt",
  c(
    "nct_id", "allocation", "intervention_model", "observational_model",
    "primary_purpose", "masking"
  )
) %>%
  filter(nct_id %in% candidate_nct_ids)

outcomes_csv <- file.path(out_dir, "outcomes_candidates.csv")
if (file.exists(outcomes_csv)) {
  message("Using existing ", outcomes_csv)
  outcome_stream_counts <- tibble(
    table = "outcomes.txt",
    n_rows = NA_integer_,
    n_kept = NA_integer_
  )
} else {
  outcome_stream_counts <- stream_outcomes(candidate_outcome_ids, outcomes_csv)
}
outcomes <- read_csv(
  outcomes_csv,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

groups_csv <- file.path(out_dir, "result_groups_candidates.csv")
if (file.exists(groups_csv)) {
  message("Using existing ", groups_csv)
  group_stream_counts <- tibble(
    table = "result_groups.txt",
    n_rows = NA_integer_,
    n_kept = NA_integer_
  )
} else {
  group_stream_counts <- stream_result_groups(candidate_group_ids, groups_csv)
}
result_groups <- read_csv(
  groups_csv,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

outcome_counts <- read_aact(
  "outcome_counts.txt",
  c(
    "id", "nct_id", "outcome_id", "result_group_id",
    "ctgov_group_code", "scope", "units", "count"
  )
) %>%
  semi_join(
    candidate_keys_all,
    by = c("nct_id", "outcome_id", "result_group_id", "ctgov_group_code")
  )

outcome_analyses <- read_aact(
  "outcome_analyses.txt",
  c(
    "id", "nct_id", "outcome_id", "param_type", "param_value",
    "p_value_modifier", "p_value", "ci_n_sides", "ci_percent",
    "ci_lower_limit", "ci_upper_limit", "method"
  )
) %>%
  filter(nct_id %in% candidate_nct_ids, outcome_id %in% candidate_outcome_ids)

outcome_analysis_groups <- read_aact(
  "outcome_analysis_groups.txt",
  c(
    "id", "nct_id", "outcome_analysis_id", "result_group_id",
    "ctgov_group_code"
  )
) %>%
  filter(nct_id %in% candidate_nct_ids)

row_counts <- bind_rows(
  measurement_counts %>%
    transmute(table = "outcome_measurements", n_rows, n_distinct_nct_id = NA_integer_),
  tibble(
    table = "outcome_measurements_candidates",
    n_rows = measurement_candidates_n,
    n_distinct_nct_id = measurement_candidates_nct
  ),
  measurement_counts_endpoint %>%
    transmute(
      table = "outcome_measurements",
      n_rows,
      n_distinct_nct_id = NA_integer_
    ),
  tibble(
    table = "outcome_measurements_candidates",
    n_rows = nrow(measurement_candidates_endpoint),
    n_distinct_nct_id = n_distinct(measurement_candidates_endpoint$nct_id)
  ),
  count_rows(studies, "studies_candidates"),
  count_rows(designs, "designs_candidates"),
  outcome_stream_counts %>%
    transmute(table = "outcomes", n_rows, n_distinct_nct_id = NA_integer_),
  count_rows(outcomes, "outcomes_candidates"),
  group_stream_counts %>%
    transmute(table = "result_groups", n_rows, n_distinct_nct_id = NA_integer_),
  count_rows(result_groups, "result_groups_candidates"),
  count_rows(outcome_counts, "outcome_counts_candidates"),
  count_rows(outcome_analyses, "outcome_analyses_candidates"),
  count_rows(outcome_analysis_groups, "outcome_analysis_groups_candidates")
)

# Indexes -----

study_index <- studies %>%
  mutate(
    completion_date = as.Date(completion_date),
    primary_completion_date = as.Date(primary_completion_date),
    year = year(completion_date),
    enrollment = as_num(enrollment),
    number_of_arms = as_num(number_of_arms),
    number_of_groups = as_num(number_of_groups),
    overall_status_norm = norm_code(overall_status),
    study_type_norm = norm_code(study_type),
    is_interventional = study_type_norm == "interventional",
    is_completed = overall_status_norm == "completed"
  ) %>%
  left_join(
    designs %>%
      mutate(
        allocation_norm = norm_code(allocation),
        intervention_model_norm = norm_code(intervention_model)
      ),
    by = "nct_id"
  ) %>%
  mutate(
    is_randomized = allocation_norm == "randomized",
    is_parallel = intervention_model_norm %in% c("parallel", "parallel assignment"),
    is_first_pass_design_eligible =
      is_interventional & is_randomized & is_parallel
  ) %>%
  select(
    nct_id, brief_title, official_title, overall_status, completion_date,
    primary_completion_date, year, study_type, phase, enrollment,
    enrollment_type, number_of_arms, number_of_groups, allocation,
    intervention_model, observational_model, primary_purpose, masking,
    results_first_posted_date, overall_status_norm, study_type_norm,
    allocation_norm, intervention_model_norm, is_interventional,
    is_randomized, is_parallel, is_completed, is_first_pass_design_eligible
  )

outcome_index <- outcomes %>%
  rename(
    outcome_id = id,
    outcome_title = title,
    outcome_description = description,
    outcome_time_frame = time_frame,
    outcome_units = units,
    outcome_units_analyzed = units_analyzed,
    outcome_dispersion_type = dispersion_type,
    outcome_param_type = param_type
  ) %>%
  mutate(
    outcome_type_norm = norm_code(outcome_type),
    outcome_title_norm = norm_text(outcome_title),
    outcome_time_frame_norm = norm_text(outcome_time_frame),
    outcome_units_norm = norm_code(outcome_units),
    outcome_param_type_norm = norm_param_type(outcome_param_type),
    outcome_dispersion_type_norm =
      norm_dispersion_type(outcome_dispersion_type)
  )

result_group_index <- result_groups %>%
  rename(
    result_group_id = id,
    group_title = title,
    group_description = description
  ) %>%
  mutate(
    result_type_norm = norm_code(result_type),
    group_title_norm = norm_text(group_title),
    group_description_norm = norm_text(group_description),
    is_overall_group = is_overall_label(
      group_title_norm,
      group_description_norm
    ),
    is_analysis_set_group = str_detect(
      str_c(group_title_norm, group_description_norm, sep = " "),
      "analysis set|analysis population|safety population|treated population"
    ),
    arm_role_guess = infer_arm_role(group_title_norm, group_description_norm)
  )

outcome_counts_long <- outcome_counts %>%
  rename(
    outcome_count_id = id,
    outcome_count_units = units,
    outcome_count_raw = count
  ) %>%
  mutate(
    outcome_count = as_num(outcome_count_raw),
    outcome_count_units_norm = norm_code(outcome_count_units),
    is_participant_count = is_participant_unit(outcome_count_units),
    unit_warning = if_else(
      is_participant_count,
      NA_character_,
      "non_participant_or_unknown_unit"
    )
  )

outcome_count_summary <- outcome_counts_long %>%
  arrange(
    nct_id, outcome_id, result_group_id, ctgov_group_code,
    desc(is_participant_count), is.na(outcome_count)
  ) %>%
  group_by(nct_id, outcome_id, result_group_id, ctgov_group_code) %>%
  summarise(
    outcome_count = first(outcome_count),
    outcome_count_units = first(outcome_count_units),
    outcome_count_units_norm = first(outcome_count_units_norm),
    is_participant_count = first(is_participant_count),
    n_outcome_count_rows = n(),
    n_distinct_outcome_counts = n_distinct(outcome_count, na.rm = TRUE),
    .groups = "drop"
  )

if (exists("outcome_measurements_long_existing")) {
  outcome_measurements_long <- outcome_measurements_long_existing
  rm(outcome_measurements_long_existing)
  gc()
} else {
  outcome_measurements_long <- measurement_candidates %>%
    left_join(
      outcome_count_summary,
      by = c("nct_id", "outcome_id", "result_group_id", "ctgov_group_code")
    ) %>%
    filter(
      is_participant_count,
      (
        param_type_norm == "mean" &
          dispersion_type_norm == "sd" &
          !is.na(outcome_count) &
          outcome_count > 1
      ) |
        (
          param_type_norm == "count_of_participants" &
            !is.na(outcome_count) &
            outcome_count > 0 &
            param_value_num >= 0 &
            param_value_num <= outcome_count
        )
    ) %>%
    left_join(
      result_group_index %>%
        select(
          result_group_id, result_type, result_type_norm, group_title,
          group_description, group_title_norm, group_description_norm,
          is_overall_group, is_analysis_set_group, arm_role_guess
        ),
      by = "result_group_id"
    ) %>%
    filter(result_type_norm == "outcome", !is_overall_group) %>%
    left_join(
      outcome_index %>%
        select(
          nct_id, outcome_id, outcome_type, outcome_type_norm, outcome_title,
          outcome_description, outcome_time_frame, outcome_units,
          outcome_units_analyzed
        ),
      by = c("nct_id", "outcome_id")
    ) %>%
    left_join(
      study_index %>%
        select(
          nct_id, brief_title, year, phase, enrollment, number_of_arms,
          number_of_groups, study_type, allocation, intervention_model,
          primary_purpose, masking, overall_status,
          is_first_pass_design_eligible
        ),
      by = "nct_id"
    )
}

outcome_measurements_long_endpoint <- measurement_candidates_endpoint %>%
  left_join(
    outcome_count_summary,
    by = c("nct_id", "outcome_id", "result_group_id", "ctgov_group_code")
  ) %>%
  filter(
    is_participant_count,
    (
      source_row_candidate_class %in% c(
        "continuous_mean_sd", "continuous_mean_se", "continuous_mean_ci",
        "continuous_lsm_se", "continuous_lsm_ci"
      ) &
        !is.na(outcome_count) &
        outcome_count > 1
    ) |
      (
        source_row_candidate_class %in% c(
          "binary_count_explicit", "binary_number_count",
          "binary_percent_explicit"
        ) &
          !is.na(outcome_count) &
          outcome_count > 0 &
          !is.na(param_value_num)
      )
  ) %>%
  left_join(
    result_group_index %>%
      select(
        result_group_id, result_type, result_type_norm, group_title,
        group_description, group_title_norm, group_description_norm,
        is_overall_group, is_analysis_set_group, arm_role_guess
      ),
    by = "result_group_id"
  ) %>%
  filter(result_type_norm == "outcome", !is_overall_group) %>%
  left_join(
    outcome_index %>%
      select(
        nct_id, outcome_id, outcome_type, outcome_type_norm, outcome_title,
        outcome_description, outcome_time_frame, outcome_units,
        outcome_units_analyzed
      ),
    by = c("nct_id", "outcome_id")
  ) %>%
  left_join(
    study_index %>%
      select(
        nct_id, brief_title, year, phase, enrollment, number_of_arms,
        number_of_groups, study_type, allocation, intervention_model,
        primary_purpose, masking, overall_status,
        is_first_pass_design_eligible
      ),
    by = "nct_id"
  )

author_analysis_links <- outcome_analysis_groups %>%
  rename(outcome_analysis_group_id = id) %>%
  group_by(nct_id, outcome_analysis_id) %>%
  summarise(
    linked_result_group_ids = str_c(sort(unique(result_group_id)), collapse = ";"),
    linked_group_codes = str_c(sort(unique(ctgov_group_code)), collapse = ";"),
    n_linked_groups = n_distinct(result_group_id),
    .groups = "drop"
  ) %>%
  right_join(
    outcome_analyses %>%
      rename(
        outcome_analysis_id = id,
        analysis_param_type = param_type,
        analysis_param_value = param_value
      ),
    by = c("nct_id", "outcome_analysis_id")
  ) %>%
  mutate(
    analysis_param_type_norm = norm_param_type(analysis_param_type),
    analysis_param_value_num = as_num(analysis_param_value),
    p_value_num = as_num(p_value),
    ci_percent = as_num(ci_percent),
    ci_lower_limit = as_num(ci_lower_limit),
    ci_upper_limit = as_num(ci_upper_limit)
  )

# Save -----

write_csv(row_counts, file.path(out_dir, "input_row_counts.csv"))
saveRDS(study_index, file.path(out_dir, "study_index.rds"))
saveRDS(outcome_index, file.path(out_dir, "outcome_index.rds"))
saveRDS(result_group_index, file.path(out_dir, "result_group_index.rds"))
saveRDS(outcome_counts_long, file.path(out_dir, "outcome_counts_long.rds"))
saveRDS(
  outcome_measurements_long,
  file.path(out_dir, "outcome_measurements_first_pass_long.rds")
)
saveRDS(
  outcome_measurements_long_endpoint,
  file.path(out_dir, "outcome_measurements_long.rds")
)
saveRDS(author_analysis_links, file.path(out_dir, "author_analysis_links.rds"))

message("Saved reduced normalized indexes to ", out_dir)
