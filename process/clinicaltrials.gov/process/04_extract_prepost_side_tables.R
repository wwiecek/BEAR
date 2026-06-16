# Extract ClinicalTrials.gov group-level pre/post measurement side tables.
# The outputs are diagnostic side artifacts and do not alter endpoint effects.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

source("process/clinicaltrials.gov/lib/paths.R")

snapshot_zip <- ctgov_snapshot_zip
input_dir <- ctgov_intermediate_dir
out_dir <- ctgov_derived_dir
validation_dir <- ctgov_validation_dir
set.seed(20260611)

# Helpers -----

safe_divide <- function(x, y) ifelse(y != 0 & is.finite(y), x / y, NA_real_)

make_id <- function(prefix, n) sprintf("%s_%09d", prefix, seq_len(n))

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
    x0 == "number" ~ "number",
    x0 == "count of participants" ~ "count_of_participants",
    TRUE ~ x0
  )
}

norm_dispersion_type <- function(x) {
  x0 <- norm_code(x)
  case_when(
    is.na(x0) ~ NA_character_,
    x0 == "standard deviation" ~ "sd",
    x0 == "standard error" ~ "se",
    str_detect(x0, "^[0-9.]+% confidence interval$") ~
      paste0("ci_", str_extract(x0, "^[0-9.]+")),
    TRUE ~ x0
  )
}

ci_level_from_dispersion <- function(x) {
  as_num(str_match(x, "^ci_([0-9.]+)$")[, 2])
}

text_has <- function(x, pattern) !is.na(x) & str_detect(x, pattern)

warning_list <- function(...) {
  flags <- list(...)
  out <- names(flags)[vapply(flags, isTRUE, logical(1))]
  if (length(out) == 0) NA_character_ else str_c(out, collapse = ";")
}

strip_time_words <- function(x) {
  x %>%
    norm_text() %>%
    str_replace_all(
      paste(
        "change from baseline|mean change|percent change|percentage change",
        "relative change|ratio to baseline|fold change|difference from baseline",
        "baseline|pre[- ]?treatment|pretreatment|pre[- ]?intervention",
        "pre[- ]?dose|predose|screening|enrollment|randomi[sz]ation",
        "before treatment|before intervention|before first dose",
        "prior to treatment|prior to randomi[sz]ation|post[- ]?baseline",
        "post[- ]?treatment|post[- ]?intervention|post[- ]?dose",
        "follow[- ]?up|endpoint|final|end of treatment|end-of-treatment",
        "end of study|last observation|last visit|discharge",
        "\\bday [0-9.]+\\b|\\bweek [0-9.]+\\b|\\bmonth [0-9.]+\\b",
        "\\byear [0-9.]+\\b|\\bvisit [0-9.]+\\b",
        sep = "|"
      ),
      " "
    ) %>%
    str_replace_all("[^a-z0-9.%]+", " ") %>%
    str_squish()
}

construct_key <- function(outcome_title, classification, category,
                          measurement_title, measurement_description,
                          units, param_family) {
  parts <- cbind(
    strip_time_words(classification),
    strip_time_words(category),
    strip_time_words(measurement_title),
    strip_time_words(measurement_description),
    norm_code(units),
    param_family
  )
  apply(parts, 1, function(z) str_c(na.omit(z[z != ""]), collapse = "|"))
}

classify_time_role <- function(classification, category, measurement_title,
                               measurement_description, outcome_title,
                               outcome_time_frame) {
  text <- norm_text(str_c(
    coalesce(classification, ""), coalesce(category, ""),
    coalesce(measurement_title, ""), coalesce(measurement_description, ""),
    coalesce(outcome_title, ""), coalesce(outcome_time_frame, ""),
    sep = " "
  ))
  reported_change <- text_has(
    text,
    paste(
      "change from baseline|mean change|percent change|percentage change",
      "relative change|ratio to baseline|fold change|difference from baseline",
      sep = "|"
    )
  )
  baseline <- text_has(
    text,
    paste(
      "\\bbaseline\\b|pre[- ]?treatment|pretreatment|pre[- ]?intervention",
      "pre[- ]?dose|predose|screening|enrollment|randomi[sz]ation",
      "before treatment|before intervention|before first dose",
      "prior to treatment|prior to randomi[sz]ation",
      sep = "|"
    )
  )
  post <- text_has(
    text,
    paste(
      "post[- ]?baseline|post[- ]?treatment|post[- ]?intervention",
      "post[- ]?dose|follow[- ]?up|endpoint|final|end of treatment",
      "end-of-treatment|end of study|last observation|last visit|discharge",
      "\\bweek [1-9][0-9.]*\\b|\\bmonth [1-9][0-9.]*\\b",
      "\\bday [1-9][0-9.]*\\b|\\byear [1-9][0-9.]*\\b",
      "\\bvisit ([2-9]|[1-9][0-9]+)\\b",
      sep = "|"
    )
  )

  case_when(
    reported_change ~ "reported_change",
    baseline & post ~ "ambiguous_time",
    baseline ~ "baseline",
    post ~ "post",
    text_has(text, "day 0|week 0|month 0|visit 1") & baseline ~ "baseline",
    TRUE ~ "not_time_related"
  )
}

time_label <- function(classification, category, title, description, time_frame) {
  norm_text(str_c(
    coalesce(classification, ""), coalesce(category, ""), coalesce(title, ""),
    coalesce(description, ""), coalesce(time_frame, ""), sep = " "
  ))
}

parse_time_days <- function(x) {
  x <- norm_text(x)
  value <- as_num(str_match(x, "\\b(day|week|month|year)\\s*([0-9.]+)\\b")[, 3])
  unit <- str_match(x, "\\b(day|week|month|year)\\s*([0-9.]+)\\b")[, 2]
  mult <- case_when(
    unit == "day" ~ 1,
    unit == "week" ~ 7,
    unit == "month" ~ 30.4375,
    unit == "year" ~ 365.25,
    TRUE ~ NA_real_
  )
  value * mult
}

post_priority <- function(label, outcome_time_frame) {
  label <- norm_text(label)
  tf <- norm_text(outcome_time_frame)
  case_when(
    text_has(label, "final|endpoint|end of treatment|end-of-treatment|end of study|last") ~ 1L,
    !is.na(parse_time_days(label)) & parse_time_days(label) > 0 ~ 2L,
    !is.na(tf) & tf != "" & str_detect(label, fixed(tf)) ~ 3L,
    TRUE ~ 4L
  )
}

param_family <- function(source_row_class, param_type_norm) {
  case_when(
    source_row_class %in% c(
      "continuous_mean_sd", "continuous_mean_se", "continuous_mean_ci",
      "continuous_lsm_se", "continuous_lsm_ci"
    ) ~ "continuous_mean",
    source_row_class %in% c(
      "binary_count_explicit", "binary_complement_pair",
      "binary_number_count", "binary_percent_explicit"
    ) ~ "binary_event",
    param_type_norm %in% c("mean", "least_squares_mean") ~ "continuous_mean",
    param_type_norm %in% c("count_of_participants", "number") ~ "binary_event",
    TRUE ~ NA_character_
  )
}

prep_measurements <- function(df, source = "outcome") {
  if (source == "baseline") {
    df <- df %>%
      mutate(
        source_row_id = str_c("baseline_measurement:", baseline_measurement_id),
        time_role = "baseline"
      )
  } else {
    df <- df %>%
      mutate(
        source_row_id = str_c("outcome_measurement:", outcome_measurement_id),
        time_role = classify_time_role(
          classification, category, measurement_title, measurement_description,
          outcome_title, outcome_time_frame
        )
      )
  }

  df %>%
    mutate(
      param_family = param_family(source_row_class, param_type_norm),
      time_label = time_label(
        classification, category, measurement_title, measurement_description,
        outcome_time_frame
      ),
      time_days = parse_time_days(time_label),
      construct_key = construct_key(
        outcome_title, classification, category, measurement_title,
        measurement_description, measurement_units_norm, param_family
      ),
      ci_level = ci_level_from_dispersion(dispersion_type_norm),
      row_mean = if_else(param_family == "continuous_mean", param_value_num, NA_real_),
      row_sd = if_else(
        source_row_class == "continuous_mean_sd",
        dispersion_value_num,
        NA_real_
      ),
      row_se = case_when(
        source_row_class == "continuous_mean_sd" & outcome_count > 0 ~
          dispersion_value_num / sqrt(outcome_count),
        source_row_class %in% c("continuous_mean_se", "continuous_lsm_se") ~
          dispersion_value_num,
        source_row_class %in% c("continuous_mean_ci", "continuous_lsm_ci") &
          !is.na(ci_level) & ci_level > 0 & ci_level < 100 ~
          (dispersion_upper_limit - dispersion_lower_limit) /
            (2 * qnorm(1 - (1 - ci_level / 100) / 2)),
        TRUE ~ NA_real_
      ),
      row_x = if_else(param_family == "binary_event", x_eff, NA_real_),
      row_n = outcome_count,
      row_risk = safe_divide(row_x, row_n),
      x_is_fractional = coalesce(
        x_is_fractional,
        !is.na(row_x) & abs(row_x - round(row_x)) > 1e-8
      )
    )
}

make_prepost_pairs <- function(pre_rows, post_rows, source, tier,
                               group_match_method) {
  join_cols <- c(
    "nct_id", "outcome_id", "result_group_id", "measurement_units_norm",
    "param_family", "construct_key"
  )

  inner_join(
    pre_rows,
    post_rows,
    by = join_cols,
    suffix = c("_pre", "_post"),
    relationship = "many-to-many"
  ) %>%
    group_by(
      nct_id, outcome_id, result_group_id, measurement_units_norm,
      param_family, construct_key, source_row_id_pre
    ) %>%
    arrange(
      post_priority(time_label_post, outcome_time_frame_post),
      desc(coalesce(time_days_post, -Inf)),
      source_row_id_post,
      .by_group = TRUE
    ) %>%
    mutate(
      pair_selection_rank = row_number(),
      preferred_post = pair_selection_rank == 1L
    ) %>%
    ungroup() %>%
    transmute(
      nct_id, outcome_id, result_group_id,
      pre_row_id = source_row_id_pre,
      post_row_id = source_row_id_post,
      prepost_source = source,
      prepost_match_tier = tier,
      construct_key,
      pair_selection_rank, preferred_post,
      pair_type = if_else(param_family == "continuous_mean", "continuous", "binary"),
      time_role_pre = time_role_pre,
      time_role_post = time_role_post,
      time_pre_label = time_label_pre,
      time_post_label = time_label_post,
      time_post_days = time_days_post,
      group_match_method = group_match_method,
      time_role_rule_id = "v3_text_time_role",
      source_row_class_pre = source_row_class_pre,
      source_row_class_post = source_row_class_post,
      group_title = group_title_post,
      group_description = group_description_post,
      group_role = group_role_post,
      group_role_source = group_role_source_post,
      outcome_title = outcome_title_post,
      outcome_time_frame = outcome_time_frame_post,
      measurement_units = measurement_units_post,
      param_type_pre = param_type_pre,
      param_type_post = param_type_post,
      n_pre = row_n_pre,
      n_post = row_n_post,
      mean_pre = row_mean_pre,
      mean_post = row_mean_post,
      sd_pre = row_sd_pre,
      sd_post = row_sd_post,
      se_pre = row_se_pre,
      se_post = row_se_post,
      ci_pre_lower = dispersion_lower_limit_pre,
      ci_pre_upper = dispersion_upper_limit_pre,
      ci_post_lower = dispersion_lower_limit_post,
      ci_post_upper = dispersion_upper_limit_post,
      change_mean = mean_post - mean_pre,
      change_direction = if_else(pair_type == "continuous", "post_minus_pre", NA_character_),
      x_pre = row_x_pre,
      x_post = row_x_post,
      risk_pre = row_risk_pre,
      risk_post = row_risk_post,
      risk_change = risk_post - risk_pre,
      x_is_fractional_pre = x_is_fractional_pre,
      x_is_fractional_post = x_is_fractional_post,
      derived_from_percentage_pre = derived_from_percentage_pre,
      derived_from_percentage_post = derived_from_percentage_post,
      warning_flags = pmap_chr(
        list(
          non_preferred_post = !preferred_post,
          lsm_or_adjusted_summary =
            source_row_class_pre %in% c("continuous_lsm_se", "continuous_lsm_ci") |
              source_row_class_post %in% c("continuous_lsm_se", "continuous_lsm_ci"),
          fractional_binary_count =
            coalesce(x_is_fractional_pre, FALSE) |
              coalesce(x_is_fractional_post, FALSE)
        ),
        warning_list
      )
    ) %>%
    mutate(
      n_change = pmin(n_pre, n_post, na.rm = TRUE),
      se_change_rho_0 = if_else(
        pair_type == "continuous" & !is.na(sd_pre) & !is.na(sd_post) &
          n_change > 0,
        sqrt(sd_pre^2 + sd_post^2) / sqrt(n_change),
        NA_real_
      ),
      se_change_rho_025 = if_else(
        pair_type == "continuous" & !is.na(sd_pre) & !is.na(sd_post) &
          n_change > 0,
        sqrt(sd_pre^2 + sd_post^2 - 2 * 0.25 * sd_pre * sd_post) /
          sqrt(n_change),
        NA_real_
      ),
      se_change_rho_05 = if_else(
        pair_type == "continuous" & !is.na(sd_pre) & !is.na(sd_post) &
          n_change > 0,
        sqrt(sd_pre^2 + sd_post^2 - 2 * 0.5 * sd_pre * sd_post) /
          sqrt(n_change),
        NA_real_
      ),
      se_change_rho_075 = if_else(
        pair_type == "continuous" & !is.na(sd_pre) & !is.na(sd_post) &
          n_change > 0,
        sqrt(sd_pre^2 + sd_post^2 - 2 * 0.75 * sd_pre * sd_post) /
          sqrt(n_change),
        NA_real_
      ),
      se_risk_change_independent = if_else(
        pair_type == "binary" & n_pre > 0 & n_post > 0,
        sqrt(risk_pre * (1 - risk_pre) / n_pre +
          risk_post * (1 - risk_post) / n_post),
        NA_real_
      ),
      se_change_status = case_when(
        pair_type == "continuous" & !is.na(se_change_rho_0) ~
          "sensitivity_rho_grid",
        pair_type == "binary" & !is.na(se_risk_change_independent) ~
          "sensitivity_independent_timepoints",
        TRUE ~ "not_available"
      )
    ) %>%
    select(-n_change)
}

read_aact <- function(file, col_select) {
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

stream_filtered_table <- function(file, col_select, keep_fun, chunk_size = 200000) {
  out <- list()
  callback <- DataFrameCallback$new(function(x, pos) {
    kept <- x %>%
      select(any_of(col_select)) %>%
      keep_fun()
    if (nrow(kept) > 0) out[[length(out) + 1L]] <<- kept
  })
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
  bind_rows(out)
}

pair_key <- function(a, b) {
  if_else(a < b, str_c(a, b, sep = "||"), str_c(b, a, sep = "||"))
}

orient_prepost_pairs <- function(pairs, author_pairs) {
  comparator_roles <- c(
    "comparator_placebo", "comparator_sham", "comparator_no_intervention",
    "comparator_usual_care", "comparator_control", "comparator_active"
  )
  clear_comparators <- setdiff(comparator_roles, "comparator_active")

  pairs %>%
    mutate(
      pair_group_key = pair_key(result_group_id_a, result_group_id_b),
      a_comparator = group_role_a %in% comparator_roles,
      b_comparator = group_role_b %in% comparator_roles,
      a_clear_comparator = group_role_a %in% clear_comparators,
      b_clear_comparator = group_role_b %in% clear_comparators,
      a_active = group_role_a == "active",
      b_active = group_role_b == "active"
    ) %>%
    left_join(author_pairs, by = c("nct_id", "outcome_id", "pair_group_key")) %>%
    group_by(nct_id, outcome_id, construct_key) %>%
    mutate(
      n_clear_comparators = n_distinct(c(
        result_group_id_a[group_role_a %in% clear_comparators],
        result_group_id_b[group_role_b %in% clear_comparators]
      )),
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
      pair_selection_rule = case_when(
        author_linked_pair ~ "author_analysis_groups_exact_pair",
        direction_known & (a_clear_comparator | b_clear_comparator) ~
          "active_vs_clear_comparator",
        TRUE ~ "unordered_two_arm_diagnostic"
      ),
      pair_dependency_class = case_when(
        author_linked_pair ~ "author_selected_pair",
        n_distinct(c(result_group_id_a, result_group_id_b)) <= 2 ~
          "independent_two_arm",
        direction_known & (a_clear_comparator | b_clear_comparator) ~
          "multi_arm_shared_comparator",
        a_active & b_active ~ "multi_arm_active_active",
        TRUE ~ "multi_arm_unknown"
      )
    ) %>%
    ungroup()
}

make_contrasts <- function(pairs, author_pairs) {
  empty_contrasts <- tibble(
    prepost_contrast_candidate_id = character(),
    nct_id = character(), outcome_id = character(), construct_key = character(),
    pair_type = character(), prepost_source = character(),
    prepost_match_tier = character(), prepost_pair_id_t = character(),
    prepost_pair_id_c = character(), group_t_id = character(),
    group_c_id = character(), group_t_title = character(),
    group_c_title = character(), did_measure = character(),
    did_estimate = double(), did_se_rho_0 = double(),
    did_z_rho_0 = double(), did_se_rho_025 = double(),
    did_z_rho_025 = double(), did_se_rho_05 = double(),
    did_z_rho_05 = double(), did_se_rho_075 = double(),
    did_z_rho_075 = double(), did_se_independent_timepoints = double(),
    did_z_independent_timepoints = double(), did_se_status = character(),
    did_z_status = character(), did_validity = character(),
    direction_known = logical(), orientation_rule = character(),
    pair_selection_rule = character(), pair_dependency_class = character(),
    author_linked_pair = logical(), warning_flags = character()
  )

  selected <- pairs %>%
    filter(preferred_post) %>%
    select(
      prepost_pair_id, nct_id, outcome_id, result_group_id, construct_key,
      pair_type, prepost_source, prepost_match_tier, group_title, group_role,
      group_role_source, change_mean, risk_change, se_change_rho_0,
      se_change_rho_025, se_change_rho_05, se_change_rho_075,
      se_risk_change_independent, warning_flags
    )

  a <- selected %>%
    rename_with(~ paste0(.x, "_a"), c(
      "prepost_pair_id", "result_group_id", "group_title", "group_role",
      "group_role_source", "change_mean", "risk_change", "se_change_rho_0",
      "se_change_rho_025", "se_change_rho_05", "se_change_rho_075",
      "se_risk_change_independent", "warning_flags"
    ))
  b <- selected %>%
    rename_with(~ paste0(.x, "_b"), c(
      "prepost_pair_id", "result_group_id", "group_title", "group_role",
      "group_role_source", "change_mean", "risk_change", "se_change_rho_0",
      "se_change_rho_025", "se_change_rho_05", "se_change_rho_075",
      "se_risk_change_independent", "warning_flags"
    ))

  paired <- inner_join(
    a, b,
    by = c(
      "nct_id", "outcome_id", "construct_key", "pair_type",
      "prepost_source", "prepost_match_tier"
    ),
    relationship = "many-to-many"
  ) %>%
    filter(result_group_id_a < result_group_id_b) %>%
    orient_prepost_pairs(author_pairs)

  if (nrow(paired) == 0) return(empty_contrasts)

  paired %>%
    mutate(
      prepost_pair_id_t = if_else(use_a_as_treatment, prepost_pair_id_a, prepost_pair_id_b),
      prepost_pair_id_c = if_else(use_a_as_treatment, prepost_pair_id_b, prepost_pair_id_a),
      group_t_id = if_else(use_a_as_treatment, result_group_id_a, result_group_id_b),
      group_c_id = if_else(use_a_as_treatment, result_group_id_b, result_group_id_a),
      group_t_title = if_else(use_a_as_treatment, group_title_a, group_title_b),
      group_c_title = if_else(use_a_as_treatment, group_title_b, group_title_a),
      change_t = if_else(use_a_as_treatment, change_mean_a, change_mean_b),
      change_c = if_else(use_a_as_treatment, change_mean_b, change_mean_a),
      risk_change_t = if_else(use_a_as_treatment, risk_change_a, risk_change_b),
      risk_change_c = if_else(use_a_as_treatment, risk_change_b, risk_change_a),
      se_t_rho_0 = if_else(use_a_as_treatment, se_change_rho_0_a, se_change_rho_0_b),
      se_c_rho_0 = if_else(use_a_as_treatment, se_change_rho_0_b, se_change_rho_0_a),
      se_t_rho_025 = if_else(use_a_as_treatment, se_change_rho_025_a, se_change_rho_025_b),
      se_c_rho_025 = if_else(use_a_as_treatment, se_change_rho_025_b, se_change_rho_025_a),
      se_t_rho_05 = if_else(use_a_as_treatment, se_change_rho_05_a, se_change_rho_05_b),
      se_c_rho_05 = if_else(use_a_as_treatment, se_change_rho_05_b, se_change_rho_05_a),
      se_t_rho_075 = if_else(use_a_as_treatment, se_change_rho_075_a, se_change_rho_075_b),
      se_c_rho_075 = if_else(use_a_as_treatment, se_change_rho_075_b, se_change_rho_075_a),
      se_t_bin = if_else(
        use_a_as_treatment,
        se_risk_change_independent_a,
        se_risk_change_independent_b
      ),
      se_c_bin = if_else(
        use_a_as_treatment,
        se_risk_change_independent_b,
        se_risk_change_independent_a
      ),
      did_estimate = if_else(
        pair_type == "continuous",
        change_t - change_c,
        risk_change_t - risk_change_c
      ),
      did_measure = if_else(
        pair_type == "continuous",
        "mean_difference_in_change",
        "risk_difference_in_change"
      ),
      did_se_rho_0 = sqrt(se_t_rho_0^2 + se_c_rho_0^2),
      did_z_rho_0 = safe_divide(did_estimate, did_se_rho_0),
      did_se_rho_025 = sqrt(se_t_rho_025^2 + se_c_rho_025^2),
      did_z_rho_025 = safe_divide(did_estimate, did_se_rho_025),
      did_se_rho_05 = sqrt(se_t_rho_05^2 + se_c_rho_05^2),
      did_z_rho_05 = safe_divide(did_estimate, did_se_rho_05),
      did_se_rho_075 = sqrt(se_t_rho_075^2 + se_c_rho_075^2),
      did_z_rho_075 = safe_divide(did_estimate, did_se_rho_075),
      did_se_independent_timepoints = sqrt(se_t_bin^2 + se_c_bin^2),
      did_z_independent_timepoints =
        safe_divide(did_estimate, did_se_independent_timepoints),
      did_se_status = if_else(
        pair_type == "continuous",
        "sensitivity_rho_grid",
        "sensitivity_independent_timepoints"
      ),
      did_z_status = "sensitivity_only",
      did_validity = if_else(direction_known, "sensitivity_only", "diagnostic_only"),
      warning_flags = pmap_chr(
        list(
          direction_unknown = !direction_known,
          source_a_warning = !is.na(warning_flags_a),
          source_b_warning = !is.na(warning_flags_b)
        ),
        warning_list
      )
    ) %>%
    transmute(
      prepost_contrast_candidate_id =
        make_id("ctgov_prepost_contrast", n()),
      nct_id, outcome_id, construct_key, pair_type, prepost_source,
      prepost_match_tier, prepost_pair_id_t, prepost_pair_id_c,
      group_t_id, group_c_id, group_t_title, group_c_title,
      did_measure, did_estimate, did_se_rho_0, did_z_rho_0,
      did_se_rho_025, did_z_rho_025, did_se_rho_05, did_z_rho_05,
      did_se_rho_075, did_z_rho_075, did_se_independent_timepoints,
      did_z_independent_timepoints, did_se_status, did_z_status,
      did_validity, direction_known, orientation_rule, pair_selection_rule,
      pair_dependency_class, author_linked_pair, warning_flags
    )
}

# Source A: internal outcome_measurement pairs -----

source_rows <- readRDS(file.path(out_dir, "endpoint_source_rows.rds"))

continuous_classes <- c(
  "continuous_mean_sd", "continuous_mean_se", "continuous_mean_ci",
  "continuous_lsm_se", "continuous_lsm_ci"
)
binary_classes <- c(
  "binary_count_explicit", "binary_complement_pair",
  "binary_number_count", "binary_percent_explicit"
)
eligible_classes <- c(
  continuous_classes, binary_classes
)

outcome_rows <- source_rows %>%
  mutate(
    source_row_class_endpoint = source_row_class,
    prepost_source_row_class = case_when(
      source_row_candidate_class %in% continuous_classes ~
        source_row_candidate_class,
      source_row_class %in% binary_classes ~ source_row_class,
      source_row_class %in% continuous_classes ~ source_row_class,
      TRUE ~ NA_character_
    ),
    source_row_class = prepost_source_row_class
  ) %>%
  filter(
    !is.na(source_row_class),
    is_participant_count,
    !is.na(outcome_count),
    result_type_norm == "outcome"
  ) %>%
  prep_measurements("outcome")

outcome_pre <- outcome_rows %>%
  filter(time_role == "baseline")
outcome_post <- outcome_rows %>%
  filter(time_role == "post")

message("Pairing source A internal outcome pre/post rows")
source_a_pairs <- make_prepost_pairs(
  outcome_pre, outcome_post,
  source = "outcome_measurements_internal",
  tier = "A",
  group_match_method = "same_result_group_id"
)

# Source B: exact baseline_measurements to outcome_measurements matches -----

outcome_post_for_b <- outcome_post %>%
  anti_join(
    source_a_pairs %>%
      distinct(nct_id, outcome_id, result_group_id, construct_key),
    by = c("nct_id", "outcome_id", "result_group_id", "construct_key")
  )

relevant_nct_ids <- unique(outcome_post_for_b$nct_id)

message("Streaming baseline result groups for ", length(relevant_nct_ids), " trials")
baseline_groups <- stream_filtered_table(
  "result_groups.txt",
  c(
    "id", "nct_id", "ctgov_group_code", "result_type", "title",
    "description", "outcome_id"
  ),
  function(x) {
    x %>%
      mutate(result_type_norm = norm_code(result_type)) %>%
      filter(nct_id %in% relevant_nct_ids, result_type_norm == "baseline") %>%
      transmute(
        baseline_result_group_id = id,
        nct_id,
        baseline_ctgov_group_code = ctgov_group_code,
        baseline_group_title = title,
        baseline_group_description = description,
        baseline_group_title_norm = norm_text(title),
        baseline_group_description_norm = norm_text(description),
        baseline_group_key = str_c(
          baseline_group_title_norm, baseline_group_description_norm, sep = "|"
        )
      )
  }
)

baseline_group_ids <- unique(baseline_groups$baseline_result_group_id)

message("Streaming baseline counts")
baseline_counts <- stream_filtered_table(
  "baseline_counts.txt",
  c("id", "nct_id", "result_group_id", "ctgov_group_code", "units", "scope", "count"),
  function(x) {
    x %>%
      filter(result_group_id %in% baseline_group_ids) %>%
      mutate(
        baseline_count = as_num(count),
        baseline_count_units_norm = norm_code(units),
        baseline_scope_norm = norm_code(scope)
      ) %>%
      filter(
        baseline_count_units_norm %in% c("participants", "participant"),
        baseline_scope_norm == "overall"
      ) %>%
      transmute(
        nct_id, baseline_result_group_id = result_group_id,
        baseline_count
      )
  }
) %>%
  group_by(nct_id, baseline_result_group_id) %>%
  summarise(baseline_count = max(baseline_count, na.rm = TRUE), .groups = "drop")

message("Streaming baseline measurements")
baseline_measurements <- stream_filtered_table(
  "baseline_measurements.txt",
  c(
    "id", "nct_id", "result_group_id", "ctgov_group_code",
    "classification", "category", "title", "description", "units",
    "param_type", "param_value", "param_value_num", "dispersion_type",
    "dispersion_value", "dispersion_value_num", "dispersion_lower_limit",
    "dispersion_upper_limit", "number_analyzed", "number_analyzed_units"
  ),
  function(x) {
    x %>%
      filter(result_group_id %in% baseline_group_ids) %>%
      rename(
        baseline_measurement_id = id,
        baseline_result_group_id = result_group_id,
        baseline_ctgov_group_code = ctgov_group_code,
        measurement_title = title,
        measurement_description = description,
        measurement_units = units
      ) %>%
      mutate(
        param_value_num = as_num(param_value_num),
        dispersion_value_num = as_num(dispersion_value_num),
        dispersion_lower_limit = as_num(dispersion_lower_limit),
        dispersion_upper_limit = as_num(dispersion_upper_limit),
        number_analyzed = as_num(number_analyzed),
        classification_norm = norm_text(classification),
        category_norm = norm_text(category),
        measurement_title_norm = norm_text(measurement_title),
        measurement_description_norm = norm_text(measurement_description),
        measurement_units_norm = norm_code(measurement_units),
        param_type_norm = norm_param_type(param_type),
        dispersion_type_norm = norm_dispersion_type(dispersion_type)
      )
  }
)

baseline_rows <- baseline_measurements %>%
  left_join(baseline_groups, by = c(
    "nct_id", "baseline_result_group_id", "baseline_ctgov_group_code"
  )) %>%
  left_join(baseline_counts, by = c("nct_id", "baseline_result_group_id")) %>%
  mutate(
    outcome_count = coalesce(number_analyzed, baseline_count),
    outcome_count_units = number_analyzed_units,
    outcome_count_units_norm = norm_code(number_analyzed_units),
    is_participant_count = is.na(outcome_count_units_norm) |
      outcome_count_units_norm %in% c("participants", "participant"),
    source_row_class = case_when(
      param_type_norm == "mean" & dispersion_type_norm == "sd" &
        !is.na(param_value_num) & !is.na(dispersion_value_num) &
        dispersion_value_num > 0 ~ "continuous_mean_sd",
      param_type_norm == "mean" & dispersion_type_norm == "se" &
        !is.na(param_value_num) & !is.na(dispersion_value_num) &
        dispersion_value_num > 0 ~ "continuous_mean_se",
      param_type_norm == "mean" &
        !is.na(ci_level_from_dispersion(dispersion_type_norm)) &
        !is.na(param_value_num) & !is.na(dispersion_lower_limit) &
        !is.na(dispersion_upper_limit) &
        dispersion_upper_limit > dispersion_lower_limit ~
        "continuous_mean_ci",
      param_type_norm == "count_of_participants" & !is.na(param_value_num) ~
        "binary_count_explicit",
      TRUE ~ NA_character_
    ),
    x_eff = if_else(
      source_row_class == "binary_count_explicit",
      param_value_num,
      NA_real_
    ),
    x_is_fractional = !is.na(x_eff) & abs(x_eff - round(x_eff)) > 1e-8,
    derived_from_percentage = FALSE,
    outcome_title = measurement_title,
    outcome_time_frame = "baseline",
    outcome_id = NA_character_,
    result_group_id = NA_character_,
    group_title = NA_character_,
    group_description = NA_character_,
    group_role = NA_character_,
    group_role_source = NA_character_
  ) %>%
  filter(
    source_row_class %in% eligible_classes,
    is_participant_count,
    !is.na(outcome_count), outcome_count > 0
  )

outcome_group_keys <- outcome_post_for_b %>%
  mutate(
    outcome_group_title_norm = norm_text(group_title),
    outcome_group_description_norm = norm_text(group_description),
    baseline_group_key = str_c(
      outcome_group_title_norm, outcome_group_description_norm, sep = "|"
    )
  ) %>%
  select(
    nct_id, outcome_id, result_group_id, outcome_group_title_norm,
    outcome_group_description_norm, baseline_group_key, group_title,
    group_description, group_role, group_role_source
  ) %>%
  distinct()

baseline_pre_for_b <- baseline_rows %>%
  inner_join(
    outcome_group_keys,
    by = c("nct_id", "baseline_group_key"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    result_group_id = result_group_id.y,
    outcome_id = outcome_id.y,
    group_title = group_title.y,
    group_description = group_description.y,
    group_role = group_role.y,
    group_role_source = group_role_source.y,
    group_match_method = "exact_group_title_description",
    outcome_title = measurement_title,
    outcome_time_frame = "baseline"
  ) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  prep_measurements("baseline")

source_b_pairs <- make_prepost_pairs(
  baseline_pre_for_b,
  outcome_post_for_b,
  source = "baseline_measurements_matched",
  tier = "B",
  group_match_method = "exact_group_title_description"
)

# Save group-level pre/post and contrast side tables -----

prepost_pairs <- bind_rows(source_a_pairs, source_b_pairs) %>%
  arrange(nct_id, outcome_id, result_group_id, construct_key, pair_selection_rank) %>%
  mutate(prepost_pair_id = make_id("ctgov_prepost_pair", n())) %>%
  relocate(prepost_pair_id)

author_links <- readRDS(file.path(input_dir, "author_analysis_links.rds"))
author_pairs <- author_links %>%
  filter(n_linked_groups == 2, !is.na(linked_result_group_ids)) %>%
  mutate(
    linked_ids = str_split(linked_result_group_ids, ";"),
    pair_group_key = map_chr(linked_ids, ~ pair_key(.x[1], .x[2])),
    author_linked_pair = TRUE
  ) %>%
  distinct(nct_id, outcome_id, pair_group_key, author_linked_pair)

prepost_contrasts <- make_contrasts(prepost_pairs, author_pairs)

reported_change_overlap <- outcome_rows %>%
  filter(time_role == "reported_change") %>%
  select(
    nct_id, outcome_id, result_group_id, construct_key,
    reported_change_row_id = source_row_id,
    reported_change_value = row_mean,
    reported_change_title = measurement_title,
    reported_change_time_label = time_label
  ) %>%
  inner_join(
    prepost_pairs %>%
      filter(pair_type == "continuous") %>%
      select(
        prepost_pair_id, nct_id, outcome_id, result_group_id, construct_key,
        reconstructed_change = change_mean, prepost_match_tier,
        prepost_source, preferred_post
      ),
    by = c("nct_id", "outcome_id", "result_group_id", "construct_key"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    absolute_difference =
      abs(reported_change_value - reconstructed_change),
    notes = "diagnostic_overlap_only"
  )

prepost_summary <- prepost_pairs %>%
  count(
    prepost_source, prepost_match_tier, pair_type, source_row_class_pre,
    source_row_class_post, se_change_status, preferred_post, name = "n"
  ) %>%
  arrange(prepost_source, prepost_match_tier, pair_type, desc(n))

contrast_summary <- prepost_contrasts %>%
  count(
    prepost_source, prepost_match_tier, pair_type, did_measure,
    did_validity, direction_known, pair_selection_rule, name = "n"
  ) %>%
  arrange(prepost_source, prepost_match_tier, pair_type, desc(n))

saveRDS(prepost_pairs, file.path(out_dir, "prepost_measurement_pairs.rds"))
saveRDS(
  prepost_contrasts,
  file.path(out_dir, "prepost_contrast_candidates.rds")
)

write_csv(
  prepost_summary,
  file.path(validation_dir, "prepost_measurement_pairs_summary.csv")
)
write_csv(
  contrast_summary,
  file.path(validation_dir, "prepost_contrast_candidates_summary.csv")
)
write_csv(
  reported_change_overlap,
  file.path(validation_dir, "prepost_reported_change_overlap.csv")
)

message("Saved ", nrow(prepost_pairs), " pre/post group-level pairs.")
message("Saved ", nrow(prepost_contrasts), " pre/post contrast candidates.")
