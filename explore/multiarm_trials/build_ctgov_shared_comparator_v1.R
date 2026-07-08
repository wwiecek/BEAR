# Build a clinicaltrials.gov multi-arm dataset with one shared comparator
# per trial and BEAR-like effect-size columns.
# Drops Cochrane because review-level comparison labels are not arm-clean.

library(tidyverse)

zip_path <- "data_raw/clinicaltrials.gov/12082025/a1hlrix83cqvhi5qn0jya3f8mdik.zip"
out_dir <- "explore/multiarm_trials"


# Helpers -----

read_zip_delim <- function(file, col_names) {
  read_delim(
    unz(zip_path, file),
    delim = "|",
    show_col_types = FALSE,
    col_types = cols(.default = "c"),
    col_select = all_of(col_names)
  )
}


first_nonmissing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }
  x[[1]]
}


classify_measure <- function(x) {
  x0 <- str_squish(str_to_lower(x))

  out <- case_when(
    str_detect(x0, "odds\\s*ratio") ~ "Odds Ratio",
    str_detect(
      x0,
      "(^|\\W)risk\\s*ratio(\\W|$)|relative\\s*risk"
    ) ~ "Risk Ratio",
    str_detect(x0, "hazard\\s*ratio|cox\\s*proportional") ~ "Hazard Ratio",
    str_detect(x0, "(^|\\W)mean\\s*difference(\\W|$)") |
      str_detect(
        x0,
        paste(
          "ls\\s*mean\\s*difference|least\\s*square[s]?\\s*mean",
          "\\s*difference|lsm\\s*difference",
          sep = ""
        )
      ) |
      str_detect(
        x0,
        paste(
          "difference\\s*in\\s*ls\\s*means|difference\\s*of\\s*least",
          "\\s*squares\\s*means",
          sep = ""
        )
      ) |
      str_detect(
        x0,
        paste(
          "adjusted\\s*mean\\s*difference|treatment\\s*difference$|",
          "vaccine\\s*group\\s*difference",
          sep = ""
        )
      ) |
      str_detect(
        x0,
        "difference\\s*\\(change\\s*from\\s*baseline\\)"
      ) ~ "Mean Difference",
    str_detect(
      x0,
      paste(
        "risk\\s*difference|(^|\\W)rd(\\W|$)|difference\\s*in",
        "\\s*proportions?",
        sep = ""
      )
    ) |
      str_detect(
        x0,
        paste(
          "percentage\\s*of\\s*participants|difference\\s*in",
          "\\s*percentage\\s*of\\s*participants",
          sep = ""
        )
      ) ~ "Risk Difference",
    str_detect(
      x0,
      "percent(age)?\\s*difference|difference\\s*in\\s*percent(age|ages)?s?"
    ) ~ "Difference in Percentages",
    str_detect(x0, "rate\\s*ratio") ~ "Ratio",
    str_detect(x0, "geometric.*ratio|\\bgmr\\b|gmt\\s*ratio|gmc\\s*ratio") ~
      "Geometric Ratio",
    str_detect(x0, "ratio\\s*of\\s*.*geometric|test/\\s*ref|t/\\s*r") ~
      "Geometric Ratio",
    str_detect(x0, "(^|\\W)ratio(\\W|$)") ~ "Other Ratio",
    TRUE ~ "Other"
  )

  factor(
    out,
    levels = c(
      "Mean Difference",
      "Odds Ratio",
      "Risk Ratio",
      "Hazard Ratio",
      "Geometric Ratio",
      "Risk Difference",
      "Difference in Percentages",
      "Ratio",
      "Other"
    )
  )
}


zcrit <- function(ci_percent, sides) {
  ci_percent <- ifelse(is.na(ci_percent), 0.95, as.numeric(ci_percent))
  sides <- ifelse(is.na(sides) | !(sides %in% c(1, 2)), 2L, as.integer(sides))
  alpha <- pmax(1 - ci_percent / 100, .Machine$double.eps)
  ifelse(sides == 1L, qnorm(1 - alpha), qnorm(1 - alpha / 2))
}


safe_scale_transform <- function(x, scale) {
  out <- rep(NA_real_, length(x))
  raw_idx <- which(!is.na(scale) & scale == "raw")
  log_idx <- which(!is.na(scale) & scale == "log" & is.finite(x) & x > 0)
  out[raw_idx] <- x[raw_idx]
  out[log_idx] <- log(x[log_idx])
  out
}


normalise_text <- function(x) {
  x %>%
    coalesce("") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}


strip_group_prefix <- function(x) {
  x %>%
    str_replace(
      paste(
        "^(arm|group|panel|cohort|dose escalation|treatment period|phase)",
        "\\s+[a-z0-9/() -]+[:\\-]\\s*",
        sep = ""
      ),
      ""
    ) %>%
    str_replace(
      "^(arm|group|panel|cohort)\\s+[a-z0-9/() -]+\\s+",
      ""
    ) %>%
    str_squish()
}


label_variants <- function(title, description) {
  vals <- c(
    normalise_text(title),
    normalise_text(strip_group_prefix(title)),
    normalise_text(description),
    normalise_text(strip_group_prefix(description))
  )

  vals <- vals[nzchar(vals)]
  vals <- vals[nchar(vals) >= 4]
  unique(vals)
}


first_match_pos <- function(text, patterns) {
  text <- normalise_text(text)
  patterns <- unique(patterns)

  if (!nzchar(text) || length(patterns) == 0) {
    return(NA_integer_)
  }

  pos <- vapply(
    patterns,
    function(pat) {
      loc <- str_locate(text, fixed(pat))[1, 1]
      ifelse(is.na(loc), Inf, loc)
    },
    numeric(1)
  )

  pos <- pos[is.finite(pos)]
  if (length(pos) == 0) {
    return(NA_integer_)
  }
  as.integer(min(pos))
}


infer_direction <- function(text, active_title, active_desc,
                            comparator_title, comparator_desc) {
  text <- normalise_text(text)
  active_pos <- first_match_pos(
    text,
    label_variants(active_title, active_desc)
  )
  comparator_pos <- first_match_pos(
    text,
    label_variants(comparator_title, comparator_desc)
  )

  if (is.na(active_pos) || is.na(comparator_pos) || active_pos == comparator_pos) {
    return(tibble(
      direction_active_vs_comparator = NA_real_,
      orientation_source = NA_character_
    ))
  }

  tibble(
    direction_active_vs_comparator = if_else(active_pos < comparator_pos, 1, -1),
    orientation_source = "text_order"
  )
}


control_regex <- regex(
  paste(
    "placebo|control|standard of care|usual care|sham|vehicle|no treatment|",
    "waiting list|observation|monotherapy|standard therapy|standard treatment",
    sep = ""
  ),
  ignore_case = TRUE
)


# Read source tables -----

studies <- read_zip_delim(
  "studies.txt",
  c(
    "nct_id", "brief_title", "overall_status", "completion_date",
    "study_type", "phase", "enrollment", "number_of_arms"
  )
)

designs <- read_zip_delim(
  "designs.txt",
  c(
    "nct_id", "allocation", "intervention_model", "primary_purpose",
    "masking"
  )
)

outcomes <- read_zip_delim(
  "outcomes.txt",
  c("nct_id", "id", "outcome_type", "title", "time_frame")
) %>%
  rename(
    outcome_id = id,
    outcome_title = title,
    outcome_time_frame = time_frame
  )

outcome_analyses <- read_zip_delim(
  "outcome_analyses.txt",
  c(
    "nct_id", "outcome_id", "id", "param_type", "param_value",
    "dispersion_type", "dispersion_value", "p_value_modifier", "p_value",
    "ci_n_sides", "ci_percent", "ci_lower_limit", "ci_upper_limit",
    "estimate_description", "groups_description", "method"
  )
) %>%
  rename(outcome_analysis_id = id)

outcome_analysis_groups <- read_zip_delim(
  "outcome_analysis_groups.txt",
  c("nct_id", "outcome_analysis_id", "result_group_id", "ctgov_group_code")
)

result_groups <- read_zip_delim(
  "result_groups.txt",
  c(
    "id", "nct_id", "ctgov_group_code", "result_type", "title",
    "description", "outcome_id"
  )
) %>%
  transmute(
    result_group_id = id,
    nct_id,
    ctgov_group_code,
    result_type,
    group_title = title,
    group_description = description,
    outcome_id
  )


# Restrict to eligible randomized multi-arm trials -----

eligible_trials <- studies %>%
  left_join(designs, by = "nct_id") %>%
  mutate(
    number_of_arms = suppressWarnings(as.numeric(number_of_arms)),
    enrollment = suppressWarnings(as.numeric(enrollment)),
    completion_date = as.Date(completion_date)
  ) %>%
  filter(
    overall_status == "COMPLETED",
    study_type == "INTERVENTIONAL",
    allocation == "RANDOMIZED",
    !is.na(number_of_arms),
    number_of_arms > 2
  )

outcomes <- outcomes %>%
  semi_join(eligible_trials, by = "nct_id")

outcome_analyses <- outcome_analyses %>%
  semi_join(eligible_trials, by = "nct_id")

outcome_analysis_groups <- outcome_analysis_groups %>%
  semi_join(eligible_trials, by = "nct_id")

result_groups <- result_groups %>%
  filter(result_type == "Outcome") %>%
  semi_join(eligible_trials, by = "nct_id")


# Identify a single shared comparator per trial -----

pairwise_analyses <- outcome_analysis_groups %>%
  distinct(nct_id, outcome_analysis_id, result_group_id, ctgov_group_code) %>%
  count(nct_id, outcome_analysis_id, name = "n_groups") %>%
  filter(n_groups == 2)

pair_groups <- outcome_analysis_groups %>%
  semi_join(pairwise_analyses, by = c("nct_id", "outcome_analysis_id")) %>%
  distinct(nct_id, outcome_analysis_id, result_group_id, ctgov_group_code) %>%
  left_join(
    result_groups,
    by = c("nct_id", "result_group_id", "ctgov_group_code")
  )

analysis_pairs <- pair_groups %>%
  arrange(nct_id, outcome_analysis_id, ctgov_group_code, result_group_id) %>%
  group_by(nct_id, outcome_analysis_id) %>%
  summarise(
    group1_id = first(result_group_id),
    group1_code = first(ctgov_group_code),
    group1_title = first(group_title),
    group1_desc = first(group_description),
    group2_id = nth(result_group_id, 2),
    group2_code = nth(ctgov_group_code, 2),
    group2_title = nth(group_title, 2),
    group2_desc = nth(group_description, 2),
    .groups = "drop"
  )

group_labels <- pair_groups %>%
  group_by(nct_id, result_group_id) %>%
  summarise(
    group_title = first_nonmissing(group_title),
    group_description = first_nonmissing(group_description),
    .groups = "drop"
  )

edges <- bind_rows(
  analysis_pairs %>%
    transmute(nct_id, group_id = group1_id, partner_id = group2_id),
  analysis_pairs %>%
    transmute(nct_id, group_id = group2_id, partner_id = group1_id)
) %>%
  distinct() %>%
  left_join(
    group_labels,
    by = c("nct_id", "group_id" = "result_group_id")
  )

hubs <- edges %>%
  group_by(nct_id, group_id, group_title, group_description) %>%
  summarise(
    n_partners = n_distinct(partner_id),
    control_like = str_detect(
      coalesce(first_nonmissing(group_title), ""),
      control_regex
    ) |
      str_detect(
        coalesce(first_nonmissing(group_description), ""),
        control_regex
      ),
    .groups = "drop"
  ) %>%
  filter(n_partners >= 2)

hub_choice <- hubs %>%
  group_by(nct_id) %>%
  summarise(
    n_hubs = n(),
    n_control_hubs = sum(control_like),
    chosen_group_id = case_when(
      n_control_hubs == 1 ~ group_id[control_like][1],
      n_control_hubs == 0 & n_hubs == 1 ~ group_id[1],
      TRUE ~ NA_character_
    ),
    comparator_choice = case_when(
      n_control_hubs == 1 ~ "unique_control_hub",
      n_control_hubs == 0 & n_hubs == 1 ~ "unique_hub_no_control_label",
      TRUE ~ "ambiguous"
    ),
    .groups = "drop"
  )

selected_comparators <- hub_choice %>%
  filter(!is.na(chosen_group_id))

selected_pair_rows <- analysis_pairs %>%
  inner_join(selected_comparators, by = "nct_id") %>%
  mutate(
    comparator_id = chosen_group_id,
    active_id = case_when(
      group1_id == comparator_id ~ group2_id,
      group2_id == comparator_id ~ group1_id,
      TRUE ~ NA_character_
    ),
    comparator_group_code = case_when(
      group1_id == comparator_id ~ group1_code,
      group2_id == comparator_id ~ group2_code,
      TRUE ~ NA_character_
    ),
    active_group_code = case_when(
      group1_id == comparator_id ~ group2_code,
      group2_id == comparator_id ~ group1_code,
      TRUE ~ NA_character_
    ),
    comparator_title = case_when(
      group1_id == comparator_id ~ group1_title,
      group2_id == comparator_id ~ group2_title,
      TRUE ~ NA_character_
    ),
    comparator_desc = case_when(
      group1_id == comparator_id ~ group1_desc,
      group2_id == comparator_id ~ group2_desc,
      TRUE ~ NA_character_
    ),
    active_title = case_when(
      group1_id == comparator_id ~ group2_title,
      group2_id == comparator_id ~ group1_title,
      TRUE ~ NA_character_
    ),
    active_desc = case_when(
      group1_id == comparator_id ~ group2_desc,
      group2_id == comparator_id ~ group1_desc,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(active_id))

multiarm_trials <- selected_pair_rows %>%
  group_by(nct_id, comparator_id) %>%
  summarise(
    n_active_arms = n_distinct(active_id),
    n_pairwise_analyses = n(),
    .groups = "drop"
  ) %>%
  filter(n_active_arms >= 2)


# Build a BEAR-like long dataset -----

comparison_map <- selected_pair_rows %>%
  semi_join(multiarm_trials, by = c("nct_id", "comparator_id")) %>%
  distinct(
    nct_id,
    comparator_id,
    active_id,
    active_group_code,
    active_title,
    comparator_group_code,
    comparator_title
  ) %>%
  arrange(nct_id, active_group_code, active_title, active_id) %>%
  group_by(nct_id, comparator_id) %>%
  mutate(
    comparison_id = str_glue("CMP-{str_pad(row_number(), 3, pad = '0')}")
  ) %>%
  ungroup()

ctgov_multiarm <- selected_pair_rows %>%
  semi_join(multiarm_trials, by = c("nct_id", "comparator_id")) %>%
  left_join(comparison_map,
            by = c(
              "nct_id", "comparator_id", "active_id", "active_group_code",
              "active_title", "comparator_group_code", "comparator_title"
            )) %>%
  left_join(
    outcome_analyses,
    by = c("nct_id", "outcome_analysis_id")
  ) %>%
  left_join(outcomes, by = c("nct_id", "outcome_id")) %>%
  left_join(eligible_trials, by = "nct_id") %>%
  mutate(
    ci_n_sides = case_when(
      ci_n_sides == "ONE_SIDED" ~ 1,
      ci_n_sides == "TWO_SIDED" ~ 2,
      TRUE ~ NA_real_
    ),
    across(
      c(param_value, ci_percent, ci_lower_limit, ci_upper_limit, p_value),
      ~ suppressWarnings(as.numeric(.x))
    )
  ) %>%
  mutate(
    measure_class = classify_measure(param_type),
    log_ok = ci_lower_limit > 0 & param_value > 0 & ci_upper_limit > 0,
    scale = if_else(str_detect(as.character(measure_class), "Ratio") & log_ok,
                    "log", "raw")
  ) %>%
  mutate(
    effect_reported = safe_scale_transform(param_value, scale),
    lo_x = safe_scale_transform(ci_lower_limit, scale),
    hi_x = safe_scale_transform(ci_upper_limit, scale),
    z_crit = zcrit(ci_percent, ci_n_sides),
    se_hi = (hi_x - effect_reported) / z_crit,
    se_lo = (effect_reported - lo_x) / z_crit,
    se = case_when(
      is.finite(se_hi) & is.finite(se_lo) ~ (se_hi + se_lo) / 2,
      is.finite(se_hi) ~ se_hi,
      is.finite(se_lo) ~ se_lo,
      TRUE ~ NA_real_
    ),
    sym_ratio = pmin(hi_x - effect_reported, effect_reported - lo_x) /
      pmax(hi_x - effect_reported, effect_reported - lo_x),
    bad_type = str_detect(
      str_to_lower(param_type),
      "median|hodges|posterior|bayes|exact|fieller|bootstrap|permutation"
    ),
    z = if_else(
      is.finite(effect_reported) & is.finite(se) & se > 0 &
        !bad_type & (is.na(sym_ratio) | sym_ratio > 0.8),
      effect_reported / se,
      NA_real_
    )
  ) %>%
  filter(is.finite(effect_reported), is.finite(se), is.finite(z)) %>%
  mutate(
    context_text = paste(
      estimate_description,
      groups_description,
      sep = " | "
    )
  ) %>%
  rowwise() %>%
  mutate(
    orientation = list(
      infer_direction(
        text = context_text,
        active_title = active_title,
        active_desc = active_desc,
        comparator_title = comparator_title,
        comparator_desc = comparator_desc
      )
    )
  ) %>%
  ungroup() %>%
  unnest_wider(orientation) %>%
  mutate(
    b_active_vs_comparator = if_else(
      is.na(direction_active_vs_comparator),
      NA_real_,
      direction_active_vs_comparator * effect_reported
    ),
    z_active_vs_comparator = if_else(
      is.na(direction_active_vs_comparator),
      NA_real_,
      direction_active_vs_comparator * z
    ),
    z_se = 1,
    orientation_inferred = !is.na(direction_active_vs_comparator),
    comparison_label = str_glue("{active_title} vs {comparator_title}"),
    completion_year = lubridate::year(completion_date)
  ) %>%
  transmute(
    dataset = "clinicaltrialsgov_multiarm_shared_comparator",
    trial_id = nct_id,
    comparison_id,
    comparison_label,
    comparator_choice,
    active_group_id = active_id,
    active_group_code,
    active_group_title = active_title,
    comparator_group_id = comparator_id,
    comparator_group_code,
    comparator_group_title = comparator_title,
    outcome_id,
    outcome_analysis_id,
    outcome_type,
    outcome_title,
    outcome_time_frame,
    method,
    param_type,
    measure_class = as.character(measure_class),
    scale,
    b_reported = effect_reported,
    se,
    z_reported = z,
    z_se,
    orientation_inferred,
    orientation_source,
    direction_active_vs_comparator,
    b_active_vs_comparator,
    z_active_vs_comparator,
    number_of_arms,
    enrollment,
    phase,
    intervention_model,
    primary_purpose,
    masking,
    completion_year,
    brief_title,
    estimate_description,
    groups_description
  )


# Summaries and notes -----

trial_summary <- ctgov_multiarm %>%
  group_by(trial_id, comparison_id, active_group_title, comparator_group_title) %>%
  summarise(
    n_rows = n(),
    n_primary = sum(outcome_type == "PRIMARY", na.rm = TRUE),
    pct_oriented = mean(orientation_inferred),
    .groups = "drop"
  )

dataset_summary <- ctgov_multiarm %>%
  summarise(
    n_rows = n(),
    n_trials = n_distinct(trial_id),
    n_comparisons = n_distinct(paste(trial_id, comparison_id)),
    mean_comparisons_per_trial = n_comparisons / n_trials,
    pct_rows_oriented = mean(orientation_inferred),
    pct_trials_with_any_primary = mean(
      trial_id %in% unique(trial_id[outcome_type == "PRIMARY"])
    )
  )

cochrane_note <- c(
  "# Cochrane Check",
  "",
  paste(
    "Cochrane was not carried forward into this dataset.",
    "The raw table contains review-level comparison labels",
    "(`comparison.id`, `comparison.name`) but not arm identifiers."
  ),
  paste(
    "A single `study.name` often appears under many comparison IDs within",
    "the same review, so there is no clean way to guarantee that retained",
    "rows are all active-versus-the-same-comparator at the trial level."
  ),
  paste(
    "For this reason the shared-comparator dataset is built from",
    "clinicaltrials.gov only."
  )
)

notes <- c(
  "# Clinicaltrials Multiarm Shared Comparator Dataset",
  "",
  "Built by `build_ctgov_shared_comparator_v1.R`.",
  "",
  "## Inclusion",
  "",
  "- Completed, interventional, randomized clinicaltrials.gov studies.",
  "- Trials with `number_of_arms > 2`.",
  "- Outcome analyses linked to exactly two result groups.",
  "- Trials where one comparator group can be selected conservatively.",
  "- Only trials with at least two distinct active arms against that comparator.",
  "",
  "## Orientation",
  "",
  "- `b_reported` and `z_reported` keep the reported direction.",
  "- `b_active_vs_comparator` and `z_active_vs_comparator` are filled only",
  "- when text order in the analysis description lets us orient the contrast.",
  "",
  "## Summary",
  "",
  str_glue("- Rows: {dataset_summary$n_rows}"),
  str_glue("- Trials: {dataset_summary$n_trials}"),
  str_glue("- Comparisons: {dataset_summary$n_comparisons}"),
  str_glue(
    "- Mean comparisons per trial: ",
    "{round(dataset_summary$mean_comparisons_per_trial, 2)}"
  ),
  str_glue(
    "- Fraction of rows with inferred active-vs-comparator direction: ",
    "{round(dataset_summary$pct_rows_oriented, 3)}"
  )
)


# Write outputs -----

saveRDS(
  ctgov_multiarm,
  file.path(out_dir, "ctgov_multiarm_shared_comparator_v1.rds")
)

write_csv(
  ctgov_multiarm,
  file.path(out_dir, "ctgov_multiarm_shared_comparator_v1.csv.gz")
)

write_csv(
  trial_summary,
  file.path(out_dir, "ctgov_multiarm_shared_comparator_summary_v1.csv")
)

write_lines(
  notes,
  file.path(out_dir, "ctgov_multiarm_shared_comparator_notes_v1.md")
)

write_lines(
  cochrane_note,
  file.path(out_dir, "cochrane_shared_comparator_check_v1.md")
)

cat("Wrote:\n")
cat("- ", file.path(out_dir, "ctgov_multiarm_shared_comparator_v1.rds"), "\n",
    sep = "")
cat("- ", file.path(out_dir, "ctgov_multiarm_shared_comparator_v1.csv.gz"), "\n",
    sep = "")
cat("- ",
    file.path(out_dir, "ctgov_multiarm_shared_comparator_summary_v1.csv"),
    "\n", sep = "")
cat("- ",
    file.path(out_dir, "ctgov_multiarm_shared_comparator_notes_v1.md"),
    "\n", sep = "")
cat("- ",
    file.path(out_dir, "cochrane_shared_comparator_check_v1.md"),
    "\n", sep = "")
