# Build trial-level ClinicalTrials.gov characteristics for later joins.
# Outputs are side artifacts keyed by nct_id and do not modify BEAR data.

library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(tibble)

source("process/clinicaltrials.gov/lib/paths.R")

snapshot_zip <- ctgov_snapshot_zip
input_dir <- ctgov_intermediate_dir
out_dir <- ctgov_derived_dir
validation_dir <- ctgov_validation_dir
manual_review_dir <- ctgov_manual_review_dir
domain_map_path <- ctgov_domain_map_path

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

norm_chr <- function(x) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  x %>%
    str_replace_all("[\\u2010-\\u2015]", "-") %>%
    str_replace_all("\\u00a0", " ") %>%
    str_squish() %>%
    str_to_lower()
}

snake <- function(x) {
  norm_chr(x) %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

as_num <- function(x) suppressWarnings(as.numeric(x))

as_date_safe <- function(x) suppressWarnings(ymd(x))

stream_collect <- function(file, col_select, keep_fun, chunk_size = 200000) {
  pieces <- list()
  i <- 0L
  callback <- DataFrameCallback$new(function(x, pos) {
    kept <- x %>%
      select(any_of(col_select)) %>%
      keep_fun()
    if (nrow(kept) > 0) {
      i <<- i + 1L
      pieces[[i]] <<- kept
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

  bind_rows(pieces)
}

summarise_set <- function(x) {
  x <- sort(unique(x[!is.na(x) & x != ""]))
  if (length(x) == 0) NA_character_ else str_c(x, collapse = ";")
}

has_text <- function(x, pattern) {
  !is.na(x) & str_detect(x, regex(pattern, ignore_case = TRUE))
}

sample_rows <- function(df, n = 250) {
  if (nrow(df) <= n) df else slice_sample(df, n = n)
}

match_domain_rules <- function(df, term_col, source_col, map) {
  out <- vector("list", nrow(map))
  term <- df[[term_col]]
  for (i in seq_len(nrow(map))) {
    pattern <- gsub("\\\\\\\\", "\\\\", map$mesh_term_pattern[i])
    hit <- !is.na(term) & str_detect(term, regex(pattern))
    if (any(hit)) {
      out[[i]] <- df[hit, , drop = FALSE] %>%
        transmute(
          nct_id,
          domain = map$domain[i],
          domain_source = .data[[source_col]],
          source_table,
          source_term,
          mesh_type = if ("mesh_type" %in% names(.)) mesh_type else NA_character_,
          matched_rule_id = map$rule_id[i],
          priority = map$priority[i]
        )
    }
  }
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) {
    return(tibble(
      nct_id = character(), domain = character(), domain_source = character(),
      source_table = character(), source_term = character(),
      mesh_type = character(), matched_rule_id = character(), priority = integer()
    ))
  }

  bind_rows(out) %>%
    distinct(nct_id, domain, domain_source, source_table, source_term,
             mesh_type, matched_rule_id, priority)
}

make_dictionary <- function(trial_characteristics, domains_long, interventions_long) {
  dict_base <- bind_rows(
    tibble(
      variable = names(trial_characteristics),
      output_table = "trial_characteristics.rds",
      type = vapply(trial_characteristics, function(x) class(x)[1], character(1))
    ),
    tibble(
      variable = names(domains_long),
      output_table = "trial_domains_long.rds",
      type = vapply(domains_long, function(x) class(x)[1], character(1))
    ),
    tibble(
      variable = names(interventions_long),
      output_table = "trial_interventions_long.rds",
      type = vapply(interventions_long, function(x) class(x)[1], character(1))
    )
  ) %>%
    distinct(variable, output_table, .keep_all = TRUE)

  dict_base %>%
    mutate(
      allowed_values = case_when(
        str_detect(variable, "domain|intervention_type|sponsor_class|summary") ~
          "see observed values and rule notes",
        str_detect(variable, "^is_|^has_|^in_") ~ "TRUE;FALSE;NA",
        TRUE ~ NA_character_
      ),
      source_table = case_when(
        variable %in% c("brief_title", "official_title", "study_type", "phase",
                        "overall_status", "enrollment", "enrollment_type",
                        "number_of_arms", "number_of_groups",
                        "results_first_posted_date", "primary_completion_date",
                        "completion_date", "results_first_posted_year",
                        "primary_completion_year", "has_posted_results") ~ "studies.txt",
        variable %in% c("allocation", "intervention_model", "primary_purpose",
                        "masking", "is_factorial", "is_crossover") ~ "designs.txt",
        str_detect(variable, "comparator") | variable == "is_multi_arm" ~
          "design_groups.txt;studies.txt",
        str_detect(variable, "domain") ~
          "browse_conditions.txt;conditions.txt;keywords.txt",
        str_detect(variable, "intervention|vaccine|cell_or_gene|diagnostic") ~
          "interventions.txt;browse_interventions.txt",
        str_detect(variable, "sponsor") ~ "sponsors.txt",
        str_detect(variable, "baseline") ~ "baseline_measurements.txt",
        str_detect(variable, "coverage|old_bear|author|raw_endpoint|prepost") ~
          "workflow artifacts",
        TRUE ~ "derived"
      ),
      source_column = "see script rule",
      cleaning_rule = case_when(
        str_detect(variable, "domain") ~ "derive_domain_from_mesh_and_text_v1",
        str_detect(variable, "intervention|vaccine|cell_or_gene|diagnostic") ~
          "derive_intervention_flags_v1",
        str_detect(variable, "comparator") ~
          "derive_comparator_flags_from_design_groups_v1",
        str_detect(variable, "baseline") ~ "derive_has_baseline_measurements_v1",
        TRUE ~ "normalize_trial_characteristics_v1"
      ),
      missing_rule = "NA for missing source values; FALSE for evaluated absence flags",
      description = variable,
      notes = "Generated by 12_build_trial_characteristics.R"
    )
}

# Trial universe -----

coverage <- read_csv(
  file.path(validation_dir, "trial_coverage_audit.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  mutate(across(c(in_primary_denominator, in_old_bear, has_author_analysis,
                  has_raw_endpoint, has_prepost_rows), ~ .x == "TRUE"))

old_bear <- readRDS(file.path(out_dir, "author_reported_candidates.rds"))
raw_endpoint <- readRDS(file.path(out_dir, "raw_endpoint_effects.rds"))
author_links <- readRDS(file.path(input_dir, "author_analysis_links.rds"))
prepost <- readRDS(file.path(out_dir, "prepost_measurement_pairs.rds"))

target_nct_ids <- bind_rows(
  coverage %>% distinct(nct_id),
  old_bear %>% distinct(nct_id),
  raw_endpoint %>% distinct(nct_id),
  author_links %>% distinct(nct_id),
  prepost %>% distinct(nct_id)
) %>%
  filter(!is.na(nct_id)) %>%
  distinct(nct_id) %>%
  pull(nct_id)

# Basic trial fields -----

studies <- read_aact(
  "studies.txt",
  c(
    "nct_id", "brief_title", "official_title", "study_type", "phase",
    "overall_status", "enrollment", "enrollment_type", "number_of_arms",
    "number_of_groups", "results_first_posted_date",
    "primary_completion_date", "completion_date"
  )
) %>%
  filter(nct_id %in% target_nct_ids) %>%
  transmute(
    nct_id, brief_title, official_title,
    study_type = snake(study_type),
    phase = snake(phase),
    overall_status = snake(overall_status),
    enrollment = as_num(enrollment),
    enrollment_type = case_when(
      snake(enrollment_type) %in% c("actual", "anticipated") ~ snake(enrollment_type),
      is.na(enrollment_type) ~ NA_character_,
      TRUE ~ "unknown"
    ),
    number_of_arms = as_num(number_of_arms),
    number_of_groups = as_num(number_of_groups),
    results_first_posted_date = as_date_safe(results_first_posted_date),
    primary_completion_date = as_date_safe(primary_completion_date),
    completion_date = as_date_safe(completion_date),
    results_first_posted_year = year(results_first_posted_date),
    primary_completion_year = year(primary_completion_date),
    has_posted_results = !is.na(results_first_posted_date)
  ) %>%
  distinct(nct_id, .keep_all = TRUE)

designs <- read_aact(
  "designs.txt",
  c("nct_id", "allocation", "intervention_model", "primary_purpose", "masking")
) %>%
  filter(nct_id %in% target_nct_ids) %>%
  transmute(
    nct_id,
    allocation = snake(allocation),
    intervention_model = snake(intervention_model),
    primary_purpose = snake(primary_purpose),
    masking = norm_chr(masking),
    is_factorial = has_text(intervention_model, "factorial"),
    is_crossover = has_text(intervention_model, "crossover|cross-over")
  ) %>%
  distinct(nct_id, .keep_all = TRUE)

# Comparator and design-group flags -----

design_groups <- stream_collect(
  "design_groups.txt",
  c("id", "nct_id", "group_type", "title", "description"),
  function(x) x %>% filter(nct_id %in% target_nct_ids)
) %>%
  mutate(
    group_text = norm_chr(str_c(group_type, title, description, sep = " ")),
    from_text = is.na(group_type) | snake(group_type) %in% c("", "other"),
    placebo = has_text(group_text, "\\bplacebo\\b|\\bvehicle\\b"),
    sham = has_text(group_text, "\\bsham\\b"),
    no_intervention = has_text(
      group_text,
      "no intervention|usual care|standard care|standard of care|waitlist|\\bcontrol\\b"
    ),
    active = has_text(group_text, "active comparator|active control|\\bcomparator\\b")
  )

design_group_counts <- design_groups %>%
  count(nct_id, name = "n_design_groups")

comparator_flags <- design_groups %>%
  group_by(nct_id) %>%
  summarise(
    has_placebo_comparator = any(placebo, na.rm = TRUE),
    has_sham_comparator = any(sham, na.rm = TRUE),
    has_no_intervention_comparator = any(no_intervention, na.rm = TRUE),
    has_active_comparator = any(active, na.rm = TRUE),
    comparator_text_only = any(from_text & (placebo | sham | no_intervention | active), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    comparator_summary = summarise_set(c(
      if (has_placebo_comparator) "placebo",
      if (has_sham_comparator) "sham",
      if (has_no_intervention_comparator) "no_intervention_or_usual_care",
      if (has_active_comparator) "active"
    )),
    comparator_summary = coalesce(comparator_summary, "none_detected")
  ) %>%
  ungroup()

# Domain classification -----

domain_map <- read_csv(domain_map_path, show_col_types = FALSE) %>%
  mutate(priority = as.integer(priority))

browse_conditions <- stream_collect(
  "browse_conditions.txt",
  c("nct_id", "mesh_term", "downcase_mesh_term", "mesh_type"),
  function(x) x %>% filter(nct_id %in% target_nct_ids)
) %>%
  transmute(
    nct_id,
    source_table = "browse_conditions.txt",
    source_term = coalesce(downcase_mesh_term, mesh_term),
    source_term_norm = norm_chr(source_term),
    mesh_type,
    source_kind = "mesh"
  )

condition_text <- stream_collect(
  "conditions.txt",
  c("nct_id", "name", "downcase_name"),
  function(x) x %>% filter(nct_id %in% target_nct_ids)
) %>%
  transmute(
    nct_id,
    source_table = "conditions.txt",
    source_term = coalesce(downcase_name, name),
    source_term_norm = norm_chr(source_term),
    mesh_type = NA_character_,
    source_kind = "text"
  )

keyword_text <- stream_collect(
  "keywords.txt",
  c("nct_id", "name", "downcase_name"),
  function(x) x %>% filter(nct_id %in% target_nct_ids)
) %>%
  transmute(
    nct_id,
    source_table = "keywords.txt",
    source_term = coalesce(downcase_name, name),
    source_term_norm = norm_chr(source_term),
    mesh_type = NA_character_,
    source_kind = "text"
  )

mesh_domains <- match_domain_rules(
  browse_conditions, "source_term_norm", "source_kind", domain_map
) %>%
  mutate(domain_source = "mesh")

mesh_resolved_ids <- unique(mesh_domains$nct_id)
text_domains <- match_domain_rules(
  bind_rows(condition_text, keyword_text) %>%
    filter(!nct_id %in% mesh_resolved_ids),
  "source_term_norm", "source_kind", domain_map
) %>%
  mutate(domain_source = "text_fallback_only")

trial_domains_long <- bind_rows(mesh_domains, text_domains) %>%
  arrange(nct_id, priority, domain, source_table, source_term) %>%
  distinct(nct_id, domain, domain_source, source_table, source_term,
           mesh_type, matched_rule_id, priority)

domain_summary <- trial_domains_long %>%
  group_by(nct_id) %>%
  summarise(
    domain_all = summarise_set(domain),
    domain_n = n_distinct(domain),
    domain_source = case_when(
      all(domain_source == "mesh") ~ "mesh",
      any(domain_source == "mesh") ~ "mesh_plus_text_fallback",
      any(domain_source == "text_fallback_only") ~ "text_fallback_only",
      TRUE ~ "missing"
    ),
    domain_oncology = any(domain == "oncology"),
    domain_primary = case_when(
      n_distinct(domain) == 1 ~ first(domain),
      any(domain == "oncology") ~ "oncology",
      TRUE ~ domain[which.min(priority)]
    ),
    .groups = "drop"
  )

# Intervention classes -----

interventions <- stream_collect(
  "interventions.txt",
  c("nct_id", "intervention_type", "name", "description"),
  function(x) x %>% filter(nct_id %in% target_nct_ids)
) %>%
  transmute(
    nct_id,
    intervention_type_raw = intervention_type,
    intervention_type_norm = snake(intervention_type),
    intervention_name = name,
    intervention_description = description,
    intervention_source = "registered_intervention",
    source_table = "interventions.txt",
    source_term = norm_chr(str_c(intervention_type, name, description, sep = " "))
  )

browse_interventions <- stream_collect(
  "browse_interventions.txt",
  c("nct_id", "mesh_term", "downcase_mesh_term", "mesh_type"),
  function(x) x %>% filter(nct_id %in% target_nct_ids)
) %>%
  transmute(
    nct_id,
    intervention_type_raw = NA_character_,
    intervention_type_norm = NA_character_,
    intervention_name = coalesce(downcase_mesh_term, mesh_term),
    intervention_description = NA_character_,
    intervention_source = "browse_intervention_mesh",
    source_table = "browse_interventions.txt",
    source_term = norm_chr(intervention_name)
  )

intervention_long_base <- bind_rows(interventions, browse_interventions) %>%
  mutate(
    is_drug_trial = intervention_type_norm %in% c("drug") |
      has_text(source_term, "\\b(drug|tablet|capsule|dose|pharmaceutical)\\b"),
    is_biological_trial = intervention_type_norm %in% c("biological", "biologic") |
      has_text(source_term, "\\b(biologic|biological|antibody|protein)\\b"),
    is_device_trial = intervention_type_norm == "device",
    is_procedure_trial = intervention_type_norm %in% c("procedure", "surgery"),
    is_behavioral_trial = intervention_type_norm == "behavioral",
    is_vaccine_trial = has_text(source_term, "\\bvaccin"),
    is_cell_or_gene_therapy_trial = has_text(
      source_term,
      "cell therapy|gene therapy|car-t|cart|stem cell|viral vector"
    ),
    is_diagnostic_trial = intervention_type_norm == "diagnostic_test" |
      has_text(source_term, "diagnostic|screening|imaging|test\\b")
  )

priority_intervention <- tibble(
  intervention_type_norm = c(
    "drug", "biological", "device", "procedure", "radiation", "behavioral",
    "diagnostic_test", "dietary_supplement", "genetic", "other", "unknown"
  ),
  intervention_priority = seq_along(intervention_type_norm)
)

trial_interventions_long <- intervention_long_base %>%
  mutate(
    intervention_type_norm = coalesce(intervention_type_norm, "unknown"),
    matched_rule_id = case_when(
      is_vaccine_trial ~ "intervention_vaccine_text_v1",
      is_cell_or_gene_therapy_trial ~ "intervention_cell_gene_text_v1",
      is_diagnostic_trial ~ "intervention_diagnostic_text_v1",
      TRUE ~ "intervention_type_structured_v1"
    )
  ) %>%
  select(
    nct_id, intervention_type_norm, intervention_name, intervention_source,
    source_table, is_drug_trial, is_biological_trial, is_device_trial,
    is_procedure_trial, is_behavioral_trial, is_vaccine_trial,
    is_cell_or_gene_therapy_trial, is_diagnostic_trial, matched_rule_id
  ) %>%
  distinct() %>%
  arrange(nct_id, intervention_type_norm, intervention_name)

intervention_summary <- trial_interventions_long %>%
  left_join(priority_intervention, by = "intervention_type_norm") %>%
  mutate(intervention_priority = coalesce(intervention_priority, 99L)) %>%
  group_by(nct_id) %>%
  summarise(
    intervention_types_all = summarise_set(intervention_type_norm),
    intervention_type_n = n_distinct(intervention_type_norm),
    intervention_type_primary =
      intervention_type_norm[which.min(intervention_priority)],
    is_drug_trial = any(is_drug_trial, na.rm = TRUE),
    is_biological_trial = any(is_biological_trial, na.rm = TRUE),
    is_device_trial = any(is_device_trial, na.rm = TRUE),
    is_procedure_trial = any(is_procedure_trial, na.rm = TRUE),
    is_behavioral_trial = any(is_behavioral_trial, na.rm = TRUE),
    is_vaccine_trial = any(is_vaccine_trial, na.rm = TRUE),
    is_cell_or_gene_therapy_trial =
      any(is_cell_or_gene_therapy_trial, na.rm = TRUE),
    is_diagnostic_trial = any(is_diagnostic_trial, na.rm = TRUE),
    .groups = "drop"
  )

# Sponsors and baseline availability -----

sponsors <- read_aact(
  "sponsors.txt",
  c("nct_id", "agency_class", "lead_or_collaborator", "name")
) %>%
  filter(nct_id %in% target_nct_ids) %>%
  mutate(
    agency_class_norm = case_when(
      snake(agency_class) %in% c("industry") ~ "industry",
      snake(agency_class) %in% c("nih") ~ "nih",
      snake(agency_class) %in% c("u_s_fed", "us_fed") ~ "us_fed",
      snake(agency_class) %in% c("other_gov", "fed") ~ "other_gov",
      snake(agency_class) %in% c("network") ~ "network",
      snake(agency_class) %in% c("individual") ~ "individual",
      is.na(agency_class) ~ "unknown",
      TRUE ~ "other"
    ),
    lead_flag = snake(lead_or_collaborator) == "lead"
  )

sponsor_summary <- sponsors %>%
  group_by(nct_id) %>%
  summarise(
    lead_sponsor_name = name[which.max(lead_flag)],
    lead_sponsor_class = agency_class_norm[which.max(lead_flag)],
    has_industry_sponsor = any(agency_class_norm == "industry", na.rm = TRUE),
    .groups = "drop"
  )

baseline_counts <- stream_collect(
  "baseline_measurements.txt",
  c("nct_id", "id"),
  function(x) {
    x %>%
      filter(nct_id %in% target_nct_ids) %>%
      count(nct_id, name = "n_baseline_measurement_rows")
  },
  chunk_size = 200000
) %>%
  group_by(nct_id) %>%
  summarise(n_baseline_measurement_rows = sum(n_baseline_measurement_rows),
            .groups = "drop")

# Assemble and save outputs -----

workflow_flags <- tibble(nct_id = target_nct_ids) %>%
  left_join(
    coverage %>%
      select(nct_id, in_primary_denominator, in_old_bear, has_author_analysis,
             has_raw_endpoint, has_prepost_rows),
    by = "nct_id"
  ) %>%
  mutate(
    in_trial_coverage_audit = nct_id %in% coverage$nct_id,
    in_old_bear = coalesce(in_old_bear, nct_id %in% old_bear$nct_id),
    has_author_analysis =
      coalesce(has_author_analysis, nct_id %in% author_links$nct_id),
    has_raw_endpoint =
      coalesce(has_raw_endpoint, nct_id %in% raw_endpoint$nct_id),
    has_prepost_rows =
      coalesce(has_prepost_rows, nct_id %in% prepost$nct_id),
    in_primary_denominator = coalesce(in_primary_denominator, FALSE)
  )

trial_characteristics <- tibble(nct_id = target_nct_ids) %>%
  left_join(studies, by = "nct_id") %>%
  left_join(designs, by = "nct_id") %>%
  left_join(design_group_counts, by = "nct_id") %>%
  left_join(comparator_flags, by = "nct_id") %>%
  left_join(domain_summary, by = "nct_id") %>%
  left_join(intervention_summary, by = "nct_id") %>%
  left_join(sponsor_summary, by = "nct_id") %>%
  left_join(baseline_counts, by = "nct_id") %>%
  left_join(workflow_flags, by = "nct_id") %>%
  mutate(
    n_design_groups = coalesce(n_design_groups, 0L),
    is_multi_arm = coalesce(number_of_arms > 2, FALSE) |
      coalesce(number_of_groups > 2, FALSE) | n_design_groups > 2,
    across(
      c(has_placebo_comparator, has_sham_comparator,
        has_no_intervention_comparator, has_active_comparator,
        comparator_text_only, is_drug_trial, is_biological_trial,
        is_device_trial, is_procedure_trial, is_behavioral_trial,
        is_vaccine_trial, is_cell_or_gene_therapy_trial, is_diagnostic_trial,
        has_industry_sponsor),
      ~ coalesce(.x, FALSE)
    ),
    comparator_summary = coalesce(comparator_summary, "none_detected"),
    domain_primary = coalesce(domain_primary, "unknown"),
    domain_all = coalesce(domain_all, "unknown"),
    domain_n = coalesce(domain_n, 0L),
    domain_source = coalesce(domain_source, "missing"),
    domain_oncology = coalesce(domain_oncology, FALSE),
    intervention_type_primary = coalesce(intervention_type_primary, "unknown"),
    intervention_types_all = coalesce(intervention_types_all, "unknown"),
    intervention_type_n = coalesce(intervention_type_n, 0L),
    lead_sponsor_class = coalesce(lead_sponsor_class, "unknown"),
    n_baseline_measurement_rows = coalesce(n_baseline_measurement_rows, 0L),
    has_baseline_measurements = n_baseline_measurement_rows > 0
  ) %>%
  arrange(nct_id)

qc_summary <- bind_rows(
  tibble(section = "row_count", category = "rows", n = nrow(trial_characteristics),
         pct = NA_real_),
  tibble(section = "row_count", category = "distinct_nct_id",
         n = n_distinct(trial_characteristics$nct_id), pct = NA_real_),
  tibble(section = "row_count", category = "duplicate_nct_id",
         n = sum(duplicated(trial_characteristics$nct_id)), pct = NA_real_),
  trial_characteristics %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>%
    tidyr::pivot_longer(everything(), names_to = "category", values_to = "n") %>%
    mutate(section = "missingness", pct = n / nrow(trial_characteristics)),
  trial_characteristics %>%
    count(section = "domain_primary", category = domain_primary, name = "n") %>%
    mutate(pct = n / nrow(trial_characteristics)),
  trial_characteristics %>%
    count(section = "intervention_type_primary",
          category = intervention_type_primary, name = "n") %>%
    mutate(pct = n / nrow(trial_characteristics)),
  trial_characteristics %>%
    count(section = "lead_sponsor_class", category = lead_sponsor_class, name = "n") %>%
    mutate(pct = n / nrow(trial_characteristics)),
  trial_characteristics %>%
    count(section = "primary_purpose", category = primary_purpose, name = "n") %>%
    mutate(pct = n / nrow(trial_characteristics)),
  trial_characteristics %>%
    count(section = "intervention_model", category = intervention_model, name = "n") %>%
    mutate(pct = n / nrow(trial_characteristics)),
  tibble(
    section = "flag_counts",
    category = c(
      "has_placebo_comparator", "has_sham_comparator",
      "has_no_intervention_comparator", "has_active_comparator",
      "has_baseline_measurements", "domain_unknown",
      "intervention_type_unknown", "multiple_domains",
      "multiple_intervention_types"
    ),
    n = c(
      sum(trial_characteristics$has_placebo_comparator),
      sum(trial_characteristics$has_sham_comparator),
      sum(trial_characteristics$has_no_intervention_comparator),
      sum(trial_characteristics$has_active_comparator),
      sum(trial_characteristics$has_baseline_measurements),
      sum(trial_characteristics$domain_primary == "unknown"),
      sum(trial_characteristics$intervention_type_primary == "unknown"),
      sum(trial_characteristics$domain_n > 1),
      sum(trial_characteristics$intervention_type_n > 1)
    ),
    pct = n / nrow(trial_characteristics)
  )
) %>%
  arrange(section, category)

dictionary <- make_dictionary(
  trial_characteristics, trial_domains_long, trial_interventions_long
)

stopifnot(
  nrow(trial_characteristics) == length(unique(target_nct_ids)),
  n_distinct(trial_characteristics$nct_id) == nrow(trial_characteristics),
  all(old_bear$nct_id %in% trial_characteristics$nct_id),
  all(raw_endpoint$nct_id %in% trial_characteristics$nct_id)
)

old_join_n <- old_bear %>%
  left_join(trial_characteristics, by = "nct_id") %>%
  nrow()
raw_join_n <- raw_endpoint %>%
  left_join(trial_characteristics, by = "nct_id") %>%
  nrow()
stopifnot(old_join_n == nrow(old_bear), raw_join_n == nrow(raw_endpoint))

saveRDS(trial_characteristics, file.path(out_dir, "trial_characteristics.rds"))
saveRDS(trial_domains_long, file.path(out_dir, "trial_domains_long.rds"))
saveRDS(trial_interventions_long, file.path(out_dir, "trial_interventions_long.rds"))
write_csv(dictionary, file.path(out_dir, "trial_characteristics_dictionary.csv"))
write_csv(qc_summary, file.path(validation_dir, "trial_characteristics_qc_summary.csv"))

write_csv(
  trial_characteristics %>%
    filter(domain_primary == "unknown") %>%
    select(nct_id, brief_title, official_title, phase, primary_purpose) %>%
    sample_rows(),
  file.path(manual_review_dir, "domain_unknown.csv")
)
write_csv(
  trial_characteristics %>%
    filter(domain_n > 1) %>%
    select(nct_id, brief_title, domain_primary, domain_all, domain_source) %>%
    sample_rows(),
  file.path(manual_review_dir, "domain_multilabel.csv")
)
write_csv(
  trial_characteristics %>%
    filter(intervention_type_n > 1) %>%
    select(nct_id, brief_title, intervention_type_primary, intervention_types_all) %>%
    sample_rows(),
  file.path(manual_review_dir, "intervention_multitype.csv")
)
write_csv(
  trial_characteristics %>%
    filter(comparator_text_only) %>%
    select(nct_id, brief_title, comparator_summary,
           has_placebo_comparator, has_sham_comparator,
           has_no_intervention_comparator, has_active_comparator) %>%
    sample_rows(),
  file.path(manual_review_dir, "comparator_flags.csv")
)

message("Saved ", nrow(trial_characteristics), " trial-characteristic rows.")
