# Process SCORE matched original/replication reports into a BEAR-ready dataset.
# The canonical output is `data/SCORE_replications.rds`; the paired audit table
# remains under `nosek_replicate_bear/output/`.

suppressPackageStartupMessages(library(tidyverse))
source("R/helpers.R")
source("R/score_helpers.R")

raw_path <- "data_raw/SCORE/replication/analyst data.RData"
audit_dir <- "nosek_replicate_bear/output"
table_dir <- file.path(audit_dir, "tables")
dir.create("data", recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

score_raw <- new.env(parent = emptyenv())
load(raw_path, envir = score_raw)

orig_outcomes <- as_tibble(score_raw$orig_outcomes)
repli_outcomes <- as_tibble(score_raw$repli_outcomes)
paper_metadata <- as_tibble(score_raw$paper_metadata)

paper_lookup <- paper_metadata %>%
  transmute(
    paper_id,
    doi = DOI,
    citation,
    journal = publication_standard,
    discipline = COS_pub_category,
    year = pub_year,
    paper_is_covid = is_covid
  )

main_replication_source <- repli_outcomes %>%
  filter(repli_version_of_record, !is_covid)

main_claim_ids <- main_replication_source %>%
  distinct(claim_id) %>%
  pull(claim_id)

original_long <- orig_outcomes %>%
  filter(claim_id %in% main_claim_ids) %>%
  left_join(paper_lookup, by = "paper_id") %>%
  mutate(
    source = "original",
    sample_size = dplyr::coalesce(
      as.numeric(original_effective_sample_size),
      as.numeric(orig_sample_size_value)
    ),
    sign_hint = dplyr::coalesce(
      sign_nonzero(orig_coef_value),
      sign_nonzero(orig_effect_size_value_repro),
      sign_nonzero(orig_effect_size_value_repli),
      sign_nonzero(orig_conv_r),
      sign_nonzero(orig_stat_value)
    ),
    coef_value = as.numeric(orig_coef_value),
    coef_se = as.numeric(orig_coef_se),
    stat_type = orig_stat_type,
    stat_value = as.numeric(orig_stat_value),
    stat_dof_1 = as.numeric(orig_stat_dof_1),
    p_value = as.numeric(orig_p_value),
    p_value_type = orig_p_value_type
  ) %>%
  derive_preferred_z(
    coef_value = "coef_value",
    coef_se = "coef_se",
    stat_type = "stat_type",
    stat_value = "stat_value",
    stat_dof_1 = "stat_dof_1",
    p_value = "p_value",
    p_value_type = "p_value_type",
    sign_hint = "sign_hint"
  ) %>%
  transmute(
    dataset = "SCORE",
    metaid = journal,
    studyid = doi,
    estimate_id = paste0(claim_id, "_original"),
    paper_id,
    doi,
    claim_id,
    report_id = NA_character_,
    citation,
    journal,
    discipline,
    year,
    source,
    subset = "main",
    measure = "r",
    z = truncate_score_z(z_preferred),
    abs_z = abs(z),
    z_operator,
    p = p_value,
    b = as.numeric(orig_conv_r),
    se = conv_r_se_from_bounds(
      orig_conv_r, orig_conv_r_lb, orig_conv_r_ub, z_preferred
    ),
    ss = sample_size,
    significant = derive_significance(p, z),
    replication_type = NA_character_,
    type_internal = NA_character_,
    z_source,
    z_from_coef,
    z_from_stat,
    z_from_p,
    original_effect_size_value_repro = orig_effect_size_value_repro,
    original_effect_size_value_repli = orig_effect_size_value_repli,
    original_stat_type = orig_stat_type,
    original_p_value_type = orig_p_value_type
  )

replication_long <- main_replication_source %>%
  left_join(paper_lookup, by = "paper_id") %>%
  mutate(
    source = "replication",
    sample_size = dplyr::coalesce(
      as.numeric(repli_effective_sample_size),
      as.numeric(repli_sample_size_value)
    ),
    sign_hint = dplyr::coalesce(
      sign_nonzero(repli_coef_value),
      sign_nonzero(repli_effect_size_value),
      sign_nonzero(repli_effect_size_value_raw),
      sign_nonzero(repli_conv_r),
      sign_nonzero(repli_stat_value)
    ),
    coef_value = as.numeric(repli_coef_value),
    coef_se = as.numeric(repli_coef_se),
    stat_type = repli_stat_type,
    stat_value = as.numeric(repli_stat_value),
    stat_dof_1 = as.numeric(repli_stat_dof_1),
    p_value = as.numeric(repli_p_value),
    p_value_type = ifelse(!is.na(repli_p_value), "exact", NA_character_)
  ) %>%
  derive_preferred_z(
    coef_value = "coef_value",
    coef_se = "coef_se",
    stat_type = "stat_type",
    stat_value = "stat_value",
    stat_dof_1 = "stat_dof_1",
    p_value = "p_value",
    p_value_type = "p_value_type",
    sign_hint = "sign_hint"
  ) %>%
  transmute(
    dataset = "SCORE",
    metaid = journal,
    studyid = doi,
    estimate_id = paste0(claim_id, "_replication"),
    paper_id,
    doi,
    claim_id,
    report_id,
    citation,
    journal,
    discipline,
    year,
    source,
    subset = "main",
    measure = "r",
    z = truncate_score_z(z_preferred),
    abs_z = abs(z),
    z_operator,
    p = p_value,
    b = as.numeric(repli_conv_r),
    se = conv_r_se_from_bounds(
      repli_conv_r, repli_conv_r_lb, repli_conv_r_ub, z_preferred
    ),
    ss = sample_size,
    significant = derive_significance(p, z),
    replication_type = as.character(repli_type),
    type_internal,
    z_source,
    z_from_coef,
    z_from_stat,
    z_from_p,
    repli_effect_size_value,
    repli_effect_size_value_raw,
    repli_stat_type,
    repli_effect_direction,
    repli_pattern_criteria_met,
    repli_score_criteria_met
  )

score_replications <- bind_rows(original_long, replication_long) %>%
  calc_study_weights() %>%
  arrange(paper_id, claim_id, source) %>%
  select(
    dataset, metaid, studyid, estimate_id, paper_id, doi, claim_id, report_id,
    citation, journal, discipline, year, source, subset, measure, z, abs_z,
    z_operator, p, b, se, ss, weights, significant,
    replication_type, type_internal, z_source, z_from_coef, z_from_stat,
    z_from_p, everything()
  )

score_replication_pairs_main <- main_replication_source %>%
  left_join(paper_lookup, by = "paper_id") %>%
  left_join(
    original_long %>%
      transmute(
        claim_id,
        orig_z = z,
        orig_abs_z = abs_z,
        orig_b = b,
        orig_se = se,
        orig_p = p,
        orig_ss = ss
      ),
    by = "claim_id"
  ) %>%
  left_join(
    replication_long %>%
      transmute(
        report_id,
        repli_z = z,
        repli_abs_z = abs_z,
        repli_b = b,
        repli_se = se,
        repli_p = p,
        repli_ss = ss
      ),
    by = "report_id"
  ) %>%
  transmute(
    paper_id,
    doi,
    claim_id,
    report_id,
    citation,
    journal,
    discipline,
    year,
    subset = "main",
    type_internal,
    replication_type = as.character(repli_type),
    orig_z,
    orig_abs_z,
    orig_b,
    orig_se,
    orig_p,
    orig_ss,
    repli_z,
    repli_abs_z,
    repli_b,
    repli_se,
    repli_p,
    repli_ss,
    repli_effect_direction,
    repli_pattern_criteria_met,
    repli_score_criteria_met
  )

main_replication_sig_rate <- mean(
  main_replication_source$repli_p_value <= 0.05,
  na.rm = TRUE
)

validation_checks <- tibble(
  check = c(
    "raw_original_rows",
    "raw_replication_rows",
    "main_replication_rows",
    "matched_original_rows",
    "paired_audit_rows",
    "canonical_long_rows",
    "score_success_rate",
    "main_replication_p_significance_rate",
    "discipline_complete",
    "weights_sum_to_distinct_papers"
  ),
  actual = c(
    nrow(orig_outcomes),
    nrow(repli_outcomes),
    nrow(main_replication_source),
    nrow(original_long),
    nrow(score_replication_pairs_main),
    nrow(score_replications),
    mean(score_replication_pairs_main$repli_score_criteria_met, na.rm = TRUE),
    mean(
      score_replications$significant[
        score_replications$source == "replication"
      ],
      na.rm = TRUE
    ),
    as.numeric(!any(is.na(score_replications$discipline))),
    sum(score_replications$weights)
  ),
  expected = c(
    825,
    427,
    274,
    274,
    274,
    548,
    151 / 274,
    main_replication_sig_rate,
    1,
    n_distinct(score_replications$paper_id)
  ),
  tolerance = c(0, 0, 0, 0, 0, 0, 1e-8, 1e-8, 0, 1e-8)
) %>%
  mutate(passed = abs(actual - expected) <= tolerance)

print(validation_checks, n = nrow(validation_checks), width = Inf)
stopifnot(all(validation_checks$passed))
stopifnot(!anyDuplicated(score_replications$estimate_id))

saveRDS(score_replications, "data/SCORE_replications.rds")
saveRDS(
  score_replication_pairs_main,
  file.path(audit_dir, "score_replication_pairs_main.rds")
)
write_csv_if_present(
  validation_checks,
  file.path(audit_dir, "score_replications_validation.csv")
)

capture.output(
  {
    cat("SCORE replications validation\n\n")
    print(validation_checks, n = nrow(validation_checks), width = Inf)
    cat("\nMain replication discipline counts\n\n")
    print(count(score_replication_pairs_main, discipline), n = Inf)
  },
  file = file.path(table_dir, "score_replications_validation.txt")
)

cat("\nSaved SCORE matched replication outputs.\n")
