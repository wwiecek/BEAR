# Process all SCORE claim-text statistics into a BEAR-ready claim-level dataset.
# The canonical output is `data/SCORE_all_claims.rds`; audit outputs remain
# under `nosek_replicate_bear/output/`.

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

claims_non_significant_bushels <- as_tibble(
  score_raw$claims_non_significant_bushels
)
paper_metadata <- as_tibble(score_raw$paper_metadata)

paper_lookup <- paper_metadata %>%
  transmute(
    paper_id,
    citation,
    journal = publication_standard,
    discipline = COS_pub_category,
    year = pub_year
  )

claim_fragments <- claims_non_significant_bushels %>%
  mutate(
    coded_stat_evidence = as.character(coded_stat_evidence),
    fragments = stringr::str_split(coded_stat_evidence, ";")
  ) %>%
  tidyr::unnest_longer(fragments, values_to = "stat_fragment") %>%
  mutate(stat_fragment = stringr::str_squish(stat_fragment)) %>%
  filter(!is.na(stat_fragment), stat_fragment != "") %>%
  group_by(claim4_id) %>%
  mutate(fragment_id = row_number()) %>%
  ungroup()

parsed_fragments <- purrr::map_dfr(
  claim_fragments$stat_fragment,
  parse_score_fragment
)

score_all_claims_fragments <- bind_cols(claim_fragments, parsed_fragments) %>%
  left_join(paper_lookup, by = "paper_id") %>%
  mutate(
    dataset = "SCORE",
    metaid = journal,
    studyid = paper_id,
    estimate_id = paste0(claim4_id, "_frag", fragment_id),
    claim_id = claim4_id,
    report_id = NA_character_,
    source = "claim_text",
    subset = "all_claims",
    ss = NA_real_,
    selected_significant = nonsig != "T",
    fragment_significant =
      derive_fragment_significance(p, p_operator, z, z_operator),
    significance_rule = describe_significance_rule(p, p_operator, z, z_operator),
    source_rank = match(
      z_source,
      c(
        "reported_z", "reported_t", "coef_over_se", "sqrt_f_df1_eq_1",
        "ci_95", "two_sided_p", "unparsed"
      )
    ),
    exactness_rank = case_when(
      z_operator == "=" ~ 0L,
      z_operator %in% c("<", ">") ~ 1L,
      TRUE ~ 2L
    ),
    selection_agrees_coded = !is.na(fragment_significant) &
      fragment_significant == selected_significant,
    z = truncate_score_z(z)
  ) %>%
  arrange(paper_id, claim4_id, fragment_id)

claim_selection_summary <- score_all_claims_fragments %>%
  group_by(claim4_id) %>%
  summarise(
    n_fragments = n(),
    n_parsed_fragments = sum(!is.na(z)),
    n_significant_fragments = sum(fragment_significant %in% TRUE, na.rm = TRUE),
    n_nonsignificant_fragments =
      sum(fragment_significant %in% FALSE, na.rm = TRUE),
    n_ambiguous_fragments = sum(is.na(fragment_significant)),
    within_claim_disagreement =
      n_significant_fragments > 0 & n_nonsignificant_fragments > 0,
    .groups = "drop"
  )

score_all_claims <- score_all_claims_fragments %>%
  left_join(claim_selection_summary, by = "claim4_id") %>%
  group_by(claim4_id) %>%
  arrange(
    desc(selection_agrees_coded),
    source_rank,
    exactness_rank,
    desc(!is.na(b)),
    desc(!is.na(se)),
    fragment_id,
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    estimate_id = claim4_id,
    abs_z = abs(z),
    significant = fragment_significant,
    claim_selection_status = case_when(
      selection_agrees_coded ~ "aligned_with_score_code",
      !is.na(fragment_significant) ~ "parsed_mismatch",
      TRUE ~ "unparsed"
    )
  ) %>%
  calc_study_weights() %>%
  select(
    dataset, metaid, studyid, estimate_id, paper_id, claim_id, report_id,
    citation, journal, discipline, year, source, subset, measure, z, abs_z,
    z_operator, p, b, se, ss, weights, significant,
    claim4_id, p_operator, coded_stat_evidence, stat_fragment, fragment_id,
    nonsig, selected_significant, fragment_significant, significance_rule,
    claim_selection_status, selection_agrees_coded, n_fragments,
    n_parsed_fragments, n_significant_fragments, n_nonsignificant_fragments,
    n_ambiguous_fragments, within_claim_disagreement, z_source, effect_source,
    se_source, sign_source, parse_status
  ) %>%
  arrange(paper_id, claim4_id)

validation_counts <- bind_rows(
  score_all_claims_fragments %>%
    count(z_source, name = "n") %>%
    mutate(table = "z_source", value = z_source) %>%
    select(table, value, n),
  score_all_claims_fragments %>%
    count(z_operator, name = "n") %>%
    mutate(table = "z_operator", value = z_operator) %>%
    select(table, value, n),
  score_all_claims_fragments %>%
    count(effect_source, name = "n") %>%
    mutate(table = "effect_source", value = effect_source) %>%
    select(table, value, n),
  score_all_claims_fragments %>%
    count(se_source, name = "n") %>%
    mutate(table = "se_source", value = se_source) %>%
    select(table, value, n),
  score_all_claims_fragments %>%
    count(parse_status, name = "n") %>%
    mutate(table = "parse_status", value = parse_status) %>%
    select(table, value, n),
  score_all_claims %>%
    count(claim_selection_status, name = "n") %>%
    mutate(table = "claim_selection_status", value = claim_selection_status) %>%
    select(table, value, n)
)

claim_level_summary <- score_all_claims %>%
  summarise(
    n_claims = n(),
    n_with_z = sum(!is.na(z)),
    n_with_significance_decision = sum(!is.na(significant)),
    score_selected_significant_rate = mean(selected_significant),
    derived_significant_rate = mean(significant, na.rm = TRUE),
    n_within_claim_disagreement = sum(within_claim_disagreement),
    n_aligned_with_score_code =
      sum(claim_selection_status == "aligned_with_score_code"),
    n_parsed_mismatch = sum(claim_selection_status == "parsed_mismatch"),
    n_unparsed = sum(claim_selection_status == "unparsed")
  )

known_fragment_ok <- function(fragment, predicate) {
  score_all_claims_fragments %>%
    filter(stringr::str_detect(stat_fragment, stringr::fixed(fragment))) %>%
    summarise(ok = any({{ predicate }}, na.rm = TRUE)) %>%
    pull(ok)
}

validation_checks <- tibble(
  check = c(
    "raw_claim_rows",
    "fragment_rows_gt_raw_claim_rows",
    "claim_level_rows",
    "claim_level_unique_claim4_id",
    "discipline_complete",
    "valid_z_operator_values",
    "claim_level_significance_rate_close_to_score",
    "exact_p_values_match_qnorm",
    "p_less_than_maps_to_z_greater_than",
    "p_greater_than_maps_to_z_less_than",
    "known_reported_z_example",
    "known_reported_t_example",
    "known_f_example",
    "known_b_t_ci_example",
    "known_beta_se_z_example"
  ),
  passed = c(
    nrow(claims_non_significant_bushels) == 3066,
    nrow(score_all_claims_fragments) > nrow(claims_non_significant_bushels),
    nrow(score_all_claims) == 3066,
    !anyDuplicated(score_all_claims$claim4_id),
    !any(is.na(score_all_claims$discipline)),
    all(is.na(score_all_claims$z_operator) |
          score_all_claims$z_operator %in% c("=", "<", ">")),
    abs(
      mean(score_all_claims$significant, na.rm = TRUE) -
        mean(score_all_claims$selected_significant)
    ) < 0.02,
    score_all_claims_fragments %>%
      filter(z_source == "two_sided_p", p_operator == "=", p > 0) %>%
      summarise(ok = all(abs(abs(z) - z_from_two_sided_p(p)) < 1e-10)) %>%
      pull(ok),
    score_all_claims_fragments %>%
      filter(z_source == "two_sided_p", p_operator == "<") %>%
      summarise(ok = all(z_operator == ">")) %>%
      pull(ok),
    score_all_claims_fragments %>%
      filter(z_source == "two_sided_p", p_operator == ">") %>%
      summarise(ok = all(z_operator == "<")) %>%
      pull(ok),
    known_fragment_ok(
      "z = -.10, p > .19",
      z_source == "reported_z" & abs(z + 0.10) < 1e-12
    ),
    known_fragment_ok(
      "t(34) = -.17",
      z_source == "reported_t" & abs(z + 0.17) < 1e-12
    ),
    known_fragment_ok(
      "F(1,38)=5.40, p=.026, partial eta-squared = .12",
      z_source == "sqrt_f_df1_eq_1" & abs(abs(z) - sqrt(5.40)) < 1e-12
    ),
    known_fragment_ok(
      "B = .04, t(169) = 2.28, p = .02, 95% CI: [.01, .07]",
      z_source == "reported_t" &
        abs(z - 2.28) < 1e-12 &
        abs(b - 0.04) < 1e-12 &
        !is.na(se)
    ),
    known_fragment_ok(
      "beta = .03, SE = .04",
      abs(b - 0.03) < 1e-12 &
        abs(se - 0.04) < 1e-12 &
        z_source %in% c("coef_over_se", "reported_z")
    )
  )
)

print(validation_checks, n = nrow(validation_checks), width = Inf)
stopifnot(all(validation_checks$passed))

saveRDS(score_all_claims, "data/SCORE_all_claims.rds")
saveRDS(
  score_all_claims_fragments,
  file.path(audit_dir, "score_all_claims_fragments.rds")
)
write_csv_if_present(
  score_all_claims,
  file.path(audit_dir, "score_all_claims_claim_level.csv")
)
write_csv_if_present(
  validation_checks,
  file.path(audit_dir, "score_all_claims_validation.csv")
)

capture.output(
  {
    cat("SCORE all-claims validation\n\n")
    print(validation_checks, n = nrow(validation_checks), width = Inf)
    cat("\nClaim-level summary\n\n")
    print(claim_level_summary, n = nrow(claim_level_summary), width = Inf)
    cat("\nCounts\n\n")
    print(validation_counts, n = nrow(validation_counts), width = Inf)
  },
  file = file.path(table_dir, "score_all_claims_validation.txt")
)

cat("\nClaim-level summary:\n")
print(claim_level_summary, n = nrow(claim_level_summary), width = Inf)
cat("\nSaved SCORE all-claims outputs.\n")
