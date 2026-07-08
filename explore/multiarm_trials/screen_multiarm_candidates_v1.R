# Screen BEAR datasets for within-study multiplicity that may reflect
# multi-arm trials or multiple outcomes per trial.
# Writes a broad BEAR ranking plus a targeted candidate screen.

library(tidyverse)

out_dir <- "explore/multiarm_trials"


# Helpers -----

summarise_counts <- function(n) {
  tibble(
    n_units = length(n),
    mean_rows = mean(n),
    median_rows = median(n),
    p90_rows = unname(quantile(n, 0.9)),
    p95_rows = unname(quantile(n, 0.95)),
    max_rows = max(n),
    pct_multi = mean(n > 1),
    pct_ge3 = mean(n >= 3),
    pct_ge5 = mean(n >= 5)
  )
}


summarise_bear_dataset <- function(df, dataset_name) {
  counts <- df %>%
    filter(dataset == dataset_name) %>%
    count(studyid, name = "rows")

  summarise_counts(counts$rows) %>%
    mutate(
      dataset = dataset_name,
      source_table = "BEAR",
      unit = "studyid"
    ) %>%
    select(dataset, source_table, unit, everything())
}


candidate_row <- function(dataset,
                          source_table,
                          unit,
                          counts,
                          pct_multioutcome = NA_real_,
                          pct_multicomparison = NA_real_,
                          pct_multiarm = NA_real_,
                          mean_outcomes = NA_real_,
                          mean_comparisons = NA_real_,
                          mean_arms = NA_real_,
                          caution = NA_character_) {
  summarise_counts(counts) %>%
    mutate(
      dataset = dataset,
      source_table = source_table,
      unit = unit,
      pct_multioutcome = pct_multioutcome,
      pct_multicomparison = pct_multicomparison,
      pct_multiarm = pct_multiarm,
      mean_outcomes = mean_outcomes,
      mean_comparisons = mean_comparisons,
      mean_arms = mean_arms,
      caution = caution
    ) %>%
    select(
      dataset, source_table, unit, everything()
    )
}


fmt <- function(x) {
  ifelse(is.na(x), "NA", format(round(x, 3), nsmall = 3, trim = TRUE))
}


# Broad BEAR screen -----

bear <- readRDS("BEAR.rds")

bear_screen <- bear  %>%
  bind_rows(.id = "dataset") %>%
  count(dataset, studyid, name = "rows") %>%
  group_by(dataset) %>%
  summarise(
    n_studies = n(),
    n_rows = sum(rows),
    mean_rows_per_study = mean(rows),
    median_rows_per_study = median(rows),
    p90_rows_per_study = unname(quantile(rows, 0.9)),
    p95_rows_per_study = unname(quantile(rows, 0.95)),
    max_rows_per_study = max(rows),
    pct_studies_multi = mean(rows > 1),
    pct_studies_ge3 = mean(rows >= 3),
    pct_rows_in_multi = sum(rows[rows > 1]) / sum(rows),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_rows_per_study))

write_csv(
  bear_screen,
  file.path(out_dir, "bear_study_multiplicity_v1.csv")
)


# Targeted candidate screen -----

wwc <- readRDS("data/WWC.rds") %>%
  group_by(study_id) %>%
  summarise(
    rows = n(),
    n_outcomes = n_distinct(OutcomeMeasureID),
    n_domains = n_distinct(Outcome_Domain),
    .groups = "drop"
  )

clinical_raw <- readRDS("data_raw/clinicaltrials.gov/derived/clinicaltrials.gov_aug2025.rds") %>%
  filter(
    overall_status == "COMPLETED",
    outcome_type == "PRIMARY"
  ) %>%
  group_by(nct_id) %>%
  summarise(
    rows = n(),
    n_outcomes = n_distinct(outcome_id),
    n_analyses = n_distinct(outcome_analysis_id),
    n_arms = first(number_of_arms),
    .groups = "drop"
  )

euctr <- readRDS("data/euctr.rds") %>%
  group_by(id) %>%
  summarise(rows = n(), .groups = "drop")

cochrane_raw <- readRDS("data/Cochrane.rds") %>%
  filter(
    total1 > 0,
    total2 > 0,
    !is.na(measure_group),
    outcome.flag %in% c("CONT", "DICH")
  ) %>%
  group_by(id, study.name) %>%
  summarise(
    rows = n(),
    n_comparisons = n_distinct(comparison.id),
    n_outcomes = n_distinct(outcome.id),
    .groups = "drop"
  )

cochrane_bear <- bear %>%
  filter(dataset == "Cochrane") %>%
  count(studyid, name = "rows")

metapsy <- readRDS("data/Metapsy.rds") %>%
  group_by(metaid, study) %>%
  summarise(
    rows = n(),
    n_measures = n_distinct(measure),
    .groups = "drop"
  )

psymetadata <- readRDS("data/psymetadata.rds") %>%
  group_by(studyid) %>%
  summarise(
    rows = n(),
    n_metaid = n_distinct(metaid),
    .groups = "drop"
  )

yang <- readRDS("data/Yang.rds") %>%
  group_by(meta_id, study_ID) %>%
  summarise(
    rows = n(),
    n_measures = n_distinct(measure),
    .groups = "drop"
  )

costello_fox <- readRDS("data/CostelloFox.rds") %>%
  group_by(meta.analysis.id, study2) %>%
  summarise(
    rows = n(),
    n_grouped_es = n_distinct(grouped_es),
    .groups = "drop"
  )

candidate_screen <- bind_rows(
  candidate_row(
    dataset = "WWC",
    source_table = "data/WWC.rds",
    unit = "study_id",
    counts = wwc$rows,
    pct_multioutcome = mean(wwc$n_outcomes > 1),
    mean_outcomes = mean(wwc$n_outcomes),
    caution = paste(
      "Strong multiple-outcome signal.",
      "No explicit arm-level structure in the processed table."
    )
  ),
  candidate_row(
    dataset = "clinicaltrialsgov",
    source_table = "data_raw/clinicaltrials.gov/derived/clinicaltrials.gov_aug2025.rds",
    unit = "nct_id",
    counts = clinical_raw$rows,
    pct_multioutcome = mean(clinical_raw$n_outcomes > 1),
    pct_multiarm = mean(clinical_raw$n_arms > 2, na.rm = TRUE),
    mean_outcomes = mean(clinical_raw$n_outcomes),
    mean_arms = mean(clinical_raw$n_arms, na.rm = TRUE),
    caution = paste(
      "Best source here with an explicit number_of_arms field.",
      "Rows can still reflect multiple analyses per outcome, not only extra outcomes."
    )
  ),
  candidate_row(
    dataset = "euctr",
    source_table = "data/euctr.rds",
    unit = "id",
    counts = euctr$rows,
    caution = "Essentially one row per trial in the current extraction."
  ),
  candidate_row(
    dataset = "Cochrane_raw",
    source_table = "data/Cochrane.rds",
    unit = "review_id x study.name",
    counts = cochrane_raw$rows,
    pct_multioutcome = mean(cochrane_raw$n_outcomes > 1),
    pct_multicomparison = mean(cochrane_raw$n_comparisons > 1),
    mean_outcomes = mean(cochrane_raw$n_outcomes),
    mean_comparisons = mean(cochrane_raw$n_comparisons),
    caution = paste(
      "Best raw source for explicit comparison and outcome multiplicity.",
      "Same trial can appear in more than one review, so correlation work should stay within review-study units."
    )
  ),
  candidate_row(
    dataset = "Cochrane_in_BEAR",
    source_table = "BEAR.rds",
    unit = "studyid",
    counts = cochrane_bear$rows,
    caution = paste(
      "Current BEAR build keeps comparison.nr == 1 and outcome.nr == 1,",
      "so most raw multiplicity is suppressed."
    )
  ),
  candidate_row(
    dataset = "Metapsy",
    source_table = "data/Metapsy.rds",
    unit = "metaid x study",
    counts = metapsy$rows,
    pct_multioutcome = mean(metapsy$rows > 1),
    mean_outcomes = mean(metapsy$rows),
    caution = paste(
      "Useful for repeated effect sizes within meta-study units.",
      "All rows use the same measure label, so multiplicity is not driven by mixed effect metrics."
    )
  ),
  candidate_row(
    dataset = "psymetadata",
    source_table = "data/psymetadata.rds",
    unit = "studyid",
    counts = psymetadata$rows,
    pct_multicomparison = mean(psymetadata$n_metaid > 1),
    mean_comparisons = mean(psymetadata$n_metaid),
    caution = paste(
      "Large study-level repetition, but much of it is cross-meta duplication",
      "rather than multiple outcomes inside one trial."
    )
  ),
  candidate_row(
    dataset = "Yang",
    source_table = "data/Yang.rds",
    unit = "meta_id x study_ID",
    counts = yang$rows,
    pct_multioutcome = mean(yang$rows > 1),
    mean_outcomes = mean(yang$rows),
    caution = paste(
      "Substantial within-study multiplicity in ecology data.",
      "Not a trial dataset, and most study units use a single measure type."
    )
  ),
  candidate_row(
    dataset = "CostelloFox",
    source_table = "data/CostelloFox.rds",
    unit = "meta.analysis.id x study2",
    counts = costello_fox$rows,
    pct_multioutcome = mean(costello_fox$rows > 1),
    mean_outcomes = mean(costello_fox$rows),
    caution = paste(
      "Large repeated-effect structure within study units.",
      "Not a trial dataset, and grouped effect-size labels are almost always constant within study."
    )
  )
) %>%
  arrange(desc(mean_rows))

write_csv(
  candidate_screen,
  file.path(out_dir, "candidate_screen_v1.csv")
)


# Notes for quick reuse -----

top_bear <- bear_screen %>%
  filter(dataset %in% c("WWC", "clinicaltrials.gov", "Cochrane", "Metapsy")) %>%
  mutate(
    line = paste0(
      "- ", dataset, ": mean rows/study = ", fmt(mean_rows_per_study),
      ", pct multi-study rows = ", fmt(pct_studies_multi)
    )
  ) %>%
  pull(line)

top_candidates <- candidate_screen %>%
  mutate(
    line = paste0(
      "- ", dataset, " [", source_table, "]: mean rows/unit = ",
      fmt(mean_rows), ", pct_multi = ", fmt(pct_multi),
      ", pct_multioutcome = ", fmt(pct_multioutcome),
      ", pct_multicomparison = ", fmt(pct_multicomparison),
      ", pct_multiarm = ", fmt(pct_multiarm)
    )
  ) %>%
  pull(line)

notes <- c(
  "# Multiarm Screening Notes",
  "",
  "This note is generated by `screen_multiarm_candidates_v1.R`.",
  "",
  "## Main takeaways",
  "",
  paste(
    "1. The strongest raw source for explicit multi-arm or multi-outcome",
    "structure is Cochrane before the BEAR first-comparison/first-outcome",
    "restriction."
  ),
  paste(
    "2. The strongest source with an explicit arm count field is",
    "clinicaltrialsgov."
  ),
  paste(
    "3. WWC looks strong for multiple outcomes per study, but not for",
    "explicit multi-arm identification."
  ),
  paste(
    "4. euctr is not promising for this purpose in the current extraction."
  ),
  paste(
    "5. psymetadata has a lot of repeated study IDs, but much of that is",
    "cross-meta duplication rather than within-trial multiplicity."
  ),
  "",
  "## Current BEAR quick screen",
  "",
  top_bear,
  "",
  "## Targeted candidate screen",
  "",
  top_candidates
)

write_lines(
  notes,
  file.path(out_dir, "screen_notes_v1.md")
)


# Console summary -----

cat("Wrote:\\n")
cat("- ", file.path(out_dir, "bear_study_multiplicity_v1.csv"), "\\n", sep = "")
cat("- ", file.path(out_dir, "candidate_screen_v1.csv"), "\\n", sep = "")
cat("- ", file.path(out_dir, "screen_notes_v1.md"), "\\n", sep = "")
