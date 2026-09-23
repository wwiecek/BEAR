# Build data/Cochrane.rds from a parsed checkpoint or existing RM5 files.
# Manifest abstracts and review-group codes are optional annotations.

library(tidyverse)
library(fs)
library(metafor)

source("process/cochrane/Cochrane_helpers.R")
source("process/cochrane/Cochrane_rct_score.R")

manifest_path <- "data_raw/Cochrane/data/cdsr_interventions_9jul2026.csv"
rm5_dir <- "data_raw/Cochrane/rm5"
checkpoint_path <- "data_raw/Cochrane/data/cdsr_rm5_results.rds"
output_path <- "data/Cochrane.rds"

dir_create(path_dir(checkpoint_path))
dir_create(path_dir(output_path))

# Parse local RM5 files only when no reusable parsed checkpoint is available.
if (file_exists(checkpoint_path)) {
  results_all <- readRDS(checkpoint_path)
} else {
  rm5_manifest <- manifest_from_rm5(rm5_dir)
  if (nrow(rm5_manifest) == 0L) {
    stop(
      "No Cochrane checkpoint found at: ", checkpoint_path, ".\n",
      "No RM5 files found in: ", rm5_dir, "."
    )
  }
  if (!requireNamespace("cochrane", quietly = TRUE)) {
    stop(
      "The cochrane package is required to parse RM5 files.\n",
      "Install it with remotes::install_github(\"schw4b/cochrane\")."
    )
  }

  message("Parsing ", nrow(rm5_manifest), " existing RM5 files.")
  results_all <- purrr::pmap_dfr(
    rm5_manifest[c("file", "doi", "cochrane_id")],
    ~ read_rm5_one(..1, rm5_dir, ..2, ..3)
  )
  saveRDS(results_all, checkpoint_path)
  message("Saved Cochrane checkpoint: ", checkpoint_path)
}

if (!is.data.frame(results_all) || nrow(results_all) == 0L) {
  stop("The Cochrane checkpoint contains no review rows.")
}

# Normalize absent legacy columns before processing older checkpoints.
if (!"cochrane_id" %in% names(results_all)) {
  results_all$cochrane_id <- id_from_doi(results_all$file)
}
if (!"doi" %in% names(results_all)) {
  results_all$doi <- NA_character_
}

results_all_fixed <- results_all %>%
  mutate(
    cochrane_id = coalesce(cochrane_id, id_from_doi(doi), id_from_doi(file)),
    studies = map(studies, ~ {
      # Some reviews have NULL or logical placeholders instead of study data.
      if (is.null(.x) || is.logical(.x)) {
        return(tibble())
      }

      as_tibble(.x) %>%
        mutate(
          across(any_of(c("subgroup.nr", "subgroup.id")), as.character)
        )
    })
  )

# Accept valid years through next calendar year to tolerate online-first data.
sanitize_study_year <- function(
    x, min_year = 1900L,
    max_year = as.integer(format(Sys.Date(), "%Y")) + 1L) {
  year_text <- str_trim(as.character(x))
  year_text[year_text == ""] <- NA_character_
  output <- rep(NA_integer_, length(year_text))

  plain <- !is.na(year_text) & str_detect(year_text, "^[0-9]{4}$")
  output[plain] <- suppressWarnings(as.integer(year_text[plain]))

  remaining <- is.na(output) & !is.na(year_text)
  if (any(remaining)) {
    # Use the final plausible four-digit year in labels such as "Smith 2006a".
    match <- str_match(year_text[remaining], ".*((?:19|20)[0-9]{2})\\D*$")[, 2]
    output[remaining] <- suppressWarnings(as.integer(match))
  }

  output[output < min_year | output > max_year] <- NA_integer_
  output
}

classify_outcome_group <- function(comparison_name, outcome_name,
                                   subgroup_name) {
  text <- str_c(
    coalesce(comparison_name, ""),
    coalesce(outcome_name, ""),
    coalesce(subgroup_name, ""),
    sep = " "
  ) %>%
    str_squish()

  bias <- str_detect(
    text,
    regex(
      "bias|risk of bias|sensitivity analys|sensitivity analisys",
      ignore_case = TRUE
    )
  )
  dropout_base <- str_detect(
    text,
    regex(
      paste(
        "withdrawn|withdrawing|withdrew|withdrawal[s]*|withdrawl[s]*",
        "leaving\\s*[the]*\\s*(study|studies|trial|treatment)",
        "dropout[s]*|acceptability|tolerability",
        sep = "|"
      ),
      ignore_case = TRUE
    )
  )
  dropout_exception <- str_detect(
    text,
    regex(
      "alcohol withdrawal|withdrawal symptoms|steroid withdrawal",
      ignore_case = TRUE
    )
  )
  safety <- str_detect(
    text,
    regex(
      "toxicity|adverse|adverse effects|side effect|serious adverse|SAE",
      ignore_case = TRUE
    )
  )

  case_when(
    safety ~ "safety",
    dropout_base & !dropout_exception ~ "dropouts",
    bias ~ "bias",
    TRUE ~ "efficacy"
  )
}

# Add optional review metadata without creating many-to-many joins. DOI takes
# priority because manifests can occasionally contain repeated review IDs.
if (file_exists(manifest_path)) {
  manifest <- manifest_from_csv(manifest_path)
  manifest$rct <- NA
  has_abstract <- !is.na(manifest$abstract) & manifest$abstract != ""
  if (any(has_abstract)) {
    manifest$rct[has_abstract] <- classify_design_v2(
      manifest$abstract[has_abstract]
    )$rct_only_high_conf
  }
} else {
  message("No optional Cochrane metadata manifest found at: ", manifest_path)
  manifest <- tibble(
    cochrane_id = character(), doi = character(),
    abstract = character(), specialty = character(), rct = logical()
  )
}

doi_lookup <- !duplicated(manifest$doi) & !is.na(manifest$doi)
id_lookup <- !duplicated(manifest$cochrane_id) & !is.na(manifest$cochrane_id)
doi_match <- match(results_all_fixed$doi, manifest$doi[doi_lookup])
id_match <- match(
  results_all_fixed$cochrane_id,
  manifest$cochrane_id[id_lookup]
)

doi_specialty <- manifest$specialty[doi_lookup][doi_match]
id_specialty <- manifest$specialty[id_lookup][id_match]
doi_rct <- manifest$rct[doi_lookup][doi_match]
id_rct <- manifest$rct[id_lookup][id_match]

results_all_fixed <- results_all_fixed %>%
  mutate(
    specialty = coalesce(doi_specialty, id_specialty),
    rct = coalesce(doi_rct, id_rct)
  )

studies_long <- results_all_fixed %>%
  select(cochrane_id, doi, specialty, rct, studies) %>%
  unnest(studies) %>%
  mutate(
    study.year = sanitize_study_year(study.year),
    id = factor(id),
    outcome.nr = suppressWarnings(as.integer(outcome.nr)),
    comparison.nr = suppressWarnings(as.integer(comparison.nr)),
    outcome.flag = factor(outcome.flag),
    specialty = factor(specialty),
    measure_group = factor(case_when(
      outcome.measure %in% c("RR", "Risk Ratio") ~ "RR",
      outcome.measure == "OR" ~ "OR",
      outcome.measure == "PETO_OR" ~ "PETO_OR",
      outcome.measure == "RD" ~ "RD",
      outcome.measure %in% c("MD", "Mean Difference") ~ "MD",
      outcome.measure %in% c("SMD", "Std. Mean Difference") ~ "SMD",
      TRUE ~ NA_character_
    )),
    z = effect.size / se,
    outcome_group = factor(
      classify_outcome_group(comparison.name, outcome.name, subgroup.name),
      levels = c("efficacy", "safety", "dropouts", "bias")
    ),
    phase = NA
  ) %>%
  filter(total1 > 0, total2 > 0)

if (nrow(studies_long) == 0L) {
  stop("No Cochrane study rows with positive arm totals were available.")
}

# Recalculate continuous effects as SMD and binary effects on the probit scale.
continuous <- studies_long %>% filter(outcome.flag == "CONT")
if (nrow(continuous) > 0L) {
  continuous <- continuous %>%
    escalc(
      m1i = mean1, sd1i = sd1, n1i = total1,
      m2i = mean2, sd2i = sd2, n2i = total2,
      measure = "SMD", data = ., append = TRUE
    ) %>%
    as_tibble() %>%
    mutate(measure = "SMD")
} else {
  continuous <- tibble()
}

binary <- studies_long %>% filter(outcome.flag == "DICH")
if (nrow(binary) > 0L) {
  binary <- binary %>%
    escalc(
      ai = events1, n1i = total1,
      ci = events2, n2i = total2,
      measure = "PBIT", data = ., append = TRUE
    ) %>%
    as_tibble() %>%
    mutate(measure = "probit")
} else {
  binary <- tibble()
}

cdsr <- bind_rows(continuous, binary) %>%
  mutate(z = yi / sqrt(vi))

saveRDS(cdsr, output_path)
message("Saved processed Cochrane data: ", output_path)
