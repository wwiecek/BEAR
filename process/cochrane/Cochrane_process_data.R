# Build data/Cochrane.rds from a parsed checkpoint or existing RM5 files.
# Manifest abstracts and review-group codes are optional annotations.

library(tidyverse)
library(fs)
library(metafor)

source("process/cochrane/Cochrane_helpers.R")
source("process/cochrane/Cochrane_rct_score.R")

manifest_path <- "data_raw/Cochrane/data/cdsr_interventions_23sep2026.csv"
older_manifest_path <- "data_raw/Cochrane/data/cdsr_interventions_19nov2025.csv"
rm5_dir <- "data_raw/Cochrane/rm5"
checkpoint_path <- "data_raw/Cochrane/data/cdsr_rm5_results.rds"
corrections_path <- "process/cochrane/edition_corrections.csv"
audit_path <- "data_raw/Cochrane/data/edition_audit.csv"
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

# The checkpoint can label one RM5 file with several edition DOIs. Each file
# represents one source edition and must contribute its study rows only once.
results_all <- results_all %>%
  filter(ok %in% TRUE, !is.na(file)) %>%
  mutate(cochrane_id = id_from_doi(file))
headers <- map_dfr(unique(results_all$file), rm5_header, rm5_dir = rm5_dir)
corrections <- read_csv(corrections_path, show_col_types = FALSE)
stopifnot(!anyDuplicated(corrections[c("cochrane_id", "label_doi")]))
corrections <- corrections %>% rename(correction_doi = doi)
review_corrections <- corrections %>%
  distinct(cochrane_id, correction_doi) %>%
  rename(duplicate_doi = correction_doi)
stopifnot(!anyDuplicated(review_corrections$cochrane_id))
file_labels <- results_all %>%
  group_by(file) %>%
  summarise(n_labels = n_distinct(doi), .groups = "drop")
results_all <- results_all %>%
  distinct(file, .keep_all = TRUE) %>%
  rename(label_doi = doi) %>%
  mutate(label_doi = str_to_lower(label_doi)) %>%
  left_join(headers, by = "file") %>%
  left_join(corrections, by = c("cochrane_id", "label_doi")) %>%
  left_join(review_corrections, by = "cochrane_id") %>%
  left_join(file_labels, by = "file") %>%
  mutate(across(c(label_doi, source_doi, correction_doi), str_to_lower))

conflict <- results_all %>%
  filter(!is.na(source_doi), !is.na(correction_doi),
         source_doi != correction_doi)
if (nrow(conflict) > 0L) {
  stop("Edition corrections disagree with embedded RM5 DOIs: ",
       paste(conflict$file, collapse = ", "))
}
if (any(is.na(results_all$status))) {
  stop("RM5 review status is missing for: ",
       paste(results_all$file[is.na(results_all$status)], collapse = ", "))
}
if (any(!results_all$status %in% c("A", "W"))) {
  stop("Unexpected RM5 review status: ",
       paste(unique(results_all$status[!results_all$status %in% c("A", "W")]),
             collapse = ", "))
}

results_all <- results_all %>%
  mutate(doi = coalesce(source_doi, correction_doi,
                        if_else(n_labels > 1L, duplicate_doi, NA_character_),
                        if_else(n_labels == 1L, label_doi, NA_character_)),
         withdrawn = as.integer(status == "W"),
         doi_basis = case_when(
           !is.na(source_doi) ~ "embedded_doi",
           !is.na(correction_doi) ~ basis,
           n_labels > 1L & !is.na(duplicate_doi) ~ "duplicate_label_audit",
           !is.na(doi) ~ "checkpoint_label_unverified",
           TRUE ~ "unresolved"
         ))
write_csv(results_all %>%
            select(cochrane_id, file, label_doi, doi, doi_basis,
                   status, withdrawn, n_labels), audit_path)

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

# Review abstracts describe an edition. Join only when their DOI matches the
# source edition, using the older export for editions absent from the newer one.
manifest_paths <- c(manifest_path, older_manifest_path)
manifest_paths <- manifest_paths[file_exists(manifest_paths)]
if (length(manifest_paths) > 0L) {
  manifest <- map_dfr(manifest_paths, manifest_from_csv) %>%
    mutate(doi = str_to_lower(doi)) %>%
    distinct(doi, .keep_all = TRUE)
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

# Match the source edition by DOI, then use its review ID when the DOI is
# absent or not present in the manifests. RM5 group_id values are codes, not
# the reader-facing specialty labels used for topic.
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
  select(cochrane_id, doi, withdrawn, specialty, rct, studies) %>%
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
    outcome_group = factor(
      classify_outcome_group(comparison.name, outcome.name, subgroup.name),
      levels = c("efficacy", "safety", "dropouts", "bias")
    )
  ) %>%
  select(-weight, -order) %>%
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
    mutate(
      measure = "SMD",
      measure_detailed = "SMD (Hedges' g)"
    )
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
    mutate(measure = "probit", measure_detailed = "probit")
} else {
  binary <- tibble()
}

cdsr <- bind_rows(continuous, binary) %>%
  # Note that z is not calculated as reported effect.size divided by reported SE,
  # but instead based on raw inputs like means, SDs, Ns, events
  mutate(z = yi / sqrt(vi))

saveRDS(cdsr, output_path)
message("Saved processed Cochrane data: ", output_path)
