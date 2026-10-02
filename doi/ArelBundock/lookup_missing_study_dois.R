# Look up Briggs source references with bibliographic data but no DOI.
# Save candidates for review; processing uses only the accepted local mapping.

library(tidyverse)
library(httr2)
source("R/doi_lookup.R")

references <- read_csv("doi/ArelBundock/derived/study_references.csv",
                       show_col_types = FALSE) %>%
  filter(is.na(reference_doi), !is.na(reference_title),
         !is.na(reference_author), !is.na(reference_year))
keys <- readRDS("data/ArelBundock.rds") %>%
  filter(sample == "Briggs") %>%
  distinct(meta_id, study_id, study_year, study_journal)
requests <- keys %>% inner_join(references, by = "meta_id",
  relationship = "many-to-many") %>%
  filter(study_year == reference_year) %>%
  mutate(first_author = word(normalise_text(reference_author), 1),
    label_author_match = str_detect(normalise_text(study_id),
      paste0("^", first_author, "\\b"))) %>%
  filter(label_author_match) %>%
  mutate(source_journal = coalesce(reference_journal, study_journal),
    query = str_c(reference_title, reference_author, reference_year,
                  coalesce(source_journal, ""), sep = " "))
stopifnot(!anyDuplicated(requests[c("meta_id", "study_id", "study_year",
                                   "study_journal", "reference_index")]))

results <- lookup_identifiers(requests %>% transmute(query,
  source_title = reference_title, source_journal,
  source_year = reference_year, source_author = reference_author) %>%
  distinct(), "doi/ArelBundock/derived/references_without_doi_v1.rds")
review <- requests %>% left_join(results %>%
  select(-any_of(c("source_title", "source_journal", "source_year",
                   "source_author", "source_doi_pattern", "source_doi"))),
  by = "query") %>%
  mutate(accepted = status == "matched" &
    candidate_type == "journal-article" &
    coalesce(title_similarity >= 0.95, FALSE) &
    coalesce(author_match, FALSE) & coalesce(year_match, FALSE) &
    (is.na(source_journal) | coalesce(journal_match, FALSE)))
write_csv(review, "doi/ArelBundock/derived/references_without_doi_review.csv",
          na = "")

key <- c("meta_id", "study_id", "study_year", "study_journal")
mapping <- review %>% filter(accepted) %>%
  select(all_of(key), doi_study = doi) %>% distinct()
conflicts <- mapping %>% count(across(all_of(key))) %>% filter(n > 1L)
stopifnot(!nrow(conflicts))
write_csv(mapping, "doi/ArelBundock/final/references_without_doi_mapping.csv",
          na = "NA")
print(review %>% count(status, accepted))
