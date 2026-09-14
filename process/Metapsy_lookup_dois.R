# Recover source article DOIs and look up complete references within meta-analyses.
# Run this optional network stage before process/Metapsy_process.R.

library(tidyverse)
library(httr2)
source("R/doi_lookup.R")
source("process/reference_doi_helpers.R")

papers <- readRDS("data_raw/Metapsy/data/Metapsy_Jan2026.rds") %>%
  imap_dfr(function(df, metaid) {
    for (field in c("doi", "full_ref", "full_reference", "reference")) {
      if (!field %in% names(df)) df[[field]] <- NA_character_
    }
    df %>% transmute(metaid, study,
      reference = coalesce(na_if(full_ref, ""), na_if(full_reference, ""),
                           na_if(reference, "")),
      source_doi = coalesce(extract_doi(doi), extract_doi(reference)))
  }) %>% distinct()

# Retain established screened matches while refreshing only the original
# residual review set. This cache uses corrected full-reference metadata.
previous_lookup <- read_csv("doi/Metapsy/doi_lookup.csv", show_col_types = FALSE) %>%
  mutate(retrieved_at = as.character(retrieved_at))
review_keys <- read_csv("doi/Metapsy/doi_review.csv", show_col_types = FALSE) %>%
  distinct(metaid, study, reference)
stopifnot(nrow(review_keys) == 374L)
pending <- papers %>% semi_join(review_keys,
                                by = c("metaid", "study", "reference")) %>%
  mutate(source_year = coalesce(reference_metadata(reference)$source_year,
                                as.integer(str_extract(study, "(?:19|20)[[:digit:]]{2}"))))
refreshed <- lookup_reference_dois(pending,
  "doi/Metapsy/crossref_references_v3.rds") %>%
  mutate(retrieved_at = as.character(retrieved_at),
         candidate_years = as.character(candidate_years))
papers <- previous_lookup %>% anti_join(review_keys,
  by = c("metaid", "study", "reference")) %>%
  mutate(candidate_years = as.character(candidate_years)) %>%
  bind_rows(refreshed)
write_csv(papers, "doi/Metapsy/doi_lookup.csv")

# A repeated author-year label can refer to different publications even within
# one database. Keep the exact source reference in the mapping key.
mapping <- papers %>% select(metaid, study, reference, doi) %>% distinct()
stopifnot(!anyDuplicated(mapping[c("metaid", "study", "reference")]))
write_csv(papers %>% group_by(metaid, study) %>%
  filter(n_distinct(reference) > 1L) %>% ungroup(),
  "doi/Metapsy/doi_multiple_references.csv")
saveRDS(mapping, "doi/Metapsy/doi_mapping.rds")
print(count(papers, doi_source, review))
