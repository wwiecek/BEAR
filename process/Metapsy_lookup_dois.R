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
papers <- lookup_reference_dois(papers,
  "data_raw/Metapsy/derived/crossref_references_v1.rds")
write_csv(papers, "data_raw/Metapsy/derived/doi_lookup.csv")

# A repeated author-year label can refer to different publications even within
# one database. Keep the exact source reference in the mapping key.
mapping <- papers %>% select(metaid, study, reference, doi) %>% distinct()
stopifnot(!anyDuplicated(mapping[c("metaid", "study", "reference")]))
write_csv(papers %>% group_by(metaid, study) %>%
  filter(n_distinct(reference) > 1L) %>% ungroup(),
  "data_raw/Metapsy/derived/doi_multiple_references.csv")
saveRDS(mapping, "data_raw/Metapsy/derived/doi_mapping.rds")
print(count(papers, doi_source, review))
