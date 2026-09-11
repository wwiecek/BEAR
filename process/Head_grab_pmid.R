# Complete the historical DOI-to-PMID mapping without refreshing saved entries.
# Missing PMIDs already in that mapping remain unchanged.
library(tidyverse)
library(httr2)
source("R/doi_lookup.R")

progress <- if (file.exists("data_raw/Head/derived/doi2pmid_progress.rds")) {
  readRDS("data_raw/Head/derived/doi2pmid_progress.rds")
} else tibble(doi = character(), pmid = character())
stopifnot(!anyDuplicated(progress$doi))
head_dois <- read_csv("data_raw/Head/derived/p_values_cleaned_ww.csv",
                      col_select = first.doi, show_col_types = FALSE)
pending <- head_dois %>% distinct(query = first.doi) %>%
  filter(!query %in% progress$doi)
if (nrow(pending)) {
  results <- lookup_identifiers(pending,
    "data_raw/Head/derived/epmc.rds", "doi_to_pmid")
  additions <- results %>% filter(status != "error") %>%
    transmute(doi = query, pmid)
  progress <- bind_rows(as_tibble(progress), additions)
  saveRDS(progress, "data_raw/Head/derived/doi2pmid_progress.rds")
  write_csv(progress, "data_raw/Head/derived/doi2pmid_progress.csv")
}
