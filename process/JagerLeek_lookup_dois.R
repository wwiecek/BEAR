# Convert unique source PMIDs to article DOIs, retaining title checks for review.
# Run before process/JagerLeek.R; batches resume from the saved mapping.

library(tidyverse)
library(httr2)
source("R/doi_lookup.R")

load("data_raw/JagerLeek/data/pvalueData.rda")
papers <- as_tibble(pvalueData) %>% distinct(pubmedID, title)
results <- lookup_pmids(papers$pubmedID,
  "doi/JagerLeek/pmid_to_doi_v1.rds")
audit <- papers %>% mutate(query = as.character(pubmedID)) %>%
  left_join(results, by = "query") %>%
  mutate(title_overlap = map2_dbl(title, candidate_title, title_overlap),
         title_flag = !is.na(doi) & (is.na(title_overlap) | title_overlap < 0.9),
         review_note = if_else(pubmedID %in% c(
           "10824073", "10666428", "10639539", "11027739", "10639540"),
           "Checked: source title appends the research group name", NA_character_),
         review = title_flag & is.na(review_note))
stopifnot(all(str_starts(normalise_text(audit$title[!is.na(audit$review_note)]),
  fixed(normalise_text(audit$candidate_title[!is.na(audit$review_note)])))))
write_csv(audit, "doi/JagerLeek/doi_lookup.csv")
saveRDS(distinct(audit, pubmedID, doi),
        "doi/JagerLeek/doi_mapping.rds")
print(count(audit, status, review))
