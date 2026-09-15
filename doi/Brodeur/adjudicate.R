# Convert the reviewed Brodeur lookup into the final article-DOI mapping.
# Run after resolving any rows in final/doi_decisions.csv, then run process/Brodeur.R.

library(tidyverse)
library(haven)
source("R/doi_lookup.R")

papers <- read_csv("doi/Brodeur/derived/brodeur_doi_lookup.csv",
                   show_col_types = FALSE)
source_data <- read_dta("data_raw/Brodeur/data/merged.dta", encoding = "latin1") %>%
  mutate(across(where(is.character),
                ~ iconv(.x, from = "latin1", to = "UTF-8", sub = "")),
         title_key = normalise_text(title),
         source_doi = extract_doi(na_if(doinumber, ""))) %>%
  distinct(title_key, journal, year, source_doi)
doi_map <- papers %>%
  transmute(title_key = normalise_text(title), journal, year, doi) %>%
  left_join(source_data, by = c("title_key", "journal", "year")) %>%
  select(title_key, journal, year, source_doi, doi)
stopifnot(nrow(doi_map) == 329L,
          !anyDuplicated(doi_map[c("title_key", "journal", "year")]),
          !anyNA(doi_map$doi))
write_csv(doi_map, "doi/Brodeur/final/doi_map.csv")
