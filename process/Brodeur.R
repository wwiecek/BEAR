# Preserve raw Brodeur data and apply the final DOI adjudication offline.
# `process/Brodeur_doi_map.csv` is the tracked, title-level final mapping.
library(tidyverse)
library(haven)
source("R/doi_lookup.R")

df <- read_dta("data_raw/Brodeur/data/merged.dta", encoding = "latin1") %>%
  mutate(across(where(is.character),
                ~ iconv(.x, from = "latin1", to = "UTF-8", sub = "")),
         source_doi = extract_doi(na_if(doinumber, "")),
         title_key = normalise_text(title))
doi_map <- read_csv("process/Brodeur_doi_map.csv", show_col_types = FALSE)

checked_map <- doi_map %>% left_join(
  df %>% distinct(title_key, journal, year, actual_source_doi = source_doi),
  by = c("title_key", "journal", "year"))
stopifnot(nrow(doi_map) == 329L,
          !anyDuplicated(doi_map[c("title_key", "journal", "year")]),
          !anyNA(doi_map$doi),
          identical(checked_map$actual_source_doi, checked_map$source_doi))

df <- df %>% select(-source_doi) %>%
  join_identifiers(select(doi_map, title_key, journal, year, doi),
                   c("title_key", "journal", "year"), "doi") %>%
  select(-title_key)
saveRDS(df, file = "data/Brodeur.rds")
