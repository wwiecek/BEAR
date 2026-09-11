# Preserve source data and attach cached article DOIs when available.
library(haven)
library(dplyr)
source("R/doi_lookup.R")
df <- read_dta("data_raw/Brodeur/data/merged.dta", encoding = "latin1")
df <- df %>%
  mutate(across(where(is.character),
                ~ iconv(.x, from = "latin1", to = "UTF-8", sub = "")))
if (file.exists("data_raw/Brodeur/derived/brodeur_doi_lookup.csv")) {
  lookup <- readr::read_csv("data_raw/Brodeur/derived/brodeur_doi_lookup.csv",
                            show_col_types = FALSE)
  df <- join_identifiers(df, lookup, c("title", "journal", "year"), "doi")
}
saveRDS(df, file = "data/Brodeur.rds")
