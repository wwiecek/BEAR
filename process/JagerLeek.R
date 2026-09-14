library(tidyverse)
source("R/doi_lookup.R")
load("data_raw/JagerLeek/data/pvalueData.rda")

jager_leek <- pvalueData %>%
  data.frame() %>% 
  select(-abstract) %>% 
  mutate_at(c('pvalue','year'), as.numeric)
jager_leek$doi <- NA_character_
if (file.exists("doi/JagerLeek/doi_mapping.rds")) {
  # Assign the joined column alone to retain source row names and data-frame class.
  jager_leek$doi <- join_identifiers(jager_leek,
    readRDS("doi/JagerLeek/doi_mapping.rds"), "pubmedID", "doi")$doi
}
saveRDS(jager_leek, "data/JagerLeek.rds")
