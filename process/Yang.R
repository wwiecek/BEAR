library(tidyverse)
load("data_raw/Yang/EcoEvo_PB_datasets.Rdata")

# Datafile dat_processed_rob.csv may be downloaded from 
# https://github.com/Yefeng0920/replication_EcoEvo_git/blob/main/data/sensitivity/dat_processed_rob.csv

extract_year_yang <- function(x) {
  # Find the last 4 consecutive digits in the string
  year_match <- regexpr("\\d{4}[^0-9]*$", x)
  if (year_match > 0) {
    potential_year <- as.numeric(substr(x, year_match, year_match + 3))
    if (potential_year >= 1950 && potential_year <= 2025) {
      return(potential_year)
    }
  }
  return(NA)
}

df <- rbind(
  lnRR %>% 
    lapply(function(x) select(x, study_ID, year_pub, es, sei) %>% 
             mutate(study_ID = as.character(study_ID))) %>% 
    bind_rows(.id = "meta_id") %>% 
    mutate(measure = "lnRR"),
  
  Zr %>% 
    lapply(function(x) select(x, study_ID, year_pub, es, sei) %>% 
             mutate(study_ID = as.character(study_ID))) %>% 
    bind_rows(.id = "meta_id") %>% 
    mutate(measure = "Zr"),
  
  SMD %>% 
    lapply(function(x) select(x, study_ID, year_pub, es, sei) %>% 
             mutate(study_ID = as.character(study_ID))) %>% 
    bind_rows(.id = "meta_id") %>% 
    mutate(measure = "SMD")
)

saveRDS(df, "data/Yang.rds")
rm(df)


# this derived dataset is OK, but it does not have year, method, and meta_id
# that I'd like to use, therefore I do minimal data processing in data/Yang/
# to get data from Yang 2023 paper

# dtlist[["Yang"]] <- read.csv("data/Yang/dat_processed_rob.csv") %>% 
#   transmute(
#     metaid = NA, 
#     studyid = study,
#     method = NA,
#     z = z,
#     b = es,
#     se = se) %>%
#   mutate(year = sapply(studyid, extract_year_yang))
