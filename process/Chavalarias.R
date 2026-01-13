# short script for naming cols, compressing CSV data into RDS and adding source column
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/6FMTT3

library(tidyverse)

chava <- rbind(
  read_csv("data_raw/Chavalarias/medline_full_txt_pv.csv", 
           col_names = c("studyid", "journal", "operator", "p", "year", "p_format", "flag")) %>% 
    mutate(source = "PMC full text"),
  read_csv("data_raw/Chavalarias/medline_pt.csv", 
           col_names = c("operator", "p", "p_format", "year", "journal", "studyid", "flag")) %>% 
    mutate(source = "MEDLINE")
) 

saveRDS(chava, file = "data/Chavalarias.rds")
