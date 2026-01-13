library(tidyverse)
load("data_raw/JagerLeek/pvalueData.rda")

pvalueData %>% 
  data.frame() %>% 
  select(-abstract) %>% 
  mutate_at(c('pvalue','year'), as.numeric) %>% 
  saveRDS("data/JagerLeek.rds")
