library(haven)
library(dplyr)
df <- read_dta("data_raw/Brodeur/data/merged.dta", encoding = "latin1")
saveRDS(df, file = "data/Brodeur.rds")
