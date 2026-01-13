library(haven)
library(dplyr)
df <- read_dta("data_raw/Brodeur/merged.dta") 
saveRDS(df, file = "data/Brodeur.rds")
