library(haven)
df <- read_dta("data_raw/Askarov/Mandatory data-sharing 30 Aug 2022.dta")
saveRDS(df, file = "data/Askarov.rds")
rm(df)
