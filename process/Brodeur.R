library(haven)
library(dplyr)
df <- read_dta("data_raw/Brodeur/data/merged.dta", encoding = "latin1")
df <- df %>%
  mutate(across(where(is.character),
                ~ iconv(.x, from = "latin1", to = "UTF-8", sub = "")))
saveRDS(df, file = "data/Brodeur.rds")
