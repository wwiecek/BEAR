# Data from @costello2022decline which were further processed by @yang2024large 
# and made available at 
# [Github](https://github.com/Yefeng0920/replication_EcoEvo_git/blob/main/data/main/main_dat_processed.csv)

df <- read.csv("data_raw/Yang/data/main_dat_processed.csv")
char_cols <- vapply(df, is.character, logical(1))
df[char_cols] <- lapply(df[char_cols], iconv, from = "", to = "UTF-8",
                        sub = "")
saveRDS(df, "data/CostelloFox.rds")
rm(df)
