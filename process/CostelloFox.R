# Data from @costello2022decline which were further processed by @yang2024large 
# and made available at 
# [Github](https://github.com/Yefeng0920/replication_EcoEvo_git/blob/main/data/main/main_dat_processed.csv)

df <- read.csv("data/Yang/main_dat_processed.csv") 
saveRDS(df, "data/CostelloFox.rds")
rm(df)