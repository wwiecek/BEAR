library(readr)
exercise <- read_csv("data_raw/Bartos/data/data_processed.csv")
saveRDS(exercise, file = "data/Bartos.rds")
