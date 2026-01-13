library(readr)
exercise <- read_csv("data/Bartos/data_processed.csv")
saveRDS(exercise, file = "data/Bartos.rds")
