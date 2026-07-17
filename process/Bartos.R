library(dplyr)
library(readr)

exercise <- read_csv("data_raw/Bartos/data/data_processed.csv") %>%
  mutate(
    measure_detailed = case_when(
      effect_size_type %in% c("SMD (Hedge's g)", "SMD (Hedges'g)") ~
        "SMD (Hedges' g)",
      effect_size_type %in% c("Cohen's d", "SMD (Cohen's d)") ~
        "SMD (Cohen's d)",
      effect_size_type == "SMD (Hedges’ d/Cohen's d)" ~
        "SMD", # ambiguous source label; Hedges' d is non-standard
      TRUE ~ effect_size_type
    )
  )
saveRDS(exercise, file = "data/Bartos.rds")
