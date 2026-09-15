library(tidyverse)
source("R/doi_lookup.R")

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
# The reference identifies the source meta-analysis, not a primary trial.
exercise$doi <- extract_doi(exercise$reference)
exercise$doi_scope <- "meta-analysis"
if (file.exists("doi/Bartos/final/doi_mapping.rds")) {
  exercise <- join_identifiers(exercise,
    readRDS("doi/Bartos/final/doi_mapping.rds"), "reference", "doi")
}
saveRDS(exercise, file = "data/Bartos.rds")
