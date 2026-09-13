# Recover DOIs of the cited meta-analyses, not the underlying primary trials.
# Run this optional network stage before process/Bartos.R.

library(tidyverse)
library(httr2)
source("R/doi_lookup.R")
source("process/reference_doi_helpers.R")

papers <- read_csv("data_raw/Bartos/data/data_processed.csv",
                   show_col_types = FALSE) %>%
  distinct(reference) %>% mutate(source_doi = extract_doi(reference))
papers <- lookup_reference_dois(papers,
  "data_raw/Bartos/derived/crossref_references_v1.rds")
write_csv(papers, "data_raw/Bartos/derived/doi_lookup.csv")
saveRDS(select(papers, reference, doi, doi_source),
        "data_raw/Bartos/derived/doi_mapping.rds")
print(count(papers, doi_source, review))
