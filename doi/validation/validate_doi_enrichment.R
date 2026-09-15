# Compare enriched datasets with the snapshots taken before DOI enrichment.
# Save a compact local validation report without rebuilding BEAR.rds.

library(tidyverse)
source("data_raw/doi_validation/test_lang_dois.R")

checks <- map_dfr(c("Lang", "Head", "Brodeur"), function(dataset) {
  old <- readRDS(paste0("data_raw/doi_validation/", dataset, ".rds"))
  new <- readRDS(paste0("data/", dataset, ".rds"))
  unchanged <- if (dataset == "Lang") {
    setdiff(names(old), c("studyid", "source_title", "doi"))
  } else names(old)
  stopifnot(identical(old[unchanged], new[unchanged]))
  tibble(dataset, rows = nrow(new), unchanged_columns_preserved = TRUE,
         corrected_columns = if_else(dataset == "Lang",
                                      "studyid;source_title;doi", ""),
         doi_rows = sum(!is.na(new$doi)))
})
head <- readRDS("data/Head.rds")
stopifnot(identical(head$doi, head$first.doi))
old <- readRDS("data_raw/doi_validation/doi2pmid_progress.rds")
new <- readRDS("data_raw/Head/derived/doi2pmid_progress.rds")
stopifnot(identical(old, new))
stopifnot(identical(
  readBin("data_raw/doi_validation/lang_doi_candidates.csv", "raw",
          n = file.info("data_raw/doi_validation/lang_doi_candidates.csv")$size),
  readBin("data_raw/Lang/derived/lang_doi_candidates.csv", "raw",
          n = file.info("data_raw/Lang/derived/lang_doi_candidates.csv")$size)))
write_csv(checks, "data_raw/doi_validation/enrichment_checks.csv")
print(checks)
cat("Historical Lang candidates and Head mappings are unchanged.\n")
