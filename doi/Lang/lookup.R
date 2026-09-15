# Audit Lang's journal versions, including citations without separate titles.
# Canonical processing applies the documented manual overrides after this lookup.
library(tidyverse)
library(httr2)
source("process/Lang.R")

journals <- read_csv("process/Lang_journals.csv", show_col_types = FALSE) %>%
  mutate(journal_key = normalise_journal(journal)) %>%
  select(journal_key, source_doi_pattern = doi_pattern)
journal_names <- paste(c("American Economic Review", "Quarterly Journal of Economics",
  "Review of Economic Studies", "Journal of Political Economy", "Econometrica"),
  collapse = "|")
papers <- lang %>%
  distinct(paper_id, studyid, source_title, citation, journal, year, source_author1,
           previous_doi = doi) %>%
  mutate(
    citation_clean = str_replace_all(citation, fixed("¬†"), " "),
    source_journal = coalesce(journal,
      str_extract(citation_clean, regex(journal_names, ignore_case = TRUE))),
    citation_title = str_remove(citation_clean, "^.*?\\([0-9]{4}\\)\\.?\\s*"),
    citation_title = str_remove(citation_title,
      regex(paste0("\\.?\\s*(?:The )?(?:", journal_names, ").*$"),
            ignore_case = TRUE)),
    source_title = coalesce(source_title,
      str_remove(str_trim(citation_title), "[.]$")),
    source_year = year,
    source_author = coalesce(source_author1, str_extract(citation_clean, "^[^,]+")),
    journal_key = normalise_journal(source_journal)) %>%
  left_join(journals, by = "journal_key")
stopifnot(nrow(papers) == 736L, !anyDuplicated(papers$paper_id),
          !anyNA(papers$source_title), !anyNA(papers$source_journal),
          !anyNA(papers$source_doi_pattern))

# Preserve the historical selections as the audit baseline, even on repeat runs.
legacy <- read_csv("doi/Lang/derived/lang_doi_candidates.csv",
                   show_col_types = FALSE) %>%
  group_by(paper_id) %>% summarise(
    legacy_doi = str_c(unique(doi), collapse = ";"),
    legacy_weak_title = any(title_overlap < 0.9, na.rm = TRUE), .groups = "drop")
papers <- papers %>% left_join(legacy, by = "paper_id") %>%
  mutate(
    legacy_prefix_mismatch = map2_lgl(legacy_doi, source_doi_pattern,
      ~ !all(str_detect(str_split(.x, ";")[[1]], .y))),
    query = str_c(source_title, source_journal, source_year, source_author, sep = " "))
pending <- papers %>% filter(legacy_prefix_mismatch | legacy_weak_title |
                              is.na(journal) | studyid %in% names(manual_dois))
message("Journal-aware lookup: ", nrow(pending), " source paper IDs; ",
        sum(papers$legacy_prefix_mismatch), " historical DOI-prefix mismatches")
results <- lookup_identifiers(pending,
  "doi/Lang/derived/crossref_journal_v3.rds")
candidates <- pending %>%
  left_join(results, by = c("query", "source_title", "source_journal",
    "source_year", "source_author", "source_doi_pattern")) %>%
  mutate(
    doi_automatic = doi, automatic_status = status,
    manual_doi = unname(manual_dois[studyid]),
    doi = coalesce(manual_doi, doi),
    status = if_else(!is.na(manual_doi), "matched", status),
    assignment_method = if_else(!is.na(manual_doi), "manual", "journal_lookup"),
    changed_from_legacy = doi != legacy_doi)
write_csv(candidates, "doi/Lang/derived/lang_doi_journal_lookup.csv")
write_csv(filter(candidates, status != "matched"),
          "doi/Lang/derived/lang_doi_journal_review.csv")
source("process/Lang.R")
print(candidates %>% count(status, assignment_method, changed_from_legacy))
