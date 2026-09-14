# Look up article DOIs independently, then compare overlapping Lang articles.
# Trial-registration DOIs in doinumber are not article identifiers.

library(tidyverse)
library(httr2)
source("R/doi_lookup.R")

brodeur <- readRDS("data/Brodeur.rds")
papers <- brodeur %>% distinct(title, journal, year, author1) %>%
  mutate(source_title = title, source_journal = journal, source_year = year,
         source_author = author1,
         query = str_c(title, journal, year, coalesce(author1, ""), sep = " "))
stopifnot(!anyDuplicated(papers$title))
legacy_dois <- brodeur %>% distinct(title, journal, year, legacy_doi = doi)
stopifnot(!anyDuplicated(legacy_dois[c("title", "journal", "year")]))
papers <- papers %>% left_join(legacy_dois, by = c("title", "journal", "year"))
papers <- papers %>% mutate(source_doi = legacy_doi)
# Do not reuse legacy title-ranked candidates for metadata-aware rankings.
results <- lookup_identifiers(papers,
  "doi/Brodeur/crossref_journal_v3.rds")
papers <- papers %>% left_join(results, by = c("query", "source_title",
  "source_journal", "source_year", "source_author", "source_doi"))
if (!"doi" %in% names(papers)) papers$doi <- NA_character_

# Normalised titles identify comparison candidates, not verified equivalences.
lang <- readRDS("data/Lang.rds") %>%
  distinct(paper_id, source_title, doi) %>%
  mutate(title_key = str_remove_all(normalise_text(source_title), " ")) %>%
  filter(!is.na(title_key), nzchar(title_key)) %>%
  group_by(title_key) %>% summarise(
    lang_papers = n_distinct(paper_id),
    lang_dois = str_c(sort(unique(na.omit(doi))), collapse = ";"),
    .groups = "drop")
papers <- papers %>%
  mutate(title_key = str_remove_all(normalise_text(title), " ")) %>%
  left_join(lang, by = "title_key") %>%
  mutate(
    doi_lookup = doi,
    lang_comparison = case_when(
      is.na(lang_papers) ~ "no_overlap",
      lang_papers > 1 | str_detect(lang_dois, ";") ~ "ambiguous",
      is.na(doi) | lang_dois == "" ~ "missing_doi",
      str_to_lower(str_trim(doi)) == str_to_lower(str_trim(lang_dois)) ~ "agree",
      TRUE ~ "disagree"),
    weak_title = is.na(title_similarity) | title_similarity < 0.88,
    check_year = is.na(year_match) | !year_match,
    check_journal = is.na(candidate_journal) |
      normalise_journal(candidate_journal) != normalise_journal(journal),
    non_article = is.na(candidate_type) | candidate_type != "journal-article",
    review = !str_detect(decision, "^auto_accept") | weak_title | check_year |
      check_journal |
      non_article | lang_comparison %in% c("disagree", "ambiguous", "missing_doi")) %>%
  mutate(
    doi = if_else(str_detect(decision, "^auto_accept") & !review,
                  doi_lookup, legacy_doi),
    doi_selection = case_when(
      doi != legacy_doi ~ "metadata_supported_update",
      str_detect(decision, "^auto_accept") & !review ~
        "metadata_confirmed",
      TRUE ~ "legacy_retained_for_review"))

# The review table is a one-off Brodeur adjudication layer. It is deliberately
# outside the generic matcher and is optional so the workflow remains usable
# for a fresh lookup before manual review is complete.
if (file.exists("doi/Brodeur/doi_decisions.csv")) {
  decisions <- read_csv("doi/Brodeur/doi_decisions.csv", show_col_types = FALSE) %>%
    select(title, journal, year, manual_decision = decision,
           manual_doi = accepted_doi, manual_notes = notes)
  stopifnot(!anyDuplicated(decisions[c("title", "journal", "year")]))
  papers <- papers %>% left_join(decisions,
    by = c("title", "journal", "year")) %>%
    mutate(
      doi = case_when(
        manual_decision == "replace_with_candidate" ~ manual_doi,
        manual_decision == "keep_legacy" ~ legacy_doi,
        TRUE ~ doi),
      doi_selection = case_when(
        manual_decision == "replace_with_candidate" ~
          "manual_version_adjudication",
        manual_decision == "keep_legacy" ~ "manual_legacy_adjudication",
        TRUE ~ doi_selection))
}
write_csv(papers, "doi/Brodeur/brodeur_doi_lookup.csv")
write_csv(filter(papers, review), "doi/Brodeur/doi_review.csv")
# Metadata-supported updates are explicit above; all other legacy candidates stay.
brodeur <- brodeur %>% select(-doi) %>%
  join_identifiers(papers, c("title", "journal", "year"), "doi")
saveRDS(brodeur, "data/Brodeur.rds")
print(papers %>% count(status, review, doi_selection))
print(papers %>% count(lang_comparison))
