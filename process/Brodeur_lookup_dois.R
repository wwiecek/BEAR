# Look up article DOIs independently, then compare overlapping Lang articles.
# Trial-registration DOIs in doinumber are not article identifiers.

library(tidyverse)
library(httr2)
source("R/doi_lookup.R")

brodeur <- readRDS("data/Brodeur.rds")
papers <- brodeur %>% distinct(title, journal, year, author1) %>%
  mutate(source_title = title,
         query = str_c(title, journal, year, coalesce(author1, ""), sep = " "))
stopifnot(!anyDuplicated(papers$title))
results <- lookup_identifiers(papers, "data_raw/Brodeur/derived/crossref.rds")
papers <- papers %>% left_join(results, by = c("query", "source_title"))
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
    lang_comparison = case_when(
      is.na(lang_papers) ~ "no_overlap",
      lang_papers > 1 | str_detect(lang_dois, ";") ~ "ambiguous",
      is.na(doi) | lang_dois == "" ~ "missing_doi",
      str_to_lower(str_trim(doi)) == str_to_lower(str_trim(lang_dois)) ~ "agree",
      TRUE ~ "disagree"),
    weak_title = is.na(title_overlap) | title_overlap < 0.90,
    check_year = is.na(candidate_year) | candidate_year != year,
    check_journal = is.na(candidate_journal) |
      normalise_text(candidate_journal) != normalise_text(journal),
    non_article = is.na(candidate_type) | candidate_type != "journal-article",
    review = weak_title | check_year | check_journal | non_article |
      lang_comparison %in% c("disagree", "ambiguous", "missing_doi"))
write_csv(papers, "data_raw/Brodeur/derived/brodeur_doi_lookup.csv")
write_csv(filter(papers, review), "data_raw/Brodeur/derived/doi_review.csv")
brodeur <- join_identifiers(brodeur, papers, c("title", "journal", "year"), "doi")
saveRDS(brodeur, "data/Brodeur.rds")
print(papers %>% count(status, review))
print(papers %>% count(lang_comparison))
