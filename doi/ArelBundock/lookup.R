# Match source meta-analytic publications to DOIs, then review candidates.
# Run from the project root; processing reads only the accepted local mapping.

library(tidyverse)
library(httr2)
source("R/doi_lookup.R")

articles <- read_csv("doi/ArelBundock/article_metadata.csv",
                     show_col_types = FALSE) %>%
  mutate(query = str_c(article_title, source_journal, source_year,
                       source_author, sep = " "))
stopifnot(nrow(articles) == 46L, !anyDuplicated(articles$meta_id))

matches <- lookup_identifiers(
  articles %>% select(query, source_title = article_title, source_journal,
                      source_year, source_author),
  "doi/ArelBundock/derived/crossref_articles_v1.rds")
review <- articles %>% left_join(matches, by = "query") %>%
  mutate(accepted = status == "matched" &
           coalesce(journal_match, FALSE) &
           coalesce(title_match, FALSE) &
           coalesce(author_match, FALSE) &
           coalesce(year_match, FALSE) &
           candidate_type == "journal-article")
write_csv(review, "doi/ArelBundock/derived/doi_review.csv")
dir.create("doi/ArelBundock/final", recursive = TRUE, showWarnings = FALSE)
write_csv(review %>% transmute(meta_id,
  doi_collection = if_else(accepted, doi, NA_character_)),
  "doi/ArelBundock/final/doi_mapping.csv")
print(review %>% count(status, accepted))
print(review %>% filter(!accepted) %>%
  select(meta_id, article_title, doi, candidate_title, status))
