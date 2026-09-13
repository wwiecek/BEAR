# Enrich full source citations; uncertain Crossref candidates stay in the audit.
# Source after R/doi_lookup.R, with tidyverse and httr2 loaded.

lookup_reference_dois <- function(papers, cache_path) {
  queries <- papers %>% filter(is.na(source_doi), !is.na(reference)) %>%
    distinct(query = reference) %>% mutate(source_title = query)
  results <- lookup_identifiers(queries, cache_path) %>%
    mutate(
      # HTML entities and missing spaces before volume numbers are formatting.
      candidate_journal_check = str_replace_all(candidate_journal, "&amp;", "&"),
      citation_check = str_replace_all(query, "(?<=[A-Za-z])(?=[0-9])", " "),
      citation_title_match = map2_dbl(candidate_title, query, ~ title_overlap(.x, .y)),
      citation_journal_match = map2_dbl(candidate_journal_check, citation_check,
                                        ~ title_overlap(.x, .y)),
      citation_author_match = map2_lgl(candidate_author, query,
        ~ !is.na(.x) && str_detect(paste0(" ", normalise_text(.y), " "),
                                  fixed(paste0(" ", normalise_text(.x), " ")))),
      citation_year_match = map2_lgl(query, candidate_year,
        ~ !is.na(.y) && str_detect(.x, fixed(as.character(.y)))),
      accepted = coalesce(citation_title_match >= 0.95 &
        citation_journal_match >= 0.8 & citation_author_match &
        citation_year_match & candidate_type == "journal-article", FALSE),
      candidate_doi = doi, doi = if_else(accepted, extract_doi(doi), NA_character_))
  papers %>% left_join(select(results, -source_title),
                       by = c("reference" = "query")) %>%
    mutate(doi = coalesce(source_doi, doi), doi_source = case_when(
      !is.na(source_doi) ~ "source", !is.na(doi) ~ "crossref",
      TRUE ~ NA_character_), review = is.na(doi) & !is.na(candidate_doi))
}
