# Enrich full source citations; uncertain Crossref candidates stay in the audit.
# Source after R/doi_lookup.R, with tidyverse and httr2 loaded.

reference_metadata <- function(reference) {
  map_dfr(reference, function(x) {
    clean <- x %>% repair_mojibake() %>% str_replace_all("&amp;", "&") %>%
      str_remove_all("</?[[:alnum:]]+[^>]*>") %>%
      str_remove("\\s+https?://.*$") %>% str_squish()
    year <- str_extract(clean,
      "(?<![[:digit:]])(?:19|20)[[:digit:]]{2}(?![[:digit:]])")
    author <- str_extract(clean, "^[^,(]+") %>%
      str_extract("[[:alpha:]][[:alpha:]'’-]+")
    early_year <- str_detect(clean,
      "^.{0,250}?(?:19|20)[[:digit:]]{2}\\)?[.:;]\\s+")
    if (early_year) {
      after_year <- str_replace(clean,
        "^.*?(?:19|20)[[:digit:]]{2}\\)?[.:;]\\s+", "")
      title <- str_match(after_year, "^[\"“]?(.+?)[\"”]?\\.\\s+")[, 2] %>%
        na_if("")
      after_title <- if_else(is.na(title), NA_character_,
        str_remove(after_year, "^[\"“]?.+?[\"”]?\\.\\s+"))
      journal <- str_match(after_title,
        "^(.+?)(?=,|\\.\\s*\\d|\\s+\\d+\\s*(?:\\(|:|,)|$)")[, 2] %>%
        str_squish() %>% str_remove("\\.$") %>% na_if("")
    } else {
      before_year <- if_else(is.na(year), clean,
        str_remove(clean, "\\.?\\s*(?:19|20)[[:digit:]]{2}.*$"))
      parts <- str_split(before_year, "\\.\\s+")[[1]] %>% str_squish()
      title <- if (length(parts) >= 3L) str_c(parts[2:(length(parts) - 1L)],
        collapse = ". ") else NA_character_
      journal <- if (length(parts) >= 3L) parts[[length(parts)]] else NA_character_
      journal <- journal %>% str_remove(",?\\s*\\d+.*$") %>%
        str_remove("\\.$") %>% na_if("")
    }
    tibble(source_title = title, source_journal = journal,
           source_year = as.integer(year), source_author = author)
  })
}

lookup_reference_dois <- function(papers, cache_path) {
  queries <- papers %>% filter(is.na(source_doi), !is.na(reference)) %>%
    distinct(query = reference, input_year = source_year) %>%
    bind_cols(reference_metadata(.$query), .) %>%
    mutate(source_year = coalesce(source_year, input_year)) %>%
    select(-input_year)
  results <- lookup_identifiers(queries, cache_path) %>%
    mutate(
      accepted = status == "matched" & !is.na(source_title) &
        !is.na(source_journal) & !is.na(source_year),
      candidate_doi = doi, doi = if_else(accepted, extract_doi(doi), NA_character_))
  papers %>% left_join(select(results, -source_doi),
                       by = c("reference" = "query")) %>%
    mutate(doi = coalesce(source_doi, doi), doi_source = case_when(
      !is.na(source_doi) ~ "source", !is.na(doi) ~ "crossref",
      TRUE ~ NA_character_), review = is.na(doi) & !is.na(candidate_doi))
}
