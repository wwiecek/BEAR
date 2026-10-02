# Match Briggs study labels to references cited by their source articles.
# Keep citation evidence and candidate decisions separate from processing.

library(tidyverse)
source("R/doi_lookup.R")

collections <- readRDS("doi/ArelBundock/derived/collection_works.rds")
works <- readRDS("doi/ArelBundock/derived/openalex_reference_works.rds")
briggs <- readRDS("data/ArelBundock.rds") %>%
  filter(sample == "Briggs") %>%
  distinct(meta_id, study_id, study_year, study_journal,
           doi_collection)
stopifnot(n_distinct(briggs$meta_id) == 33L,
          !anyDuplicated(briggs[c("meta_id", "study_id", "study_year",
                                   "study_journal")]))
sources <- briggs %>% distinct(meta_id, doi_collection)

# Publisher references may contain only a DOI; registry fields are kept apart.
references <- map_dfr(seq_len(nrow(sources)), function(i) {
  source <- sources %>% slice(i)
  cited <- collections[[source$doi_collection]]$reference
  map_dfr(seq_along(cited), function(j) {
    ref <- cited[[j]]
    doi <- str_to_lower(pluck(ref, "DOI", .default = NA_character_))
    item <- if (!is.na(doi)) works[[doi]] else NULL
    if (!is.null(item$status)) item <- NULL
    author <- if (is.null(item)) NA_character_ else
      pluck(item, "authorships", 1, "author", "display_name",
            .default = NA_character_)
    surname <- if (is.na(author)) NA_character_ else
      word(author, -1)
    tibble(meta_id = source$meta_id, doi_collection = source$doi_collection,
      reference_index = j,
      reference_key = pluck(ref, "key", .default = NA_character_),
      reference_doi_asserted_by = pluck(ref, "doi-asserted-by",
        .default = NA_character_),
      reference_text = pluck(ref, "unstructured", .default = NA_character_),
      reference_title = pluck(ref, "article-title", .default = NA_character_),
      reference_author = pluck(ref, "author", .default = NA_character_),
      reference_year = suppressWarnings(as.integer(pluck(ref, "year",
        .default = NA_character_))),
      reference_journal = pluck(ref, "journal-title", .default = NA_character_),
      reference_doi = doi,
      registry_title = if (is.null(item)) NA_character_ else
        pluck(item, "display_name", .default = NA_character_),
      registry_author = author, registry_surname = surname,
      registry_years = if (is.null(item)) "" else
        as.character(pluck(item, "publication_year", .default = NA_integer_)),
      registry_journal = if (is.null(item)) NA_character_ else
        pluck(item, "primary_location", "source", "display_name",
              .default = NA_character_),
      registry_type = if (is.null(item)) NA_character_ else
        pluck(item, "type", .default = NA_character_))
  })
})
stopifnot(!anyDuplicated(references[c("meta_id", "reference_index")]))
write_csv(references, "doi/ArelBundock/derived/study_references.csv",
          na = "")

keys <- briggs %>% mutate(study_year = as.integer(study_year))
candidates <- keys %>% inner_join(references, by = c("meta_id",
  "doi_collection"), relationship = "many-to-many") %>%
  filter(!is.na(reference_doi), !is.na(study_id), study_id != "NA",
         !str_detect(study_id, "^[0-9]+$"),
         !is.na(study_year), !is.na(registry_surname)) %>%
  mutate(year_match = (!is.na(reference_year) &
           study_year == reference_year) |
           study_year == suppressWarnings(as.integer(registry_years))) %>%
  filter(year_match) %>%
  mutate(author_match = str_detect(normalise_text(study_id),
    paste0("^", normalise_text(registry_surname), "\\b"))) %>%
  filter(author_match) %>%
  mutate(
    journal_match = if_else(is.na(study_journal) | study_journal == "NA", NA,
      normalise_journal(study_journal) ==
        normalise_journal(registry_journal)),
    title_similarity = map2_dbl(reference_title, registry_title,
                                title_similarity),
    title_match = if_else(is.na(reference_title), NA,
                          title_similarity >= 0.88),
    plausible = author_match & year_match &
      coalesce(journal_match, TRUE) & coalesce(title_match, TRUE)) %>%
  filter(plausible)

key_columns <- c("meta_id", "study_id", "study_year", "study_journal")
choices <- candidates %>%
  group_by(across(all_of(key_columns))) %>%
  mutate(candidate_dois = n_distinct(reference_doi),
    decision = case_when(
      candidate_dois > 1L ~ "review_multiple",
      registry_type != "article" ~ "review_publication_type",
      is.na(reference_year) & !str_detect(registry_years,
        paste0("(^|;)", study_year, "($|;)")) ~ "review_year",
      TRUE ~ "accept")) %>%
  ungroup()
write_csv(choices, "doi/ArelBundock/derived/study_candidates.csv",
          na = "")

mapping <- choices %>% filter(decision == "accept") %>%
  select(all_of(key_columns), doi_study = reference_doi) %>% distinct()
additional_path <- "doi/ArelBundock/final/references_without_doi_mapping.csv"
if (file.exists(additional_path)) {
  additional <- read_csv(additional_path, show_col_types = FALSE)
  mapping <- bind_rows(mapping, additional)
}
stopifnot(!anyDuplicated(mapping[key_columns]))
write_csv(mapping, "doi/ArelBundock/final/study_doi_mapping.csv",
          na = "NA")
print(choices %>% count(decision))
print(tibble(source_keys = nrow(keys), doi_keys = nrow(mapping),
             doi_rows = nrow(keys %>% inner_join(mapping, by = key_columns,
               relationship = "one-to-one"))))

source_data <- readRDS("data/ArelBundock.rds")
article_info <- read_csv("doi/ArelBundock/article_metadata.csv",
                         show_col_types = FALSE) %>%
  select(meta_id, article_title)
inventory <- source_data %>% distinct(meta_id, sample, doi_collection) %>%
  group_by(meta_id, doi_collection) %>%
  summarise(sample = str_c(sort(unique(sample)), collapse = "; "),
            .groups = "drop") %>%
  left_join(article_info, by = "meta_id") %>%
  mutate(publisher_references = map_int(doi_collection, ~ {
    if (is.na(.x)) return(NA_integer_)
    item <- collections[[.x]]
    if (is.null(item)) NA_integer_ else length(item$reference)
  }),
  publisher_reference_dois = map_int(doi_collection, ~ {
    if (is.na(.x)) return(NA_integer_)
    item <- collections[[.x]]
    if (is.null(item)) return(NA_integer_)
    sum(map_lgl(item$reference, ~ !is.null(.x$DOI)))
  }))
stopifnot(nrow(inventory) == 46L)
write_csv(inventory, "doi/ArelBundock/derived/source_inventory.csv",
          na = "")
