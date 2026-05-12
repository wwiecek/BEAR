# Draft DOI lookup for Lang papers using Crossref metadata.
# This is an audit/enrichment helper, not part of the canonical Lang processing.

library(tidyverse)
library(httr2)

input_path <- "data/Lang.rds"
output_path <- "data_raw/Lang/derived/lang_doi_candidates.csv"
crossref_mailto <- Sys.getenv("CROSSREF_MAILTO", unset = NA_character_)
lookup_keys <- c("paper_id", "source_title", "citation", "journal", "year",
                 "lang_source")

lang_papers <- readRDS(input_path) %>%
  distinct(paper_id, source_title, citation, journal, year, lang_source) %>%
  mutate(
    query = case_when(
      !is.na(source_title) ~ str_c(source_title, journal, year, sep = " "),
      !is.na(citation) ~ citation,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(query))

normalise_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

title_overlap <- function(source_title, candidate_title) {
  if (is.na(source_title) || is.na(candidate_title)) return(NA_real_)
  source_words <- str_split(normalise_text(source_title), " ")[[1]]
  candidate_words <- str_split(normalise_text(candidate_title), " ")[[1]]
  source_words <- source_words[nchar(source_words) > 2]
  candidate_words <- candidate_words[nchar(candidate_words) > 2]
  if (length(source_words) == 0) return(NA_real_)
  mean(source_words %in% candidate_words)
}

lookup_crossref <- function(query, source_title = NA_character_) {
  req <- request("https://api.crossref.org/works") %>%
    req_url_query(`query.bibliographic` = query, rows = 3)

  if (!is.na(crossref_mailto) && nzchar(crossref_mailto)) {
    req <- req %>% req_url_query(mailto = crossref_mailto)
  }

  resp <- req %>%
    req_user_agent("BEAR DOI lookup (https://github.com/wwiecek/BEAR)") %>%
    req_timeout(30) %>%
    req_perform()

  items <- resp %>%
    resp_body_json(simplifyVector = TRUE) %>%
    pluck("message", "items")

  if (length(items) == 0 || nrow(items) == 0) {
    return(tibble(doi = NA_character_, crossref_score = NA_real_,
                  candidate_title = NA_character_, title_overlap = NA_real_))
  }

  tibble(
    doi = items$DOI,
    crossref_score = items$score,
    candidate_title = map_chr(items$title, ~if (length(.x)) .x[[1]] else NA_character_)
  ) %>%
    mutate(title_overlap = map2_dbl(rep(source_title, n()),
                                    candidate_title, title_overlap)) %>%
    slice_max(order_by = crossref_score, n = 1, with_ties = FALSE)
}

doi_candidates <- lang_papers %>%
  mutate(
    result = pmap(
      list(query, source_title),
      ~{
        Sys.sleep(0.2)
        tryCatch(
          lookup_crossref(..1, ..2),
          error = function(e) tibble(
            doi = NA_character_,
            crossref_score = NA_real_,
            candidate_title = NA_character_,
            title_overlap = NA_real_,
            error = conditionMessage(e)
          )
        )
      }
    )
  ) %>%
  unnest(result)

write_csv(doi_candidates, output_path)

# Add the accepted Crossref DOI candidate back to the Lang row-level data.
if (any(is.na(doi_candidates$doi))) {
  stop("DOI lookup produced missing DOI candidates; review ",
       output_path, " before updating ", input_path, call. = FALSE)
}

doi_lookup <- doi_candidates %>%
  distinct(across(all_of(lookup_keys)), doi)

if (nrow(doi_lookup) != nrow(doi_candidates)) {
  stop("DOI candidate keys are not unique; review ", output_path,
       " before updating ", input_path, call. = FALSE)
}

lang <- readRDS(input_path)
lang_nrow <- nrow(lang)

lang_with_doi <- lang %>%
  select(-any_of("doi")) %>%
  left_join(doi_lookup, by = lookup_keys)

if (nrow(lang_with_doi) != lang_nrow) {
  stop("DOI join changed Lang row count from ", lang_nrow, " to ",
       nrow(lang_with_doi), call. = FALSE)
}
if (any(is.na(lang_with_doi$doi))) {
  stop("DOI join left ", sum(is.na(lang_with_doi$doi)),
       " Lang rows without DOI; review join keys before saving.",
       call. = FALSE)
}

saveRDS(lang_with_doi, input_path)

cat("Updated", input_path, "with DOI values for",
    n_distinct(lang_with_doi$doi), "source articles across",
    nrow(lang_with_doi), "Lang tests.\n")
