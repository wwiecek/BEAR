# Cache source article references and resolve their cited DOIs in batches.
# Run from the BEAR project root before the study DOI review stage.

library(tidyverse)
library(httr2)
source("R/doi_lookup.R")

cache_path <- "doi/ArelBundock/derived/collection_works.rds"
reference_path <- "doi/ArelBundock/derived/openalex_reference_works.rds"
dir.create("doi/ArelBundock/derived", recursive = TRUE,
           showWarnings = FALSE)

data <- readRDS("data/ArelBundock.rds")
sources <- data %>% distinct(meta_id, doi_collection)
briggs <- data %>% filter(sample == "Briggs") %>%
  distinct(meta_id, doi_collection)
stopifnot(nrow(sources) == 46L, nrow(briggs) == 33L,
          !anyNA(briggs$doi_collection))

get_work <- function(doi) {
  request(paste0("https://api.crossref.org/works/",
                 URLencode(doi, reserved = TRUE))) %>%
    req_user_agent("BEAR lookup (https://github.com/wwiecek/BEAR)") %>%
    req_timeout(30) %>%
    req_retry(max_tries = 3, max_seconds = 100,
              retry_on_failure = TRUE) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = FALSE) %>%
    pluck("message")
}

collections <- if (file.exists(cache_path)) readRDS(cache_path) else list()
for (doi in na.omit(sources$doi_collection)) {
  if (!is.null(collections[[doi]])) next
  Sys.sleep(crossref_delay())
  collections[[doi]] <- tryCatch(get_work(doi), error = function(e)
    list(error = conditionMessage(e)))
  saveRDS(collections, cache_path)
}

source_works <- collections[briggs$doi_collection]
stopifnot(all(map_lgl(source_works, ~ is.null(.x$error))))
reference_dois <- source_works %>%
  map(~ map_chr(.x$reference, ~ pluck(.x, "DOI", .default = NA_character_))) %>%
  unlist(use.names = FALSE) %>%
  discard(is.na) %>%
  str_to_lower() %>%
  unique()

references <- if (file.exists(reference_path)) readRDS(reference_path) else
  list()
remaining <- setdiff(reference_dois, names(references))
batches <- split(remaining, ceiling(seq_along(remaining) / 40L))
for (i in seq_along(batches)) {
  batch <- batches[[i]]
  body <- request("https://api.openalex.org/works") %>%
    req_url_query(filter = paste0("doi:", str_c("https://doi.org/",
      batch, collapse = "|")), `per-page` = 200) %>%
    req_user_agent("BEAR lookup (https://github.com/wwiecek/BEAR)") %>%
    req_timeout(60) %>%
    req_retry(max_tries = 3, max_seconds = 100,
              retry_on_failure = TRUE) %>%
    req_perform() %>% resp_body_json(simplifyVector = FALSE)
  found <- body$results
  found <- set_names(found, map_chr(found, ~ str_to_lower(str_remove(
    .x$doi, fixed("https://doi.org/")))))
  for (doi in batch) references[doi] <- list(if (is.null(found[[doi]]))
    list(status = "not_found") else found[[doi]])
  saveRDS(references, reference_path)
  message("Reference batches checked: ", i, " / ", length(batches))
  Sys.sleep(0.5)
}
