# Shared article identifier lookup; source after loading tidyverse and httr2.
# Network results are checkpointed separately from canonical dataset processing.

# Preserve balanced parentheses and angle brackets in legacy DOI suffixes.
extract_doi <- function(x) {
  doi <- str_extract(x, regex("10\\.\\d{4,9}/[^\\s\"]+",
                              ignore_case = TRUE)) %>%
    str_remove("[.,;]+$")
  extra <- !is.na(doi) & str_count(doi, fixed(")")) >
    str_count(doi, fixed("(")) & str_ends(doi, fixed(")"))
  doi[extra] <- str_remove(doi[extra], "\\)$")
  extra <- !is.na(doi) & str_count(doi, fixed(">")) >
    str_count(doi, fixed("<")) & str_ends(doi, fixed(">"))
  doi[extra] <- str_remove(doi[extra], ">$")
  str_to_lower(doi)
}

# Europe PMC supports exact PMID queries in batches; cache every completed batch.
lookup_pmids <- function(pmids, cache_path, batch_size = 100L) {
  pmids <- unique(as.character(pmids[!is.na(pmids)]))
  stopifnot(all(str_detect(pmids, "^\\d+$")), batch_size <= 1000L)
  cache <- if (file.exists(cache_path)) readRDS(cache_path) else
    tibble(query = character(), doi = character(), status = character())
  stopifnot(!anyDuplicated(cache$query))
  remaining <- setdiff(pmids, cache$query[cache$status != "error"])
  batches <- split(remaining, ceiling(seq_along(remaining) / batch_size))
  for (batch in batches) {
    body <- request("https://www.ebi.ac.uk/europepmc/webservices/rest/search") %>%
      req_url_query(query = paste0("SRC:MED AND (",
        str_c("EXT_ID:", batch, collapse = " OR "), ")"),
        format = "json", pageSize = batch_size, synonym = "false") %>%
      identifier_request()
    if (is.null(body$hitCount)) stop("Missing Europe PMC hit count")
    found <- map_dfr(body$resultList$result, function(item) {
      stopifnot(identical(item$source, "MED"), item$id %in% batch)
      tibble(query = item$id, doi = pluck(item, "doi", .default = NA_character_),
             candidate_title = pluck(item, "title", .default = NA_character_))
    })
    if (!nrow(found)) found <- tibble(query = character(), doi = character(),
                                    candidate_title = character())
    stopifnot(nrow(found) == body$hitCount, !anyDuplicated(found$query))
    result <- tibble(query = batch) %>% left_join(found, by = "query") %>%
      mutate(doi = extract_doi(doi), provider = "pmid_to_doi",
        status = if_else(is.na(doi), "no_match", "matched"),
        retrieved_at = format(Sys.time(), tz = "UTC"))
    cache <- bind_rows(filter(cache, !query %in% batch), result)
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(cache, cache_path)
    message("PMIDs completed: ", sum(pmids %in% cache$query), " / ", length(pmids))
  }
  filter(cache, query %in% pmids)
}

repair_mojibake <- function(x) {
  x <- enc2utf8(x)
  str_replace_all(x, c(
    "Ã¢ÂÂ" = "'", "Ã¢ÂÂ" = "'", "Ã¢??" = "'",
    "â€™" = "'", "â€˜" = "'", "â€“" = "-", "â€”" = "-",
    "Ã©" = "é", "Ã¨" = "è", "Ã«" = "ë", "Ã¶" = "ö",
    "Ã¼" = "ü", "Ã±" = "ñ", "Ã§" = "ç", "Ã¥" = "å"))
}

normalise_text <- function(x) {
  x %>% repair_mojibake() %>%
    str_replace_all("&amp;", "&") %>%
    str_replace_all("&", " and ") %>%
    str_remove_all("</?[[:alnum:]]+[^>]*>") %>%
    str_remove("^(retracted|withdrawn|expression of concern):\\s*") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("\\bbehaviour\\b", "behavior") %>%
    str_replace_all("\\bbehavioural\\b", "behavioral") %>%
    str_replace_all(c("\\borganised\\b" = "organized",
                      "\\brecognised\\b" = "recognized",
                      "\\bstandardised\\b" = "standardized",
                      "\\brandomised\\b" = "randomized",
                      "\\bprioritised\\b" = "prioritized")) %>%
    str_replace_all("[^a-z0-9]", " ") %>% str_squish()
}

title_overlap <- function(source_title, candidate_title) {
  if (is.na(source_title) || is.na(candidate_title)) return(NA_real_)
  words <- str_split(normalise_text(source_title), " ")[[1]]
  candidate <- str_split(normalise_text(candidate_title), " ")[[1]]
  words <- words[nchar(words) > 2]
  candidate <- candidate[nchar(candidate) > 2]
  if (!length(words) || !length(candidate)) return(NA_real_)
  mean(words %in% candidate)
}

title_similarity <- function(source_title, candidate_title) {
  if (is.na(source_title) || is.na(candidate_title)) return(NA_real_)
  source <- normalise_text(source_title)
  candidate <- normalise_text(candidate_title)
  if (!nzchar(source) || !nzchar(candidate)) return(NA_real_)
  token_values <- c(title_overlap(source, candidate),
                    title_overlap(candidate, source))
  token <- if (all(is.na(token_values))) NA_real_ else
    max(token_values, na.rm = TRUE)
  compact <- str_remove_all(c(source, candidate), " ")
  character <- 1 - as.numeric(adist(compact[1], compact[2])) /
    max(nchar(compact))
  max(token, character)
}

identifier_request <- function(req) {
  req %>%
    req_user_agent("BEAR identifier lookup (https://github.com/wwiecek/BEAR)") %>%
    req_timeout(30) %>%
    req_retry(max_tries = 3, max_seconds = 100, retry_on_failure = TRUE) %>%
    req_perform() %>% resp_body_json(simplifyVector = FALSE)
}

# Keep below Crossref's public (1/s) and polite-pool (3/s) list-query limits.
crossref_delay <- function(mailto = Sys.getenv("CROSSREF_MAILTO")) {
  if (nzchar(mailto)) 0.4 else 1.1
}

# Journal aliases are normalised for comparison, not written into source data.
normalise_journal <- function(x) {
  normalise_text(x) %>%
    str_replace("^the ", "") %>%
    str_replace_all("\\bj\\b", "journal") %>%
    str_replace_all("\\bmed\\b", "medical") %>%
    str_replace_all("\\bres\\b", "research") %>%
    str_replace_all("\\bpsychiatry\\b", "psychiatry") %>%
    str_replace_all("\\bpsychol\\b", "psychology") %>%
    str_replace_all("\\bbehav\\b", "behavior") %>%
    str_replace_all("\\bther\\b", "therapy") %>%
    str_replace_all("\\bclin\\b", "clinical") %>%
    str_replace_all("\\bint\\b", "international") %>%
    str_replace_all("\\bbr\\b", "british") %>%
    str_replace("^aej\\b", "american economic journal") %>%
    str_replace("^journal dev econ\\b", "journal development economics") %>%
    str_replace("^journal econ\\b", "journal economic") %>%
    str_replace("^journal hum res\\b", "journal human resources") %>%
    str_replace("^journal public econ\\b", "journal public economics") %>%
    str_replace("^econ journal\\b", "economic journal") %>%
    str_replace("^q journal econ\\b", "quarterly journal economics") %>%
    str_replace_all("\\bof\\b", " ") %>% str_squish()
}

crossref_years <- function(item) {
  map_int(c("published", "published-print", "published-online", "issued"),
          ~ as.integer(pluck(item, .x, "date-parts", 1, 1,
                             .default = NA_integer_))) %>% unique()
}

author_names <- function(item) {
  authors <- pluck(item, "author", .default = list())
  if (!length(authors)) return(character())
  map_chr(authors, function(author) {
    str_squish(str_c(pluck(author, "given", .default = ""),
                     pluck(author, "family", .default = ""), sep = " "))
  })
}

author_matches <- function(source, candidates) {
  if (is.na(source) || !nzchar(source) || !length(candidates)) return(NA)
  source_words <- str_split(normalise_text(source), " ")[[1]]
  any(map_lgl(candidates, function(candidate) {
    candidate_words <- str_split(normalise_text(candidate), " ")[[1]]
    length(candidate_words) > 0 &&
      last(candidate_words) %in% source_words
  }))
}

# With a journal, prefer consistent versions of record over search relevance.
# Without publication metadata, retain the historical best-candidate behaviour.
crossref_candidates <- function(items, source_title = NA_character_,
  source_journal = NA_character_, source_year = NA_integer_,
  source_author = NA_character_, source_doi_pattern = NA_character_,
  source_doi = NA_character_) {
  source_year <- as.integer(source_year)
  if (!is.list(items)) stop("Malformed Crossref items")
  if (!length(items)) return(tibble(
    doi = NA_character_, crossref_score = NA_real_,
    candidate_title = NA_character_, title_overlap = NA_real_,
    candidate_journal = NA_character_, candidate_year = NA_integer_,
    candidate_type = NA_character_, candidate_author = NA_character_,
    candidate_authors = NA_character_, title_similarity = NA_real_,
    candidate_years = NA_character_, year_distance = NA_integer_,
    journal_match = NA, year_match = NA, author_match = NA,
    prefix_match = NA, title_match = NA, decision = "no_match",
    status = "no_match"))
  candidates <- map_dfr(items, function(item) {
    if (!is.list(item) || length(item$DOI) != 1L ||
        length(item$score) != 1L || !is.numeric(item$score) ||
        is.na(item$score)) stop("Malformed Crossref candidate")
    authors <- author_names(item)
    years <- crossref_years(item)
    tibble(
      doi = pluck(item, "DOI", .default = NA_character_),
      crossref_score = pluck(item, "score", .default = NA_real_),
      candidate_title = pluck(item, "title", 1, .default = NA_character_) %>%
        str_replace_all("&amp;", "&") %>%
        str_remove_all("</?[[:alnum:]]+[^>]*>"),
      candidate_journal = pluck(item, "container-title", 1,
                                .default = NA_character_),
      candidate_year = as.integer(pluck(item, "published", "date-parts",
                                       1, 1, .default = NA_integer_)),
      candidate_type = pluck(item, "type", .default = NA_character_),
      candidate_author = if (length(authors)) authors[[1]] else NA_character_,
      candidate_authors = str_c(authors, collapse = "; "),
      candidate_years = str_c(years[!is.na(years)], collapse = ";"),
      year_distance = if (is.na(source_year) || !length(years) ||
                          all(is.na(years))) NA_integer_ else
        min(abs(source_year - years), na.rm = TRUE)
    )
  }) %>%
    mutate(title_overlap = map_dbl(candidate_title,
                                   ~ title_overlap(source_title, .x)),
           title_similarity = map_dbl(candidate_title,
                                      ~ title_similarity(source_title, .x))) %>%
    mutate(
      journal_match = normalise_journal(candidate_journal) ==
        normalise_journal(source_journal),
      title_match = title_similarity >= 0.88,
      author_match = map_lgl(seq_len(n()), ~ author_matches(
        source_author, str_split(candidate_authors[.x], fixed("; "))[[1]])),
      year_match = if (is.na(source_year)) rep(NA, n()) else
        year_distance == 0 | (candidate_type == "journal-article" &
          coalesce(journal_match, FALSE) & coalesce(title_match, FALSE) &
          year_distance <= 1),
      prefix_match = if (is.na(source_doi_pattern)) NA else
        str_detect(str_to_lower(doi), source_doi_pattern))
  if (is.na(source_journal)) {
    return(candidates %>% slice_max(crossref_score, n = 1, with_ties = FALSE) %>%
      mutate(decision = if_else(is.na(doi), "no_match", "auto_accept"),
             status = if_else(decision == "auto_accept", "matched", decision)))
  }
  candidates %>%
    mutate(bibliographic_match = coalesce(journal_match, FALSE) &
      coalesce(title_match, FALSE) & candidate_type == "journal-article" &
      coalesce(year_match, FALSE),
      decision = case_when(
        bibliographic_match ~ "auto_accept",
        candidate_type != "journal-article" ~ "no_match",
        !coalesce(journal_match, FALSE) ~ "no_match",
        !coalesce(title_match, FALSE) ~ "no_match",
        TRUE ~ "review")) %>%
    arrange(desc(bibliographic_match), desc(journal_match),
            desc(title_similarity), desc(title_match), desc(year_match),
            desc(author_match),
            desc(prefix_match), desc(crossref_score)) %>%
    slice_head(n = 1) %>%
    mutate(status = if_else(str_detect(decision, "auto_accept"), "matched", decision)) %>%
    select(-bibliographic_match)
}

lookup_crossref <- function(query, source_title = NA_character_,
  source_journal = NA_character_, source_year = NA_integer_,
  source_author = NA_character_, source_doi_pattern = NA_character_,
  source_doi = NA_character_) {
  req <- request("https://api.crossref.org/works") %>%
    req_url_query(`query.bibliographic` = query,
                  rows = if (is.na(source_journal)) 3 else 20)
  if (!is.na(source_journal)) {
    req <- req %>% req_url_query(filter = "type:journal-article",
      `query.container-title` = str_replace(source_journal, "^AEJ:",
                                             "American Economic Journal:"))
  }
  mailto <- Sys.getenv("CROSSREF_MAILTO")
  if (nzchar(mailto)) req <- req %>% req_url_query(mailto = mailto)
  body <- identifier_request(req)
  if (is.null(body$message$items)) stop("Missing Crossref items")
  crossref_candidates(body$message$items, source_title, source_journal,
                      source_year, source_author, source_doi_pattern,
                      source_doi)
}

# Use the same first-result MED rule as the historical Head lookup.
epmc_identifier <- function(results, from) {
  if (!is.list(results)) stop("Malformed Europe PMC results")
  item <- if (length(results)) results[[1]] else list()
  med <- identical(item$source, "MED")
  value <- if (med) {
    if (from == "doi") item$id else item$doi
  } else NULL
  value <- if (is.null(value) || !nzchar(value)) NA_character_ else value
  column <- if (from == "doi") "pmid" else "doi"
  tibble(!!column := value,
         status = if_else(is.na(value), "no_match", "matched"))
}

lookup_epmc <- function(identifier, from = c("doi", "pmid")) {
  from <- match.arg(from)
  query <- if (from == "doi") paste0("DOI:", identifier) else
    paste0("EXT_ID:", identifier, " AND SRC:MED")
  body <- request("https://www.ebi.ac.uk/europepmc/webservices/rest/search") %>%
    req_url_query(query = query, format = "json", pageSize = 1,
                  synonym = "false") %>% identifier_request()
  if (is.null(body$hitCount)) stop("Missing Europe PMC hit count")
  results <- body$resultList$result
  if (body$hitCount > 0 && is.null(results)) stop("Missing Europe PMC results")
  epmc_identifier(if (is.null(results)) list() else results, from)
}

# Input has query and optionally source_title. Errors remain retryable on resume.
# Refreshes belong in a different cache: never overwrite established mappings.
lookup_identifiers <- function(papers, cache_path, provider = "crossref") {
  stopifnot(provider %in% c("crossref", "doi_to_pmid", "pmid_to_doi"))
  fields <- c("source_title", "source_journal", "source_year", "source_author",
              "source_doi_pattern", "source_doi")
  papers <- papers %>% select(query, any_of(fields)) %>% distinct()
  for (field in setdiff(fields, names(papers))) papers[[field]] <- NA_character_
  papers <- papers %>% filter(!is.na(query), nzchar(query))
  stopifnot(!anyDuplicated(papers$query))
  cache <- if (file.exists(cache_path)) readRDS(cache_path) else tibble(
    query = character(), source_title = character(), provider = character(),
    status = character())
  stopifnot(!anyDuplicated(cache$query), all(cache$provider == provider))
  if (provider == "crossref" && nrow(cache) &&
      any(!is.na(papers$source_journal))) {
    if (!all(c(fields, "lookup_version") %in% names(cache)) ||
        any(cache$lookup_version != 3L)) {
      stop("Journal-aware lookup requires a new cache path")
    }
    # A query cannot silently reuse results scored against different metadata.
    common <- inner_join(papers, cache, by = "query", suffix = c("", "_old"))
    for (field in fields) {
      if (!identical(as.character(common[[field]]),
                     as.character(common[[paste0(field, "_old")]]))) {
        stop("Lookup metadata changed; use a new cache path")
      }
    }
  }
  done <- cache$query[cache$status != "error"]
  remaining <- papers %>% filter(!query %in% done)
  for (i in seq_len(nrow(remaining))) {
    query <- remaining$query[i]
    source_title <- remaining$source_title[i]
    Sys.sleep(if (provider == "crossref") crossref_delay() else 0.25)
    result <- tryCatch({
      if (provider == "crossref") {
        args <- as.list(remaining[i, c("query", fields)])
        # Preserve the two-argument interface for metadata-free lookups.
        if (is.na(args$source_journal)) args <- args[c("query", "source_title")]
        do.call(lookup_crossref, args)
      } else
        lookup_epmc(query, if (provider == "doi_to_pmid") "doi" else "pmid")
    }, error = function(e) {
      empty <- if (provider == "crossref") crossref_candidates(list()) else
        epmc_identifier(list(), if (provider == "doi_to_pmid") "doi" else "pmid")
      mutate(empty, status = "error", error = conditionMessage(e))
    })
    result <- bind_cols(remaining[i, c("query", fields)],
                        tibble(provider, lookup_version = 3L,
                               retrieved_at = format(Sys.time(), tz = "UTC")),
                        result)
    cache <- bind_rows(filter(cache, .data$query != .env$query), result)
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(cache, cache_path)
    if (i %% 25 == 0) message("Looked up ", i, " / ", nrow(remaining))
  }
  semi_join(cache, papers, by = "query")
}

# Join only enrichment columns, without altering existing row values or order.
join_identifiers <- function(data, lookup, by, columns) {
  lookup <- lookup %>% select(all_of(c(by, columns))) %>% distinct()
  if (anyDuplicated(lookup[by])) stop("Identifier lookup keys are not unique")
  overlap <- intersect(columns, names(data))
  original <- data
  result <- data %>% select(-any_of(overlap)) %>%
    left_join(lookup, by = by, relationship = "many-to-one")
  if (nrow(result) != nrow(original)) stop("Identifier join changed row count")
  # Joins may coerce key types (e.g. integer years read from CSV as doubles).
  unchanged <- setdiff(names(original), overlap)
  result[unchanged] <- original[unchanged]
  for (column in overlap) {
    old <- original[[column]]
    conflict <- !is.na(old) & !is.na(result[[column]]) &
      old != result[[column]]
    if (any(conflict)) stop("Identifier join conflicts with existing ", column)
    result[[column]] <- coalesce(old, result[[column]])
  }
  result %>% select(all_of(names(original)), any_of(columns))
}
