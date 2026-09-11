# Shared article identifier lookup; source after loading tidyverse and httr2.
# Network results are checkpointed separately from canonical dataset processing.

normalise_text <- function(x) {
  x %>% str_to_lower() %>% str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

title_overlap <- function(source_title, candidate_title) {
  if (is.na(source_title) || is.na(candidate_title)) return(NA_real_)
  words <- str_split(normalise_text(source_title), " ")[[1]]
  candidate <- str_split(normalise_text(candidate_title), " ")[[1]]
  words <- words[nchar(words) > 2]
  if (!length(words)) return(NA_real_)
  mean(words %in% candidate[nchar(candidate) > 2])
}

identifier_request <- function(req) {
  req %>%
    req_user_agent("BEAR identifier lookup (https://github.com/wwiecek/BEAR)") %>%
    req_timeout(30) %>%
    req_retry(max_tries = 3, max_seconds = 100, retry_on_failure = TRUE) %>%
    req_perform() %>% resp_body_json(simplifyVector = FALSE)
}

# Journal aliases are normalised for comparison, not written into source data.
normalise_journal <- function(x) {
  normalise_text(x) %>%
    str_replace("^the ", "") %>%
    str_replace("^aej ", "american economic journal ")
}

# With a journal, prefer consistent versions of record over search relevance.
# Without publication metadata, retain the historical best-candidate behaviour.
crossref_candidates <- function(items, source_title = NA_character_,
  source_journal = NA_character_, source_year = NA_integer_,
  source_author = NA_character_, source_doi_pattern = NA_character_) {
  if (!is.list(items)) stop("Malformed Crossref items")
  if (!length(items)) return(tibble(
    doi = NA_character_, crossref_score = NA_real_,
    candidate_title = NA_character_, title_overlap = NA_real_,
    candidate_journal = NA_character_, candidate_year = NA_integer_,
    candidate_type = NA_character_, candidate_author = NA_character_,
    journal_match = NA, year_match = NA, author_match = NA,
    prefix_match = NA, title_match = NA, status = "no_match"))
  candidates <- map_dfr(items, function(item) {
    if (!is.list(item) || length(item$DOI) != 1L ||
        length(item$score) != 1L || !is.numeric(item$score) ||
        is.na(item$score)) stop("Malformed Crossref candidate")
    tibble(
      doi = pluck(item, "DOI", .default = NA_character_),
      crossref_score = pluck(item, "score", .default = NA_real_),
      candidate_title = pluck(item, "title", 1, .default = NA_character_),
      candidate_journal = pluck(item, "container-title", 1,
                                .default = NA_character_),
      candidate_year = as.integer(pluck(item, "published", "date-parts",
                                       1, 1, .default = NA_integer_)),
      candidate_type = pluck(item, "type", .default = NA_character_),
      candidate_author = pluck(item, "author", 1, "family",
                               .default = NA_character_),
      year_match = if (is.na(source_year)) NA else source_year %in%
        map_int(c("published", "published-print", "published-online", "issued"),
          ~ as.integer(pluck(item, .x, "date-parts", 1, 1,
                             .default = NA_integer_)))
    )
  }) %>%
    mutate(title_overlap = map_dbl(candidate_title,
                                   ~ title_overlap(source_title, .x))) %>%
    mutate(
      journal_match = normalise_journal(candidate_journal) ==
        normalise_journal(source_journal),
      title_match = pmax(title_overlap,
        map_dbl(candidate_title, ~ title_overlap(.x, source_title)),
        na.rm = TRUE) >= 0.9,
      author_match = map_lgl(candidate_author, function(author) {
        if (is.na(source_author) || is.na(author)) return(NA)
        str_detect(paste0(" ", normalise_text(source_author), " "),
                   fixed(paste0(" ", normalise_text(author), " ")))
      }),
      prefix_match = if (is.na(source_doi_pattern)) NA else
        str_detect(str_to_lower(doi), source_doi_pattern))
  if (is.na(source_journal)) {
    return(candidates %>% slice_max(crossref_score, n = 1, with_ties = FALSE) %>%
      mutate(status = if_else(is.na(doi), "no_match", "matched")))
  }
  candidates %>%
    mutate(supported = coalesce(journal_match & title_match, FALSE) &
      candidate_type == "journal-article" & coalesce(year_match, TRUE) &
      coalesce(author_match, TRUE) & coalesce(prefix_match, TRUE)) %>%
    arrange(desc(supported), desc(journal_match & title_match),
            desc(journal_match), desc(title_match), desc(author_match),
            desc(year_match), desc(prefix_match), desc(crossref_score)) %>%
    slice_head(n = 1) %>%
    mutate(status = if_else(supported, "matched", "review")) %>%
    select(-supported)
}

lookup_crossref <- function(query, source_title = NA_character_,
  source_journal = NA_character_, source_year = NA_integer_,
  source_author = NA_character_, source_doi_pattern = NA_character_) {
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
                      source_year, source_author, source_doi_pattern)
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
              "source_doi_pattern")
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
        any(cache$lookup_version != 2L)) {
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
    Sys.sleep(0.25)
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
                        tibble(provider, lookup_version = 2L,
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
