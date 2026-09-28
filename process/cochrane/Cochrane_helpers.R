# Helpers shared by the Cochrane download and processing scripts.

id_from_doi <- function(doi) {
  stringr::str_extract(stringr::str_to_upper(doi), "CD[0-9]{6}")
}

file_from_id <- function(id) {
  paste0(id, "StatsDataOnly.rm5")
}

file_from_doi <- function(doi) {
  file_from_id(id_from_doi(doi))
}

# Read review-level provenance from the opening tag of a saved RM5 file.
rm5_header <- function(file, rm5_dir) {
  path <- fs::path(rm5_dir, file)
  if (!fs::file_exists(path)) stop("Missing Cochrane RM5 file: ", path)
  con <- base::file(path, "rb")
  on.exit(close(con))
  header <- ""
  repeat {
    chunk <- readBin(con, "raw", n = 8192L)
    if (length(chunk) == 0L) break
    header <- paste0(header, rawToChar(chunk))
    if (stringr::str_detect(header, "<COCHRANE_REVIEW\\b[^>]*>")) break
    if (nchar(header) > 1000000L) break
  }
  tag <- stringr::str_extract(header, "<COCHRANE_REVIEW\\b[^>]*>")
  if (is.na(tag)) stop("Missing COCHRANE_REVIEW tag in: ", path)
  attribute <- function(name) {
    value <- stringr::str_match(
      tag, paste0("\\b", name, '="([^"]*)"')
    )[, 2]
    if (is.na(value) || value == "") NA_character_ else value
  }
  tibble::tibble(file = file, source_doi = attribute("DOI"),
                 status = attribute("STATUS"), type = attribute("TYPE"),
                 group_id = attribute("GROUP_ID"))
}

# Read the minimum DOI manifest and standardize optional review annotations.
manifest_from_csv <- function(path) {
  manifest <- readr::read_csv(path, show_col_types = FALSE)
  doi_col <- intersect(c("DOI", "doi"), names(manifest))

  if (length(doi_col) == 0L) {
    stop("Cochrane manifest must contain a DOI column named DOI or doi.")
  }

  id_col <- intersect(
    c("Cochrane Review ID", "cochrane_id", "id"),
    names(manifest)
  )
  abstract_col <- intersect(c("Abstract", "abstract"), names(manifest))
  specialty_col <- intersect(
    c("Cochrane Review Group Code", "specialty"),
    names(manifest)
  )

  doi <- stringr::str_squish(as.character(manifest[[doi_col[[1]]]]))
  supplied_id <- if (length(id_col) > 0L) {
    id_from_doi(as.character(manifest[[id_col[[1]]]]))
  } else {
    rep(NA_character_, nrow(manifest))
  }

  tibble::tibble(
    cochrane_id = dplyr::coalesce(supplied_id, id_from_doi(doi)),
    doi = dplyr::na_if(doi, ""),
    abstract = if (length(abstract_col) > 0L) {
      as.character(manifest[[abstract_col[[1]]]])
    } else {
      rep(NA_character_, nrow(manifest))
    },
    specialty = if (length(specialty_col) > 0L) {
      as.character(manifest[[specialty_col[[1]]]])
    } else {
      rep(NA_character_, nrow(manifest))
    }
  ) %>%
    dplyr::filter(!is.na(doi), !is.na(cochrane_id)) %>%
    dplyr::distinct(doi, .keep_all = TRUE)
}

# Build a processing manifest when only previously downloaded RM5 files exist.
manifest_from_rm5 <- function(rm5_dir) {
  if (!fs::dir_exists(rm5_dir)) {
    return(tibble::tibble(
      cochrane_id = character(), doi = character(),
      abstract = character(), specialty = character(),
      file = character(), path = character()
    ))
  }

  paths <- fs::dir_ls(
    rm5_dir,
    regexp = "CD[0-9]{6}StatsDataOnly[.]rm5$",
    type = "file",
    fail = FALSE
  )
  files <- fs::path_file(paths)

  tibble::tibble(
    cochrane_id = id_from_doi(files),
    doi = NA_character_,
    abstract = NA_character_,
    specialty = NA_character_,
    file = files,
    path = as.character(paths)
  ) %>%
    dplyr::filter(!is.na(cochrane_id)) %>%
    dplyr::distinct(file, .keep_all = TRUE)
}

# Keep a stable checkpoint schema for successful and failed review reads.
empty_cochrane_results <- function() {
  tibble::tibble(
    cochrane_id = character(), id = character(), doi = character(),
    file = character(), studies = list(), ma = list(),
    ok = logical(), error = character(), failure_reason = character()
  )
}

classify_failure_reason <- function(error) {
  if (is.na(error) || !nzchar(error)) return(NA_character_)

  if (stringr::str_detect(
    error, stringr::regex("Too Many Requests|HTTP 429", ignore_case = TRUE)
  )) {
    return("rate_limited")
  }

  if (stringr::str_detect(error, stringr::regex("HTTP 404", ignore_case = TRUE))) {
    return("review_not_found")
  }

  if (stringr::str_detect(
    error,
    stringr::regex(
      "could not open html session|cloudflare|\\b403\\b", ignore_case = TRUE
    )
  )) {
    return("access_blocked")
  }

  if (stringr::str_detect(
    error,
    stringr::regex(
      "RM5 file not found after download|could not download data",
      ignore_case = TRUE
    )
  )) {
    return("download_unavailable")
  }

  if (stringr::str_detect(error, stringr::regex("File is not XML", TRUE))) {
    return("html_or_invalid_rm5")
  }

  "parse_error"
}

# Read one RM5 file while preserving failures as checkpoint rows.
read_rm5_one <- function(file, rm5_dir, doi = NA_character_,
                         cochrane_id = id_from_doi(file)) {
  tryCatch(
    {
      review <- cochrane::read.review(file = file, path = rm5_dir)
      tibble::tibble(
        cochrane_id = cochrane_id,
        id = id_from_doi(file),
        doi = doi,
        file = file,
        studies = list(review[[1]]),
        ma = list(review[[2]]),
        ok = TRUE,
        error = NA_character_,
        failure_reason = NA_character_
      )
    },
    error = function(e) {
      tibble::tibble(
        cochrane_id = cochrane_id,
        id = id_from_doi(file),
        doi = doi,
        file = file,
        studies = list(NULL),
        ma = list(NULL),
        ok = FALSE,
        error = conditionMessage(e),
        failure_reason = classify_failure_reason(conditionMessage(e))
      )
    }
  )
}
