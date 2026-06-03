# Process Sladekova et al. source meta-analytic datasets into BEAR inputs.
# Study IDs are approximate, conservative within-meta-analysis groupings based
# on descriptive source fields rather than raw row/sample counters alone.

library(tidyverse)

raw_files <- list.files("data_raw/Sladekova/data", full.names = TRUE)
dfs <- lapply(raw_files, read.csv)
names(dfs) <- basename(raw_files)

# sort(table(unlist(lapply(dfs, function(f) names(f)))), decreasing = TRUE)

best_col <- function(df, candidates) {
  candidates <- intersect(candidates, names(df))
  if(length(candidates) == 0) return(NA_character_)
  nonmissing <- sapply(candidates, function(col) {
    sum(!is.na(df[[col]]) & df[[col]] != "")
  })
  if(max(nonmissing) == 0) return(NA_character_)
  candidates[which.max(nonmissing)]
}

clean_text <- function(x) {
  x <- stringr::str_squish(as.character(x))
  dplyr::na_if(x, "")
}

normalize_study_text <- function(x) {
  x <- stringr::str_to_lower(clean_text(x))
  x <- stringr::str_replace_all(x, "\\[[0-9]+\\]", "")
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", " ")
  x <- stringr::str_squish(x)
  dplyr::na_if(x, "")
}

extract_year <- function(x) {
  year <- stringr::str_extract(as.character(x), "(18|19|20)[0-9]{2}")
  suppressWarnings(as.numeric(year))
}

raw_id_cols <- c("cluster", "study", "Study", "Study.N", "StudyID",
                 "Study_ID", "study_no", "id_sample", "id_document",
                 "sample_id", "id", "ID", "Sample..", "X1", "X")
study_label_cols <- c("author", "Author", "authors", "Authors", "Study.name",
                      "Stady.name", "Study.Author", "Source", "sample",
                      "Sample_Description")
year_cols <- c("year", "Year", "Study.Year", "pub_year", "pub_year_full",
               "Publication_year", "std_year", "std_pub_year", "year_full")

onedf <- lapply(dfs, function(df) {
  
  if(all(c("n_treatment", "n_control") %in% colnames(df))) {
    df$n <- df$n_treatment + df$n_control
  }
  
  ret <- df %>% 
    dplyr::filter(!is.na(yi) & !is.na(vi)) %>% 
    # Keep boundary correlations; they yield signed infinite Fisher's z values.
    dplyr::filter((1 + yi)/(1 - yi) >= 0) %>% 
    dplyr::mutate(
      b = 0.5*log((1 + yi)/(1 - yi)),
      se = sqrt(vi)) 
  
  # Trying to count sample sizes where possible
  
  if(nrow(ret) == 0)
    return(data.frame())
  
  # Reconstruct study IDs within each imported meta-analysis. The source
  # `study_id` is just the file suffix. Raw numeric/source IDs are retained for
  # inspection, but only DOI-like or descriptive fields are allowed to group
  # rows; otherwise rows receive unique conservative identifiers.
  doi_col <- best_col(ret, grep("doi", names(ret), ignore.case = TRUE,
                                value = TRUE))
  raw_id_col <- best_col(ret, raw_id_cols)
  label_col <- best_col(ret, study_label_cols)
  year_col <- best_col(ret, year_cols)
  
  doi <- if(is.na(doi_col)) NA_character_ else clean_text(ret[[doi_col]])
  doi <- stringr::str_extract(doi, "10\\.[0-9]{4,9}/[^[:space:]]+")
  doi <- normalize_study_text(doi)
  
  ret$study_label <- if(is.na(label_col)) {
    NA_character_
  } else {
    clean_text(ret[[label_col]])
  }
  ret$study_label_norm <- normalize_study_text(ret$study_label)
  ret$study_label_norm[stringr::str_detect(ret$study_label_norm, "^[0-9]+$")] <-
    NA_character_
  
  ret$study_year <- if(is.na(year_col)) NA_real_ else extract_year(ret[[year_col]])
  descriptive_id <- ifelse(
    !is.na(ret$study_label_norm) & !is.na(ret$study_year),
    paste(ret$study_label_norm, ret$study_year, sep = "_"),
    ret$study_label_norm
  )
  row_unique_id <- paste0("row_", stringr::str_pad(seq_len(nrow(ret)), 5, pad = "0"))
  
  ret$studyid <- dplyr::case_when(
    !is.na(doi) ~ paste0("doi_", doi),
    !is.na(descriptive_id) ~ paste0("label_", descriptive_id),
    TRUE ~ row_unique_id
  )
  ret$studyid_basis <- dplyr::case_when(
    !is.na(doi) ~ "doi",
    !is.na(ret$study_label_norm) & !is.na(ret$study_year) ~
      "descriptive_label_year",
    !is.na(ret$study_label_norm) ~ "descriptive_label",
    TRUE ~ "row_unique_no_descriptive_id"
  )
  ret$studyid_source <- dplyr::case_when(
    !is.na(doi) ~ doi_col,
    !is.na(ret$study_label_norm) & !is.na(ret$study_year) ~
      paste(label_col, year_col, sep = "+"),
    !is.na(ret$study_label_norm) ~ label_col,
    TRUE ~ NA_character_
  )
  ret$raw_studyid_source <- raw_id_col
  ret$raw_studyid <- if(is.na(raw_id_col)) NA_character_ else clean_text(ret[[raw_id_col]])
  
  if(is.null(ret$year)) {
    ret$year <- as.numeric(NA)
  } else {
    ret$year <- extract_year(ret$year)
  }
  if(is.null(ret$n)) {
    ret$ss <- as.numeric(NA)
  } else {
    ret$ss <- as.numeric(ret$n)
  }
  
  ret %>% 
    dplyr::select(
      b, se, studyid, studyid_source, studyid_basis, study_label,
      study_label_norm, study_year, raw_studyid_source, raw_studyid, year, ss
    )
}) %>% 
  dplyr::bind_rows(.id = "metaid")

saveRDS(onedf, "data/Sladekova.rds")
rm(onedf)
rm(dfs)
