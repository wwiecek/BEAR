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
  candidates[which.max(nonmissing)]
}

study_id_cols <- c("cluster", "study", "Study", "Study.N", "StudyID",
                   "Study_ID", "study_no", "id_sample", "id_document",
                   "sample_id", "id", "ID", "Sample..", "X1", "X")
study_label_cols <- c("author", "Author", "authors", "Authors", "Study.name",
                      "Stady.name", "Study.Author", "Study.N", "Study")

onedf <- lapply(dfs, function(df) {
  
  if(all(c("n_treatment", "n_control") %in% colnames(df))) {
    df$n <- df$n_treatment + df$n_control
  }
  
  ret <- df %>% 
    dplyr::filter(!is.na(yi) & !is.na(vi)) %>% 
    # Fisher's z won't work otherwise; this affects less than 0.5% of observations
    dplyr::filter((1 + yi)/(1 - yi) > 0) %>% 
    dplyr::mutate(
      b = 0.5*log((1 + yi)/(1 - yi)),
      se = sqrt(vi)) 
  
  # Trying to count sample sizes where possible
  
  if(nrow(ret) == 0)
    return(data.frame())
  
  # Reconstruct study IDs within each imported meta-analysis. The source
  # `study_id` is just the file suffix, so use `cluster` where present and
  # otherwise fall back to study/sample columns. These are within-metaid
  # grouping IDs, not fully resolved bibliographic identifiers.
  study_col <- best_col(ret, study_id_cols)
  label_col <- best_col(ret, study_label_cols)
  ret$studyid <- if(is.na(study_col)) {
    as.character(1:nrow(ret))
  } else {
    as.character(ret[[study_col]])
  }
  ret$studyid_source <- study_col
  ret$study_label <- if(is.na(label_col)) {
    NA_character_
  } else {
    as.character(ret[[label_col]])
  }
  if(is.null(ret$year)) {
    ret$year <- as.numeric(NA)
  } else {
    ret$year <- as.numeric(ret$year)
  }
  if(is.null(ret$n)) {
    ret$ss <- as.numeric(NA)
  } else {
    ret$ss <- as.numeric(ret$n)
  }
  
  ret %>% 
    dplyr::select(b, se, studyid, studyid_source, study_label, year, ss)
}) %>% 
  dplyr::bind_rows(.id = "metaid")

saveRDS(onedf, "data/Sladekova.rds")
rm(onedf)
rm(dfs)
