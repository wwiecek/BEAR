# psymetadata -----
library(psymetadata)
library(tidyverse)

ps_items <- data(package = "psymetadata")$results %>%
  as_tibble() %>%
  pull(Item) %>%
  as.character()

load_psy <- function(item) {
  e <- new.env(parent = emptyenv())
  data(list = item, package = "psymetadata", envir = e)
  e[[item]]
}

standardise_psy <- function(df, group) {
  df <- as_tibble(df)
  
  pick1 <- function(cands) {
    hit <- cands[cands %in% names(df)][1]
    if (length(hit) == 0) NA_character_ else hit
  }
  
  get_num <- function(col) {
    if (is.na(col) || !(col %in% names(df))) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(as.character(df[[col]])))
  }
  
  # ---- year (robust): column if present, else regex from a text field ----
  year_col <- pick1(c("pub_year", "year", "study_year", "studyyear", "publication_year"))
  year <- suppressWarnings(readr::parse_integer(as.character(if (!is.na(year_col)) df[[year_col]] else NA)))
  
  if (all(is.na(year))) {
    txt_col <- pick1(c("author", "authors", "title", "brief_title", "study_title"))
    if (!is.na(txt_col)) {
      year <- suppressWarnings(as.integer(stringr::str_extract(as.character(df[[txt_col]]),
                                                               "(19\\d{2}|20\\d{2})")))
    } else {
      year <- rep(NA_integer_, nrow(df))
    }
  }
  year <- ifelse(year < 1900 | year > 2026, NA_integer_, year)
  
  # ---- sample size (ss): try common columns, else sums ----
  ss <- dplyr::coalesce(
    get_num("ss"),
    get_num("n"),
    get_num("ni"),
    get_num("n_total"),
    get_num("participants"),
    get_num("n1") + get_num("n2"),
    get_num("n_treatment") + get_num("n_control")
  )
  
  # metaid
  if(group == "manylabs2018")
    metaid <- df$analysis
  if(group == "sala2019")
    metaid <- df$study_id
  else
    metaid <- NA_character_
  
  # ---- study id: use an existing numeric-ish id if present; else fill later ----
  sid_col <- pick1(c("study_id", "studyid", "id", "sample_id", "samp_id"))
  studyid_raw <- suppressWarnings(readr::parse_integer(as.character(if (!is.na(sid_col)) df[[sid_col]] else NA)))
  
  out <- tibble(
    group   = group,
    studyid = studyid_raw,
    b       = coalesce(get_num("yi"), get_num("yi_r")),
    se      = sqrt(coalesce(get_num("vi"), get_num("vi_r"))),
    year    = as.numeric(year),
    ss      = ss
  ) %>%
    filter(!is.na(b), !is.na(se), is.finite(b), is.finite(se), se > 0) %>%
    mutate(
      studyid = if_else(is.na(studyid), row_number(), studyid),
      z       = b / se,
      metaid  = as.character(metaid),
      subset   = as.character(group),
      year    = as.numeric(year),
      ss      = as.numeric(ss)
    ) %>%
    select(subset, metaid, studyid, year, z, b, se, ss)
  
  out
}


df_psymetadata <-
  ps_items %>%
  set_names() %>%
  map(load_psy) %>%
  imap_dfr(standardise_psy)

saveRDS(df_psymetadata, "data/psymetadata.rds")
rm(df_psymetadata)

