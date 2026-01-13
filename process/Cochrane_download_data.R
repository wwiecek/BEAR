library(tidyverse)
library(cochrane)
library(fs)
library(purrr)

# 1. Point to your CSV -----------------------------------------------------
# csv_path <- "data/Cochrane/cdsr_child_18nov2025.csv"
csv_path <- "data/Cochrane/cdsr_interventions_19nov2025.csv"
coch_csv <- readr::read_csv(csv_path)

# 2. Where to store the .rm5 files ----------------------------------------
dl_path <- "data/Cochrane/rm5"
fs::dir_create(dl_path)

# 3. Where to store progress ----------------------------------------------
checkpoint_path <- "data/Cochrane/cdsr_rm5_results.rds"

# 4. Helper functions -----------------------------------------------------

id_from_doi <- function(doi) stringr::str_extract(doi, "CD[0-9]{6}")
file_from_doi <- function(doi) paste0(id_from_doi(doi), "StatsDataOnly.rm5")

download_and_read_one <- function(doi, dl_path, sleep_sec = 3) {
  id   <- id_from_doi(doi)
  file <- file_from_doi(doi)
  
  cochrane::get.review(doi = doi, path = dl_path, show.terms = FALSE)
  
  full_path <- fs::path(dl_path, file)
  if (!file.exists(full_path)) {
    return(tibble(
      cochrane_id = NA_character_,
      id      = id,
      doi     = doi,
      file    = file,
      studies = list(NULL),
      ma      = list(NULL),
      ok      = FALSE,
      error   = "rm5 file not found after download"
    ))
  }
  
  rev <- cochrane::read.review(file = file, path = dl_path)
  Sys.sleep(sleep_sec)
  
  tibble(
    cochrane_id = NA_character_,   # filled below from CSV
    id      = id,
    doi     = doi,
    file    = file,
    studies = list(rev[[1]]),
    ma      = list(rev[[2]]),
    ok      = TRUE,
    error   = NA_character_
  )
}

safe_dl <- purrr::safely(download_and_read_one, otherwise = NULL)

# 5. Data from CSV --------------------------------------------------------

coch_tbl <- coch_csv %>%
  transmute(
    cochrane_id = `Cochrane Review ID`,
    doi         = DOI
  ) %>%
  filter(!is.na(doi))

# 6. Load checkpoint if it exists -----------------------------------------

if (file.exists(checkpoint_path)) {
  results_all <- readRDS(checkpoint_path)
  done_dois   <- unique(results_all$doi)
} else {
  results_all <- tibble()
  done_dois   <- character(0)
}

pending <- coch_tbl %>% filter(!doi %in% done_dois)

message("Total in CSV: ", nrow(coch_tbl))
message("Already done: ", length(done_dois))
message("Still to do : ", nrow(pending))

# 7. Main loop (resumable) -----------------------------------------------

if (nrow(pending) > 0) {
  for (i in seq_len(nrow(pending))) {
    this <- pending[i, ]
    
    out <- safe_dl(this$doi, dl_path)
    if (!is.null(out$result)) {
      res_row <- out$result
    } else {
      res_row <- tibble(
        cochrane_id = this$cochrane_id,
        id      = id_from_doi(this$doi),
        doi     = this$doi,
        file    = file_from_doi(this$doi),
        studies = list(NULL),
        ma      = list(NULL),
        ok      = FALSE,
        error   = as.character(out$error$message)
      )
    }
    
    # attach cochrane_id from CSV
    res_row$cochrane_id <- this$cochrane_id
    
    results_all <- bind_rows(results_all, res_row)
    
    # save every 20 reviews (adjust if you like)
    if (i %% 20 == 0L) {
      saveRDS(results_all, checkpoint_path)
      message("Saved checkpoint at i = ", i,
              " (total rows: ", nrow(results_all), ")")
    }
  }
  
  # final save
  saveRDS(results_all, checkpoint_path)
}

# results_all is your final one-row-per-review table
results_all

