# Download Cochrane RM5 files from a DOI manifest and checkpoint parsed reviews.
# Existing RM5 files are parsed without being downloaded again.

library(tidyverse)
library(fs)

source("process/cochrane/Cochrane_helpers.R")

manifest_path <- Sys.getenv(
  "BEAR_COCHRANE_MANIFEST",
  "data_raw/Cochrane/data/cdsr_interventions_19nov2025.csv"
)
rm5_dir <- Sys.getenv("BEAR_COCHRANE_RM5_DIR", "data/Cochrane/rm5")
checkpoint_path <- Sys.getenv(
  "BEAR_COCHRANE_CHECKPOINT",
  "data_raw/Cochrane/data/cdsr_rm5_results.rds"
)
sleep_sec <- 3

dir_create(rm5_dir)
dir_create(path_dir(checkpoint_path))

if (!file_exists(manifest_path)) {
  stop(
    "No Cochrane DOI manifest found at: ", manifest_path, ".\n",
    "Set BEAR_COCHRANE_MANIFEST to a CSV with at least a DOI column.\n",
    paste0(
      "Existing RM5 files can still be parsed by ",
      "process/cochrane/Cochrane_process_data.R."
    )
  )
}

manifest <- manifest_from_csv(manifest_path)
if (nrow(manifest) == 0L) {
  stop("The Cochrane manifest contains no usable DOI/review ID rows.")
}
if (!requireNamespace("cochrane", quietly = TRUE)) {
  stop(
    "The cochrane package is required for RM5 downloads and parsing.\n",
    "Install it with remotes::install_github(\"schw4b/cochrane\")."
  )
}

# Download only when needed, then parse the expected RM5 file into one row.
download_and_read_one <- function(doi, rm5_dir, sleep_sec = 3) {
  cochrane_id <- id_from_doi(doi)
  file <- file_from_id(cochrane_id)
  full_path <- path(rm5_dir, file)

  if (!file_exists(full_path)) {
    download_error <- tryCatch(
      {
        cochrane::get.review(
          doi = doi,
          path = rm5_dir,
          show.terms = FALSE
        )
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    Sys.sleep(sleep_sec)

    if (!file_exists(full_path)) {
      error <- if (is.null(download_error)) {
        "RM5 file not found after download"
      } else {
        download_error
      }
      return(tibble(
        cochrane_id = cochrane_id,
        id = cochrane_id,
        doi = doi,
        file = file,
        studies = list(NULL),
        ma = list(NULL),
        ok = FALSE,
        error = error
      ))
    }
  }

  read_rm5_one(file, rm5_dir, doi, cochrane_id)
}

# Resume from any existing checkpoint, including a valid zero-row checkpoint.
if (file_exists(checkpoint_path)) {
  results_all <- readRDS(checkpoint_path)
  if (!is.data.frame(results_all)) {
    stop("The Cochrane checkpoint is not a data frame: ", checkpoint_path)
  }
  results_all <- bind_rows(empty_cochrane_results(), results_all)
} else {
  results_all <- empty_cochrane_results()
}

done_dois <- unique(stats::na.omit(results_all$doi))
pending <- manifest %>% filter(!doi %in% done_dois)

message("Total in manifest: ", nrow(manifest))
message("Already done: ", length(done_dois))
message("Still to do: ", nrow(pending))

if (nrow(pending) > 0L) {
  for (i in seq_len(nrow(pending))) {
    result <- download_and_read_one(pending$doi[[i]], rm5_dir, sleep_sec)
    results_all <- bind_rows(results_all, result)

    if (i %% 20L == 0L) {
      saveRDS(results_all, checkpoint_path)
      message(
        "Saved checkpoint at pending row ", i,
        " (total rows: ", nrow(results_all), ")"
      )
    }
  }
}

saveRDS(results_all, checkpoint_path)
message("Saved Cochrane checkpoint: ", checkpoint_path)
