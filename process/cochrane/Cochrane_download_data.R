# Download individual Cochrane RM5 files from a "DOI manifest" (= CSV file from CDSR)
# Existing RM5 files are parsed without being downloaded again.

# WW: this script is simple but also very long; the bulk of it is checking
#     for reason why fetching failed, recording failures, autoresuming,
#     or writing "checkpoints"

library(tidyverse)
library(fs)

source("process/cochrane/Cochrane_helpers.R")

manifest_path   <- "data_raw/Cochrane/data/cdsr_interventions_9jul2026.csv"
rm5_dir         <- "data_raw/Cochrane/rm5"
checkpoint_path <- "data_raw/Cochrane/data/cdsr_rm5_results.rds"
retryable_failure_path <- "data_raw/Cochrane/data/cdsr_rm5_retryable_failures.rds"
retryable_rm5_dir <- "data_raw/Cochrane/rm5_failed/retryable"
sleep_sec <- 15
max_consecutive_access_failures <- 3L

dir_create(rm5_dir)
dir_create(path_dir(checkpoint_path))
dir_create(retryable_rm5_dir)

if (!file_exists(manifest_path)) {
  stop(
    "No Cochrane DOI manifest found at: ", manifest_path, ".\n",
    "Set manifest_path to a CSV with at least a DOI column.\n",
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
download_and_read_one <- function(doi, rm5_dir, sleep_sec = 15) {
  cochrane_id <- id_from_doi(doi)
  file <- file_from_id(cochrane_id)
  full_path <- path(rm5_dir, file)

  if (!file_exists(full_path)) {
    download_warnings <- character()
    download_error <- tryCatch(
      withCallingHandlers(
        {
          cochrane::get.review(
            doi = doi,
            path = rm5_dir,
            show.terms = FALSE
          )
          NULL
        },
        warning = function(w) {
          download_warnings <<- c(download_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) conditionMessage(e)
    )
    Sys.sleep(sleep_sec)

    if (!file_exists(full_path)) {
      error <- if (!is.null(download_error)) {
        download_error
      } else if (length(download_warnings) > 0L) {
        paste(unique(download_warnings), collapse = "; ")
      } else {
        "RM5 file not found after download"
      }
      return(tibble(
        cochrane_id = cochrane_id,
        id = cochrane_id,
        doi = doi,
        file = file,
        studies = list(NULL),
        ma = list(NULL),
        ok = FALSE,
        error = error,
        failure_reason = classify_failure_reason(error)
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

if (file_exists(retryable_failure_path)) {
  retryable_results <- bind_rows(
    empty_cochrane_results(), readRDS(retryable_failure_path)
  )
} else {
  retryable_results <- empty_cochrane_results()
}

done_dois <- unique(stats::na.omit(results_all$doi))
pending <- manifest %>% filter(!doi %in% done_dois)
done_in_manifest <- nrow(manifest) - nrow(pending)

message("Total in manifest: ", nrow(manifest))
message("Already done in manifest: ", done_in_manifest)
message("Still to do: ", nrow(pending))

if (nrow(pending) > 0L) {
  consecutive_access_failures <- 0L

  for (i in seq_len(nrow(pending))) {
    result <- download_and_read_one(pending$doi[[i]], rm5_dir, sleep_sec)

    is_retryable_failure <- !result$ok[[1]] &&
      result$failure_reason[[1]] %in% c(
        "rate_limited", "access_blocked", "download_unavailable",
        "html_or_invalid_rm5"
      )

    if (is_retryable_failure) {
      retryable_results <- bind_rows(retryable_results, result)
      saveRDS(retryable_results, retryable_failure_path)

      if (result$failure_reason[[1]] == "html_or_invalid_rm5") {
        source_path <- path(rm5_dir, result$file[[1]])
        destination_path <- path(retryable_rm5_dir, result$file[[1]])
        if (file_exists(destination_path)) {
          destination_path <- path(
            retryable_rm5_dir,
            paste0(
              path_ext_remove(result$file[[1]]), "_",
              format(Sys.time(), "%Y%m%d%H%M%S"), ".rm5"
            )
          )
        }
        if (file_exists(source_path)) file_move(source_path, destination_path)
      }
    } else {
      results_all <- bind_rows(results_all, result)
    }

    consecutive_access_failures <- if (is_retryable_failure) {
      consecutive_access_failures + 1L
    } else {
      0L
    }

    if (consecutive_access_failures >= max_consecutive_access_failures) {
      saveRDS(results_all, checkpoint_path)
      stop(
        "Stopped after ", max_consecutive_access_failures,
        " consecutive rate-limit or access failures. Wait before retrying.",
        call. = FALSE
      )
    }

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
