# Refresh dataset download file sizes used by the generated website pages.
# Run this after updating local processed datasets under data/.

downloads_file <- "dataset_downloads.csv"
data_dir <- "../data"

format_file_size <- function(size) {
  units <- c("B", "KB", "MB", "GB")
  unit <- min(floor(log(size, 1024)) + 1, length(units))
  value <- size / 1024^(unit - 1)
  digits <- if (value >= 10 || unit == 1) 0 else 1
  paste0(format(round(value, digits), nsmall = digits, trim = TRUE), " ",
         units[unit])
}

downloads <- read.csv(downloads_file, stringsAsFactors = FALSE)
local_paths <- file.path(data_dir, downloads$file_name)
file_sizes <- file.info(local_paths)$size

if (any(is.na(file_sizes))) {
  missing_files <- downloads$file_name[is.na(file_sizes)]
  stop("Missing local data files: ", paste(missing_files, collapse = ", "))
}

downloads$file_size <- file_sizes
downloads$file_size_label <- vapply(file_sizes, format_file_size, character(1))

write.csv(downloads, downloads_file, row.names = FALSE, na = "")
message("Updated ", downloads_file, " with ", nrow(downloads), " files.")
