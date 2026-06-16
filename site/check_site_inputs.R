# Check generated inputs required before rendering the Quarto site.

dataset_classification_file <- "../doc/dataset_classification.csv"
source("../R/settings.R", local = TRUE)
source("../R/site_dataset_config.R", local = TRUE)

dataset_sources <- list.files("../doc/datasets", pattern = "\\.Rmd$")
dataset_pages <- list.files("datasets", pattern = "\\.qmd$")
mixture_plots <- list.files("assets/mixture_plots", pattern = "\\.png$")
downloads_file <- "dataset_downloads.csv"
expected_plot_index <- site_dataset_plot_index("../doc/datasets")

if (!file.exists("_dataset_index.md")) {
  stop("Missing _dataset_index.md. Run Rscript --vanilla build_dataset_pages.R")
}

if (!file.exists("_site_metrics.md")) {
  stop("Missing _site_metrics.md. Run Rscript --vanilla ../workflow/write_site_metrics.R")
}

if (!file.exists(downloads_file)) {
  stop("Missing ", downloads_file, ". Run Rscript --vanilla update_dataset_downloads.R")
}

if (length(dataset_pages) != length(dataset_sources)) {
  stop("Expected ", length(dataset_sources), " dataset pages, found ",
       length(dataset_pages), ". Re-run build_dataset_pages.R")
}

required_download_cols <- c("source_file", "file_name", "file_size",
                            "file_size_label")
downloads <- read.csv(downloads_file, stringsAsFactors = FALSE)
missing_download_cols <- setdiff(required_download_cols, names(downloads))
if (length(missing_download_cols) > 0) {
  stop("Missing download metadata columns: ",
       paste(missing_download_cols, collapse = ", "))
}

missing_downloads <- setdiff(dataset_sources, downloads$source_file)
unknown_downloads <- setdiff(downloads$source_file, dataset_sources)

if (length(missing_downloads) > 0) {
  stop("Missing download metadata for: ", paste(missing_downloads, collapse = ", "))
}

if (length(unknown_downloads) > 0) {
  stop("Download metadata references unknown source files: ",
       paste(unknown_downloads, collapse = ", "))
}

if (any(!grepl("\\.rds$", downloads$file_name))) {
  stop("Download metadata contains non-RDS files.")
}

if (any(is.na(downloads$file_size) | downloads$file_size <= 0 |
        downloads$file_size_label == "")) {
  stop("Download metadata has missing file sizes or labels.")
}

index <- readLines("_dataset_index.md", warn = FALSE)
if (any(grepl("|  |", index, fixed = TRUE))) {
  stop("Dataset index has blank domain/data cells.")
}

if (!file.exists("assets/favicon.png")) {
  stop("Missing assets/favicon.png.")
}

if (!file.exists("assets/bear_banner.png")) {
  stop("Missing assets/bear_banner.png.")
}

if (!file.exists("assets/selection_mixture_plot.png")) {
  stop("Missing assets/selection_mixture_plot.png. Run Rscript --vanilla workflow/write_selection_plot.R")
}

expected_plots <- unique(expected_plot_index$plot_file)
missing_plots <- setdiff(expected_plots, mixture_plots)
if (length(missing_plots) > 0) {
  stop("Missing dataset mixture plots: ", paste(missing_plots, collapse = ", "))
}

message("Site inputs OK: ", length(dataset_pages), " dataset pages and ",
        length(expected_plots), " dataset mixture plots.")
