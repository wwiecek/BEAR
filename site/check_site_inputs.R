# Check generated inputs required before rendering the Quarto site.

dataset_sources <- list.files("../doc/datasets", pattern = "\\.Rmd$")
dataset_pages <- list.files("datasets", pattern = "\\.qmd$")

if (!file.exists("_dataset_index.md")) {
  stop("Missing _dataset_index.md. Run Rscript --vanilla build_dataset_pages.R")
}

if (length(dataset_pages) != length(dataset_sources)) {
  stop("Expected ", length(dataset_sources), " dataset pages, found ",
       length(dataset_pages), ". Re-run build_dataset_pages.R")
}

index <- readLines("_dataset_index.md", warn = FALSE)
if (any(grepl("|  |", index, fixed = TRUE))) {
  stop("Dataset index has blank domain/category cells.")
}

message("Site inputs OK: ", length(dataset_pages), " dataset pages.")
