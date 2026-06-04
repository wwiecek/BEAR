# Check generated inputs required before rendering the Quarto site.

dataset_sources <- list.files("../doc/datasets", pattern = "\\.Rmd$")
dataset_pages <- list.files("datasets", pattern = "\\.qmd$")
mixture_plots <- list.files("../results/mixture_plots", pattern = "\\.png$")

if (!file.exists("_dataset_index.md")) {
  stop("Missing _dataset_index.md. Run Rscript --vanilla build_dataset_pages.R")
}

if (length(dataset_pages) != length(dataset_sources)) {
  stop("Expected ", length(dataset_sources), " dataset pages, found ",
       length(dataset_pages), ". Re-run build_dataset_pages.R")
}

index <- readLines("_dataset_index.md", warn = FALSE)
if (any(grepl("|  |", index, fixed = TRUE))) {
  stop("Dataset index has blank domain/data cells.")
}

if (!file.exists("assets/favicon.png")) {
  stop("Missing assets/favicon.png.")
}

expected_plots <- sub("\\.qmd$", ".png", dataset_pages)
missing_plots <- setdiff(expected_plots, mixture_plots)
if (length(missing_plots) > 0) {
  stop("Missing dataset mixture plots: ", paste(missing_plots, collapse = ", "))
}

message("Site inputs OK: ", length(dataset_pages), " dataset pages and ",
        length(expected_plots), " mixture plots.")
