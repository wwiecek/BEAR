# Generate Quarto wrappers for BEAR dataset documentation.
# Source prose remains in doc/datasets/*.Rmd.

dataset_dir <- "../doc/datasets"
settings_file <- "../R/settings.R"
output_dir <- "datasets"
index_file <- "_dataset_index.md"
dataset_pdf_url <- paste0("https://github.com/wwiecek/BEAR/blob/main/",
                          "doc/datasets.pdf")

source(settings_file, local = TRUE)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

dataset_files <- list.files(dataset_dir, pattern = "\\.Rmd$", full.names = TRUE)
dataset_files <- dataset_files[order(tolower(basename(dataset_files)))]

slugify <- function(x) {
  x <- gsub("\\.Rmd$", "", basename(x))
  x <- gsub("_", "-", x)
  x <- gsub("[^A-Za-z0-9-]", "-", x)
  x <- gsub("-+", "-", x)
  tolower(gsub("^-|-$", "", x))
}

read_title <- function(path) {
  lines <- readLines(path, warn = FALSE)
  heading <- grep("^##[[:space:]]+", lines, value = TRUE)[1]
  if (is.na(heading)) tools::file_path_sans_ext(basename(path)) else
    sub("^##[[:space:]]+", "", heading)
}

dataset_key <- function(path) {
  key <- tools::file_path_sans_ext(basename(path))
  switch(key,
         "clinicaltrials_gov" = "clinicaltrials",
         "CostelloFox_Yang" = "CostelloFox",
         "EUCTR" = "euctr",
         "psymetadata_Nuijten" = "psymetadata",
         "SCORE" = "SCORE_claims",
         key)
}

metadata_override <- function(path, key) {
  file_key <- tools::file_path_sans_ext(basename(path))
  if (file_key == "SCORE") {
    return(c(domain = unname(bear_domain["SCORE_claims"]),
             category = "curated / replications"))
  }
  if (file_key == "psymetadata_Nuijten") {
    return(c(domain = "psychology / intelligence",
             category = unname(bear_classification[key])))
  }
  c(domain = unname(bear_domain[key]),
    category = unname(bear_classification[key]))
}

yaml_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  gsub("\"", "\\\\\"", x)
}

dataset_rows <- lapply(dataset_files, function(path) {
  slug <- slugify(path)
  key <- dataset_key(path)
  metadata <- metadata_override(path, key)
  title <- read_title(path)
  page <- file.path(output_dir, paste0(slug, ".qmd"))
  source_path <- file.path("../../doc/datasets", basename(path))

  page_lines <- c(
    "---",
    paste0("title: \"", yaml_escape(title), "\""),
    "format:",
    "  html:",
    "    toc: true",
    "---",
    "",
    paste0("{{< include ", source_path, " >}}"),
    "",
    "## Full dataset appendix",
    "",
    "The full stitched appendix is available as ",
    paste0("[datasets.pdf](", dataset_pdf_url, ").")
  )
  writeLines(page_lines, page)

  data.frame(
    title = title,
    key = key,
    domain = unname(metadata["domain"]),
    category = unname(metadata["category"]),
    page = file.path("datasets", paste0(slug, ".qmd")),
    stringsAsFactors = FALSE
  )
})

dataset_index <- do.call(rbind, dataset_rows)
dataset_index$domain[is.na(dataset_index$domain)] <- ""
dataset_index$category[is.na(dataset_index$category)] <- ""

link <- function(title, page) {
  paste0("[", title, "](", page, ")")
}

table_lines <- c(
  "| Dataset | Domain | Category |",
  "|:--|:--|:--|",
  paste0("| ", link(dataset_index$title, dataset_index$page), " | ",
         dataset_index$domain, " | ", dataset_index$category, " |")
)

writeLines(table_lines, index_file)
