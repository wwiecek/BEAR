# Generate Quarto wrappers for BEAR dataset documentation.
# Source prose remains in doc/datasets/*.Rmd.

dataset_dir <- "../doc/datasets"
settings_file <- "../R/settings.R"
output_dir <- "datasets"
index_file <- "_dataset_index.md"
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

data_label_override <- function(path, key) {
  file_key <- tools::file_path_sans_ext(basename(path))
  labels <- c(
    "ArelBundock" = "46 meta-analyses",
    "Askarov" = "352 meta-analyses",
    "BarnettWren" = "50,000 studies",
    "Bartos" = "215 meta-analyses",
    "Brodeur" = "328 studies",
    "Chavalarias" = "50,000 studies",
    "clinicaltrials" = "16,597 studies",
    "Cochrane" = "5,871 meta-analyses",
    "CostelloFox" = "466 meta-analyses",
    "euctr" = "8,650 studies",
    "Head" = "50,000 studies",
    "JagerLeek" = "5,322 studies",
    "Lang" = "736 studies",
    "ManyLabs2" = "128 replication studies",
    "Metapsy" = "20 meta-analyses",
    "OSC" = "97 replication studies",
    "Sladekova" = "406 meta-analyses",
    "Szucs" = "2,261 studies",
    "WWC" = "1,408 studies"
  )
  switch(file_key,
         "CostelloFox_Yang" = "553 meta-analyses",
         "psymetadata_Nuijten" = "2,634 studies",
         "SCORE" = "159 claims; 163 replications",
         unname(labels[key]))
}

regex_escape <- function(x) {
  gsub("([][(){}.*+?^$\\\\|])", "\\\\\\1", x)
}

clean_dataset_title <- function(title, domain) {
  parenthetical <- regmatches(title, gregexpr("\\([^()]+\\)", title))[[1]]
  if(length(parenthetical) == 0) return(trimws(title))

  domain_aliases <- c(
    domain,
    gsub("&", "and", domain),
    tolower(domain),
    "psychology datasets",
    "cognitive neuroscience"
  )
  domain_aliases <- tolower(trimws(domain_aliases))

  for(term in parenthetical) {
    contents <- tolower(trimws(gsub("^\\(|\\)$", "", term)))
    if(contents %in% domain_aliases)
      title <- gsub(paste0("[[:space:]]*", regex_escape(term)), "", title)
  }

  trimws(title)
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
  index_title <- clean_dataset_title(title, unname(metadata["domain"]))
  data_label <- data_label_override(path, key)
  page <- file.path(output_dir, paste0(slug, ".qmd"))
  source_path <- file.path("../../doc/datasets", basename(path))
  plot_path <- file.path("../../results/mixture_plots", paste0(slug, ".png"))

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
    "## Model of z-values",
    "",
    paste0("![](", plot_path, "){.dataset-mixture-plot fig-alt=\"",
           yaml_escape(title), " mixture model plot\"}"),
    "",
    "The fitted mixture model is shown over the empirical distribution of",
    "absolute z-values. The dashed line shows the corrected distribution used",
    "for the model-based summaries."
  )
  writeLines(page_lines, page)

  data.frame(
    title = title,
    index_title = index_title,
    key = key,
    domain = unname(metadata["domain"]),
    category = unname(metadata["category"]),
    data_label = data_label,
    page = file.path("datasets", paste0(slug, ".qmd")),
    stringsAsFactors = FALSE
  )
})

dataset_index <- do.call(rbind, dataset_rows)
dataset_index$domain[is.na(dataset_index$domain)] <- ""
dataset_index$category[is.na(dataset_index$category)] <- ""
dataset_index$data_label[is.na(dataset_index$data_label)] <- ""

link <- function(title, page) {
  paste0("[", title, "](", page, ")")
}

table_lines <- c(
  "| Dataset | Domain | Data |",
  "|:--|:--|:--|",
  paste0("| ", link(dataset_index$index_title, dataset_index$page), " | ",
         dataset_index$domain, " | ", dataset_index$data_label, " |")
)

writeLines(table_lines, index_file)
