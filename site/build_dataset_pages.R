# Generate Quarto wrappers for BEAR dataset documentation.
# Source prose remains in doc/datasets/*.Rmd.

dataset_dir <- "../doc/datasets"
settings_file <- "../R/settings.R"
output_dir <- "datasets"
index_file <- "_dataset_index.md"
metrics_file <- "_site_metrics.md"
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
    "ArelBundock" = "2.3k studies from 46 meta-analyses",
    "Askarov" = "1.9k studies from 352 meta-analyses",
    "BarnettWren" = "416k studies from MEDLINE/PubMed",
    "Bartos" = "2.2k studies in 215 meta-analyses",
    "Brodeur" = "328 RCTs from econ journals",
    "Chavalarias" = "1.9mln studies from MEDLINE/PubMed",
    "clinicaltrials" = "registry of 16.6k clinical trials",
    "Cochrane" = "90k studies in 6.6k Cochrane reviews",
    "CostelloFox" = "13k studies from 466 meta-analyses",
    "euctr" = "registry of 8.7k clinical trials",
    "Head" = "219k studies from PubMed",
    "JagerLeek" = "5.3k articles from 5 medical journals",
    "Lang" = "736 papers from econ journals",
    "ManyLabs2" = "128 replications of 28 effects",
    "Metapsy" = "1.5k studies in 20 meta-analyses",
    "OSC" = "97 replications of experiments",
    "Sladekova" = "3.5k studies in 406 meta-analyses",
    "Szucs" = "2.3k cognitive neuroscience studies",
    "WWC" = "1.4k education studies"
  )
  switch(file_key,
         "CostelloFox_Yang" = "13k studies in 553 meta-analyses",
         "psymetadata_Nuijten" = "2.6k studies across psychology datasets",
         "SCORE" = "replications of 163 claims + 1.9k claims from papers",
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

fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE)

fmt_mln <- function(x) {
  paste0(format(round(x / 1e6, 1), nsmall = 1), " mln")
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
  plot_path <- file.path("../assets/mixture_plots", paste0(slug, ".png"))

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
    "absolute z-values.",
    "The solid line is a mixture of half-normals, with selection.",
    "The dashed line shows the distribution without selection.",
    "If there are inequalities (e.g. studies",
    "reporting p < 0.05) the histogram resamples values from the appropriate set."
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

bear <- readRDS("../BEAR.rds")
data_files <- list.files("../data", pattern = "\\.rds$", full.names = TRUE)
data_points <- if (length(data_files) > 0) {
  sum(vapply(data_files, function(path) nrow(readRDS(path)), integer(1)))
} else {
  nrow(bear)
}

metrics_lines <- c(
  "::: {.bear-metrics}",
  paste0("<div class=\"bear-metric\"><div class=\"bear-metric-value\">",
         fmt_mln(data_points), "</div><div class=\"bear-metric-label\">",
         "data points</div></div>"),
  paste0("<div class=\"bear-metric\"><div class=\"bear-metric-value\">",
         fmt_int(length(unique(na.omit(bear$metaid)))),
         "</div><div class=\"bear-metric-label\">meta-analyses</div></div>"),
  paste0("<div class=\"bear-metric\"><div class=\"bear-metric-value\">",
         fmt_int(length(unique(bear$dataset))),
         "</div><div class=\"bear-metric-label\">datasets</div></div>"),
  ":::"
)

writeLines(metrics_lines, metrics_file)
