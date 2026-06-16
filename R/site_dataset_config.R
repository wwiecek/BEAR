# Metadata helpers shared by the BEAR website dataset pages and plots.

site_slugify <- function(x) {
  x <- gsub("\\.Rmd$", "", basename(x))
  x <- gsub("_", "-", x)
  x <- gsub("[^A-Za-z0-9-]", "-", x)
  x <- gsub("-+", "-", x)
  tolower(gsub("^-|-$", "", x))
}

site_read_title <- function(path) {
  lines <- readLines(path, warn = FALSE)
  heading <- grep("^##[[:space:]]+", lines, value = TRUE)[1]
  if (is.na(heading)) tools::file_path_sans_ext(basename(path)) else
    sub("^##[[:space:]]+", "", heading)
}

site_dataset_key <- function(path) {
  file_name <- basename(path)
  key <- unname(site_dataset_key_override[file_name])
  if (is.na(key)) tools::file_path_sans_ext(file_name) else key
}

site_dataset_key_override <- c(
  "clinicaltrials_gov.Rmd" = "clinicaltrials",
  "CostelloFox_Yang.Rmd" = "CostelloFox",
  "EUCTR.Rmd" = "euctr",
  "psymetadata_Nuijten.Rmd" = "psymetadata",
  "SCORE.Rmd" = "SCORE_claims"
)

site_documented_mixtures <- list(
  "clinicaltrials_gov.Rmd" = "ctgov_euctr",
  "CostelloFox_Yang.Rmd" = c("CostelloFox", "Yang"),
  "EUCTR.Rmd" = "ctgov_euctr",
  "psymetadata_Nuijten.Rmd" = c("psymetadata", "Nuijten"),
  "SCORE.Rmd" = c("SCORE_claims", "SCORE_replications")
)

site_dataset_plot_keys <- function(path, key = site_dataset_key(path)) {
  file_name <- basename(path)
  datasets <- site_documented_mixtures[[file_name]]
  if (is.null(datasets)) key else datasets
}

site_metadata_override <- function(path, key) {
  file_key <- tools::file_path_sans_ext(basename(path))
  if (file_key == "SCORE") {
    return(c(domain = unname(bear_domain["SCORE_claims"]),
             category = "curated / replications"))
  }
  if (file_key == "psymetadata_Nuijten") {
    return(c(domain = "psychology / intelligence",
             category = unname(bear_dataset_classes$workflow_classification[key])))
  }
  c(domain = unname(bear_domain[key]),
    category = unname(bear_dataset_classes$workflow_classification[key]))
}

site_summary_group_override <- function(path, key) {
  datasets <- site_dataset_plot_keys(path, key)
  groups <- unique(unname(bear_dataset_classes$summary_group[datasets]))
  groups <- groups[!is.na(groups)]
  if ("Replication efforts" %in% groups) return("Replication efforts")
  if (length(groups) == 1) return(groups)
  groups[order(match(groups, bear_data_summary_group_levels))][1]
}

site_data_label_override <- function(path, key) {
  file_key <- tools::file_path_sans_ext(basename(path))
  labels <- c(
    "ArelBundock" = "2.3k studies in 351 meta-analyses",
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

site_regex_escape <- function(x) {
  gsub("([][(){}.*+?^$\\\\|])", "\\\\\\1", x)
}

site_clean_dataset_title <- function(title, domain) {
  parenthetical <- regmatches(title, gregexpr("\\([^()]+\\)", title))[[1]]
  if (length(parenthetical) == 0) return(trimws(title))

  domain_aliases <- c(
    domain,
    gsub("&", "and", domain),
    tolower(domain),
    "PubMed/Medline",
    "psychology datasets",
    "social science",
    "social and behavioural sciences",
    "social and behavioral sciences",
    "cognitive neuroscience"
  )
  domain_aliases <- tolower(trimws(domain_aliases))

  for (term in parenthetical) {
    contents <- tolower(trimws(gsub("^\\(|\\)$", "", term)))
    if (contents %in% domain_aliases)
      title <- gsub(paste0("[[:space:]]*", site_regex_escape(term)), "", title)
  }

  trimws(title)
}

site_plot_title <- function(dataset) {
  label <- unname(bear_labels[dataset])
  if (is.na(label)) label <- unname(bear_names[dataset])
  if (is.na(label)) label <- dataset
  gsub("\n", ": ", label)
}

site_plot_note <- function(dataset) {
  notes <- c(
    "ctgov_euctr" = paste(
      "BEAR fits a single model to ClinicalTrials.gov and EU CTR because",
      "the two clinical-trials registries have similar reporting structures",
      "and z-value derivations. The plot therefore shows the combined",
      "clinical-trials fit."
    )
  )
  note <- unname(notes[dataset])
  if (is.na(note)) "" else note
}

site_dataset_page_index <- function(dataset_dir) {
  dataset_files <- list.files(dataset_dir, pattern = "\\.Rmd$", full.names = TRUE)
  dataset_files <- dataset_files[order(tolower(basename(dataset_files)))]

  do.call(rbind, lapply(dataset_files, function(path) {
    key <- site_dataset_key(path)
    metadata <- site_metadata_override(path, key)
    title <- site_read_title(path)
    slug <- site_slugify(path)
    data.frame(
      source_file = basename(path),
      slug = slug,
      title = title,
      index_title = site_clean_dataset_title(title, unname(metadata["domain"])),
      key = key,
      domain = unname(metadata["domain"]),
      category = unname(metadata["category"]),
      summary_group = site_summary_group_override(path, key),
      data_label = site_data_label_override(path, key),
      page = file.path("datasets", paste0(slug, ".qmd")),
      stringsAsFactors = FALSE
    )
  }))
}

site_dataset_plot_index <- function(dataset_dir) {
  pages <- site_dataset_page_index(dataset_dir)
  do.call(rbind, lapply(seq_len(nrow(pages)), function(i) {
    page <- pages[i, ]
    path <- file.path(dataset_dir, page$source_file)
    plot_datasets <- site_dataset_plot_keys(path, page$key)
    stems <- plot_datasets
    data.frame(
      page[rep(1, length(plot_datasets)), ],
      plot_dataset = plot_datasets,
      plot_stem = stems,
      plot_file = paste0(stems, ".png"),
      plot_title = vapply(plot_datasets, site_plot_title, character(1)),
      plot_note = vapply(plot_datasets, site_plot_note, character(1)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }))
}
