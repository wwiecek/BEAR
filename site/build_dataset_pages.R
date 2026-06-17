# Generate Quarto wrappers for BEAR dataset documentation.
# Source prose remains in doc/datasets/*.Rmd.

dataset_dir <- "../doc/datasets"
output_dir <- "datasets"
index_file <- "_dataset_index.md"
downloads_file <- "dataset_downloads.csv"
download_base_url <- "https://github.com/wwiecek/BEAR_data/blob/main"
dataset_classification_file <- "../doc/dataset_classification.csv"
source("../R/settings.R", local = TRUE)
source("../R/site_dataset_config.R", local = TRUE)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

yaml_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  gsub("\"", "\\\\\"", x)
}

download_metadata <- read.csv(downloads_file, stringsAsFactors = FALSE)

data_callout <- function(source_file) {
  rows <- download_metadata[download_metadata$source_file == source_file, ]
  if (nrow(rows) == 0) stop("Missing download metadata for: ", source_file)
  links <- paste0("[", rows$file_name, "](", download_base_url, "/",
                  rows$file_name,
                  ") (", rows$file_size_label, ")")
  c(
    "::: {.callout-note}",
    paste0("To download only this data file: ", paste(links, collapse = "; ")),
    "",
    "To download all BEAR datasets, click [here](../getting-started.qmd).",
    ":::"
  )
}

model_text <- function(plot_rows, all_plot_rows) {
  shared <- all_plot_rows[
    all_plot_rows$source_file != plot_rows$source_file[1] &
      all_plot_rows$plot_dataset %in% plot_rows$plot_dataset,
  ]
  shared <- unique(shared[, c("title", "page")])
  shared$page <- basename(shared$page)

  lines <- unique(plot_rows$plot_note[plot_rows$plot_note != ""])
  if (nrow(shared) > 0) {
    links <- paste0("[", shared$title, "](", shared$page, ")")
    lines <- c(lines, paste0("The companion dataset page",
                             if (length(links) > 1) "s are " else " is ",
                             paste(links, collapse = ", "), "."))
  }
  if (nrow(plot_rows) > 1) {
    lines <- c(lines,
               "This documentation page covers more than one fitted dataset,",
               "so the fitted models are shown separately.")
  }
  c(lines, "The fitted mixture model is shown over the empirical distribution",
    "of absolute z-values.",
    "The solid line is a mixture of half-normals, with selection.",
    "The dashed line shows the distribution without selection.",
    "If there are inequalities (e.g. studies",
    "reporting p < 0.05) the histogram resamples values from the appropriate set.")
}

plot_lines <- function(plot_rows) {
  unlist(lapply(seq_len(nrow(plot_rows)), function(i) {
    plot_row <- plot_rows[i, ]
    plot_path <- file.path("../assets/mixture_plots", plot_row$plot_file)
    c(
      if (nrow(plot_rows) > 1) c(paste0("### ", plot_row$plot_title), "") else
        character(),
      paste0("![](", plot_path, "){.dataset-mixture-plot fig-alt=\"",
             yaml_escape(plot_row$plot_title), " mixture model plot\"}"),
      ""
    )
  }))
}

dataset_index <- site_dataset_page_index(dataset_dir)
dataset_plot_index <- site_dataset_plot_index(dataset_dir)

invisible(lapply(seq_len(nrow(dataset_index)), function(i) {
  row <- dataset_index[i, ]
  source_file <- row$source_file
  page <- file.path(output_dir, paste0(row$slug, ".qmd"))
  source_path <- file.path("../../doc/datasets", source_file)
  plot_rows <- dataset_plot_index[dataset_plot_index$source_file == source_file, ]

  page_lines <- c(
    "---",
    paste0("title: \"", yaml_escape(row$title), "\""),
    "format:",
    "  html:",
    "    toc: true",
    "---",
    "",
    data_callout(source_file),
    "",
    paste0("{{< include ", source_path, " >}}"),
    "",
    "## Model of z-values",
    "",
    model_text(plot_rows, dataset_plot_index),
    "",
    plot_lines(plot_rows)
  )
  writeLines(page_lines, page)
}))

dataset_index$domain[is.na(dataset_index$domain)] <- ""
dataset_index$category[is.na(dataset_index$category)] <- ""
dataset_index$summary_group[is.na(dataset_index$summary_group)] <- ""
dataset_index$data_label[is.na(dataset_index$data_label)] <- ""

if (any(dataset_index$summary_group == "")) {
  stop("Missing data summary group for: ",
       paste(dataset_index$key[dataset_index$summary_group == ""],
             collapse = ", "))
}

link <- function(title, page) {
  paste0("[", title, "](", page, ")")
}

table_lines <- unlist(lapply(bear_data_summary_group_levels, function(group) {
  group_rows <- dataset_index[dataset_index$summary_group == group, ]
  if (nrow(group_rows) == 0) return(character())
  c(
    paste0("### ", group),
    "",
    "| Dataset | Domain | Data |",
    "|:--|:--|:--|",
    paste0("| ", link(group_rows$index_title, group_rows$page), " | ",
           group_rows$domain, " | ", group_rows$data_label, " |"),
    ""
  )
}))

writeLines(table_lines, index_file)
