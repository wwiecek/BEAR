# Generate Quarto wrappers for BEAR dataset documentation.
# Source prose remains in doc/datasets/*.Rmd.

dataset_dir <- "../doc/datasets"
output_dir <- "datasets"
index_file <- "_dataset_index.md"
downloads_file <- "dataset_downloads.csv"
model_summary_file <- "../paper/power_sign_rep.csv"
download_base_url <- "https://github.com/wwiecek/BEAR_data/blob/main"
dataset_classification_file <- "../doc/dataset_classification.csv"
source("../R/settings.R", local = TRUE)
source("../R/site_dataset_config.R", local = TRUE)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

yaml_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  gsub("\"", "\\\\\"", x)
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

dataset_tag_metadata <- data.frame(
  column = c("replication", "meta_analysis", "database",
             "metascience_paper", "pubmed_scrape", "random_sample",
             "primary_outcome", "rct"),
  label = c("replication", "meta analysis", "database",
            "metascience paper", "PubMed", "random sample",
            "primary outcome", "RCT"),
  index_label = c("replication", "meta", "database", "metascience",
                  "PubMed", "random", "primary", "RCT"),
  description = c(
    "Direct replication project or matched original/replication records.",
    "Organized around meta-analyses, systematic reviews, or reusable meta-analytic datasets.",
    "Standing database, registry, clearinghouse, or package database.",
    "Assembled for a metascience paper or project studying research practice.",
    "Built primarily from PubMed, MEDLINE, PubMed Central, or biomedical journal records.",
    "Random or quasi-random sample of papers, trials, records, or studies.",
    "Restricted to primary, focal, or author-emphasized outcomes.",
    "Contains exclusively or primarily randomised controlled studies."
  ),
  stringsAsFactors = FALSE
)

dataset_tag_rows <- function(datasets) {
  rows <- bear_df_classes[bear_df_classes$dataset %in% datasets, ]
  if (nrow(rows) == 0) stop("Missing tag metadata for: ",
                            paste(datasets, collapse = ", "))
  keep <- vapply(dataset_tag_metadata$column, function(column) {
    any(rows[[column]], na.rm = TRUE)
  }, logical(1))
  dataset_tag_metadata[keep, ]
}

dataset_tags_html <- function(datasets, context = "index") {
  tags <- dataset_tag_rows(datasets)
  if (nrow(tags) == 0) return("")
  labels <- if (context == "index") tags$index_label else tags$label
  paste0(
    "<span class=\"dataset-tags dataset-tags-", context, "\">",
    paste0(
      "<span class=\"dataset-tag dataset-tag-", tags$column,
      "\" data-tooltip=\"", html_escape(tags$description),
      "\" title=\"", html_escape(tags$description),
      "\" tabindex=\"0\" aria-label=\"",
      html_escape(paste(tags$label, tags$description)),
      "\">", html_escape(labels), "</span>",
      collapse = ""
    ),
    "</span>"
  )
}

dataset_page_tags_html <- function(datasets) {
  paste0(
    "<div class=\"dataset-page-tags\"><span class=\"dataset-tag-label\">Tags:</span>",
    dataset_tags_html(datasets, "page"),
    "<span class=\"dataset-hover-hint\">Hover over the tag text for details.</span></div>"
  )
}

dataset_tag_legend <- function() {
  paste0(
    "<div class=\"dataset-tag-legend\" aria-label=\"Dataset tag legend\">",
    "<div class=\"dataset-tag-legend-title\">Dataset tags <span class=\"dataset-hover-hint\">",
    "(hover for explanation)</span></div>",
    "<div class=\"dataset-tag-legend-items\">",
    paste0(
      "<span class=\"dataset-tag dataset-tag-", dataset_tag_metadata$column,
      "\" data-tooltip=\"", html_escape(dataset_tag_metadata$description),
      "\" title=\"", html_escape(dataset_tag_metadata$description),
      "\" tabindex=\"0\" aria-label=\"",
      html_escape(paste(dataset_tag_metadata$label,
                        dataset_tag_metadata$description)),
      "\">", html_escape(dataset_tag_metadata$label), "</span>",
      collapse = ""
    ),
    "</div>",
    "</div>"
  )
}

download_metadata <- read.csv(downloads_file, stringsAsFactors = FALSE)
model_summary <- read.csv(model_summary_file, stringsAsFactors = FALSE)

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
  lines
}

plot_lines <- function(plot_rows) {
  unlist(lapply(seq_len(nrow(plot_rows)), function(i) {
    plot_row <- plot_rows[i, ]
    plot_path <- file.path("../assets/mixture_plots", plot_row$plot_file)
    caption <- paste(
      "The fitted mixture model is shown over the empirical distribution of",
      "absolute z-values. The solid line is a mixture of half-normals, with",
      "selection. The dashed line shows the distribution without selection.",
      "If there are inequalities (e.g. studies reporting p < 0.05), the",
      "histogram resamples values from the appropriate set."
    )
    c(
      if (nrow(plot_rows) > 1) c(paste0("### ", plot_row$plot_title), "") else
        character(),
      paste0("![](", plot_path, "){.dataset-mixture-plot fig-alt=\"",
             yaml_escape(plot_row$plot_title), " mixture model plot\" fig-cap=\"",
             html_escape(caption), "\"}"),
      ""
    )
  }))
}

dictionary_lines <- function(source_file) {
  dictionary_source <- file.path(
    dataset_dir,
    paste0(tools::file_path_sans_ext(source_file), "_dictionary.md")
  )
  if (!file.exists(dictionary_source)) return(character())
  dictionary_include <- file.path(
    "../../doc/datasets",
    basename(dictionary_source)
  )
  c(paste0("{{< include ", dictionary_include, " >}}"), "")
}

dataset_description_lines <- function(source_file) {
  lines <- readLines(file.path(dataset_dir, source_file), warn = FALSE)
  heading <- grep("^##[[:space:]]+", lines)[1]
  if (!is.na(heading)) lines <- lines[-heading]
  lines
}

dataset_at_a_glance_html <- function(row) {
  paste0(
    "<div class=\"dataset-at-a-glance\" aria-label=\"Dataset at a glance\">",
    "<div class=\"dataset-at-a-glance-item\"><span>Domain</span><strong>",
    html_escape(row$domain), "</strong></div>",
    "<div class=\"dataset-at-a-glance-item\"><span>Data</span><strong>",
    html_escape(row$data_label), "</strong></div></div>"
  )
}

model_summary_table_html <- function(row, title = "") {
  characteristics <- c(
    "Probability of significance",
    "Relative probability of publication for |z| &lt; 1.96",
    "Successful replication for |z| &gt; 1.96",
    "Correct sign for |z| &gt; 1.96"
  )
  values <- c(
    sprintf("%.0f%%", 100 * row$assurance), sprintf("%.2f", row$omega),
    sprintf("%.0f%%", 100 * row$repl_signif), sprintf("%.0f%%", 100 * row$sign_signif)
  )
  paste0(
    "<div class=\"model-summary\">", title,
    "<table aria-label=\"Model summary\"><thead><tr><th>Characteristic</th>",
    "<th>Estimate</th></tr></thead><tbody>",
    paste0("<tr><th scope=\"row\">", characteristics, "</th><td><strong>",
           values, "</strong></td></tr>", collapse = ""),
    "</tbody></table></div>"
  )
}

model_summary_html <- function(plot_rows) {
  rows <- model_summary[match(plot_rows$plot_dataset, model_summary$dataset), ]
  if (any(is.na(rows$dataset))) {
    stop("Missing model summary for: ",
         paste(plot_rows$plot_dataset[is.na(rows$dataset)], collapse = ", "))
  }
  tables <- vapply(seq_len(nrow(rows)), function(i) {
    title <- if (nrow(rows) > 1) {
      paste0("<div class=\"model-summary-title\">",
             html_escape(plot_rows$plot_title[i]), "</div>")
    } else ""
    model_summary_table_html(rows[i, ], title)
  }, character(1))
  paste0(
    paste(tables, collapse = ""),
    "<details class=\"model-summary-details\"><summary>What do these terms mean?</summary>",
    "<dl><dt>Probability of significance</dt><dd>The reported value is the ",
    "assurance: the proportion of significant results adjusted for publication bias.</dd>",
    "<dt>Relative probability of publication</dt><dd>The relative probability of observing a result below ",
    "the |z| = 1.96 threshold rather than above it. Values below one indicate ",
    "lower observation probability below the conventional two-sided significance threshold.</dd>",
    "<dt>Successful replication</dt><dd>The probability that an exact replication ",
    "has the same sign and |z| greater than 1.96, conditional on the original ",
    "result having |z| greater than 1.96.</dd><dt>Correct sign</dt><dd>The probability that ",
    "the observed effect has the same direction as the true effect, conditional ",
    "on an original result with |z| greater than 1.96.</dd></dl></details>"
  )
}

dataset_index <- site_dataset_page_index(dataset_dir)
dataset_plot_index <- site_dataset_plot_index(dataset_dir)

invisible(lapply(seq_len(nrow(dataset_index)), function(i) {
  row <- dataset_index[i, ]
  source_file <- row$source_file
  page <- file.path(output_dir, paste0(row$slug, ".qmd"))
  plot_rows <- dataset_plot_index[dataset_plot_index$source_file == source_file, ]
  page_datasets <- site_dataset_plot_keys(file.path(dataset_dir, source_file),
                                          row$key)

  page_lines <- c(
    "---",
    paste0("title: \"", yaml_escape(site_clean_dataset_title(row$title,
                                                                row$domain)), "\""),
    "format:",
    "  html:",
    "    toc: true",
    "---",
    "",
    dataset_page_tags_html(page_datasets),
    "",
    dataset_at_a_glance_html(row),
    "",
    data_callout(source_file),
    "",
    "## Description",
    "",
    dataset_description_lines(source_file),
    "",
    "## Model of z-values",
    "",
    model_text(plot_rows, dataset_plot_index),
    "",
    model_summary_html(plot_rows),
    "",
    plot_lines(plot_rows),
    "",
    dictionary_lines(source_file)
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
    paste0("| ", link(group_rows$index_title, group_rows$page), " ",
           vapply(seq_len(nrow(group_rows)), function(i) {
             row <- group_rows[i, ]
             dataset_tags_html(site_dataset_plot_keys(
               file.path(dataset_dir, row$source_file), row$key))
           }, character(1)), " | ",
           group_rows$domain, " | ", group_rows$data_label, " |"),
    ""
  )
}))

writeLines(c(dataset_tag_legend(), "", table_lines), index_file)
