# Generate Quarto wrappers for BEAR dataset documentation.
# Source prose remains in doc/datasets/*.Rmd.

dataset_dir <- "../doc/datasets"
output_dir <- "datasets"
index_file <- "_dataset_index.md"
downloads_file <- "dataset_downloads.csv"
download_base_url <- "https://github.com/wwiecek/BEAR_data/blob/main"
dataset_classification_file <- "../doc/dataset_classification.csv"
ctgov_dictionary_file <- "../doc/datasets/clinicaltrials_gov_dictionary.csv"
ctgov_data_file <- "../data/clinicaltrialsgov.rds"
source("../R/settings.R", local = TRUE)
source("../R/site_dataset_config.R", local = TRUE)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

yaml_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  gsub("\"", "\\\\\"", x)
}

markdown_cell <- function(x) {
  x[is.na(x)] <- ""
  x <- gsub("\n", " ", x, fixed = TRUE)
  gsub("|", "\\\\|", x, fixed = TRUE)
}

download_metadata <- read.csv(downloads_file, stringsAsFactors = FALSE)
ctgov_dictionary <- read.csv(ctgov_dictionary_file, stringsAsFactors = FALSE)
ctgov_public <- readRDS(ctgov_data_file)

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

format_n <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_pct <- function(x) {
  paste0(format(round(100 * x, 1), nsmall = 1, trim = TRUE), "%")
}

compact_categories <- function(x, top_n = 5) {
  x <- ifelse(is.na(x) | x == "", "missing", as.character(x))
  counts <- sort(table(x), decreasing = TRUE)
  if (length(counts) > top_n) {
    top <- counts[seq_len(top_n)]
    other <- sum(counts[-seq_len(top_n)])
    counts <- c(top, remaining = other)
  }
  total <- sum(counts)
  paste0(names(counts), " ", format_n(as.integer(counts)), " (",
         format_pct(as.integer(counts) / total), ")", collapse = "; ")
}

compact_numeric <- function(x) {
  missing_n <- sum(is.na(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(paste0("all missing (n=", format_n(missing_n), ")"))
  qs <- quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)
  summary <- paste0(
    "median ", format_n(round(qs[2])), " [",
    format_n(round(qs[1])), ", ", format_n(round(qs[3])), "]; range ",
    format_n(round(min(x))), "-", format_n(round(max(x)))
  )
  if (missing_n > 0) summary <- paste0(summary, "; missing ", format_n(missing_n))
  summary
}

logical_summary <- function(x) {
  total <- length(x)
  true_n <- sum(x %in% TRUE, na.rm = TRUE)
  missing_n <- sum(is.na(x))
  parts <- paste0("TRUE ", format_n(true_n), " (", format_pct(true_n / total), ")")
  if (missing_n > 0) {
    parts <- c(parts, paste0("missing ", format_n(missing_n), " (",
                             format_pct(missing_n / total), ")"))
  }
  paste(parts, collapse = "; ")
}

ctgov_dictionary_summary <- function(variable) {
  if (!variable %in% names(ctgov_public)) return("")
  if (variable %in% c("n_effect_rows_per_study", "n_outcomes_per_study")) {
    study_rows <- unique(ctgov_public[, c("nct_id", variable)])
    return(compact_numeric(study_rows[[variable]]))
  }
  x <- ctgov_public[[variable]]

  category_vars <- c(
    "effect_source", "study_type", "phase", "overall_status", "allocation",
    "intervention_model", "primary_purpose", "masking", "domain_primary",
    "intervention_type_primary", "lead_sponsor_class", "outcome_type",
    "measure_class", "scale", "z_operator", "p_sides", "raw_measure",
    "raw_effect_family", "import_decision"
  )
  numeric_vars <- c(
    "year", "enrollment", "number_of_arms", "number_of_groups",
    "n_design_groups", "n_effect_rows_per_study", "n_outcomes_per_study",
    "n_raw_overlap_matches"
  )
  logical_vars <- c(
    "has_posted_results", "is_factorial", "is_crossover",
    "has_placebo_comparator",
    "domain_oncology", "is_drug_trial", "is_biological_trial",
    "is_device_trial", "is_procedure_trial", "is_behavioral_trial",
    "is_vaccine_trial", "is_cell_or_gene_therapy_trial",
    "is_diagnostic_trial", "has_industry_sponsor", "is_multi_arm",
    "has_baseline_measurements", "raw_multi_arm_trial", "include_in_bear"
  )

  if (variable %in% category_vars) return(compact_categories(x))
  if (variable %in% numeric_vars) return(compact_numeric(x))
  if (variable %in% logical_vars) return(logical_summary(x))
  ""
}

dictionary_lines <- function(source_file) {
  if (source_file != "clinicaltrials_gov.Rmd") return(character())

  ctgov_dictionary$summary <- vapply(
    ctgov_dictionary$variable, ctgov_dictionary_summary, character(1)
  )

  unlist(c(
    list(
      "## Data dictionary",
      "",
      paste0("The summary column is computed from the public ",
             "`clinicaltrialsgov.rds` file at website-build time. Counts are ",
             "row-level effect counts, not unique trial counts."),
      ""
    ),
    lapply(unique(ctgov_dictionary$group), function(group) {
      rows <- ctgov_dictionary[ctgov_dictionary$group == group, ]
      c(
        paste0("### ", group),
        "",
        "| Variable | Definition | Summary |",
        "|:--|:--|:--|",
        paste0("| `", markdown_cell(rows$variable), "` | ",
               markdown_cell(rows$definition), " | ",
               markdown_cell(rows$summary), " |"),
        ""
      )
    })
  ))
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
    paste0("| ", link(group_rows$index_title, group_rows$page), " | ",
           group_rows$domain, " | ", group_rows$data_label, " |"),
    ""
  )
}))

writeLines(table_lines, index_file)
