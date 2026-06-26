# Build the ClinicalTrials.gov data dictionary markdown from the saved public
# analytic file and the hand-maintained dictionary CSV.

library(readr)

dictionary_file <- "doc/datasets/clinicaltrials_gov_dictionary.csv"
data_file <- "data/clinicaltrialsgov.rds"
out_file <- "doc/datasets/clinicaltrials_gov_dictionary.md"

format_n <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_pct <- function(x) {
  paste0(format(round(100 * x, 1), nsmall = 1, trim = TRUE), "%")
}

markdown_cell <- function(x) {
  x[is.na(x)] <- ""
  x <- gsub("\n", " ", x, fixed = TRUE)
  gsub("|", "\\\\|", x, fixed = TRUE)
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

dictionary_summary <- function(variable, data) {
  if (!variable %in% names(data)) return("")
  if (variable %in% c("n_effect_rows_per_study", "n_outcomes_per_study")) {
    study_rows <- unique(data[, c("nct_id", variable)])
    return(compact_numeric(study_rows[[variable]]))
  }
  x <- data[[variable]]

  category_vars <- c(
    "effect_source", "study_type", "phase", "overall_status", "allocation",
    "intervention_model", "primary_purpose", "masking", "domain_primary",
    "intervention_type_primary", "lead_sponsor_class", "outcome_type",
    "measure_class", "scale", "z_operator", "raw_measure",
    "raw_effect_family", "import_decision"
  )
  numeric_vars <- c(
    "year", "enrollment", "number_of_arms", "number_of_groups",
    "n_design_groups", "n_effect_rows_per_study", "n_outcomes_per_study",
    "n_raw_overlap_matches"
  )
  logical_vars <- c(
    "direction_unknown", "has_posted_results", "is_factorial", "is_crossover",
    "has_placebo_comparator", "domain_oncology", "is_drug_trial",
    "is_biological_trial", "is_device_trial", "is_procedure_trial",
    "is_behavioral_trial", "is_vaccine_trial",
    "is_cell_or_gene_therapy_trial", "is_diagnostic_trial",
    "has_industry_sponsor", "is_multi_arm", "has_baseline_measurements",
    "raw_multi_arm_trial", "include_in_bear"
  )

  if (variable %in% category_vars) return(compact_categories(x))
  if (variable %in% numeric_vars) return(compact_numeric(x))
  if (variable %in% logical_vars) return(logical_summary(x))
  ""
}

dictionary <- read_csv(dictionary_file, show_col_types = FALSE)
public <- readRDS(data_file)

stopifnot(
  setequal(dictionary$variable, names(public)),
  all(table(dictionary$variable) == 1)
)

dictionary$summary <- vapply(
  dictionary$variable, dictionary_summary, character(1), data = public
)

lines <- unlist(c(
  list(
    "## Data dictionary",
    "",
    paste0("The summary column is computed from `clinicaltrialsgov.rds` when ",
           "this dictionary is regenerated. Counts are row-level effect ",
           "counts, not unique trial counts."),
    ""
  ),
  lapply(unique(dictionary$group), function(group) {
    rows <- dictionary[dictionary$group == group, ]
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

writeLines(lines, out_file)
message("Wrote ", out_file)
