# Build the Cochrane dictionary from its definitions and saved public dataset.

library(readr)

dictionary <- read_csv("doc/datasets/Cochrane_dictionary.csv",
                       show_col_types = FALSE)
data <- readRDS("data/Cochrane.rds")
stopifnot(setequal(dictionary$variable, names(data)),
          !anyDuplicated(dictionary$variable))

format_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
cell <- function(x) gsub("|", "\\\\|", gsub("\n", " ", x, fixed = TRUE),
                         fixed = TRUE)
summary_for <- function(name) {
  x <- data[[name]]
  missing <- sum(is.na(x))
  if (name %in% c("withdrawn", "rct", "outcome_group", "measure",
                  "study.data_source")) {
    counts <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
    return(paste(paste0(names(counts), " ", format_n(as.integer(counts))),
                 collapse = "; "))
  }
  if (missing > 0L) paste0("missing ", format_n(missing)) else ""
}

dictionary$summary <- vapply(dictionary$variable, summary_for, character(1))
lines <- c(
  "## Data dictionary", "",
  "One row is one study result within a Cochrane review analysis. Summaries count rows, not distinct reviews.",
  ""
)
for (group in unique(dictionary$group)) {
  rows <- dictionary[dictionary$group == group, ]
  lines <- c(lines, paste0("### ", group), "",
             "| Variable | Definition | Summary |",
             "|:--|:--|:--|",
             paste0("| `", cell(rows$variable), "` | ", cell(rows$definition),
                    " | ", cell(rows$summary), " |"), "")
}
writeLines(lines, "doc/datasets/Cochrane_dictionary.md")
