# Build the Cochrane dictionary from its definitions and saved public dataset.

library(readr)

dictionary <- read_csv("doc/datasets/Cochrane_dictionary.csv",
                       show_col_types = FALSE)
data <- readRDS("data/Cochrane.rds")
stopifnot(setequal(dictionary$variable, setdiff(names(data), c("yi", "vi", "z"))),
          all(c("yi", "vi", "z") %in% names(data)),
          !anyDuplicated(dictionary$variable))

format_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
format_pct <- function(x) sprintf("%.1f%%", 100 * x)
cell <- function(x) gsub("|", "\\\\|", gsub("\n", " ", x, fixed = TRUE),
                         fixed = TRUE)
summary_for <- function(name) {
  x <- data[[name]]
  if (name %in% c("withdrawn", "specialty", "rct", "outcome.flag",
                  "study.data_source", "measure_group", "outcome_group",
                  "measure", "measure_detailed")) {
    values <- as.character(x)
    values[is.na(values) | values == ""] <- "missing"
    counts <- sort(table(values), decreasing = TRUE)
    if (name == "specialty") {
      named <- counts[names(counts) != "missing"]
      top <- head(named, 5)
      counts <- c(top, remaining = sum(tail(named, -length(top))),
                  missing = sum(values == "missing"))
      counts <- counts[counts > 0 | names(counts) == "missing"]
    }
    return(paste0(names(counts), " ", format_n(as.integer(counts)),
                  " / ", format_n(length(x)), " (",
                  format_pct(as.integer(counts) / length(x)), ")",
                  collapse = "; "))
  }
  if (name %in% c("events1", "total1", "mean1", "sd1",
                  "events2", "total2", "mean2", "sd2",
                  "effect.size", "se", "ci.lower", "ci.upper")) return("")
  missing <- sum(is.na(x))
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
