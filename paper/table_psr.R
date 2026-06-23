# Build paper result tables from fitted mixture summaries.

library(tidyverse)

source("R/settings.R")
source("R/paper_selection.R")

psr_results <- read_csv("paper/power_sign_rep.csv", show_col_types = FALSE)
psr_table <- psr_results %>% transmute(dataset, PoS = assurance)
paper_groups <- lapply(paper_dataset_groups, order_paper_datasets,
                       psr_table = psr_table)

format_num <- function(x) sprintf("%.2f", round(x, 2))

format_result_row <- function(dataset_key, cols) {
  row <- psr_results %>% filter(.data$dataset == .env$dataset_key)
  values <- row %>%
    select(all_of(cols)) %>%
    mutate(across(everything(), format_num)) %>%
    unlist(use.names = FALSE)
  paste0(bear_names[dataset_key], " & ", paste(values, collapse = " & "), "\\\\")
}

grouped_rows <- function(cols, n_cols) {
  group_header <- function(group_key) {
    paste0("\\multicolumn{", n_cols, "}{l}{\\emph{",
           paper_collection_group_labels[group_key], "}}\\\\")
  }
  rule <- paste0("\\cmidrule(lr){1-", n_cols, "}")

  c(
    group_header("curated_evidence"),
    rule,
    vapply(paper_groups$curated_evidence, format_result_row, character(1),
           cols = cols),
    "\\addlinespace",
    rule,
    group_header("domain_sample"),
    rule,
    vapply(paper_groups$domain_sample, format_result_row, character(1),
           cols = cols),
    "\\addlinespace",
    rule,
    group_header("replication_effort"),
    rule,
    vapply(paper_groups$replication_effort, format_result_row, character(1),
           cols = cols)
  )
}

table2_cols <- c(
  "omega", "prop_signif", "assurance",
  "replication", "repl_196", "repl_signif",
  "sign", "sign_196", "sign_signif"
)

table2_lines <- c(
  "\\begin{table}",
  "\\centering",
  "\\scriptsize",
  "\\begin{tabular}{lccccccccc}",
  "\\multicolumn{2}{c}{}",
  "& \\multicolumn{2}{c}{Significance}",
  "& \\multicolumn{3}{c}{``Successful'' replication}",
  "& \\multicolumn{3}{c}{Correct sign} \\\\",
  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}",
  "& & & & & \\multicolumn{2}{c}{\\scriptsize Conditional on}",
  "& & \\multicolumn{2}{c}{\\scriptsize Conditional on} \\\\",
  "Corpus & \\multicolumn{1}{c}{\\scriptsize $\\hat\\omega$}",
  "        & \\multicolumn{1}{c}{\\scriptsize signif.}",
  "        & \\multicolumn{1}{c}{\\scriptsize $\\overline{PoS}$}",
  "        & \\multicolumn{1}{c}{\\scriptsize uncond.}",
  "        & \\multicolumn{1}{c}{\\scriptsize $|z| = 1.96$}",
  "        & \\multicolumn{1}{c}{\\scriptsize $|z| \\geq 1.96$}",
  "        & \\multicolumn{1}{c}{\\scriptsize uncond.}",
  "        & \\multicolumn{1}{c}{\\scriptsize $|z| = 1.96$}",
  "        & \\multicolumn{1}{c}{\\scriptsize $|z| \\geq 1.96$} \\\\",
  "\\cmidrule(lr){1-10}",
  grouped_rows(table2_cols, 10),
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\caption{Summary of signal-to-noise ratio modeling results for ",
    "the 16 corpora used in the paper. Rows are grouped into curated sets, ",
    "representative domain samples, and replication projects. ",
    "Calculations of power, replication, and direction of effects use the ",
    "distributions of $z$-values and signal-to-noise ratios by fitted ",
    "mixture models without selection.}"
  ),
  "\\label{tab:results_summary_table2}",
  "\\end{table}"
)

writeLines(table2_lines, "paper/tables/table2.tex")

table3_cols <- c(
  "omega", "prop_signif", "assurance",
  "pos_80pct", "pos_80pct_196", "pos_80pct_signif"
)

table3_rows <- function(cols) {
  grouped_rows(cols, 7)
}

table3_lines <- c(
  "\\begin{table}",
  "\\centering",
  "\\small",
  "\\begin{tabular}{lcccccc}",
  "\\multicolumn{2}{c}{}",
  "& \\multicolumn{2}{c}{Significance}",
  "& \\multicolumn{3}{c}{PoS at least 80\\%} \\\\",
  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-7}",
  "& & & & & \\multicolumn{2}{c}{\\scriptsize Conditional on} \\\\",
  "Corpus & \\multicolumn{1}{c}{\\scriptsize $\\hat\\omega$}",
  "        & \\multicolumn{1}{c}{\\scriptsize signif.}",
  "        & \\multicolumn{1}{c}{\\scriptsize $\\overline{PoS}$}",
  "        & \\multicolumn{1}{c}{\\scriptsize uncond.}",
  "        & \\multicolumn{1}{c}{\\scriptsize $|z| = 1.96$}",
  "        & \\multicolumn{1}{c}{\\scriptsize $|z| \\geq 1.96$} \\\\",
  "\\cmidrule(lr){1-7}",
  table3_rows(table3_cols),
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\caption{Summary of probability-of-significance results for the ",
    "16 corpora used in the paper. Rows are grouped into curated sets, ",
    "representative domain samples, and replication projects.}"
  ),
  "\\label{tab:results_summary_table3}",
  "\\end{table}"
)

writeLines(table3_lines, "paper/tables/table3.tex")
