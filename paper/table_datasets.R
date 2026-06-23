# Build the paper dataset summary table from BEAR.rds and paper selection rules.

library(tidyverse)
library(scales)

source("R/settings.R")
source("R/paper_selection.R")

bear <- readRDS("BEAR.rds")

paper_corpus_labels <- c(
  "Nuijten" = "\\textcite{nuijten2020intelligence}",
  "Askarov" = "\\textcite{askarov2023significance}",
  "Metapsy" = "Metapsy \\parencite{harrer2022metapsydata}",
  "ArelBundock" = "\\textcite{arel2022quantitative}",
  "CostelloFox" = "\\textcite{costello2022decline}",
  "psymetadata" = "psymetadata \\parencite{rodriguez2022psymetadata}",
  "WWC" = "What Works Clearinghouse \\parencite{wwc2025studyfindings}",
  "Cochrane" = "Cochrane Database of Systematic Reviews",
  "Chavalarias" = "\\textcite{chavalarias2016evolution}",
  "SCORE_replications" = "SCORE replications (Tyner et al 2026)",
  "SCORE_claims" = "SCORE claims (Tyner et al 2026)",
  "Lang" = "\\textcite{lang2025credible}",
  "OSC" = "\\textcite{opensciencecollaborationEstimatingReproducibilityPsychological2015a}",
  "Szucs" = "\\textcite{szucs2017empirical}",
  "ctgov_euctr" = "clinicaltrials.gov + EU CTR",
  "BarnettWren" = "\\textcite{barnett2019examination}"
)

paper_corpus_purpose <- c(
  "Nuijten" = "Meta-meta-analysis of intelligence research",
  "Askarov" = "Economics meta-analyses and leading-journal estimates",
  "Metapsy" = "Meta-analytic psychotherapy trial databases",
  "ArelBundock" = "Meta-analytic estimates in political science",
  "CostelloFox" = "Meta-analyses of decline effects in ecology and evolution",
  "psymetadata" = "Curated psychology meta-analysis datasets",
  "WWC" = "Education intervention effect sizes",
  "Cochrane" = "Systematic reviews in health and medicine",
  "Chavalarias" = "Text-mined $p$-values from Medline and PubMed",
  "SCORE_replications" = "Matched replications of published claims",
  "SCORE_claims" = "Claims from papers selected for SCORE",
  "Lang" = "Principal and abstract results from economics papers",
  "OSC" = "Replications of psychology experiments",
  "Szucs" = "Text-mined $t$-tests in cognitive neuroscience",
  "ctgov_euctr" = "Clinical trial registries in the United States and Europe",
  "BarnettWren" = "Text-mined confidence intervals in health and medicine"
)

table_bear <- bear %>%
  mutate(dataset = if_else(dataset %in% c("clinicaltrials", "euctr"),
                           "ctgov_euctr", dataset),
         z_operator = replace_na(z_operator, "=")) %>%
  filter(dataset %in% paper_selected_datasets) %>%
  group_by(dataset) %>%
  summarise(
    study_units = n_distinct(studyid),
    mean_k = n() / n_distinct(studyid),
    pct_signif = 100 * sum(abs(z) > 1.959 & z_operator != "<") / n(),
    .groups = "drop"
  )

paper_groups <- lapply(paper_dataset_groups, function(datasets) {
  table_bear %>%
    filter(dataset %in% datasets) %>%
    arrange(desc(pct_signif)) %>%
    pull(dataset)
})

format_row <- function(dataset_key) {
  row <- table_bear %>% filter(.data$dataset == .env$dataset_key)
  paste0(
    paper_corpus_labels[dataset_key], " & ",
    paper_corpus_purpose[dataset_key], " & ",
    comma(row$study_units), " & ",
    sprintf("%.1f", row$mean_k), " & ",
    sprintf("%.0f\\%%", row$pct_signif), "\\\\"
  )
}

body_lines <- c(
  paste0("\\multicolumn{5}{l}{\\emph{",
         paper_collection_group_labels["curated_evidence"], "}}\\\\"),
  "\\midrule",
  vapply(paper_groups$curated_evidence, format_row, character(1)),
  "\\addlinespace",
  "\\midrule",
  paste0("\\multicolumn{5}{l}{\\emph{",
         paper_collection_group_labels["domain_sample"], "}}\\\\"),
  "\\midrule",
  vapply(paper_groups$domain_sample, format_row, character(1)),
  "\\addlinespace",
  "\\midrule",
  paste0("\\multicolumn{5}{l}{\\emph{",
         paper_collection_group_labels["replication_effort"], "}}\\\\"),
  "\\midrule",
  vapply(paper_groups$replication_effort, format_row, character(1))
)

table_lines <- c(
  "\\begin{table}",
  "\\centering",
  "\\small",
  "\\begin{tabular}{>{\\raggedright\\arraybackslash}p{4.3cm}@{\\hspace{0.6cm}}p{6.8cm}rrc}",
  "\\textbf{Corpus} & \\textbf{Purpose of dataset} & \\textbf{study units} & \\textbf{$\\bar{k}$} & \\textbf{\\% signif.} \\\\",
  "\\toprule",
  body_lines,
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\caption{Datasets included in the paper, grouped into curated sets, ",
    "representative domain samples, and replication projects. ",
    "Study units are source-specific studies, trials, papers, or claims. ",
    "$\\bar{k}$ is the mean number of observations per study unit, and ",
    "``\\% signif.'' is the percentage of absolute $z$-values exceeding ",
    "1.96. Barnett and Wren and Chavalarias are represented by 50,000-row ",
    "BEAR subsamples; after the table filters their processed source files ",
    "contain 416,027 and 1,887,178 study units, respectively.}"
  ),
  "\\label{tab:dataset_summary}",
  "\\end{table}"
)

writeLines(table_lines, "paper/tables/table1.tex")
