# Process Szucs and Ioannidis text-mined t-test records for BEAR.
# Writes all source fields to data/Szucs.rds and validation outputs to
# data_raw/Szucs/audit/.

library(tidyverse)
library(R.matlab)

source("R/helpers.R")

raw_path <- "data_raw/Szucs/source/pbio.2000797.s005.mat"
output_path <- "data/Szucs.rds"
validation_path <- "data_raw/Szucs/audit/Szucs_validation.md"
counts_path <- "data_raw/Szucs/audit/Szucs_validation_counts.csv"

expected_n <- 26841
expected_sig <- 17207
expected_nonsig <- 9634

journal_fields <- tibble(
  journal_code = 1:18,
  journal_name = c(
    "Psychological Science", "Cognitive Psychology", "Cognition",
    "Acta Psychologica", "JECP", "Nature Neuroscience", "Neuron", "Brain",
    "The Journal of Neuroscience", "Cerebral Cortex", "Neuroimage", "Cortex",
    "Biological Psychology", "Neuropsychologia", "Neuroscience",
    "Biological Psychiatry", "J Psychiatric Research",
    "Neurobiology of Aging"
  ),
  field = case_when(
    journal_code <= 5 ~ "Psychology",
    journal_code <= 15 ~ "Cognitive neuroscience",
    TRUE ~ "Medical"
  )
)

# Helpers -----

read_mcos_double_runs <- function(mat, n) {
  blob_id <- which(map_lgl(mat, ~ is.integer(.x) && length(.x) > n * 8))
  stopifnot(length(blob_id) == 1)

  blob <- as.raw(as.integer(mat[[blob_id]][1, ]))

  # R.matlab reads the MATLAB table as an opaque MCOS payload. Its numeric
  # columns are stored as aligned double vectors in that payload.
  map_dfr(0:7, function(shift) {
    len <- floor((length(blob) - shift) / 8)
    vals <- readBin(blob[(shift + 1):(shift + len * 8)], "double", n = len,
                    size = 8, endian = "little")
    ok <- is.finite(vals) & vals == floor(vals) & vals >= 0 & vals < 1e7
    runs <- rle(ok)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1
    run_id <- which(runs$values & runs$lengths == n)

    map_dfr(run_id, function(i) {
      values <- list(vals[starts[i]:ends[i]])
      tibble(
        shift = shift,
        offset = shift + (starts[i] - 1) * 8,
        min_value = min(values[[1]]),
        max_value = max(values[[1]]),
        n_unique = n_distinct(values[[1]]),
        values = values
      )
    })
  })
}

extract_journal_table <- function(mat, n) {
  runs <- read_mcos_double_runs(mat, n)

  journal <- runs %>%
    dplyr::filter(min_value == 1, max_value == 18, n_unique == 18) %>%
    pull(values)
  article <- runs %>%
    dplyr::filter(max_value > 100, n_unique > 100) %>%
    pull(values)

  stopifnot(length(journal) == 1, length(article) == 1)

  tibble(
    journal_code = as.integer(journal[[1]]),
    article_id = as.integer(article[[1]])
  )
}

count_line <- function(name, value, expected = NA) {
  if (is.na(expected)) {
    sprintf("- %s: %s", name, value)
  } else {
    status <- if (as.numeric(value) == as.numeric(expected)) "PASS" else "FAIL"
    sprintf("- %s: %s; expected %s [%s]", name, value, expected, status)
  }
}

# Read and process -----

raw_mat <- readMat(raw_path)
tvalues <- as.numeric(raw_mat$D[[1]])
df <- as.numeric(raw_mat$D[[2]])

journal_table <- extract_journal_table(raw_mat, expected_n) %>%
  left_join(journal_fields, by = "journal_code")

# The row unit is one text-mined t-test record extracted from article text.
# Tables were not mined, and atypical or stand-alone nonsignificant results may
# be under-recovered relative to significant results reported conventionally.
source_records <- tibble(
  record_id = seq_along(tvalues),
  t = tvalues,
  df = df
) %>%
  bind_cols(journal_table) %>%
  mutate(
    p = 2 * pt(-abs(t), df),
    z = sign(t) * z_from_p(p),
    z_operator = if_else(p <= 0, ">", "="),
    significant_05 = p <= 0.05
  )

# The source statistic is a t value with known df, so BEAR stores signed
# normal-equivalent z values derived from the corresponding two-sided p-value.
szucs <- source_records %>%
  transmute(
    dataset = "Szucs",
    metaid = NA_character_,
    studyid = paste0("journal_", journal_code, "_article_", article_id),
    method = "text_mined_t_tests",
    measure = "t_test",
    subset = field,
    field = field,
    z = z,
    z_operator = z_operator,
    p = p,
    b = NA_real_,
    se = NA_real_,
    ss = NA_real_,
    source = "PLOS supporting MAT file",
    record_id = record_id,
    article_id = article_id,
    journal_code = journal_code,
    journal_name = journal_name,
    t = t,
    df = df
  )

# Validate -----

field_counts <- szucs %>%
  count(field, name = "n") %>%
  mutate(
    expected_n = case_when(
      field == "Psychology" ~ 7888L,
      field == "Cognitive neuroscience" ~ 16887L,
      field == "Medical" ~ 2066L,
      TRUE ~ NA_integer_
    ),
    pass = n == expected_n
  )

journal_counts <- szucs %>%
  count(field, journal_code, journal_name, name = "n")

significance_counts <- szucs %>%
  summarise(
    significant_05 = sum(p <= 0.05),
    nonsignificant_05 = sum(p > 0.05),
    median_df_significant = median(df[p <= 0.05]),
    median_df_nonsignificant = median(df[p > 0.05]),
    max_df = max(df)
  )

core_checks <- tibble(
  check = c(
    "row_count", "finite_tvalues", "finite_df", "max_df_below_10000",
    "significant_05_count", "nonsignificant_05_count",
    "median_df_significant_about_20", "median_df_nonsignificant_about_19",
    "field_counts", "z_operator_values", "single_underflow_z"
  ),
  pass = c(
    nrow(szucs) == expected_n,
    all(is.finite(szucs$t)),
    all(is.finite(szucs$df)),
    max(szucs$df) < 10000,
    significance_counts$significant_05 == expected_sig,
    significance_counts$nonsignificant_05 == expected_nonsig,
    abs(significance_counts$median_df_significant - 20) <= 1,
    abs(significance_counts$median_df_nonsignificant - 19) <= 1,
    all(field_counts$pass),
    all(szucs$z_operator %in% c("=", ">")),
    sum(is.infinite(szucs$z)) == 1
  )
)

validation_counts <- bind_rows(
  tibble(type = "field", name = field_counts$field, n = field_counts$n,
         expected_n = field_counts$expected_n, pass = field_counts$pass),
  tibble(type = "significance", name = "p <= 0.05",
         n = significance_counts$significant_05, expected_n = expected_sig,
         pass = significance_counts$significant_05 == expected_sig),
  tibble(type = "significance", name = "p > 0.05",
         n = significance_counts$nonsignificant_05, expected_n = expected_nonsig,
         pass = significance_counts$nonsignificant_05 == expected_nonsig)
)

stopifnot(all(core_checks$pass))

# Save artifacts -----

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(validation_path), showWarnings = FALSE, recursive = TRUE)

saveRDS(szucs, output_path)
write_csv(validation_counts, counts_path)

validation_md <- c(
  "# Szucs Processing Validation",
  "",
  "## Source Inventory",
  "",
  "- Source: PLOS Biology supporting MAT file `pbio.2000797.s005.mat`.",
  "- Paper: Denes Szucs and John P. A. Ioannidis, \"Empirical assessment of published effect sizes and power in the recent cognitive neuroscience and psychology literature\".",
  "- Local source note: `data_raw/Szucs/source/README` records post-publication corrections.",
  "- The README correction matters for reproducing power and FRP quantities. In particular, two-sample t-test power calculations should use `df + 2`, not `(df + 2) / 2`.",
  "",
  "## Row Unit And Limitations",
  "",
  "- One row is one text-mined t-test record from article text.",
  "- The source extraction did not mine tables.",
  "- Atypical or stand-alone nonsignificant records may be missed because text-mining targeted conventional test-statistic reporting.",
  "- `z` is a signed normal-equivalent z value: first derive the two-sided t-test p-value from `t` and `df`, then apply the sign of `t` to `qnorm(1 - p / 2)`.",
  "- Rows where the derived p-value underflows to zero retain signed infinite `z` and use `z_operator = \">\"`.",
  "- `ss` is left missing. The paper's power calculations distinguish one-sample/matched tests from two-sample tests, but that test-type label is not recovered in this BEAR file.",
  "",
  "## Validation Results",
  "",
  count_line("rows", nrow(szucs), expected_n),
  count_line("finite t values", all(is.finite(szucs$t))),
  count_line("finite df values", all(is.finite(szucs$df))),
  count_line("max df", significance_counts$max_df),
  count_line("p <= .05 records", significance_counts$significant_05, expected_sig),
  count_line("p > .05 records", significance_counts$nonsignificant_05, expected_nonsig),
  count_line("median df, p <= .05", significance_counts$median_df_significant),
  count_line("median df, p > .05", significance_counts$median_df_nonsignificant),
  count_line("infinite z values", sum(is.infinite(szucs$z)), 1),
  "",
  "## Field Counts",
  "",
  field_counts %>%
    transmute(line = sprintf("- %s: %s records; expected %s [%s]",
                             field, n, expected_n,
                             if_else(pass, "PASS", "FAIL"))) %>%
    pull(line),
  "",
  "## Journal Counts",
  "",
  journal_counts %>%
    transmute(line = sprintf("- %s / %s: %s", field, journal_name, n)) %>%
    pull(line),
  "",
  "## Files Written",
  "",
  sprintf("- `%s`: all Szucs BEAR rows.", output_path),
  sprintf("- `%s`: compact validation counts.", counts_path),
  sprintf("- `%s`: this validation report.", validation_path)
)

writeLines(validation_md, validation_path)
