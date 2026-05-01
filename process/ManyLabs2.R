# Build the Many Labs 2 BEAR dataset directly from the OSF replication package.
# Outputs all 28 preregistered paper-included replication analyses.

library(tidyverse)

zip_path <- "data_raw/ManyLabs/OSFdata.zip"
out_path <- "data/ManyLabs2.rds"

primary_results_path <- "OSFdata/!!RawData/ML2_results_primary_all.rds"
secondary_results_path <- "OSFdata/!!RawData/ML2_results_secondary_all.rds"
original_effects_path <- "OSFdata/!!RawData/ML2_OriginalEffects.csv"
key_path <- "OSFdata/!!KeyTables/ML2_KeyTable.csv"

# Helpers -----

read_zip_rds <- function(path) {
  utils::unzip(zip_path, files = path, exdir = tempdir(), overwrite = TRUE)
  readRDS(file.path(tempdir(), path))
}

parse_p_value <- function(x) {
  readr::parse_number(as.character(x), na = c("", "NA", "N/A"))
}

extract_analysis <- function(metaid, analysis_subset, primary_results,
                             secondary_results) {
  result <- if (analysis_subset == "primary") {
    primary_results$aggregated[[metaid]]
  } else {
    secondary_results$aggregated[[metaid]]
  }

  if (is.null(result)) {
    stop("Missing Many Labs 2 result object for ", metaid, call. = FALSE)
  }

  result %>%
    transmute(
      metaid = analysis.name,
      studyid = str_c(as.character(study.source), " | ", source.Filename),
      method = NA_character_,
      measure = "r",
      z = if_else(
        !is.na(ESCI.var.r) & ESCI.var.r > 0,
        ESCI.r / sqrt(ESCI.var.r),
        test.statistic
      ),
      z_operator = "=",
      p = coalesce(test.p.value, ESCI.p.value),
      b = ESCI.r,
      se = if_else(
        !is.na(ESCI.var.r) & ESCI.var.r > 0,
        sqrt(ESCI.var.r),
        if_else(!is.na(test.statistic) & test.statistic != 0,
                abs(ESCI.r / test.statistic), NA_real_)
      ),
      ss = stat.N,
      year = NA_real_,
      source = "replication",
      subset = analysis_subset
    ) %>%
    filter(!is.na(z), !is.na(b), !is.na(se), !is.na(ss),
           is.finite(z), is.finite(b), is.finite(se), se > 0)
}

stop_if_not <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

# Prepare data -----

key <- read_csv(unz(zip_path, key_path), show_col_types = FALSE)
primary_results <- read_zip_rds(primary_results_path)
secondary_results <- read_zip_rds(secondary_results_path)
original_effects_raw <- read_csv(unz(zip_path, original_effects_path),
                                 show_col_types = FALSE)

analysis_key <- key %>%
  filter(study.table1.include == 1) %>%
  mutate(
    subset = case_when(
      study.primary.include == 1 ~ "primary",
      study.secondary.include == 1 ~ "secondary",
      TRUE ~ NA_character_
    ),
    orig_p_from_key = parse_p_value(orig.stat.p.value)
  ) %>%
  arrange(unique.id) %>%
  select(metaid = study.analysis, subset, orig_p_from_key)

stop_if_not(nrow(analysis_key) == 28, "Expected 28 paper-included analyses.")
stop_if_not(!any(is.na(analysis_key$subset)),
            "Every included analysis should be primary or secondary.")

original_effects <- analysis_key %>%
  left_join(original_effects_raw, by = c("metaid" = "study.analysis")) %>%
  transmute(
    metaid,
    orig.z = if_else(
      !is.na(ESCI.r) & !is.na(ESCI.var.r) & ESCI.var.r > 0,
      ESCI.r / sqrt(ESCI.var.r),
      testInfo.statistic
    ),
    orig.z_operator = if_else(!is.na(orig.z), "=", NA_character_),
    orig.p = coalesce(testInfo.p.value, orig_p_from_key),
    orig.b = ESCI.r,
    orig.se = if_else(!is.na(ESCI.var.r) & ESCI.var.r > 0,
                      sqrt(ESCI.var.r), NA_real_),
    orig.ss = N
  )

manylabs2 <- map2(
  analysis_key$metaid,
  analysis_key$subset,
  extract_analysis,
  primary_results = primary_results,
  secondary_results = secondary_results
) %>%
  bind_rows() %>%
  left_join(original_effects, by = "metaid") %>%
  transmute(
    metaid = as.character(metaid),
    studyid = as.character(studyid),
    method,
    measure,
    z = as.numeric(z),
    z_operator,
    p = as.numeric(p),
    b = as.numeric(b),
    se = as.numeric(se),
    ss = as.numeric(ss),
    year = as.numeric(year),
    source,
    subset,
    orig.z = as.numeric(orig.z),
    orig.z_operator,
    orig.p = as.numeric(orig.p),
    orig.b = as.numeric(orig.b),
    orig.se = as.numeric(orig.se),
    orig.ss = as.numeric(orig.ss)
  )

saveRDS(manylabs2, out_path)

# Checks -----

old_manylabs <- readRDS("data/psymetadata.rds") %>%
  filter(subset == "manylabs2018") %>%
  select(metaid, z, b, se, ss) %>%
  arrange(metaid) %>%
  group_by(metaid) %>%
  mutate(row = row_number()) %>%
  ungroup()

old_comparison <- manylabs2 %>%
  filter(metaid %in% unique(old_manylabs$metaid)) %>%
  select(metaid, z, b, se, ss) %>%
  arrange(metaid) %>%
  group_by(metaid) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  inner_join(old_manylabs, by = c("metaid", "row"), suffix = c("", "_old")) %>%
  summarise(
    n = n(),
    z = max(abs(z - z_old)),
    b = max(abs(b - b_old)),
    se = max(abs(se - se_old)),
    ss = max(abs(ss - ss_old)),
    .groups = "drop"
  )

coverage <- manylabs2 %>%
  distinct(metaid, .keep_all = TRUE) %>%
  summarise(
    original_missing = list(metaid[is.na(orig.b) & is.na(orig.se) &
                                 is.na(orig.ss)]),
    original_b = sum(!is.na(orig.b)),
    original_se = sum(!is.na(orig.se)),
    original_ss = sum(!is.na(orig.ss)),
    .groups = "drop"
  )

stop_if_not(n_distinct(manylabs2$metaid) == 28, "Expected 28 analyses.")
stop_if_not(nrow(manylabs2) == 1592, "Expected 1,592 rows.")
stop_if_not(all(!is.na(manylabs2$z) & !is.na(manylabs2$b) &
                  !is.na(manylabs2$se) & !is.na(manylabs2$ss)),
            "Replication z, b, se, and ss should be complete.")
stop_if_not(all(manylabs2$measure == "r"), "Expected measure == 'r'.")
stop_if_not(all(manylabs2$z_operator == "="), "Expected exact z values.")
stop_if_not(setequal(unique(manylabs2$subset), c("primary", "secondary")),
            "Expected only primary and secondary subsets.")
stop_if_not(
  identical(as.integer(count(distinct(manylabs2, metaid, subset), subset)$n),
            c(23L, 5L)),
  "Expected 23 primary and 5 secondary analyses."
)
stop_if_not(all(old_comparison[1, c("z", "b", "se", "ss")] == 0),
            "Historical 25 analyses should match psymetadata exactly.")
stop_if_not(identical(coverage$original_missing[[1]],
                      c("Huang.1", "Alter.1", "Savani.3a")),
            "Unexpected analyses missing original-study data.")
stop_if_not(coverage$original_b == 23, "Expected original b for 23 analyses.")
stop_if_not(coverage$original_se == 22, "Expected original se for 22 analyses.")
stop_if_not(coverage$original_ss == 25, "Expected original ss for 25 analyses.")

print(manylabs2 %>% summarise(rows = n(), analyses = n_distinct(metaid)))
print(manylabs2 %>% distinct(metaid, subset) %>% count(subset))
print(old_comparison)
print(coverage)
