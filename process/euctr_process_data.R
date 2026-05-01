# Process EU Clinical Trials Register primary endpoints into BEAR-ready
# z-values, using the shared trial p-value/CI derivation workflow.

library(dplyr)
library(lubridate)

source("R/z_derivation_helpers.R")

in_path <- "data_raw/eutrials/data_euctr.rds"
out_path <- "data/euctr.rds"

d_raw <- readRDS(in_path)

d_primary <- d_raw %>%
  ungroup() %>%
  filter(endpoint == "primary")

d_clean <- d_primary %>%
  mutate(
    p_raw = coalesce(pval.calc, pval),
    p_operator_raw = ifelse(is.na(trunc), "=", trunc)
  ) %>%
  bind_cols(
    derive_trial_z(
      estimate = .$estimate,
      lower = .$lower,
      upper = .$upper,
      p_value = .$p_raw,
      p_operator = .$p_operator_raw,
      ci_level = .$level,
      ci_sides = .$side,
      p_sides = .$side,
      measure_label = .$estimand,
      default_ci_level = 95
    )
  ) %>%
  mutate(
    b = b,
    completion_date = as.Date(completion_date),
    year = year(coalesce(completion_date, as.Date(date)))
  ) %>%
  filter(!is.na(z)) %>%
  mutate(
    phase = as.character(phase),
    phase = case_when(
      phase == "phase 1"       ~ "phase1",
      phase == "phase 1+2"     ~ "phase1/phase2",
      phase == "phase 2"       ~ "phase2",
      phase == "phase 2+3"     ~ "phase2/phase3",
      phase == "phase 3"       ~ "phase3",
      phase == "phase 4"       ~ "phase4",
      phase == "phase 1+2+3"   ~ "phase2/phase3",
      phase == "phase 3+4"     ~ "phase4",
      phase == "phase 2+4"     ~ "phase4",
      phase == "phase 1+2+3+4" ~ "phase4",
      TRUE ~ NA_character_
    )
  )

validation_checks <- tibble::tibble(
  check = c(
    "required_columns",
    "valid_z_operator_values",
    "all_saved_rows_have_z",
    "no_output_row_gain"
  ),
  passed = c(
    all(c("id", "measure_class", "z", "b", "se", "z_operator") %in%
          names(d_clean)),
    all(is.na(d_clean$z_operator) |
          d_clean$z_operator %in% c("=", "<", ">")),
    all(!is.na(d_clean$z)),
    nrow(d_clean) <= nrow(d_primary)
  )
)

print(validation_checks, n = nrow(validation_checks), width = Inf)
stopifnot(all(validation_checks$passed))

saveRDS(d_clean, out_path)
