# Select one BEAR-facing raw-derived scale while retaining all alternatives.
# Continuous raw-only candidates use SMD; binary raw-only candidates use probit.

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

source("process/clinicaltrials.gov/lib/paths.R")

candidate_path <- file.path(ctgov_derived_dir, "endpoint_candidates.rds")
selected_path <- file.path(ctgov_derived_dir, "raw_endpoint_selected_for_bear.rds")
nonselected_path <- file.path(ctgov_derived_dir, "raw_endpoint_nonselected_scales.rds")
comparison_path <- file.path(ctgov_validation_dir, "raw_scale_comparison_summary.csv")
trial_characteristics_path <- file.path(ctgov_derived_dir, "trial_characteristics.rds")

safe_divide <- function(x, y) ifelse(y != 0 & is.finite(y), x / y, NA_real_)

probit_rows <- function(binary_rows) {
  binary_rows %>%
    distinct(
      nct_id, outcome_id, measurement_stratum_id, group_t_id, group_c_id,
      .keep_all = TRUE
    ) %>%
    mutate(
      p_t = safe_divide(x_t, n_t),
      p_c = safe_divide(x_c, n_c),
      q_t = qnorm(p_t),
      q_c = qnorm(p_c),
      phi_t = dnorm(q_t),
      phi_c = dnorm(q_c),
      b = q_t - q_c,
      se = sqrt(
        safe_divide(p_t * (1 - p_t), n_t * phi_t^2) +
          safe_divide(p_c * (1 - p_c), n_c * phi_c^2)
      ),
      z = safe_divide(b, se),
      p = 2 * pnorm(-abs(z)),
      measure = "probit_difference",
      derivation_rule_id = str_c(derivation_rule_id, "_probit_delta"),
      effect_id = str_c("ctgov_raw_probit_", row_number())
    ) %>%
    filter(
      is.finite(p_t), is.finite(p_c), p_t > 0, p_t < 1, p_c > 0, p_c < 1,
      is.finite(b), is.finite(se), se > 0, is.finite(z)
    )
}

raw_candidates <- readRDS(candidate_path) %>%
  mutate(
    outcome_id = as.character(outcome_id),
    group_t_id = as.character(group_t_id),
    group_c_id = as.character(group_c_id),
    raw_group_pair_key = if_else(
      !is.na(group_t_id) & !is.na(group_c_id),
      if_else(group_t_id < group_c_id,
              str_c(group_t_id, group_c_id, sep = "||"),
              str_c(group_c_id, group_t_id, sep = "||")),
      NA_character_
    )
  )

completed_trials <- readRDS(trial_characteristics_path) %>%
  transmute(
    nct_id,
    completed_trial = str_to_lower(overall_status) == "completed"
  )

binary_probit <- raw_candidates %>%
  filter(effect_family == "binary", measure == "risk_difference") %>%
  probit_rows()

raw_with_probit <- bind_rows(raw_candidates, binary_probit) %>%
  left_join(completed_trials, by = "nct_id") %>%
  filter(completed_trial) %>%
  select(-completed_trial) %>%
  mutate(
    raw_event_t = x_t, raw_event_c = x_c,
    raw_n_t = n_t, raw_n_c = n_c,
    raw_mean_t = mean_t, raw_mean_c = mean_c,
    raw_sd_t = sd_t, raw_sd_c = sd_c,
    raw_group_t_id = group_t_id, raw_group_c_id = group_c_id,
    raw_group_t_title = group_t_title, raw_group_c_title = group_c_title,
    raw_measure = measure,
    raw_effect_family = effect_family
  )

selected_raw <- raw_with_probit %>%
  filter(
    (effect_family == "continuous" & measure == "standardized_mean_difference") |
      (effect_family == "binary" & measure == "probit_difference")
  ) %>%
  mutate(
    import_source = "raw_derived",
    import_recommendation = "selected_for_bear",
    source_path = "raw_endpoint_measurements",
    measure_class = case_when(
      effect_family == "continuous" ~ "Standardized Mean Difference",
      effect_family == "binary" ~ "Probit Difference",
      TRUE ~ NA_character_
    ),
    scale = case_when(
      effect_family == "continuous" ~ "smd",
      effect_family == "binary" ~ "probit",
      TRUE ~ NA_character_
    ),
    effect = b
  ) %>%
  arrange(nct_id, outcome_id, measurement_stratum_id, effect_family, measure)

nonselected_raw <- anti_join(
  raw_with_probit,
  selected_raw %>% select(effect_id),
  by = "effect_id"
) %>%
  mutate(import_recommendation = "retained_nonselected_scale")

scale_wide <- raw_with_probit %>%
  select(
    nct_id, outcome_id, measurement_stratum_id, raw_group_pair_key,
    effect_family, measure, b, se, z
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = c(b, se, z),
    values_fn = list(b = dplyr::first, se = dplyr::first, z = dplyr::first)
  )

comparison_summary <- bind_rows(
  scale_wide %>%
    filter(effect_family == "continuous") %>%
    summarise(
      section = "continuous_smd_vs_mean_difference",
      n_compared = sum(!is.na(z_standardized_mean_difference) &
        !is.na(z_mean_difference)),
      sign_changes = sum(sign(b_standardized_mean_difference) !=
        sign(b_mean_difference), na.rm = TRUE),
      significance_side_changes = sum((abs(z_standardized_mean_difference) >=
        1.96) != (abs(z_mean_difference) >= 1.96), na.rm = TRUE),
      median_abs_z_difference = median(abs(abs(z_standardized_mean_difference) -
        abs(z_mean_difference)), na.rm = TRUE)
    ),
  scale_wide %>%
    filter(effect_family == "binary") %>%
    summarise(
      section = "binary_probit_vs_log_or_rr_rd",
      n_compared = sum(!is.na(z_probit_difference) &
        (!is.na(z_log_odds_ratio) | !is.na(z_log_risk_ratio) |
          !is.na(z_risk_difference))),
      sign_changes = sum(
        sign(b_probit_difference) != sign(coalesce(
          b_log_odds_ratio, b_log_risk_ratio, b_risk_difference
        )),
        na.rm = TRUE
      ),
      significance_side_changes = sum(
        (abs(z_probit_difference) >= 1.96) !=
          (abs(coalesce(
            z_log_odds_ratio, z_log_risk_ratio, z_risk_difference
          )) >= 1.96),
        na.rm = TRUE
      ),
      median_abs_z_difference = median(abs(abs(z_probit_difference) -
        abs(coalesce(
          z_log_odds_ratio, z_log_risk_ratio, z_risk_difference
        ))), na.rm = TRUE)
    )
)

stopifnot(
  anyDuplicated(raw_candidates$effect_id) == 0,
  anyDuplicated(selected_raw$effect_id) == 0,
  all(selected_raw$nct_id %in% completed_trials$nct_id[completed_trials$completed_trial]),
  all(c("raw_event_t", "raw_event_c", "raw_n_t", "raw_n_c") %in%
        names(selected_raw)),
  all(!is.na(selected_raw$b)),
  all(!is.na(selected_raw$se)),
  all(!is.na(selected_raw$z))
)

saveRDS(selected_raw, selected_path)
saveRDS(nonselected_raw, nonselected_path)
write_csv(comparison_summary, comparison_path)

message("Saved ", nrow(selected_raw), " selected raw endpoint rows.")
