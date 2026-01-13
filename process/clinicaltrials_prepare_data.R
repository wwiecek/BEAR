# https://aact.ctti-clinicaltrials.org/downloads/
# https://aact.ctti-clinicaltrials.org/downloads/flatfiles_instructions

# packages
# install.packages(c("readr","dplyr","tidyr","stringr"))
library(readr); library(dplyr); library(tidyr); library(stringr)

dir <- "data_raw/clinicaltrials.gov/12082025"

# --- core tables ---
studies <- read_delim(
  file.path(dir, "studies.txt"), delim="|",
  col_select = c(nct_id, brief_title, overall_status, completion_date, study_type,
                 phase, enrollment, number_of_arms)
) %>% mutate(completion_date = as.Date(completion_date))

designs <- read_delim(
  file.path(dir, "designs.txt"), delim="|",
  col_types = cols(.default = "c"),
  col_select = c(nct_id, allocation, intervention_model, observational_model, primary_purpose, masking)
)

outcomes <- read_delim(
  file.path(dir, "outcomes.txt"), delim="|",
  col_types = cols(.default = "c"),
  col_select = c(nct_id, id, outcome_type, title, time_frame)
) %>% rename(outcome_id = id, outcome_title = title, outcome_time_frame = time_frame)

oa <- read_delim(
  file.path(dir, "outcome_analyses.txt"), delim="|",
  col_select = c(
    nct_id, outcome_id, id,
    param_type, param_value,
    ci_percent, ci_n_sides, ci_lower_limit, ci_upper_limit,
    p_value, p_value_modifier
  )
) %>%
  mutate(outcome_id = as.character(outcome_id)) %>% 
  rename(outcome_analysis_id = id) %>% 
  mutate(ci_n_sides = case_when(
    ci_n_sides == "ONE_SIDED" ~ 1,
    ci_n_sides == "TWO_SIDED" ~ 2,
    TRUE ~ NA
  ))


# --- put it all together ---
eff <- oa %>%
  left_join(outcomes, by = c("nct_id","outcome_id")) %>%
  left_join(studies,  by = "nct_id") %>%
  left_join(designs,  by = "nct_id") %>%
  select(
    nct_id, brief_title, overall_status, completion_date,
    phase, enrollment, number_of_arms,
    # designs
    allocation, study_type,
    intervention_model, observational_model, primary_purpose, masking,
    # outcomes
    outcome_id, outcome_type, outcome_title, outcome_time_frame,
    outcome_analysis_id,
    param_type, param_value,
    ci_percent, ci_n_sides, ci_lower_limit, ci_upper_limit,
    p_value, p_value_modifier
  ) 


saveRDS(eff, file = "data_raw/clinicaltrials.gov/clinicaltrials.gov_aug2025.rds") 

