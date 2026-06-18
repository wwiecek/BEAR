# Fit clinicaltrials.gov subgroup mixtures used by the paper subset figure.
# Run from the BEAR project root.

library(tidyverse)
library(tictoc)
library(digest)
library(gridExtra)
library(patchwork)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")

df_ct_raw <- readRDS("data/clinicaltrialsgov.rds")
df_ct <- df_ct_raw %>%
  group_by(nct_id) %>%
  filter(n() < 20) %>%
  ungroup() %>%
  transmute(
    studyid = nct_id,
    study_type = tolower(study_type),
    allocation = tolower(allocation),
    primary_purpose = tolower(primary_purpose),
    masking = tolower(masking),
    year = year,
    phase = tolower(phase),
    method = "RCT",
    measure = measure_class,
    z = z,
    b = effect,
    se = se,
    ss = enrollment,
    z_operator = "="
  )

ct_subsets <- list(
  obs = df_ct %>% filter(study_type == "observational"),
  ph1 = df_ct %>% filter(primary_purpose == "treatment", phase == "phase1"),
  ph2 = df_ct %>% filter(primary_purpose == "treatment", phase == "phase2"),
  ph3 = df_ct %>% filter(primary_purpose == "treatment", phase == "phase3"),
  ph4 = df_ct %>% filter(primary_purpose == "treatment", phase == "phase4"),
  ph3lrg = df_ct %>% filter(primary_purpose == "treatment", phase == "phase3", ss > 1000),
  ph3sml = df_ct %>% filter(primary_purpose == "treatment", phase == "phase3", ss < 1000),
  ph3best = df_ct %>% filter(primary_purpose == "treatment", phase == "phase3", ss > 1000, masking != "none")
) %>% 
  lapply(function(x) x %>% group_by(studyid) %>% mutate(weights = 1/n()) %>% ungroup())

ct_fits <- lapply(ct_subsets, fit_mixture_df)
save(ct_subsets, ct_fits, file = "paper/results/clinicaltrials_subset_fits.Rdata")

