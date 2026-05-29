library(tidyverse)
library(tictoc)
library(digest)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")
# source("R/mix_v2.R") #faster alternatives!
set.seed(1990)

bear_list <-
  readRDS("BEAR.rds") %>%
  mutate(z_operator = ifelse(is.na(z_operator), "=", z_operator)) %>%
  # Choice of subsets, merging of datasets etc. for BEAR modelling paper
  # for now only...
  # (1) We merge EUDRA CT and clinicaltrials.gov into a single database of trials
  mutate(dataset = ifelse(dataset == "clinicaltrials" | dataset == "euctr",  "ctgov_euctr", dataset)) %>%
  # Divide into the relevant datasets (which may be processed further individually)
  split(., .$dataset) %>% 
  lapply(calc_study_weights)

# Add original-study distributions for the replication datasets.
original_bear_list <- list(
  "SCORE original" = bear_list[["SCORE_replications"]],
  "Many Labs original" = bear_list[["ManyLabs2"]] %>%
    distinct(metaid, .keep_all = TRUE) %>%
    mutate(studyid = metaid),
  "OSC original" = bear_list[["OSC"]]
) %>%
  map(~ .x %>%
        dplyr::filter(!is.na(orig.z)) %>%
        transmute(
          metaid, studyid, method, measure, subset,
          z = orig.z,
          b = orig.b,
          se = orig.se,
          year, field,
          ss = orig.ss,
          z_operator = ifelse(is.na(orig.z_operator), "=", orig.z_operator),
          p = orig.p,
          outcome_group, group, n, source
        ) %>%
        calc_study_weights())

bear_list <- c(bear_list, original_bear_list)

bear_hash <- lapply(bear_list, digest)

bear_list_thin <- lapply(bear_list, thin_df)

previous_hash <- if(file.exists("results/mixtures_hash.rds")) {
  readRDS("results/mixtures_hash.rds")
} else {
  list()
}

# Find out what changed
changed <- names(bear_hash)[
  !(names(bear_hash) %in% names(previous_hash)) |
    vapply(names(bear_hash), function(nm) {
      !identical(bear_hash[[nm]], previous_hash[[nm]])
    }, logical(1))
]
missing_fit <- names(bear_hash)[
  !file.exists(file.path("mixtures", paste0(names(bear_hash), ".rds")))
]
mtofit <- union(changed, missing_fit)

# Nelder-Mead optimisation we use here will take ~a few minutes 
# (up to 10 maybe) per dataset
for(nm in mtofit) {
  fnm <- paste0("mixtures/", nm, ".rds")
  cat(nm); cat("\n")
  tic()
  df <- bear_list_thin[[nm]]
  cfit <- fit_mixture(z = df$z, operator = df$z_operator, weights = df$weights)
  saveRDS(cfit, fnm)
  toc()
}

# This is not part of the repo, but useful to save it as it takes a moment to derive
# save(bear_list, bear_list_thin, bear_hash, file = "transformed_data/bear_lists.Rdata")
save(bear_list_thin, bear_hash, file = "paper/bear_lists.Rdata")
saveRDS(bear_hash, file = "results/mixtures_hash.rds")
