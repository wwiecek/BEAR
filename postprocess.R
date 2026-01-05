# This is the post-processing done to fit mixture distributions to BEAR datasets
# which we present and discuss in detail in the accompanying paper
# The steps here are really minimal

library(tidyverse)
library(tictoc)
library(digest)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")
# source("R/mix_v2.R") #faster alternatives!

bear <- readRDS("data/BEAR.rds")

set.seed(1990)

bear_processed <- 
  bear %>% 
  mutate(z_operator = ifelse(is.na(z_operator), "=", z_operator)) %>% 
  calc_study_weights() # Calculate the weights = 1/n rows per study

bear_list <- split(bear_processed, bear_processed$dataset)

# For BEAR modelling paper we merge EUDRA CT and clinicaltrials.gov into a single database of trials
bear_list[["ctgov_euctr"]] <- rbind(bear_list[["clinicaltrials"]], bear_list[["euctr"]])
bear_list[["clinicaltrials"]] <- bear_list[["euctr"]] <- NULL

bear_hash <- lapply(bear_list, digest)

# If there are too many rows, first try to pick only one estimate per study then "thin it out"
thin_df <- function(df) {
  if(nrow(df) > 50000){
    # single observation per study; done in base R for speed
    indices <- aggregate(seq_len(nrow(df)), by = list(studyid = df$studyid), FUN = sample, size = 1)$x
    df <- df[indices, ]
  }
  if(nrow(df) > 50000)
    df <- df %>% slice_sample(n = 50000)
  
  # Re-calculate weights after this procedure
  calc_study_weights(df)
}

bear_list_thin <- bear_list %>% lapply(thin_df)

# This is not part of the repo, but useful to save it as it takes a moment to derive
# save(bear_list, bear_list_thin, bear_hash, file = "transformed_data/bear_lists.Rdata")
save(bear_list_thin, bear_hash, file = "paper/bear_lists.Rdata")
