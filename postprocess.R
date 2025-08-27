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
  mutate(z_operator = ifelse(is.na(z_operator), "=", z_operator))# %>% 
  # large values make no sense in experimental research 
  # and are likely entry errors or rounding artefacts
  # mutate(z_operator = ifelse(abs(z) > 30, ">", z_operator)) %>% 
  # mutate(z = ifelse(abs(z) > 30, sign(z)*30, z))
# mutate(truncated  = ifelse(z_operator != "=", 1, 0))
# in some datasets some fraction of a % of studies have truncation going in the 
# opposite of "expected" direction,  think e.g. "p > 0.1"; easiest to remove them
# since in analysis we will assume that meaning of "truncated" flips as we cross ~1.96
# filter(!((z_operator == "<" & z > 1.96) | (z_operator == ">" & z <= 1.959)))

bear_list <- split(bear_processed, bear_processed$dataset)

bear_hash <- lapply(bear_list, digest)

# For test purposes, if there are too many rows
# first try to pick only one estimate per study
# then "thin it out"

thin_df <- function(df) {
  if(nrow(df) > 50000){
    # single observation per study; done in base R for speed
    indices <- aggregate(seq_len(nrow(df)), by = list(studyid = df$studyid), FUN = sample, size = 1)$x
    df <- df[indices, ]
  }
  if(nrow(df) > 50000)
    df <- df %>% slice_sample(n = 50000)
  
  # Calculate weights after this procedure
  df %>%   
    group_by(metaid, studyid) %>% 
    mutate(k = n()) %>% 
    ungroup() %>% 
    mutate(weights = 1/k) 
}

bear_list_thin <- bear_list %>% lapply(thin_df)

# This is not part of the repo, but useful to save it as it takes a moment to derive
save(bear_list, bear_list_thin, bear_hash, file = "data/bear_lists.Rdata")
