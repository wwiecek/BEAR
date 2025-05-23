library(tidyverse)
library(digest)
source("R/helpers.R")
source("R/mix.R")

bear <- readRDS("data/BEAR.rds")

bear_list <- 
  bear %>% 
  mutate(truncated = ifelse(is.na(truncated), "not truncated", truncated)) %>% 
  split(bear$dataset)

bear_hash <- lapply(bear_list, digest)

# For test purposes, pick only up to 10,000 rows from each dataset
# This may take a while to run, because 
bear_list_thinned <- bear_list %>% lapply(function(df) {
  if(nrow(df) > 10000)
    df <- df %>% group_by(studyid) %>% slice_sample(n = 1) %>% mutate(k = 1)
  if(nrow(df) > 10000)
    df <- df %>% slice_sample(n = 10000)
  df %>% 
    mutate(weight = 1/k)
})
bear_list_thin <- bear_list %>% lapply(function(df) {
  if(nrow(df) > 10000){
    # done in base R for speed
    indices <- aggregate(seq_len(nrow(df)), by = list(studyid = df$studyid), FUN = sample, size = 1)$x
    df <- df[indices, ]
  }
  if(nrow(df) > 10000)
    df <- df %>% slice_sample(n = 10000)
  df %>% 
    mutate(weight = 1/k)
})

previous_hash <- readRDS("results/mixtures_hash.rds")

# Each takes 1-10 minutes per dataset on my puny laptop
for(nm in names(bear_list_thin)) {
  fnm <- paste0("results/mixtures/", nm, ".rds")
  if(!file.exists(fnm) || previous_hash[[nm]] != bear_hash[[nm]]) {
    cat(nm); cat("\n")
    df <- bear_list_thin[[nm]]
    cfit <- fit_mixture(df$z, df$truncated, k = 4, weight = df$weight)
    saveRDS(cfit, fnm)
  }
}
