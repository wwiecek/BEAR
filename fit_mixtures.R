source("R/helpers.R")
source("R/mix.R")

bear <- readRDS("data/BEAR.rds")

bear_list <- 
  bear %>% 
  mutate(truncated = ifelse(is.na(truncated), "not truncated", truncated)) %>% 
  split(bear$dataset)

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

mixture_fit_list <- list()
for(nm in names(bear_list_thin)) {
  print(nm)
  df <- bear_list_thin[[nm]]
  mixture_fit_list[[nm]] <- fit_mixture(df$z, df$truncated, k = 4, weight = df$weight)
}

saveRDS(mixture_fit_list, "results/mixturesR.rds")
