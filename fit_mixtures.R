library(tidyverse)
library(tictoc)
library(digest)
source("R/helpers.R")
source("R/mix.R")
source("R/mix_v2.R")

bear <- readRDS("data/BEAR.rds")

bear_list <- 
  bear %>% 
  mutate(truncated = ifelse(is.na(truncated), "not truncated", truncated)) %>% 
  mutate(truncated = ifelse(truncated == "truncated", 1, 0)) %>% 
  split(bear$dataset)

bear_hash <- lapply(bear_list, digest)

# For test purposes, if there are too many rows
# first try to pick only one estimate per study
# then "thin it out"
bear_list_thin <- bear_list %>% lapply(function(df) {
  if(nrow(df) > 25000){
    # single observation per study; done in base R for speed
    indices <- aggregate(seq_len(nrow(df)), by = list(studyid = df$studyid), FUN = sample, size = 1)$x
    df <- df[indices, ]
  }
  if(nrow(df) > 25000)
    df <- df %>% slice_sample(n = 25000)
  
  # Calculate weights after this procedure
  df %>%   
    group_by(metaid, studyid) %>% 
    mutate(k = n()) %>% 
    ungroup() %>% 
    mutate(weight = 1/k) %>% 
    select(-j)
})

previous_hash <- readRDS("results/mixtures_hash.rds")

# Nelder-Mead will take minutes, sometimes 10+ per dataset
# L-BFGS will take <1 min. and has been shown to give similar results
# although the optimisation constraints are shoddy
mixture_fit_list <- list()
for(nm in names(bear_list_thin)) {
  fnm <- paste0("results/mixtures/", nm, ".rds")
  if(!file.exists(fnm) || previous_hash[[nm]] != bear_hash[[nm]]) {
    cat(nm); cat("\n")
    tic()
    df <- bear_list_thin[[nm]]
    mixture_fit_list[[nm]] <- fit_mixture(z = df$z, 
                                          truncated = df$truncated, 
                                          weight = df$weight, 
                                          optimiser = "L-BFGS")
    saveRDS(mixture_fit_list[[nm]], fnm)
    toc()
  }
}



# Look at all the plots
pl <- list()
for(nm in nms) {
  pl[[nm]] <- plot_mixture(mixture_fit_list[[nm]],
                           bear_list_thin[[nm]]$z,
                           bear_list_thin[[nm]]$weight)
}
library(gridExtra)
grid.arrange(grobs = Map(function(p, name) {
  p + ggtitle(name)
}, pl, names(pl)), ncol = 4)

