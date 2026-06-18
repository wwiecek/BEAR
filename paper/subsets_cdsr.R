# Fit Cochrane specialty mixtures used by the paper subset figure.
# Run from the BEAR project root.

library(tidyverse)
library(tictoc)
library(digest)
library(gridExtra)
library(patchwork)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")

bear <- readRDS("BEAR.rds")

# Fitting ------

cdsr_sub <- bear %>% filter(dataset == "Cochrane") %>% 
  mutate(z_operator = ifelse(is.na(z_operator), "=", z_operator)) %>%  
  calc_study_weights() %>% 
  split(.$subset)

cdsr_fits <- lapply(cdsr_sub, fit_mixture_df)  

# 2 worst and 2 best subdomains
ezvec <- cdsr_sub %>% lapply(function(x) mean(abs(x$z))) %>% unlist %>% sort
pl_choose <- names(ezvec)[c(1,2,length(ezvec)-1, length(ezvec))]

# What if we only look at RCTs
cdsr_rct_sub <- cdsr_sub[pl_choose[c(3, 4)]]
cdsr_rct_fits <-  cdsr_rct_sub %>% 
  lapply(function(x) filter(x, method == "RCT")) %>% 
  lapply(fit_mixture_df)

save(cdsr_sub, cdsr_fits, cdsr_rct_sub, cdsr_rct_fits, ezvec, pl_choose, 
     file = "paper/results/cdsr_subset_fits.Rdata")
