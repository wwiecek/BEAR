library(tidyverse)
library(tictoc)
library(digest)
library(gridExtra)
library(patchwork)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")

bear <- readRDS("data/BEAR.rds")

cdsr_sub <- bear %>% filter(dataset == "Cochrane") %>% 
  mutate(z_operator = ifelse(is.na(z_operator), "=", z_operator)) %>%  
  calc_study_weights() %>% split(.$group)
cdsr_fits <- lapply(cdsr_sub, fit_mixture_df)  

save(cdsr_sub, cdsr_fits, file = "paper/results/cdsr_subset_fits.Rdata")

# What if we only look at RCTs
cdsr_rct_sub <- cdsr_sub[pl_choose[c(3, 4)]]
cdsr_rct_fits <-  cdsr_rct_sub %>% 
  lapply(function(x) filter(x, method == "RCT")) %>% 
  lapply(fit_mixture_df)

# Plotting
cdsr_pl <- list()
for(nm in names(cdsr_fits)) 
  cdsr_pl[[nm]] <- plot_mixture_v3(cdsr_fits[[nm]], cdsr_sub[[nm]], 
                                   nbreaks = 40,
                                   ymax = 0.6) + ggtitle(nm)
# wrap_plots(cdsr_pl, ncol = 4)

ezvec <- cdsr_sub %>% lapply(function(x) mean(abs(x$z))) %>% unlist %>% sort
pl_choose <- names(ezvec)[c(1,2,length(ezvec)-1, length(ezvec))]
# wrap_plots(cdsr_pl[pl_choose], ncol = 4)

save(cdsr_sub, cdsr_fits, cdsr_pl, pl_choose, file = "paper/results/cdsr_subset_fits.Rdata")

cdsr_rct_pl <- list()
for(nm in names(cdsr_rct_fits)) 
  cdsr_rct_pl[[nm]] <- plot_mixture_v3(cdsr_rct_fits[[nm]], cdsr_rct_sub[[nm]], 
                                       nbreaks = 40, ymax = 0.6) + ggtitle(paste(nm, "RCT"))

# wrap_plots(cdsr_pl[pl_choose], ncol = 4)
wrap_plots(c(cdsr_pl[pl_choose[c(1,2)]], cdsr_rct_pl), ncol = 4)

ggsave("paper/figures/mixtures_plot_cochrane_4.pdf", height = 5, width = 16, units = "cm")




# Combined plot (load clinicaltrials_subsets.R outputs first)
wrap_plots(c(cdsr_pl[pl_choose[c(1,2)]], cdsr_rct_pl, ct_pl[2:5]), ncol = 4)

(wrap_plots(c(cdsr_pl[pl_choose[c(1,2)]], cdsr_rct_pl), ncol = 4) + 
    plot_annotation(title = "First Row Title")) /
  (wrap_plots(c(ct_pl[2:5]), ncol = 4) + 
     plot_annotation(title = "Second Row Title"))


wrap_plots(cdsr_pl[pl_choose[c(1,2)]], ncol = 2, tag_level = "new") + plot_annotation(title = "First Row") |
  wrap_plots(c(cdsr_rct_pl, ct_pl[2:5]), ncol = 4) + plot_annotation(title = "Second Row")
