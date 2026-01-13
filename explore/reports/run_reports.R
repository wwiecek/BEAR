# Read all mixtures
mixture_fit_list <- list()
nms <- gsub(".rds", "", list.files("mixtures/"))
for(nm in nms) {
  fnm <- paste0("mixtures/", nm, ".rds")
  mixture_fit_list[[nm]] <- readRDS(fnm)
}

# Load BEAR data
bear <- readRDS("data/BEAR.rds")
bear_list <- 
  bear %>% 
  mutate(truncated = ifelse(is.na(truncated), "not truncated", truncated)) %>% 
  split(bear$dataset)

identical(names(bear_list), names(mixture_fit_list))

source("R/knit.R")
for(nm in nms)
  knit_with_df(bear_list[[nm]], 
               mixture_fit_list[[nm]], 
               output_file = paste0("results/reports/", nm, ".pdf"))




# WIP
# stitch_and_knit("brodeur_desc.Rmd", "template.Rmd", d, fit, "template_stitch.pdf")

# d <- bear_list[[nm]]
# fit <- mixture_fit_list[[nm]]






library(rstan)
library(tidybayes)
rstan_options(auto_write = TRUE)
sm_mixture <- stan_model("mixture.stan")
options(mc.cores = 4)
standt <- compose_data(bear_list$WWC %>% 
                         filter(!is.infinite(z)) %>% 
                         transmute(z = abs(z),
                                   w = 1/k,
                                   is_trunc = 0,
                                   is_high = abs(z) >= 1.96),
                       K = 4, .n_name = n_prefix("N"))
fit <- sampling(data = standt, sm_mixture, iter = 400)
