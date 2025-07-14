library(tidyverse)
library(tictoc)
library(digest)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")
source("R/mix_v2.R") #faster alternatives!

bear <- readRDS("data/BEAR.rds")

set.seed(1990)

bear_processed <- 
  bear %>% 
  mutate(z_operator = ifelse(is.na(z_operator), "=", z_operator)) %>% 
  mutate(truncated  = ifelse(z_operator != "=", 1, 0)) %>% 
  # in some datasets some fraction of a % of studies have truncation going in the 
  # opposite of "expected" direction,  think e.g. "p > 0.1"; easiest to remove them
  # since in analysis we will assume that meaning of "truncated" flips as we cross ~1.96
  filter(!((z_operator == "<" & z > 1.96) | (z_operator == ">" & z <= 1.959)))
  
bear_list <- split(bear_processed, bear_processed$dataset)



bear_hash <- lapply(bear_list, digest)
previous_hash <- readRDS("results/mixtures_hash.rds")
for(nm in names(bear_hash)){
  if(bear_hash[[nm]] != previous_hash[[nm]])
    cat(nm)
}


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
    # large values make no sense in experimental research 
    # and are likely entry errors or rounding artefacts
    mutate(z = ifelse(abs(z) > 100, sign(z)*100, z)) %>% 
    select(-j)
})


# Nelder-Mead will take minutes, sometimes 10+ per dataset
# L-BFGS will take <1 min. and has been shown to give similar results
# although the optimisation constraints are shoddy
mixture_fit_list <- list()
mtofit <- names(bear_list_thin)
for(nm in mtofit) {
  fnm <- paste0("results/mixtures/", nm, ".rds")
  if(!file.exists(fnm) || !(nm %in% names(previous_hash)) || previous_hash[[nm]] != bear_hash[[nm]]) {
    cat(nm); cat("\n")
    tic()
    df <- bear_list_thin[[nm]]
    mixture_fit_list[[nm]] <- fit_mixture(z = df$z, 
                                          operator = df$z_operator,
                                          # truncated = df$truncated, 
                                          weight = df$weight, 
                                          optimiser = "L-BFGS")
    saveRDS(mixture_fit_list[[nm]], fnm)
    toc()
  }
}

saveRDS(bear_hash, file="results/mixtures_hash.rds")




# Plot some figures of mixtures -----

# Years of studies (after thinning)

bind_rows(bear_list_thin) %>% 
  ggplot(aes(year)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") + 
  coord_cartesian(xlim = c(1980, 2020)) +
  ylab("N studies (after thinning)") + xlab("year (of study/publication)")

ggsave(width = 5, height = 5, file = "results/years_thinned.pdf")


# Look at all the plots
pl <- list()
# read mixtures
mixture_fit_list <- list()
nms <- gsub(".rds", "", list.files("results/mixtures/"))
for(nm in nms) {
  fnm <- paste0("results/mixtures/", nm, ".rds")
  mixture_fit_list[[nm]] <- readRDS(fnm)
}

for(nm in names(bear_list_thin)) {
  pl[[nm]] <- plot_mixture(mixture_fit_list[[nm]],
                           bear_list_thin[[nm]]$z,
                           bear_list_thin[[nm]]$weight)
}
library(gridExtra)
combined_plot <- arrangeGrob(grobs = Map(function(p, name) {
  p + ggtitle(bear_names[[name]])
}, pl, names(pl)), ncol = 5)

ggsave("results/mixtures_plot.pdf", combined_plot, width = 16, height = 9)
