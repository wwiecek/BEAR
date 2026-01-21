library(tidyverse)
library(tictoc)
library(digest)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")
# source("R/mix_v2.R") #faster alternatives!

# bear <- readRDS("data/BEAR.rds")

load("paper/bear_lists.Rdata")

# previous_hash <- readRDS("results/mixtures_hash.rds")
# for(nm in names(bear_hash)){
#   if(bear_hash[[nm]] != previous_hash[[nm]])
#     cat(nm)
# }

# Nelder-Mead optimisation we use here will take ~a few minutes (up to 10 maybe) per dataset
mtofit <- names(bear_list_thin)

for(nm in mtofit) {
  fnm <- paste0("mixtures/", nm, ".rds")
  # if(!file.exists(fnm) || !(nm %in% names(previous_hash)) || previous_hash[[nm]] != bear_hash[[nm]]) {
  if(!file.exists(fnm)) {
    cat(nm); cat("\n")
    tic()
    df <- bear_list_thin[[nm]]
    cfit <- fit_mixture(z = df$z, 
                        operator = df$z_operator,
                        weight = df$weights)
    saveRDS(cfit, fnm)
    toc()
  }
}

saveRDS(bear_hash, file="results/mixtures_hash.rds")
