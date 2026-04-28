library(tidyverse)
library(tictoc)
library(digest)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")
# source("R/mix_v2.R") #faster alternatives!

# bear <- readRDS("data/BEAR.rds")

load("paper/bear_lists.Rdata")

previous_hash <- if(file.exists("results/mixtures_hash.rds")) {
  readRDS("results/mixtures_hash.rds")
} else {
  list()
}

# Nelder-Mead optimisation we use here will take ~a few minutes (up to 10 maybe) per dataset
changed <- names(bear_hash)[
  !(names(bear_hash) %in% names(previous_hash)) |
    vapply(names(bear_hash), function(nm) {
      !identical(bear_hash[[nm]], previous_hash[[nm]])
    }, logical(1))
]
missing_fit <- names(bear_hash)[
  !file.exists(file.path("mixtures", paste0(names(bear_hash), ".rds")))
]
mtofit <- union(changed, missing_fit)

for(nm in mtofit) {
  fnm <- paste0("mixtures/", nm, ".rds")
  cat(nm); cat("\n")
  tic()
  df <- bear_list_thin[[nm]]
  cfit <- fit_mixture(z = df$z, operator = df$z_operator, weight = df$weights)
  saveRDS(cfit, fnm)
  toc()
}

saveRDS(bear_hash, file = "results/mixtures_hash.rds")
