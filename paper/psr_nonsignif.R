library(tidyverse)

source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R")

set.seed(1990)

keep_sets <- names(bear_classification[bear_classification %in% c("meta", "curated")])
keep_sets <- setdiff(keep_sets, paper_do_not_include)

bear_ns <- readRDS("BEAR.rds") %>%
  mutate(
    dataset = ifelse(dataset %in% c("clinicaltrials", "euctr"), "ctgov_euctr", dataset),
    z_operator = ifelse(is.na(z_operator), "=", z_operator)
  ) %>%
  filter(dataset %in% keep_sets, abs(z) < 1.96)

if (nrow(bear_ns) > 50000) {
  bear_ns <- bear_ns %>% slice_sample(n = 50000)
}

bear_ns <- calc_study_weights(bear_ns)
fit <- fit_mixture_df(bear_ns)

save(fit, bear_ns, file = "paper/results/nonsignif_mixture.Rdata")

psr <- powsignrep(fit) %>%
  summarise(
    power = mean(power),
    sign = mean(sgn),
    replication = mean(rep)
  ) %>%
  mutate(n = nrow(bear_ns), datasets = length(unique(bear_ns$dataset)))

print(psr)
