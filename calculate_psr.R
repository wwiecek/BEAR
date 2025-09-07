# analytical workflow for each dataset
library(tidyverse)
library(ggplot2)

source("R/settings.R")
source("R/exaggeration.R")
source("R/gap.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R")


mfl <- load_all_mixtures()

# Calculate the derived quantities -----

#will take a while to calculate for 100,000 rows per dataset without vectorising gap()

df_psr <- lapply(mfl, psr_foo) %>% bind_rows(.id = "dataset")
df_psr_169 <- lapply(mfl, psr_foo, 1.69) %>% bind_rows(.id = "dataset")

# saveRDS(df_psr, file = "results/power_sign_replication.rds")

