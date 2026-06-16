# Plot fitted selection strength against replication probability among
# significant original results.

library(tidyverse)
library(ggrepel)
library(scales)

source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R")
source("R/plot_omega_repl.R")

omega_repl <- build_omega_repl_data()

omega_vs_repl <- plot_omega_repl(
  omega_repl,
  omega_scale = "odds"
)

ggsave("paper/figures/omega_vs_repl.pdf", omega_vs_repl,
       width = 18, height = 13, units = "cm")
