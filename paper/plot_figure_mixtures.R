library(tidyverse)
library(patchwork)
source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/plot_mixture.R")
source("R/fit_density_calc.R")

mfl <- load_all_mixtures()
load("paper/bear_lists.Rdata")

c_star <- 1.96
ds_tbl <- tibble(dataset = names(mfl),
                 group = bear_classification[dataset]) %>% 
  filter(!(dataset %in% paper_do_not_include))
cols_grp <- c(curated = "#E41A1C", meta = "#377EB8", scrape = "#4DAF4A")

mk_small <- function(ds) {
  dt <- bear_list_thin[[ds]] %>% mutate(group = bear_classification[ds])
  plot_mixture_v4(mfl[[ds]], dt, nm = bear_names[ds], 
                  color_map = cols_grp, nbreaks = 25,
                  show_corrected = T)
}

cur <- ds_tbl %>% filter(group == "curated") %>% pull(dataset)
met <- ds_tbl %>% filter(group == "meta")    %>% pull(dataset)
scr <- ds_tbl %>% filter(group == "scrape")  %>% pull(dataset)

row1  <- lapply(cur, mk_small)
row23 <- lapply(met, mk_small)
row4  <- lapply(scr, mk_small)

wrap_plots(c(row1, row23, row4), ncol = 4)

ggsave("paper/figures/mixtures_plot.pdf", height = 16.5, width = 14, units = "cm")
