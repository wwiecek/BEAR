library(tidyverse)
library(patchwork)
source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
mfl <- load_all_mixtures()
load("transformed_data/bear_lists.Rdata")

c_star <- 1.96
ds_tbl <- tibble(dataset = names(mfl),
                 group = bear_classification[dataset])
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
row23 <- { p <- lapply(met, mk_small)
c(p, replicate(max(0, 8 - length(p)), plot_spacer(), simplify = FALSE)) }
row4  <- lapply(scr, mk_small)

wrap_plots(c(row1, row23, row4), ncol = 4)

ggsave("paper/figures/mixtures_plot_pmixonly.pdf", height = 16.5, width = 14, units = "cm")
