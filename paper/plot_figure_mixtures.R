library(tidyverse)
library(patchwork)
source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/plot_mixture.R")
source("R/psr.R") # for powsignrep()
source("R/fit_density_calc.R")

mfl <- load_all_mixtures()
load("paper/bear_lists.Rdata")

set.seed(42)

c_star <- 1.96
ds_tbl <- tibble(dataset = names(mfl),
                 group = bear_classification[dataset]) %>% 
  filter(!(dataset %in% paper_do_not_include)) %>%
  mutate(PoS = map_dbl(dataset, ~ mean(powsignrep(mfl[[.x]])$power))) 
  # mutate(annotate_text = paste0("bar(PoS) ==", round(PoS,2)))

mk_small <- function(ds) {
  # print(ds)
  dt <- bear_list_thin[[ds]] %>% mutate(group = bear_classification[ds])
  plot_mixture_v4(mfl[[ds]], dt, nm = bear_labels[ds], 
                  color_map = bear_colors, nbreaks = 25,
                  meanpwr = round(ds_tbl$PoS[ds_tbl$dataset == ds], 2),
                  show_corrected = T)
}

# cur <- ds_tbl %>% filter(group == "curated") %>% arrange(desc(PoS)) %>% pull(dataset)
# met <- ds_tbl %>% filter(group == "meta")    %>% arrange(desc(PoS)) %>% pull(dataset)
curmet <- ds_tbl %>% 
  filter(group %in% c("curated", "meta")) %>% 
  arrange(desc(PoS)) %>% pull(dataset)
rep <- ds_tbl %>% filter(group == "replications")  %>% pull(dataset)
scr <- ds_tbl %>% filter(group == "scrape")  %>% pull(dataset)

# plotlist <- lapply(c(cur, met, rep, scr), mk_small)
plotlist <- lapply(c(curmet, rep, scr), mk_small)

plotlist <- append(plotlist, list(plot_spacer()), after = 11)

wrap_plots(plotlist, ncol = 4) +
  plot_annotation(caption = "|z|") &
  theme(
    plot.caption = element_text(hjust = 0.5),
    axis.text    = element_text(size = 8 * 0.75)
    # plot.title = element_text(size = 8 * 0.75),
    # axis.text.y  = element_blank(),
    # axis.ticks.y = element_blank()
  )

ggsave("paper/figures/mixtures_plot.pdf", height = 17, width = 14, units = "cm")
