library(tidyverse)
library(patchwork)
source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R") # for powsignrep()
source("R/fit_density_calc.R")
source("R/plot_mixture.R")

load_bear_mixture_inputs()

set.seed(42)

c_star <- 1.96

ds_tbl <- psr_table


# cur <- ds_tbl %>% filter(group == "curated") %>% arrange(desc(PoS)) %>% pull(dataset)
# met <- ds_tbl %>% filter(group == "meta")    %>% arrange(desc(PoS)) %>% pull(dataset)
curmet <- ds_tbl %>% 
  dplyr::filter(group %in% c("curated", "meta")) %>% 
  arrange(desc(PoS)) %>% pull(dataset)
orig <- c("SCORE original", "Many Labs original", "OSC original")
rep <- ds_tbl %>%
  dplyr::filter(group == "replications", !(dataset %in% orig)) %>%
  pull(dataset)
scr <- ds_tbl %>%
  dplyr::filter(group == "scrape") %>%
  pull(dataset)

plotlist <- lapply(c(curmet, rep, scr), plot_bear_mixture_panel)

plotlist <- append(plotlist, list(plot_spacer(), plot_spacer()), after = 18)
plotlist <- c(plotlist, lapply(orig, plot_bear_mixture_panel), list(plot_spacer()))

wrap_plots(plotlist, ncol = 4) +
  plot_annotation(caption = "|z|") &
  theme(
    plot.caption = element_text(hjust = 0.5),
    axis.text    = element_text(size = 8 * 0.75)
    # plot.title = element_text(size = 8 * 0.75),
    # axis.text.y  = element_blank(),
    # axis.ticks.y = element_blank()
  )

ggsave("paper/figures/mixtures_plot.pdf", height = 23, width = 14, units = "cm")
