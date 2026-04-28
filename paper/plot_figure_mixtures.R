library(tidyverse)
library(patchwork)
source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R") # for powsignrep()
source("R/fit_density_calc.R")
source("R/plot_mixture.R")

mixture_inputs <- load_bear_mixture_inputs()

set.seed(42)

c_star <- 1.96

ds_tbl <- mixture_inputs$psr_table


# cur <- ds_tbl %>% filter(group == "curated") %>% arrange(desc(PoS)) %>% pull(dataset)
# met <- ds_tbl %>% filter(group == "meta")    %>% arrange(desc(PoS)) %>% pull(dataset)
curmet <- ds_tbl %>% 
  filter(group %in% c("curated", "meta")) %>% 
  arrange(desc(PoS)) %>% pull(dataset)
rep <- ds_tbl %>% filter(group == "replications") %>% pull(dataset)
scr <- ds_tbl %>% filter(group == "scrape")  %>% pull(dataset)

# plotlist <- lapply(c(cur, met, rep, scr), plot_bear_mixture_panel,
#                    inputs = mixture_inputs)
plotlist <- lapply(c(curmet, rep, scr), plot_bear_mixture_panel,
                   inputs = mixture_inputs)

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
