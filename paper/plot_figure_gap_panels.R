# library(geomtextpath)
library(tidyverse)
library(patchwork)
library(geomtextpath)

source("R/settings.R")
source("R/psr.R")
source("R/helpers.R")
source("R/mix.R")

mfl <- load_all_mixtures(exclude = paper_do_not_include)

small_gap_plot <- function(fit, single = FALSE){
  z  <- seq(0,5,0.1)
  pr <- gap_vec(z, p = fit$p, m = fit$m, s_snr = fit$sigma_SNR)
  df <- data.frame(z, row.names = NULL) %>% mutate(sgn = pr$sgn, rep = pr$rep)  %>%
    pivot_longer(cols=c('sgn', 'rep'),
                 names_to='label',
                 values_to='prob') %>%
    mutate(label=recode_factor(label,
                               "rep" = "successful replication",
                               "sgn" = "correct sign"))


  snr <- rmix(1e05, p = fit$p, m = fit$m, s = fit$sigma_SNR)
  pow <- mean((1 - pnorm(1.96, snr, 1)) + pnorm(-1.96, snr, 1))

  ggplot(df,aes(x=z,y=prob,group=label, color = label)) +
    geom_abline(intercept=pow,slope=0,linetype="dashed",alpha=0.5) +
    geom_line() +
    {if(single) geom_textline(aes(label = label),vjust=1.5,size=3.5)} +
    # scale_y_continuous(minor_breaks = seq(0,1,0.05), breaks = seq(0,1,0.1),lim=c(0,1)) +
    scale_y_continuous(breaks = seq(0,1,0.25),lim=c(0,1)) +
    scale_x_continuous(minor_breaks = seq(0,5,0.5),
                       breaks = seq(0,5,1),lim=c(0,5)) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(legend.position = "none")
}
# 
# small_gap_plot(mfl$clinicaltrials, single = T)
# 
# ggsave("paper/figures/gap_plot_one.pdf", height = 9, width = 12, units = "cm")



# Arranging all of the plots on a single page
ds_tbl <- read_csv("power_sign_rep.csv") %>% 
  mutate(group = bear_classification[dataset]) %>%
  transmute(dataset, group, PoS = assurance) 

mk_small_gap <- function(ds)
  small_gap_plot(mfl[[ds]]) +
  labs(title = bear_labels[ds]) +
  theme(plot.title  = element_text(size = 8),
        axis.text   = element_text(size = 7),
        axis.title.x  = element_text(size = 7),
        legend.position = "none")

curmet <- ds_tbl %>% 
  filter(group %in% c("curated", "meta")) %>% 
  arrange(desc(PoS)) %>% pull(dataset)
rep <- ds_tbl %>% filter(group == "replications")  %>% pull(dataset)
scr <- ds_tbl %>% filter(group == "scrape")  %>% pull(dataset)

plotlist <- lapply(c(curmet, rep, scr), mk_small_gap)
plotlist <- append(plotlist, list(plot_spacer()), after = 11)
wrap_plots(plotlist, ncol = 4) +
  plot_annotation(caption = "|z|") &
  theme(
    plot.caption = element_text(hjust = 0.5),
    axis.text    = element_text(size = 8 * 0.75)
  )

ggsave("paper/figures/gap_plot_all.pdf", height = 17, width = 14, units = "cm")
