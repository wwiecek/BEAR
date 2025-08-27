# library(geomtextpath)
library(tidyverse)
library(patchwork)
library(geomtextpath)

source("R/settings.R")
source("R/exaggeration.R")
source("R/gap.R")
source("R/helpers.R")
source("R/mix.R")

mfl <- load_all_mixtures()

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
    ylab("") + xlab("|z|") +
    theme_bw() +
    theme(legend.position = "none")
}

small_gap_plot(mfl$clinicaltrials, single = T)

ggsave("results/gap_plot_one.pdf", height = 9, width = 12, units = "cm")



# Arranging all of the plots on a single page
ds_tbl <- tibble(dataset = names(mfl),
                 group   = bear_classification[dataset])

mk_small_gap <- function(ds)
  small_gap_plot(mfl[[ds]]) +
  labs(title = ds) +
  theme(plot.title  = element_text(size = 9),
        axis.text   = element_text(size = 6),
        axis.title.x  = element_text(size = 7),
        legend.position = "none")

cur <- ds_tbl %>% filter(group == "curated") %>% pull(dataset)
met <- ds_tbl %>% filter(group == "meta")    %>% pull(dataset)
scr <- ds_tbl %>% filter(group == "scrape")  %>% pull(dataset)

row1  <- c(lapply(cur, mk_small_gap), list(plot_spacer()))
row23 <- { p <- lapply(met, mk_small_gap)
c(p, replicate(max(0, 8 - length(p)), plot_spacer(), simplify = FALSE)) }
row4  <- c(lapply(scr, mk_small_gap))
wrap_plots(c(row1, row23, row4), ncol = 4)

ggsave("results/gap_plot_all.pdf", height = 16, width = 14, units = "cm")
