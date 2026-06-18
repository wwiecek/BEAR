# library(geomtextpath)
library(tidyverse)
library(patchwork)

source("R/settings.R")
source("R/paper_selection.R")
source("R/psr.R")
source("R/helpers.R")
source("R/mix.R")

set.seed(42)

mfl <- load_all_mixtures(exclude = paper_analysis_exclude)
plots_per_row <- 4
panel_title_size <- 7.2
a4_text_width_cm <- 16.5

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

  z_hit <- 1.96
  pr_hit <- gap_vec(z_hit, p = fit$p, m = fit$m, s_snr = fit$sigma_SNR)
  hit_df <- tibble(
    z = z_hit,
    sgn = pr_hit$sgn,
    rep = pr_hit$rep
  ) %>%
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
    geom_point(data = hit_df, size = 1) +
    {if(single) geom_text(aes(label = label), hjust = 1, size = 3.5)} +
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
psr_table <- read_csv("paper/power_sign_rep.csv", show_col_types = FALSE) %>%
  transmute(dataset, PoS = assurance)

mk_small_gap <- function(ds)
  small_gap_plot(mfl[[ds]]) +
  labs(title = bear_labels[ds]) +
  theme(plot.title  = element_text(size = panel_title_size),
        axis.text   = element_text(size = panel_title_size * 0.75),
        axis.title.x  = element_text(size = panel_title_size * 0.75),
        legend.position = "none")

category_row <- function(datasets) {
  datasets <- order_paper_datasets(datasets, psr_table)
  plots <- lapply(datasets, mk_small_gap)
  n_spacers <- (plots_per_row - length(plots) %% plots_per_row) %% plots_per_row
  c(plots, rep(list(plot_spacer()), n_spacers))
}

available_datasets <- intersect(names(mfl), paper_selected_datasets)
paper_groups <- lapply(paper_dataset_groups, intersect, available_datasets)

plotlist <- c(
  category_row(paper_groups$curated_evidence),
  category_row(paper_groups$domain_sample)
)

expected_group_sizes <- c(curated_evidence = 8L, domain_sample = 8L)
actual_group_sizes <- c(curated_evidence = length(paper_groups$curated_evidence),
                        domain_sample = length(paper_groups$domain_sample))
if (!identical(actual_group_sizes, expected_group_sizes)) {
  stop("Expected two collection groups of 8 datasets, but found: ",
       paste(names(actual_group_sizes), actual_group_sizes,
             sep = " = ", collapse = ", "))
}

rows <- length(plotlist) / plots_per_row

gap_plot_all <- wrap_plots(plotlist, ncol = plots_per_row) +
  plot_annotation(caption = "|z|") &
  theme(
    plot.caption = element_text(hjust = 0.5),
    axis.text = element_text(size = panel_title_size * 0.75)
  )

ggsave("paper/figures/gap_plot_all.pdf", gap_plot_all,
       height = rows * 4 * a4_text_width_cm / 14,
       width = a4_text_width_cm, units = "cm",
       device = cairo_pdf)
