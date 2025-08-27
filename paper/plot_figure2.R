# analytical workflow for each dataset
library(tidyverse)
library(ggplot2)

source("R/settings.R")
source("R/exaggeration.R")
source("R/gap.R")
source("R/helpers.R")
source("R/mix.R")


mfl <- load_all_mixtures()
N <- 1e04

df_psr_small <- lapply(mfl, function(fit) {
  snr <- rmix(N, p = fit$p, m = fit$m, s = fit$sigma_SNR)
  z   <- snr + rnorm(N)
  power <- (1 - pnorm(1.96, snr, 1)) + pnorm(-1.96, snr, 1)
  pr <- gap_vec(z, p = fit$p, m = fit$m, s_snr = fit$sigma_SNR)
  data.frame(snr, z, power, row.names = NULL) %>%
    mutate(sgn = pr$sgn, rep = pr$rep)
}) %>% 
  bind_rows(.id = "dataset") %>%
  mutate(z = abs(z)) %>% 
  mutate(group = bear_classification[dataset]) %>%
  pivot_longer(c(power, sgn, rep), names_to = "metric", values_to = "value") %>% 
  mutate(metric = factor(metric, levels = c("power", "rep", "sgn"),
                         labels = c("Power", "Pr(Exact Replication)", "Pr(Sign)")))

# Create smooth ECDF with fewer points (smaller PDF file at the end)
df_ecdf_smooth <- df_psr_small %>%
  group_by(dataset, metric, group) %>%
  do({
    vals <- seq(min(.$value), max(.$value), length.out = 200)
    ecdf_fun <- ecdf(.$value)
    data.frame(value = vals, ecdf = ecdf_fun(vals))
  }) %>%
  ungroup()

custom_breaks <- function(x) {
  if (min(x, na.rm = TRUE) < 0.4) {  # Power and Exact Replication
    c(0.05, 0.5, 0.8, 1)
  } else {  # Sign 
    c(0.5, 0.8, 1)
  }
}

df_ecdf_smooth %>%
  ggplot(aes(x = value, y = 1 - ecdf, group = dataset, color = group)) +
  geom_vline(xintercept = 0.8, lty = "dashed", alpha = .5) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  scale_color_manual(values = cols_grp, 
                     labels = c("Curated", "Meta-analysis", "Scraped")) +
  facet_grid(~metric, scales = "free_x") +
  scale_x_continuous(breaks = custom_breaks) +
  labs(x = NULL, y = "Proportion achieving") +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("results/psr_ecdf.pdf", units = "cm", height = 8, width = 14)

