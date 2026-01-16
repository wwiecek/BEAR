# This combines plot_figure_ecdf and power panel from plot_figure_psr
# load them in first

gg1 <- df_ecdf_smooth %>%
  filter(metric == "Power") %>% 
  mutate(metric = "1 - (Probability of Significance CDF)") %>% 
  ggplot(aes(x = value, y = 1 - ecdf, group = dataset, color = group)) +
  geom_vline(xintercept = 0.8, lty = "dashed", alpha = .5) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  facet_grid(~metric, scales = "free_x") +
  scale_x_continuous(breaks = c(0.05, 0.5, 0.8, 1)) +
  scale_colour_manual(values = bear_colors) +
  labs(x = "PoS", y = "Proportion of studies achieving") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") 
        # axis.text.x = element_text(angle = 45, hjust = 1))


z <- seq(0, 5, 0.1)
gg2 <-   map_dfr(names(mfl), ~{
  pr <- gap_vec(z, p = mfl[[.x]]$p, m = mfl[[.x]]$m, s_snr = mfl[[.x]]$sigma_SNR)
  data.frame(z, sgn = pr$sgn, rep = pr$rep, dataset = .x)
}) %>%
  mutate(colgr = bear_classification[dataset]) %>% 
  pivot_longer(c(sgn, rep), names_to = 'label', values_to = 'prob') %>%
  mutate(label = recode_factor(label, rep = "successful replication", sgn = "correct sign")) %>%
  mutate(metric = "Sign and replication probability") %>% 
  ggplot(aes(z, prob, group = interaction(dataset, label), color = colgr)) +
  geom_line(alpha = 0.6, linewidth = 0.4) +
  # annotate("text", x = 1.5, y = 0.85, label = "correct sign", angle = 45) +
  # annotate("text", x = 2.5, y = 0.5, label = "successful replication", angle = 45) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), lim = c(0, 1)) +
  scale_x_continuous(breaks = 0:5, lim = c(0, 5)) +
  scale_colour_manual(values = bear_colors) +
  facet_grid(~metric, scales = "free_x") +
  labs(x = "|z|", y = "Probability") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") 

gg2

library(patchwork) 
gg1 | gg2


ggsave("paper/figures/pos_gap.pdf", height = 8.5, width = 16.5, units = "cm")
