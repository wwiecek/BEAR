library(scales)

df_psr %>%
  filter(dataset == "Cochrane") %>%
  select(snr, z) %>%
  pivot_longer(everything(), names_to = "stat", values_to = "val") %>%
  ggplot(aes(x = val, fill = stat, colour = stat)) +
  annotate("rect", xmin = -5, xmax = -1.96, ymin = 0, ymax = Inf,
           fill = "grey85", alpha = 0.2, colour = NA) +
  annotate("rect", xmin =  1.96, xmax =  5,   ymin = 0, ymax = Inf,
           fill = "grey85", alpha = 0.2, colour = NA) +
  geom_vline(xintercept = c(-1.96, 1.96), linetype = "dashed", colour = "grey40") +
  geom_density(alpha = 0.25, linewidth = 0.9) +
  scale_colour_manual(values = c(snr = "#377EB8", z = "#E41A1C"),
                      labels = c(snr = "SNR (S)", z = "Observed Z")) +
  scale_fill_manual(values = c(snr = alpha("#377EB8", 0.25), z = alpha("#E41A1C", 0.25)),
                    labels = c(snr = "SNR (S)", z = "Observed Z")) +
  scale_x_continuous(breaks = c(-5, -1.96, 0, 1.96, 5)) +
  coord_cartesian(xlim = c(-5,5)) +
  labs(x = "", y = "Density") +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank())

ggsave("results/snr_vs_z.pdf", units = "cm", width = 14, height = 9)


