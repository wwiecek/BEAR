source("R/fit_density_calc.R")
source("R/plot_mixture.R")
# source("calculate_psr.R")
library(scales)

mfl <- load_all_mixtures()

nm <- names(bear_names)[1]

gg1 <- fit_density_calc(mfl[[nm]])$df %>% 
  transmute(x =x, snr = corrected_f_snr, z = corrected_fz) %>% 
  gather(stat, val, -x) %>% 
  # rbind(data.frame(stat = "z_orig", val = bear_list_thin$Cochrane$z)) %>% 
  # mutate(val = abs(val)) %>% 
  ggplot(aes(x = x, y = val, fill = stat, colour = stat, group = stat)) +
  annotate("rect", xmin = -5, xmax = -1.96, ymin = 0, ymax = Inf,
           fill = "grey85", alpha = 0.2, colour = NA) +
  annotate("rect", xmin =  1.96, xmax =  5,   ymin = 0, ymax = Inf,
           fill = "grey85", alpha = 0.2, colour = NA) +
  geom_vline(xintercept = c(-1.96, 1.96), linetype = "dashed", colour = "grey40") +
  geom_line(linewidth = 0.9) +
  geom_ribbon(aes(ymin = 0, ymax = val), alpha = 0.25, colour = NA) +
  scale_colour_manual(values = c(snr = "#377EB8", z = "#E41A1C"),
                      labels = c(snr = "SNR (S)", z = "Observed Z")) +
  scale_fill_manual(values = c(snr = alpha("#377EB8", 0.25), z = alpha("#E41A1C", 0.25)),
                    labels = c(snr = "SNR (S)", z = "Observed Z")) +
  scale_x_continuous(breaks = c(-5, -1.96, 1.96, 5)) +
  coord_cartesian(xlim = c(-7,7)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")


gg2 <- plot_mixture_v4(mfl[[nm]], bear_list_thin[[nm]], show_corrected = T)

library(patchwork)
gg2 | gg1


ggsave("paper/figures//snr_vs_z.pdf", units = "cm", width = 14, height = 9)
