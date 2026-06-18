# Plot PoS survival curves and sign/replication probabilities for paper datasets.

library(tidyverse)
library(patchwork)

source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R")
source("R/paper_selection.R")

set.seed(42)

mfl <- load_all_mixtures(exclude = paper_analysis_exclude)
paper_datasets <- intersect(paper_selected_datasets, names(mfl))
mfl <- mfl[paper_datasets]

N <- 1e5
paper_group_colors <- paper_collection_group_colors

df_psr_small <- lapply(mfl, function(fit) {
  snr <- rmix(N, p = fit$p, m = fit$m, s = fit$sigma_SNR)
  power <- pnorm(-1.96, snr, 1) + 1 - pnorm(1.96, snr, 1)
  data.frame(power, row.names = NULL)
}) %>%
  bind_rows(.id = "dataset") %>%
  mutate(group = paper_dataset_color_group(dataset))

df_ecdf_smooth <- df_psr_small %>%
  group_by(dataset, group) %>%
  reframe(
    value = seq(min(power), max(power), length.out = 200),
    ecdf = ecdf(power)(value)
  )

gg1 <- df_ecdf_smooth %>%
  ggplot(aes(x = value, y = 1 - ecdf, group = dataset, color = group)) +
  geom_vline(xintercept = 0.8, lty = "dashed", alpha = .5) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  scale_x_continuous(breaks = c(0.05, 0.5, 0.8, 1)) +
  scale_colour_manual(values = paper_group_colors) +
  labs(x = "PoS", y = "Proportion of studies achieving") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

z <- seq(0, 5, 0.1)
gg2 <- map_dfr(names(mfl), ~{
  pr <- gap_vec(z, p = mfl[[.x]]$p, m = mfl[[.x]]$m,
                s_snr = mfl[[.x]]$sigma_SNR)
  data.frame(z, sgn = pr$sgn, rep = pr$rep, dataset = .x)
}) %>%
  mutate(group = paper_dataset_color_group(dataset)) %>%
  pivot_longer(c(sgn, rep), names_to = "label", values_to = "prob") %>%
  mutate(
    label = recode_factor(label, rep = "successful replication",
                          sgn = "correct sign"),
    metric = "Sign and replication probability"
  ) %>%
  ggplot(aes(z, prob, group = interaction(dataset, label), color = group)) +
  geom_line(alpha = 0.6, linewidth = 0.4) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), lim = c(0, 1)) +
  scale_x_continuous(breaks = 0:5, lim = c(0, 5)) +
  scale_colour_manual(values = paper_group_colors) +
  labs(x = "|z|", y = "Probability") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

pos_gap <- gg1 | gg2
ggsave("paper/figures/pos_gap.pdf", pos_gap,
       height = 8.5, width = 16.5, units = "cm")
