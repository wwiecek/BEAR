df_psr_v2 <- lapply(mfl, powsignrep, N = 1e04) %>% bind_rows(.id = "dataset")

df_psr_v2 %>% 
  mutate(snr = abs(snr)) %>% 
  mutate(col = bear_classification[dataset]) %>% 
  filter(snr < 100) %>% 
  ggplot(aes(snr, fill = col)) + 
  geom_histogram(breaks = seq(0, 5, length = 20)) + 
  coord_cartesian(xlim = c(0, 5)) +
  facet_wrap(~dataset) +
  theme(legend.position = "none") +
  xlab("|SNR| (10,000 draws from fit)")
