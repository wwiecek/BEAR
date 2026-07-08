halfnorm_mix_density <- function(z, p, sigma) {
  # pdf of |Z| when Z~Σ p_k N(0, σ_k²)
  2 * vapply(z, function(x) sum(p * dnorm(x, 0, sigma)), numeric(1))
}

sel_weighted_density <- function(z, p, sigma, omega) {
  # selection‑weighted pdf of |Z|  (publication bias)
  base <- halfnorm_mix_density(z, p, sigma)
  adj  <- ifelse(z < 1.96, omega, 1)
  B1   <- 2 * sum(p * pnorm(-1.96, 0, sigma))
  B2   <- 1 - B1
  adj * base / (B1 + omega * B2)
}

library(dplyr)
library(tidyr)
library(posterior)

K <- 4
grid <- tibble(z = seq(0, 8, 0.05))

# keep only the vector parameters we need; handle omega separately
vec_draws <-
  as_draws_df(fit) %>%
  select(starts_with("p["), starts_with("sigma["), .chain, .iteration, .draw)

omega_draws <-
  as_draws_df(fit) %>%
  select(omega, .chain, .iteration, .draw)

library(purrr)

dens_draws <-
  vec_draws %>%
  pivot_longer(
    cols = -c(.chain, .iteration, .draw),
    names_to = c(".var", "idx"),
    names_pattern = "([A-Za-z_]+)\\[(\\d+)\\]",
    values_to = "value",
    values_drop_na = TRUE) %>%
  mutate(idx = as.integer(idx)) %>%
  pivot_wider(names_from = .var, values_from = value) %>%
  left_join(omega_draws, by = c(".chain", ".iteration", ".draw")) %>%
  group_by(.draw) %>% nest() %>%                    # one tibble per posterior draw
  crossing(grid) %>%                                # add the z grid
  mutate(fz = map2_dbl(z, data,                    # <- row‑wise mapping
                       ~ sel_weighted_density(.x,
                                              p     = .y$p[[1]],   # first list element is the
                                              sigma = .y$sigma[[1]],# numeric vector of length K
                                              omega = .y$omega[[1]]))) %>%
  ungroup()


summary_df <-
  dens_draws %>%
  group_by(z) %>%
  summarise(mean   = mean(fz),
            lower  = quantile(fz, 0.025),
            upper  = quantile(fz, 0.975),
            .groups = "drop")

library(ggplot2)

ggplot() +
  geom_histogram(data = df,
  aes(x = abs(z), y = after_stat(density), weight = weight),
  breaks = seq(0, 10, 0.5),
  colour = "black", fill = "white") +
  geom_ribbon(data = summary_df,
              aes(x = z, ymin = lower, ymax = upper),
              alpha = 0.2, fill = "steelblue") +
  geom_line(data = summary_df,
            aes(x = z, y = mean),
            colour = "steelblue", size = 1) +
  labs(x = "absolute z statistic", y = "") +
  coord_cartesian(xlim = c(0, 10)) +
  theme_bw()
