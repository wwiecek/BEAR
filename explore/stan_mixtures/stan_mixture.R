library(rstan)
library(tidybayes)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

sm1 <- stan_model("mixture.stan")
sm2 <- stan_model("mixture_v2.stan")
sm3 <- stan_model("mixture_non0.stan")

df <- bear_list_thin[["Adda"]] %>% 
  # filter(truncated != 1) %>% 
  filter(z > .01) %>% 
  sample_n(1000)

# df <- rbind(
#   data.frame(truncated = 1*(rnorm(1000) - 0.8) > 0, 
#              weight = 1, 
#              z = abs(rnorm(1000, 0, 5))),
#   data.frame(truncated = 1*(rnorm(1000) - 0.8) > 0, 
#              weight = 1, 
#              z = abs(rnorm(1000, 2, 2))),
#   data.frame(truncated = 1*(rnorm(1000) - 0.8) > 0, 
#              weight = 1, 
#              z = abs(rnorm(1000, 5, 2)))
# )

df <- rbind(
  data.frame(truncated = 0, weight = 1, z = abs(rnorm(3000, 0, 5))),
  data.frame(truncated = 0, weight = 1, z = abs(rnorm(1500, 2, 2))),
  data.frame(truncated = 0, weight = 1, z = abs(rnorm(500, 5, 2)))
)
df$z %>% density %>% plot
  
standt <- df %>% 
  mutate(z = abs(z)) %>% 
  rename(w = weight) %>% 
  mutate(is_high = 1*(z > 1.96)) %>% 
  select(truncated, z, w, is_high) %>% 
  compose_data(K=3, N = nrow(.), thresh = 1.96)

fit <- sampling(
  sm1, 
  data  = standt,
  chains = 4, iter = 500, 
  # seed = 20250616,
  control = list(adapt_delta = 0.9)
)

fit3 <- sampling(
  sm3, 
  data  = standt,
  chains = 4, iter = 500, 
  # seed = 20250616,
  control = list(adapt_delta = 0.9)
)

print(fit, pars = c("theta", "mu", "sigma", "omega"))
