d$truncated="not truncated"
fit <- fit_mixture(z=d$z, truncated=d$truncated, k=4, weight=1/d$k)

knit_with_df(d, fit)
stitch_and_knit("brodeur_desc.Rmd", "template.Rmd", d, fit, "template_stitch.pdf")

#
source("R/helpers.R")
source("R/mix.R")
bear %>% group_by(dataset, truncated) %>% tally() %>% spread(truncated, n) 
bear_list <- 
  bear %>% 
  mutate(weight = 1/k) %>% 
  mutate(truncated = ifelse(is.na(truncated), "not truncated", truncated)) %>% 
  split(bear$dataset)

fitm <- fit_mixture(df$z, df$truncated, k = k, weight = df$weight)
plot_mixture(fitm, df$z, 1/df$k)
knit_with_df()

library(rstan)
library(tidybayes)
rstan_options(auto_write = TRUE)
sm_mixture <- stan_model("mixture.stan")
options(mc.cores = 4)
standt <- compose_data(bear_list$WWC %>% 
                         filter(!is.infinite(z)) %>% 
                         transmute(z = abs(z),
                                   w = 1/k,
                                   is_trunc = 0,
                                   is_high = abs(z) >= 1.96),
                       K = 4, .n_name = n_prefix("N"))
fit <- sampling(data = standt, sm_mixture, iter = 400)
