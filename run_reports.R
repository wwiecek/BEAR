d$truncated="not truncated"
fit <- fit_mixture(z=d$z, truncated=d$truncated, k=4, weight=1/d$k)

knit_with_df(d, fit)
stitch_and_knit("brodeur_desc.Rmd", "template.Rmd", d, fit, "template_stitch.pdf")

#

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
