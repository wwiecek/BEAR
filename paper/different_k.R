# Let's try fitting k = 2 and k = 3
fit2 <- fit_mixture_df(ct_subsets$ph1, k = 2)
fit3 <- fit_mixture_df(ct_subsets$ph1, k = 3)
fit4 <- ct_fits$ph1
fit5 <- fit_mixture_df(ct_subsets$ph1, k = 5)
fit6 <- fit_mixture_df(ct_subsets$ph1, k = 6)
fit7 <- fit_mixture_df(ct_subsets$ph1, k = 7)

wrap_plots(lapply(list(fit2,fit3,fit4,fit5,fit6,fit7), 
                  function(x) plot_mixture_v3(x, ct_subsets$ph1)), ncol = 3)

fit3o <- fit_mixture_df(ct_subsets$ph1, k = 3, legacy_mode = TRUE)

plot_mixture_v3(fit2, ct_subsets$ph1, "")
plot_mixture_v3(ct_fits$ph1, ct_subsets$ph1, "")
plot_mixture_v3(fit5, ct_subsets$ph1, "")

edf <- expand_grid(dataset = names(ct_subsets), k = c(2,3,4,5)) %>% 
  mutate(df = ct_subsets[dataset]) %>% 
  mutate(fit = map2(df, k, function(x,y) fit_mixture_df(x, k = y)))

edf %>% unnest(fit) %>% select(df, k, AIC, BIC) %>% distinct
edf %>% unnest(fit) %>% select(df, k, omega) %>% distinct %>% spread(k, omega)

pl <- edf %>% 
  mutate(plot = map2(fit, df, plot_mixture_v3)) %>% 
  pull(plot)

plot_mixture_v3(fit = edf$fit[[1]], dt = edf$df[[1]])

wrap_plots(pl, ncol = 4)
