osc_raw <- read.csv("data/OSC/rpp_data.csv",header=TRUE)
# dt_osc <-  osc_raw %>% 
#   transmute(p_orig = T_pval_USE..O., 
#             p_repl = T_pval_USE..R.) %>% 
#   mutate(z_orig = qnorm(1 - p_orig/2),
#          z_repl = qnorm(1 - p_repl/2))

dt_osc <-  osc_raw %>% 
  rename(p_value = T_pval_USE..R.) %>% 
  transmute(
    metaid = NA,
    studyid = Study.Num,
    method = "RCT",
    measure = NA,
    z = z_from_p(p_value),
    z_operator = ifelse(p_value > 0, "=", ">"),
    b = NA,
    se = NA,
    year = NA,
    group = NA
    # ss = as.numeric(N..O.)
    ) %>% 
  filter(!is.na(z)) %>% 
  calc_study_weights()

fit_osc <- dt_osc  %>% fit_mixture_df(k = 3)
plot_mixture_v3(fit_osc, dt_osc)
psr <- psr_foo(fit_osc)

psr %>% 
  summarise(
    p = mean(power),
    p80 = sum(power > .8)/n(),
    s = mean(sgn),
    r = mean(rep))

pr_df <- function(df, fit) gap_vec(abs(df$z), p = fit$p, m = fit$m, s_snr = fit$sigma_SNR)

pr_df(dt_osc, fit_osc)$rep %>% mean
