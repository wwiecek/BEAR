# Absolutely zero data work here

osc_raw <- read.csv("data_raw/OSC/rpp_data.csv",header=TRUE)
# dt_osc <-  osc_raw %>% 
#   transmute(p_orig = T_pval_USE..O., 
#             p_repl = T_pval_USE..R.) %>% 
#   mutate(z_orig = qnorm(1 - p_orig/2),
#          z_repl = qnorm(1 - p_repl/2))
saveRDS(osc_raw, "data/OSC.rds")