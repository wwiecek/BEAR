library(kableExtra)
library(tidyverse)
library(ggplot2)

source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R")

# Calcualte performance of datasets 
mfl <- load_all_mixtures(exclude = paper_do_not_include)
df_psr <- lapply(mfl, powsignrep) %>% bind_rows(.id = "dataset")
# now only keeping significant results 
# (power columns won't make sense/won't change!----but I do not use them)
df_psr196 <- lapply(mfl, powsignrep, z_star = 1.959) %>% bind_rows(.id = "dataset")

# Erik: I’d like to put the following in Table 2
# 
# - name of the dataset
# - omega (risk ratio of publication)
# - raw proportion of significant
# - assurance
# - predictive power when |z|=1.96
# - P(|SNR|>2.8) = P(PoS>0.8) (PoS stands for the probability of significance)
# - probability of the correct sign
# - probability of the correct sign when |z|=1.96

bear <- readRDS("BEAR.rds")
bear_summary_psr <- bear %>% 
  mutate(dataset = ifelse(dataset == "clinicaltrials" | dataset == "euctr", "ctgov_euctr", dataset)) %>% 
  group_by(dataset) %>% 
  mutate(z_operator = ifelse(is.na(z_operator), "=", z_operator)) %>% 
  # if we used 1.96 here, we'd lose a big chunk of scraping studies that reported p of 0.05
  summarise(prop_signif = sum((abs(z) > 1.959) & (z_operator != "<"))/n())

# Table with study summaries ------
summarise_psr <- function(df) 
  df %>% 
  group_by(dataset) %>% 
  summarise(
    assurance = mean(power),
    pos_80pct = sum(power > .8)/n(),
    sign = mean(sgn),
    replication = mean(rep)) 

tab2 <- summarise_psr(df_psr) %>% 
  left_join(summarise_psr(df_psr196) %>% 
              transmute(dataset, 
                        sign_signif = sign, 
                        repl_signif = replication), 
            by = "dataset") %>% 
  left_join(bear_summary_psr, by = "dataset") %>% 
  rowwise() %>%
  mutate(omega = mfl[[dataset]]$omega[1],
         sign_196 = gap_vec_fit(1.96, mfl[[dataset]])$sgn,
         repl_196 = gap_vec_fit(1.96, mfl[[dataset]])$rep) %>%
  ungroup() %>% 
  select(dataset, prop_signif, omega, assurance, pos_80pct, 
         replication, repl_196, repl_signif,
         sign, sign_196, sign_signif) %>% 
  mutate_if(is.numeric, function(x) round(x, 3)) #to make csv readable

rm(bear)
write_csv(tab2, file = "power_sign_rep.csv")
