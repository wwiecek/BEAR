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
# df_psr_169 <- lapply(mfl, powsignrep, 1.69) %>% bind_rows(.id = "dataset")





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

bear <- readRDS("data/BEAR.rds")
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
  left_join(bear_summary_psr, by = "dataset") %>% 
  rowwise() %>%
  mutate(omega = mfl[[dataset]]$omega[1],
         sign_196 = gap_vec_fit(1.96, mfl[[dataset]])$sgn,
         repl_196 = gap_vec_fit(1.96, mfl[[dataset]])$rep) %>%
  ungroup() %>% 
  mutate(gr = bear_classification[dataset]) %>% 
  mutate(gr = ifelse(gr %in% c("curated", "meta"), "curated+meta", gr)) %>% 
  arrange(gr, desc(assurance)) %>% 
  mutate(dataset = bear_names[dataset]) %>% 
  select(dataset, prop_signif, omega, assurance, pos_80pct, replication, repl_196, sign, sign_196)

tab2 
tab2 %>%
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>% 
  kable(format = "latex", booktabs = TRUE, escape = FALSE) %>% 
        # col.names = c("Dataset", "Assurance", "Pr(power > 0.8)", "Correct sign", 
                      # "Replication")) %>% 
        # align = c("p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(11, hline_after = TRUE) %>%
  row_spec(13, hline_after = TRUE) %>%
  writeLines("paper/tables/table2.tex")

# What if studies were larger?

# left_join(summarise_psr(df_psr),
#           summarise_psr(df_psr_169), 
#           by = "dataset") %>% 
#   mutate(diff_p = p.x - p.y,
#          diff_r = r.x - r.y) %>% 
#   mutate_if(is.numeric, function(x) scales::percent(as.numeric(x), accuracy = 1))
