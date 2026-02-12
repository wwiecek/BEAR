library(kableExtra)
library(tidyverse)
library(ggplot2)

source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R")

tab_psr <- read_csv("paper/power_sign_rep.csv") %>% 
  mutate(gr = bear_classification[dataset]) %>% 
  mutate(gr = ifelse(gr %in% c("curated", "meta"), "curated+meta", gr)) %>% 
  arrange(gr, desc(assurance)) %>% 
  mutate(dataset = bear_names[dataset]) %>%
  # select(dataset, prop_signif, omega, assurance, pos_80pct, replication, repl_196, sign, sign_196)
  select(dataset, omega, assurance, pos_80pct, replication, repl_signif, sign, sign_signif)

tab_psr 
tab_psr %>%
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>% 
  kable(format = "latex", booktabs = TRUE, escape = FALSE) %>% 
        # col.names = c("Dataset", "Assurance", "Pr(power > 0.8)", "Correct sign", 
                      # "Replication")) %>% 
        # align = c("p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(11, hline_after = TRUE) %>%
  row_spec(13, hline_after = TRUE) %>%
  writeLines("paper/tables/table2.tex")

# results with |z| = 1.96
read_csv("paper/power_sign_rep.csv") %>% 
  mutate(gr = bear_classification[dataset]) %>% 
  mutate(gr = ifelse(gr %in% c("curated", "meta"), "curated+meta", gr)) %>% 
  arrange(gr, desc(assurance)) %>% 
  mutate(dataset = bear_names[dataset]) %>%
  select(dataset, prop_signif, omega, replication, repl_196, sign, sign_196) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>% 
  kable(format = "latex", booktabs = TRUE, escape = FALSE) %>% 
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(11, hline_after = TRUE) %>%
  row_spec(13, hline_after = TRUE) %>%
  writeLines("paper/tables/table3.tex")


# What if studies were larger?

# left_join(summarise_psr(df_psr),
#           summarise_psr(df_psr_169), 
#           by = "dataset") %>% 
#   mutate(diff_p = p.x - p.y,
#          diff_r = r.x - r.y) %>% 
#   mutate_if(is.numeric, function(x) scales::percent(as.numeric(x), accuracy = 1))
